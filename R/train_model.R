require(RPostgreSQL)
require(rgdal)
require(rgeos)
require(maptools)
require(data.table)
require(zoo)
require(doMC)
require(lubridate)
require(plyr)
require(R2jags)
require(rstan)
require(spatstat)
require(raster)
require(geostatsp)
require(pscl)
require(lmtest)
require(MASS)
require(rstanarm)
require(rethinking)

drv <- dbDriver("PostgreSQL")  #Specify a driver for postgreSQL type database
con <- dbConnect(drv, dbname="qaeco_spatial", user="qaeco", password="Qpostgres15", host="boab.qaeco.com", port="5432")  #Connection to database server on Boab

#############Data#############

## Get master dataset
coll_db_h <- as.data.table(dbGetQuery(con,"
SELECT
  grid.id AS id, pts.hour AS hour, COUNT(pts.id) AS coll
FROM
  vline.vic_gda9455_fauna_egkcoll_train_onnetwork AS pts, gis_victoria.vic_gda9455_admin_state_1kmgrid AS grid
WHERE
  ST_contains(grid.geom, pts.geom)
GROUP BY
  grid.id, pts.hour;
  "))
setkey(coll_db_h,id,hour)
coll_db_h[,coll:=as.integer(coll)]


coll_db_bgh <- as.data.table(dbGetQuery(con,"
    SELECT
      x.id AS id, x.egk AS egk, AVG(x.train) AS trains, AVG(x.speed) AS speed, x.hour AS hour, CAST(SUM(x.coll) AS integer) AS coll
    FROM
      (SELECT 
          grid.id AS id, grid.egk AS egk, COUNT(seg.train)*ST_Length(ST_LineMerge(ST_Union(ST_intersection(grid.geom, seg.geom))))/1000 AS TRAIN, AVG(seg.speed) AS speed, seg.hour AS hour, CAST(0 AS integer) AS coll
        FROM
          vline.vic_gda9455_rail_vline_speeds AS seg, 
          (SELECT 
            g.id as id,
            ST_Value(p.rast,ST_Centroid(g.geom)) AS egk,
            g.geom as geom
          FROM 
            gis_victoria.vic_gda9455_grid_egk_preds_brt AS p,
            gis_victoria.vic_gda9455_admin_state_1kmgrid AS g
          WHERE
            ST_Intersects(p.rast,ST_Centroid(g.geom))) AS grid
        WHERE
          ST_intersects(grid.geom, seg.geom)
        AND
          grid.egk NOTNULL
        AND
          seg.speed <> 'Infinity'
        AND
          seg.speed <= 160
        GROUP BY
          grid.id, grid.egk, seg.train, seg.hour
        ) as x
    GROUP BY
      x.id, x.egk, x.hour
    "))
setkey(coll_db_bgh,id,hour)

model.data.h <- coll_db_bgh
model.data.h[coll_db_h, coll := coll_db_h$coll]

nomatch <- coll_db_h[!model.data.h]

#write.csv(model.data.h, file = "data/model_data_h.csv", row.names=FALSE)
#model.data.h <- as.data.table(read.delim("data/model_data_h.csv", header=T, sep=","))

## Monthly variation in dawn/dusk
coll_db_hm <- as.data.table(dbGetQuery(con,"
  SELECT
    pts.id AS pid, grid.id AS id, pts.hour AS hour, pts.day AS day, pts.month AS month, pts.year as year, pts.x AS x, pts.y AS y
  FROM
    vline.vic_gda9455_fauna_egkcoll_train_onnetwork AS pts, gis_victoria.vic_gda9455_admin_state_1kmgrid AS grid
  WHERE
    ST_contains(grid.geom, pts.geom)
  "))

spatial <- as.matrix(cbind(x=145,y=-37))

for (i in 1:nrow(coll_db_hm)){
  coll_db_hm[i,dawn:=(crepuscule(spatial,as.POSIXct(paste0("2012-",month,"-15")),solarDep=6,direction=c("dawn"),POSIXct.out=FALSE)*24)]
  coll_db_hm[i,dusk:=(crepuscule(spatial,as.POSIXct(paste0("2012-",month,"-15")),solarDep=6,direction=c("dusk"),POSIXct.out=FALSE)*24)]
  coll_db_hm[i,dawndusk:=(crepuscule(spatial,as.POSIXct(paste0("2012-",month,"-15")),solarDep=6,direction=c("dusk"),POSIXct.out=FALSE)*24)-(crepuscule(spatial,as.POSIXct(paste0("2012-",month,"-15")),solarDep=6,direction=c("dawn"),POSIXct.out=FALSE)*24)]
}
setkey(coll_db_hm,id,hour,dawndusk)

registerDoMC(detectCores() - 1)
coll_db_bghm <- foreach (i = 1:12, .combine=rbind) %dopar% {
  cbind(coll_db_bgh, "dawn"=(crepuscule(spatial,as.POSIXct(paste0("2012-",as.integer(sprintf("%02d",i)),"-15")),solarDep=6,direction=c("dawn"),POSIXct.out=FALSE)*24), "dusk"=(crepuscule(spatial,as.POSIXct(paste0("2012-",as.integer(sprintf("%02d",i)),"-15")),solarDep=6,direction=c("dusk"),POSIXct.out=FALSE)*24), "dawndusk"=(crepuscule(spatial,as.POSIXct(paste0("2012-",as.integer(sprintf("%02d",i)),"-15")),solarDep=6,direction=c("dusk"),POSIXct.out=FALSE)*24)-(crepuscule(spatial,as.POSIXct(paste0("2012-",as.integer(sprintf("%02d",i)),"-15")),solarDep=6,direction=c("dawn"),POSIXct.out=FALSE)*24))
}
setkey(coll_db_bghm,id,hour,dawndusk)

model.data.hm <- coll_db_bghm
c <- coll_db_hm[model.data.hm,.N,by=c("id","hour","dawndusk"),nomatch=0]
model.data.hm[c, coll := c$N]

nomatch <- coll_db_hm[!model.data.hm]

#write.csv(model.data.hm, file = "data/model_data_hm.csv", row.names=FALSE)
#model.data.hm <- as.data.table(read.delim("data/model_data_hm.csv", header=T, sep=","))

summary(model.data.hm)
-
model.data.hm[coll>=1,.N]
model.data.hm[coll==0,.N]
mean(model.data.hm$coll)
var(model.data.hm$coll)
length(unique(model.data.hm[,id]))

curve(exp(-((x - mean(model.data.hm$dawn))^2) / 12) + exp(-((x - mean(model.data.hm$dusk))^2) / 12), from=0, to=24)

dawn.or.dusk <- function (h, dawn = 6, dusk = 18, slope = 2) {
  # compose a two-Gaussian curve, for day/night
  # dawn and dusk or dusk and dawn
  
  # get inner and outer differences
  diff_inner <- (dusk - dawn) / slope
  diff_outer <- ((24 - dusk) + dawn) / slope
  
  ifelse(h < dawn,
         d(h, dawn, dusk - 24, diff_outer),
         ifelse(h < dusk,
                d(h, dawn, dusk, diff_inner),
                d(h, dawn + 24, dusk, diff_outer)))
}

model.data.hm$dawnordusk <- dawn.or.dusk(h=model.data.hm$hour,dawn=model.data.hm$dawn,dusk=model.data.hm$dusk)

model.data.hm$light <- sin((2 * pi * (model.data.hm$hour - 6)) / 24) # ambient light intensity

model.data.hm$light2 <- model.data.hm$light ^ 2

## Get cross-validation datasets (by year)

for(i in seq(2009,2015,1)){
  temp <- as.data.table(dbGetQuery(con,paste0("
    SELECT
      grid.id AS ID, pts.hour AS HOUR, COUNT(pts.id) AS COLL
    FROM
      vline.vic_gda9455_fauna_egkcoll_train_onnetwork AS pts, gis_victoria.vic_gda9455_admin_state_1kmgrid AS grid
    WHERE
      ST_contains(grid.geom, pts.geom)
    AND
      year != ",i,"
    GROUP BY
      grid.id, pts.hour;
  ")))
  temp2 <- as.data.table(dbGetQuery(con,paste0("
    SELECT
      grid.id AS ID, pts.hour AS HOUR, CAST(0 AS INTEGER) AS COLL
    FROM
      vline.vic_gda9455_fauna_egkcoll_train_onnetwork AS pts, gis_victoria.vic_gda9455_admin_state_1kmgrid AS grid
    WHERE
      ST_contains(grid.geom, pts.geom)
    AND
      year = ",i,"
    GROUP BY
      grid.id, pts.hour;
  ")))
  temp2[,coll:=NA]
  assign(paste0("coll_",i),rbind(temp,temp2))
}



#############GLM#############
# coll.glm <- glm(formula = coll ~ egk + speed + offset(log(trains)), family = poisson(link = "log"), data = model.data)
# summary(coll.glm)
# paste("% Error Explained: ",round(((coll.glm$null.deviance - coll.glm$deviance)/coll.glm$null.deviance)*100,2),sep="")  #Report reduction in deviance

# coll.glm.h <- glm(formula = coll ~ egk + speed + sin(2*pi*(hour/24)) + cos(2*pi*(hour/24)) + sin(4*pi*(hour/24)) + cos(4*pi*(hour/24)) + offset(log(trains)), family = poisson(link = "log"), data = model.data.h)
# summary(coll.glm.h)
# paste("% Error Explained: ",round(((coll.glm.h$null.deviance - coll.glm.h$deviance)/coll.glm.h$null.deviance)*100,2),sep="")  #Report reduction in deviance

coll.glm.h2 <- glm.nb(formula = coll ~ egk + speed + sin(2*pi*(hour/24)) + cos(2*pi*(hour/24)) + sin(4*pi*(hour/24)) + cos(4*pi*(hour/24)) + offset(log(trains)), data = model.data.h2)
summary(coll.glm.h2)
paste("% Error Explained: ",round(((coll.glm.h2$null.deviance - coll.glm.h2$deviance)/coll.glm.h2$null.deviance)*100,2),sep="")  #Report reduction in deviance

coll.glm.hm <- glm.nb(formula = coll ~ egk + speed + exp(-((hour - dawn)^2) / 12) + exp(-((hour - dusk)^2) / 12) + offset(log(trains)), data = model.data.hm, trace = TRUE, init.theta = .005)
summary(coll.glm.hm)
paste("% Error Explained: ",round(((coll.glm.hm$null.deviance - coll.glm.hm$deviance)/coll.glm.hm$null.deviance)*100,2),sep="")  #Report reduction in deviance

coll.glm.hm2 <- glm.nb(formula = coll ~ egk + speed + light + light2 + dawnordusk + offset(log(trains)), data = model.data.hm, trace = TRUE, init.theta = .005)
summary(coll.glm.hm2)
paste("% Error Explained: ",round(((coll.glm.hm2$null.deviance - coll.glm.hm2$deviance)/coll.glm.hm2$null.deviance)*100,2),sep="")  #Report reduction in deviance



model.data.hm$l.trains <- log(model.data.hm$trains)
model.data.hm$pk.one <- exp(-((model.data.hm$hour - model.data.hm$dawn)^2) / 12)
model.data.hm$pk.two <- exp(-((model.data.hm$hour - model.data.hm$dusk)^2) / 12)


gbm.model <- gbm.step(data = model.data.hm, gbm.x = c(2,4,11,12,10), gbm.y = 6, family = "poisson", tree.complexity = 3, learning.rate = 0.005, bag.fraction = 0.5)


curve(2.92988 * exp(-((x - 7)^2) / 12) + 2.67212 * exp(-((x - 19)^2) / 12),from=0,to=23,n=24, ylab = "Influence on Collision Rate", xlab = "Hour")


coll.glm.zip.hm <- zeroinfl(formula = coll ~ speed + exp(-((hour - dawn)^2) / 12) + exp(-((hour - dusk)^2) / 12) + offset(log(trains)) | egk, data = model.data.hm, dist = "poisson", link = "logit")
summary(coll.glm.zip.hm)


# coll.glm.zinb.h <- zeroinfl(coll ~ speed + sin(2*pi*(hour/24)) + cos(2*pi*(hour/24)) + sin(4*pi*(hour/24)) + cos(4*pi*(hour/24)) + offset(log(trains)) | egk, data = model.data.h, dist = "negbin", link = "logit")
# coll.glm.zinb.hn <- update(coll.glm.zinb.h, . ~ 1)

#pchisq(2 * (logLik(coll.glm.zinb.h) - logLik(coll.glm.zinb.hn)), df = 3, lower.tail=FALSE)

#lrtest(coll.glm.zip.h, coll.glm.zinb.h)

#vuong(coll.glm.zip.h, coll.glm.zinb.h)

#vuong(coll.glm, coll.glm.zip)

pr <- predict(coll.glm.zinb.h,type="zero")  # π

mu <- predict(coll.glm.zinb.h,type="count") # μ

zinb <- pr + (1-pr)*exp(-mu)

mean(zinb)
#0.9813324

mnb <- glm.nb(coll ~ egk + speed + sin(2*pi*(hour/24)) + cos(2*pi*(hour/24)) + sin(4*pi*(hour/24)) + cos(4*pi*(hour/24)) + log(trains), data=model.data.h)

munb <- exp(predict(mnb))

theta <- mnb$theta

znb <- (theta/(munb+theta))^theta

mean(znb)
#0.9847442

curve(invlogit(-9.573714+3.467664*x),from=0,to=1,n=100, xlab = "EGK Likelihood", ylab = "Rate of Collisions")

curve(0.245064 * sin(2*pi*(x/24)) + 0.582947 * cos(2*pi*(x/24)) - 0.423084 * sin(4*pi*(x/24)) - 1.013136 * cos(4*pi*(x/24)),from=0,to=23,n=24, ylab = "Influence on Collision Rate", xlab = "Hour")

EP <- resid(mnb, type = "pearson")
ED <- resid(mnb, type = "deviance")
mu <- predict(mnb, type = "response")
E <- model.data.h$coll - mu
EP2 <- E / sqrt(7.630148 * mu)
op <- par(mfrow = c(2, 2))
plot(x = mu, y = E, main = "Response residuals")
plot(x = mu, y = EP, main = "Pearson residuals")
plot(x = mu, y = EP2,
     main = "Pearson residuals scaled")
plot(x = mu, y = ED, main = "Deviance residuals")
par(op)

#_____________________

EP <- residuals(coll.glm.zip.h, type = "pearson")

EstPar <- coef(coll.glm.zipb.h,model = "zero")
Z <- model.matrix(coll.glm.zipb.h,model = "zero")
g <- Z %*% EstPar
p <- exp(g) / (1 + exp(g))

EstPar2 <- coef(coll.glm.zipb.h, model = "count")
X <- model.matrix(coll.glm.zipb.h, model = "count")
g <- X %*% EstPar2
mu1 <- exp(g)

mu <- (1 - p) * mu1

require(cplm)
summary(cpglm(coll ~ egk + speed + sin(2*pi*(hour/24)) + cos(2*pi*(hour/24)) + sin(4*pi*(hour/24)) + cos(4*pi*(hour/24)) + log(trains), data=model.data.h))

summary(glm.nb(coll ~ egk + speed + sin(2*pi*(hour/24)) + cos(2*pi*(hour/24)) + sin(4*pi*(hour/24)) + cos(4*pi*(hour/24)) + log(trains), data=model.data.h))


require(MASS)
require(vcd)
fit <- goodfit(model.data.h$coll) 
summary(fit) 
rootogram(fit)

Ord_plot(model.data.h$coll)

distplot(model.data.h$coll, type="poisson")
distplot(model.data.h$coll, type="nbinomial")

distplot(model.data.hm$coll, type="poisson")
distplot(model.data.hm$coll, type="nbinomial")

mod1 <- glm(coll ~ egk + speed + sin(2*pi*(hour/24)) + cos(2*pi*(hour/24)) + sin(4*pi*(hour/24)) + cos(4*pi*(hour/24)) + offset(log(trains)), data=model.data.h, family="poisson")
summary(mod1)
anova(mod1, test="Chisq")

require(VGAM)

fit <- vglm(coll ~ egk + speed + exp(-((hour - dawn)^2) / 12) + exp(-((hour - dusk)^2) / 12) + offset(log(trains)), genpoisson, data = model.data.hm, trace = TRUE)
coef(fit, matrix = TRUE)
summary(fit)

require(AER)
deviance(mod1)/mod1$df.residual
dispersiontest(mod1)

require(car)
influencePlot(mod1)

require(pscl)
mod2 <- zeroinfl(coll ~ egk + speed + sin(2*pi*(hour/24)) + cos(2*pi*(hour/24)) + sin(4*pi*(hour/24)) + cos(4*pi*(hour/24)) + offset(log(trains)), data=model.data.h, dist="poisson")
AIC(mod1, mod2)

res <- residuals(mod2, type="pearson")

pred <- predict(mod2)

plot(log(predict(mod2)), res)
abline(h=0, lty=2)
qqnorm(res)
qqline(res)

require(faraway)
halfnorm(residuals(mod2))

plot(res)


require(bayesm)
id=levels(as.factor(model.data.hm$id))
nresp<-length(unique(id))
lgtdata=NULL

for (i in 1:nresp)
{
  respdata=model.data.hm[id==id[i],]
  ty<-NULL
  tdesign<-NULL
  ty=respdata$coll
  nobs=length(ty)
  for (j in 1:nobs) {
    design<-as.matrix(respdata[j,.(egk,trains,speed,hour,dawn,dusk)])
    tdesign<-rbind(tdesign,design)
  }
  lgtdata[[i]]=list(y=ty,X=as.matrix(tdesign))
}

##
if(nchar(Sys.getenv("LONG_TEST")) != 0) {R=2000} else {R=10}
##
set.seed(123)
simnegbin =
  function(X, beta, alpha) {
    #   Simulate from the Negative Binomial Regression
    lambda = exp(X %*% beta)
    y=NULL
    for (j in 1:length(lambda))
      y = c(y,rnbinom(1,mu = lambda[j],size = alpha))
    return(y)
  }
nreg = 100        # Number of cross sectional units
T = 50            # Number of observations per unit
nobs = nreg*T
nvar=2            # Number of X variables
nz=2              # Number of Z variables
# Construct the Z matrix
Z = cbind(rep(1,nreg),rnorm(nreg,mean=1,sd=0.125))
Delta = cbind(c(4,2), c(0.1,-1))
alpha = 5
Vbeta = rbind(c(2,1),c(1,2))
# Construct the regdata (containing X)
simnegbindata = NULL
for (i in 1:nreg) {
  betai = as.vector(Z[i,]%*%Delta) + chol(Vbeta)%*%rnorm(nvar)
  X = cbind(rep(1,T),rnorm(T,mean=2,sd=0.25))
  simnegbindata[[i]] = list(y=simnegbin(X,betai,alpha), X=X,beta=betai)
}
Beta = NULL
for (i in 1:nreg) {Beta=rbind(Beta,matrix(simnegbindata[[i]]$beta,nrow=1))}
Data1 = list(regdata=simnegbindata, Z=Z)
Mcmc1 = list(R=R)
out = rhierNegbinRw(Data=Data1, Mcmc=Mcmc1)
cat("Summary of Delta draws",fill=TRUE)
summary(out$Deltadraw,tvalues=as.vector(Delta))
cat("Summary of Vbeta draws",fill=TRUE)
summary(out$Vbetadraw,tvalues=as.vector(Vbeta[upper.tri(Vbeta,diag=TRUE)]))
cat("Summary of alpha draws",fill=TRUE)
summary(out$alpha,tvalues=alpha)
if(0){
  ## plotting examples
  plot(out$betadraw)
  plot(out$alpha,tvalues=alpha)
  plot(out$Deltadraw,tvalues=as.vector(Delta))
}


##### Partition data for day/night and crep/non-crep and fit models!!!




#############JAGS#############
n <- nrow(model.data)
coll <- model.data$coll
egk <- model.data$egk
trains <- model.data$trains
speed <- model.data$speed

model.p <- function() {
  for (i in 1:n)
  {
    coll[i] ~ dpois(l[i])
    log(l[i]) <- log(trains[i]) + a + b[1] * egk[i] + b[2] * speed[i]
  }
  a ~ dnorm(0, 1)
  b[1] ~ dnorm(0, 1)
  b[2] ~ dnorm(0, 1)
}

jags.data <- list("n", "coll", "egk", "trains", "speed")

inits.p <- function() list(a=-5, b=c(1,1))

parameters.p <- c("a", "b")

set.seed(123)
out.p <- jags.parallel(data = jags.data, inits = inits.p, parameters.to.save = parameters.p, model.file = model.p, n.chains=3, n.iter=10000)
out.p
traceplot(out.p)

model.zip <- function() {
  for (i in 1:n)
  {
    coll[i] ~ dpois(mu[i])
    mu[i] <- (1-u[i])*l[i]
    u[i] ~ dbern(p[i])
    log(l[i]) <- log(trains[i]) + al + bl * speed[i]
    logit(p[i]) <- ap + bp * egk[i]
  }
  al ~ dnorm(0, 1)
  ap ~ dnorm(0, 1)  
  bl ~ dnorm(0, 1)
  bp ~ dnorm(0, 1)
}

jags.data <- list("n", "coll", "egk", "trains", "speed")

inits.zip <- function() list(al=-5,ap=-5,bl=1,bp=1)

parameters.zip <- c("al","ap","bl","bp")

set.seed(123)
out.zip <- jags.parallel(data = jags.data, inits = inits.zip, parameters.to.save = parameters.zip, model.file = model.zip, n.chains=3, n.iter=10000)
out.zip
traceplot(out.zip)


n <- nrow(model.data.h)
coll <- model.data.h$coll
egk <- model.data.h$egk
trains <- model.data.h$trains
speed <- model.data.h$speed
hour <- model.data.h$hour

model.zip.h <- function() {
  for (i in 1:n)
  {
    coll[i] ~ dpois(mu[i])
    mu[i] <- (1-u[i])*l[i]
    u[i] ~ dbern(p[i])
    log(l[i]) <- log(trains[i]) + al + bl1 * speed[i] + bl2 * hour[i]
    logit(p[i]) <- ap + bp * egk[i]
  }
  al ~ dnorm(0, 1)
  ap ~ dnorm(0, 1)  
  bl1 ~ dnorm(0, 1)
  bl2 ~ dnorm(0, 1)
  bp ~ dnorm(0, 1)
}

jags.data <- list("n", "coll", "egk", "trains", "speed", "hour")

inits.zip <- function() list(al=-5,ap=-5,bl1=1,bl2=1,bp=1)

parameters.zip <- c("al","ap","bl1","bl2","bp")

set.seed(123)
out.zip.h <- jags.parallel(data = jags.data, inits = inits.zip, parameters.to.save = parameters.zip, model.file = model.zip.h, n.chains=3, n.iter=10000)
out.zip.h
traceplot(out.zip.h)

model.nb.h <- function() {
  for (i in 1:n)
  {
    mu[i] <- a + b1 * egk[i] + b2 * speed[i] + b3 * sin(2*pi*(hour[i]/24)) + b4 * cos(2*pi*(hour[i]/24)) + b5 * sin(4*pi*(hour[i]/24)) +  b6 * cos(4*pi*(hour[i]/24)) + log(trains[i])
    lambda[i] <- exp(mu[i])
    Q[i] <- theta / (theta + lambda[i])
    coll[i] ~ dnegbin(Q[i],theta)
  }
  a ~ dnorm(0,1)
  b1 ~ dnorm(0,1)  
  b2 ~ dnorm(0,1)
  b3 ~ dnorm(0,1)
  b4 ~ dnorm(0,1)
  b5 ~ dnorm(0,1)
  b6 ~ dnorm(0,1)
  theta ~ dt(0,1,1);T(0,)
}

jags.data <- list("n", "coll", "egk", "trains", "speed", "hour", "pi")

inits.nb <- function() list(a=1,b1=1,b2=1,b3=1,b4=1,b5=1,b6=1,theta=0.5)

parameters.nb <- c("a","b1","b2","b3","b4","b5","b6","theta")

set.seed(123)
out.nb.h <- jags.parallel(data = jags.data, inits = inits.nb, parameters.to.save = parameters.nb, model.file = model.nb.h, n.chains=3, n.iter=10000)
out.nb.h
traceplot(out.nb.h)



#############STAN#############

n <- nrow(model.data)
coll <- model.data$coll
egk <- model.data$egk
trains <- model.data$trains
speed <- model.data$speed

scode <- "
data {
int<lower=0> n; 
vector[n] trains;
vector[n] egk;
vector[n] speed;
int<lower=0> coll[n];
}
transformed data {
vector[n] log_trains;
log_trains <- log(trains);
}
parameters {
real alpha; 
vector[2] beta;
} 
model {
coll ~ poisson_log(log_trains + alpha + beta[1] * egk + beta[2] * speed);
}
"
coll_model_fit <- stan(model_code = scode, iter = 100, chains = 1, cores = 1, seed=123, verbose = FALSE, control =list(stepsize=0.01, adapt_delta=0.9, max_treedepth=20))
precis(coll_model_fit, depth=2)

traceplot(As.mcmc.list(coll_model_fit,c("alpha","beta[1]","beta[2]")))





N <- nrow(model.data)
y <- model.data$coll
o <- log(model.data$trains)
x1 <- model.data$egk
x2 <- model.data$speed

scode <- "
data{
int<lower=1> N;
int<lower=0> y[N];
real o[N];
real x1[N];
real x2[N];
}
parameters{
real a;
real b1;
real b2;
}
model{
b2 ~ normal( 0 , 1 );
b1 ~ normal( 0 , 1 );
a ~ normal( 0 , 1 );
l <- o + a + b1 * x1 + b2 * x2);
y ~ poisson(l);
}
"
coll_model_fit <- stan(model_code = scode, init = function() list(a=0.5, b1=1, b2=1), iter = 10, chains = 3, cores = 3, seed=123, verbose = FALSE, control =list(stepsize=0.01, adapt_delta=0.9, max_treedepth=15))
precis(coll_model_fit)
traceplot(As.mcmc.list(coll_model_fit,c("a","b1","b2")))


#############Rethinking Package#############

set.seed(123) 
coll.model.map <- map(
  alist(
    coll ~ dpois(lambda),
    log(lambda) <- log(trains) + a + b1*egk + b2*speed,
    a ~ dnorm(0,1),
    b1 ~ dnorm(0,1),
    b2 ~ dnorm(0,1)
  ),
  #start=list(a=1,b1=1,b2=1,b3=1),
  data=list(coll=model.data$coll,egk=model.data$egk,trains=model.data$trains,speed=model.data$speed)
)
precis(coll.model.map)

set.seed(123) 
coll.model.map2 <- map(
  alist(
    y ~ dzipois(p, lambda), 
    logit(p) <- ap + bp*x1,
    log(lambda) <- al + bl*x2 + log(x3),
    ap ~ dnorm(0,1),
    al ~ dnorm(0,1),
    bp ~ dnorm(0,1),
    bl ~ dnorm(0,1)
  ),
  #start=list(a=1,b1=1,b2=1,b3=1),
  data=list(y=model.data$coll,x1=model.data$egk,x2=model.data$speed,x3=model.data$trains)
)
precis(coll.model.map2)

compare12 <- compare(coll.model.map, coll.model.map2)


set.seed(123) 
coll.model.map3 <- map(
  alist(
    y ~ dpois(lambda),
    log(lambda) <- log(x3) + a + b1*x1 + b2*x2,
    a ~ dnorm(0,1),
    b1 ~ dnorm(0,1),
    b2 ~ dnorm(0,1)
  ),
  #start=list(a=1,b1=1,b2=1,b3=1),
  data=list(y=model.data.h$coll,x1=model.data.h$egk,x2=model.data.h$speed,x3=model.data.h$trains)
)
precis(coll.model.map3)

set.seed(123) 
coll.model.map4 <- map(
  alist(
    y ~ dpois(lambda),
    log(lambda) <- log(x3) + a + b1*x1 + b2*x2 + b3*x4,
    a ~ dnorm(0,1),
    b1 ~ dnorm(0,1),
    b2 ~ dnorm(0,1),
    b3 ~ dnorm(0,1)
  ),
  #start=list(a=1,b1=1,b2=1,b3=1),
  data=list(y=model.data.h$coll,x1=model.data.h$egk,x2=model.data.h$speed,x3=model.data.h$trains,x4=model.data.h$hour)
)
precis(coll.model.map4)

set.seed(123) 
coll.model.map5 <- map(
  alist(
    y ~ dzipois(p, lambda), 
    logit(p) <- ap + bp*x1,
    log(lambda) <- al + bl*x2 + log(x3),
    ap ~ dnorm(0,1),
    al ~ dnorm(0,1),
    bp ~ dnorm(0,1),
    bl ~ dnorm(0,1)
  ),
  #start=list(a=1,b1=1,b2=1,b3=1),
  data=list(y=model.data.h$coll,x1=model.data.h$egk,x2=model.data.h$speed,x3=model.data.h$trains)
)
precis(coll.model.map5)

set.seed(123) 
coll.model.map6 <- map(
  alist(
    y ~ dzipois(p, lambda), 
    logit(p) <- ap + bp*x1,
    log(lambda) <- al + bl1*x2 + log(x3) + bl2*x4,
    ap ~ dnorm(0,1),
    al ~ dnorm(0,1),
    bp ~ dnorm(0,1),
    bl1 ~ dnorm(0,1),
    bl2 ~ dnorm(0,1)
  ),
  #start=list(a=1,b1=1,b2=1,b3=1),
  data=list(y=model.data.h$coll,x1=model.data.h$egk,x2=model.data.h$speed,x3=model.data.h$trains,x4=model.data.h$hour)
)
precis(coll.model.map6)

set.seed(123) 
coll.model.map7 <- map(
  alist(
    y ~ dzipois(p, lambda), 
    logit(p) <- ap + bp*x1,
    log(lambda) <- al + bl1*x2 + log(x3) + bl2*sin((x4-3)/12*2*pi),
    ap ~ dnorm(0,1),
    al ~ dnorm(0,1),
    bp ~ dnorm(0,1),
    bl1 ~ dnorm(0,1),
    bl2 ~ dnorm(0,1)
  ),
  #start=list(a=1,b1=1,b2=1,b3=1),
  data=list(y=model.data.h$coll,x1=model.data.h$egk,x2=model.data.h$speed,x3=model.data.h$trains,x4=model.data.h$hour)
)
precis(coll.model.map7)

# Mean StdDev   5.5% 94.5%
# ap   2.99   0.14   2.77  3.21
# al  -9.75   0.37 -10.35 -9.15
# bp  -3.88   0.40  -4.51 -3.24
# bl1  0.03   0.00   0.03  0.04
# bl2  0.95   0.10   0.80  1.10

compare34567 <- compare(coll.model.map3, coll.model.map4, coll.model.map5, coll.model.map6, coll.model.map7)

# WAIC pWAIC dWAIC weight     SE   dSE
# coll.model.map7 3943.1   5.3   0.0      1 174.48    NA
# coll.model.map6 4046.5   5.5 103.4      0 178.91 19.16
# coll.model.map5 4060.0   4.1 116.9      0 179.66 19.65
# coll.model.map4 4221.3   4.7 278.2      0 194.32 44.04
# coll.model.map3 4233.1   2.9 290.0      0 195.60 46.20

set.seed(123) 
coll.model.map8 <- map(
  alist(
    y ~ dzipois(p, lambda), 
    logit(p) <- ap + bp*x1,
    log(lambda) <- al + bl1*x2 + log(x3) + bl2*sin(2*pi*(x4/24)) + bl3*cos(2*pi*(x4/24)) + bl4*sin(4*pi*(x4/24)) + bl5*cos(4*pi*(x4/24)),
    ap ~ dnorm(0,1),
    al ~ dnorm(0,1),
    bp ~ dnorm(0,1),
    bl1 ~ dnorm(0,1),
    bl2 ~ dnorm(0,1),
    bl3 ~ dnorm(0,1),
    bl4 ~ dnorm(0,1),
    bl5 ~ dnorm(0,1)
  ),
  #start=list(a=1,b1=1,b2=1,b3=1),
  data=list(y=model.data.h$coll,x1=model.data.h$egk,x2=model.data.h$speed,x3=model.data.h$trains,x4=model.data.h$hour)
)
precis(coll.model.map8)

# Mean StdDev   5.5% 94.5%
# ap   2.85   0.14   2.63  3.08
# al  -9.48   0.37 -10.07 -8.89
# bp  -4.29   0.44  -4.98 -3.59
# bl1  0.03   0.00   0.02  0.04
# bl2  0.60   0.07   0.48  0.72
# bl3  1.15   0.12   0.96  1.34
# bl4 -0.09   0.10  -0.26  0.08
# bl5 -0.74   0.10  -0.90 -0.59

compare58 <- compare(coll.model.map5, coll.model.map8)



set.seed(123) 
coll.model.map9 <- map(
  alist(
    y ~ dzipois(p, lambda), 
    logit(p) <- ap + bp*x1,
    log(lambda) <- al + bl1*x2 + log(x3) + bl2*sin(2*pi*(x4/24)) + bl3*cos(2*pi*(x4/24)) + bl4*sin(4*pi*(x4/24)) + bl5*cos(4*pi*(x4/24)),
    ap ~ dnorm(0,1),
    al ~ dnorm(0,1),
    bp ~ dnorm(0,1),
    bl1 ~ dnorm(0,1),
    bl2 ~ dnorm(0,1),
    bl3 ~ dnorm(0,1),
    bl4 ~ dnorm(0,1),
    bl5 ~ dnorm(0,1)
  ),
  #start=list(a=1,b1=1,b2=1,b3=1),
  data=list(y=model.data.h$coll,x1=model.data.h$egk,x2=model.data.h$speed,x3=model.data.h$trains,x4=model.data.h$hour)
)
precis(coll.model.map9)


dzinbinom <- function (x, p, lambda, log = FALSE) 
{
  ll <- rep(0, length(x))
  p_i <- p[1]
  lambda_i <- lambda[1]
  for (i in 1:length(x)) {
    if (length(p) > 1) 
      p_i <- p[i]
    if (length(lambda) > 1) 
      lambda_i <- lambda[i]
    if (x[i] == 0) {
      ll[i] <- log_sum_exp(c(log(p_i), log(1 - p_i) + dnbinom(x[i], mu=lambda_i, TRUE)))
    }
    else {
      ll[i] <- log(1 - p_i) + dnbinom(x[i], mu=lambda_i, TRUE)
    }
  }
  if (log == FALSE) 
    ll <- exp(ll)
  return(ll)
}

dzinbinom <- function(x, mu, size, zprob) {
  ifelse(x == 0, zprob + (1 - zprob) * dnbinom(0, mu = mu, size = size), (1 - zprob) * dnbinom(x, mu = mu, size = size))
}

dzinbinom <- function (x, mu, scale, log = FALSE) 
{
  shape <- mu/scale
  prob <- 1/(1 + scale)
  dnbinom(x, size = shape, prob = prob, log = log)
}
<environment: namespace:rethinking>


N <- nrow(model.data.hm)  
y <- model.data.hm$coll
egk <- model.data.hm$egk
speed <- model.data.hm$speed
trains <- model.data.hm$trains
hour <- model.data.hm$hour
dawn <- model.data.hm$dawn
dusk <- model.data.hm$dusk
light <- model.data.hm$light
light2 <- model.data.hm$light2
dawnordusk <- model.data.hm$dawnordusk

x1 <- egk
x2 <- speed
x3 <- light
x4 <- light2
x5 <- dawnordusk
x6 <- log(trains)

set.seed(123) 
coll.model.map.nb.null <- map(
  alist(
    y ~ dgampois(mu, theta), 
    log(mu) <- a,
    a ~ dnorm(0,1),
    theta ~ dcauchy(0,5)
  ),
  start=list(a=1),
  data=list(y=y)
)
precis(coll.model.map.nb.null)
WAIC(coll.model.map.nb.null)

set.seed(123) 
coll.model.map.nb <- map(
  alist(
    y ~ dgampois(mu, theta), 
    log(mu) <- a + b1*x1 + b2*x2 + b3*x3 + b4*x4 + b5*x5 + x6,
    a ~ dnorm(0,1),
    b1 ~ dnorm(0,1),
    b2 ~ dnorm(0,1),
    b3 ~ dnorm(0,1),
    b4 ~ dnorm(0,1),
    b5 ~ dnorm(0,1),
    theta ~ dcauchy(0,5)
  ),
  start=list(a=1,b1=1,b2=1,b3=1,b4=1,b5=1),
  data=list(y=y,x1=x1,x2=x2,x3=x3,x4=x4,x5=x5,x6=x6)
)
precis(coll.model.map.nb)
WAIC(coll.model.map.nb)

compare(coll.model.map.nb.null,coll.model.map.nb)

set.seed(123) 
coll.model.map.zip <- map(
  alist(
    y ~ dzipois(p, lambda), 
    logit(p) <- ap + bp*x1,
    log(lambda) <- al + bl2*x2 + bl3*x3 + bl4*x4 + bl5*x5 + x6,
    ap ~ dnorm(0,1),
    bp ~ dnorm(0,1),
    al ~ dnorm(0,1),
    bl2 ~ dnorm(0,1),
    bl3 ~ dnorm(0,1),
    bl4 ~ dnorm(0,1),
    bl5 ~ dnorm(0,1)
  ),
  start=list(ap=1,bp=1,al=1,bl2=1,bl3=1,bl4=1,bl5=1),
  data=list(y=y,x1=x1,x2=x2,x3=x3,x4=x4,x5=x5,x6=x6)
)
precis(coll.model.map.zip)
WAIC(coll.model.map.zip)

compare.zip_nb <- compare(coll.model.map.nb.null,coll.model.map.nb,coll.model.map.zip)

#stancode(coll.model.map.nb)

n <- 3000
y <- sample(model.data.h$coll,n)
x1 <- sample(model.data.h$egk,n)
x2 <- sample(model.data.h$speed,n)
x3 <- sample(model.data.h$trains,n)
x4 <- sample(model.data.h$hour,n)

n <- nrow(model.data.h)  
y <- model.data.h$coll
x1 <- model.data.h$egk
x2 <- model.data.h$speed
x3 <- model.data.h$trains
x4 <- model.data.h$hour


scode <- "
data{
  int<lower=1> n;
  int<lower=0> y[n];
  real<lower=0> x1[n];
  real<lower=0> x2[n];
  real<lower=0> x3[n];
  int x4[n];
}
parameters{
  real ap;
  real al;
  real bp;
  real bl1;
  real bl2;
}
model{
  vector[n] lambda;
  vector[n] p;
  bl2 ~ normal( 0 , 1 );
  bl1 ~ normal( 0 , 1 );
  bp ~ normal( 0 , 1 );
  al ~ normal( 0 , 1 );
  ap ~ normal( 0 , 1 );
  for ( i in 1:n ) {
    lambda[i] <- al + bl1 * x2[i] + log(x3[i]) + bl2 * x4[i];
    lambda[i] <- exp(lambda[i]);
  }
  for ( i in 1:n ) {
    p[i] <- ap + bp * x1[i];
    p[i] <- inv_logit(p[i]);
  }
  for ( i in 1:n )
    if (y[i] == 0)
      increment_log_prob(log_sum_exp(bernoulli_log(1,p[i]),
                                     bernoulli_log(0,p[i]) + poisson_log(y[i],lambda[i])));
  else
    increment_log_prob(bernoulli_log(0,p[i]) + poisson_log(y[i],lambda[i]));
}

generated quantities{
  vector[n] lambda;
  vector[n] p;
  real dev;
  dev <- 0;
  for ( i in 1:n ) {
    lambda[i] <- al + bl1 * x2[i] + log(x3[i]) + bl2 * x4[i];
    lambda[i] <- exp(lambda[i]);
  }
  for ( i in 1:n ) {
    p[i] <- ap + bp * x1[i];
    p[i] <- inv_logit(p[i]);
  }
  for ( i in 1:n )
    if (y[i] == 0)
      dev <- dev + (-2)*(log_sum_exp(bernoulli_log(1,p[i]),
                                     bernoulli_log(0,p[i]) + poisson_log(y[i],lambda[i])));
  else
    dev <- dev + (-2)*(bernoulli_log(0,p[i]) + poisson_log(y[i],lambda[i]));
}
"
# generated quantities {
#   real pred[n];
#   real resid[n];
#   real log_lik[n];
#   for(i in 1:n) {
#     if (y[i]== 0) {
#       log_lik[i] <- log_sum_exp(bernoulli_log(1, p[i]),
#                                 bernoulli_log(0, p[i])
#                                 + poisson_log(y[i], lambda[i]));}
#     else {
#       log_lik[i] <- bernoulli_log(0, p[i]) + poisson_log(y[i], lambda[i]);
#     }
#     pred[i] <- (1-p[i]) * lambda[i];
#     resid[i] <- (y[i] - pred[i])/sqrt(pred[i]);
#   }
# }

coll_model_fit <- stan(model_code = scode, init = function() list(ap=-0.5, bp=0.5, al=-0.5, bl1=0.5, bl2=0.5), iter = 1000, chains = 3, cores = 3, seed=123, verbose = FALSE)#, control =list(stepsize=0.01, adapt_delta=0.9, max_treedepth=15))

#save(coll_model_fit, file = "coll_model_fit")
#load("coll_model_fit")

precis(coll_model_fit)

#        Mean StdDev lower 0.89 upper 0.89 n_eff Rhat
# ap     3.28   0.13       3.07       3.49   517 1.00
# al    -8.35   0.40      -8.97      -7.73   480 1.01
# bp    -3.74   0.37      -4.30      -3.10   493 1.00
# bl1    0.03   0.00       0.03       0.04   588 1.00
# bl2   -0.05   0.01      -0.07      -0.04   696 1.01
# dev 4040.40   7.86    4028.17    4052.34   606 1.00

WAIC(coll_model_fit)

traceplot(As.mcmc.list(coll_model_fit,c("ap","bp","al","bl1","bl2")))

post <- extract.samples(coll_model_fit)

egk <- mean(model.data.h$egk)
trains <- mean(model.data.h$trains)
speed <- mean(model.data.h$speed)

(1-invlogit(3.28-3.74*egk))*exp(log(trains)-8.35+0.03*speed-0.05*7)

curve((1-invlogit(3.28-3.74*egk))*exp(log(x)-8.35+0.03*speed-0.05*8),from=1,to=500,n=500)



scode.null <- "
data{
  int<lower=1> n;
int<lower=0> y[n];
}
parameters{
real ap;
real al;
}
model{
vector[n] lambda;
vector[n] p;
al ~ normal( 0 , 1 );
ap ~ normal( 0 , 1 );
for ( i in 1:n ) {
lambda[i] <- al;
lambda[i] <- exp(lambda[i]);
}
for ( i in 1:n ) {
p[i] <- ap;
p[i] <- inv_logit(p[i]);
}
for ( i in 1:n )
if (y[i] == 0)
increment_log_prob(log_sum_exp(bernoulli_log(1,p[i]),
bernoulli_log(0,p[i]) + poisson_log(y[i],lambda[i])));
else
increment_log_prob(bernoulli_log(0,p[i]) + poisson_log(y[i],lambda[i]));
}
generated quantities{
vector[n] lambda;
vector[n] p;
real dev;
dev <- 0;
for ( i in 1:n ) {
lambda[i] <- al;
lambda[i] <- exp(lambda[i]);
}
for ( i in 1:n ) {
p[i] <- ap;
p[i] <- inv_logit(p[i]);
}
for ( i in 1:n )
if (y[i] == 0)
dev <- dev + (-2)*(log_sum_exp(bernoulli_log(1,p[i]),
bernoulli_log(0,p[i]) + poisson_log(y[i],lambda[i])));
else
dev <- dev + (-2)*(bernoulli_log(0,p[i]) + poisson_log(y[i],lambda[i]));
}
"

coll_model_null <- stan(model_code = scode.null, init = function() list(ap=-0.5, al=-0.5), iter = 1000, chains = 3, cores = 3, seed=123, verbose = FALSE)#, control =list(stepsize=0.01, adapt_delta=0.9, max_treedepth=15))

precis(coll_model_null)

#         Mean StdDev lower 0.89 upper 0.89 n_eff Rhat
# ap     2.36   0.16       2.12       2.62   216    1
# al    -1.60   0.15      -1.86      -1.38   226    1
# dev 4093.47   1.97    4091.44    4095.83   304    1

traceplot(As.mcmc.list(coll_model_null,c("ap","al")))

compare_stan <- compare(coll_model_null,coll_model_fit)


scode2 <- "
data{
  int<lower=1> n;
int<lower=0> y[n];
real<lower=0> x1[n];
real<lower=0> x2[n];
real<lower=0> x3[n];
int x4[n];
}
parameters{
real ap;
real al;
real bp;
real bl1;
real bl2;
real<lower=0> kappa;
}
model{
vector[n] lambda;
vector[n] p;
kappa ~ normal( 0 , 5 );
bl2 ~ normal( 0 , 1 );
bl1 ~ normal( 0 , 1 );
bp ~ normal( 0 , 1 );
al ~ normal( 0 , 1 );
ap ~ normal( 0 , 1 );
for ( i in 1:n ) {
lambda[i] <- al + bl1 * x2[i] + log(x3[i]) + bl2 * x4[i];
lambda[i] <- exp(lambda[i]);
}
for ( i in 1:n ) {
p[i] <- ap + bp * x1[i];
p[i] <- inv_logit(p[i]);
}
for ( i in 1:n )
if (y[i] == 0)
increment_log_prob(log_sum_exp(bernoulli_log(1,p[i]), bernoulli_log(0,p[i]) + neg_binomial_log(y[i],lambda[i],kappa)));
else
increment_log_prob(bernoulli_log(0,p[i]) + neg_binomial_log(y[i],lambda[i],kappa));
}

generated quantities{
vector[n] lambda;
vector[n] p;
real dev;
dev <- 0;
for ( i in 1:n ) {
lambda[i] <- al + bl1 * x2[i] + log(x3[i]) + bl2 * x4[i];
lambda[i] <- exp(lambda[i]);
}
for ( i in 1:n ) {
p[i] <- ap + bp * x1[i];
p[i] <- inv_logit(p[i]);
}
for ( i in 1:n )
if (y[i] == 0)
dev <- dev + (-2)*(log_sum_exp(bernoulli_log(1,p[i]), bernoulli_log(0,p[i]) + neg_binomial_log(y[i],lambda[i],kappa)));
else
dev <- dev + (-2)*(bernoulli_log(0,p[i]) + neg_binomial_log(y[i],lambda[i],kappa));
}
"

coll_model_fit2 <- stan(model_code = scode2, init = function() list(ap=-0.5, bp=0.5, al=-0.5, bl1=0.5, bl2=0.5, kappa=100), iter = 1000, chains = 3, cores = 3, seed=123, verbose = FALSE)#, control =list(stepsize=0.01, adapt_delta=0.9, max_treedepth=15))

#save(coll_model_fit2, file = "coll_model_fit2")
#load("coll_model_fit2")

precis(coll_model_fit2)

#           Mean StdDev lower 0.89 upper 0.89 n_eff Rhat
# ap       3.11   0.15       2.86       3.33   656    1
# al      -6.45   0.43      -7.16      -5.79   700    1
# bp      -3.95   0.42      -4.63      -3.28   734    1
# bl1      0.04   0.00       0.03       0.04   995    1
# bl2     -0.05   0.01      -0.07      -0.03   961    1
# kappa   13.23   2.75       8.79      17.19   835    1
# dev   4015.41   7.12    4004.83    4026.32   835    1

traceplot(As.mcmc.list(coll_model_fit2,c("ap","bp","al","bl1","bl2","kappa")))


scode3 <- "
data{
int<lower=1> n;
int<lower=0> y[n];
real<lower=0> x1[n];
real<lower=0> x2[n];
real<lower=0> x3[n];
real<lower=0> x4[n];
}
parameters{
real ap;
real al;
real bp;
real bl1;
real bl2;
real bl3;
real bl4;
real bl5;
real<lower=0> kappa;
}
model{
vector[n] lambda;
vector[n] p;
kappa ~ normal( 0 , 5 );
bl5 ~ normal( 0 , 1 );
bl4 ~ normal( 0 , 1 );
bl3 ~ normal( 0 , 1 );
bl2 ~ normal( 0 , 1 );
bl1 ~ normal( 0 , 1 );
bp ~ normal( 0 , 1 );
al ~ normal( 0 , 1 );
ap ~ normal( 0 , 1 );
for ( i in 1:n ) {
lambda[i] <- al + bl1 * x2[i] + log(x3[i]) + bl2 * sin(2*pi()*(x4[i]/24)) + bl3 * cos(2*pi()*(x4[i]/24)) + bl4 * sin(4*pi()*(x4[i]/24)) + bl5 * cos(4*pi()*(x4[i]/24));
lambda[i] <- exp(lambda[i]);
}
for ( i in 1:n ) {
p[i] <- ap + bp * x1[i];
p[i] <- inv_logit(p[i]);
}
for ( i in 1:n )
if (y[i] == 0)
increment_log_prob(log_sum_exp(bernoulli_log(1,p[i]), bernoulli_log(0,p[i]) + neg_binomial_log(y[i],lambda[i],kappa)));
else
increment_log_prob(bernoulli_log(0,p[i]) + neg_binomial_log(y[i],lambda[i],kappa));
}

generated quantities{
vector[n] lambda;
vector[n] p;
real dev;
dev <- 0;
for ( i in 1:n ) {
lambda[i] <- al + bl1 * x2[i] + log(x3[i]) + bl2 * sin(2*pi()*(x4[i]/24)) + bl3 * cos(2*pi()*(x4[i]/24)) + bl4 * sin(4*pi()*(x4[i]/24)) + bl5 * cos(4*pi()*(x4[i]/24));
lambda[i] <- exp(lambda[i]);
}
for ( i in 1:n ) {
p[i] <- ap + bp * x1[i];
p[i] <- inv_logit(p[i]);
}
for ( i in 1:n )
if (y[i] == 0)
dev <- dev + (-2)*(log_sum_exp(bernoulli_log(1,p[i]), bernoulli_log(0,p[i]) + neg_binomial_log(y[i],lambda[i],kappa)));
else
dev <- dev + (-2)*(bernoulli_log(0,p[i]) + neg_binomial_log(y[i],lambda[i],kappa));
}
"

coll_model_fit3 <- stan(model_code = scode3, init = function() list(alpha=-0.5, beta_egk=0.5, omega=-0.5, beta_speed=0.5, beta_hour1=0.5, beta_hour2=0.5, beta_hour3=0.5, beta_hour4=0.5, kappa=100), iter = 1000, chains = 3, cores = 3, seed=123, verbose = FALSE)#, control =list(stepsize=0.01, adapt_delta=0.9, max_treedepth=15))


n <- nrow(model.data.h)  
coll <- model.data.h$coll
egk <- model.data.h$egk
speed <- model.data.h$speed
trains <- model.data.h$trains
hour <- model.data.h$hour

mean(coll)

sd(coll)

tab<-table(coll)
zero.prop<-as.numeric(tab[1])/sum(as.numeric(tab))


k <- 3
mu <- 10
lambda <- rgamma(1000, shape = k, scale = mu/k)
z <- rpois(1000, lambda)
P1 <- table(factor(z, levels = 0:max(z)))/1000
plot(P1)
P2 <- dnbinom(0:max(z), mu = 10, size = 3)
points(0:max(z), P2)

u1 <- runif(1000)
z <- ifelse(u1 < 0.3, rnorm(1000, mean = 1, sd = 2),rnorm(1000, mean = 5, sd = 1))
hist(z, breaks = 100, freq = FALSE)

curve(0.3 * dnorm(x, mean = 1, sd = 2) + 0.7 * dnorm(x, mean = 5, sd = 1), add = TRUE, lwd = 2)

dzinbinom = function(x, mu, size, zprob) {
  ifelse(x == 0, zprob + (1 - zprob) * dnbinom(0, mu = mu, size = size), (1 - zprob) * dnbinom(x, mu = mu, size = size))
}

rzinbinom = function(n, mu, size, zprob) {
  ifelse(runif(n) < zprob, 0, rnbinom(n, mu = mu, size = size))
}

set.seed(123)
n <- 1000
egk <- rnbinom(n,10,.3)
egk <- egk/max(egk)

trains <- rpois(n, 2)+1
trains <- trains*10

speed <- rnorm(n,90,2)
  
hour <- as.integer(runif(n,0,23))

alpha <- .005
omega <- .005
beta_egk <- .005
beta_speed <- .005
beta_hour1 <- .005
beta_hour2 <- .005
beta_hour3 <- .005
beta_hour4 <- .005
kappa <- .1

p <- alpha + beta_egk*egk
p <- invlogit(p)

l <- omega + beta_speed * speed + log(trains) + beta_hour1 * sin(2*pi*(hour/24)) + beta_hour2 * cos(2*pi*(hour/24)) + beta_hour3 * sin(4*pi*(hour/24)) + beta_hour4 * cos(4*pi*(hour/24))
l <- exp(l)

temp1 <- rbinom(n,1, prob = p)
temp2 <- rnbinom(n, size = kappa, mu = l)
coll <- temp1 * temp2 # outcome

coll <- ifelse(l == 0, p + (1 - p) * dnbinom(0, mu = l, size = kappa), (1 - p) * dnbinom(1, mu = l, size = kappa))
# 
# set.seed(75)
# N <- 1000
# phi <- 4
# b0 <- 1.5
# b1 <- 2.5
# x <- runif(N) # nb regressor
# z <- runif(N) # logit regressor
# temp1 <- rbinom(N,1, prob = z)
# temp2 <- rnbinom(N, size = phi, mu = exp(b0 + x * b1))
# y <- temp1 * temp2 # outcome


scode3 <- "
data{
int<lower=1> n;
int<lower=0> coll[n];
real<lower=0> egk[n];
real<lower=0> speed[n];
real<lower=0> trains[n];
real<lower=0> hour[n];
}
parameters{
real ap;
real al;
real bp;
real bl1;
real bl2;
real bl3;
real bl4;
real bl5;
real<lower=0> kappa;
}
model{
kappa ~ cauchy( 0 , 5 );
bl5 ~ normal( 0 , 1 );
bl4 ~ normal( 0 , 1 );
bl3 ~ normal( 0 , 1 );
bl2 ~ normal( 0 , 1 );
bl1 ~ normal( 0 , 1 );
bp ~ normal( 0 , 1 );
al ~ normal( 0 , 1 );
ap ~ normal( 0 , 1 );
for ( i in 1:n ) {
if (coll[i] == 0)
increment_log_prob(log_sum_exp(bernoulli_logit_log(1,ap + bp * egk[i]), bernoulli_logit_log(0,ap + bp * egk[i]) + neg_binomial_2_log_log(coll[i],al + bl1 * speed[i] + log(trains[i]) + bl2 * sin(2*pi()*(hour[i]/24)) + bl3 * cos(2*pi()*(hour[i]/24)) + bl4 * sin(4*pi()*(hour[i]/24)) + bl5 * cos(4*pi()*(hour[i]/24)),kappa)));
else
increment_log_prob(bernoulli_logit_log(0,ap + bp * egk[i]) + neg_binomial_2_log_log(coll[i],al + bl1 * speed[i] + log(trains[i]) + bl2 * sin(2*pi()*(hour[i]/24)) + bl3 * cos(2*pi()*(hour[i]/24)) + bl4 * sin(4*pi()*(hour[i]/24)) + bl5 * cos(4*pi()*(hour[i]/24)),kappa));
}
}

generated quantities{
real dev;
dev <- 0;
for ( i in 1:n ) {
if (coll[i] == 0)
dev <- dev + (-2)*(log_sum_exp(bernoulli_logit_log(1,ap + bp * egk[i]), bernoulli_logit_log(0,ap + bp * egk[i]) + neg_binomial_2_log_log(coll[i],al + bl1 * speed[i] + log(trains[i]) + bl2 * sin(2*pi()*(hour[i]/24)) + bl3 * cos(2*pi()*(hour[i]/24)) + bl4 * sin(4*pi()*(hour[i]/24)) + bl5 * cos(4*pi()*(hour[i]/24)),kappa)));
else
dev <- dev + (-2)*(bernoulli_logit_log(0,ap + bp * egk[i]) + neg_binomial_2_log_log(coll[i],al + bl1 * speed[i] + log(trains[i]) + bl2 * sin(2*pi()*(hour[i]/24)) + bl3 * cos(2*pi()*(hour[i]/24)) + bl4 * sin(4*pi()*(hour[i]/24)) + bl5 * cos(4*pi()*(hour[i]/24)),kappa));
}
}
"

coll_model_fit3 <- stan(model_code = scode3, iter = 1500, chains = 3, cores = 3, seed=123, verbose = FALSE)#, control =list(stepsize=0.01, adapt_delta=0.9, max_treedepth=15))

precis(coll_model_fit3)

n <- nrow(model.data.h)  
y <- model.data.h$coll
x1 <- model.data.h$egk
x2 <- model.data.h$speed
x3 <- model.data.h$trains
x4 <- model.data.h$hour

scode4 <- "
data{
  int<lower=1> n;
  int<lower=0> y[n];
  real<lower=0> x1[n];
  real<lower=0> x2[n];
  real<lower=0> x3[n];
  real<lower=0> x4[n];
}
parameters{
  real a;
  real b0;
  real b1;
  real b2;
  real b3;
  real b4;
  real b5;
  real<lower=0> phi;
}
model{
  vector[n] mu;
  phi ~ cauchy( 0 , 5 );
  b5 ~ normal( 0 , 1 );
  b4 ~ normal( 0 , 1 );
  b3 ~ normal( 0 , 1 );
  b2 ~ normal( 0 , 1 );
  b1 ~ normal( 0 , 1 );
  b0 ~ normal( 0 , 1 );
  a ~ normal( 0 , 1 );
  for ( i in 1:n ) {
    mu[i] <- a + b0 * x1[i] + b1 * x2[i] + log(x3[i]) + b2 * sin(2 * pi() * (x4[i]/24)) + b3 * cos(2 * pi() * (x4[i]/24)) + b4 * sin(4 * pi() * (x4[i]/24)) +      b5 * cos(4 * pi() * (x4[i]/24));
    mu[i] <- exp(mu[i]);
  }
  y ~ neg_binomial_2( mu , phi );
}
generated quantities {
  vector[n] y_rep;
  for (i in 1:n) {
    y_rep[i] <- neg_binomial_2_rng(mu[i],phi);
  }
}
"
# generated quantities{
#   vector[n] mu;
#   vector[n] log_lik;
#   real dev;
#   dev <- 0;
#   for ( i in 1:n ) {
#     mu[i] <- a + b0 * x1[i] + b1 * x2[i] + log(x3[i]) + b2 * sin(2 * pi() * (x4[i]/24)) + b3 * cos(2 * pi() * (x4[i]/24)) + b4 * sin(4 * pi() * (x4[i]/24)) +      b5 * cos(4 * pi() * (x4[i]/24));
#     mu[i] <- exp(mu[i]);
#   }
#   for ( i in 1:n ) {
#     log_lik[i] <- neg_binomial_2_log( y , mu , theta );
#   }
#   dev <- dev + (-2)*neg_binomial_2_log( y , mu , theta );
# }

scode4a <- "
data {
  int<lower=1> n;
  int<lower=0> y[n];
  vector[n] x1;
  vector[n] x2;
  vector[n] x3;
  vector[n] x4;
}
transformed data {
  vector[n] x4a;
  vector[n] x4b;
  vector[n] x4c;
  vector[n] x4d;
  vector[n] x3l;
  for ( i in 1:n ) {
    x4a[i] <- sin(2 * pi() * (x4[i]/24));
    x4b[i] <- cos(2 * pi() * (x4[i]/24));
    x4c[i] <- sin(4 * pi() * (x4[i]/24));
    x4d[i] <- cos(4 * pi() * (x4[i]/24));
  }
  x3l <- log(x3);
}
parameters {
  vector[7] beta;
  vector[n] lambda;
  real<lower=0> tau;
} 
transformed parameters {
  real<lower=0> sigma;
  sigma <- 1.0 / sqrt(tau);
}
model {
  tau ~ gamma(0.001, 0.001);
  for (i in 1:n) {
    lambda[i] ~ normal(0, sigma);
    y[i] ~ poisson_log(lambda[i] + x3l[i] + beta[1] + beta[2] * x1[i] + beta[3] * x2[i]
                        + beta[4] * x4a + beta[5] * x4b + beta[6] * x4c + beta[7] * x4d);
  }
}
"

scode4 <- "
data{
  int<lower=1> n;
  int<lower=0> y[n];
  vector[n] x1;
  vector[n] x2;
  vector[n] x3;
  vector[n] x4;
}
transformed data{
  vector[n] x4a;
  vector[n] x4b;
  vector[n] x4c;
  vector[n] x4d;
  vector[n] x3l;
  for ( i in 1:n ) {
    x4a[i] <- sin(2 * pi() * (x4[i]/24));
    x4b[i] <- cos(2 * pi() * (x4[i]/24));
    x4c[i] <- sin(4 * pi() * (x4[i]/24));
    x4d[i] <- cos(4 * pi() * (x4[i]/24));
  }
  x3l <- log(x3);
}
parameters{
  real a;
  real b0;
  real b1;
  real b2;
  real b3;
  real b4;
  real b5;
  real<lower=0> theta;
}
model{
  vector[n] mu;
  theta ~ cauchy( 0 , 5 );
  b5 ~ normal( 0 , 1 );
  b4 ~ normal( 0 , 1 );
  b3 ~ normal( 0 , 1 );
  b2 ~ normal( 0 , 1 );
  b1 ~ normal( 0 , 1 );
  b0 ~ normal( 0 , 1 );
  a ~ normal( 0 , 1 );
  mu <- a + b0 * x1 + b1 * x2 + x3l + b2 * x4a + b3 * x4b + b4 * x4c + b5 * x4d;
  mu <- exp(mu);
  y ~ neg_binomial_2(mu,theta);
}
"

coll_model_fit4 <- stan(model_code = scode4, iter = 500, chains = 3, cores = 3, seed=123, verbose = FALSE)#, control =list(stepsize=0.01, adapt_delta=0.9, max_treedepth=15))

traceplot(As.mcmc.list(coll_model_fit4,c("a","b0","b1","b2","b3","b4","b5","theta")))

launch_shinystan(coll_model_fit4)

stan_glm1 <- stan_glm(coll ~ egk + speed + sin(2*pi*(hour/24)) + cos(2*pi*(hour/24)) + sin(4*pi*(hour/24)) + cos(4*pi*(hour/24)), offset = log(trains),
                      data = model.data.h, family = poisson, 
                      prior = normal(0,2.5), prior_intercept = normal(0,5),
                      chains = 3, cores = 3, seed = 123)

prop_zero <- function(y) mean(y == 0)
(prop_zero_test1 <- pp_check(stan_glm1, check = "test", test = "prop_zero"))

stan_glm2 <- update(stan_glm1, family = neg_binomial_2)

yrep <- posterior_predict(stan_glm2)

summary(stan_glm2)

newdata <- data.frame(egk = 0.5, trains = 30, speed = 90, hour = 8)

i <- 8
curve(exp(coef(stan_glm2)[1]+coef(stan_glm2)[2]*x+coef(stan_glm2)[3]*mean(model.data.h$speed)+coef(stan_glm2)[4]*sin((2*pi*i)/24)+coef(stan_glm2)[5]*cos((2*pi*i)/24)+coef(stan_glm2)[6]*sin((4*pi*i)/24)+coef(stan_glm2)[7]*cos((4*pi*i)/24)),from=0,to=1,n=100, xlab = "EGK Likelihood", ylab = "Change in Rate of Collisions")

occ <- NULL
for (i in 1:24) {
  temp_df <- data.frame(x=seq(0,1,.01), y=exp(coef(stan_glm2)[1]+coef(stan_glm2)[2]*x+coef(stan_glm2)[3]*mean(model.data.h$speed)+coef(stan_glm2)[4]*sin((2*pi*(i-1))/24)+coef(stan_glm2)[5]*cos((2*pi*(i-1))/24)+coef(stan_glm2)[6]*sin((4*pi*(i-1))/24)+coef(stan_glm2)[7]*cos((4*pi*(i-1))/24)), hour=rep(i-1, each=101))
  occ <- rbind(occ,temp_df)
  rm(temp_df)
}  

occ$crep <- 0

occ$crep[occ$hour >= 5 & occ$hour <= 9 | occ$hour >= 17 & occ$hour <= 21] <- 1

ggplot(occ,aes(x=x,y=y,group=hour,colour=factor(hour))) +
  geom_line(size=1.5) +
  ylab("Rate of Collisions\n") +
  xlab("\nLikelihood of Species Occurrence") +
  labs(color = "Hour") +
  theme_bw() +
  theme(legend.key = element_blank()) +
  #scale_colour_manual(values=heat.colors(24, alpha = 1)) +
  theme(text = element_text(size = 20)) +
  scale_x_continuous(breaks=seq(0,1,by=.1), expand = c(0, 0), lim=c(0,1)) #+
  #scale_y_continuous(breaks=seq(0,.7,by=.1), expand = c(0, 0), lim=c(0,.7)) #+
#guides(colour=FALSE)

ggplot(occ,aes(x=x,y=y,group=crep)) +
  geom_line(colour=c("grey70"),size=.75) +
  geom_point(size=2.5) +
  ylab("Moran's I") +
  xlab("Distance (km)") +
  labs(shape = "Species") +
  theme_bw() +
  theme(legend.key = element_blank()) +
  #scale_colour_manual(values=plotPal) +
  scale_shape_manual(values=hour) +
  geom_hline(aes(yintercept=0), linetype=2) +
  #scale_x_continuous(breaks=seq(1, 20, 1))



fun <- as.list(NULL)
for (i in 1:24){
  assign(paste0("fun",i),function(x) exp(coef(stan_glm2)[1]+coef(stan_glm2)[2]*x+coef(stan_glm2)[3]*mean(model.data.h$speed)+coef(stan_glm2)[4]*sin((2*pi*(i-1))/24)+coef(stan_glm2)[5]*cos((2*pi*(i-1))/24)+coef(stan_glm2)[6]*sin((4*pi*(i-1))/24)+coef(stan_glm2)[7]*cos((4*pi*(i-1))/24)))
}
x <- seq(0,1,.01)
matplot(x,cbind(fun1(x),fun8(x)),type="l",col=heat.colors(24, alpha = 1))


preds <- posterior_predict(stan_glm2,newdata)

(prop_zero_test2 <- pp_check(stan_glm2, check = "test", test = "prop_zero"))

loo1 <- loo(stan_glm1)
loo2 <- loo(stan_glm2)

par(mfrow = 1:2, mar = c(5,3.8,1,0) + 0.1, las = 3)
plot(stan_glm1, label_points = TRUE)
plot(stan_glm2, label_points = TRUE)

compare(loo1, loo2)

launch_shinystan(stan_glm2)

coll_model_fit4a <- map2stan(coll.model.map10)

#save(coll_model_fit4, file = "coll_model_fit4")
#load("coll_model_fit4")

# precis(coll_model_fit3) - first dataset
# 
#         Mean StdDev lower 0.89 upper 0.89 n_eff Rhat
# ap       2.68   0.16       2.43       2.91   911 1.01
# al      -7.48   0.43      -8.14      -6.78   823 1.00
# bp      -4.67   0.51      -5.50      -3.87  1011 1.00
# bl1      0.03   0.00       0.03       0.04  1227 1.00
# bl2      0.60   0.07       0.49       0.72  1275 1.00
# bl3      1.14   0.12       0.96       1.33  1245 1.00
# bl4     -0.10   0.11      -0.26       0.07  1002 1.00
# bl5     -0.74   0.10      -0.89      -0.57  1275 1.00
# kappa   14.06   2.95       9.32      18.59   985 1.00
# dev   3779.79   8.93    3766.37    3794.59  1057 1.00

# precis(coll_model_fit3) - second dataset
# 
#         Mean StdDev lower 0.89 upper 0.89 n_eff Rhat
# ap       2.24   0.19       1.93       2.55  1100 1.00
# al      -5.30   0.43      -5.98      -4.61   894 1.01
# bp      -4.08   0.56      -4.96      -3.20  1206 1.00
# bl1      0.03   0.00       0.03       0.04  1500 1.00
# bl2      0.20   0.07       0.09       0.31  1500 1.00
# bl3      0.54   0.11       0.37       0.72  1040 1.00
# bl4     -0.50   0.10      -0.66      -0.33  1160 1.00
# bl5     -0.91   0.10      -1.05      -0.74  1227 1.00
# kappa   11.77   2.62       7.52      15.57   978 1.00
# dev   3758.82   7.41    3747.19    3769.71  1075 1.00

# precis(coll_model_fit4)
#           Mean StdDev lower 0.89 upper 0.89 n_eff Rhat
# a       -9.44   0.35      -9.92      -8.81   577    1
# b0       3.11   0.39       2.51       3.72   623    1
# b1       0.03   0.00       0.02       0.03   659    1
# b2       0.23   0.08       0.11       0.35   650    1
# b3       0.59   0.12       0.40       0.80   640    1
# b4      -0.49   0.10      -0.66      -0.34   652    1
# b5      -0.95   0.10      -1.10      -0.78   672    1
# theta    0.14   0.02       0.11       0.18   478    1
# dev   3769.50   8.37    3756.55    3782.39   533    1

# precis(coll_model_fit4a)






curve(invlogit(2.68-4.67*x),from=0,to=1,n=100, xlab = "EGK Likelihood", ylab = "Likelihood of False Zero")
curve(invlogit(2.24-4.08*x),from=0,to=1,n=100, xlab = "EGK Likelihood", ylab = "Likelihood of False Zero")
curve(invlogit(-9.44+3.11*x),from=0,to=1,n=100, xlab = "EGK Likelihood", ylab = "Change in Rate of Collisions")

curve(0.60 * sin(2*pi*(x/24)) + 1.14 * cos(2*pi*(x/24)) - 0.10 * sin(4*pi*(x/24)) - 0.74 * cos(4*pi*(x/24)),from=0,to=23,n=24, ylab = "Influence on Collision Rate", xlab = "Hour")
curve(0.20 * sin(2*pi*(x/24)) + 0.54 * cos(2*pi*(x/24)) - 0.50 * sin(4*pi*(x/24)) - 0.91 * cos(4*pi*(x/24)),from=0,to=23,n=24, ylab = "Influence on Collision Rate", xlab = "Hour")
curve(0.23 * sin(2*pi*(x/24)) + 0.59 * cos(2*pi*(x/24)) - 0.49 * sin(4*pi*(x/24)) - 0.95 * cos(4*pi*(x/24)),from=0,to=23,n=24, ylab = "Influence on Collision Rate", xlab = "Hour")


a <- 0.23
b <- 0.59
c <- -0.49
d <- -0.95

a <- 1
b <- 1
c <- 1
d <- 1

x <- 6

curve(a * sin(2*pi*(x/24)) + b * cos(2*pi*(x/24)) + c * sin(4*pi*(x/24)) + d * cos(4*pi*(x/24)),from=0,to=23,n=24, ylab = "Influence on Collision Rate", xlab = "Hour")

a * sin(2*pi*(x/24)) + b * cos(2*pi*(x/24)) + c * sin(4*pi*(x/24)) + d * cos(4*pi*(x/24))

max <- -(pi*(2*d*sin(pi*x/6)-2*c*cos(pi*x/6)+b*sin(pi*x/12)-a*cos(pi*x/12))/12)

fun1 <- function(x) sin(2*pi*(x/24))
fun2 <- function(x) cos(2*pi*(x/24))
fun3 <- function(x) sin(4*pi*(x/24))
fun4 <- function(x) cos(4*pi*(x/24))
x <- seq(0,23,1)
matplot(x,cbind(fun1(x),fun2(x),fun3(x),fun4(x)),type="l",col=c("blue","red","green","purple"))


curve(1*sin(2*pi*(x/24)) + cos(2*pi*(x/24)) + 2*sin(4*pi*(x/24)) - 1* cos(4*pi*(x/24)),from=0,to=23,n=24)

curve(sin(2*pi*(x/24)) + cos(2*pi*(x/24)) + sin(4*pi*(x/24)) + cos(4*pi*(x/24)),from=0,to=23,n=24)

e <- 1

x2 <- (24*(x/24)^e)

plot(cbind(x,x-x2))

curve(sin(2*pi*((24*(x/24)^e)/24)) + cos(2*pi*((24*(x/24)^e)/24)) + sin(4*pi*((24*(x/24)^e)/24)) + cos(4*pi*((24*(x/24)^e)/24)),from=0,to=23,n=24)

sin(2*pi*((24*(x/24)^e)/24)) + cos(2*pi*((24*(x/24)^e)/24)) + sin(4*pi*((24*(x/24)^e)/24)) + cos(4*pi*((24*(x/24)^e)/24))

curve(sin(2*pi*(x/24)^e) + cos(2*pi*(x/24)^e) + sin(4*pi*(x/24)^e) + cos(4*pi*(x/24)^e),from=0,to=23,n=24)

sin(2*pi*(x/24)^e) + cos(2*pi*(x/24)^e) + sin(4*pi*(x/24)^e) + cos(4*pi*(x/24)^e)

curve(sin(2*pi*(x/24)) + cos(2*pi*(x/24)) + sin(4*pi*(x/24)) + cos(4*pi*(x/24)),from=0,to=23,n=24)

m <- 1
v <- 2

curve(m*(2*floor(v*x)-floor(2*v*x)+1),from=0,to=23,n=24)


log_lik1 <- extract_log_lik(coll_model_fit4)
loo1 <- loo(log_lik1)
print(loo1)

require(coda)
coll_model_fit3.coda<-mcmc.list(lapply(1:ncol(coll_model_fit3),function(x) mcmc(as.array(coll_model_fit3)[,x,])))
plot(coll_model_fit3.coda)

coll_model_fit4.coda<-mcmc.list(lapply(1:ncol(coll_model_fit4),function(x) mcmc(as.array(coll_model_fit4)[,x,])))
plot(coll_model_fit4.coda)

x4 <- as.numeric(x4)

x4a <- NULL
x4b <- NULL
x4c <- NULL
x4d <- NULL

for ( i in 1:n ) {
  x4a[i] <- sin(2 * pi * (x4[i]/24))
  x4b[i] <- cos(2 * pi * (x4[i]/24))
  x4c[i] <- sin(4 * pi * (x4[i]/24))
  x4d[i] <- cos(4 * pi * (x4[i]/24))
}

x <- seq(0, 23, length=24)
x <- seq(0, 1, length=100)
y <- function(x, m1, m2, w1, w2, a1, a2) {
  out <- a1 * exp(-((x - m1) ** 2) / w1) +
    a2 * exp(-((x - m2) ** 2) / w2)
  out
}

plot(y(x, 6, 19, 1, 1, 2, 2) ~ x, type='l', las=1, bty='l', xlab="x", ylab="f(x)", cex.lab=1.25)

n <- nrow(model.data.hm)  
y <- model.data.hm$coll
x1 <- model.data.hm$egk
x2 <- model.data.hm$speed
x3 <- model.data.hm$trains
x4 <- model.data.hm$hour
x5 <- model.data.hm$dawn
x6 <- model.data.hm$dusk

scode5 <- "
  data{
    int<lower=1> n;
    int<lower=0> y[n];
    real<lower=0> x1[n];
    real<lower=0> x2[n];
    real<lower=0> x3[n];
    real<lower=0> x4[n];
    real<lower=0> x5[n];
    real<lower=0> x6[n];
  }
  parameters{
    real a;
    real b0;
    real b1;
    real b2;
    real b3;
    real<lower=0> phi;
  }
  model{
    vector[n] mu;
    phi ~ cauchy( 0 , 5 );
    b3 ~ normal( 0 , 1 );
    b2 ~ normal( 0 , 1 );
    b1 ~ normal( 0 , 1 );
    b0 ~ normal( 0 , 1 );
    a ~ normal( 0 , 1 );
    for ( i in 1:n ) {
    mu[i] <- a + b0 * x1[i] + b1 * x2[i] + log(x3[i]) + b2 * exp(-((x4[i] - x5[i])^2) / 12) + b3 * exp(-((x4[i] - x6[i])^2) / 12);
    mu[i] <- exp(mu[i]);
  }
  y ~ neg_binomial_2( mu , phi );
  }
"
#   generated quantities {
#     vector[n] y_rep;
#     for (i in 1:n) {
#       y_rep[i] <- neg_binomial_2_rng(mu[i],phi);
#     }
#   }
# "

coll_model_fit5 <- stan(model_code = scode5, iter = 100, chains = 3, cores = 3, seed=123, verbose = FALSE)#, control =list(stepsize=0.01, adapt_delta=0.9, max_treedepth=15))


N <- nrow(model.data.hm)  
y <- model.data.hm$coll
egk <- model.data.hm$egk
speed <- model.data.hm$speed
trains <- model.data.hm$trains
hour <- model.data.hm$hour
dawn <- model.data.hm$dawn
dusk <- model.data.hm$dusk

x1 <- egk
x2 <- speed
x3 <- log(trains)
x4 <- exp(-((hour - dawn)^2) / 12) + exp(-((hour - dusk)^2) / 12)

X <- as.matrix(cbind(x1,x2,x3,x4))
K <- ncol(X)


N <- 10000
y <- sample(model.data.hm$coll, N)
egk <- sample(model.data.hm$egk, N)
speed <- sample(model.data.hm$speed, N)
trains <- sample(model.data.hm$trains, N)
hour <- sample(model.data.hm$hour, N)
dawn <- sample(model.data.hm$dawn, N)
dusk <- sample(model.data.hm$dusk, N)

x1 <- egk
x2 <- speed
x3 <- log(trains)
x4 <- exp(-((hour - dawn)^2) / 12) + exp(-((hour - dusk)^2) / 12)

X <- as.matrix(cbind(x1,x2,x3,x4))
K <- ncol(X)


N <- 1000 #sample size
dat <- data.frame(x1=runif(N,.01,.92),x2=runif(N,5,148),x3=runif(N,-6.5,4.2),x4=runif(N,.02,.99))
#the model
X <- model.matrix(~x1 + x2 + x3 + x4,dat)
K <- dim(X)[2] #number of regression params
#the regression slopes
betas <- runif(K,-1,1)
#the overdispersion for the simulated data
phi <- 5
#simulate the response
y_nb <- rnbinom(1000,size=phi,mu=exp(X%*%betas))

#fit the model
m_nb <- stan(model_code = scode6,data = list(N=N,K=K,X=X,y=y_nb),pars=c("beta","phi"), chains=3, cores=3)

#diagnose and explore the model using shinystan
launch_shinystan(m_nb)

scode6 <- "
data {
  int N; //the number of observations
  int K; //the number of columns in the model matrix
  int y[N]; //the response
  matrix[N,K] X; //the model matrix
}
parameters {
  vector[K] beta; //the regression parameters
  real phi; //the overdispersion parameters
}
transformed parameters {
  vector[N] mu;//the linear predictor
  mu <- exp(X*beta); //using the log link 
}
model {  
  beta[1] ~ cauchy(0,10); //prior for the intercept following Gelman 2008
  
  for(i in 2:K)
    beta[i] ~ cauchy(0,2.5);//prior for the slopes following Gelman 2008
  
  y ~ neg_binomial_2(mu,phi);
}
"
# generated quantities {
#   vector[N] y_rep;
#   for(n in 1:N){
#     y_rep[n] <- neg_binomial_2_rng(mu[n],phi); //posterior draws to get posterior predictive checks
#   }
# }
# "

coll_model_fit6 <- stan(model_code = scode6, iter = 100, chains = 1, cores = 1, seed=123, verbose = FALSE)#, control =list(stepsize=0.01, adapt_delta=0.9, max_treedepth=15))

stan_glm.nb(formula=coll ~ egk + speed + exp(-((hour - dawn)^2) / 12) + exp(-((hour - dusk)^2) / 12) + log(trains), model.data.hm)

coll ~ egk + speed + exp(-((hour - dawn)^2) / 12) + exp(-((hour - dusk)^2) / 12) + offset(log(trains))


##############INLA#############
require(INLA)

data = list(
  coll=model.data.hm$coll,
  egk=model.data.hm$egk,
  speed=model.data.hm$speed,
  light=model.data.hm$light,
  light2=model.data.hm$light2,
  dawnordusk=model.data.hm$dawnordusk,
  trains=log(model.data.hm$trains)
  )
formula = coll ~ egk + speed + light + light2 + dawnordusk + offset(trains)
coll.inla = inla(formula, family = "nbinomial", data = data)
summary(coll.inla)
