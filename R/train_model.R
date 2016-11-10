require(data.table)
require(pscl)
require(R2jags)
require(rstan)
require(rstanarm)
require(spatstat)
require(rethinking)
require(INLA)
require(vcd)
require(MASS)
require(loo)
require(dplyr)
require(arm)
require(doMC)
require(boot)

model.data <- as.data.table(read.delim("data/model_data_hm.csv", header=T, sep=","))
#model.data.hex <- as.data.table(read.delim("data/model_data_hm_hex.csv", header=T, sep=","))

model.data.bin <- copy(model.data)
model.data.bin[coll>1,coll:=1]
#write.csv(model.data.bin, "data/model_data_hm_bin.csv", row.names=FALSE)
rm(model.data)

summary(model.data.bin)
#summary(model.data.hex)

model.data$speed100 <- model.data$speed/100
model.data$offset <- log(model.data$trains*model.data$length)

#model.data.hex$speed100 <- model.data.hex$speed/100
#model.data.hex$offset <- log(model.data.hex$trains*model.data.hex$length)

cor(model.data[,.(egk,trains,speed100,light,light2,dawnordusk)])

#cor(model.data.hex[,.(egk,trains,speed100,light,light2,dawnordusk)])

hist(model.data$coll)
hist(model.data$egk)
hist(log(model.data$trains*model.data$length))
hist(model.data$speed100)
hist(model.data$light)
hist(model.data$light2)
hist(model.data$dawnordusk)

model.data[coll>=1,.N]
model.data[coll==0,.N]
mean(model.data$coll)
var(model.data$coll)
length(unique(model.data[,id]))

distplot(model.data$coll, type="poisson")
distplot(model.data$coll, type="nbinomial")

std_sd_half <- function (x) (x-mean(x))/sd(x)*0.5

write.csv(model.data.bin, "data/model_data_hm_bin.csv", row.names=FALSE)

model.data.hex.bin <- copy(model.data.hex)
model.data.hex.bin[coll>1,coll:=1]

model.data.cs <- model.data
model.data.cs[coll>1,coll:=1]
model.data.cs <- as.data.frame(model.data.cs) %>% mutate_each_(funs(std_sd_half),vars=c("egk","speed","light","light2","dawnordusk","trains"))

model.data.cs$egk <- (model.data.cs$egk-mean(model.data.cs$egk))/sd(model.data.cs$egk)*0.5
model.data.cs$speed <- (model.data.cs$speed-mean(model.data.cs$speed))/sd(model.data.cs$speed)*0.5
model.data.cs$light <- (model.data.cs$light-mean(model.data.cs$light))/sd(model.data.cs$light)*0.5
model.data.cs$light2 <- (model.data.cs$light2-mean(model.data.cs$light2))/sd(model.data.cs$light2)*0.5
model.data.cs$dawnordusk <- (model.data.cs$dawnordusk-mean(model.data.cs$dawnordusk))/sd(model.data.cs$dawnordusk)*0.5
model.data.cs$trains <- (model.data.cs$trains-mean(model.data.cs$trains))/sd(model.data.cs$trains)*0.5



#############Sample Data#############
set.seed(123)

n <- 1000

s.egk <- rpois(n,1)
s.egk <- s.egk/max(s.egk)
s.speed100 <- rnorm(n,0.86,.19)
s.light <- runif(n,-1,1)
s.light2 <- runif(n,0,1)
s.dawnordusk <- runif(n,-1,1)
s.log_trains <- log(runif(n,0,70))

b1 <- 1.9
b2 <- 3.9
b3 <- -0.7
b4 <- -1.8
b5 <- 0.3

#POIS Parameters
a <- -9.0

#NB Parameters
a <- -9.0
k <- 0.2

#ZIP Parameters
al <- 0.5
ap <- 0.5

#POIS
mu <- exp(a + b1*s.egk + b2*s.speed100 + b3*s.light + b4*s.light2 + b5*s.dawnordusk + s.log_trains)

s.coll.pois <- rpois(n = n, lambda = mu)
hist(s.coll.pois)
distplot(s.coll.pois, type="poisson")
distplot(s.coll.pois, type="nbinomial")


#NB
mu <- exp(a + b1*s.egk + b2*s.speed100 + b3*s.light + b4*s.light2 + b5*s.dawnordusk + s.log_trains)

s.coll.nb <- rnegbin(n = n, mu = mu, theta = k)
hist(s.coll.nb)

#ZIP
p <- inv_logit(ap + b1*s.egk)
l <- exp(al + b2*s.speed100 + b3*s.light + b4*s.light2 + b5*s.dawnordusk + s.log_trains)

occ <- rbinom( n , 1 , p )

s.coll.zip <- (1-occ)*rpois( n , l )
hist(s.coll.zip)

#HUR
p <- inv_logit(ap + b1*s.egk)
l <- exp(al + b2*s.speed100 + b3*s.light + b4*s.light2 + b5*s.dawnordusk + s.log_trains)

occ <- rbinom( n , 1 , p )
count <- rpois( n , l )

s.coll.hur <- c(occ[occ==0],sample(count[count>0],n-length(occ[occ==0])))

hist(s.coll.hur)


#############GLM#############
# model.data2 <- model.data
# model.data2[coll==2,coll:=1]
# coll.glm.bin <- glm(formula = coll ~ egk + speed100 + light + light2 + dawnordusk, family = binomial(link = "logit"), data = model.data2, offset = log_trains)
# summary(coll.glm.bin)
# paste("% Error Explained: ",round(((coll.glm.bin$null.deviance - coll.glm.bin$deviance)/coll.glm.bin$null.deviance)*100,2),sep="")


coll.glm.pois <- glm(formula = coll ~ egk + speed100 + light + light2 + dawnordusk, family = poisson(link = "log"), data = model.data, offset = trains*length)
summary(coll.glm.pois)
paste("% Error Explained: ",round(((coll.glm.pois$null.deviance - coll.glm.pois$deviance)/coll.glm.pois$null.deviance)*100,2),sep="")  #Report reduction in deviance
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -11.57312    0.31532 -36.703  < 2e-16 ***
#   egk           1.85627    0.25720   7.217 5.31e-13 ***
#   speed100      3.84155    0.30062  12.779  < 2e-16 ***
#   light        -0.54297    0.10320  -5.261 1.43e-07 ***
#   light2       -1.70586    0.17068  -9.995  < 2e-16 ***
#   dawnordusk    0.10922    0.06374   1.713   0.0866 .  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for poisson family taken to be 1)
# 
# Null deviance: 5543.7  on 291119  degrees of freedom
# Residual deviance: 5147.8  on 291114  degrees of freedom
# AIC: 5972

coll.glm.nb <- glm.nb(formula = coll ~ egk + speed100 + light + light2 + dawnordusk + offset(log_trains), data = model.data)
summary(coll.glm.nb)
paste("% Error Explained: ",round(((coll.glm.nb$null.deviance - coll.glm.nb$deviance)/coll.glm.nb$null.deviance)*100,2),sep="")  #Report reduction in deviance

coll.glm.zip <- zeroinfl(formula = coll ~ speed100 + light + light2 + dawnordusk | egk, offset=log_trains, data = model.data, dist = "poisson", start=list(count=c(1,1,1,1,1),zero=c(1,1)))
summary(coll.glm.zip)
# Count model coefficients (poisson with log link):
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -8.22558    0.41503 -19.819   <2e-16 ***
#   speed100     4.04258    0.33029  12.240   <2e-16 ***
#   light       -0.62157    0.11076  -5.612    2e-08 ***
#   light2      -1.77713    0.17910  -9.923   <2e-16 ***
#   dawnordusk   0.17671    0.07081   2.496   0.0126 *  
#   
#   Zero-inflation model coefficients (binomial with logit link):
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)   3.4315     0.2762  12.426  < 2e-16 ***
#   egk          -1.9682     0.2883  -6.827 8.65e-12 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
# 
# Number of iterations in BFGS optimization: 47 
# Log-likelihood: -2962 on 7 Df

coll.glm.hur <- hurdle(formula = coll ~ speed100 + light + light2 + dawnordusk | egk, offset=log_trains, data = model.data, dist = "poisson", start=list(count=c(1,1,1,1,1),zero=c(1,1)))
summary(coll.glm.hur)


coll.glm.zinb <- zeroinfl(coll ~ speed100 + light + light2 + dawnordusk | egk, offset=log_trains, data = model.data, dist = "negbin", start=list(count=c(1,1,1,1,1),zero=c(1,1)))
summary(coll.glm.zinb)


vuong(coll.glm.zip, coll.glm.zinb)


#############Real Data#############
n <- nrow(model.data)
coll <- model.data$coll
egk <- model.data$egk
speed100 <- model.data$speed100
light <- model.data$light
light2 <- model.data$light2
dawnordusk <- model.data$dawnordusk
log_trains <- model.data$log_trains

n <- nrow(model.data.hex)
coll <- model.data.hex$coll
egk <- model.data.hex$egk
speed100 <- model.data.hex$speed100
light <- model.data.hex$light
light2 <- model.data.hex$light2
dawnordusk <- model.data.hex$dawnordusk
log_trains <- model.data.hex$log_trains

n <- nrow(model.data) + nrow(model.data)
coll <- c(model.data$coll,rep(NA,length(model.data$coll)))
egk <- c(model.data$egk,model.data$egk)
speed100 <- c(model.data$speed100,model.data$speed100)
light <- c(model.data$light,model.data$light)
light2 <- c(model.data$light2,model.data$light2)
dawnordusk <- c(model.data$dawnordusk,model.data$dawnordusk)
log_trains <- c(model.data$log_trains,model.data$log_trains)


#############JAGS#############
jags.model.pois <- function() {
  for (i in 1:n)
  {
    coll[i] ~ dpois(l[i])
    log(l[i]) <- a + b[1]*egk[i] + b[2]*speed100[i] + b[3]*light[i] + b[4]*light2[i] + b[5]*dawnordusk[i] + log_trains[i]
  }
  a ~ dnorm(0, 1)
  b[1] ~ dnorm(0, 1)
  b[2] ~ dnorm(0, 1)
  b[3] ~ dnorm(0, 1)
  b[4] ~ dnorm(0, 1)
  b[5] ~ dnorm(0, 1)
}

jags.data <- list("n", "coll", "egk", "speed100", "light", "light2", "dawnordusk", "log_trains")

inits.pois <- function() list(a=0.5, b=c(0,0,0,0,0))

parameters.pois <- c("a", "b")

set.seed(123)
coll.jags.pois <- jags.parallel(data = jags.data, inits = inits.pois, parameters.to.save = parameters.pois, model.file = jags.model.pois, n.chains=3, n.iter=100)
coll.jags.pois
traceplot(coll.jags.pois)


jags.model.zip <- function() {
  for (i in 1:n)
  {
    coll[i] ~ dpois(mu[i])
    mu[i] <- (1-u[i])*l[i]
    u[i] ~ dbern(p[i])
    logit(p[i]) <- ap + bp*egk[i]
    log(l[i]) <- al + bl[1]*speed100[i] + bl[2]*light[i] + bl[3]*light2[i] + bl[4]*dawnordusk[i] + log_trains[i]
  }
  ap ~ dnorm(0, 1)  
  bp ~ dnorm(0, 1)
  al ~ dnorm(0, 1)
  bl[1] ~ dnorm(0, 1)
  bl[2] ~ dnorm(0, 1)
  bl[3] ~ dnorm(0, 1)
  bl[4] ~ dnorm(0, 1)
}

jags.data <- list("n", "coll", "egk", "speed100", "light", "light2", "dawnordusk", "log_trains")

inits.zip <- function() list(ap=1,bp=1,al=1,bl=c(1,1,1,1))

parameters.zip <- c("ap","bp","al","bl")

set.seed(123)
coll.jags.zip <- jags.parallel(data = jags.data, inits = inits.zip, parameters.to.save = parameters.zip, model.file = jags.model.zip, n.chains=3, n.iter=100)
coll.jags.zip
traceplot(coll.jags.zip)


jags.model.nb <- function() {
  for (i in 1:n)
  {
    mu[i] <- a + b[1]*egk[i] + b[2]*speed100[i] + b[3]*light[i] + b[4]*light2[i] + b[5]*dawnordusk[i] + log_trains[i]
    lambda[i] <- exp(mu[i])
    Q[i] <- theta / (theta + lambda[i])
    coll[i] ~ dnegbin(Q[i],theta)
  }
  a ~ dnorm(0,1)
  b[1] ~ dnorm(0,1)  
  b[2] ~ dnorm(0,1)
  b[3] ~ dnorm(0,1)
  b[4] ~ dnorm(0,1)
  b[5] ~ dnorm(0,1)
  #theta ~ dt(0,1,1);T(0,)
  theta ~ dt(0,1,1);T(0,)
}

jags.data <- list("n", "coll", "egk", "speed100", "light", "light2", "dawnordusk", "log_trains")

inits.nb <- function() list(a=1,b=c(1,1,1,1,1),theta=0.5)

parameters.nb <- c("a","b","theta")

set.seed(123)
coll.jags.nb <- jags.parallel(data = jags.data, inits = inits.nb, parameters.to.save = parameters.nb, model.file = jags.model.nb, n.chains=1, n.iter=50)
coll.jags.nb
traceplot(coll.jags.nb)


#############MAP#############

set.seed(123) 
coll.map.nb.null <- map(
  alist(
    y ~ dgampois(mu, theta), 
    log(mu) <- a,
    a ~ dnorm(0,1),
    theta ~ dcauchy(0,5)
  ),
  start=list(a=1),
  data=list(y=coll)
)
precis(coll.map.nb.null)
#       Mean StdDev  5.5% 94.5%
# a     -4.09   0.02 -4.12 -4.07
# theta  0.17   0.01  0.15  0.18
WAIC(coll.map.nb.null)
# [1] 48800.97
# attr(,"lppd")
# [1] -24398.29
# attr(,"pWAIC")
# [1] 2.197219
# attr(,"se")
# [1] 602.9264

set.seed(123) 
coll.map.nb <- map(
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
  start=list(a=1,b1=1,b2=1,b3=1,b4=1,b5=1,theta=100),
  data=list(y=coll,x1=egk,x2=speed100,x3=light,x4=light2,x5=dawnordusk,x6=log_trains)
)
precis(coll.map.nb)
#           Mean StdDev  5.5% 94.5%
# a       -4.80   0.25 -5.20 -4.40
# b1       1.54   0.25  1.14  1.94
# b2       3.10   0.27  2.67  3.53
# b3      -0.57   0.10 -0.73 -0.41
# b4      -1.63   0.17 -1.89 -1.36
# b5       0.11   0.06  0.01  0.21
# theta 3298.73    NaN   NaN   NaN
WAIC(coll.map.nb)
# [1] 46038.04
# attr(,"lppd")
# [1] -23012.24
# attr(,"pWAIC")
# [1] 6.7741
# attr(,"se")
# [1] 590.9393

compare(coll.map.nb.null,coll.map.nb)

set.seed(123) 
coll.map.zip <- map(
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
  data=list(y=coll,x1=egk,x2=speed100,x3=light,x4=light2,x5=dawnordusk,x6=log_trains)
)
precis(coll.map.zip)
#      Mean StdDev  5.5% 94.5%
# ap   1.80   0.04  1.73  1.87
# bp  -2.37   0.12 -2.57 -2.17
# al  -7.13   0.11 -7.31 -6.96
# bl2  0.04   0.00  0.04  0.04
# bl3 -0.70   0.03 -0.75 -0.64
# bl4 -1.75   0.05 -1.84 -1.67
# bl5  0.25   0.02  0.21  0.28
WAIC(coll.map.zip)
# [1] 45896.11
# attr(,"lppd")
# [1] -22940.63
# attr(,"pWAIC")
# [1] 7.424752
# attr(,"se")
# [1] 592.028

compare.zip_nb <- compare(coll.map.nb.null,coll.map.nb,coll.map.zip)
plot(compare.zip_nb)

##############INLA#############
n <- nrow(model.data)
data = list(
  coll=model.data$coll,
  egk=model.data$egk,
  speed100=model.data$speed100,
  light=model.data$light,
  light2=model.data$light2,
  dawnordusk=model.data$dawnordusk,
  log_trains=model.data$log_trains
  # coll=c(model.data$coll,rep(NA,n)),
  # egk=c(model.data$egk,model.data$egk),
  # speed100=c(model.data$speed100,model.data$speed100),
  # light=c(model.data$light,model.data$light),
  # light2=c(model.data$light2,model.data$light2),
  # dawnordusk=c(model.data$dawnordusk,model.data$dawnordusk),
  # log_trains=c(model.data$log_trains,model.data$log_trains)
)
link = rep(NA, n)
formula = coll ~ egk + speed100 + light + light2 + dawnordusk
coll.inla = inla(formula, offset=log_trains, family = "poisson", data = data, control.fixed=list(mean=0, prec=1, mean.intercept=0.5, prec.intercept=1), control.predictor = list(link = link))
summary(coll.inla)
# Time used:
#   Pre-processing    Running inla Post-processing           Total 
#           0.6637       2379.1865         11.3013       2391.1515 (40 min)
# 
# Fixed effects:
#                 mean     sd 0.025quant 0.5quant 0.975quant     mode kld
# (Intercept) -10.3495 0.2623   -10.8711 -10.3472    -9.8405 -10.3428   0
# egk           1.4280 0.2475     0.9342   1.4307     1.9066   1.4361   0
# speed100      2.6881 0.2596     2.1812   2.6871     3.1999   2.6852   0
# light        -0.5826 0.1018    -0.7826  -0.5826    -0.3832  -0.5824   0
# light2       -1.6749 0.1671    -2.0080  -1.6731    -1.3515  -1.6697   0
# dawnordusk    0.1319 0.0625     0.0097   0.1318     0.2551   0.1314   0
# 
# The model has no random effects
# 
# The model has no hyperparameters
# 
# Expected number of effective parameters(std dev): 5.767(0.00)
# Number of equivalent replicates : 50478.80 
# 
# Marginal log-Likelihood:  -3066.04 


plot(coll.inla$summary.fitted.values$mean)


plot(coll.inla$marginals.fixed$egk, type="l")
plot(coll.inla$marginals.fixed$speed100, type="l")
plot(coll.inla$marginals.fixed$light, type="l")
plot(coll.inla$marginals.fixed$light2, type="l")
plot(coll.inla$marginals.fixed$dawnordusk, type="l")

#coll.inla.res <- (model.data$coll - coll.inla$summary.fitted.values$mean) / coll.inla$summary.fitted.values$sd
#plot(coll.inla.res)

count.egk <- inla.tmarginal(function(x) exp(x), coll.inla$marginals.fixed$egk)

inla.emarginal(exp, coll.inla$marginals.fixed$egk)

sum(coll.inla$cpo$cpo)
-mean(log(coll.inla$cpo$cpo))
coll.inla$cpo$failure

plot(coll.inla)


#############STAN#############

#Simulated Data:
y_pois <- s.coll.pois
y_nb <- s.coll.nb
y_zip <- s.coll.zip
y_hur <- s.coll.hur
x1 <- s.egk
x2 <- s.speed100
x3 <- s.light
x4 <- s.light2
x5 <- s.dawnordusk
x6 <- s.log_trains

#Real Data:
n <- nrow(model.data)
y_pois <- model.data$coll
y_nb <- model.data$coll
y_zip <- model.data$coll
x1 <- model.data$egk
x2 <- model.data$speed100
x3 <- model.data$light
x4 <- model.data$light2
x5 <- model.data$dawnordusk
x6 <- log(model.data$trains*model.data$length)

#Real Data (hex):
n <- nrow(model.data.hex)
y_nb <- model.data.hex$coll
y_zip <- model.data.hex$coll
x1 <- model.data.hex$egk
x2 <- model.data.hex$speed100
x3 <- model.data.hex$light
x4 <- model.data.hex$light2
x5 <- model.data.hex$dawnordusk
x6 <- model.data.hex$log_trains

scode.pois <- "
data{
int<lower=1> n;
int<lower=0> y_pois[n];
real x1[n];
real x2[n];
real x3[n];
real x4[n];
real x5[n];
real x6[n];
}
parameters{
real a;
real b1;
real b2;
real b3;
real b4;
real b5;
}
transformed parameters {
vector[n] mu;
for(i in 1:n)  mu[i] = exp(a + b1*x1[i] + b2*x2[i] + b3*x3[i] + b4*x4[i] + b5*x5[i] + x6[i]);
}
model{
b5 ~ normal( 0 , 1 );
b4 ~ normal( 0 , 1 );
b3 ~ normal( 0 , 1 );
b2 ~ normal( 0 , 1 );
b1 ~ normal( 0 , 1 );
a ~ normal( 0 , 1 );
y_pois ~ poisson( mu );
}
"
# generated quantities {
# vector[n] log_lik;
# for(i in 1:n) log_lik[i] <- poisson_rng(mu[i]);
# }
# "

coll.stan.pois <- stan(model_code = scode.pois, iter = 500, warmup = 250, chains = 3, cores = 3, seed=123)#, control =list(stepsize=0.01, adapt_delta=0.9, max_treedepth=15)) 2594.92 seconds (Total)

print(coll.stan.pois, pars=c("a","b1","b2","b3","b4","b5"))

# Inference for Stan model: 695d3a217b7e6ea94ed65c4ee0c0ec54.
# 3 chains, each with iter=500; warmup=250; thin=1; 
# post-warmup draws per chain=250, total post-warmup draws=750.
# 
#           mean se_mean   sd      2.5%       25%       50%       75%     97.5% n_eff Rhat
# a        -9.01    0.01 0.11     -9.21     -9.09     -9.01     -8.94     -8.80   305 1.00
# b1        1.90    0.00 0.09      1.72      1.84      1.90      1.96      2.07   750 1.00
# b2        3.96    0.01 0.11      3.74      3.89      3.97      4.04      4.16   406 1.00
# b3       -0.71    0.00 0.04     -0.78     -0.74     -0.71     -0.69     -0.64   612 1.00
# b4       -1.84    0.00 0.06     -1.96     -1.88     -1.84     -1.80     -1.72   589 1.00
# b5        0.26    0.00 0.02      0.22      0.24      0.26      0.27      0.30   677 1.00
# k         0.14    0.00 0.01      0.12      0.13      0.14      0.14      0.15   750 1.00
# lp__ -22612.49    0.11 1.95 -22616.94 -22613.60 -22612.10 -22611.05 -22609.73   332 1.01

traceplot(coll.stan.pois, pars=c("a","b1","b2","b3","b4","b5"))

plot(coll.stan.pois, pars=c("a","b1","b2","b3","b4","b5"))

pairs(coll.stan.pois, pars=c("a","b1","b2","b3","b4","b5"))

scode.nb <- "
data{
  int<lower=1> n;
  int<lower=0> y_nb[n];
  real x1[n];
  real x2[n];
  real x3[n];
  real x4[n];
  real x5[n];
  real x6[n];
}
parameters{
  real a;
  real b1;
  real b2;
  real b3;
  real b4;
  real b5;
  real<lower=0> k;
}
transformed parameters {
  vector[n] mu;
  for(i in 1:n)  mu[i] <- exp(a + b1*x1[i] + b2*x2[i] + b3*x3[i] + b4*x4[i] + b5*x5[i] + x6[i]);
}
model{
  k ~ cauchy( 0 , 5 );
  b5 ~ normal( 0 , 1 );
  b4 ~ normal( 0 , 1 );
  b3 ~ normal( 0 , 1 );
  b2 ~ normal( 0 , 1 );
  b1 ~ normal( 0 , 1 );
  a ~ normal( 0 , 1 );
  y_nb ~ neg_binomial_2( mu , k );
}
generated quantities {
  vector[n] log_lik;
  for(i in 1:n) log_lik[i] <- neg_binomial_2_rng(mu[i],k);
}
"

coll.stan.nb <- stan(model_code = scode.nb, iter = 1000, warmup = 250, chains = 1, cores = 1, seed=123)#, control =list(stepsize=0.01, adapt_delta=0.9, max_treedepth=15)) 2594.92 seconds (Total)

print(coll.stan.nb, pars=c("a","b1","b2","b3","b4","b5","k"))

# Inference for Stan model: 695d3a217b7e6ea94ed65c4ee0c0ec54.
# 3 chains, each with iter=500; warmup=250; thin=1; 
# post-warmup draws per chain=250, total post-warmup draws=750.
# 
#           mean se_mean   sd      2.5%       25%       50%       75%     97.5% n_eff Rhat
# a        -9.01    0.01 0.11     -9.21     -9.09     -9.01     -8.94     -8.80   305 1.00
# b1        1.90    0.00 0.09      1.72      1.84      1.90      1.96      2.07   750 1.00
# b2        3.96    0.01 0.11      3.74      3.89      3.97      4.04      4.16   406 1.00
# b3       -0.71    0.00 0.04     -0.78     -0.74     -0.71     -0.69     -0.64   612 1.00
# b4       -1.84    0.00 0.06     -1.96     -1.88     -1.84     -1.80     -1.72   589 1.00
# b5        0.26    0.00 0.02      0.22      0.24      0.26      0.27      0.30   677 1.00
# k         0.14    0.00 0.01      0.12      0.13      0.14      0.14      0.15   750 1.00
# lp__ -22612.49    0.11 1.95 -22616.94 -22613.60 -22612.10 -22611.05 -22609.73   332 1.01

traceplot(coll.stan.nb, pars=c("a","b1","b2","b3","b4","b5","k"))

plot(coll.stan.nb, pars=c("a","b1","b2","b3","b4","b5","k"))

pairs(coll.stan.nb, pars=c("a","b1","b2","b3","b4","b5","k"))

plot(coll.stan.nb, show_density = TRUE, ci_level = 0.5, fill_color = "purple", pars=c("a","b1","b2","b3","b4","b5","k"))
plot(coll.stan.nb, plotfun = "hist", pars=c("a","b1","b2","b3","b4","b5","k"))
plot(coll.stan.nb, plotfun = "trace", pars=c("a","b1","b2","b3","b4","b5","k"), inc_warmup = TRUE)
plot(coll.stan.nb, plotfun = "rhat") + ggtitle("Example of adding title to plot")
plot(coll.stan.nb, pars=c("a","b1","b2","b3","b4","b5","k"), plotfun="stan_hist")

#post <- extract(coll.stan.nb, permuted=TRUE)
post <- extract.samples(coll.stan.nb)

mu.link <- function(egk) post$a + post$b1*egk + post$b2*

save(coll.stan.nb, file="data/coll_stan_nb")

scode.zip <- "
data{
int<lower=1> n;
int<lower=0> y_zip[n];
real x1[n];
real x2[n];
real x3[n];
real x4[n];
real x5[n];
real x6[n];
}
parameters{
real al;
real ap;
real b1;
real b2;
real b3;
real b4;
real b5;
}
model{
vector[n] mu;
vector[n] p;
b5 ~ normal( 0 , 1 );
b4 ~ normal( 0 , 1 );
b3 ~ normal( 0 , 1 );
b2 ~ normal( 0 , 1 );
b1 ~ normal( 0 , 1 );
ap ~ normal( 0 , 1 );
al ~ normal( 0 , 1 );
for ( i in 1:n ) {
  mu[i] <- al + b2*x2[i] + b3 *x3[i] + b4*x4[i] + b5*x5[i] + x6[i];
  mu[i] <- exp(mu[i]);
}
for ( i in 1:n ) {
  p[i] <- ap + b1*x1[i];
  p[i] <- inv_logit(p[i]);
}
  for ( i in 1:n )
    if (y_zip[i] == 0)
      increment_log_prob(log_sum_exp(bernoulli_log(1,p[i]), bernoulli_log(0,p[i]) + poisson_log(y_zip[i],mu[i])));
  else
    increment_log_prob(bernoulli_log(0,p[i]) + poisson_log(y_zip[i],mu[i]));
}
"

coll.stan.zip <- stan(model_code = scode.zip, iter = 500, chains = 3, cores = 3, seed=123)#, control =list(stepsize=0.01, adapt_delta=0.9, max_treedepth=15)) 4375.02 seconds (Total)

coll.stan.zip

traceplot(coll.stan.zip)

save(coll.stan.zip, file="data/coll_stan_zip")



scode.hur <- "
data{
int<lower=1> n;
int<lower=0> y_hur[n];
real x1[n];
real x2[n];
real x3[n];
real x4[n];
real x5[n];
real x6[n];
}
parameters{
real al;
real ap;
real b1;
real b2;
real b3;
real b4;
real b5;
}
model{
vector[n] mu;
vector[n] p;
b5 ~ normal( 0 , 1 );
b4 ~ normal( 0 , 1 );
b3 ~ normal( 0 , 1 );
b2 ~ normal( 0 , 1 );
b1 ~ normal( 0 , 1 );
ap ~ normal( 0 , 1 );
al ~ normal( 0 , 1 );
for ( i in 1:n ) {
mu[i] <- al + b2*x2[i] + b3 *x3[i] + b4*x4[i] + b5*x5[i] + x6[i];
mu[i] <- exp(mu[i]);
}
for ( i in 1:n ) {
p[i] <- ap + b1*x1[i];
p[i] <- inv_logit(p[i]);
}
for ( i in 1:n ) {
(y_hur[i] == 0) ~ bernoulli(p[i]);
if (y_hur[i] > 0)
 y_hur[i] ~ poisson(mu[i]);
}
}
"

coll.stan.hur <- stan(model_code = scode.hur, iter = 500, chains = 3, cores = 3, seed=123)#, control =list(stepsize=0.01, adapt_delta=0.9, max_treedepth=15))# 4375.02 seconds (Total)

coll.stan.hur

traceplot(coll.stan.hur)



n <- nrow(model.data.bin)
y <- model.data.bin$coll
egk <- model.data.bin$egk
speed <- model.data.bin$speed100
light <- model.data.bin$light
light2 <- model.data.bin$light2
dawnordusk <- model.data.bin$dawnordusk
offset <- model.data.bin$offset

scode.bin <- "
data{
  int<lower=1> n;
  int<lower=0,upper=1> y[n];
  vector[n] egk;
  vector[n] speed;
  vector[n] light;
  vector[n] light2;
  vector[n] dawnordusk;
  vector[n] offset;
}
parameters{
  vector[7] b;
}
transformed parameters {
  vector [n] p;
  for (i in 1:n) p[i] = inv_cloglog(b[1] + b[2]*egk[i] + b[3]*speed[i] + b[4]*light[i] + b[5]*light2[i] + b[6]*dawnordusk[i] + offset[i]);
}
model{
  b ~ normal(0,1);
  y ~ bernoulli(p);
}
"
# generated quantities{
#   vector[n] log_lik;
#   for(i in 1:n) log_lik[i] <- bernoulli_logit_log(y[n], b[1] + b[2]*egk[n] + b[3]*speed[n] + b[4]*light[n] + b[5]*light2[n] + b[6]*dawnordusk[n] + b[7]*trains[n]);
# }
# "

# scode.bin2 <- "
# data{
#   int<lower=1> n;
#   int<lower=0,upper=1> y[n];
#   vector[n] egk;
#   vector[n] speed;
#   vector[n] light;
#   vector[n] light2;
#   vector[n] dawnordusk;
#   vector[n] trains;
# }
# parameters{
#   vector[7] b;
# }
# model{
#   b ~ cauchy( 0 , 2.5 );
#   y ~ bernoulli_logit(b[1] + b[2]*egk + b[3]*speed + b[4]*light + b[5]*light2 + b[6]*dawnordusk + b[7]*trains);
# }
# generated quantities{
#   vector[n] log_lik;
#   for(i in 1:n) log_lik[i] <- bernoulli_logit_log(y[i], b[1] + b[2]*egk[i] + b[3]*speed[i] + b[4]*light[i] + b[5]*light2[i] + b[6]*dawnordusk[i] + b[7]*trains[i]);
# }
# "

coll.stan.bin <- stan(model_code = scode.bin, iter = 500, chains = 3, cores = 3, seed=123)#, control =list(stepsize=0.01, adapt_delta=0.9, max_treedepth=15))# 962.779 seconds (Total)


coll.stan.bin
#           mean se_mean   sd     2.5%      25%      50%      75%    97.5% n_eff Rhat
# b[1]    -7.25    0.00 0.08    -7.41    -7.30    -7.25    -7.20    -7.11   481 1.00
# b[2]     0.69    0.00 0.08     0.53     0.64     0.69     0.74     0.84   677 1.00
# b[3]     1.55    0.01 0.13     1.29     1.47     1.56     1.64     1.79   377 1.00
# b[4]    -0.79    0.01 0.14    -1.04    -0.88    -0.78    -0.70    -0.52   536 1.00
# b[5]    -1.37    0.01 0.12    -1.62    -1.44    -1.37    -1.28    -1.12   515 1.00
# b[6]     0.30    0.00 0.10     0.11     0.23     0.30     0.36     0.48   393 1.00
# b[7]     0.28    0.00 0.09     0.10     0.21     0.28     0.34     0.45   465 1.00
# lp__ -2828.46    0.12 2.02 -2833.41 -2829.57 -2828.17 -2826.97 -2825.56   287 1.02

traceplot(As.mcmc.list(coll.stan.bin))
save(coll.stan.bin,file="data/coll_stan_bin")

coll.arm.bin <- bayesglm(coll ~ egk + speed100 + light + light2 + dawnordusk, offset=offset, data=model.data.bin, family=binomial(link="cloglog"))
summary(coll.arm.bin)
paste("% Error Explained: ",round(((coll.arm.bin$null.deviance - coll.arm.bin$deviance)/coll.arm.bin$null.deviance)*100,2),sep="") 
# Coefficients:
#               Estimate Std. Error z value Pr(>|z|)    
# (Intercept)   -11.8056     0.3146 -37.521  < 2e-16 ***
#   egk           2.0523     0.2387   8.598  < 2e-16 ***
#   speed100      4.3494     0.3046  14.279  < 2e-16 ***
#   light        -1.0836     0.1076 -10.067  < 2e-16 ***
#   light2       -1.5999     0.1695  -9.439  < 2e-16 ***
#   dawnordusk    0.4091     0.0648   6.313 2.74e-10 ***
# 
# Null deviance: 6219.9  on 291119  degrees of freedom
# Residual deviance: 5684.1  on 291114  degrees of freedom
# AIC: 5696.1


coll.arm.pois <- bayesglm(coll ~ egk + speed100 + light + light2 + dawnordusk, offset = offset, data=model.data, family=poisson(link="log"))
summary(coll.arm.pois)
paste("% Error Explained: ",round(((coll.arm.pois$null.deviance - coll.arm.pois$deviance)/coll.arm.pois$null.deviance)*100,2),sep="") 
# Coefficients:
#                Estimate Std. Error z value Pr(>|z|)    
# (Intercept)   -11.80923    0.31245 -37.796  < 2e-16 ***
#   egk           2.05827    0.23634   8.709  < 2e-16 ***
#   speed100      4.37288    0.30224  14.468  < 2e-16 ***
#   light        -1.08927    0.10738 -10.144  < 2e-16 ***
#   light2       -1.62782    0.16907  -9.628  < 2e-16 ***
#   dawnordusk    0.42896    0.06423   6.679 2.41e-11 ***
# 
# Null deviance: 5511.5  on 291119  degrees of freedom
# Residual deviance: 4956.4  on 291114  degrees of freedom
# AIC: 5780.7

options(mc.cores = parallel::detectCores())
coll.stanarm.bin <- stan_glm(coll ~ log(egk) + log(trains) + log(speed) + light + light2 + dawnordusk, offset=log(length), data=model.data.bin, family=binomial(link="cloglog")) #2847.49 seconds (Total)
summary(coll.stanarm.bin)

save(coll.stanarm.bin,file="data/coll_stanarm_bin")

stancode <- rstan::get_stancode(coll.stanarm.bin$stanfit)
cat(stancode)

sims <- as.matrix(m1.stan)
y_sim <- rnorm(n = nrow(sims), mean = sims[,1:(ncol(sims)-1)] %*% t(as.matrix(s1)), 
               sd = sims[,ncol(sims)])
mean(y_sim)


coll.loo.bin <- loo(coll.stanarm.bin)

launch_shinystan(coll.stanarm.bin)

log_lik.nb <- extract_log_lik(coll.stan.nb)

loo.nb <- loo(log_lik.nb)

plot(log_lik.nb[1,],s.coll.nb)

for(i in 1:ncol(log_lik.nb)) abline(lm(s.coll~log_lik.nb[i,]))

pp_check(coll.stanarm.bin, check = "distributions", overlay = FALSE, nreps = 5)

###################Current Model###########################

n <- nrow(model.data.bin)
y <- model.data.bin$coll
egk <- model.data.bin$egk
trains <- model.data.bin$trains
speed <- model.data.bin$speed
light <- model.data.bin$light
light2 <- model.data.bin$light2
dawnordusk <- model.data.bin$dawnordusk
kilometre <- model.data.bin$length

stan_rdump(c('n','y','egk','trains','speed','light','light2','dawnordusk','kilometre'),file="/home/casey/cmdstan-2.12.0/examples/bern_cloglog/data.R")

file.create("/home/casey/cmdstan-2.12.0/examples/bern_cloglog/model.stan")
fileConn<-file("/home/casey/cmdstan-2.12.0/examples/bern_cloglog/model.stan")
writeLines(c("data{
int<lower=1> n;
int<lower=0,upper=1> y[n];
vector[n] egk;
vector[n] trains;
vector[n] speed;
vector[n] light;
vector[n] light2;
vector[n] dawnordusk;
vector[n] kilometre;
}
transformed data{
vector[n] log_egk_c;
vector[n] log_trains_c;
vector[n] log_speed_c;
vector[n] light_c;
vector[n] light2_c;
vector[n] dawnordusk_c;
vector[n] log_kilometre;
             
log_egk_c = log(egk)-mean(log(egk));
log_trains_c = log(trains)-mean(log(trains));
log_speed_c = log(speed)-mean(log(speed));
             
light_c = light-mean(light);
light2_c = light2-mean(light2);
dawnordusk_c = dawnordusk-mean(dawnordusk);
             
log_kilometre = log(kilometre);
}
parameters{
vector[7] b;
}
transformed parameters{
vector[n] p;
for (i in 1:n) p[i] = inv_cloglog(b[1] + b[2]*log_egk_c[i] + b[3]*log_trains_c[i] + b[4]*log_speed_c[i] + b[5]*light_c[i] + b[6]*light2_c[i] + b[7]*dawnordusk_c[i] + log_kilometre[i]);
}
model{
b ~ normal(0,1);
y ~ bernoulli(p);
}
generated quantities{
vector log_lik;
vector sum_log_lik

             
for(i in 1:n) log_lik[i] = binomial_lpmf(y[i]|1,p[i]); //posterior draws to get posterior predictive checks
}"), fileConn)
close(fileConn)

scode.bin <- "
data{
int<lower=1> n;
int<lower=0,upper=1> y[n];
vector[n] egk;
vector[n] trains;
vector[n] speed;
vector[n] light;
vector[n] light2;
vector[n] dawnordusk;
vector[n] kilometre;
}
transformed data{
vector[n] log_egk_c;
vector[n] log_trains_c;
vector[n] log_speed_c;
vector[n] light_c;
vector[n] light2_c;
vector[n] dawnordusk_c;
vector[n] log_kilometre;

log_egk_c = log(egk)-mean(log(egk));
log_trains_c = log(trains)-mean(log(trains));
log_speed_c = log(speed)-mean(log(speed));

light_c = light-mean(light);
light2_c = light2-mean(light2);
dawnordusk_c = dawnordusk-mean(dawnordusk);

log_kilometre = log(kilometre);
}
parameters{
vector[7] b;
}
transformed parameters{
vector[n] p;
for (i in 1:n) p[i] = inv_cloglog(b[1] + b[2]*log_egk_c[i] + b[3]*log_trains_c[i] + b[4]*log_speed_c[i] + b[5]*light_c[i] + b[6]*light2_c[i] + b[7]*dawnordusk_c[i] + log_kilometre[i]);
}
model{
b ~ normal(0,1);
y ~ bernoulli(p);
}
generated quantities{
//vector[n] log_lik;
//for(i in 1:n) log_lik[i] = binomial_lpmf(y[i]|1,p[i]); //posterior draws to get posterior predictive checks

    real loglik;
    real total_loglik;
    real y_hat[n];
    # initialize total_loglik
    total_loglik = 0;
    
    for (i in 1:n) {
      y_hat[i] <- alpha + beta * ln_alf_heldout[i];
      loglik <- normal_log(y[i], y_hat[i], sigma_obs);
      total_loglik_heldout = total_loglik + loglik;
      # We are only monitoring the summed loglik here.
      # But above can easily be modified to give obervation log likelihood
      # However, if you are dealing with large datasets this will consume alot of memory.
    }

}
"

coll.stan.bin <- stan(model_code = scode.bin, iter = 500, chains = 1, cores = 1, seed=123)#, control =list(stepsize=0.01, adapt_delta=0.9, max_treedepth=15))

coll.stan.bin
#           mean se_mean   sd     2.5%      25%      50%      75%    97.5% n_eff Rhat
# b[1]    -7.09    0.00 0.08    -7.25    -7.15    -7.09    -7.04    -6.93   942    1
# b[2]     0.58    0.00 0.06     0.47     0.54     0.58     0.62     0.69  1313    1
# b[3]     0.01    0.00 0.09    -0.16    -0.05     0.01     0.07     0.18  1204    1
# b[4]     3.21    0.01 0.29     2.64     3.01     3.20     3.40     3.76  1243    1
# b[5]    -0.65    0.00 0.11    -0.85    -0.72    -0.65    -0.58    -0.44   989    1
# b[6]    -1.76    0.00 0.15    -2.07    -1.86    -1.76    -1.65    -1.46  1352    1
# b[7]     0.26    0.00 0.07     0.13     0.21     0.26     0.30     0.39   994    1
# lp__ -2794.54    0.07 1.79 -2798.80 -2795.42 -2794.21 -2793.21 -2792.02   668    1

traceplot(As.mcmc.list(coll.stan.bin))
save(coll.stan.bin,file="data/coll_stan_bin")

csvfiles <- dir('/home/casey/cmdstan-2.12.0/examples/bern_cloglog/', pattern = 'output.csv', full.names = TRUE)

coll.stan.bin <- read_stan_csv(csvfiles)

data <- data.frame("y"=model.data.bin$coll,
           "egk"=log(model.data.bin$egk)-mean(log(model.data.bin$egk)),
           "trains"=log(model.data.bin$trains)-mean(log(model.data.bin$trains)),
           "speed"=log(model.data.bin$speed)-mean(log(model.data.bin$speed)),
           "light"=model.data.bin$light-mean(model.data.bin$light),
           "light2"=model.data.bin$light2-mean(model.data.bin$light2),
           "dawnordusk"=model.data.bin$dawnordusk-mean(model.data.bin$dawnordusk),
           "kilometre"=log(model.data.bin$length)
           )

range(cor(data[,1:8])[cor(data[,1:8])!=1])

rstan_options (auto_write=TRUE)
options (mc.cores=parallel::detectCores() - 1)

set.seed(123)
coll.stan.bin <- stan_glm(formula=y ~ egk + trains + speed + light + light2 + dawnordusk,
                          offset=kilometre,
                          family=binomial(link="cloglog"),
                          data=data,
                          chains=3,
                          iter=500)

cross.val <- kfold(coll.stan.bin, K = 10)

coll.glm <- glm(formula=y ~ egk + trains + speed + light + light2 + dawnordusk,
           offset=kilometre,
           family=binomial(link="cloglog"),
           data=data)

summary(coll.glm)

write.csv(signif(summary(coll.glm)$coefficients, digits=4),"output/all_coll_coef.csv",row.names=FALSE)

#####################cross validation#######################

split_into_kfolds <- function(data, k=10) {
  # make dataset an even multiple of 10
  data <- data[seq_len(floor(nrow(data) / k) * k), ]
  # execute the split
  # use an ordered vector so that all species distributed
  # approx. equally across groups
  fold <- rep(seq_len(k), nrow(data)/k)
  split(data, fold)
}

LogLoss <- function(actual, predicted, eps=0.00001) {
  predicted <- pmin(pmax(predicted, eps), 1-eps)
  -1/length(actual)*(sum(actual*log(predicted)+(1-actual)*log(1-predicted)))
}

roc <- function (obsdat, preddat){
  if (length(obsdat) != length(preddat)) 
    stop("obs and preds must be equal lengths")
  n.x <- length(obsdat[obsdat == 0])
  n.y <- length(obsdat[obsdat == 1])
  xy <- c(preddat[obsdat == 0], preddat[obsdat == 1])
  rnk <- rank(xy)
  roc <- ((n.x * n.y) + ((n.x * (n.x + 1))/2) - sum(rnk[1:n.x]))/(n.x * n.y)
  return(round(roc, 4))
}

# test <- split_into_kfolds(model.data, 10)
# sapply(test, function(x) length(x$coll[x$coll==1]))
# sapply(test, function(x) range(x$dawndusk))
# test <- split_into_kfolds(data, 10)
# sapply(test, function(x) length(x$y[x$y==1]))
# sapply(test, function(x) range(x$dawnordusk))
# 
# test2 <- split(model.data, sample(1:10, nrow(model.data), replace=T))
# sapply(test2, function(x) length(x$coll[x$coll==1]))
# sapply(test2, function(x) range(x$dawndusk))
# test2 <- split(data, sample(1:10, nrow(data), replace=T))
# sapply(test2, function(x) length(x$y[x$y==1]))
# sapply(test2, function(x) range(x$dawnordusk))

N <- 30

cv.data <- split_into_kfolds(data, N)
sapply(cv.data, function(x) length(x$y[x$y==1]))
sapply(cv.data, function(x) range(x$dawnordusk))

registerDoMC(detectCores() - 1)
perform.glm <-foreach(i = 1:N, .combine=cbind) %dopar% {
  fit <- glm(formula=y ~ egk + trains + speed + light + light2 + dawnordusk,
                          offset=kilometre,
                          family=binomial(link="cloglog"),
                          data=cv.data[[i]])
  c(coef(fit), summary(fit)$coefficients[, 2], summary(coll.glm)$coefficients[, 4], "ll"=LogLoss(data$y,predict(fit, data, type="response")), "roc"=roc(data$y,predict(fit, data, type="response")))
}


perform.glm.30 <- cbind("coef"=signif(apply(perform.glm[1:7,], 1, mean), digits=4),
                     "coef_sd"=signif(apply(perform.glm[1:7,], 1, sd), digits=4),
                     "sig_no"=apply(perform.glm[15:21,], 1, function(x) sum(ifelse(x>=2E-16, 0, 1)))
)

signif(mean(perform.glm[22,]), digits=4)
signif(sd(perform.glm[22,]), digits=4)

signif(mean(perform.glm[23,]), digits=4)
signif(sd(perform.glm[23,]), digits=4)

write.csv(perform.glm.30,"output/coll_coef_30f.csv",row.names=FALSE)


#val.pred.glm <- predict(coll.glm, test[[2]], type="link")  #Make predictions with regression model fit on link scale

#summary(glm(test[[2]]$y ~ val.pred.glm, family = binomial(link = "cloglog")))  #slope is close to ine therefore model is well calibrated to external data after accounting for multiplicative differences

