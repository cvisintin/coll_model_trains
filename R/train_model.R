require(data.table)
require(lmtest)
require(foreach)
require(doMC)
require(arm)
require(corrplot)

### Define functions #######
invcloglog <- function (x) {1-exp(-exp(x))}

d <- function (h, peak, trough, l) {
  # evaluate a two-Gaussian function for given peak/trough/lengthscale
  exp(-((h - peak) / l) ^ 2) -   exp(-((h - trough) / l) ^ 2)
}

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

dev <- function (model) {
  round(((model$null.deviance - model$deviance)/model$null.deviance)*100,2)
}
################

model.data <- as.data.table(read.delim("data/model_data_hm.csv", header=TRUE, sep=","))

model.data.bin <- copy(model.data)
model.data.bin[coll>1,coll:=1]
rm(model.data)

summary(model.data.bin)

write.csv(model.data.bin, "data/model_data_hm_bin.csv", row.names=FALSE)

data <- data.frame("y"=model.data.bin$coll,
                   "egk"=model.data.bin$egk,
                   "trains"=model.data.bin$trains,
                   "speed"=model.data.bin$speed,
                   "egk_lc"=log(model.data.bin$egk)-mean(log(model.data.bin$egk)),
                   "trains_lc"=log(model.data.bin$trains)-mean(log(model.data.bin$trains)),
                   "speed_lc"=log(model.data.bin$speed)-mean(log(model.data.bin$speed)),
                   "light"=model.data.bin$light,
                   "light2"=model.data.bin$light2,
                   "dawnordusk"=model.data.bin$dawnordusk,
                   "kilometre"=log(model.data.bin$length)
                    )

data <- cbind(data,model.data.bin[,c(6,10:11),with=FALSE])

col_grad <- colorRampPalette(c("#000001", "#d9d9d9","#000000"))

corr.coll <- cor(data[data$y==1,c("egk","trains","speed","hour")])
png('figs/cor_coll.png', pointsize = 8, res=300, width = 1100, height = 900, bg='transparent')
corrplot.mixed(corr.coll, tl.col="black", tl.cex=1.2, cl.cex=0.8, cl.align.text='l', number.cex=0.8, number.digits=3, lower.col = "#4d4d4d", upper.col = col_grad(100), title="Cells with collisions", mar=c(0,0,1,0))
dev.off()

corr.nocoll <- cor(data[data$y==0,c("egk","trains","speed","hour")])
png('figs/cor_nocoll.png', pointsize = 8, res=300, width = 1100, height = 900, bg='transparent')
corrplot.mixed(corr.nocoll, tl.col="black", tl.cex=1.2, cl.cex=0.8, cl.align.text='l', number.cex=0.8, number.digits=3, lower.col = "#4d4d4d", upper.col = col_grad(100), title="Cells with no collisions", mar=c(0,0,1,0))
dev.off()
#corr2 <- cor(data[,5:10])
#corrplot.mixed(corr2, tl.col="black", tl.cex=1.2, number.cex=0.8, number.digits=3, lower.col = "#4d4d4d", upper.col = col_grad(100))


hist(data[data$y==1,"egk"])
hist(data[data$y==1,"trains"])
hist(data[data$y==1,"speed"])
hist(data[data$y==1,"hour"])

plot(aggregate(data$trains,by=list(Hour=data$hour),mean), type='l')

save(data, file="data/coll_glm_data")
write.csv(data, file="data/coll_glm_data.csv", row.names=F)

coll.glm.wo_trains <- stats::glm(formula=y ~ egk_lc + speed_lc + light + light2 + dawnordusk,
                            offset=kilometre,
                            family=stats::binomial(link="cloglog"),
                            data=data)
dev(coll.glm.wo_trains)

coll.glm.wo_egk <- stats::glm(formula=y ~ speed_lc + light + light2 + dawnordusk,
                                 offset=kilometre,
                                 family=stats::binomial(link="cloglog"),
                                 data=data)
dev(coll.glm.wo_egk)

coll.glm.wo_speed <- stats::glm(formula=y ~ egk_lc + light + light2 + dawnordusk,
                              offset=kilometre,
                              family=stats::binomial(link="cloglog"),
                              data=data)
dev(coll.glm.wo_speed)

coll.glm.wo_temp <- stats::glm(formula=y ~ egk_lc + speed_lc,
                                offset=kilometre,
                                family=stats::binomial(link="cloglog"),
                                data=data)
dev(coll.glm.wo_temp)

coll.glm.w_trains <- stats::glm(formula=y ~ egk_lc + trains_lc + speed_lc + light + light2 + dawnordusk,
                            offset=kilometre,
                            family=stats::binomial(link="cloglog"),
                            data=data)
dev(coll.glm.w_trains)


lrtest(coll.glm.wo_trains, coll.glm.wo_egk)
lrtest(coll.glm.wo_trains, coll.glm.wo_speed)
lrtest(coll.glm.wo_trains, coll.glm.wo_temp)
lrtest(coll.glm.wo_trains, coll.glm.w_trains)


save(coll.glm.wo_trains, coll.glm.wo_egk, coll.glm.wo_speed, coll.glm.wo_temp, coll.glm.w_trains, file="output/coll_glm")


ci.sim <- function(model, x.range, ind.var){
  conf.df <- data.frame("egk_lc"=rep(mean(data$egk_lc),length(x.range)),"trains_lc"=mean(data$trains_lc),"speed_lc"=mean(data$speed_lc),"light"=mean(data$light),"light2"=mean(data$light2),"dawnordusk"=mean(data$dawnordusk),"kilometre"=mean(data$kilometre))
  if(length(ind.var)==1){
    conf.df[ ,grep(ind.var, colnames(conf.df))] <- log(x.range) - mean(log(data[ ,grep(ind.var, colnames(data))[1]]))    
  }

  if(length(ind.var)>1){
    for(i in 1:length(ind.var)){
      conf.df[ ,grep(ind.var[i], colnames(conf.df))] <- log(x.range) - mean(log(data[ ,grep(ind.var[i], colnames(data))[i]]))     
    }
  }
  
  conf.df.predict <- predict(model, se.fit = TRUE, newdata = conf.df)
  
  draws_link <- outer(rnorm(1000), conf.df.predict$se.fit)
  draws_link <- sweep(draws_link, 2, conf.df.predict$fit, FUN = "+")
  draws <- invcloglog(draws_link)
  
  mean <- colMeans(draws)
  cis <- t(apply(draws, 2, quantile, c(0.025, 0.975)))
  
  return(data.frame("y"=mean, "ymin"=cis[ ,1], "ymax"=cis[ ,2], "x"=x.range))
}

ci.sim.temporal <- function(model, x.range, ind.var){
  conf.df <- data.frame("egk_lc"=rep(mean(data$egk_lc),length(x.range)),"trains_lc"=mean(data$trains_lc),"speed_lc"=mean(data$speed_lc),"light"=mean(data$light),"light2"=mean(data$light2),"dawnordusk"=mean(data$dawnordusk),"kilometre"=mean(data$kilometre))
  conf.df[ ,grep(ind.var[1], colnames(conf.df))[1]] <- (sin((2 * pi * (x.range - 6)) / 24)-mean(sin((2 * pi * (x.range - 6)) / 24)))
  conf.df[ ,grep(ind.var[2], colnames(conf.df))] <- (sin((2 * pi * (x.range - 6)) / 24)^2-mean(sin((2 * pi * (x.range - 6)) / 24)^2))
  conf.df[ ,grep(ind.var[3], colnames(conf.df))] <- (dawn.or.dusk(h=x.range,
                                                                  dawn=data[ ,grep(ind.var[3], colnames(data))[2]],
                                                                  dusk=data[ ,grep(ind.var[4], colnames(data))[2]]
                                                                  )-mean(dawn.or.dusk(h=x.range,
                                                                                      dawn=data[ ,grep(ind.var[3], colnames(data))[2]],
                                                                                      dusk=data[ ,grep(ind.var[4], colnames(data))[2]]
                                                                                      )
                                                                         )
                                                     )

  conf.df.predict <- predict(model, se.fit = TRUE, newdata = conf.df)
  
  draws_link <- outer(rnorm(1000), conf.df.predict$se.fit)
  draws_link <- sweep(draws_link, 2, conf.df.predict$fit, FUN = "+")
  draws <- invcloglog(draws_link)
  
  mean <- colMeans(draws)
  cis <- t(apply(draws, 2, quantile, c(0.025, 0.975)))
  
  return(data.frame("y"=mean, "ymin"=cis[ ,1], "ymax"=cis[ ,2], "x"=x.range))
}


egk.range <- seq(0, 1, length.out = 10001)[-1] # Define range for independent variable (egk)
egk_mean_ci <- ci.sim(coll.glm.w_trains, egk.range, "egk")

trains.range <- seq(1, 20, length.out = 10000) # Define range for independent variable (trains)
trains_mean_ci <- ci.sim(coll.glm.w_trains, trains.range, "trains")

speed.range <- seq(1, 160, length.out = 10000) # Define range for independent variable (speed)
speed_mean_ci <- ci.sim(coll.glm.w_trains, speed.range, "speed")

hour.range <- seq(0, 24, length.out = nrow(data)+1)[-1] # Define range for independent variable (hour)
hour_mean_ci <- ci.sim.temporal(coll.glm.w_trains, hour.range, c("light","light2","dawn","dusk"))

save(egk_mean_ci, trains_mean_ci, speed_mean_ci, hour_mean_ci, file="output/mar_effects_ci")


# Randomised quantile residuals - Dunn & Smyth (1996)

quant.resid <- function(model, n) {
  registerDoMC(cores=detectCores()-1)
  prob <- predict(model, type = 'response')
  resid <- foreach(i = 1:nrow(data), .combine=c) %dopar% {
    simulations.c = c()
    while (length(simulations.c) < 10000 &
           all(simulations.c != model$y[i])) {
      set.seed(123+i)
      simulations.c = c(simulations.c, rbinom(n, 1, prob[i]))
    }
    if (!any(simulations.c == model$y[i]))
      warning(sprintf('datapoint %i had no valid samples', i))
    
    # add jitter
    set.seed(123+i)
    fuzzy.y <- model$y[i] + runif(1, -0.5, 0.5)
    set.seed(123+i)
    fuzzy.simulations <- simulations.c + runif(n, -0.5, 0.5)
    
    # make sure ecdf doesn't go to 1 or 0
    sim.limits <- range(sort(unique(fuzzy.simulations))[-c(1,length(unique(fuzzy.simulations)))])
    fuzzy.y <- pmin(pmax(fuzzy.y, sim.limits[1]), sim.limits[2])
    
    ecdf(fuzzy.simulations)(fuzzy.y)
  }
  return(resid)
}

coll.resid <- quant.resid(coll.glm.w_trains, 5000)

save(coll.resid,file="output/coll_resid")
#load("output/coll_resid")

coll.resid.norm <- qnorm(coll.resid)

png('figs/brq_prob.png', pointsize = 12, res=300, width = 1200, height = 900, bg='transparent')
  par(mar=c(3.0,3.5,1.5,0.5))
  binnedplot(log(predict(coll.glm.w_trains, type = 'response')), coll.resid.norm, ylab="RQ Residual", xlab="", main='', cex.axis=0.6, las=1, mgp=c(2.5,1,0), cex.lab=0.8, xaxt='n', cex.pts=0.4)
  axis(1, cex.axis=0.6, mgp=c(2.5,0.5,0))
  title(xlab='Estimated Pr of Collision (log-transformed)', mgp=c(1.8,0.5,0), cex.lab=0.8)
dev.off()

png('figs/brq_egk.png', pointsize = 12, res=300, width = 900, height = 900, bg='transparent')
  par(mar=c(3.0,3.5,1.5,0.5))
  binnedplot(log(data$egk), coll.resid.norm, ylab="RQ Residual", xlab="", main='', cex.axis=0.6, las=1, mgp=c(2.5,1,0), cex.lab=0.8, xaxt='n', cex.pts=0.4)
  axis(1, cex.axis=0.6, mgp=c(2.5,0.5,0))
  title(xlab='Kangaroo Occurrence (log-transformed)', mgp=c(1.8,0.5,0), cex.lab=0.8)
dev.off()

png('figs/brq_trains.png', pointsize = 12, res=300, width = 900, height = 900, bg='transparent')
  par(mar=c(3.0,3.5,1.5,0.5))  
  binnedplot(log(data$trains), coll.resid.norm, ylab="RQ Residual", xlab="", main='', cex.axis=0.6, las=1, mgp=c(2.5,1,0), cex.lab=0.8, xaxt='n', cex.pts=0.4)
  axis(1, cex.axis=0.6, mgp=c(2.5,0.5,0))
  title(xlab='Number of Trains (log-transformed)', mgp=c(1.8,0.5,0), cex.lab=0.8)
dev.off()

png('figs/brq_speed.png', pointsize = 12, res=300, width = 900, height = 900, bg='transparent')
  par(mar=c(3.0,3.5,1.5,0.5))
  binnedplot(data$speed, coll.resid.norm, ylab="RQ Residual", xlab="", main='', cex.axis=0.6, las=1, mgp=c(2.5,1,0), cex.lab=0.8, xaxt='n', cex.pts=0.4)
  axis(1, cex.axis=0.6, mgp=c(2.5,0.5,0))
  title(xlab='Train Speed', mgp=c(1.8,0.5,0), cex.lab=0.8)
dev.off()

png('figs/brq_hour.png', pointsize = 12, res=300, width = 900, height = 900, bg='transparent')
  par(mar=c(3.0,3.5,1.5,0.5))
  binnedplot(data$hour, coll.resid.norm, ylab="RQ Residual", xlab="", main='', cex.axis=0.6, las=1, mgp=c(2.5,1,0), cex.lab=0.8, xaxt='n', cex.pts=0.4)
  axis(1, cex.axis=0.6, mgp=c(2.5,0.5,0))
  title(xlab='Hour', mgp=c(1.8,0.5,0), cex.lab=0.8)
dev.off()

#####################cross validation#######################

# K <- 10
# N <- 100
# model_formula <- formula(coll.glm.w_trains)

cv.sim <- function(K, N, model_formula, data, cores) {
  perform.glm.cv <- foreach(i = 1:N, .combine=cbind) %do% {
    set.seed(i*123)
    cv.data <- split(data, sample(1:K, nrow(data), replace=T))
    registerDoMC(cores)
    glm.cv <-foreach(j = 1:K, .combine=cbind) %dopar% {
      
      roc <- function(obsdat, preddat){
        if (length(obsdat) != length(preddat)) 
          stop("obs and preds must be equal lengths")
        n.x <- length(obsdat[obsdat == 0])
        n.y <- length(obsdat[obsdat == 1])
        xy <- c(preddat[obsdat == 0], preddat[obsdat == 1])
        rnk <- rank(xy)
        roc <- ((n.x * n.y) + ((n.x * (n.x + 1))/2) - sum(rnk[1:n.x]))/(n.x * n.y)
        return(round(roc, 4))
      }
      
      data.train <- do.call("rbind", cv.data[-j])
      data.test <- cv.data[[j]]
      fit <- glm(formula=model_formula,
                 offset=kilometre,
                 family=binomial(link="cloglog"),
                 data=data.train)
      
      y <- data.test$y
      p <- predict(fit, data.test, type="response")
      calib <- glm(y~log(p), family=binomial(link=cloglog))
      roc <- roc(y,p)
      rbind(calib_int=calib$coefficients[1], calib_slope=calib$coefficients[2], roc)
    }
    colnames(glm.cv) <- paste0(i,".",seq(1,K,1))
    glm.cv
  }
  return(perform.glm.cv)
}

cv_coll.glm.w_trains <- cv.sim(K=10, N=100, model_formula = formula(coll.glm.w_trains), data = data, cores = 7) # runtime: approx 30 minutes - best to run on server
cv_coll.glm.wo_egk <- cv.sim(K=10, N=100, model_formula = formula(coll.glm.wo_egk), data = data, cores = 7)
cv_coll.glm.wo_speed <- cv.sim(K=10, N=100, model_formula = formula(coll.glm.wo_speed), data = data, cores = 7)
cv_coll.glm.wo_temp <- cv.sim(K=10, N=100, model_formula = formula(coll.glm.wo_temp), data = data, cores = 7)
cv_coll.glm.wo_trains <- cv.sim(K=10, N=100, model_formula = formula(coll.glm.wo_trains), data = data, cores = 7)

save(cv_coll.glm.w_trains, cv_coll.glm.wo_egk, cv_coll.glm.wo_speed, cv_coll.glm.wo_temp, cv_coll.glm.wo_trains, file="output/perform_glm_cv")

##################### Simulate management interventions ################
data.a <- as.data.table(data)
data.b <- as.data.table(cbind(data,"hour"=model.data.bin$hour, "id"=model.data.bin$id, "trips"=model.data.bin$trains))
data.c <- as.data.table(cbind(data,"hour"=model.data.bin$hour, "id"=model.data.bin$id, "length"=model.data.bin$length))

data.b[egk_lc >= 1.408607 & ((hour >= 5 & hour < 9) | (hour >= 16 & hour < 20)) & speed_lc > -0.3394367, mean(trips), by="id"]
#data.b[egk_lc >= 1.91918 & ((hour >= 5 & hour < 9) | (hour >= 16 & hour < 20)) & speed_lc > -0.05173123, sum(y), by="id"]
sum(data.b[egk_lc >= 1.408607 & ((hour >= 5 & hour < 9) | (hour >= 16 & hour < 20)) & speed_lc > -0.3394367, mean(trips), by="id"]$V1)
data.b[egk_lc >= 1.408607 & ((hour >= 5 & hour < 9) | (hour >= 16 & hour < 20)) & speed > -0.3394367, speed_lc := -0.3394367] #note values for egk and speed are on the transformed scale
#range(data.b[egk >= 2.324645 & ((hour >= 6 & hour < 9) | (hour >= 17 & hour < 20)), speed])

data.c[speed_lc > 0.3537339, .N, by="id"]
#data.c[speed_lc > 0.3537339, sum(y), by="id"]
sum(data.c[speed_lc > 0.3537339, mean(length), by="id"]$V1)
#data.c[speed_lc > 0.3537339, egk_lc := egk_lc-log(2)] #note values for egk and speed are on the transformed scale
data.c[speed_lc > 0.3537339, egk_lc := log(.01)]
#range(data.c[speed > 0.1714123, egk])

pred.sim <- function(model,data,sims){
  coll.sim <- sim(model, sims)
  beta.sim <- coll.sim@coef
  N.sim <- length(data$y)
  X.sim <- cbind(rep(1,N.sim),data$egk_lc,data$speed_lc,data$light,data$light2,data$dawnordusk)
  offset.sim <- data$kilometre
  rate.sim <- array(NA,c(sims,N.sim))
  for(s in 1:sims){
    rate.sim[s,] <- exp(X.sim %*% beta.sim[s, ] + offset.sim)
  }
  coll.totals <- rowSums(rate.sim)
  return(c(quantile(coll.totals, 0.025), mean(coll.totals), quantile(coll.totals, 0.975)))
}

set.seed(123)
manage.a <- pred.sim(coll.glm.wo_trains, data.a, 1000)

set.seed(123)
manage.b <- pred.sim(coll.glm.wo_trains, data.b, 1000)

set.seed(123)
manage.c <- pred.sim(coll.glm.wo_trains, data.c, 1000)
