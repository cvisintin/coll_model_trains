require(data.table)

model.data <- as.data.table(read.delim("data/model_data_hm.csv", header=T, sep=","))

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
                   "light"=model.data.bin$light-mean(model.data.bin$light),
                   "light2"=model.data.bin$light2-mean(model.data.bin$light2),
                   "dawnordusk"=model.data.bin$dawnordusk-mean(model.data.bin$dawnordusk),
                   "kilometre"=log(model.data.bin$length)
                    )

data <- cbind(data,model.data.bin[,c(6,10:11),with=FALSE])

save(data, file="data/coll_glm_data")
write.csv(data, file="data/coll_glm_data.csv", row.names=F)

coll.glm <- glm(formula=y ~ egk_lc + trains_lc + speed_lc + light + light2 + dawnordusk,
           offset=kilometre,
           family=binomial(link="cloglog"),
           data=data)

summary(coll.glm)

save(coll.glm, file="output/coll_glm")

#Randomised quantile residuals - Dunn & Smyth (1996)
prob <- predict(coll.glm, type = 'response')
n <- 5000

registerDoMC(cores=detectCores()-1)

system.time(
  coll.resid <- foreach(i = 1:nrow(data), .combine=c) %dopar% {
    simulations.c = c()
    while (length(simulations.c) < 10000 &
           all(simulations.c != coll.glm$y[i])) {
      set.seed(123+i)
      simulations.c = c(simulations.c, rbinom(n, 1, prob[i]))
    }
    if (!any(simulations.c == coll.glm$y[i]))
      warning(sprintf('datapoint %i had no valid samples', i))
    
    # add jitter
    set.seed(123+i)
    fuzzy.y <- coll.glm$y[i] + runif(1, -0.5, 0.5)
    set.seed(123+i)
    fuzzy.simulations <- simulations.c + runif(n, -0.5, 0.5)
    
    # make sure ecdf doesn't go to 1 or 0
    sim.limits <- range(sort(unique(fuzzy.simulations))[-c(1,length(unique(fuzzy.simulations)))])
    fuzzy.y <- pmin(pmax(fuzzy.y, sim.limits[1]), sim.limits[2])
    
    ecdf(fuzzy.simulations)(fuzzy.y)
  }
) ###214 second runtime

save(coll.resid,file="output/coll_resid")
#load("output/coll_resid")

coll.resid.norm <- qnorm(coll.resid)

png('figs/brq_prob.png', pointsize = 12, res=300, width = 1200, height = 900, bg='transparent')
  par(mar=c(3.0,3.5,1.5,0.5))
  binnedplot(log(prob), coll.resid.norm, ylab="RQ Residual", xlab="", main='', cex.axis=0.6, las=1, mgp=c(2.5,1,0), cex.lab=0.8, xaxt='n')
  axis(1, cex.axis=0.6, mgp=c(2.5,0.5,0))
  title(xlab='Estimated Pr of Collision (log-transformed)', mgp=c(1.8,0.5,0), cex.lab=0.8)
dev.off()

png('figs/brq_egk.png', pointsize = 12, res=300, width = 900, height = 900, bg='transparent')
  par(mar=c(3.0,3.5,1.5,0.5))
  binnedplot(log(data$egk), coll.resid.norm, ylab="RQ Residual", xlab="", main='', cex.axis=0.6, las=1, mgp=c(2.5,1,0), cex.lab=0.8, xaxt='n')
  axis(1, cex.axis=0.6, mgp=c(2.5,0.5,0))
  title(xlab='Kangaroo Occurrence (log-transformed)', mgp=c(1.8,0.5,0), cex.lab=0.8)
dev.off()

png('figs/brq_trains.png', pointsize = 12, res=300, width = 900, height = 900, bg='transparent')
  par(mar=c(3.0,3.5,1.5,0.5))  
  binnedplot(log(data$trains), coll.resid.norm, ylab="RQ Residual", xlab="", main='', cex.axis=0.6, las=1, mgp=c(2.5,1,0), cex.lab=0.8, xaxt='n')
  axis(1, cex.axis=0.6, mgp=c(2.5,0.5,0))
  title(xlab='Number of Trains (log-transformed)', mgp=c(1.8,0.5,0), cex.lab=0.8)
dev.off()

png('figs/brq_speed.png', pointsize = 12, res=300, width = 900, height = 900, bg='transparent')
  par(mar=c(3.0,3.5,1.5,0.5))
  binnedplot(data$speed, coll.resid.norm, ylab="RQ Residual", xlab="", main='', cex.axis=0.6, las=1, mgp=c(2.5,1,0), cex.lab=0.8, xaxt='n')
  axis(1, cex.axis=0.6, mgp=c(2.5,0.5,0))
  title(xlab='Train Speed', mgp=c(1.8,0.5,0), cex.lab=0.8)
dev.off()

png('figs/brq_hour.png', pointsize = 12, res=300, width = 900, height = 900, bg='transparent')
  par(mar=c(3.0,3.5,1.5,0.5))
  binnedplot(data$hour, coll.resid.norm, ylab="RQ Residual", xlab="", main='', cex.axis=0.6, las=1, mgp=c(2.5,1,0), cex.lab=0.8, xaxt='n')
  axis(1, cex.axis=0.6, mgp=c(2.5,0.5,0))
  title(xlab='Hour', mgp=c(1.8,0.5,0), cex.lab=0.8)
dev.off()

#####################cross validation#######################

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

N <- 10
R <- 100

registerDoMC(detectCores() - 1)

system.time(
perform.glm.cv <- foreach(i = 1:R, .combine=cbind) %do% {
  set.seed(i*123)
  cv.data <- split(data, sample(1:N, nrow(data), replace=T))
  glm.cv <-foreach(j = 1:N, .combine=cbind) %dopar% {
    data.train <- do.call("rbind", cv.data[-j])
    data.test <- cv.data[[j]]
    fit <- glm(formula=y ~ egk + trains + speed + light + light2 + dawnordusk,
               offset=kilometre,
               family=binomial(link="cloglog"),
               data=data.train)
    
    y <- data.test$y
    p <- predict(fit, data.test, type="response")
    calib <- glm(y~log(p), family=binomial(link=cloglog))
    roc <- roc(y,p)
    rbind(calib_int=calib$coefficients[1], calib_slope=calib$coefficients[2], roc)
  }
  colnames(glm.cv) <- paste0(i,".",seq(1,N,1))
  glm.cv
}
) # 1855 seconds

save(perform.glm.cv,file="output/perform_glm_cv")

##################### Simulate management interventions ################
data.a <- as.data.table(data)
data.b <- as.data.table(cbind(data,"hour"=model.data.bin$hour, "id"=model.data.bin$id, "trips"=model.data.bin$trains))
data.c <- as.data.table(cbind(data,"hour"=model.data.bin$hour, "id"=model.data.bin$id, "length"=model.data.bin$length))

data.b[egk_lc >= 1.91918 & ((hour >= 5 & hour < 9) | (hour >= 16 & hour < 20)) & speed_lc > -0.05173123, mean(trips), by="id"]
sum(data.b[egk_lc >= 1.91918 & ((hour >= 5 & hour < 9) | (hour >= 16 & hour < 20)) & speed_lc > -0.05173123, mean(trips), by="id"]$V1)
data.b[egk_lc >= 1.91918 & ((hour >= 5 & hour < 9) | (hour >= 16 & hour < 20)) & speed > -0.05173123, speed_lc := -0.05173123] #note values for egk and speed are on the transformed scale
#range(data.b[egk >= 2.324645 & ((hour >= 6 & hour < 9) | (hour >= 17 & hour < 20)), speed])

data.c[speed_lc > 0.3537339, .N, by="id"]
sum(data.c[speed_lc > 0.3537339, mean(length), by="id"]$V1)
data.c[speed_lc > 0.3537339, egk_lc := egk_lc-log(2)] #note values for egk and speed are on the transformed scale
#range(data.c[speed > 0.1714123, egk])

manage.a <- sum(predict(coll.glm, data.a, type="response"))

manage.b <- sum(predict(coll.glm, data.b, type="response"))

manage.c <- sum(predict(coll.glm, data.c, type="response"))
