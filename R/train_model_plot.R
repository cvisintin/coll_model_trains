require(ggplot2)

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

set.seed(123)

mu <- link(coll.map.nb, data=data.frame(x1 = seq(0,1,.1),
                                        x2 = mean(model.data$speed100),
                                        x3 = mean(model.data$light),
                                        x4 = mean(model.data$light2),
                                        x5 = mean(model.data$dawnordusk),
                                        x6 = mean(model.data$log_trains)
), n=1000)

mu.mean <- apply(mu,2,mean)
mu.PI <- apply(mu,2,PI)

plot(seq(0,1,.1),exp(mu.mean),type="l", xlab="Kangaroo Occurrence", ylab="Rate of Collisions")
lines(seq(0,1,.1),exp(mu.PI[1,]),lty=2)
lines(seq(0,1,.1),exp(mu.PI[2,]),lty=2)

mu <- link(coll.map.nb, data=data.frame(x1 = mean(model.data$egk),
                                        x2 = seq(0,1.6,.1),
                                        x3 = mean(model.data$light),
                                        x4 = mean(model.data$light2),
                                        x5 = mean(model.data$dawnordusk),
                                        x6 = mean(model.data$log_trains)
), n=1000)

mu.mean <- apply(mu,2,mean)
mu.PI <- apply(mu,2,PI)

plot(seq(0,1.6,.1),exp(mu.mean),type="l", xlab="Speed of Trains (100 km/hour)", ylab="Rate of Collisions")
lines(seq(0,1.6,.1),exp(mu.PI[1,]),lty=2)
lines(seq(0,1.6,.1),exp(mu.PI[2,]),lty=2)

mu <- link(coll.map.nb, data=data.frame(x1 = mean(model.data$egk),
                                        x2 = mean(model.data$speed100),
                                        x3 = sin((2 * pi * (seq(0,23,1) - 6)) / 24),
                                        x4 = (sin((2 * pi * (seq(0,23,1) - 6)) / 24))^2,
                                        x5 = dawn.or.dusk(h=seq(0,23,1),dawn=6,dusk=18),
                                        x6 = mean(model.data$log_trains)
), n=1000)

mu.mean <- apply(mu,2,mean)
mu.PI <- apply(mu,2,PI)

plot(seq(0,23,1),exp(mu.mean),type="l", xlab="Hour", ylab="Rate of Collisions")
lines(seq(0,23,1),exp(mu.PI[1,]),lty=2)
lines(seq(0,23,1),exp(mu.PI[2,]),lty=2)




mu <- link(coll.map.zip, data=data.frame(x1 = seq(0,1,.1),
                                        x2 = mean(model.data$speed100),
                                        x3 = mean(model.data$light),
                                        x4 = mean(model.data$light2),
                                        x5 = mean(model.data$dawnordusk),
                                        x6 = mean(model.data$log_trains)
), n=1000)

mu.mean <- apply(mu$p,2,mean)
mu.PI <- apply(mu$p,2,PI)

plot(seq(0,1,.1),mu.mean,type="l", xlab="Kangaroo Occurrence", ylab="Probability of Zero Collisions")
lines(seq(0,1,.1),mu.PI[1,],lty=2)
lines(seq(0,1,.1),mu.PI[2,],lty=2)

mu <- link(coll.map.zip, data=data.frame(x1 = mean(model.data$egk),
                                        x2 = seq(0,1.6,.1),
                                        x3 = mean(model.data$light),
                                        x4 = mean(model.data$light2),
                                        x5 = mean(model.data$dawnordusk),
                                        x6 = mean(model.data$log_trains)
), n=1000)

mu.mean <- apply(mu$lambda,2,mean)
mu.PI <- apply(mu$lambda,2,PI)

plot(seq(0,1.6,.1),exp(mu.mean),type="l", xlab="Speed of Trains (100 km/hour)", ylab="Rate of Collisions")
lines(seq(0,1.6,.1),exp(mu.PI[1,]),lty=2)
lines(seq(0,1.6,.1),exp(mu.PI[2,]),lty=2)

mu <- link(coll.map.zip, data=data.frame(x1 = mean(model.data$egk),
                                        x2 = mean(model.data$speed100),
                                        x3 = sin((2 * pi * (seq(0,23,1) - 6)) / 24),
                                        x4 = (sin((2 * pi * (seq(0,23,1) - 6)) / 24))^2,
                                        x5 = dawn.or.dusk(h=seq(0,23,1),dawn=6,dusk=18),
                                        x6 = mean(model.data$log_trains)
), n=1000)

mu.mean <- apply(mu$lambda,2,mean)
mu.PI <- apply(mu$lambda,2,PI)

plot(seq(0,23,1),exp(mu.mean),type="l", xlab="Hour", ylab="Rate of Collisions")
lines(seq(0,23,1),exp(mu.PI[1,]),lty=2)
lines(seq(0,23,1),exp(mu.PI[2,]),lty=2)




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
  

n <- nrow(model.data.bin)
y <- model.data.bin$coll
egk <- model.data.bin$egk
speed100 <- model.data.bin$speed100
light <- model.data.bin$light
light2 <- model.data.bin$light2
dawnordusk <- model.data.bin$dawnordusk
offset <- model.data.bin$offset
light <- model.data.bin$light
light2 <- model.data.bin$light2
dawnordusk <- model.data.bin$dawnordusk

model.coefs <- NULL
model.coefs.err  <- NULL
for(i in 1:6){
  model.coefs[i] <- mean(c(unlist(coll.stan.bin@sim$samples[[1]][i])[251:500],unlist(coll.stan.bin@sim$samples[[2]][i])[251:500],unlist(coll.stan.bin@sim$samples[[3]][i])[251:500])) 
  model.coefs.err[i] <- sd(c(unlist(coll.stan.bin@sim$samples[[1]][i])[251:500],unlist(coll.stan.bin@sim$samples[[2]][i])[251:500],unlist(coll.stan.bin@sim$samples[[3]][i])[251:500]))
}
  
png('figs/egk.png', pointsize = 6, res=300, width = 900, height = 900, bg='transparent')
ggplot(data = model.data.bin) +
  geom_line(
    aes(
      x = egk,
      y = invcloglog(
        model.coefs[1] + model.coefs[2]*egk + model.coefs[3]*mean(speed100) + model.coefs[4]*mean(light) + model.coefs[5]*mean(light2) + model.coefs[6]*mean(dawnordusk) + mean(offset)
      )
    )
  ) +
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,.1)) +
  #scale_y_continuous(limits = c(0,.01), breaks = seq(0,.01,.001)) +
  xlab("EGK") +
  ylab("RELATIVE COLLISION LIKELIHOOD") +
  geom_ribbon(
    aes(
      x = egk,
      ymin=invcloglog(
        model.coefs[1] + (model.coefs[2]*egk - model.coefs.err[2]*egk) + model.coefs[3]*mean(speed100) + model.coefs[4]*mean(light) + model.coefs[5]*mean(light2) + model.coefs[6]*mean(dawnordusk) + mean(offset) 
      ),
      ymax=invcloglog(
        model.coefs[1] + (model.coefs[2]*egk + model.coefs.err[2]*egk) + model.coefs[3]*mean(speed100) + model.coefs[4]*mean(light) + model.coefs[5]*mean(light2) + model.coefs[6]*mean(dawnordusk) + mean(offset) 
      )
    ),
    alpha=0.3
  ) +
  theme(plot.background = element_rect(fill = "transparent",colour = NA))
dev.off()

png('figs/speed.png', pointsize = 6, res=300, width = 900, height = 900, bg='transparent')
ggplot(data = model.data.bin) +
  geom_line(
    aes(
      x = speed,
      y = invcloglog(
        model.coefs[1] + model.coefs[2]*mean(egk) + model.coefs[3]*speed100 + model.coefs[4]*mean(light) + model.coefs[5]*mean(light2) + model.coefs[6]*mean(dawnordusk) + mean(offset)
      )
    )
  ) +
  scale_x_continuous(limits = c(10,160), breaks = seq(10,160,10)) +
  #scale_y_continuous(limits = c(0,.01), breaks = seq(0,.01,.001)) +
  xlab("TRAIN SPEED") +
  ylab("RELATIVE COLLISION LIKELIHOOD") +
  geom_ribbon(
    aes(
      x = speed,
      ymin=invcloglog(
        model.coefs[1] + model.coefs[2]*mean(egk) + (model.coefs[3]*speed100 - model.coefs.err[3]*speed100) + model.coefs[4]*mean(light) + model.coefs[5]*mean(light2) + model.coefs[6]*mean(dawnordusk) + mean(offset) 
      ),
      ymax=invcloglog(
        model.coefs[1] + model.coefs[2]*mean(egk) + (model.coefs[3]*speed100 + model.coefs.err[3]*speed100) + model.coefs[4]*mean(light) + model.coefs[5]*mean(light2) + model.coefs[6]*mean(dawnordusk) + mean(offset) 
      )
    ),
    alpha=0.3
  ) +
  theme(plot.background = element_rect(fill = "transparent",colour = NA))
dev.off()

png('figs/hour.png', pointsize = 6, res=300, width = 900, height = 900, bg='transparent')
ggplot(data = model.data.bin) +
  geom_smooth(
    aes(
      x = hour,
      ymin=invcloglog(
        model.coefs[1] + model.coefs[2]*mean(egk) + model.coefs[3]*mean(speed100) + (model.coefs[4]*light - model.coefs.err[4]*light) + (model.coefs[5]*light2 - model.coefs.err[5]*light2) + (model.coefs[6]*dawnordusk - model.coefs.err[6]*dawnordusk) + mean(offset)
      ),
      ymax=invcloglog(
        model.coefs[1] + model.coefs[2]*mean(egk) + model.coefs[3]*mean(speed100) + (model.coefs[4]*light + model.coefs.err[4]*light) + (model.coefs[5]*light2 + model.coefs.err[5]*light2) + (model.coefs[6]*dawnordusk + model.coefs.err[6]*dawnordusk) + mean(offset)      )
    ),
    alpha=0.3,
    col='black',
    lwd=0.6
  ) +
  scale_x_continuous(limits = c(0,23), breaks = seq(0,23,1)) +
  #scale_y_continuous(limits = c(0,.01), breaks = seq(0,.01,.001)) +
  xlab("HOUR") +
  ylab("RELATIVE COLLISION LIKELIHOOD") +
  theme(plot.background = element_rect(fill = "transparent",colour = NA))
dev.off()