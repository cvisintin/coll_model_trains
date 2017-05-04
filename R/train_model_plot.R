require(ggplot2)
require(rstan)
require(plyr)
require(plotROC)

# plotPal <- c("#94d1c7")

#perform.glm.1000 <- read.csv("output/perform_glm_1000.csv", header=T)
load(file="output/coll_glm")
load(file="output/perform_glm_cv")
load(file="data/coll_glm_data")

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
# 
# model.data.bin <- as.data.table(read.delim("data/model_data_hm_bin.csv", header=T, sep=","))
# 
# load("data/coll_stan_bin")
# 
# n <- nrow(model.data.bin)
# y <- model.data.bin$coll
# egk <- model.data.bin$egk
# trains <- model.data.bin$trains
# speed <- model.data.bin$speed
# light <- model.data.bin$light
# light2 <- model.data.bin$light2
# dawn <- model.data.bin$dawn
# dusk <- model.data.bin$dusk
# dawnordusk <- model.data.bin$dawnordusk
# kilometre <- model.data.bin$length
# 
# model.coefs <- get_posterior_mean(coll.stan.bin, pars="b")
# model.coefs.ci <- summary(coll.stan.bin, pars = c("b"), probs = c(0.025, 0.975))$summary[, c("2.5%", "97.5%")]

# model.coefs <- NULL
# model.coefs.err  <- NULL
# for(i in 1:7){
#   model.coefs[i] <- mean(c(unlist(coll.stan.bin@sim$samples[[1]][i])[251:500],unlist(coll.stan.bin@sim$samples[[2]][i])[251:500],unlist(coll.stan.bin@sim$samples[[3]][i])[251:500])) 
#   model.coefs2.5[i] <- sd(c(unlist(coll.stan.bin@sim$samples[[1]][i])[251:500],unlist(coll.stan.bin@sim$samples[[2]][i])[251:500],unlist(coll.stan.bin@sim$samples[[3]][i])[251:500]))
#   model.coefs97.5[i] <- sd(c(unlist(coll.stan.bin@sim$samples[[1]][i])[251:500],unlist(coll.stan.bin@sim$samples[[2]][i])[251:500],unlist(coll.stan.bin@sim$samples[[3]][i])[251:500]))
# }

model.coefs <- coef(coll.glm)
model.coefs.ci <- confint(coll.glm)#cbind(coef(coll.glm) - summary(fit)$coefficients[, 2], coef(coll.glm) + summary(fit)$coefficients[, 2])

names(data)[1]

png('figs/egk.png', pointsize = 12, res=300, width = 900, height = 900, bg='transparent')
x <- seq(0,1,1/nrow(data))[-(1:1)]
ggplot(data) +
  geom_line(aes(x=x,
                y=invcloglog(
                  model.coefs[1] +
                    model.coefs[2]*(log(x)-mean(log(x))) +
                    model.coefs[3]*mean(trains_lc) +
                    model.coefs[4]*mean(speed_lc) +
                    model.coefs[5]*mean(light) +
                    model.coefs[6]*mean(light2) +
                    model.coefs[7]*mean(dawnordusk) +
                    mean(kilometre)
                )
  ),
  size=0.8) +
  geom_ribbon(aes(x=x,
                  ymin=invcloglog(
                    model.coefs[1] +
                      model.coefs.ci[2,1]*(log(x)-mean(log(x))) +
                      model.coefs[3]*mean(trains_lc) +
                      model.coefs[4]*mean(speed_lc) +
                      model.coefs[5]*mean(light) +
                      model.coefs[6]*mean(light2) +
                      model.coefs[7]*mean(dawnordusk) +
                      mean(kilometre)
                  ),
                  ymax=invcloglog(
                    model.coefs[1] +
                      model.coefs.ci[2,2]*(log(x)-mean(log(x))) +
                      model.coefs[3]*mean(trains_lc) +
                      model.coefs[4]*mean(speed_lc) +
                      model.coefs[5]*mean(light) +
                      model.coefs[6]*mean(light2) +
                      model.coefs[7]*mean(dawnordusk) +
                      mean(kilometre)
                  )
  ),
  alpha=0.3) +
  ylab("Likelihood of Collision") +
  xlab("Likelihood of Species Occurrence") +
  theme_bw() +
  theme(legend.key = element_blank()) +
  theme(plot.margin=unit(c(.5,0,.1,.1),"cm")) +
  theme(axis.title.x = element_text(margin=unit(c(.3,0,0,0),"cm"))) +
  theme(axis.title.y = element_text(margin=unit(c(0,.3,0,0),"cm"))) +
  theme(panel.grid.major = element_line(size=0.1),panel.grid.minor = element_line(size=0.1)) +
  theme(text = element_text(size = 10)) +
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,.1)) #+
#scale_y_continuous(limits = c(0,.001), breaks = seq(0,.001,.0001))
dev.off()


png('figs/trains.png', pointsize = 12, res=300, width = 900, height = 900, bg='transparent')
x <- seq(0,60,60/nrow(data))[-(1:1)]
ggplot(data) +
  geom_line(aes(x=x,
                y=invcloglog(
                  model.coefs[1] +
                    model.coefs[2]*mean(egk_lc) +
                    model.coefs[3]*(log(x)-mean(log(x))) +
                    model.coefs[4]*mean(speed_lc) +
                    model.coefs[5]*mean(light) +
                    model.coefs[6]*mean(light2) +
                    model.coefs[7]*mean(dawnordusk) +
                    mean(kilometre)
                )
  ),
  size=0.8) +
  geom_ribbon(aes(x=x,
                  ymin=invcloglog(
                    model.coefs[1] +
                      model.coefs[2]*mean(egk_lc) +
                      model.coefs.ci[3,1]*(log(x)-mean(log(x))) +
                      model.coefs[4]*mean(speed_lc) +
                      model.coefs[5]*mean(light) +
                      model.coefs[6]*mean(light2) +
                      model.coefs[7]*mean(dawnordusk) +
                      mean(kilometre)
                  ),
                  ymax=invcloglog(
                    model.coefs[1] +
                      model.coefs[2]*mean(egk_lc) +
                      model.coefs.ci[3,2]*(log(x)-mean(log(x))) +
                      model.coefs[4]*mean(speed_lc) +
                      model.coefs[5]*mean(light) +
                      model.coefs[6]*mean(light2) +
                      model.coefs[7]*mean(dawnordusk) +
                      mean(kilometre)
                  )
  ),
  alpha=0.3) +
  ylab("Likelihood of Collision") +
  xlab("Number of Trains") +
  theme_bw() +
  theme(legend.key = element_blank()) +
  theme(plot.margin=unit(c(.5,0,.1,.1),"cm")) +
  theme(axis.title.x = element_text(margin=unit(c(.3,0,0,0),"cm"))) +
  theme(axis.title.y = element_text(margin=unit(c(0,.3,0,0),"cm"))) +
  theme(panel.grid.major = element_line(size=0.1),panel.grid.minor = element_line(size=0.1)) +
  theme(text = element_text(size = 10)) +
  scale_x_continuous(limits = c(0,60), breaks = seq(0,60,10)) #+
#scale_y_continuous(limits = c(0,.001), breaks = seq(0,.001,.0001))
dev.off()


png('figs/speed.png', pointsize = 12, res=300, width = 900, height = 900, bg='transparent')
x <- seq(10,160,150/nrow(data))[1:nrow(data)]
ggplot(data) +
  geom_line(aes(x=x,
                y=invcloglog(
                  model.coefs[1] +
                    model.coefs[2]*mean(egk_lc) +
                    model.coefs[3]*mean(trains_lc) +
                    model.coefs[4]*(log(x)-mean(log(x))) +
                    model.coefs[5]*mean(light) +
                    model.coefs[6]*mean(light2) +
                    model.coefs[7]*mean(dawnordusk) +
                    mean(kilometre)
                )
  ),
  size=0.8) +
  geom_ribbon(aes(x=x,
                  ymin=invcloglog(
                    model.coefs[1] +
                      model.coefs[2]*mean(egk_lc) +
                      model.coefs[3]*mean(trains_lc) +
                      model.coefs.ci[4,1]*(log(x)-mean(log(x))) +
                      model.coefs[5]*mean(light) +
                      model.coefs[6]*mean(light2) +
                      model.coefs[7]*mean(dawnordusk) +
                      mean(kilometre)
                  ),
                  ymax=invcloglog(
                    model.coefs[1] +
                      model.coefs[2]*mean(egk_lc) +
                      model.coefs[3]*mean(trains_lc) +
                      model.coefs.ci[4,2]*(log(x)-mean(log(x))) +
                      model.coefs[5]*mean(light) +
                      model.coefs[6]*mean(light2) +
                      model.coefs[7]*mean(dawnordusk) +
                      mean(kilometre)
                  )
  ),
  alpha=0.3) +
  ylab("Likelihood of Collision") +
  xlab("Train Speed (km/hr)") +
  theme_bw() +
  theme(legend.key = element_blank()) +
  theme(plot.margin=unit(c(.5,0,.1,.1),"cm")) +
  theme(axis.title.x = element_text(margin=unit(c(.3,0,0,0),"cm"))) +
  theme(axis.title.y = element_text(margin=unit(c(0,.3,0,0),"cm"))) +
  theme(panel.grid.major = element_line(size=0.1),panel.grid.minor = element_line(size=0.1)) +
  theme(text = element_text(size = 10)) +
  scale_x_continuous(limits = c(10,160), breaks = seq(10,160,25)) #+
#scale_y_continuous(limits = c(0,.012), breaks = seq(0,.012,.0012))
dev.off()

png('figs/hour.png', pointsize = 12, res=300, width = 900, height = 900, bg='transparent')
x <- seq(0,24,24/nrow(data))[-(1:1)]
ggplot(data) +
  geom_smooth(aes(x=x,
                y=invcloglog(
                  model.coefs[1] +
                    model.coefs[2]*mean(egk_lc) +
                    model.coefs[3]*mean(trains_lc) +
                    model.coefs[4]*mean(speed_lc) +
                    model.coefs[5]*(sin((2 * pi * (x - 6)) / 24)-mean(sin((2 * pi * (x - 6)) / 24))) +
                    model.coefs[6]*(sin((2 * pi * (x - 6)) / 24)^2-mean(sin((2 * pi * (x - 6)) / 24)^2)) +
                    model.coefs[7]*(dawn.or.dusk(h=x,dawn=dawn,dusk=dusk)-mean(dawn.or.dusk(h=x,dawn=dawn,dusk=dusk))) +
                    mean(kilometre)
                )
  ),
  size=0.8, col='black') +
  geom_ribbon(aes(x=x,
                  ymin=invcloglog(
                    model.coefs[1] +
                      model.coefs[2]*mean(egk_lc) +
                      model.coefs[3]*mean(trains_lc) +
                      model.coefs[4]*mean(speed_lc) +
                      model.coefs.ci[5,1]*(sin((2 * pi * (x - 6)) / 24)-mean(sin((2 * pi * (x - 6)) / 24))) +
                      model.coefs.ci[6,1]*(sin((2 * pi * (x - 6)) / 24)^2-mean(sin((2 * pi * (x - 6)) / 24)^2)) +
                      model.coefs.ci[7,1]*(dawn.or.dusk(h=x,dawn=dawn,dusk=dusk)-mean(dawn.or.dusk(h=x,dawn=dawn,dusk=dusk))) +
                      mean(kilometre)
                  ),
                  ymax=invcloglog(
                    model.coefs[1] +
                      model.coefs[2]*mean(egk_lc) +
                      model.coefs[3]*mean(trains_lc) +
                      model.coefs[4]*mean(speed_lc) +
                      model.coefs.ci[5,2]*(sin((2 * pi * (x - 6)) / 24)-mean(sin((2 * pi * (x - 6)) / 24))) +
                      model.coefs.ci[6,2]*(sin((2 * pi * (x - 6)) / 24)^2-mean(sin((2 * pi * (x - 6)) / 24)^2)) +
                      model.coefs.ci[7,2]*(dawn.or.dusk(h=x,dawn=dawn,dusk=dusk)-mean(dawn.or.dusk(h=x,dawn=dawn,dusk=dusk))) +
                      mean(kilometre)
                  )
  ),
  alpha=0.3) +
  ylab("Likelihood of Collision") +
  xlab("Hour") +
  theme_bw() +
  theme(legend.key = element_blank()) +
  theme(plot.margin=unit(c(.5,0,.1,.1),"cm")) +
  theme(axis.title.x = element_text(margin=unit(c(.3,0,0,0),"cm"))) +
  theme(axis.title.y = element_text(margin=unit(c(0,.3,0,0),"cm"))) +
  theme(panel.grid.major = element_line(size=0.1),panel.grid.minor = element_line(size=0.1)) +
  theme(text = element_text(size = 10)) +
  scale_x_continuous(limits = c(0,24), breaks = seq(0,24,2)) #+
#scale_y_continuous(limits = c(0,.002), breaks = seq(0,.002,.0002))
dev.off()






# png('figs/egk.png', pointsize = 12, res=300, width = 900, height = 900, bg='transparent')
# ggplot(model.data.bin) +
#   geom_line(aes(x=seq(0,1,1/nrow(model.data.bin))[1:nrow(model.data.bin)],
#                 y=invcloglog(
#                   model.coefs[1,4] +
#                     model.coefs[2,4]*(log(x)-mean(log(x))) +
#                     model.coefs[3,4]*mean(log(trains)-mean(log(trains))) +
#                     model.coefs[4,4]*mean(log(speed)-mean(log(speed))) +
#                     model.coefs[5,4]*mean(light-mean(light)) +
#                     model.coefs[6,4]*mean(light2-mean(light2)) +
#                     model.coefs[7,4]*mean(dawnordusk-mean(dawnordusk)) +
#                     mean(log(kilometre))
#                 )
#   ),
#   size=0.5) +
#   geom_ribbon(aes(x=seq(0,1,1/nrow(model.data.bin))[1:nrow(model.data.bin)],
#                   ymin=invcloglog(
#                     model.coefs[1,4] +
#                       model.coefs.ci[2,1]*(log(x)-mean(log(x))) +
#                       model.coefs[3,4]*mean(log(trains)-mean(log(trains))) +
#                       model.coefs[4,4]*mean(log(speed)-mean(log(speed))) +
#                       model.coefs[5,4]*mean(light-mean(light)) +
#                       model.coefs[6,4]*mean(light2-mean(light2)) +
#                       model.coefs[7,4]*mean(dawnordusk-mean(dawnordusk)) +
#                       mean(log(kilometre))
#                   ),
#                   ymax=invcloglog(
#                     model.coefs[1,4] +
#                       model.coefs.ci[2,2]*(log(x)-mean(log(x))) +
#                       model.coefs[3,4]*mean(log(trains)-mean(log(trains))) +
#                       model.coefs[4,4]*mean(log(speed)-mean(log(speed))) +
#                       model.coefs[5,4]*mean(light-mean(light)) +
#                       model.coefs[6,4]*mean(light2-mean(light2)) +
#                       model.coefs[7,4]*mean(dawnordusk-mean(dawnordusk)) +
#                       mean(log(kilometre))
#                   )
#   ),
#   alpha=0.2) +
#   ylab("Likelihood of Collision") +
#   xlab("Likelihood of Species Occurrence") +
#   theme_bw() +
#   theme(legend.key = element_blank()) +
#   theme(plot.margin=unit(c(.5,0,.1,.1),"cm")) +
#   theme(axis.title.x = element_text(margin=unit(c(.3,0,0,0),"cm"))) +
#   theme(axis.title.y = element_text(margin=unit(c(0,.3,0,0),"cm"))) +
#   theme(panel.grid.major = element_line(size=0.1),panel.grid.minor = element_line(size=0.1)) +
#   theme(text = element_text(size = 10)) +
#   scale_x_continuous(limits = c(0,1), breaks = seq(0,1,.1)) #+
# #scale_y_continuous(limits = c(0,.001), breaks = seq(0,.001,.0001))
# dev.off()
# 
# 
# png('figs/trains.png', pointsize = 12, res=300, width = 900, height = 900, bg='transparent')
# ggplot(model.data.bin) +
#   geom_line(aes(x=seq(0,60,60/nrow(model.data.bin))[1:nrow(model.data.bin)],
#                 y=invcloglog(
#                   model.coefs[1,4] +
#                     model.coefs[2,4]*mean(log(egk)-mean(log(egk))) +
#                     model.coefs[3,4]*(log(x)-mean(log(x))) +
#                     model.coefs[4,4]*mean(log(speed)-mean(log(speed))) +
#                     model.coefs[5,4]*mean(light-mean(light)) +
#                     model.coefs[6,4]*mean(light2-mean(light2)) +
#                     model.coefs[7,4]*mean(dawnordusk-mean(dawnordusk)) +
#                     mean(log(kilometre))
#                 )
#   ),
#   size=0.5) +
#   geom_ribbon(aes(x=seq(0,60,60/nrow(model.data.bin))[1:nrow(model.data.bin)],
#                   ymin=invcloglog(
#                     model.coefs[1,4] +
#                       model.coefs[2,4]*mean(log(egk)-mean(log(egk))) +
#                       model.coefs.ci[3,1]*(log(x)-mean(log(x))) +
#                       model.coefs[4,4]*mean(log(speed)-mean(log(speed))) +
#                       model.coefs[5,4]*mean(light-mean(light)) +
#                     theme(plot.background = element_rect(fill = "transparent",colour = NA), axis.text.y = element_blank())  model.coefs[6,4]*mean(light2-mean(light2)) +
#                       model.coefs[7,4]*mean(dawnordusk-mean(dawnordusk)) +
#                       mean(log(kilometre))
#                   ),
#                   ymax=invcloglog(
#                     model.coefs[1,4] +
#                       model.coefs[2,4]*mean(log(egk)-mean(log(egk))) +
#                       model.coefs.ci[3,2]*(log(x)-mean(log(x))) +
#                       model.coefs[4,4]*mean(log(speed)-mean(log(speed))) +
#                       model.coefs[5,4]*mean(light-mean(light)) +
#                       model.coefs[6,4]*mean(light2-mean(light2)) +
#                       model.coefs[7,4]*mean(dawnordusk-mean(dawnordusk)) +
#                       mean(log(kilometre))
#                   )
#   ),
#   alpha=0.2) +
#   ylab("Likelihood of Collision") +
#   xlab("Number of Trains") +
#   theme_bw() +
#   theme(legend.key = element_blank()) +
#   theme(plot.margin=unit(c(.5,0,.1,.1),"cm")) +
#   theme(axis.title.x = element_text(margin=unit(c(.3,0,0,0),"cm"))) +
#   theme(axis.title.y = element_text(margin=unit(c(0,.3,0,0),"cm"))) +
#   theme(panel.grid.major = element_line(size=0.1),panel.grid.minor = element_line(size=0.1)) +
#   theme(text = element_text(size = 10)) +
#   scale_x_continuous(limits = c(0,60), breaks = seq(0,60,10)) #+
# #scale_y_continuous(limits = c(0,.001), breaks = seq(0,.001,.0001))
# dev.off()
# 
# 
# png('figs/speed.png', pointsize = 12, res=300, width = 900, height = 900, bg='transparent')
# ggplot(data = model.data.bin) +
#   geom_line(aes(x=seq(10,160,150/nrow(model.data.bin))[1:nrow(model.data.bin)],
#                 y=invcloglog(
#                   model.coefs[1,4] +
#                     model.coefs[2,4]*mean(log(egk)-mean(log(egk))) +
#                     model.coefs[3,4]*mean(log(trains)-mean(log(trains))) +
#                     model.coefs[4,4]*(log(x)-mean(log(x))) +
#                     model.coefs[5,4]*mean(light-mean(light)) +
#                     model.coefs[6,4]*mean(light2-mean(light2)) +
#                     model.coefs[7,4]*mean(dawnordusk-mean(dawnordusk)) +
#                     mean(log(kilometre))
#                 )
#   ),
#   size=0.5) +
#   geom_ribbon(aes(x=seq(10,160,150/nrow(model.data.bin))[1:nrow(model.data.bin)],
#                   ymin=invcloglog(
#                     model.coefs[1,4] +
#                       model.coefs[2,4]*mean(log(egk)-mean(log(egk))) +
#                       model.coefs[3,4]*mean(log(trains)-mean(log(trains))) +
#                       model.coefs.ci[4,1]*(log(x)-mean(log(x))) +
#                       model.coefs[5,4]*mean(light-mean(light)) +
#                       model.coefs[6,4]*mean(light2-mean(light2)) +
#                       model.coefs[7,4]*mean(dawnordusk-mean(dawnordusk)) +
#                       mean(log(kilometre))
#                   ),
#                   ymax=invcloglog(
#                     model.coefs[1,4] +
#                       model.coefs[2,4]*mean(log(egk)-mean(log(egk))) +
#                       model.coefs[3,4]*mean(log(trains)-mean(log(trains))) +
#                       model.coefs.ci[4,2]*(log(x)-mean(log(x))) +
#                       model.coefs[5,4]*mean(light-mean(light)) +
#                       model.coefs[6,4]*mean(light2-mean(light2)) +
#                       model.coefs[7,4]*mean(dawnordusk-mean(dawnordusk)) +
#                       mean(log(kilometre))
#                   )
#   ),
#   alpha=0.2) +
#   ylab("Likelihood of Collision") +
#   xlab("Train Speed (km/hr)") +
#   theme_bw() +
#   theme(legend.key = element_blank()) +
#   theme(plot.margin=unit(c(.5,0,.1,.1),"cm")) +
#   theme(axis.title.x = element_text(margin=unit(c(.3,0,0,0),"cm"))) +
#   theme(axis.title.y = element_text(margin=unit(c(0,.3,0,0),"cm"))) +
#   theme(panel.grid.major = element_line(size=0.1),panel.grid.minor = element_line(size=0.1)) +
#   theme(text = element_text(size = 10)) +
#   scale_x_continuous(limits = c(10,160), breaks = seq(10,160,25)) #+
# #scale_y_continuous(limits = c(0,.012), breaks = seq(0,.012,.0012))
# dev.off()
# 
# 
# png('figs/hour.png', pointsize = 12, res=300, width = 900, height = 900, bg='transparent')
# ggplot(data = model.data.bin) +
#   geom_line(aes(x=seq(0,24,24/nrow(model.data.bin))[-1],
#                 y=invcloglog(
#                   model.coefs[1,4] +
#                     model.coefs[2,4]*mean(log(egk)-mean(log(egk))) +
#                     model.coefs[3,4]*mean(log(trains)-mean(log(trains))) +
#                     model.coefs[4,4]*mean(log(speed)-mean(log(speed))) +
#                     model.coefs[5,4]*(sin((2 * pi * (x - 6)) / 24)-mean(sin((2 * pi * (x - 6)) / 24))) +
#                     model.coefs[6,4]*(sin((2 * pi * (x - 6)) / 24)^2-mean(sin((2 * pi * (x - 6)) / 24)^2)) +
#                     model.coefs[7,4]*(dawn.or.dusk(h=x,dawn=dawn,dusk=dusk)-mean(dawn.or.dusk(h=x,dawn=dawn,dusk=dusk))) +
#                     mean(log(kilometre))
#                 )
#   ),
#   size=0.5) +
#   geom_ribbon(aes(x=seq(0,24,24/nrow(model.data.bin))[-1],
#                   ymin=invcloglog(
#                     model.coefs[1,4] +
#                       model.coefs[2,4]*mean(log(egk)-mean(log(egk))) +
#                       model.coefs[3,4]*mean(log(trains)-mean(log(trains))) +
#                       model.coefs[4,4]*mean(log(speed)-mean(log(speed))) +
#                       model.coefs.ci[5,1]*(sin((2 * pi * (x - 6)) / 24)-mean(sin((2 * pi * (x - 6)) / 24))) +
#                       model.coefs.ci[6,1]*(sin((2 * pi * (x - 6)) / 24)^2-mean(sin((2 * pi * (x - 6)) / 24)^2)) +
#                       model.coefs.ci[7,1]*(dawn.or.dusk(h=x,dawn=dawn,dusk=dusk)-mean(dawn.or.dusk(h=x,dawn=dawn,dusk=dusk))) +
#                       mean(log(kilometre))
#                   ),
#                   ymax=invcloglog(
#                     model.coefs[1,4] +
#                       model.coefs[2,4]*mean(log(egk)-mean(log(egk))) +
#                       model.coefs[3,4]*mean(log(trains)-mean(log(trains))) +
#                       model.coefs[4,4]*mean(log(speed)-mean(log(speed))) +
#                       model.coefs.ci[5,2]*(sin((2 * pi * (x - 6)) / 24)-mean(sin((2 * pi * (x - 6)) / 24))) +
#                       model.coefs.ci[6,2]*(sin((2 * pi * (x - 6)) / 24)^2-mean(sin((2 * pi * (x - 6)) / 24)^2)) +
#                       model.coefs.ci[7,2]*(dawn.or.dusk(h=x,dawn=dawn,dusk=dusk)-mean(dawn.or.dusk(h=x,dawn=dawn,dusk=dusk))) +
#                       mean(log(kilometre))
#                   )
#   ),
#   alpha=0.2) +
#   ylab("Likelihood of Collision") +
#   xlab("Hour") +
#   theme_bw() +
#   theme(legend.key = element_blank()) +
#   theme(plot.margin=unit(c(.5,0,.1,.1),"cm")) +
#   theme(axis.title.x = element_text(margin=unit(c(.3,0,0,0),"cm"))) +
#   theme(axis.title.y = element_text(margin=unit(c(0,.3,0,0),"cm"))) +
#   theme(panel.grid.major = element_line(size=0.1),panel.grid.minor = element_line(size=0.1)) +
#   theme(text = element_text(size = 10)) +
#   scale_x_continuous(limits = c(0,24), breaks = seq(0,24,2)) #+
# #scale_y_continuous(limits = c(0,.002), breaks = seq(0,.002,.0002))
# dev.off()



# png('figs/egk.png', pointsize = 6, res=300, width = 1100, height = 900, bg='transparent')
# ggplot(data = model.data.bin) +
#   geom_line(
#     aes(
#       x = egk,
#       y = invcloglog(
#         model.coefs[1] + model.coefs[2]*egk + model.coefs[3]*mean(speed100) + model.coefs[4]*mean(light) + model.coefs[5]*mean(light2) + model.coefs[6]*mean(dawnordusk) + mean(offset)
#       )
#     )
#   ) +
#   scale_x_continuous(limits = c(0,1), breaks = seq(0,1,.1)) +
#   scale_y_continuous(limits = c(-.001,.006), breaks = seq(0,.006,.001)) +
#   xlab("EGK (Pr(occ=1))") +
#   ylab("RELATIVE COLLISION RATE") +
#   geom_ribbon(
#     aes(
#       x = egk,
#       ymin=invcloglog(
#         model.coefs[1] + (model.coefs[2]*egk - model.coefs.err[2]*egk) + model.coefs[3]*mean(speed100) + model.coefs[4]*mean(light) + model.coefs[5]*mean(light2) + model.coefs[6]*mean(dawnordusk) + mean(offset) 
#       ),
#       ymax=invcloglog(
#         model.coefs[1] + (model.coefs[2]*egk + model.coefs.err[2]*egk) + model.coefs[3]*mean(speed100) + model.coefs[4]*mean(light) + model.coefs[5]*mean(light2) + model.coefs[6]*mean(dawnordusk) + mean(offset) 
#       )
#     ),
#     alpha=0.2
#   ) +
#   theme(plot.background = element_rect(fill = "transparent",colour = NA))
# dev.off()
# 
# png('figs/speed.png', pointsize = 6, res=300, width = 1100, height = 900, bg='transparent')
# ggplot(data = model.data.bin) +
#   geom_line(
#     aes(
#       x = speed,
#       y = invcloglog(
#         model.coefs[1] + model.coefs[2]*mean(egk) + model.coefs[3]*speed100 + model.coefs[4]*mean(light) + model.coefs[5]*mean(light2) + model.coefs[6]*mean(dawnordusk) + mean(offset)
#       )
#     )
#   ) +
#   scale_x_continuous(limits = c(10,150), breaks = seq(10,150,20)) +
#   scale_y_continuous(limits = c(-.001,.006), breaks = seq(0,.006,.001)) +
#   xlab("TRAIN SPEED (km/hr)") +
#   ylab("") +
#   geom_ribbon(
#     aes(
#       x = speed,
#       ymin=invcloglog(
#         model.coefs[1] + model.coefs[2]*mean(egk) + (model.coefs[3]*speed100 - model.coefs.err[3]*speed100) + model.coefs[4]*mean(light) + model.coefs[5]*mean(light2) + model.coefs[6]*mean(dawnordusk) + mean(offset) 
#       ),
#       ymax=invcloglog(
#         model.coefs[1] + model.coefs[2]*mean(egk) + (model.coefs[3]*speed100 + model.coefs.err[3]*speed100) + model.coefs[4]*mean(light) + model.coefs[5]*mean(light2) + model.coefs[6]*mean(dawnordusk) + mean(offset) 
#       )
#     ),
#     alpha=0.2
#   ) +
#   theme(plot.background = element_rect(fill = "transparent",colour = NA), axis.text.y = element_blank())
# dev.off()
# 
# png('figs/hour.png', pointsize = 6, res=300, width = 1100, height = 900, bg='transparent')
# ggplot(data = model.data.bin) +
#   geom_smooth(
#     aes(
#       x = hour,
#       ymin=invcloglog(
#         model.coefs[1] + model.coefs[2]*mean(egk) + model.coefs[3]*mean(speed100) + (model.coefs[4]*light - model.coefs.err[4]*light) + (model.coefs[5]*light2 - model.coefs.err[5]*light2) + (model.coefs[6]*dawnordusk - model.coefs.err[6]*dawnordusk) + mean(offset)
#       ),
#       ymax=invcloglog(
#         model.coefs[1] + model.coefs[2]*mean(egk) + model.coefs[3]*mean(speed100) + (model.coefs[4]*light + model.coefs.err[4]*light) + (model.coefs[5]*light2 + model.coefs.err[5]*light2) + (model.coefs[6]*dawnordusk + model.coefs.err[6]*dawnordusk) + mean(offset)      )
#     ),
#     alpha=0.3,
#     col='black',
#     lwd=0.6
#   ) +
#   scale_x_continuous(limits = c(0,23), breaks = seq(0,23,2)) +
#   scale_y_continuous(limits = c(-.001,.006), breaks = seq(0,.006,.001)) +
#   xlab("HOUR") +
#   ylab("") +
#   theme(plot.background = element_rect(fill = "transparent",colour = NA), axis.text.y = element_blank())
# dev.off()

#####################################

y <- coll.glm$data$y
p <- predict(coll.glm, coll.glm$data, type="response")

p.b <- .bincode(p,seq(min(p),max(p),(max(p)-min(p))/10),include.lowest = TRUE)

yp_bins <- data.frame(y,p,p.b)


plot.info <- ddply(yp_bins, ~p.b, summarise,
                   count=length(y),
                   prop_coll=sum(y)/length(y),
                   prop_nocoll=(length(y)-sum(y))/length(y),
                   prop_lo=binom.test(sum(y), length(y), 0.5)$conf.int[1],
                   prop_hi=binom.test(sum(y), length(y), 0.5)$conf.int[2],
                   median_p=median(p)
)

calib <- glm(y~log(p), family=binomial(link=cloglog), data=yp_bins)
roc <- roc(y,p)
perform.glm <- rbind(calib_int=calib$coefficients[1], calib_slope=calib$coefficients[2], roc)
colnames(perform.glm) <- "0.0"

png('figs/calib.png', pointsize = 12, res=300, width = 900, height = 900, bg='transparent')
#png('figs/calib.png', pointsize = 12, res=300, width = 1400, height = 900, bg='transparent')
ggplot() +
  #geom_smooth(data=plot.glm, aes(y=y,x=x), formula=y~log(x), method=glm, size = 0.2, colour='black', inherit.aes=FALSE) +
  geom_line(data=yp_bins, aes(y=invcloglog(calib$coefficients[1]+calib$coefficients[2]*log(p)),x=p)) +
  geom_pointrange(data=plot.info, aes(x=median_p, y=prop_coll, ymin=prop_lo, ymax=prop_hi), size = 0.2, inherit.aes=FALSE) +
  geom_text(data=plot.info, aes(x=median_p, y=prop_coll, label=count),hjust=-0.3, vjust=-1, angle = 90, size = 2.0, inherit.aes=FALSE) +
  geom_segment(aes(x = 0, y = 0, xend = .04, yend = .04), linetype=2, size=0.1, inherit.aes=FALSE) +
  #coord_flip() +
  ylab("Observed Rate") +
  xlab("Predicted Rate") +
  theme_bw() +
  theme(plot.margin=unit(c(.5,0,.1,.1),"cm")) +
  theme(axis.title.x = element_text(margin=unit(c(.3,0,0,0),"cm"))) +
  theme(axis.title.y = element_text(margin=unit(c(0,.3,0,0),"cm"))) +
  theme(panel.grid.major = element_line(size=0.1),panel.grid.minor = element_line(size=0.1)) +
  theme(text = element_text(size = 12))
dev.off()

#ggplot(yp_bins, aes(x=p)) + geom_density(aes(group=y, colour=y, fill=y), alpha=0.3)

png('figs/roc.png', pointsize = 12, res=300, width = 900, height = 900, bg='transparent')
ggplot() +
  stat_roc(data=yp_bins, aes(d=y,m=p), n.cuts = 0, size=0.5) +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), linetype=2, size=0.1, inherit.aes=FALSE) +
  ylab("True Positive Fraction") +
  xlab("False Positive Fraction") +
  theme_bw() +
  theme(plot.margin=unit(c(.5,0,.1,.1),"cm")) +
  theme(axis.title.x = element_text(margin=unit(c(.3,0,0,0),"cm"))) +
  theme(axis.title.y = element_text(margin=unit(c(0,.3,0,0),"cm"))) +
  theme(panel.grid.major = element_line(size=0.1),panel.grid.minor = element_line(size=0.1)) +
  theme(text = element_text(size = 12))
dev.off()

perform.glm.1000 <- cbind("full_model"=signif(perform.glm, digits=4),
                          "cv_model_mean"=signif(apply(perform.glm.cv,1,mean), digits=4),
                          "cv_model_sd"=signif(apply(perform.glm.cv,1,sd), digits=4),
                          "cv_model_rlo"=signif(apply(perform.glm.cv,1,range), digits=4)[1,],
                          "cv_model_rhi"=signif(apply(perform.glm.cv,1,range), digits=4)[2,]
)

#write.csv(perform.glm.1000,"output/perform_glm_1000.csv",row.names=FALSE)

metrics <- factor(c("Intercept","Slope","ROC"),
                  levels=rev(c("Intercept","Slope","ROC")))

cv_plot <- transform(data.frame(x = metrics, y = perform.glm.1000[, 2], y_orig = perform.glm.1000[, 1]),
                  ylo=perform.glm.1000[, 2] - 2*perform.glm.1000[, 3],
                  yhi=perform.glm.1000[, 2] + 2*perform.glm.1000[, 3]
                  )

png('figs/validate.png', pointsize = 12, res=300, width = 900, height = 900, bg='transparent')
p <- ggplot() +
  geom_pointrange(data=cv_plot, aes(x=x, y=y, ymin=ylo, ymax=yhi), size = 0.1) +
  #geom_hline(yintercept = 0, linetype=2, size=0.2) +
  coord_flip() +
  ylab("Value") +
  xlab("Test Metric") +
  theme_bw() +
  theme(plot.margin=unit(c(.5,0,.1,.1),"cm")) +
  theme(axis.title.x = element_text(margin=unit(c(.3,0,0,0),"cm"))) +
  theme(axis.title.y = element_text(margin=unit(c(0,.3,0,0),"cm"))) +
  theme(panel.grid.major = element_line(size=0.1),panel.grid.minor = element_line(size=0.1)) +
  theme(text = element_text(size = 12))
p + geom_point(data=cv_plot, aes(x=x, y=y_orig), size = 2, shape=1, inherit.aes=FALSE) +
  geom_segment(aes(x = 2.5, y = 0, xend = 3.5, yend = 0), linetype=2, size=0.1) +
  geom_segment(aes(x = 0.5, y = 1, xend = 2.5, yend = 1), linetype=2, size=0.1)
dev.off()

#####################Plots for VicBio Conf talk######################

pdf('/home/casey/Research/Projects/VicBioConf/graphics/tspd3.pdf', pointsize = 16)
x <- seq(0.01,160,.1)
ggplot() +
  geom_line(aes(x=x,
                y=invcloglog(
                  model.coefs[1] +
                    model.coefs[2]*mean(data$egk_lc) +
                    model.coefs[3]*mean(data$trains_lc) +
                    model.coefs[4]*(log(x)-mean(log(x))) +
                    model.coefs[5]*mean(data$light) +
                    model.coefs[6]*mean(data$light2) +
                    model.coefs[7]*mean(data$dawnordusk) +
                    mean(data$kilometre)
                )
  ),
  size=0.5) +
  # geom_ribbon(aes(x=x,
  #                 ymin=invcloglog(
  #                   model.coefs[1] +
  #                     model.coefs[2]*mean(egk_lc) +
  #                     model.coefs[3]*mean(trains_lc) +
  #                     model.coefs.ci[4,1]*(log(x)-mean(log(x))) +
  #                     model.coefs[5]*mean(light) +
  #                     model.coefs[6]*mean(light2) +
  #                     model.coefs[7]*mean(dawnordusk) +
  #                     mean(kilometre)
  #                 ),
  #                 ymax=invcloglog(
  #                   model.coefs[1] +
  #                     model.coefs[2]*mean(egk_lc) +
  #                     model.coefs[3]*mean(trains_lc) +
  #                     model.coefs.ci[4,2]*(log(x)-mean(log(x))) +
  #                     model.coefs[5]*mean(light) +
  #                     model.coefs[6]*mean(light2) +
  #                     model.coefs[7]*mean(dawnordusk) +
  #                     mean(kilometre)
  #                 )
  # ),
  # alpha=0.2) +
  ylab("Relative Collision Rate") +
  xlab("Train Speed (km/hr)") +
  theme_bw() +
  theme(legend.key = element_blank()) +
  theme(plot.margin=unit(c(.5,.5,.1,.1),"cm")) +
  theme(axis.title.x = element_text(margin=unit(c(.3,0,0,0),"cm"))) +
  theme(axis.title.y = element_text(margin=unit(c(0,.3,0,0),"cm"))) +
  theme(panel.grid.major = element_line(size=0.1),panel.grid.minor = element_line(size=0.1)) +
  theme(text = element_text(size = 16)) +
  scale_x_continuous(breaks = seq(0,160,20), expand = c(0, 0), lim = c(0,160)) #+
#scale_y_continuous(limits = c(0,.012), breaks = seq(0,.012,.0012))
dev.off()

pdf('/home/casey/Research/Projects/VicBioConf/graphics/hour.pdf', pointsize = 16)
x <- seq(0,24,24/nrow(data))[-(1:1)]
ggplot(data) +
  geom_smooth(aes(x=x,
                  y=invcloglog(
                    model.coefs[1] +
                      model.coefs[2]*mean(egk_lc) +
                      model.coefs[3]*mean(trains_lc) +
                      model.coefs[4]*mean(speed_lc) +
                      model.coefs[5]*(sin((2 * pi * (x - 6)) / 24)-mean(sin((2 * pi * (x - 6)) / 24))) +
                      model.coefs[6]*(sin((2 * pi * (x - 6)) / 24)^2-mean(sin((2 * pi * (x - 6)) / 24)^2)) +
                      model.coefs[7]*(dawn.or.dusk(h=x,dawn=dawn,dusk=dusk)-mean(dawn.or.dusk(h=x,dawn=dawn,dusk=dusk))) +
                      mean(kilometre)
                  )
  ),
  size=0.5, col='black') +
  # geom_ribbon(aes(x=x,
  #                 ymin=invcloglog(
  #                   model.coefs[1] +
  #                     model.coefs[2]*mean(egk_lc) +
  #                     model.coefs[3]*mean(trains_lc) +
  #                     model.coefs[4]*mean(speed_lc) +
  #                     model.coefs.ci[5,1]*(sin((2 * pi * (x - 6)) / 24)-mean(sin((2 * pi * (x - 6)) / 24))) +
  #                     model.coefs.ci[6,1]*(sin((2 * pi * (x - 6)) / 24)^2-mean(sin((2 * pi * (x - 6)) / 24)^2)) +
  #                     model.coefs.ci[7,1]*(dawn.or.dusk(h=x,dawn=dawn,dusk=dusk)-mean(dawn.or.dusk(h=x,dawn=dawn,dusk=dusk))) +
  #                     mean(kilometre)
  #                 ),
  #                 ymax=invcloglog(
  #                   model.coefs[1] +
  #                     model.coefs[2]*mean(egk_lc) +
  #                     model.coefs[3]*mean(trains_lc) +
  #                     model.coefs[4]*mean(speed_lc) +
  #                     model.coefs.ci[5,2]*(sin((2 * pi * (x - 6)) / 24)-mean(sin((2 * pi * (x - 6)) / 24))) +
  #                     model.coefs.ci[6,2]*(sin((2 * pi * (x - 6)) / 24)^2-mean(sin((2 * pi * (x - 6)) / 24)^2)) +
  #                     model.coefs.ci[7,2]*(dawn.or.dusk(h=x,dawn=dawn,dusk=dusk)-mean(dawn.or.dusk(h=x,dawn=dawn,dusk=dusk))) +
  #                     mean(kilometre)
  #                 )
  # ),
  # alpha=0.2) +
  ylab("Relative Collision Rate") +
  xlab("Hour") +
  theme_bw() +
  theme(legend.key = element_blank()) +
  theme(plot.margin=unit(c(.5,.5,.1,.1),"cm")) +
  theme(axis.title.x = element_text(margin=unit(c(.3,0,0,0),"cm"))) +
  theme(axis.title.y = element_text(margin=unit(c(0,.3,0,0),"cm"))) +
  theme(panel.grid.major = element_line(size=0.1),panel.grid.minor = element_line(size=0.1)) +
  theme(text = element_text(size = 16)) +
  scale_x_continuous(limits = c(0,24), breaks = seq(0,24,2), expand = c(0, 0)) #+
#scale_y_continuous(limits = c(0,.002), breaks = seq(0,.002,.0002))
dev.off()

###### alternative based on predictions for phd talk######

png('/home/casey/Research/Projects/PhD_Thesis/completion_talk/graphics/train_occ_k.png', pointsize = 16, res=300, width = 1000, height = 900)
x <- seq(0,1,1/nrow(data))[-(1:1)]
ggplot(data) +
  geom_line(aes(x=x,
                y=invcloglog(
                  model.coefs[1] +
                    model.coefs[2]*(log(x)-mean(log(x))) +
                    model.coefs[3]*mean(trains_lc) +
                    model.coefs[4]*mean(speed_lc) +
                    model.coefs[5]*mean(light) +
                    model.coefs[6]*mean(light2) +
                    model.coefs[7]*mean(dawnordusk) +
                    mean(kilometre)
                )
  ),
  size=0.8,colour='#94d1c7') +
  geom_ribbon(aes(x=x,
                  ymin=invcloglog(
                    model.coefs[1] +
                      model.coefs.ci[2,1]*(log(x)-mean(log(x))) +
                      model.coefs[3]*mean(trains_lc) +
                      model.coefs[4]*mean(speed_lc) +
                      model.coefs[5]*mean(light) +
                      model.coefs[6]*mean(light2) +
                      model.coefs[7]*mean(dawnordusk) +
                      mean(kilometre)
                  ),
                  ymax=invcloglog(
                    model.coefs[1] +
                      model.coefs.ci[2,2]*(log(x)-mean(log(x))) +
                      model.coefs[3]*mean(trains_lc) +
                      model.coefs[4]*mean(speed_lc) +
                      model.coefs[5]*mean(light) +
                      model.coefs[6]*mean(light2) +
                      model.coefs[7]*mean(dawnordusk) +
                      mean(kilometre)
                  )
  ),
  alpha=0.3) +
  ylab("RELATIVE COLLISION RATE") +
  xlab("LIKELIHOOD OF SPECIES OCCURRENCE") +
  theme_bw() +
  theme(legend.key = element_blank()) +
  theme(plot.margin=unit(c(.5,.5,.1,.1),"cm")) +
  theme(axis.title.x = element_text(margin=unit(c(.3,0,0,0),"cm"))) +
  theme(axis.title.y = element_text(margin=unit(c(0,.3,0,0),"cm"))) +
  theme(panel.grid.major = element_line(size=0.1),panel.grid.minor = element_line(size=0.1)) +
  theme(text = element_text(size = 8)) +
  scale_x_continuous(breaks=seq(0,1,by=.1), expand = c(0, 0), lim=c(0,1))
dev.off()


png('/home/casey/Research/Projects/PhD_Thesis/completion_talk/graphics/train_vol_k.png', pointsize = 16, res=300, width = 1000, height = 900)
x <- seq(0,60,60/nrow(data))[-(1:1)]
ggplot(data) +
  geom_line(aes(x=x,
                y=invcloglog(
                  model.coefs[1] +
                    model.coefs[2]*mean(egk_lc) +
                    model.coefs[3]*(log(x)-mean(log(x))) +
                    model.coefs[4]*mean(speed_lc) +
                    model.coefs[5]*mean(light) +
                    model.coefs[6]*mean(light2) +
                    model.coefs[7]*mean(dawnordusk) +
                    mean(kilometre)
                )
  ),
  size=0.8,colour='#94d1c7') +
  geom_ribbon(aes(x=x,
                  ymin=invcloglog(
                    model.coefs[1] +
                      model.coefs[2]*mean(egk_lc) +
                      model.coefs.ci[3,1]*(log(x)-mean(log(x))) +
                      model.coefs[4]*mean(speed_lc) +
                      model.coefs[5]*mean(light) +
                      model.coefs[6]*mean(light2) +
                      model.coefs[7]*mean(dawnordusk) +
                      mean(kilometre)
                  ),
                  ymax=invcloglog(
                    model.coefs[1] +
                      model.coefs[2]*mean(egk_lc) +
                      model.coefs.ci[3,2]*(log(x)-mean(log(x))) +
                      model.coefs[4]*mean(speed_lc) +
                      model.coefs[5]*mean(light) +
                      model.coefs[6]*mean(light2) +
                      model.coefs[7]*mean(dawnordusk) +
                      mean(kilometre)
                  )
  ),
  alpha=0.3) +
  ylab("RELATIVE COLLISION RATE") +
  xlab("TRAIN FREQUENCY (TRAINS/HR)") +
  theme_bw() +
  theme(legend.key = element_blank()) +
  theme(plot.margin=unit(c(.5,.5,.1,.1),"cm")) +
  theme(axis.title.x = element_text(margin=unit(c(.3,0,0,0),"cm"))) +
  theme(axis.title.y = element_text(margin=unit(c(0,.3,0,0),"cm"))) +
  theme(panel.grid.major = element_line(size=0.1),panel.grid.minor = element_line(size=0.1)) +
  theme(text = element_text(size = 8)) +
  scale_x_continuous(breaks=seq(0,60,by=10), expand = c(0, 0), lim=c(0,60))
dev.off()


png('/home/casey/Research/Projects/PhD_Thesis/completion_talk/graphics/train_spd_k.png', pointsize = 16, res=300, width = 1000, height = 900)
x <- seq(0,160,160/nrow(data))[-c(1,length(seq(0,160,160/(nrow(data)+1))))]
ggplot(data) +
  geom_line(aes(x=x,
                y=invcloglog(
                  model.coefs[1] +
                    model.coefs[2]*mean(egk_lc) +
                    model.coefs[3]*mean(trains_lc) +
                    model.coefs[4]*(log(x)-mean(log(x))) +
                    model.coefs[5]*mean(light) +
                    model.coefs[6]*mean(light2) +
                    model.coefs[7]*mean(dawnordusk) +
                    mean(kilometre)
                )
  ),
  size=0.8,colour='#94d1c7') +
  geom_ribbon(aes(x=x,
                  ymin=invcloglog(
                    model.coefs[1] +
                      model.coefs[2]*mean(egk_lc) +
                      model.coefs[3]*mean(trains_lc) +
                      model.coefs.ci[4,1]*(log(x)-mean(log(x))) +
                      model.coefs[5]*mean(light) +
                      model.coefs[6]*mean(light2) +
                      model.coefs[7]*mean(dawnordusk) +
                      mean(kilometre)
                  ),
                  ymax=invcloglog(
                    model.coefs[1] +
                      model.coefs[2]*mean(egk_lc) +
                      model.coefs[3]*mean(trains_lc) +
                      model.coefs.ci[4,2]*(log(x)-mean(log(x))) +
                      model.coefs[5]*mean(light) +
                      model.coefs[6]*mean(light2) +
                      model.coefs[7]*mean(dawnordusk) +
                      mean(kilometre)
                  )
  ),
  alpha=0.3) +
  ylab("RELATIVE COLLISION RATE") +
  xlab("TRAIN SPEED (KM/HR)") +
  theme_bw() +
  theme(legend.key = element_blank()) +
  theme(plot.margin=unit(c(.5,.5,.1,.1),"cm")) +
  theme(axis.title.x = element_text(margin=unit(c(.3,0,0,0),"cm"))) +
  theme(axis.title.y = element_text(margin=unit(c(0,.3,0,0),"cm"))) +
  theme(panel.grid.major = element_line(size=0.1),panel.grid.minor = element_line(size=0.1)) +
  theme(text = element_text(size = 8)) +
  scale_x_continuous(breaks=seq(0,160,by=20), expand = c(0, 0), lim=c(0,160))
dev.off()

png('/home/casey/Research/Projects/PhD_Thesis/completion_talk/graphics/train_hour_k.png', pointsize = 16, res=300, width = 1000, height = 900)
x <- seq(0,24,24/nrow(data))[-(1:1)]
ggplot(data) +
  geom_smooth(aes(x=x,
                  y=invcloglog(
                    model.coefs[1] +
                      model.coefs[2]*mean(egk_lc) +
                      model.coefs[3]*mean(trains_lc) +
                      model.coefs[4]*mean(speed_lc) +
                      model.coefs[5]*(sin((2 * pi * (x - 6)) / 24)-mean(sin((2 * pi * (x - 6)) / 24))) +
                      model.coefs[6]*(sin((2 * pi * (x - 6)) / 24)^2-mean(sin((2 * pi * (x - 6)) / 24)^2)) +
                      model.coefs[7]*(dawn.or.dusk(h=x,dawn=dawn,dusk=dusk)-mean(dawn.or.dusk(h=x,dawn=dawn,dusk=dusk))) +
                      mean(kilometre)
                  )
  ),
  size=0.8, colour='#94d1c7') +
  geom_ribbon(aes(x=x,
                  ymin=invcloglog(
                    model.coefs[1] +
                      model.coefs[2]*mean(egk_lc) +
                      model.coefs[3]*mean(trains_lc) +
                      model.coefs[4]*mean(speed_lc) +
                      model.coefs.ci[5,1]*(sin((2 * pi * (x - 6)) / 24)-mean(sin((2 * pi * (x - 6)) / 24))) +
                      model.coefs.ci[6,1]*(sin((2 * pi * (x - 6)) / 24)^2-mean(sin((2 * pi * (x - 6)) / 24)^2)) +
                      model.coefs.ci[7,1]*(dawn.or.dusk(h=x,dawn=dawn,dusk=dusk)-mean(dawn.or.dusk(h=x,dawn=dawn,dusk=dusk))) +
                      mean(kilometre)
                  ),
                  ymax=invcloglog(
                    model.coefs[1] +
                      model.coefs[2]*mean(egk_lc) +
                      model.coefs[3]*mean(trains_lc) +
                      model.coefs[4]*mean(speed_lc) +
                      model.coefs.ci[5,2]*(sin((2 * pi * (x - 6)) / 24)-mean(sin((2 * pi * (x - 6)) / 24))) +
                      model.coefs.ci[6,2]*(sin((2 * pi * (x - 6)) / 24)^2-mean(sin((2 * pi * (x - 6)) / 24)^2)) +
                      model.coefs.ci[7,2]*(dawn.or.dusk(h=x,dawn=dawn,dusk=dusk)-mean(dawn.or.dusk(h=x,dawn=dawn,dusk=dusk))) +
                      mean(kilometre)
                  )
  ),
  alpha=0.3) +
  ylab("RELATIVE COLLISION RATE") +
  xlab("HOUR") +
  theme_bw() +
  theme(legend.key = element_blank()) +
  theme(plot.margin=unit(c(.5,.5,.1,.1),"cm")) +
  theme(axis.title.x = element_text(margin=unit(c(.3,0,0,0),"cm"))) +
  theme(axis.title.y = element_text(margin=unit(c(0,.3,0,0),"cm"))) +
  theme(panel.grid.major = element_line(size=0.1),panel.grid.minor = element_line(size=0.1)) +
  theme(text = element_text(size = 8)) +
  scale_x_continuous(breaks=seq(0,24,by=2), expand = c(0, 0), lim=c(0,24))
dev.off()
#############################################