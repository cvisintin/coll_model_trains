require(ggplot2)
require(rstan)

# plotPal <- c("#94d1c7")

perform.glm.1000 <- read.csv("output/coll_perform.csv", header=T)

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
ggplot(data) +
  geom_line(aes(x=seq(0,1,1/nrow(data))[-(1:1)],
                y=invcloglog(
                  model.coefs[1] +
                    model.coefs[2]*(log(x)-mean(log(x))) +
                    model.coefs[3]*mean(trains) +
                    model.coefs[4]*mean(speed) +
                    model.coefs[5]*mean(light) +
                    model.coefs[6]*mean(light2) +
                    model.coefs[7]*mean(dawnordusk) +
                    mean(kilometre)
                )
  ),
  size=0.5) +
  geom_ribbon(aes(x=seq(0,1,1/nrow(data))[-(1:1)],
                  ymin=invcloglog(
                    model.coefs[1] +
                      model.coefs.ci[2,1]*(log(x)-mean(log(x))) +
                      model.coefs[3]*mean(trains) +
                      model.coefs[4]*mean(speed) +
                      model.coefs[5]*mean(light) +
                      model.coefs[6]*mean(light2) +
                      model.coefs[7]*mean(dawnordusk) +
                      mean(kilometre)
                  ),
                  ymax=invcloglog(
                    model.coefs[1] +
                      model.coefs.ci[2,2]*(log(x)-mean(log(x))) +
                      model.coefs[3]*mean(trains) +
                      model.coefs[4]*mean(speed) +
                      model.coefs[5]*mean(light) +
                      model.coefs[6]*mean(light2) +
                      model.coefs[7]*mean(dawnordusk) +
                      mean(kilometre)
                  )
  ),
  alpha=0.2) +
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
ggplot(data) +
  geom_line(aes(x=seq(0,60,60/nrow(data))[-(1:1)],
                y=invcloglog(
                  model.coefs[1] +
                    model.coefs[2]*mean(egk) +
                    model.coefs[3]*(log(x)-mean(log(x))) +
                    model.coefs[4]*mean(speed) +
                    model.coefs[5]*mean(light) +
                    model.coefs[6]*mean(light2) +
                    model.coefs[7]*mean(dawnordusk) +
                    mean(kilometre)
                )
  ),
  size=0.5) +
  geom_ribbon(aes(x=seq(0,60,60/nrow(data))[-(1:1)],
                  ymin=invcloglog(
                    model.coefs[1] +
                      model.coefs[2]*mean(egk) +
                      model.coefs.ci[3,1]*(log(x)-mean(log(x))) +
                      model.coefs[4]*mean(speed) +
                      model.coefs[5]*mean(light) +
                      model.coefs[6]*mean(light2) +
                      model.coefs[7]*mean(dawnordusk) +
                      mean(kilometre)
                  ),
                  ymax=invcloglog(
                    model.coefs[1] +
                      model.coefs[2]*mean(egk) +
                      model.coefs.ci[3,2]*(log(x)-mean(log(x))) +
                      model.coefs[4]*mean(speed) +
                      model.coefs[5]*mean(light) +
                      model.coefs[6]*mean(light2) +
                      model.coefs[7]*mean(dawnordusk) +
                      mean(kilometre)
                  )
  ),
  alpha=0.2) +
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
ggplot(data = data) +
  geom_line(aes(x=seq(10,160,150/nrow(data))[1:nrow(data)],
                y=invcloglog(
                  model.coefs[1] +
                    model.coefs[2]*mean(egk) +
                    model.coefs[3]*mean(trains) +
                    model.coefs[4]*(log(x)-mean(log(x))) +
                    model.coefs[5]*mean(light) +
                    model.coefs[6]*mean(light2) +
                    model.coefs[7]*mean(dawnordusk) +
                    mean(kilometre)
                )
  ),
  size=0.5) +
  geom_ribbon(aes(x=seq(10,160,150/nrow(data))[1:nrow(data)],
                  ymin=invcloglog(
                    model.coefs[1] +
                      model.coefs[2]*mean(egk) +
                      model.coefs[3]*mean(trains) +
                      model.coefs.ci[4,1]*(log(x)-mean(log(x))) +
                      model.coefs[5]*mean(light) +
                      model.coefs[6]*mean(light2) +
                      model.coefs[7]*mean(dawnordusk) +
                      mean(kilometre)
                  ),
                  ymax=invcloglog(
                    model.coefs[1] +
                      model.coefs[2]*mean(egk) +
                      model.coefs[3]*mean(trains) +
                      model.coefs.ci[4,2]*(log(x)-mean(log(x))) +
                      model.coefs[5]*mean(light) +
                      model.coefs[6]*mean(light2) +
                      model.coefs[7]*mean(dawnordusk) +
                      mean(kilometre)
                  )
  ),
  alpha=0.2) +
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

data <- cbind(data,model.data.bin[,c(6,8:9),with=FALSE])

png('figs/hour.png', pointsize = 12, res=300, width = 900, height = 900, bg='transparent')
ggplot(data = data) +
  geom_line(aes(x=seq(0,24,24/nrow(data))[-(1:1)],
                y=invcloglog(
                  model.coefs[1] +
                    model.coefs[2]*mean(egk) +
                    model.coefs[3]*mean(trains) +
                    model.coefs[4]*mean(speed) +
                    model.coefs[5]*(sin((2 * pi * (x - 6)) / 24)-mean(sin((2 * pi * (x - 6)) / 24))) +
                    model.coefs[6]*(sin((2 * pi * (x - 6)) / 24)^2-mean(sin((2 * pi * (x - 6)) / 24)^2)) +
                    model.coefs[7]*(dawn.or.dusk(h=x,dawn=dawn,dusk=dusk)-mean(dawn.or.dusk(h=x,dawn=dawn,dusk=dusk))) +
                    mean(kilometre)
                )
  ),
  size=0.5) +
  geom_ribbon(aes(x=seq(0,24,24/nrow(data))[-(1:1)],
                  ymin=invcloglog(
                    model.coefs[1] +
                      model.coefs[2]*mean(egk) +
                      model.coefs[3]*mean(trains) +
                      model.coefs[4]*mean(speed) +
                      model.coefs.ci[5,1]*(sin((2 * pi * (x - 6)) / 24)-mean(sin((2 * pi * (x - 6)) / 24))) +
                      model.coefs.ci[6,1]*(sin((2 * pi * (x - 6)) / 24)^2-mean(sin((2 * pi * (x - 6)) / 24)^2)) +
                      model.coefs.ci[7,1]*(dawn.or.dusk(h=x,dawn=dawn,dusk=dusk)-mean(dawn.or.dusk(h=x,dawn=dawn,dusk=dusk))) +
                      mean(kilometre)
                  ),
                  ymax=invcloglog(
                    model.coefs[1] +
                      model.coefs[2]*mean(egk) +
                      model.coefs[3]*mean(trains) +
                      model.coefs[4]*mean(speed) +
                      model.coefs.ci[5,2]*(sin((2 * pi * (x - 6)) / 24)-mean(sin((2 * pi * (x - 6)) / 24))) +
                      model.coefs.ci[6,2]*(sin((2 * pi * (x - 6)) / 24)^2-mean(sin((2 * pi * (x - 6)) / 24)^2)) +
                      model.coefs.ci[7,2]*(dawn.or.dusk(h=x,dawn=dawn,dusk=dusk)-mean(dawn.or.dusk(h=x,dawn=dawn,dusk=dusk))) +
                      mean(kilometre)
                  )
  ),
  alpha=0.2) +
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

metrics <- factor(c("Somer's D","ROC","Adj. R2","Brier Score","Intercept","Slope","Emax","S:z","S:p","Eavg"),
                  levels=c("Dxy","C (ROC)","R2","Brier","Intercept","Slope","Emax","S:z","S:p","Eavg"))

cv_plot <- transform(data.frame(x = factor(rownames(perform.glm.1000),levels=rev(unique(rownames(perform.glm.1000)))), y = perform.glm.1000[, 2], y_orig = perform.glm.1000[, 1]),
                  ylo=y-(2*perform.glm.1000[, 3]),
                  yhi=y+(2*perform.glm.1000[, 3])
                  )

png('figs/validate.png', pointsize = 6, res=300, width = 900, height = 900, bg='transparent')
p <- ggplot(cv_plot, aes(x=x, y=y, ymin=ylo, ymax=yhi)) +
  geom_pointrange(size = 0.1) +
  geom_hline(yintercept = 0, linetype=2, size=0.2) +
  coord_flip() +
  ylab("Value") +
  xlab("Test Metric") +
  theme_bw() +
  theme(plot.margin=unit(c(.5,0,.1,.1),"cm")) +
  theme(axis.title.x = element_text(margin=unit(c(.3,0,0,0),"cm"))) +
  theme(axis.title.y = element_text(margin=unit(c(0,.3,0,0),"cm"))) +
  theme(panel.grid.major = element_line(size=0.1),panel.grid.minor = element_line(size=0.1)) +
  theme(text = element_text(size = 10))
p + geom_point(aes(x=x, y=y_orig), size = 2, shape=1, inherit.aes=FALSE)
dev.off()
