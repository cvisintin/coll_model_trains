require(ggplot2)
require(rstan)

plotPal <- c("#94d1c7")

invcloglog <- function (x) {1-exp(-exp(x))}

model.data.bin <- as.data.table(read.delim("data/model_data_hm_bin.csv", header=T, sep=","))

load("data/coll_stan_bin2")

n <- nrow(model.data.bin)
y <- model.data.bin$coll
egk <- model.data.bin$egk
trains <- model.data.bin$trains
speed <- model.data.bin$speed
light <- model.data.bin$light
light2 <- model.data.bin$light2
dawnordusk <- model.data.bin$dawnordusk
length <- model.data.bin$length

model.coefs <- get_posterior_mean(coll.stan.bin2, pars="b")
model.coefs.ci <- summary(coll.stan.bin2, pars = c("b"), probs = c(0.025, 0.975))$summary[, c("2.5%", "97.5%")]

# model.coefs <- NULL
# model.coefs.err  <- NULL
# for(i in 1:7){
#   model.coefs[i] <- mean(c(unlist(coll.stan.bin@sim$samples[[1]][i])[251:500],unlist(coll.stan.bin@sim$samples[[2]][i])[251:500],unlist(coll.stan.bin@sim$samples[[3]][i])[251:500])) 
#   model.coefs2.5[i] <- sd(c(unlist(coll.stan.bin@sim$samples[[1]][i])[251:500],unlist(coll.stan.bin@sim$samples[[2]][i])[251:500],unlist(coll.stan.bin@sim$samples[[3]][i])[251:500]))
#   model.coefs97.5[i] <- sd(c(unlist(coll.stan.bin@sim$samples[[1]][i])[251:500],unlist(coll.stan.bin@sim$samples[[2]][i])[251:500],unlist(coll.stan.bin@sim$samples[[3]][i])[251:500]))
# }
  
png('figs/egk.png', pointsize = 12, res=300, width = 900, height = 900, bg='transparent')
ggplot(model.data.bin) +
  geom_line(aes(x = egk, y = exp(model.coefs[1,1] + model.coefs[2,1]*(log(egk)-mean(log(egk))))), colour=plotPal, size=0.5) +
  geom_ribbon(aes(x = egk, ymin=exp(model.coefs[1,1] + model.coefs.ci[2,1]*(log(egk)-mean(log(egk)))), ymax=exp(model.coefs[1] + model.coefs.ci[2,2]*(log(egk)-mean(log(egk))))), alpha=0.2) +
  ylab("Likelihood of Collision") +
  xlab("Likelihood of Species Occurrence") +
  theme_bw() +
  theme(legend.key = element_blank()) +
  theme(plot.margin=unit(c(.5,0,.1,.1),"cm")) +
  theme(axis.title.x = element_text(margin=unit(c(.3,0,0,0),"cm"))) +
  theme(axis.title.y = element_text(margin=unit(c(0,.3,0,0),"cm"))) +
  theme(panel.grid.major = element_line(size=0.1),panel.grid.minor = element_line(size=0.1)) +
  theme(text = element_text(size = 10)) +
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,.1)) +
  scale_y_continuous(limits = c(-.001,.008), breaks = seq(0,.008,.001))
dev.off()

png('figs/trains.png', pointsize = 12, res=300, width = 900, height = 900, bg='transparent')
ggplot(model.data.bin) +
  geom_line(aes(x = trains, y = exp(model.coefs[1] + model.coefs[3]*(log(trains)-mean(log(trains))))), colour=plotPal, size=0.5) +
  geom_ribbon(aes(x = trains, ymin=exp(model.coefs[1] + model.coefs.ci[3,1]*(log(trains)-mean(log(trains)))), ymax=exp(model.coefs[1] + model.coefs.ci[3,2]*(log(trains)-mean(log(trains))))), alpha=0.2) +
  ylab("Likelihood of Collision") +
  xlab("Number of Trains") +
  theme_bw() +
  theme(legend.key = element_blank()) +
  theme(plot.margin=unit(c(.5,0,.1,.1),"cm")) +
  theme(axis.title.x = element_text(margin=unit(c(.3,0,0,0),"cm"))) +
  theme(axis.title.y = element_text(margin=unit(c(0,.3,0,0),"cm"))) +
  theme(panel.grid.major = element_line(size=0.1),panel.grid.minor = element_line(size=0.1)) +
  theme(text = element_text(size = 10)) +
  scale_x_continuous(limits = c(0,80), breaks = seq(0,80,10)) +
  scale_y_continuous(limits = c(-.001,.008), breaks = seq(0,.008,.001))
dev.off()

png('figs/speed.png', pointsize = 12, res=300, width = 900, height = 900, bg='transparent')
ggplot(data = model.data.bin) +
  geom_line(aes(x = speed, y = exp(model.coefs[1] + model.coefs[4]*(log(speed)-mean(log(speed))))), colour=plotPal, size=0.5) +
  geom_ribbon(aes(x = speed, ymin=exp(model.coefs[1] + model.coefs.ci[4,1]*(log(speed)-mean(log(speed)))), ymax=exp(model.coefs[1] + model.coefs.ci[4,2]*(log(speed)-mean(log(speed))))), alpha=0.2) +
  ylab("Likelihood of Collision") +
  xlab("Train Speed (km/hr)") +
  theme_bw() +
  theme(legend.key = element_blank()) +
  theme(plot.margin=unit(c(.5,0,.1,.1),"cm")) +
  theme(axis.title.x = element_text(margin=unit(c(.3,0,0,0),"cm"))) +
  theme(axis.title.y = element_text(margin=unit(c(0,.3,0,0),"cm"))) +
  theme(panel.grid.major = element_line(size=0.1),panel.grid.minor = element_line(size=0.1)) +
  theme(text = element_text(size = 10)) +
  scale_x_continuous(limits = c(10,150), breaks = seq(10,150,20)) +
  scale_y_continuous(limits = c(-.001,.008), breaks = seq(0,.008,.001))
dev.off()

png('figs/hour.png', pointsize = 12, res=300, width = 900, height = 900, bg='transparent')
ggplot(data = model.data.bin) +
  geom_smooth(aes(x = hour, ymin=exp(model.coefs[1] + model.coefs.ci[5,1]*(light-mean(light)) + model.coefs.ci[6,1]*(light2-mean(light2)) + model.coefs.ci[7,1]*(dawnordusk-mean(dawnordusk))),
                  ymax=exp(model.coefs[1] + model.coefs.ci[5,2]*(light-mean(light)) + model.coefs.ci[6,2]*(light2-mean(light2)) + model.coefs.ci[7,2]*(dawnordusk-mean(dawnordusk)))),
                  alpha=0.3, colour=plotPal, lwd=0.5) +
  ylab("Likelihood of Collision") +
  xlab("Hour") +
  theme_bw() +
  theme(legend.key = element_blank()) +
  theme(plot.margin=unit(c(.5,0,.1,.1),"cm")) +
  theme(axis.title.x = element_text(margin=unit(c(.3,0,0,0),"cm"))) +
  theme(axis.title.y = element_text(margin=unit(c(0,.3,0,0),"cm"))) +
  theme(panel.grid.major = element_line(size=0.1),panel.grid.minor = element_line(size=0.1)) +
  theme(text = element_text(size = 10)) +  
  scale_x_continuous(limits = c(0,23), breaks = seq(0,23,2)) +
  scale_y_continuous(limits = c(-.001,.008), breaks = seq(0,.008,.001))
dev.off()


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