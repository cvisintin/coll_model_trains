require(ggplot2)
require(plyr)
require(foreach)

# plotPal <- c("#94d1c7")

load(file="output/coll_glm")
load(file="output/mar_effects_ci")
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

png('figs/egk.png', pointsize = 12, res=300, width = 900, height = 900, bg='transparent')
ggplot(egk_mean_ci) +
  geom_line(aes(x=x, y=y), size=0.8) +
  geom_ribbon(aes(x=x, ymin=ymin, ymax=ymax), alpha=0.3) +
  ylab("Likelihood of Collision") +
  xlab("Likelihood of Species Occurrence") +
  theme_bw() +
  theme(legend.key = element_blank()) +
  theme(plot.margin=unit(c(.5,0,.1,.1),"cm")) +
  theme(axis.title.x = element_text(margin=unit(c(.3,0,0,0),"cm"))) +
  theme(axis.title.y = element_text(margin=unit(c(0,.3,0,0),"cm"))) +
  theme(panel.grid.major = element_line(size=0.1),panel.grid.minor = element_line(size=0.1)) +
  theme(text = element_text(size = 10)) +
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,.1))
dev.off()


png('figs/trains.png', pointsize = 12, res=300, width = 900, height = 900, bg='transparent')
ggplot(trains_mean_ci) +
  geom_line(aes(x=x, y=y), size=0.8) +
  geom_ribbon(aes(x=x, ymin=ymin, ymax=ymax), alpha=0.3) +
  ylab("Likelihood of Collision") +
  xlab("Number of Trains") +
  theme_bw() +
  theme(legend.key = element_blank()) +
  theme(plot.margin=unit(c(.5,0,.1,.1),"cm")) +
  theme(axis.title.x = element_text(margin=unit(c(.3,0,0,0),"cm"))) +
  theme(axis.title.y = element_text(margin=unit(c(0,.3,0,0),"cm"))) +
  theme(panel.grid.major = element_line(size=0.1),panel.grid.minor = element_line(size=0.1)) +
  theme(text = element_text(size = 10)) +
  scale_x_continuous(limits = c(1,20), breaks = seq(1,20,2))
dev.off()


png('figs/speed.png', pointsize = 12, res=300, width = 900, height = 900, bg='transparent')
ggplot(speed_mean_ci) +
  geom_line(aes(x=x, y=y), size=0.8) +
  geom_ribbon(aes(x=x, ymin=ymin, ymax=ymax), alpha=0.3) +
  ylab("Likelihood of Collision") +
  xlab("Train Speed (km/hr)") +
  theme_bw() +
  theme(legend.key = element_blank()) +
  theme(plot.margin=unit(c(.5,0,.1,.1),"cm")) +
  theme(axis.title.x = element_text(margin=unit(c(.3,0,0,0),"cm"))) +
  theme(axis.title.y = element_text(margin=unit(c(0,.3,0,0),"cm"))) +
  theme(panel.grid.major = element_line(size=0.1),panel.grid.minor = element_line(size=0.1)) +
  theme(text = element_text(size = 10)) +
  scale_x_continuous(limits = c(1,160), breaks = seq(1,160,20))
dev.off()

png('figs/hour.png', pointsize = 12, res=300, width = 900, height = 900, bg='transparent')
ggplot(hour_mean_ci) +
  geom_smooth(aes(x=x, y=y), size=0.8, col='black') +
  geom_ribbon(aes(x=x, ymin=ymin, ymax=ymax), alpha=0.3) +
  ylab("Likelihood of Collision") +
  xlab("Hour") +
  theme_bw() +
  theme(legend.key = element_blank()) +
  theme(plot.margin=unit(c(.5,0,.1,.1),"cm")) +
  theme(axis.title.x = element_text(margin=unit(c(.3,0,0,0),"cm"))) +
  theme(axis.title.y = element_text(margin=unit(c(0,.3,0,0),"cm"))) +
  theme(panel.grid.major = element_line(size=0.1),panel.grid.minor = element_line(size=0.1)) +
  theme(text = element_text(size = 10)) +
  scale_x_continuous(limits = c(0,24), breaks = seq(0,24,2))
dev.off()

################ Calibration plots ##################

perform.sim <- function(model){
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
  
  y <- model$data$y
  p <- predict(model, model$data, type="response")
  
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
  colnames(perform.glm) <- parent.frame()$i[]
  return(perform.glm)
}


model.list <- list(coll.glm.wo_trains, coll.glm.wo_egk, coll.glm.wo_speed, coll.glm.wo_temp, coll.glm.w_trains)
names(model.list) <- c("wo_trains", "wo_egk", "wo_speed", "wo_temp", "all")

model.perform.list <- lapply(model.list, perform.sim)

model.cv.perform.list <- list(cv_coll.glm.wo_trains, cv_coll.glm.wo_egk, cv_coll.glm.wo_speed, cv_coll.glm.wo_temp, cv_coll.glm.w_trains)

cv_plot_data <- foreach(i = 1:length(model.list), .combine=rbind) %do% {
  perform.glm.1000 <- cbind("orig_model"=signif(model.perform.list[[i]], digits=4),
                            "cv_model_mean"=signif(apply(model.cv.perform.list[[i]],1,mean), digits=4),
                            "cv_model_sd"=signif(apply(model.cv.perform.list[[i]],1,sd), digits=4),
                            "cv_model_rlo"=signif(apply(model.cv.perform.list[[i]],1,range), digits=4)[1,],
                            "cv_model_rhi"=signif(apply(model.cv.perform.list[[i]],1,range), digits=4)[2,]
  )
  
  metrics <- factor(c("Intercept","Slope","ROC"),
                    levels=rev(c("Intercept","Slope","ROC"))
                    )
  
  transform(data.frame(id = as.character(names(model.list)[i]), x = metrics, y = perform.glm.1000[, 2], y_orig = perform.glm.1000[, 1]),
                       ylo=perform.glm.1000[, 2] - 2*perform.glm.1000[, 3],
                       yhi=perform.glm.1000[, 2] + 2*perform.glm.1000[, 3]
                       )
}

# y <- coll.glm$data$y
# p <- predict(coll.glm, coll.glm$data, type="response")
# 
# p.b <- .bincode(p,seq(min(p),max(p),(max(p)-min(p))/10),include.lowest = TRUE)
# 
# yp_bins <- data.frame(y,p,p.b)
# 
# 
# plot.info <- ddply(yp_bins, ~p.b, summarise,
#                    count=length(y),
#                    prop_coll=sum(y)/length(y),
#                    prop_nocoll=(length(y)-sum(y))/length(y),
#                    prop_lo=binom.test(sum(y), length(y), 0.5)$conf.int[1],
#                    prop_hi=binom.test(sum(y), length(y), 0.5)$conf.int[2],
#                    median_p=median(p)
# )
# 
# calib <- glm(y~log(p), family=binomial(link=cloglog), data=yp_bins)
# roc <- roc(y,p)
# perform.glm <- rbind(calib_int=calib$coefficients[1], calib_slope=calib$coefficients[2], roc)
# colnames(perform.glm) <- "0.0"

# png('figs/calib.png', pointsize = 12, res=300, width = 900, height = 900, bg='transparent')
# #png('figs/calib.png', pointsize = 12, res=300, width = 1400, height = 900, bg='transparent')
# ggplot() +
#   geom_line(data=yp_bins, aes(y=invcloglog(calib$coefficients[1]+calib$coefficients[2]*log(p)),x=p)) +
#   geom_pointrange(data=plot.info, aes(x=median_p, y=prop_coll, ymin=prop_lo, ymax=prop_hi), size = 0.2, inherit.aes=FALSE) +
#   geom_text(data=plot.info, aes(x=median_p, y=prop_coll, label=count),hjust=-0.3, vjust=-1, angle = 90, size = 2.0, inherit.aes=FALSE) +
#   geom_segment(aes(x = 0, y = 0, xend = .04, yend = .04), linetype=2, size=0.1, inherit.aes=FALSE) +
#   ylab("Observed Rate") +
#   xlab("Predicted Rate") +
#   theme_bw() +
#   theme(plot.margin=unit(c(.5,0,.1,.1),"cm")) +
#   theme(axis.title.x = element_text(margin=unit(c(.3,0,0,0),"cm"))) +
#   theme(axis.title.y = element_text(margin=unit(c(0,.3,0,0),"cm"))) +
#   theme(panel.grid.major = element_line(size=0.1),panel.grid.minor = element_line(size=0.1)) +
#   theme(text = element_text(size = 12))
# dev.off()
# 
# png('figs/roc.png', pointsize = 12, res=300, width = 900, height = 900, bg='transparent')
# ggplot() +
#   stat_roc(data=yp_bins, aes(d=y,m=p), n.cuts = 0, size=0.5) +
#   geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), linetype=2, size=0.1, inherit.aes=FALSE) +
#   ylab("True Positive Fraction") +
#   xlab("False Positive Fraction") +
#   theme_bw() +
#   theme(plot.margin=unit(c(.5,0,.1,.1),"cm")) +
#   theme(axis.title.x = element_text(margin=unit(c(.3,0,0,0),"cm"))) +
#   theme(axis.title.y = element_text(margin=unit(c(0,.3,0,0),"cm"))) +
#   theme(panel.grid.major = element_line(size=0.1),panel.grid.minor = element_line(size=0.1)) +
#   theme(text = element_text(size = 12))
# dev.off()

# perform.glm.1000 <- cbind("full_model"=signif(perform.glm, digits=4),
#                           "cv_model_mean"=signif(apply(perform.glm.cv,1,mean), digits=4),
#                           "cv_model_sd"=signif(apply(perform.glm.cv,1,sd), digits=4),
#                           "cv_model_rlo"=signif(apply(perform.glm.cv,1,range), digits=4)[1,],
#                           "cv_model_rhi"=signif(apply(perform.glm.cv,1,range), digits=4)[2,]
# )

png('figs/validate_colour_01.png', pointsize = 10, res=300, width = 400, height = 1000, bg='transparent')
p <- ggplot(data=cv_plot_data[grep("calib_int",row.names(cv_plot_data)), ]) +
  geom_pointrange(aes(x=x, y=y, ymin=ylo, ymax=yhi, colour = factor(id, rev(c("wo_trains","wo_egk","wo_speed","wo_temp","all")))), size = 0.5, fatten=1, position = position_dodge(width=0.75)) +
  coord_flip() +
  ylab("Intercept") +
  xlab("") +
  labs(colour="Model") +
  geom_text(aes(x=x, y=y, label=round(y,3), colour = factor(id, rev(c("wo_trains","wo_egk","wo_speed","wo_temp","all")))), hjust=1.3, vjust=1.3, size=2, inherit.aes=FALSE, position = position_dodge(width=0.75)) +
  guides(colour = guide_legend(reverse = TRUE, override.aes = list(size=0.2))) +
  theme_bw() +
  scale_color_manual(values=c("#7eb79d", "#a09cbc", "#fb8072", "#80b1d3", "#db9546", "#969696")) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) +
  theme(plot.margin=unit(c(.5,.5,.1,.1),"cm")) +
  theme(axis.title.x = element_text(margin=unit(c(.3,0,0,0),"cm"))) +
  theme(axis.title.y = element_text(margin=unit(c(0,.3,0,0),"cm"))) +
  theme(panel.grid.major = element_line(size=0.1), panel.grid.minor = element_line(size=0.1)) +
  theme(text = element_text(size = 8))
p + geom_point(data=cv_plot_data[grep("calib_int",row.names(cv_plot_data)), ], aes(x=x, y=y_orig, colour = factor(id, rev(c("wo_trains","wo_egk","wo_speed","wo_temp","all")))), size = 2.5, shape=1, inherit.aes=FALSE, position = position_dodge(width=0.75))
dev.off()

png('figs/validate_colour_02.png', pointsize = 10, res=300, width = 400, height = 1000, bg='transparent')
p <- ggplot(data=cv_plot_data[grep("calib_slope",row.names(cv_plot_data)), ]) +
  geom_pointrange(aes(x=x, y=y, ymin=ylo, ymax=yhi, colour = factor(id, rev(c("wo_trains","wo_egk","wo_speed","wo_temp","all")))), size = 0.5, fatten=1, position = position_dodge(width=0.75)) +
  coord_flip() +
  ylab("Slope") +
  xlab("") +
  labs(colour="Model") +
  geom_text(aes(x=x, y=y, label=round(y,3), colour = factor(id, rev(c("wo_trains","wo_egk","wo_speed","wo_temp","all")))), hjust=1.3, vjust=1.3, size=2, inherit.aes=FALSE, position = position_dodge(width=0.75)) +
  guides(colour = guide_legend(reverse = TRUE, override.aes = list(size=0.2))) +
  theme_bw() +
  scale_color_manual(values=c("#7eb79d", "#a09cbc", "#fb8072", "#80b1d3", "#db9546", "#969696")) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) +
  theme(plot.margin=unit(c(.5,.5,.1,.1),"cm")) +
  theme(axis.title.x = element_text(margin=unit(c(.3,0,0,0),"cm"))) +
  theme(axis.title.y = element_text(margin=unit(c(0,.3,0,0),"cm"))) +
  theme(panel.grid.major = element_line(size=0.1), panel.grid.minor = element_line(size=0.1)) +
  theme(text = element_text(size = 8))
p + geom_point(data=cv_plot_data[grep("calib_slope",row.names(cv_plot_data)), ], aes(x=x, y=y_orig, colour = factor(id, rev(c("wo_trains","wo_egk","wo_speed","wo_temp","all")))), size = 2.5, shape=1, inherit.aes=FALSE, position = position_dodge(width=0.75))
dev.off()

png('figs/validate_colour_03.png', pointsize = 10, res=300, width = 1500, height = 1200, bg='transparent')
p <- ggplot(data=cv_plot_data[grep("roc",row.names(cv_plot_data)), ]) +
  geom_pointrange(aes(x=x, y=y, ymin=ylo, ymax=yhi, colour = factor(id, rev(c("wo_trains","wo_egk","wo_speed","wo_temp","all")))), size = 0.5, fatten=1, position = position_dodge(width=0.75)) +
  coord_flip() +
  ylab("ROC") +
  xlab("") +
  labs(colour="Model") +
  geom_text(aes(x=x, y=y, label=round(y,3), colour = factor(id, rev(c("wo_trains","wo_egk","wo_speed","wo_temp","all")))), hjust=1.3, vjust=1.3, size=2, inherit.aes=FALSE, position = position_dodge(width=0.75)) +
  guides(colour = guide_legend(reverse = TRUE, override.aes = list(size=0.2))) +
  theme_bw() +
  scale_color_manual(values=c("#7eb79d", "#a09cbc", "#fb8072", "#80b1d3", "#db9546", "#969696")) +
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) +
  theme(plot.margin=unit(c(.5,0,.1,.1),"cm")) +
  theme(axis.title.x = element_text(margin=unit(c(.3,0,0,0),"cm"))) +
  theme(axis.title.y = element_text(margin=unit(c(0,.3,0,0),"cm"))) +
  theme(panel.grid.major = element_line(size=0.1), panel.grid.minor = element_line(size=0.1)) +
  theme(text = element_text(size = 8))
p + geom_point(data=cv_plot_data[grep("roc",row.names(cv_plot_data)), ], aes(x=x, y=y_orig, colour = factor(id, rev(c("wo_trains","wo_egk","wo_speed","wo_temp","all")))), size = 2.5, shape=1, inherit.aes=FALSE, position = position_dodge(width=0.75))
dev.off()

# png('figs/validate_colour.png', pointsize = 10, res=300, width = 900, height = 900, bg='transparent')
# p <- ggplot() +
#   geom_pointrange(data=cv_plot_data, aes(x=x, y=y, ymin=ylo, ymax=yhi, colour = factor(id, rev(c("wo_trains","wo_egk","wo_speed","wo_temp","all")))), size = 0.5, fatten=1, position = position_dodge(width=0.75)) +
#   coord_flip() +
#   ylab("Value") +
#   xlab("Test Metric") +
#   labs(colour="Model") +
#   geom_text(data=cv_plot_data, aes(x=x, y=y, label=round(y,3), colour = factor(id, rev(c("wo_trains","wo_egk","wo_speed","wo_temp","all")))), hjust=1.3, vjust=1.3, size=2, inherit.aes=FALSE, position = position_dodge(width=0.75)) +
#   guides(colour = guide_legend(reverse = TRUE, override.aes = list(size=0.2))) +
#   theme_bw() +
#   scale_color_manual(values=c("#7eb79d", "#a09cbc", "#fb8072", "#80b1d3", "#db9546", "#969696")) +
#   theme(plot.margin=unit(c(.5,0,.1,.1),"cm")) +
#   theme(axis.title.x = element_text(margin=unit(c(.3,0,0,0),"cm"))) +
#   theme(axis.title.y = element_text(margin=unit(c(0,.3,0,0),"cm"))) +
#   theme(panel.grid.major = element_line(size=0.1), panel.grid.minor = element_line(size=0.1)) +
#   theme(text = element_text(size = 8)) +
#   scale_y_continuous(expand = c(0,0)) +
#   scale_x_discrete(expand = c(0,0))
# p + geom_point(data=cv_plot_data, aes(x=x, y=y_orig, colour = factor(id, rev(c("wo_trains","wo_egk","wo_speed","wo_temp","all")))), size = 2.5, shape=1, inherit.aes=FALSE, position = position_dodge(width=0.75)) +
#   geom_segment(aes(x = 2.5, y = 0, xend = 3.5, yend = 0), linetype=2, size=0.1) +
#   geom_segment(aes(x = 0.5, y = 1, xend = 2.5, yend = 1), linetype=2, size=0.1) +
#   geom_segment(aes(x = 2.5, y = -2.5, xend = 2.5, yend = 2.5), linetype=1, size=0.1) +
#   geom_segment(aes(x = 1.5, y = -2.5, xend = 1.5, yend = 2.5), linetype=1, size=0.1)
# dev.off()