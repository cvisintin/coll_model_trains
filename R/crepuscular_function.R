d <- function (h, peak, trough, l) {
  # evaluate a two-Gaussian function for given peak/trough/lengthscale
  exp(-((h - peak) / l) ^ 2) -   exp(-((h - trough) / l) ^ 2)
}

nearerDawn <- function (h, dawn = 6, dusk = 18, slope = 2) {
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

# plot a periodic curve (on 24h) which is 1 at dawn and -1 at dusk, for any
# dawn/dusk
par(mfrow = c(3, 1))
for (dd in list(c(6, 18),
                c(8, 16),
                c(4, 20))) {
  
  h_long <- seq(-4, 28, len = 100)
  h <- seq(0, 24, len = 100)
  
  plot(nearerDawn(h_long, dd[1], dd[2]) ~ h_long,
       type = 'l',
       col = grey(0.6))
  lines(nearerDawn(h, dd[1], dd[2]) ~ h)
  abline(h = 0, lty = 3)
  abline(v = dd, lty = 3)
  abline(v = c(0, 24))
}


# ~~~~~~~~~~~~
# get three covariates to model temporally-varying risk:
# light - controls whether collisions are more likely during day or night
# light2 - controls whether collisions are more likely at crepuscular times
# diff - controls whether collisions are more likely near dawn than dusk

# get fake data
h <- seq(0, 24, len = 240)
light <- sin((2 * pi * (h - 6)) / 24) # ambient light intensity

light_x <- -cos(pi*h/12)

light2 <- light ^ 2

light2_x <- (-cos(pi*h/12))^2

diff <- nearerDawn(h)

#par(mfrow = c(2, 1))
# plot the covariates
png('figs/crep01.png', pointsize = 10, res=300, width = 900, height = 900, bg='transparent')
  plot(light ~ h, type = 'l', xaxt='n', xlab='', ylab='', cex.axis=0.6, yaxp=c(-1,1,10), las=1, mgp=c(2.5,1,0), par(mar=c(3.0,2.0,1.5,0.5)), cex.lab=0.8)
  axis(1, cex.axis=0.6, xaxp=c(0,24,24), mgp=c(2.5,0.5,0))
  title(xlab='HOUR', mgp=c(1.8,0.5,0), cex.lab=0.8, line = 1.5)
  lines(light2 ~ h, lty = 2)
  lines(diff ~ h, lty = 3)
dev.off()

# parameters
pr <- c(a = 0,   # intercept
        b1 = 1.2,  # day more likely than night
        b2 = -1.3, # midday / midnight less likely than crepuscular
        b3 = 0.4)  # dawn slightly more likely than dusk

curve <- pr['a'] +
  pr['b1'] * light +
  pr['b2'] * light2 +
  pr['b3'] * diff

png('figs/crep02.png', pointsize = 10, res=300, width = 900, height = 900, bg='transparent')
plot(exp(curve) ~ h, type = 'l', xaxt='n', xlab='', ylab='RELATIVE ACTIVITY', cex.axis=0.6, las=1, mgp=c(2.5,1,0), par(mar=c(3.0,3.5,1.5,0.5)), cex.lab=0.8)
axis(1, cex.axis=0.6, xaxp=c(0,24,24), mgp=c(2.5,0.5,0))
title(xlab='HOUR', mgp=c(1.8,0.5,0), cex.lab=0.8)
dev.off()

var <- seq(-0.5,0.5,.25)

for(i in var){
  for(j in var){
    for(k in var){
      curve <- i*light + j*light2 + k*diff
      plot(exp(curve) ~ h, type = 'l', xaxt='n', xlab='', ylab='RELATIVE ACTIVITY', cex.axis=0.6, las=1, mgp=c(2.5,1,0), par(mar=c(3.0,3.5,1.5,0.5)), cex.lab=0.8, cex.main=0.8, main=paste0('b1=',i,' b2=',j,' b3=',k))
      axis(1, cex.axis=0.6, xaxp=c(0,24,24), mgp=c(2.5,0.5,0))
      title(xlab='HOUR', mgp=c(1.8,0.5,0), cex.lab=0.8)
    }
  }
}

for(i in 1:10){
  set.seed(i)
  draw <- sample(seq(-2.5,2.5,0.1),3)
  curve <- draw[1]*light + draw[2]*light2 + draw[3]*diff
  png(paste0('figs/crep',i+19,'.png'), pointsize = 10, res=300, width = 900, height = 900, bg='transparent')
  plot(exp(curve) ~ h, type = 'l', xaxt='n', xlab='', ylab='', cex.axis=0.6, las=1, mgp=c(2.5,1,0), par(mar=c(3.0,3.5,1.5,0.5)), cex.lab=0.8, cex.main=0.8, main=paste0('g1=',draw[1],'  g2=',draw[2],'  g3=',draw[3]))
  axis(1, cex.axis=0.6, xaxp=c(0,24,24), mgp=c(2.5,0.5,0))
  title(xlab='HOUR', mgp=c(1.8,0.5,0), cex.lab=0.8, line = 1.5)
  mtext(side = 2, "RELATIVE ACTIVITY", line = 2, cex=0.8)
  dev.off()
}


# plot a relative collision rate for these parameters & covariates
plot(exp(curve) ~ h, type = 'l')

#####################Plots for VicBio Conf talk######################

