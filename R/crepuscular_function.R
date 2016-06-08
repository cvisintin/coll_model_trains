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
h <- seq(0, 24, len = 100)
light <- sin((2 * pi * (h - 6)) / 24) # ambient light intensity
light2 <- light ^ 2
diff <- nearerDawn(h)

par(mfrow = c(2, 1))
# plot the covariates
plot(light ~ h,
     type = 'l')
lines(light2 ~ h,
      lty = 2)
lines(diff ~ h,
      lty = 3)

# parameters
pr <- c(a = -9.15,   # intercept
        b1 = -0.71,  # day more likely than night
        b2 = -1.85, # midday / midnight less likely than crepuscular
        b3 = 0.26)  # dawn slightly more likely than dusk

curve <- pr['a'] +
  pr['b1'] * light +
  pr['b2'] * light2 +
  pr['b3'] * diff

# plot a relative collision rate for these parameters & covariates
plot(exp(curve) ~ h, type = 'l')

