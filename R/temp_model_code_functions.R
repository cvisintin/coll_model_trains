for (i in 1:n.obs) {
y[i] ~ dpois(lambda[i])
log(lambda[i]) <- beta[1] * EGK[i] + beta[2] * TRAINS[i] + beta[3] * SPEED[i] +
#          gamma[1] * light[i] + gamma[2] * light2[i] + gamma[3] * diff[i]
                    fun_term[i]
fun_term[i] <- gamma[1] * exp(gamma[3] * h[i] dawn[i] dusk[i]) - gamma[2] * exp()
}

#priors
for (i in 1:3) {
  beta[i] ~ dnorm(0.0, 0.001)
}


"roc" <- function (obsdat, preddat){
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

set.seed(123)

n <- 5000

s.egk <- rpois(n,1)
s.egk <- s.egk/max(s.egk)
s.speed100 <- rnorm(n,0.86,.19)
s.hour <- runif(n,0,23)
s.dawn <- runif(n,5,8)
s.dusk <- runif(n,17.5,21.5)
s.offset <- rnorm(n,1,1)

for (i in 1:n) {
  diff_inner[i] <- (s.dusk[i] - s.dawn[i]) / 2
  diff_outer[i] <- ((24 - s.dusk[i]) + s.dawn[i]) / 2
  
  if (s.hour[i] < s.dawn[i]){
    s.dawnordusk[i] <- exp(-((s.hour[i] - s.dawn[i]) / diff_outer[i]) ^ 2) - exp(-((s.hour[i] - (s.dusk[i] - 24)) / diff_outer[i]) ^ 2)
  }
  if (s.hour[i] < s.dusk[i]){
    s.dawnordusk[i] <- exp(-((s.hour[i] - s.dawn[i]) / diff_inner[i]) ^ 2) -  exp(-((s.hour[i] - s.dusk[i]) / diff_inner[i]) ^ 2)
  }else{
    s.dawnordusk[i] <- exp(-((s.hour[i] - (s.dawn[i]+24)) / diff_outer[i]) ^ 2) - exp(-((s.hour[i] - s.dusk[i]) / diff_outer[i]) ^ 2)
  }
  
  s.light[i] <- sin((2 * pi * (s.hour[i] - 6)) / 24)
  
  s.light2[i] <- sin((2 * pi * (s.hour[i] - 6)) / 24) * sin((2 * pi * (s.hour[i] - 6)) / 24)
}

b <- c(-5,rep(-0.5, 5))

s.coll <- rbinom(n, 1, invcloglog(b[1] + b[2]*s.egk + b[3]*s.speed100 + b[4]*s.light + b[5]*s.light2 + b[6]*s.dawnordusk + s.offset))


y <- s.coll
egk <- s.egk
speed <- s.speed100
light <- s.light
light2 <- s.light2
dawnordusk <- s.dawnordusk
offset <- s.offset

hour <- s.hour
dawn <- s.dawn
dusk <- s.dusk

model.data <- as.data.table(read.delim("data/model_data_hm.csv", header=T, sep=","))
model.data.bin <- copy(model.data)
model.data.bin[coll>1,coll:=1]

n <- nrow(model.data.bin)
y <- model.data.bin$coll
egk <- model.data.bin$egk
trains <- model.data.bin$trains
speed <- model.data.bin$speed
light <- model.data.bin$light
light2 <- model.data.bin$light2
dawnordusk <- model.data.bin$dawnordusk
kilometre <- model.data.bin$length

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
  vector [n] log_egk_c;
  vector [n] log_trains_c;
  vector [n] log_speed_c;
  vector[n] light_c;
  vector[n] light2_c;
  vector[n] dawnordusk_c;
  vector [n] log_kilometre;
    
  log_egk_c <- log(egk)-mean(log(egk));
  log_trains_c <- log(trains)-mean(log(trains));
  log_speed_c <- log(speed)-mean(log(speed));
  
  light_c <- light-mean(light);
  light2_c <- light2-mean(light2);
  dawnordusk_c <- dawnordusk-mean(dawnordusk);

  log_kilometre <- log(kilometre);
}
  parameters{
  vector[7] b;
}
model{
  vector [n] p;
  for (i in 1:n) p[i] <- inv_cloglog(b[1] + b[2]*log_egk_c[i] + b[3]*log_trains_c[i] + b[4]*log_speed_c[i] + b[5]*light_c[i] + b[6]*light2_c[i] + b[7]*dawnordusk_c[i] + log_kilometre[i]);

  b ~ normal(0,1);
  y ~ bernoulli(p);
}
//generated quantities{
// vector[n] y_rep;
// for(i in 1:n){
//  y_rep[i] <- binomial_rng(1,inv_cloglog(b[1] + b[2]*egk[i] + b[3]*speed[i] + b[4]*light[i] + b[5]*light2[i] + b[6]*dawnordusk[i] + offset[i])); //posterior draws to get posterior predictive checks
// }
//}
" #1291.14 seconds (Total)

coll.stan.bin2 <- stan(model_code = scode.bin, warmup=250, iter = 1250, chains = 1, cores = 1, seed=123)#, control =list(stepsize=0.01, adapt_delta=0.9, max_treedepth=15))# 962.779 seconds (Total)

pairs(coll.stan.bin2)

traceplot(As.mcmc.list(coll.stan.bin2))

save(coll.stan.bin2,file="data/coll_stan_bin2")
load("data/coll_stan_bin2")

coll.arm.bin <- bayesglm(coll ~ I(log(egk)-mean(log(egk))) + I(log(trains)-mean(log(trains))) + I(log(speed)-mean(log(speed))) + I(light-mean(light)) + I(light2-mean(light2)) + I(dawnordusk-mean(dawnordusk)), offset=log(length), data=model.data.bin, family=binomial(link="cloglog"))
summary(coll.arm.bin)
paste("% Error Explained: ",round(((coll.arm.bin$null.deviance - coll.arm.bin$deviance)/coll.arm.bin$null.deviance)*100,2),sep="") 


scode.bin <- "
data{
  int<lower=1> n;
  int<lower=0,upper=1> y[n];
  vector[n] egk;
  vector[n] trains;
  vector[n] speed;
  vector[n] hour;
  vector[n] dawn;
  vector[n] dusk;
  vector[n] kilometre;
}
transformed data{
  vector [n] diff_inner;
  vector [n] diff_outer;
  vector [n] dawnordusk;
  vector [n] light2;
  vector [n] light;
  vector [n] log_egk;
  vector [n] log_trains;
  vector [n] log_speed;
  vector [n] log_kilometre;

  for (i in 1:n) {
    diff_inner[i] <- (dusk[i] - dawn[i]) / 2;

    diff_outer[i] <- ((24 - dusk[i]) + dawn[i]) / 2;

    if (hour[i] < dawn[i]) dawnordusk[i] <- exp(-((hour[i] - dawn[i]) / diff_outer[i]) ^ 2) -   exp(-((hour[i] - (dusk[i] - 24)) / diff_outer[i]) ^ 2);

    else if (hour[i] < dusk[i]) dawnordusk[i] <- exp(-((hour[i] - dawn[i]) / diff_inner[i]) ^ 2) -   exp(-((hour[i] - dusk[i]) / diff_inner[i]) ^ 2);

    else dawnordusk[i] <- exp(-((hour[i] - (dawn[i]+24)) / diff_outer[i]) ^ 2) -   exp(-((hour[i] - dusk[i]) / diff_outer[i]) ^ 2);

    light2[i] <- sin((2 * pi() * (hour[i] - 6)) / 24) * sin((2 * pi() * (hour[i] - 6)) / 24);

    light[i] <- sin((2 * pi() * (hour[i] - 6)) / 24);

    log_egk[i] <- log(egk[i]);

    log_trains[i] <- log(trains[i]);

    log_speed[i] <- log(speed[i]);

    log_kilometre[i] <- log(kilometre[i]);
  }
}
parameters{
  vector[4] b;
  vector[3] g;
}
model{
  vector [n] p;
  vector [n] crep;
    
  crep <- g[1]*light + g[2]*light2 + g[3]*dawnordusk;
  
  for (i in 1:n) p[i] <- inv_cloglog(b[1] + b[2]*log_egk[i] + b[3]*log_trains[i] + b[4]*log_speed[i] + crep[i] + log_kilometre[i]);

  b ~ normal(0,1);
  g ~ normal(0,1);
  y ~ bernoulli(p);
}
//generated quantities{
//  vector[n] y_rep;
//  //vector[n] p;
//  for(i in 1:n){
//    y_rep[i] <- binomial_rng(1,inv_cloglog(b[1] + b[2]*egk[i] + b[3]*speed[i] + g[1]*light[i] + g[2]*light2[i] + g[3]*dawnordusk[i] + offset[i])); //posterior draws to get posterior predictive checks
//    //p[i] <- inv_cloglog(b[1] + b[2]*egk[i] + b[3]*speed[i] + g[1]*light[i] + g[2]*light2[i] + g[3]*dawnordusk[i] + offset[i]);
//  }
//}
" #7681.45 seconds (Total)

n <- nrow(model.data.bin)
y <- model.data.bin$coll
egk <- model.data.bin$egk
trains <- model.data.bin$trains
speed <- model.data.bin$speed
hour <- model.data.bin$hour
dawn <- model.data.bin$dawn
dusk <- model.data.bin$dusk
kilometre <- model.data.bin$length

coll.stan.bin <- stan(model_code = scode.bin, warmup=250, iter = 500, chains = 3, cores = 3, seed=123)# 2223.63 seconds (Total) - 2713.21 seconds (Total w gen quant)

#          mean se_mean   sd     2.5%      25%      50%      75%    97.5% n_eff Rhat
# b[1]    -7.70    0.04 0.77    -9.05    -8.23    -7.75    -7.20    -6.18   305 1.01
# b[2]     0.54    0.00 0.05     0.45     0.51     0.54     0.57     0.64   505 1.00
# b[3]    -0.13    0.00 0.09    -0.30    -0.19    -0.13    -0.07     0.06   392 1.00
# b[4]     0.78    0.01 0.17     0.44     0.67     0.78     0.90     1.10   314 1.00
# g[1]    -0.70    0.01 0.11    -0.93    -0.77    -0.70    -0.62    -0.48   352 1.01
# g[2]    -1.81    0.01 0.18    -2.15    -1.93    -1.80    -1.68    -1.47   514 1.00
# g[3]     0.28    0.00 0.07     0.15     0.24     0.28     0.32     0.41   420 1.01
# lp__ -2844.39    0.10 1.86 -2848.86 -2845.49 -2844.13 -2842.97 -2841.70   368 1.00

save(coll.stan.bin,file="data/coll_stan_bin")
load("data/coll_stan_bin")

pairs(coll.stan.bin, pars=c("b","g"))

y_rep <- extract(coll.stan.bin, pars="y_rep", permuted=TRUE)$y_rep

summary(y_rep[1,])

auc <- NA
for(i in 1:length(y_rep[,1])) auc[i] <- roc(y,y_rep[i,])
mean(auc)


plot(jitter(y_rep[1,]),jitter(y))


print(coll.stan.bin, pars=c("b","g"))
#        mean se_mean   sd   2.5%    25%    50%    75%  97.5% n_eff Rhat
# b[1] -10.55    0.01 0.24 -11.04 -10.70 -10.55 -10.40 -10.08   266 1.01
# b[2]   1.64    0.01 0.26   1.13   1.47   1.64   1.83   2.16   489 1.00
# b[3]   3.15    0.01 0.24   2.69   2.98   3.14   3.31   3.62   298 1.01
# g[1]  -1.13    0.00 0.11  -1.34  -1.21  -1.13  -1.05  -0.91   569 1.00
# g[2]  -1.56    0.01 0.17  -1.89  -1.67  -1.56  -1.44  -1.21   509 1.01
# g[3]   0.43    0.00 0.07   0.30   0.38   0.43   0.48   0.56   494 1.00

# Inference for Stan model: daadf87b8c5be050ed84ffd325dee683.
# 1 chains, each with iter=1000; warmup=250; thin=1; 
# post-warmup draws per chain=750, total post-warmup draws=750.
# 
#        mean se_mean   sd   2.5%    25%    50%    75%  97.5% n_eff Rhat
# b[1] -10.56    0.01 0.25 -11.04 -10.73 -10.56 -10.40 -10.05   297 1.00
# b[2]   1.66    0.01 0.25   1.16   1.50   1.68   1.84   2.11   411 1.00
# b[3]   3.15    0.01 0.25   2.66   2.97   3.15   3.32   3.64   307 1.00
# g[1]  -1.13    0.00 0.11  -1.34  -1.21  -1.13  -1.06  -0.92   592 1.00
# g[2]  -1.56    0.01 0.16  -1.86  -1.66  -1.56  -1.46  -1.23   344 1.01
# g[3]   0.43    0.00 0.07   0.30   0.39   0.43   0.47   0.56   511 1.00

light <- model.data.bin$light
light2 <- model.data.bin$light2
dawnordusk <- model.data.bin$dawnordusk

coll.stan.bin <- stan(model_code = scode.bin, iter = 500, chains = 3, cores = 3, seed=123)# 1720.04 seconds (Total)

print(coll.stan.bin, pars=c("b"))
#        mean se_mean   sd   2.5%    25%    50%    75%  97.5% n_eff Rhat
# b[1] -10.59    0.01 0.26 -11.10 -10.77 -10.59 -10.41 -10.11   438 1.00
# b[2]   1.67    0.01 0.26   1.16   1.49   1.68   1.84   2.15   445 1.00
# b[3]   3.18    0.01 0.26   2.68   3.00   3.18   3.37   3.67   438 1.00
# b[4]  -1.13    0.01 0.11  -1.35  -1.20  -1.13  -1.04  -0.93   433 1.01
# b[5]  -1.55    0.01 0.17  -1.89  -1.66  -1.55  -1.44  -1.19   628 1.00
# b[6]   0.43    0.00 0.07   0.30   0.39   0.43   0.47   0.56   477 1.00

coll_sim <- extract(coll.stan.bin, permuted=TRUE)

n_sims <- length(coll_sim$lp__)
y_rep <- as.vector(rep(NA, n_sims))
for(s in 1:n_sims) y_rep[s] <- rbinom(1, 1, invcloglog(coll_sim$b[s,1] + coll_sim$b[s,2]*mean(egk) + coll_sim$b[s,3]*mean(speed) + coll_sim$b[s,4]*mean(light) + coll_sim$b[s,5]*mean(light2) + coll_sim$b[s,6]*mean(dawnordusk) + mean(offset)))


n <- nrow(model.data)
y <- model.data$coll
egk <- model.data$egk
speed <- model.data$speed100
hour <- model.data$hour
dawn <- model.data$dawn
dusk <- model.data$dusk
offset <- model.data$offset

scode.pois <- "
data{
int<lower=1> n;
int<lower=0> y[n];
vector[n] egk;
vector[n] speed;
vector[n] hour;
vector[n] dawn;
vector[n] dusk;
vector[n] offset;
}
transformed data{
vector [n] diff_inner;
vector [n] diff_outer;
vector [n] dawnordusk;
vector [n] light2;
vector [n] light;

for (i in 1:n) {
diff_inner[i] <- (dusk[i] - dawn[i]) / 2;

diff_outer[i] <- ((24 - dusk[i]) + dawn[i]) / 2;

if (hour[i] < dawn[i]) dawnordusk[i] <- exp(-((hour[i] - dawn[i]) / diff_outer[i]) ^ 2) -   exp(-((hour[i] - (dusk[i] - 24)) / diff_outer[i]) ^ 2);

else if (hour[i] < dusk[i]) dawnordusk[i] <- exp(-((hour[i] - dawn[i]) / diff_inner[i]) ^ 2) -   exp(-((hour[i] - dusk[i]) / diff_inner[i]) ^ 2);

else dawnordusk[i] <- exp(-((hour[i] - (dawn[i]+24)) / diff_outer[i]) ^ 2) -   exp(-((hour[i] - dusk[i]) / diff_outer[i]) ^ 2);

light2[i] <- sin((2 * pi() * (hour[i] - 6)) / 24) * sin((2 * pi() * (hour[i] - 6)) / 24);

light[i] <- sin((2 * pi() * (hour[i] - 6)) / 24);
}
}
parameters{
vector[3] b;
vector[3] g;
}
model{
vector [n] l;
vector [n] crep;

crep <- g[1]*light + g[2]*light2+ g[3]*dawnordusk;

for (i in 1:n) l[i] <- exp(b[1] + b[2]*egk[i] + b[3]*speed[i] + crep[i] + offset[i]);

b ~ normal(0,1);
g ~ normal(0,1);
y ~ poisson(l);
}
//generated quantities{
//  vector[n] y_rep;
//  //vector[n] p;
//  for(i in 1:n){
//    y_rep[i] <- binomial_rng(1,inv_cloglog(b[1] + b[2]*egk[i] + b[3]*speed[i] + g[1]*light[i] + g[2]*light2[i] + g[3]*dawnordusk[i] + offset[i])); //posterior draws to get posterior predictive checks
//    //p[i] <- inv_cloglog(b[1] + b[2]*egk[i] + b[3]*speed[i] + g[1]*light[i] + g[2]*light2[i] + g[3]*dawnordusk[i] + offset[i]);
//  }
//}
"
coll.stan.pois <- stan(model_code = scode.pois, iter = 500, chains = 3, cores = 3, seed=123)                  

print(coll.stan.pois, pars=c("b","g"))

pairs(coll.stan.pois, pars=c("b","g"))



n <- nrow(model.data.bin)
coll <- model.data.bin$coll
egk <- model.data.bin$egk
trains <- model.data.bin$trains
speed <- model.data.bin$speed
light <- model.data.bin$light
light2 <- model.data.bin$light2
dawnordusk <- model.data.bin$dawnordusk
length <- model.data.bin$length


set.seed(123) 
coll.map.bin <- map(
  alist(
    y ~ dbinom(1,p), 
    p <- 1-exp(-exp(a + b1*log(x1) + b2*log(x2) + b3*log(x3) + b4*x4 + b5*x5 + b6*x6 + log(x7))),
    a ~ dnorm(0,1),
    b1 ~ dnorm(0,1),
    b2 ~ dnorm(0,1),
    b3 ~ dnorm(0,1),
    b4 ~ dnorm(0,1),
    b5 ~ dnorm(0,1)
  ),
  start=list(a=-8.5,b1=-0.5,b2=-0.5,b3=-0.5,b4=-0.5,b5=-0.5,b6=-0.5),
  data=list(y=coll,x1=egk,x2=trains,x3=speed,x4=light,x5=light2,x6=dawnordusk,x7=length)
)
precis(coll.map.bin)

WAIC(coll.map.bin)

coll.map2stan.bin <- map2stan(coll.map.bin)

