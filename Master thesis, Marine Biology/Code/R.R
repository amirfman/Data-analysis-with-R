library(mgcv)
library(MASS)
library(caroline)

fs <- data.frame(
  'temp' = c(4.23, 5.52, 6.57, 7.46, 8.49, 9.36, 10.46, 10.72, 11.37, 12.42,
             4.23, 5.52, 6.57, 7.46, 8.49, 9.36, 10.46, 10.72, 11.37, 12.42,
             4.23, 5.52, 6.57, 7.46, 8.49, 9.36, 10.46, 10.72, 11.37, 12.42,
             4.23, 5.52, 6.57, 7.46, 8.49, 9.36, 10.46, 10.72, 11.37, 12.42),
  'Dataframe' = rep(c('PF1FERTILIZATION SUCCESS', 'PF2FERTILIZATION SUCCESS', 'PF3FERTILIZATION SUCCESS', 'PF4FERTILIZATION SUCCESS'), each = 10),
  'fert' = c(76.623377, 82.142857, 75.949367, 82.926829, 68.888889, 78.048780, 80.000000, 61.956522,
             63.333333, 52.941176,
             40.677966, 43.548387, 48.214286, 38.095238, 48.484848, 47.297297, 56.923077, 49.275362,
             46.666667, 30.000000,
             77.551020, 82.978723, 75.000000, 86.315789, 89.873418, 80.851064, 79.591837, 78.651685,
             71.428571, 60.810811,
             78.301887, 69.306931, 72.727273, 69.306931, 69.523810, 60.714286, 62.626263, 66.336634,
             56.074766, 41.747573)
)


mod <- gam(fert~s(temp, k=3), data=fs)
AIC(mod)
plot.gam(mod)
summary.gam(mod)

par(mfrow = c(1, 3))


#for Tmin data - left
px <- as.data.frame(p[p$temp<=xm,])
colnames(px) <- colnames(p)
pp <- predict(mod, newdata = px, type = "response")


lines(pp ~ temp, data = px, col = "red")
Xp <- predict(mod, px, type="lpmatrix")
beta <- coef(mod)
Vb   <- vcov(mod)

n <- 1000
set.seed(10)
mrand <- mvrnorm(n, beta, Vb)


var <- 0.90
opt <- rep(NA, n)
ilink <- family(mod)$linkinv
for (i in seq_len(n)) { 
  pred   <- ilink(Xp %*% mrand[i, ])
  opt[i] <- px$temp[which.min(abs(pred-max(pred)*var))]
}

ci <- quantile(opt, c(.05,.95))
ci
x = px$temp[which.min(abs(pred-max(pred)*var))]
x

#plot
plot(fs$fert ~ fs$temp, xlab = "Tempearture", ylab = "FERTILIZATION_Successl")
abline(v = px$temp[which.min(abs(pred-max(pred)*var))], lty = "dashed", col = "grey")
lines(pp ~ temp, data = px, col = "red")
lines(y = rep(min(fs$fert),2), x = ci, col = "blue")
points(y = min(fs$fert), x = px$temp[which.min(abs(pred-max(pred)*var))], pch = 16, col = "blue")

p  <- data.frame(temp = seq(min(fs$temp), max(fs$temp), length = 500))
pp <- predict(mod, newdata = p, type = "response")

#lines(pp ~ temp, data = p, col = "red")
Xp <- predict(mod, p, type="lpmatrix")
beta <- coef(mod)
Vb   <- vcov(mod)

n <- 1000
set.seed(10)
mrand <- mvrnorm(n, beta, Vb)


opt <- rep(NA, n)
ilink <- family(mod)$linkinv
for (i in seq_len(n)) { 
  pred   <- ilink(Xp %*% mrand[i, ])
  opt[i] <- p$temp[which.max(pred)]
}

ci <- quantile(opt, c(.05,.95))
ci
x = p$temp[which.max(pp)]
x

xm <- x

#plot
plot(fs$fert ~ fs$temp, xlab = "Tempearture", ylab = "FERTILIZATION_Successl")
abline(v = p$temp[which.max(pp)], lty = "dashed", col = "grey")
lines(pp ~ temp, data = p, col = "red")
lines(y = rep(min(fs$fert),2), x = ci, col = "blue")
points(y = min(fs$fert), x = p$temp[which.max(pp)], pch = 16, col = "blue")


#for Tmax data - right
px <- as.data.frame(p[p$temp>=xm,])
colnames(px) <- colnames(p)
pp <- predict(mod, newdata = px, type = "response")

lines(pp ~ temp, data = px, col = "red")
Xp <- predict(mod, px, type="lpmatrix")
beta <- coef(mod)
Vb   <- vcov(mod)

n <- 1000
set.seed(10)
mrand <- mvrnorm(n, beta, Vb)


var <- 0.90
opt <- rep(NA, n)
ilink <- family(mod)$linkinv
for (i in seq_len(n)) { 
  pred   <- ilink(Xp %*% mrand[i, ])
  opt[i] <- px$temp[which.min(abs(pred-max(pred)*var))]
}

ci <- quantile(opt, c(.05,.95))
ci
x = px$temp[which.min(abs(pred-max(pred)*var))]
x

#plot
plot(fs$fert ~ fs$temp, xlab = "Tempearture", ylab = "FERTILIZATION_Successl")
abline(v = px$temp[which.min(abs(pred-max(pred)*var))], lty = "dashed", col = "grey")
lines(pp ~ temp, data = px, col = "red")
lines(y = rep(min(fs$fert),2), x = ci, col = "blue")
points(y = min(fs$fert), x = px$temp[which.min(abs(pred-max(pred)*var))], pch = 16, col = "blue")




#get data
newdat_fert <- data.frame(temp=rep(-2.0:12.5, 1)) 
p2 <- predict(mod, newdata=newdat_fert, type = "response", se.fit=TRUE)
newdat_fert$fert <- p2$fit
newdat_fert$fert_se <- p2$se.fit
newdat_fert$upr <- p2$fit + (1.96 * p2$se.fit)
newdat_fert$lwr <- p2$fit - (1.96 * p2$se.fit)
head(newdat_fert, file=cod_dahlke_)
write.delim(newdat_fert,file='cod_opt2.txt')



