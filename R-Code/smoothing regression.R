#--------------------------------------------- SMOOTHING REGRESSION ---------------------------------------------
library(npreg)

data.s <- data
data.s <- subset(data.s, select = -c(status, market, ldate))
data.s <- data.s[complete.cases(data.s$tdp, 
                                data.s$bfreq, 
                                data.s$litho, 
                                data.s$ncore, 
                                data.s$temp, 
                                data.s$rprice),] 
data.ss <- subset(data.s, select = c(tdp, rprice))

mod.ss <- with(data.s, ss(rprice, tdp), lambda = 1e-15)
mod.ss <- ss(data.s$rprice, data.s$tdp, all.knots = TRUE)
mod.ss
summary(mod.ss)

# plot fit
plot(mod.ss, xlab = "rprice", ylab = "tdp")
rug(data.s$rprice)  # add rug to plot
points(data.s$rprice, data.s$tdp)