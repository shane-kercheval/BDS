library(tidyverse)

##  browser spending analysis 
browser = read.csv("web-browsers.csv")
browser %>% View()

# data histogram
browser %>%
    ggplot(aes(x=spend)) +
    geom_histogram(bins=30) +
    scale_x_log10(breaks = 10^(0:5), labels = scales::comma)

par(mai=c(.8,.8,.1,.1))
hist(log(browser$spend), freq=FALSE,
	xaxt="n", main="", xlab="total online spend", col=8, border="grey90")
lgrid = c(1,10,100,1000,10000,100000)
axis(1, at=log(lgrid), labels=lgrid)

# basic stats
nrow(browser)
# sample mean
mean(browser$spend)
# variance of *sample mean* i.e. of sampling distribution (not the variance of underlying data)
var(browser$spend)/nrow(browser)
# standard deviation of sampling distribution (not sd of underlying data)
sqrt(var(browser$spend)/nrow(browser))
# sd of underlying data
sd(browser$spend)

(xbar <- mean(browser$spend))
(xbse <-  sd(browser$spend)/sqrt(nrow(browser)))

xx <- seq(1650,2250,length=1000)

par(mai=c(.9,.8,.2,.2))
plot(xx, dnorm(xx, xbar, xbse), type="l", col="royalblue", lwd=1.5,
	xlab="average total online spend", ylab="density")

# nonparametric bootstrap
B <- 10000
mub <- c()
for (b in 1:B){
	samp_b = sample.int(nrow(browser), replace=TRUE)
	mub <- c(mub, mean(browser$spend[samp_b]))
}
mean(mub)
sd(mub)

par(mai=c(.8,.8,.2,.2))
hist(mub, main="", xlab="average total online spend", 
	col=8, border="grey90", freq=FALSE)
lines(xx, dnorm(xx, xbar, xbse), col="royalblue", lwd=1.5)

########################################################################
# Bootstrap 95% CI for R-Squared
library(boot)
# function to obtain R-Squared from the data 
get_mean <- function(data, indices) {
    d <- data[indices,]$spend # allows boot to select sample 
    return(mean(d))
} 
# bootstrapping with 1000 replications 
results <- boot(data=browser, statistic=get_mean, R=1000)
# view results
results 
plot(results)

# get 95% confidence interval 
boot.ci(results, type="bca")
########################################################################


## parametric bootstrap
xbar <- mean(browser$spend)
sig2 <-  var(browser$spend)

B <- 10000
mus <- c()
for(b in 1:B){
  xsamp <- rnorm(1e4, xbar, sqrt(sig2))
  mus <- c(mus, mean(xsamp))
}
sd(mus)
sqrt(sig2/1e4)

## usual estiamtion of variance
#set.seed(43)
#(seed <- sample.int(1000, 1))
#set.seed(seed)
set.seed(222)
sample_size <- 200
smallsamp_indices <- sample.int(n=nrow(browser), size=sample_size)
smallsamp <- browser$spend[smallsamp_indices]
(s <- sd(smallsamp)) # sample variance
sd(browser$spend)
s/sd(browser$spend)

# a potential problem is that smallsamp isn't a great representation of the underlying data
# log(browser$spend) is pretty log-normal, while log(smallsamp) is left-skewed 
hist(log(browser$spend))
hist(log(smallsamp))

## CI bootstrap
# now we are using this small sample to estimate the standard deviation
eb <- c()  # eb are the bootstrap errors (third bullet in Algo 2 pg 27)
B <- 10000
for (b in 1:B){
	sb <- sd(smallsamp[sample.int(sample_size, replace=TRUE)])
	eb <- c(eb, sb-s)
}
mean(s - eb)
sd(browser$spend)

tvals <- quantile(eb, c(0.05, 0.95))
tvals
s - tvals[2:1]
####
# function to obtain R-Squared from the data 
get_sd <- function(data, indices) {
    d <- data[indices,]$spend # allows boot to select sample 
    return(sd(d))
} 
# bootstrapping with 1000 replications 
results <- boot(data=browser[smallsamp_indices,], statistic=get_sd, R=10000)
# view results
results 
s
mean(eb) # bias from manual bootstrap
plot(results)
# get 95% confidence interval 
boot.ci(results, type=c("perc", "basic"))
####


## regression analysis
summary( glm( log(spend) ~ broadband + anychildren, data=browser) )

# nonparametric bootstrap
B <- 1000
betas <- c()
for (b in 1:B){
	samp_b = sample.int(nrow(browser), replace=TRUE)
	reg_b <- glm(log(spend) ~ broadband + anychildren, data=browser[samp_b,])
	betas <- rbind(betas, coef(reg_b))
}
head(betas)
mean(betas[, 'broadband'])
sd(betas[, 'broadband'])

cor(betas[,"broadband"], betas[,"anychildren"])

xx <- seq(min(betas[,2]),max(betas[,2]),length=100)

par(mai=c(.8,.8,.2,.2))
hist(betas[,2], main="", xlab="broadband coefficient", 
	col=8, border="grey90", freq=FALSE)
lines(xx, dnorm(xx, 0.55285, 0.04357), col="royalblue", lwd=1.5)

par(mai=c(.8,.8,.2,.2))
hist(exp(betas[,2]), main="", xlab="broadband multiplier", 
	col=8, border="grey90", freq=FALSE)


spendy <- glm( log(spend) ~ .-id, data=browser) 
round(summary(spendy)$coef,2)
pval <- summary(spendy)$coef[-1,"Pr(>|t|)"]
options(scipen=999)
round(sort(pval), 4)

pdf("fig1.9.pdf", width=4, height=4)
par(mai=c(.8,.8,.2,.2))

length(pval)
nrow(browser)

q_cutoff <- 0.1

q_cutoff / length(pval)
.1/9

plot(sort(pval), bty="n", xlab="rank", ylab=expression(italic(p)-values))
abline(a=0, b=.1/9)
points(sort(pval)[1:5], col=2, pch=20)
dev.off()

par(mai=c(.8,.8,.2,.2))
plot(c(-1,0,0,1,1,2), c(0,0,1,1,0,0), ylim=c(0,1.5), xlim=c(-0.1,1.1),
	type="l", bty="n", xlab="U", ylab="probability density", main = "uniform pdf")


par(mai=c(.8,.8,.2,.2))
plot(1:9, (1:9)/10, ylim=c(0,1),
	pch=16, col="black", bty="n", ylab="p-value", 
	xlab="order", main = "p-value order statistics")
points(1:9, sort(pval), pch=17, col=rgb(0,0,1,.5))
legend("topleft", bty="n",
	legend=c("expectation under null","observed"), pch=c(16,17),
	col=c("black",rgb(0,0,1,.5)))

