#### web-browsing data
library(gamlr)
library(tidyverse)

## Browsing History. 
## The table has three colums: [machine] id, site [id], [# of] visits
web <- read.csv("browser-domains.csv")
## Read in the actual website names and relabel site factor
sitenames <- scan("browser-sites.txt", what="character")
web$site <- factor(web$site, levels=1:length(sitenames), labels=sitenames)
## also factor machine id
web$id <- factor(web$id, levels=1:length(unique(web$id)))

head(web)
## get total visits per-machine and % of time on each site
## tapply(a,b,c) does c(a) for every level of factor b.

web %>% count(id, wt=visits) %>% head()

machinetotals <- as.vector(tapply(web$visits, web$id, sum))
machinetotals[1:6]  # same as count above
visitpercent <- 100 * web$visits / machinetotals[web$id]

dplyr_web <- web %>%
    mutate(visit_percent = visitpercent) %>%
    select(-visits)
as.numeric(object.size(dplyr_web)) / 1024^2

# the end result is that each id should add up to 100%
dplyr_web %>%
    group_by(id) %>%
    summarise(total_percent = sum(visit_percent)) %>%
    ungroup()

#this is the same thing as the sparse matrix below (only takes up more memory, presumably)
dplyr_web <- dplyr_web %>%
    pivot_wider(names_from = site,
                values_from = visit_percent)
# more than doubles in size
as.numeric(object.size(dplyr_web)) / 1024^2
head(dplyr_web[1:10, 1:5])

## use this info in a sparse matrix
## this is something you'll be doing a lot; familiarize yourself.
# this seems to auto arrange by web$id
xweb <- sparseMatrix(
	i=as.numeric(web$id),  # i = row = person; this will have duplicates as the same person can appear many times in the dataset (i.e may be assocaited with multiple websites)
	j=as.numeric(web$site),  # j = column = website; this will also have duplicates as each person is associated with many websites and a website with many people
	x=visitpercent,  # x = value at row i column j
	dims=c(nlevels(web$id),  # number of rows
	       nlevels(web$site)),  # number of columns
	dimnames=list(id=levels(web$id),  # row names
	              site=levels(web$site)))  # column names

# smaller than both before/longer dplyr_web
as.numeric(object.size(xweb)) / 1024^2

dplyr_web %>%
    filter(id == 1) %>%
    select(`atdmt.com`, `yahoo.com`, `whenu.com`) %>%
    head()
xweb[1, 1:3]

nrow(web)
nlevels(web$id)
nlevels(web$site)
dim(xweb)
10000*1000  # number of possible "cells" of sparse matrix
nrow(web)  # but only this many are populated
nrow(web) / (10000 * 1000)  # about ~23% of cells are populated
summary(xweb)

# what sites did household 1 visit?
View(as.matrix(xweb))
head(xweb[1, xweb[1,]!=0])
sum(xweb[1, xweb[1,]!=0])

## now read in the spending data 
yspend <- read.csv("browser-totalspend.csv", row.names=1)  # us 1st column as row names
yspend <- as.matrix(yspend) ## good practice to move from dataframe to matrix

## run a lasso path plot
# xweb seems to auto arrange by web$id, so these should be in the same order, even
# though it doesn't look like we did anything to manually arrange
spender <- gamlr(xweb, log(yspend), verb=TRUE)
plot(spender) ## path plot

# run a stepwise regression plot
stepspend <- gamlr(xweb, log(yspend), verb=TRUE, gamma=Inf, lmr=.1)
par(mai=c(.9,.9,.1,.1))
plot(stepspend, df=FALSE, select=FALSE) ## path plot



B <- coef(spender) ## the coefficients selected under AICc
## a few examples
B <- B[-1,] # drop intercept and remove STM formatting
B[which.min(B)] ## low spenders spend a lot of time here
B[which.max(B)] ## big spenders hang out here

coef(spender, select=which.min(BIC(spender))) ## and BIC instead
AIC(spender)
cv.spender <- cv.gamlr(xweb, log(yspend), verb=TRUE)
beta1se <- coef(cv.spender) ## 1se rule; see ?cv.gamlr
betamin <- coef(cv.spender, select="min") ## min cv selection
cbind(beta1se,betamin)[c("tvguide.com","americanexpress.com"),]

## plot them together
dev.off()
par(mfrow=c(1,2))
plot(cv.spender)
plot(cv.spender$gamlr) ## cv.gamlr includes a gamlr object

## log lambdas selected under various criteria
log(spender$lambda[which.min(AICc(spender))])
log(spender$lambda[which.min(AIC(spender))])
log(spender$lambda[which.min(BIC(spender))])
log(cv.spender$lambda.min)
log(cv.spender$lambda.1se)

## plot CV results and the various IC
ll <- log(spender$lambda) ## the sequence of lambdas
par(mfrow=c(1,2))
plot(cv.spender)
plot(ll, AIC(spender)/n, 
	xlab="log lambda", ylab="IC/n", pch=21, bg="orange")
abline(v=ll[which.min(AIC(spender))], col="orange", lty=3)
abline(v=ll[which.min(BIC(spender))], col="green", lty=3)
abline(v=ll[which.min(AICc(spender))], col="black", lty=3)
points(ll, BIC(spender)/n, pch=21, bg="green")
points(ll, AICc(spender)/n, pch=21, bg="black")
legend("topleft", bty="n",
	fill=c("black","orange","green"),legend=c("AICc","AIC","BIC"))


## all metrics, together in a path plot.
dev.off()
plot(spender, col="grey")
abline(v=ll[which.min(AICc(spender))], col="black", lty=2)
abline(v=ll[which.min(AIC(spender))], col="orange", lty=2)
abline(v=ll[which.min(BIC(spender))], col="green", lty=2)
abline(v=log(cv.spender$lambda.min), col="blue", lty=2)
abline(v=log(cv.spender$lambda.1se), col="purple", lty=2)
legend("topright", bty="n", lwd=1, 
	col=c("black","orange","green","blue","purple"),
	legend=c("AICc","AIC","BIC","CV.min","CV.1se"))
