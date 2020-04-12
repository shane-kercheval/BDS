#### ******* German Credit Data ******* ####

## read data and create some `interesting' variables
credit <- read.csv("credit.csv")

## re-level the credit history and checking account status
credit$history = factor(credit$history, levels=c("A30","A31","A32","A33","A34"))
levels(credit$history) = c("good","good","poor","poor","terrible")
## a few others
credit$foreign <- factor(credit$foreign, levels=c("A201","A202"), labels=c("foreign","german"))
credit$rent <- factor(credit$housing=="A151")
credit$purpose <- factor(credit$purpose, levels=c("A40","A41","A42","A43","A44","A45","A46","A47","A48","A49","A410"))
levels(credit$purpose) <- c("newcar","usedcar",rep("goods/repair",4),"edu",NA,"edu","biz","biz")

credit <- credit[,c("Default", "duration", "amount",
                    "installment", "age", "history",
                    "purpose", "foreign", "rent")]

## plot a mosaic
par(mai=c(.8,.8,.1,.1))
plot(factor(Default) ~ history, data=credit, col=c(8,2), ylab="Default") ## surprise!

credit %>% count(history, Default) %>% group_by(history) %>% mutate(perc_Default = n / sum(n)) %>% ungroup() %>% filter(Default == 1)
## the dangers of choice-based sampling!  

## build a design matrix 
library(gamlr)
source("naref.R")
# same data, but naref adds NA as the first factor level so sparse.model.matrix creates a dummy variable for all categories
head(credit)
head(naref(credit))

levels(credit$purpose)
levels(naref(credit$purpose))

# what is ^2 for
# it looks like it is what specifies the interaction between every cateogry/columm (pairwise interactions)
# https://stackoverflow.com/questions/22649536/model-matrix-with-all-pairwise-interactions-between-columns
credx <- sparse.model.matrix( Default ~ ., data=naref(credit))[,-1]
dim(credx)
# the -1 drops the intercept column
# the results is that we have a "one-hot encoded" matrix without the intercept
credx <- sparse.model.matrix( Default ~ .^2, data=naref(credit))[,-1]
dim(credx)

default <- credit$Default
credscore <- cv.gamlr(credx, default, family="binomial", verb=TRUE)

par(mfrow=c(1,2))
plot(credscore$gamlr)
plot(credscore)

sum(coef(credscore, s="min") != 0) # min
sum(coef(credscore$gamlr) != 0) # AICc

sum(coef(credscore) != 0) # 1se
sum(coef(credscore$gamlr, s=which.min(AIC(credscore$gamlr)))!=0) # AIC
sum(coef(credscore$gamlr, s=which.min(BIC(credscore$gamlr)))!=0) # BIC


# the OOS R^2
1 - credscore$cvm[credscore$seg.min]/credscore$cvm[1]

## What are the underlying default probabilities
## In sample probability estimates
pred <- predict(credscore$gamlr, credx, type="response")
pred <- drop(pred) # remove the sparse Matrix formatting
boxplot(pred ~ default, xlab="default", ylab="prob of default", col=c("pink","dodgerblue"))

## what are our misclassification rates?
rule <- 1/5 # move this around to see how these change

sum((pred > rule)[default==0]) / sum(pred > rule) ## false positive rate
sum((pred <= rule)[default==1]) / sum(pred <= rule) ## false negative rate



sum((pred > rule)[default==1]) / sum(default==1) ## sensitivity
# or
mean((pred > rule)[default==1])

sum((pred <= rule)[default==0]) / sum(default==0) ## specificity
# or
mean((pred <= rule)[default==0])

# OOS ROC curve
# refit the model using only 1/2 of data
test <- sample.int(1000,500)
credhalf <- gamlr(credx[-test,], default[-test], family="binomial")
predoos <- predict(credhalf, credx[test,], type="response")
defaultoos <- default[test]

summary(credhalf)
dev.off()
plot(credhalf)

interations <- as.data.frame(summary(credhalf))
interations[which(interations$aicc == min(interations$aicc)), ]
min_aicc_lambda <- interations[which(interations$aicc == min(interations$aicc)), ]$lambda
log(min_aicc_lambda)


credhalf_cv <- cv.gamlr(credx[-test,], default[-test], family="binomial")
interations <- as.data.frame(summary(credhalf_cv))
interations[which(interations$oos.r2 == min(interations$oos.r2)), ]
min_aicc_lambda_cv <- interations[which(interations$oos.r2 == min(interations$oos.r2)), ]$lambda
log(min_aicc_lambda_cv)
log(min_aicc_lambda)


## roc curve and fitted distributions
source("roc.R")

par(mai=c(.9,.9,.2,.1), mfrow=c(1,2))
roc(p=pred, y=default, bty="n", main="in-sample")
## our 1/5 rule cutoff
points(x= 1-mean((pred<.2)[default==0]), 
	y=mean((pred>.2)[default==1]), 
	cex=1.5, pch=20, col='red') 
## a standard `max prob' (p=.5) rule
points(x= 1-mean((pred<.5)[default==0]), 
	y=mean((pred>.5)[default==1]), 
	cex=1.5, pch=20, col='blue') 
legend("bottomright",fill=c("red","blue"),
	legend=c("p=1/5","p=1/2"),bty="n",title="cutoff")

roc(p=predoos, y=defaultoos, bty="n", main="out-of-sample")
## our 1/5 rule cutoff
points(x= 1-mean((predoos<.2)[defaultoos==0]), 
	y=mean((predoos>.2)[defaultoos==1]), 
	cex=1.5, pch=20, col='red') 
## a standard `max prob' (p=.5) rule
points(x= 1-mean((predoos<.5)[defaultoos==0]), 
	y=mean((predoos>.5)[defaultoos==1]), 
	cex=1.5, pch=20, col='blue') 







