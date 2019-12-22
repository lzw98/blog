
library(VGAM)
library(nnet)
#library(VGAMdata)

lake<-c(rep(1,4),rep(2,4),rep(3,4),rep(4,4))
gender<-c(1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0)

size<-c(1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0)
y1<-c(7,4,16,3,2,13,0,3,3,8,2,0,13,9,3,8)
y2<-c(1,0,3,0,2,7,1,9,7,6,4,1,10,0,9,1)
y3<-c(0,0,2,1,0,6,0,1,1,6,1,0,0,0,1,0)
y4<-c(0,1,2,2,0,0,1,0,0,3,1,0,2,1,0,0)
y5<-c(5,2,3,3,1,0,0,2,1,5,4,0,2,2,1,1)

alligators<-cbind(lake,gender,size,y1,y2,y3,y4,y5)# has been grouped 
alligators

allig<-as.data.frame(alligators)

# y1 is baseline
fit.v<-vglm(formula = cbind(y2,y3,y4,y5,y1) ~ size + factor(lake),
            family=multinomial, data=allig)
summary(fit.v) # four j and i

#y5 is baseline
fit.v1<-vglm(formula = cbind(y1,y2,y3,y4,y5) ~ size + factor(lake),
             family=multinomial, data=allig)
summary(fit.v1)


#y1 is baseline
fitm <- multinom(cbind(y1,y2,y3,y4,y5) ~ size + factor(lake), data=allig)
summary(fitm)

#y2 is baseline
fitm.1 <- multinom(cbind(y2,y3,y4,y5,y1) ~ size + factor(lake), data=allig)
summary(fitm.1)

library(icda)





#### Ordinal  Cumulative logit model
# Mental Health Example

y1<-c(rep(1,12),rep(0,28))
y2<-c(rep(0,12),rep(1,12),rep(0,16))
y3<-c(rep(0,24),rep(1,7),rep(0,9))
y4<-c(rep(0,31),rep(1,9))
ses<-c(1,1,1,1,0,1,0,1,1,1,0,0,1,0,
       1,0,1,1,0,1,1,0,1,1,0,1,0,0,1,0,0,1,1,1,0,0,0,1,0,0)

life<-c(1,9,4,3,2,0,1,3,3,7,
        1,2,5,6,3,1,8,2,5,5,9,3,3,1,0,4,3,9,6,4,3,8,2,7,5,4,4,8,8,9)

mental<-cbind(ses,life,y1,y2,y3,y4)
mental

mental<-as.data.frame(mental)
mental

#proportional odds model

library(VGAM)
fit <- vglm(cbind(y1,y2,y3,y4) ~ ses + life,
            family=cumulative(parallel=TRUE), data=mental)# parallel = TRUE means that the beta is the same
summary(fit)


##for alligator data
fitm <- vglm(cbind(y1,y2,y3,y4,y5) ~ size + factor(lake), 
             family=cumulative(parallel=TRUE),data=allig)
summary(fitm)



# with interaction
fit.inter <- vglm(cbind(y1,y2,y3,y4) ~ ses + life+ ses*life,
                  family=cumulative(parallel=TRUE), data=mental)
summary(fit.inter)


## not proportional odds
fit2 <- vglm(cbind(y1,y2,y3,y4) ~ ses + life,
             family=cumulative, data=mental)
summary(fit2)

#which one is better? LR or D1-D2

pchisq(deviance(fit)-deviance(fit2),df=df.residual(fit)-df.residual(fit2),lower.tail=FALSE)



###Adjacent-categories logit model, proportiontal odds

fit.acat <- vglm(cbind(y1,y2,y3,y4) ~ ses + life,
                 family=acat(reverse=TRUE, parallel=TRUE), data=mental)
summary(fit.acat)

fitm2 <- vglm(cbind(y1,y2,y3,y4,y5) ~ size + factor(lake), 
              family=acat(reverse=TRUE, parallel=TRUE),data=allig)#parallel表示是不是proportional odds
summary(fitm2)


# not proportiontal odds

fit.acat2 <- vglm(cbind(y1,y2,y3,y4) ~ ses + life,
                  family=acat(reverse=TRUE), data=mental)
summary(fit.acat2)

fit.acat3 <- vglm(cbind(y1,y2,y3,y4) ~ ses + life,
                  family=acat(reverse=FALSE,parallel=TRUE), data=mental)
summary(fit.acat3)


###continuation-ratio logit model
fit.cratio <- vglm(cbind(y1,y2,y3,y4) ~ ses + life,
                   family=cratio(reverse=TRUE, parallel=TRUE), data=mental)
summary(fit.cratio)


fit2.cratio <- vglm(cbind(y1,y2,y3,y4) ~ ses + life,
                    family=cratio(reverse=FALSE, parallel=TRUE), data=mental)
summary(fit2.cratio)



####### another way to do

library(icda)

help(package=icda)

data(alligators1)

alligators1

library(VGAM)
alligators.fit1 <- vglm(Food ~ Length, family=multinomial, data=alligators1)
summary(alligators.fit1)


data(alligators2)   # grouped
alligators2

## Table 7.1 from "CDA"
ftable(xtabs(Count ~ Lake + Gender + Size + Food, alligators2),
       row.vars=c("Lake","Gender","Size"))
## Table 6.14 from "ICDA"
ftable(xtabs(Count ~ Lake + Size + Food, alligators2),
       row.vars=c("Lake","Size"))
## Problem 6.3 from "ICDA"


#library(reshape2)  #need to install this package first 
#gators2w <- dcast(alligators2, Lake + Size ~ Food, sum, value.var="Count")
#gators2w


#library(VGAM)
#gators2.fit1 <-
#   vglm(cbind(Fish,Invertebrate,Reptile,Bird,Other) ~ Size + Lake,
#        family=multinomial, data=gators2w)
#summary(gators2.fit1)
