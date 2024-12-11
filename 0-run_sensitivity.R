library(ranger)
library('sensitivity')
library(corrplot)
rm(list=ls())

#### functions
source("./utils/utils.R")
source("./utils/run_tune.R")

#########################################################
##### DATA
#########################################################
load("./data/GrIS_MME_2100.RData")

#########################################################
##### RF TRAINING
#########################################################
df.tr = na.omit(df.tr1[,-c(1,2)])

NODE = c(1,5,10)
NUM = 1
MTRY = seq(1,ncol(df.tr)-1,by=1)
TT = tune(df.tr,MTRY,NODE,NUM)
OU = maxArray(TT)$CC
mod = ranger(sl ~ ., data = df.tr,num.trees=1000,importance="permutation",mtry=MTRY[OU[1]],min.node.size = NODE[OU[2]])

#########################################################
##### SENSITIVITY
#########################################################
pval = importance_pvalues(mod, method = "altmann",formula = sl ~ ., data = df.tr, num.permutations = 1000)

#########################################################
##### SAVE
#########################################################
#save(pval,file="./data/pval_Permutation_1000.RData")

#########################################################
##### PLOT
#########################################################
par(mar=c(4,7,.5,.5)+0.5)
NOM = c("ISM","RCM",expression(kappa),
	  "resol.","sliding law","thermodyn.",
	  "RCM init.","init.","init yrs","elev feedback","GSAT diff."
	)

load("./data/pval_Permutation_1000.RData")
barplot(t(pval),names.arg=NOM,las=2,horiz=T,angle=90,
	cex.axis=1.25,cex.lab=1.5,xlab="p-value")
abline(v=0.05,col=2,lwd=4)
