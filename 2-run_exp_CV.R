library(ranger)
library(ggplot2)

rm(list=ls())

#### functions
source("./utils/utils.R")
source("./utils/run_tune.R")

###########################################################
#### DATA
###########################################################
load("./data/GrIS_MME_2100.RData")
df.tr = df.tr1
#########################################################
##### LOAD RESULT of SENSITIVITY ANALYSIS
#########################################################
load("./data/pval_Permutation.RData")
## filter with the importance variables
filtre = which(pval[,"pvalue"]<0.05)

#########################################################
##### DATA FORMAT
#########################################################
df.tr00 = df.tr1[,-c(1,2)]
df.tr00 = df.tr00[,c(filtre,ncol(df.tr00))]
d = ncol(df.tr00)

#########################################################
##### PARAMETERS FOR RF TUNING
#########################################################
NODE = c(1,5,10)
NUM = 1
MTRY = seq(1,length(filtre),by=1)

#########################################################
##### DEFINE VALIDATION TEST
#########################################################
set.seed(12345)

CClist = c("RCP","RCM","KAPPA","ISM")

BB  = 25
for (bb in 1:BB){

	rr = seq(0,1,by=0.25)#runif(0.1*nrow(df.tr00))
	qq = quantile(df.tr00[,"C"],rr)
	
	ncv = NULL
	for (k in 1:(length(qq)-1)){
		f = which(df.tr00[,"C"] > qq[k] & df.tr00[,"C"] <= qq[k+1])
		ncv = c(ncv,sample(f,50,replace=FALSE))
	}
	
	df.tr0 = df.tr00[-ncv,]
	df.tr = df.tr1[-ncv,]
	df.te.BB = Te = df.tr00[ncv,]
	Y.te.BB = df.te.BB[,"sl"]

	for (CAS in CClist){
		source("./utils/CAS.R")##extract corresponding experiment
		save(ncv,ss0,nom_cas,Niter2, file=paste0("./exp_cv/exp/Exp_Cas",CAS,"_iter",bb,".RData"))
	}
}

#########################################################
##### VALIDATION FOR REFERENCE SOLUTION
#########################################################
for (bb in 1:BB){

	print(bb)

	load(file=paste0("./exp_cv/exp/Exp_Cas",CAS,"_iter",bb,".RData"))

	df.tr0 = df.tr00[-ncv,]
	df.tr = df.tr1[-ncv,]
	df.te.BB = Te = df.tr00[ncv,]
	Y.te.BB = df.te.BB[,"sl"]

	TT0 = tune(df.tr0,MTRY,NODE,NUM)
	OU0 = maxArray(TT0)$CC
	mod0 <- ranger(sl ~ ., data = df.tr0,
				importance="none",num.trees = 1000,mtry=MTRY[OU0[1]],min.node.size = NODE[OU0[2]],
				respect.unordered.factors = 'order',
				quantreg = TRUE
				)

	## PREDICT
	Yte0 <- predict(mod0,df.te.BB)$predictions
	Qte0 <- predict(mod0,df.te.BB,type = "quantiles", quantiles = seq(0,1,by=0.05))$predictions

	## PREDICT
	save(Yte0, Qte0,Y.te.BB, file=paste0("./exp_cv/init/Init_",bb,".RData"))

}

#########################################################
##### LOOP ON THE EXPERIMENTS
#########################################################
for (CAS in CClist){

for (bb in 1:BB){

	load(file=paste0("./exp_cv/exp/Exp_Cas",CAS,"_iter",bb,".RData"))

	df.tr0 = df.tr00[-ncv,]
	df.tr = df.tr1[-ncv,]
	df.te.BB = Te = df.tr00[ncv,]
	Y.te.BB = df.te.BB[,"sl"]

#### TRAIN
for (iter2 in 1:Niter2){

	## SELECT
	df.tr.BB = Tr = df.tr0[-ss0[[iter2]],]

	## TRAIN
	TT = tune(df.tr.BB,MTRY,NODE,NUM)
	OU = maxArray(TT)$CC
	mod <- ranger(sl ~ ., data = df.tr.BB,importance="none",num.trees = 1000,
			mtry=MTRY[OU[1]],min.node.size = NODE[OU[2]],
			respect.unordered.factors = 'order',
				quantreg = TRUE
			)

	## PREDICT
	Yte <- predict(mod,df.te.BB)$predictions
	Qte <- predict(mod,df.te.BB,,type = "quantiles", quantiles = seq(0,1,by=0.05))$predictions

	## PREDICT
	save(Yte, Qte,Y.te.BB, file=paste0("./exp_cv/Exp_Cas",CAS,"-Test",nom_cas[iter2],"_",bb,".RData"))

}##iter2

}##bb

}##CAS


