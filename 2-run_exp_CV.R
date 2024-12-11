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
load("./data/pval_Permutation_1000.RData")
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

BB  = 25
for (bb in 1:BB){

	rr = seq(0,1,by=0.1)#runif(0.1*nrow(df.tr00))
	qq = quantile(df.tr00[,"C"],rr)
	ncv = as.vector(sapply(qq,function(x){sample(which(abs(x - df.tr00[,"C"])==min(abs(x - df.tr00[,"C"]))),5)}))

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
	mod0 <- ranger(sl ~ ., data = df.tr0,importance="none",num.trees = 1000,mtry=MTRY[OU0[1]],min.node.size = NODE[OU0[2]])

	## PREDICT
	Yte0 <- predict(mod0,df.te.BB)$predictions

	## PREDICT
	save(Yte0, Y.te.BB, file=paste0("./exp_cv/init/Init_",bb,".RData"))

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
	mod <- ranger(sl ~ ., data = df.tr.BB,importance="none",num.trees = 1000,mtry=MTRY[OU[1]],min.node.size = NODE[OU[2]])

	## PREDICT
	Yte <- predict(mod,df.te.BB)$predictions

	## PREDICT
	save(Yte, Y.te.BB, file=paste0("./exp_cv/Exp_Cas",CAS,"-Test",nom_cas[iter2],"_",bb,".RData"))

}##iter2

}##bb

}##CAS
