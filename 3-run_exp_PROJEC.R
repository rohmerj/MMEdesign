library(ggplot2)
library(ranger)

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
##### DATA FOR SAMPLING
#########################################################
df.tr0 = df.tr[,-c(1,2)]
df.tr0 = df.tr0[,c(filtre,ncol(df.tr0))]

NODE = c(1,5,10)
NUM = 1
MTRY = seq(1,length(filtre),by=1)

nMC = 10000
kappa = read.table("./data/kappa.txt")
f.dens = density(kappa[,1])
cdf.estimate = cumsum(f.dens$y) / cumsum(f.dens$y)[length(f.dens$y)] 

### SAMPLING FOR REFERENCE SOLUTION
ISM.r0 = sample(unique(df.tr0[,"x.model"]),nMC,replace=T)
RCM.r0 = sample(unique(df.tr0[,"x.RCM"]),nMC,replace=T)
RESOL.r0 = sample(unique(df.tr0[,"x.resolution"]),nMC,replace=T)
RCM_init.r0 = sample(unique(df.tr0[,"x.RCM_init"]),nMC,replace=T)
elev_feedback.r0 = sample(unique(df.tr0[,"x.elev_feedback"]),nMC,replace=T)
kappa.r0 = replicate(nMC, f.dens$x[findInterval(runif(1), cdf.estimate)+1])
#init_yrs.r0 = sample(unique(df.tr0[,"x.init_yrs"]),nMC,replace=T)

#########################################################
##### DEFINE CASE
#########################################################
set.seed(12345)

SAUV = TRUE

CClist = c("RCP","RCM","KAPPA","ISM")

#########################################################
##### RF REFERENCE SOLUTION
#########################################################
TT0 = tune(df.tr0,MTRY,NODE,NUM)
OU0 = maxArray(TT0)$CC
mod0 = ranger(sl ~ ., data = df.tr0,importance="none",num.trees = 1000,
		  mtry=MTRY[OU0[1]],min.node.size = NODE[OU0[2]],
				respect.unordered.factors = 'order',
				quantreg = TRUE
		)

#########################################################
##### PROBABILISTIC PROJECTION
#########################################################
## loop on experiment
for (CAS in CClist){

source("./utils/CAS.R")##extract corresponding experiment

## loop on case
for (iter2 in 1:Niter2){

	## SELECT
	part = df.tr0[ss0[[iter2]],]
	df.te.BB = Te = as.data.frame(part[,-which(names(df.tr0) == "sl")])
	TeS = as.data.frame(part)
	Y.te.BB = as.vector(part[,"sl"])
	df.tr.BB = Tr = df.tr0[-ss0[[iter2]],]

	## TRAIN
	TT = tune(df.tr.BB,MTRY,NODE,NUM)
	OU = maxArray(TT)$CC
	mod <- ranger(sl ~ ., data = df.tr.BB,importance="none",num.trees = 1000,
			mtry=MTRY[OU[1]],min.node.size = NODE[OU[2]],
			respect.unordered.factors = 'order',
				quantreg = TRUE
			)

	## SAMPLE
	ISM.r = sample(unique(df.tr.BB[,"x.model"]),nMC,replace=T)
	RCM.r = sample(unique(df.tr.BB[,"x.RCM"]),nMC,replace=T)
	RESOL.r = sample(unique(df.tr.BB[,"x.resolution"]),nMC,replace=T)
	RCM_init.r = sample(unique(df.tr.BB[,"x.RCM_init"]),nMC,replace=T)
	elev_feedback.r = sample(unique(df.tr.BB[,"x.elev_feedback"]),nMC,replace=T)
	#init_yrs.r = sample(unique(df.tr.BB[,"x.init_yrs"]),nMC,replace=T)
	kappa.r = replicate(nMC, f.dens$x[findInterval(runif(1), cdf.estimate)+1])

	### GSAT scenario
	for (TEMP in c(2,4)){

	#C.r = rep(TEMP,nMC)
	C.r = runif(nMC)*0.5+TEMP-0.25
	df.te.BB = data.frame(
		x.model = ISM.r,
		x.RCM = RCM.r,
		x.retreat = kappa.r,
		x.resolution = RESOL.r,
		x.RCM_init = RCM_init.r,
		#x.init_yrs = init_yrs.r,
		x.elev_feedback = elev_feedback.r,
		C = C.r
	)
	
	df.te0 = data.frame(
		x.model = ISM.r0,
		x.RCM = RCM.r0,
		x.retreat = kappa.r0,
		x.resolution = RESOL.r0,
		x.RCM_init = RCM_init.r0,
		#x.init_yrs = init_yrs.r0,
		x.elev_feedback = elev_feedback.r0,
		C = C.r
	)
	

	## PREDICT
	Yte <- predict(mod,df.te.BB)$predictions

	## PREDICT REFERENCE SOLUTION
	Yte0 <- predict(mod0,df.te0)$predictions

	## PREDICT
	Qte <- predict(mod,df.te.BB,type = "quantiles", quantiles = seq(0,1,by=0.05))$predictions

	## PREDICT REFERENCE SOLUTION
	Qte0 <- predict(mod0,df.te0,type = "quantiles", quantiles = seq(0,1,by=0.05))$predictions
	
	save(Yte,Yte0,Qte,Qte0,df.te.BB,df.te0, file=paste0("./exp_pred/Exp_Cas",CAS,"-Test",nom_cas[iter2],"_GSAT",TEMP,".RData"))
	
	}

}##iter2

}##CAS

