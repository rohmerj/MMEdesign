library(ggplot2)
library(cowplot)
library(RColorBrewer)

rm(list=ls())

#################################################################
##### GRAPHICS - FORMAT
#################################################################
tt <- theme(
		axis.title.x = element_text(size=14),
		axis.title.y = element_text(size=14),#, face="bold"
		axis.text.y= element_text(size=14),
		axis.text.x= element_text(size=14),
		axis.ticks = element_line(size = 0.75),
		legend.title=element_blank(),
		legend.position="none",
		plot.title = element_text(size = 14, face = "bold")
		)

#### COLORS
colo = brewer.pal(9,name="Spectral")
colo[5] = "gold2"

#########################################################
##### FUNCTION
#########################################################
CAfct <- function(y,lo,up){
	id <- NULL
	for (i in 1:length(y)){
		id[i] <- ifelse(y[i] >= lo[i] & y[i] <= up[i], 1, 0)
	}
	sum(id)/length(y)
}

crps_func <- function(Y.te.BB,Qte0){
	qq = seq(0,1,by=0.05)
	nmc = length(Y.te.BB)
	cov.pi = matrix(0,length(qq),nmc)
	for (kk in 1:(length(qq))){
		quant = Qte0[,kk]
		for (imc in 1:nmc){
		if (Y.te.BB[imc] < quant[imc]){
			cov.pi[kk,imc] = (-Y.te.BB[imc]+quant[imc])*(1-qq[kk])
		}
		if (quant[imc] <= Y.te.BB[imc]){
			cov.pi[kk,imc] = (Y.te.BB[imc]-quant[imc])*(qq[kk])
		}
		}
	}
	CRPS = mean(2*apply(cov.pi,2,sum)*0.05)
	return(CRPS)
}
source("./utils/utils.R")
#########################################################
##### DATA
#########################################################
load("./data/GrIS_MME_2100.RData")
df.tr = df.tr1
load("./data/pval_Permutation.RData")
## filter with the importance variables
filtre = which(pval[,"pvalue"]<0.05)
df.tr00 = df.tr1[,-c(1,2)]
df.tr00 = df.tr00[,c(filtre,ncol(df.tr00))]
d = ncol(df.tr00)

#######################################
#### FIGURE FOR GIVEN GSAT
#######################################
graphics.off()

## List of cases
CClist = c("RCP","RCM","KAPPA","ISM")

nom_case = type=  NULL 
Iter = NULL

YHAT = TRUTH = ITER = Q1HAT = Q3HAT= Q2HAT =  QHAT = GSAT = NOM_CASE = NOM_CAT = NULL

cc = 0
for (i in 1:length(CClist)){

	EXPR = CClist[i]
	ff = list.files("./exp_cv/", pattern=EXPR)

for (ii in 1:length(ff)){
	cc = cc + 1
	cas = strsplit(strsplit(ff[ii],split=".RData")[[1]],split="_")[[1]]
	if(length(cas) > 3){
		nom_case0 = paste(cas[2],cas[3])
		Iter[cc] = as.numeric(cas[4])
	}
	if(length(cas) <= 3){
		nom_case0 = cas[2]
		Iter[cc] = as.numeric(cas[3])
	}
	nom_case[cc] = strsplit(nom_case0,split="-Test")[[1]][2]
	type[cc] = strsplit(nom_case0,split="-Test")[[1]][1]

	load(paste0("./exp_cv/",ff[ii]))

	YHAT = c(YHAT,Yte)
	Q1HAT = c(Q1HAT,Qte[,2])
	Q3HAT = c(Q3HAT,Qte[,20])
	Q2HAT = c(Q2HAT,Qte[,11])
	QHAT = rbind(QHAT,Qte)
	TRUTH = c(TRUTH,Y.te.BB)
	load(file=paste0("./exp_cv/exp/Exp_Cas",EXPR,"_iter",Iter[cc],".RData"))
	GSAT = c(GSAT,df.tr00[ncv,"C"])
	ITER = c(ITER,rep(Iter[cc],length(df.tr00[ncv,"C"])))
	NOM_CASE = c(NOM_CASE,rep(nom_case[cc],200))
	NOM_CAT = c(NOM_CAT,rep(type[cc],200))
}

}##Clist

### REFERENCE SOLUTION
for (bb in 1:25){
	cc = cc + 1
	nom_case[cc] = "reference"
	type[cc] = "reference"
	load(paste0("./exp_cv/init/init_",bb,".RData"))

	YHAT = c(YHAT,Yte0)
	Q1HAT = c(Q1HAT,Qte0[,2])
	Q3HAT = c(Q3HAT,Qte0[,20])
	Q2HAT = c(Q2HAT,Qte0[,11])
	QHAT = rbind(QHAT,Qte0)
	TRUTH = c(TRUTH,Y.te.BB)
	load(file=paste0("./exp_cv/exp/Exp_Cas","RCP","_iter",bb,".RData"))
	GSAT = c(GSAT,df.tr00[ncv,"C"])
	ITER = c(ITER,rep(bb,length(df.tr00[ncv,"C"])))

	NOM_CASE = c(NOM_CASE,rep(nom_case[cc],200))
	NOM_CAT = c(NOM_CAT,rep(type[cc],200))

}

rr = seq(0,1,by=0.25)#runif(0.1*nrow(df.tr00))
qq = quantile(df.tr00[,"C"],rr)
gsatC= cut(as.vector(GSAT),breaks=qq)

fc = unique(NOM_CASE)
ca = rae = crps = qq = cas = gsat = NULL
c0 = 0
for (k in fc){
c = 0
c0 = c0+1
for (i in unique(gsatC)){
	c = c + 1
	for (j in 1:25){	
		f0 = which(gsatC == i & ITER == j & NOM_CASE == "reference")
			rae0 = mean(abs(TRUTH[f0] - YHAT[f0])/TRUTH[f0])
			qq0 = Q2(TRUTH[f0],YHAT[f0])
			crps0 = crps_func(TRUTH[f0],QHAT[f0,])

		f = which(gsatC == i & ITER == j & NOM_CASE == k)
			rae = c(rae,(rae0-mean(abs(TRUTH[f] - YHAT[f])/TRUTH[f]))/rae0)
			qq = c(qq,(qq0-Q2(TRUTH[f],YHAT[f]))/qq0)
			crps = c(crps,(crps0-crps_func(TRUTH[f],QHAT[f,])/crps0))

			cas = c(cas,k)
			gsat = c(gsat,i)	
	}
}
}

df.plt= data.frame(
		Q2=qq,RAE=rae,CRPS=crps,
		cas,GSAT=gsat)

df.plt$cas = plyr::revalue(df.plt$cas,
			c(
				"SSP126 245" = "woSSP585",
				"SSP126 585" = "woSSP245",
				"SSP245 585" = "woSS126",
				"Extrem" = " Med. & extr. Kappa",
				"Narrow" = " Narrow Kappa"
			)
			)

f = which(df.plt$cas=="reference")
df.plt = df.plt[-f,]
df.plt$cas = factor(df.plt$cas,levels=unique(df.plt$cas)[c(1:3,8:9,6:7,4:5)])

## configruaiton of limits for plotting
L.qq=250
L.crps=-10
L.rae=-650

L.qq = 1000
L.crps=-20
L.rae=-1000

f = which(df.plt$GSAT==unique(gsatC)[2])
plt.qq1= ggplot(df.plt[f,],aes(y=cas,x=Q2*100,color=cas))+geom_boxplot(linewidth=1.25)+#geom_jitter() + 
	scale_colour_manual(values=(colo))+
	xlim(0,L.qq)+
	theme_bw()+theme(
	axis.text.x=element_blank(), 
      axis.ticks.x=element_blank())+ylab("")+xlab("Q² rel. diff. [%]")+tt+ggtitle("(b)")
plt.rae1= ggplot(df.plt[f,],aes(y=cas,x=RAE*100,color=cas))+geom_boxplot(linewidth=1.25)+#geom_jitter() + 
	scale_colour_manual(values=(colo))+
	xlim(L.rae,0)+
	theme_bw()+theme(
	axis.text.x=element_blank(), 
      axis.ticks.x=element_blank())+ylab("")+xlab("RAE rel. diff. [%]")+tt+ggtitle("(a)")
plt.crps1= ggplot(df.plt[f,],aes(y=cas,x=CRPS,color=cas))+geom_boxplot(linewidth=1.25)+#geom_jitter() + 
	scale_colour_manual(values=(colo))+
	xlim(L.crps,0)+
	theme_bw()+theme(
	axis.text.x=element_blank(), 
      axis.ticks.x=element_blank())+ylab("")+xlab("CRPS rel. diff. [%]")+tt+ggtitle("(c)")

f = which(df.plt$GSAT==unique(gsatC)[3])
plt.qq= ggplot(df.plt[f,],aes(y=cas,x=Q2*100,color=cas))+geom_boxplot(linewidth=1.25)+#geom_jitter() + 
	scale_colour_manual(values=(colo))+
	xlim(0,L.qq)+
	theme_bw()+theme(
	axis.text.x=element_blank(), 
      axis.ticks.x=element_blank())+ylab("")+xlab("Q² rel. diff. [%]")+tt+ggtitle("(e)")
plt.rae= ggplot(df.plt[f,],aes(y=cas,x=RAE*100,color=cas))+geom_boxplot(linewidth=1.25)+#geom_jitter() + 
	scale_colour_manual(values=(colo))+
	xlim(L.rae,0)+
	theme_bw()+theme(
	axis.text.x=element_blank(), 
      axis.ticks.x=element_blank())+ylab("")+xlab("RAE rel. diff. [%]")+tt+ggtitle("(d)")
plt.crps= ggplot(df.plt[f,],aes(y=cas,x=CRPS,color=cas))+geom_boxplot(linewidth=1.25)+#geom_jitter() + 
	scale_colour_manual(values=(colo))+
	xlim(L.crps,0)+
	theme_bw()+theme(
	axis.text.x=element_blank(), 
      axis.ticks.x=element_blank())+ylab("")+xlab("CRPS rel. diff. [%]")+tt+ggtitle("(f)")

gridExtra::grid.arrange(
	plt.rae1,plt.qq1,plt.crps1,
	plt.rae,plt.qq,plt.crps,
	ncol=3)

#######################################
#### FIGURE ALL GSAT
#######################################
f = which(df.plt$GSAT==unique(gsatC))
plt.qq= ggplot(df.plt[f,],aes(y=cas,x=Q2*100,color=cas))+geom_boxplot(linewidth=1.25)+#geom_jitter() + 
	scale_colour_manual(values=(colo))+
	theme_bw()+theme(
	axis.text.x=element_blank(), 
      axis.ticks.x=element_blank())+ylab("")+xlab("Q² rel. diff. [%]")+tt+ggtitle("(b)")+xlim(0,1000)#+facet_wrap(~GSAT)
plt.rae= ggplot(df.plt[f,],aes(y=cas,x=RAE*100,color=cas))+geom_boxplot(linewidth=1.25)+#geom_jitter() + 
	scale_colour_manual(values=(colo))+
	theme_bw()+theme(
	axis.text.x=element_blank(), 
      axis.ticks.x=element_blank())+ylab("")+xlab("RAE rel. diff. [%]")+tt+ggtitle("(a)")+xlim(-1000,0)#+facet_wrap(~GSAT)
plt.crps= ggplot(df.plt[f,],aes(y=cas,x=CRPS,color=cas))+geom_boxplot(linewidth=1.25)+#geom_jitter() + 
	scale_colour_manual(values=(colo))+
	theme_bw()+theme(
	axis.text.x=element_blank(), 
      axis.ticks.x=element_blank())+ylab("")+xlab("CRPS rel. diff. [%]")+tt+ggtitle("(c)")+xlim(-15,0)#+facet_wrap(~GSAT)
gridExtra::grid.arrange(plt.rae,plt.qq,plt.crps,ncol=3)