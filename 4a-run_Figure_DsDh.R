library(ggplot2)

rm(list=ls())

source("./utils/PLOT_HISTO.R")

#########################################################
##### DATA
#########################################################
load("./data/GrIS_MME_2100.RData")
df.tr = df.tr1

#########################################################
##### GRAPHIC FORMAT
#########################################################
tt <- theme(
		axis.title.x = element_text(size=14),
		axis.title.y = element_text(size=14),#, face="bold"
		axis.text.y= element_text(size=14),
		axis.text.x= element_text(size=14),
		axis.ticks = element_line(size = 0.75),
		legend.title=element_blank(),
		legend.position="none"
		)

tt1 <- theme(
		axis.title.x = element_text(size=14),
		axis.title.y = element_text(size=14),#, face="bold"
		axis.text.y= element_text(size=14),
		axis.text.x= element_text(size=14),
		axis.ticks = element_line(size = 0.75),
		legend.title=element_blank(),
		legend.position="right"
		)

## color
colo=RColorBrewer::brewer.pal(9,name="Spectral")
colo[5] = "gold2"

#########################################################
##### PLOT (Ds,Dh)
#########################################################
set.seed(12345)
CClist = c("RCP","RCM","KAPPA","ISM")
EXPERIMENT = c("woSSP245","woSSP585","woSSP126","MAR","Med. & extr. Kappa","Narrow Kappa","woCISM","CISM")

C1 = C2 = NULL
NOM = NULL
plt = list()
c = 0
for(CAS in CClist){

	source("./utils/CAS.R")

	for (ii in 1:length(ss0)){
	
		c = c + 1

		df.tr2 = df.tr1[-ss0[[ii]],]

		df.tr = rbind(df.tr1,df.tr2)
		df.tr$design = c(
			rep("orginal",nrow(df.tr1)),
			rep("experiment",nrow(df.tr2))
		)
		df.tr$design = as.factor(df.tr$design)

		plt[[c]] = PLOT_HISTO(df.tr, DEUX = TRUE)

		C2[c] = mean(DIFF_HISTO(df.tr1,df.tr2))/nrow(df.tr1)*100
		C1[c] = (nrow(df.tr1) - nrow(df.tr2))/nrow(df.tr1)*100
  
		NOM[c] = EXPERIMENT[c]

	}
}

df.plt = data.frame(C2,C1,NOM)
df.plt$NOM = factor(df.plt$NOM,levels=c("woSSP585","woSSP245","woSSP126","MAR","Narrow Kappa","Med. & extr. Kappa","CISM","woCISM")) 
ggplot(df.plt,aes(C2,C1,color=NOM))+geom_point(size=4,shape=15)+
	ylab("Decrease of the MME size Ds [%]")+xlab("Deviation from the orginal histograms Dh [%]")+
	scale_colour_manual(values=colo)+theme_bw()+tt1

#########################################################
##### PLOT (Ds,Dh) with CROSS-VALIDATION w. ORIGINAL MME
#########################################################
set.seed(12345)

C10 = C20 = NULL
NOM0 = NULL
plt0 = list()
for (bb in 1:25){

	load(file=paste0("./exp_cv/exp/Exp_Cas","RCP","_iter",bb,".RData"))
	df.tr2 = df.tr1[-ncv,]

	df.tr = rbind(df.tr1,df.tr2)
	df.tr$design = c(
		rep("orginal",nrow(df.tr1)),
		rep("experiment",nrow(df.tr2))
	)
	df.tr$design = as.factor(df.tr$design)
	plt0[[bb]] = PLOT_HISTO(df.tr, DEUX = TRUE)

	C20[bb] = mean(DIFF_HISTO(df.tr1,df.tr2))/nrow(df.tr1)*100
	C10[bb] = (nrow(df.tr1) - nrow(df.tr2))/nrow(df.tr1)*100
  
	NOM0[bb] = bb

}

df.plt = data.frame(C2,C1,NOM)
df.plt0 = data.frame(C2=mean(C20),C1=mean(C10),NOM = "reference")
df.pltA = rbind(df.plt,df.plt0) 
df.pltA$NOM = factor(df.pltA$NOM,levels=c("reference","woSSP585","woSSP245","woSSP126","MAR","Narrow Kappa","Med. & extr. Kappa","CISM","woCISM")) 

ggplot(df.pltA,aes(C2,C1,color=NOM))+geom_point(size=4,shape=15)+xlim(0,40)+ylim(0,70)+
	ylab("Decrease of the MME size Ds [%]")+xlab("Deviation from the orginal histograms Dh [%]")+
	scale_colour_manual(values=c("blue3",colo))+theme_bw()+tt1

