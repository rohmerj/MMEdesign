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
		legend.position="top",
		legend.text=element_text(size=14)
		)

## color
colo=RColorBrewer::brewer.pal(9,name="Spectral")
colo[5] = "gold2"

#########################################################
##### PLOT (Ds,Dh)
#########################################################
set.seed(12345)
CClist = c("RCP","RCM","KAPPA","ISM")
EXPERIMENT = c("woSSP245","woSSP585","woSSP126","MAR","woMAR","Med. & extr. Kappa","Narrow Kappa","woCISM","CISM")

C1 = C2 = C2M = NULL
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

		C2[c] = mean(DIFF_HISTO_KS(df.tr1,df.tr2))*100#/nrow(df.tr1)*100
		C2M[c] = mean(DIFF_HISTO_CRPS(df.tr1,df.tr2))*100#/nrow(df.tr1)*100
		C1[c] = (nrow(df.tr1) - nrow(df.tr2))/nrow(df.tr1)*100
  
		NOM[c] = EXPERIMENT[c]

	}
}
'
df.plt = data.frame(C2=C2M,C1,NOM)
#df.plt$NOM = factor(df.plt$NOM,levels=c("woSSP585","woSSP245","woSSP126","MAR","woMAR","Narrow Kappa","Med. & extr. Kappa","CISM","woCISM")) 
x11()
ggplot(df.plt,aes(C2,C1,color=NOM))+geom_point(size=4,shape=15)+
	ylab("Decrease of the MME size Ds [%]")+xlab("Deviation from the orginal histograms Dh [%]")#+
	#scale_colour_manual(values=colo)+theme_bw()+tt1
'
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

	C20[bb] = mean(DIFF_HISTO_KS(df.tr1,df.tr2))*100#/nrow(df.tr1)*100
	C10[bb] = (nrow(df.tr1) - nrow(df.tr2))/nrow(df.tr1)*100
  
	NOM0[bb] = bb

}

df.plt = data.frame(C2,C1,NOM)
df.plt0 = data.frame(C2=mean(C20),C1=mean(C10),NOM = "reference")
df.pltA = rbind(df.plt,df.plt0) 
df.pltA = df.pltA[c(2,1,3,9,8,6,7,4,5,10),]
#df.pltA$NOM = factor(df.pltA$NOM,levels=c("reference","woSSP585","woSSP245","woSSP126","CISM","woCISM","Med. & extr. Kappa","Narrow Kappa","MAR","woMAR")) 
df.pltA$NOM = factor(df.pltA$NOM,c("reference","woSSP585","woSSP245","woSSP126","CISM","woCISM","Med. & extr. Kappa","Narrow Kappa","MAR","woMAR"))

#### COLORS
colo = brewer.pal(9,name="Spectral")
colo[5] = "gold2"
colo=c("blue3",colo)

ggplot(df.pltA,aes(C2,C1,color=NOM))+geom_point(size=6,shape=15)+xlim(0,50)+ylim(0,100)+
	annotate("text", x = df.pltA$C2[9], y = df.pltA$C1[9]*0.975, label = as.character(df.pltA$NOM[9]))+
	annotate("text", x = df.pltA$C2[1], y = df.pltA$C1[1]*0.975, label = as.character(df.pltA$NOM[1]))+
	annotate("text", x = df.pltA$C2[5], y = df.pltA$C1[5]*0.975, label = as.character(df.pltA$NOM[5]))+
	annotate("text", x = df.pltA$C2[6], y = df.pltA$C1[6]*0.975, label = as.character(df.pltA$NOM[6]))+
	annotate("text", x = df.pltA$C2[4], y = df.pltA$C1[4]*0.975, label = as.character(df.pltA$NOM[4]))+
	annotate("text", x = df.pltA$C2[7]*1.5, y = df.pltA$C1[7], label = as.character(df.pltA$NOM[7]))+
	annotate("text", x = df.pltA$C2[8], y = df.pltA$C1[8]*0.975, label = as.character(df.pltA$NOM[8]))+
	annotate("text", x = df.pltA$C2[3], y = df.pltA$C1[3]*1.05, label = as.character(df.pltA$NOM[3]))+
	annotate("text", x = df.pltA$C2[2]*1.5, y = df.pltA$C1[2]*1.05, label = as.character(df.pltA$NOM[2]))+
	ylab("Decrease of the MME size Ds [%]")+xlab("Deviation from the orginal histograms Dh [%]")+
	scale_colour_manual(values=colo)+theme_bw()+tt1

