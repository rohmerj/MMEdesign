library(ggplot2)
library(viridis)
library(cowplot)
library(RColorBrewer)

rm(list=ls())

#########################################################
##### FUNCTIONS
#########################################################
source("./utils/utils.R")

#########################################################
##### PLOT PARAMETERS
#########################################################
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

#########################################################
##### DATA
#########################################################
load("./data/GrIS_MME_2100.RData")
df.tr = df.tr1

#########################################################
##### DOE
#########################################################
### HISTOGRAMM
p.scenario<-ggplot(df.tr, aes(x = x.scenario))+
	  #coord_flip()+
	  xlab('Scenario')+
        geom_bar( position = position_dodge(width = 0.8))+theme_bw()+tt
	  #ggtitle("(a)")#+ylim(0,50)

p.GCM<-ggplot(df.tr, aes(x = x.GCM))+
	  xlab('GCM')+
        geom_bar( position = position_dodge(width = 0.8))+theme_bw()+tt

p.ISM<-ggplot(df.tr, aes(x = x.model))+
	  coord_flip()+
	  xlab('ISM')+
        geom_bar( position = position_dodge(width = 0.8))+theme_bw()+tt+ylim(0,1300)

p.rcm<-ggplot(df.tr, aes(x = x.RCM))+
	  coord_flip()+
	  xlab("RCM")+
        geom_bar( position = position_dodge(width = 0.8))+theme_bw()+tt+ylim(0,1300)

p.elev_feedback<-ggplot(df.tr, aes(x = x.elev_feedback))+
	  coord_flip()+
        xlab("elev_feedback")+
        geom_bar( position = position_dodge(width = 0.8))+theme_bw()+tt+ylim(0,1300)

p.rcm_init<-ggplot(df.tr, aes(x = x.RCM_init))+
	  coord_flip()+
	  xlab("RCM init")+
        geom_bar( position = position_dodge(width = 0.8))+theme_bw()+tt+ylim(0,1300)

p.sliding<-ggplot(df.tr, aes(x = x.sliding))+
	  coord_flip()+
	  xlab("Slding law")+
        geom_bar( position = position_dodge(width = 0.8))+theme_bw()+tt+ylim(0,1300)

p.thermodyn<-ggplot(df.tr, aes(x = x.thermodyn))+
	  coord_flip()+
	  xlab("Thermodynamics")+
        geom_bar( position = position_dodge(width = 0.8))+theme_bw()+tt+ylim(0,1300)

p.init<-ggplot(df.tr, aes(x = x.init))+
	  coord_flip()+
	  xlab("Initialisation")+
        geom_bar( position = position_dodge(width = 0.8))+theme_bw()+tt+ylim(0,1300)

p.kappa<-ggplot(df.tr, aes(x = x.retreat))+
	  coord_flip()+
	  xlab(expression(paste(kappa,' [ km (',m^3, s^-1, ')x',10^-0.4,"°",C^-1,"]")))+
        geom_bar( position = position_dodge(width = 0.8),width=.01)+theme_bw()+tt+ylim(0,400)

p.resolution<-ggplot(df.tr, aes(x = x.resolution))+
	  coord_flip()+
	  xlab("Resolution [km]")+
        geom_bar( position = position_dodge(width = 0.8),width=.25)+theme_bw()+tt+ylim(0,400)

p.gsat<-ggplot(df.tr, aes(x = C))+
	  coord_flip()+
        xlab("GSAT diff. [°C]")+
        geom_bar( position = position_dodge(width = 4),width=.05)+theme_bw()+tt+ylim(0,400)

p.init_yrs<-ggplot(df.tr, aes(x = x.init_yrs))+
	  coord_flip()+
	  xlab(paste0("Number of years","\n"," for initialisation"))+
        geom_bar()+theme_bw()+tt+ylim(0,1300)
# position = position_dodge(width = 0.8),width=.05
####### PLOT continuous variables
PLTcont <- cowplot::plot_grid(
	p.kappa,
	p.resolution,
	p.gsat,
	align = "hv", ncol = 3)

####### PLOT categorical variables
PLTcat <- cowplot::plot_grid(
	p.ISM,
	p.rcm,
	p.rcm_init,
	p.elev_feedback, align = "hv", ncol = 2)

####### PLOT negligible variables
PLTnegl <- cowplot::plot_grid(
	p.init,
	p.init_yrs,
	p.thermodyn,
	p.sliding,
	align = "hv", ncol = 2)

###########################################################
#### PLOT SLC 2100
###########################################################
TXT.med = paste0("Median = ",round(median(df.tr$sl)*100,1))
TXT.q95 = paste0("",round(quantile(df.tr$sl,0.83)*100,1))
TXT.q05 = paste0("[",round(quantile(df.tr$sl,0.17)*100,1)," ; ",round(quantile(df.tr$sl,0.95)*100,1), "]")
p.sle <- ggplot(df.tr, aes(x = sl*100))+
        xlab("Sea level contribution in 2100 [cm SLE]")+
	  #ylab("Count")+
	  geom_histogram(aes(y=..density..), alpha=0.5, position="identity")+
 	  geom_density(alpha=.2,linewidth=1.25) +
        #geom_bar( position = position_dodge(width = 0.8))+
	  theme_bw()+tt+ylim(0,.1)+
	  annotate("text", x = .3*100, y = 7.5/100, label = TXT.med,size = 8)+
	  annotate("text", x = .3*100, y = 6.8/100, label = TXT.q05,size = 8)
	  #ggtitle("GSAT diff. [°C]")#

###########################################################
#### PLOT Validation ORIGINAL
###########################################################
load("./data/pval_Permutation.RData")
## filter with the importance variables
filtre = which(pval[,"pvalue"]<0.05)
df.tr00 = df.tr1[,-c(1,2)]
df.tr00 = df.tr00[,c(filtre,ncol(df.tr00))]
d = ncol(df.tr00)

RAE0 = QQ0 = IS90 = IS50 = CRPS = CA = NULL
YHAT = TRUTH = ITER = Q1HAT = Q3HAT= Q2HAT =  QHAT = NULL
GSAT.te.BB = matrix(0,200,25)
for (bb in 1:25){

	CAS = "RCP"
	load(file=paste0("./exp_cv/exp/Exp_Cas",CAS,"_iter",bb,".RData"))
	GSAT.te.BB[,bb] = Te = df.tr00[ncv,"C"]

	load(paste0("./exp_cv/init/init_",bb,".RData"))

	RAE0[bb] = mean(abs(Yte0-Y.te.BB)/Y.te.BB)
	QQ0[bb] = 1-sum((Y.te.BB-Yte0)^2)/sum((Y.te.BB-mean(Y.te.BB))^2)

	YHAT = c(YHAT,Yte0)
	Q1HAT = c(Q1HAT,Qte0[,2])
	Q3HAT = c(Q3HAT,Qte0[,20])
	Q2HAT = c(Q2HAT,Qte0[,11])
	QHAT = rbind(QHAT,Qte0)
	TRUTH = c(TRUTH,Y.te.BB)
	ITER = c(ITER, rep(bb,length(Yte0)))

	## Approximated crps
	### https://search.r-project.org/CRAN/refmans/fabletools/html/distribution_accuracy_measures.html
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
	CRPS[bb] = mean(2*apply(cov.pi,2,sum)*0.05)


}

#### scatter plot predictions versus truth
rr = seq(0,1,by=0.25)#runif(0.1*nrow(df.tr00))
qq = quantile(df.tr00[,"C"],rr)
df = data.frame(
	sle_predicted = YHAT,
	Q1 = Q1HAT,
	Q3 = Q3HAT,	
	sle_true=TRUTH,
	iter = ITER,
	gsat = as.vector(GSAT.te.BB),
	gsatC= cut(as.vector(GSAT.te.BB),breaks=qq)
)

rae = crps = qq = matrix(0,4,25)
c = 0
for (i in unique(df$gsatC)){
	c = c + 1
	for (j in 1:25){	
	f = which(df$gsatC == i & ITER == j)
	rae[c,j] = mean(abs(df$sle_true[f] - df$sle_predicted[f])/df$sle_true[f])
	qq[c,j] = Q2(df$sle_true[f],df$sle_predicted[f])
	crps[c,j] = crps_func(df$sle_true[f],QHAT[f,])	
	}
}

gsatC.ll = c("(0.705,2.14]", "(2.14,3.34]", "(3.34,3.83]", "(3.83,5]")
df.plt <- data.frame(
		CRPS=c(crps[1,],crps[2,],crps[3,],crps[4,]),
		Q2=c(qq[1,],qq[2,],qq[3,],qq[4,]),
		RAE=c(rae[1,],rae[2,],rae[3,],rae[4,]),
		id = rep("",25*4),
		GSAT=c(rep(gsatC.ll[1],25),rep(gsatC.ll[2],25),rep(gsatC.ll[3],25),rep(gsatC.ll[4],25))
		)
plt.rae= ggplot(df.plt,aes(x=GSAT,y=RAE*100,color=GSAT))+geom_boxplot(linewidth=1.25)+#geom_jitter() + 
	scale_colour_viridis_d()+
	geom_hline(yintercept=median(df.plt$RAE*100),col="red",linetype=2,linewidth=2)+
	theme_bw()+theme(
	axis.text.x=element_blank(), 
      axis.ticks.x=element_blank())+xlab("")+ylab("RAE [%]")+tt+ggtitle("(a)")

plt.qq= ggplot(df.plt,aes(x=GSAT,y=Q2*100,color=GSAT))+geom_boxplot(linewidth=1.25)+#geom_jitter() + 
	scale_colour_viridis_d()+
	geom_hline(yintercept=median(df.plt$Q2*100),col="red",linetype=2,linewidth=2)+
	theme_bw()+theme(
	axis.text.x=element_blank(), 
      axis.ticks.x=element_blank())+xlab("")+ylab("Q² [%]")+tt+ggtitle("(b)")

plt.crps= ggplot(df.plt,aes(x=GSAT,y=CRPS,color=GSAT))+geom_boxplot(linewidth=1.25)+#geom_jitter() + 
	scale_colour_viridis_d()+
	geom_hline(yintercept=median(df.plt$CRPS),col="red",linetype=2,linewidth=2)+
	theme_bw()+theme(
	axis.text.x=element_blank(), 
      axis.ticks.x=element_blank())+xlab("")+ylab("CRPS [-]")+tt+ggtitle("(c)")

gridExtra::grid.arrange(plt.rae,plt.qq,plt.crps,ncol=3)

###########################################################
#### PLOT probabilistic projections NO META UNC
###########################################################
category = NULL
nom_case = NULL 
Temp = NULL

q3_2 =  q30_2 = q1_2 =  q10_2 = moy_2 = moy0_2 = med_2 = med0_2 = Sd_2 = Sd0_2 = NULL
q3_4 =  q30_4 = q1_4 =  q10_4 = moy_4 = moy0_4 = med_4 = med0_4 = Sd_4 = Sd0_4 = NULL

c1 = c2 = c3 = c4 = 0

Prob_2 = Prob0_2 = Cat_2 = NULL
Prob_4 = Prob0_4 = Cat_4 = NULL

cc = 0
ff = list.files("./exp_pred/", pattern="KAPPA")
for (ii in 1:2){
	cc = cc + 1
	cas = strsplit(strsplit(ff[ii],split=".RData")[[1]],split="Test")
	category[cc] = strsplit(cas[[1]][1],split="_")[[1]][2]
	cas = strsplit(cas[[1]][2],"GSAT")[[1]]
	if(length(cas) == 2){
		nom_case[cc] = print(cas[1])
		Temp[cc] = as.numeric(cas[2])
		#load(paste0("./exp_pred/",ff[ii]))

		if (Temp[cc] == 2){
			c2 = c2 + 1
			load(paste0("./exp_pred/",ff[ii]))

			Prob0_2 = c(Prob0_2,Yte0)
			Cat_2 = c(Cat_2,rep(cas[1],length(Yte)))

			q30_2[c2] = quantile(Yte0,0.95)

			q10_2[c2] = quantile(Yte0,0.05)

			moy0_2[c2] = mean(Yte0)

			med0_2[c2] = median(Yte0)

			Sd0_2[c2] = sd(Yte0)
		}

		if (Temp[cc] == 4){
			c4 = c4 + 1
			load(paste0("./exp_pred/",ff[ii]))

			Prob0_4 = c(Prob0_4,Yte0)
			Cat_4 = c(Cat_4,rep(cas[1],length(Yte)))

			q30_4[c4] = quantile(Yte0,0.95)

			q10_4[c4] = quantile(Yte0,0.05)

			moy0_4[c4] = mean(Yte0)

			med0_4[c4] = median(Yte0)

			Sd0_4[c4] = sd(Yte0)
		}
		
	}
}

df = data.frame(
	sle=c(Prob0_2,Prob0_4),
	temp=c(rep("2°C",10000),rep("4°C",10000))
)
df$temp = as.factor(df$temp)

TXT2.med = paste0("Median = ",round(median(Prob0_2)*100,1))
TXT2.q05 = paste0("[",round(quantile(Prob0_2,0.17)*100,1)," ; ",round(quantile(Prob0_2,0.83)*100,1), "]")

TXT4.med = paste0("Median = ",round(median(Prob0_4)*100,1))
TXT4.q05 = paste0("[",round(quantile(Prob0_4,0.17)*100,1)," ; ",round(quantile(Prob0_4,0.83)*100,1), "]")

colo = brewer.pal(n = 8, name = "Dark2")

#### PLOT probabilistic projections
p.sle <- ggplot(df, aes(x = sle*100,fill=temp,color=temp))+
        xlab("slc in 2100 [cm SLE]")+
	  #ylab("Frequency")+
 	  geom_histogram(aes(y=..density..), alpha=0.5, 
                position="identity")+
 	  geom_density(alpha=.3,linewidth=1.25) +
        #geom_bar( position = position_dodge(width = 0.8))+
	  theme_bw()+tt+
	  theme(axis.title.x = element_text(size=18),
		axis.title.y = element_text(size=18),#, face="bold"
		axis.text.y= element_text(size=18),
		axis.text.x= element_text(size=18))+
	  scale_color_manual(values=c(colo[1],colo[2],colo[2]))+
	  scale_fill_manual(values=c(colo[1],colo[2],colo[2]))+
	  annotate("text", x = 12.5, y = .30, label = TXT2.med,size = 6,color=colo[1])+
	  annotate("text", x = 12.5, y = .275, label = TXT2.q05,size = 6,color=colo[1])+
	  annotate("text", x = 22.5, y = .10, label = TXT4.med,size = 6,color=colo[2])+
	  annotate("text", x = 22.5, y = .075, label = TXT4.q05,size = 6,color=colo[2])
	  #ggtitle("GSAT diff. [°C]")#+facet_wrap(~temp)

