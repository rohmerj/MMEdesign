library(ggplot2)
library(viridis)
library(cowplot)
library(RColorBrewer)

rm(list=ls())

#########################################################
##### DATA
#########################################################
load("./data/GrIS_MME_2100.RData")
df.tr = df.tr1

#########################################################
##### DOE
#########################################################
### HISTOGRAMM
tt <- theme(
		axis.title.x = element_text(size=14),
		axis.title.y = element_text(size=14),#, face="bold"
		axis.text.y= element_text(size=14),
		axis.text.x= element_text(size=14),
		axis.ticks = element_line(size = 0.75),
		legend.title=element_blank(),
		legend.position="none"
		)

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
        geom_bar( position = position_dodge(width = 0.8))+theme_bw()+tt+ylim(0,400)

p.resolution<-ggplot(df.tr, aes(x = x.resolution))+
	  coord_flip()+
	  xlab("Resolution [km]")+
        geom_bar( position = position_dodge(width = 0.8))+theme_bw()+tt+ylim(0,400)

p.gsat<-ggplot(df.tr, aes(x = C))+
	  coord_flip()+
        xlab("GSAT diff. [°C]")+
        geom_bar( position = position_dodge(width = 4))+theme_bw()+tt+ylim(0,400)

p.init_yrs<-ggplot(df.tr, aes(x = x.init_yrs))+
	  coord_flip()+
	  xlab("Numer of years for initialisation")+
        geom_bar( position = position_dodge(width = 0.8))+theme_bw()+tt+ylim(0,1300)

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
TXT.med = paste0("Median = ",round(median(df.tr$sl),3))
TXT.q95 = paste0("",round(quantile(df.tr$sl,0.975),2))
TXT.q05 = paste0("[",round(quantile(df.tr$sl,0.05),3)," ; ",round(quantile(df.tr$sl,0.95),3), "]")
p.sle <- ggplot(df.tr, aes(x = sl))+
        xlab("Sea level contribution in 2100 [m SLE]")+
	  #ylab("Count")+
	  geom_histogram(aes(y=..density..), alpha=0.5, position="identity")+
 	  geom_density(alpha=.2,linewidth=1.25) +
        #geom_bar( position = position_dodge(width = 0.8))+
	  theme_bw()+tt+ylim(0,9)+
	  annotate("text", x = .3, y = 7.5, label = TXT.med,size = 6)+
	  annotate("text", x = .3, y = 7., label = TXT.q05,size = 6)
	  #ggtitle("GSAT diff. [°C]")#

###########################################################
#### PLOT Validation ORIGINAL
###########################################################
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

RAE0 = QQ0 = NULL
YHAT = TRUTH = ITER =  NULL

for (bb in 1:25){

	load(paste0("./exp_cv/init/init_",bb,".RData"))

	RAE0[bb] = mean(abs(Yte0-Y.te.BB)/Y.te.BB)
	QQ0[bb] = 1-sum((Y.te.BB-Yte0)^2)/sum((Y.te.BB-mean(Y.te.BB))^2)

	YHAT = c(YHAT,Yte0)
	TRUTH = c(TRUTH,Y.te.BB)
	ITER = c(ITER, rep(bb,length(Yte0)))

}

df = data.frame(
	Q2=QQ0,
	RAE=RAE0,
	iter = 1:25,
	id=rep("",25)
)

#### boxplot Q2
p.qq= ggplot(df,aes(x=as.factor(id),y=Q2*100))+geom_boxplot(outlier.shape=NA)+
	geom_jitter(data=df, size=4,aes(color=iter)) + theme_bw()+
	scale_colour_viridis_c()+
	theme(
	axis.text.x=element_blank(), 
      axis.ticks.x=element_blank())+xlab("")+ylab("Q² [%]")+tt+ggtitle("(b)")

#### boxplot RAE
p.rr= ggplot(df,aes(x=as.factor(id),y=RAE*100,color=iter))+geom_boxplot(outlier.shape=NA)+geom_jitter(aes(color=iter), size=4) + 
	scale_colour_viridis_c()+
	theme_bw()+theme(
	axis.text.x=element_blank(), 
      axis.ticks.x=element_blank())+xlab("")+ylab("RAE [%]")+tt+ggtitle("(b)")

#### scatter plot predictions versus truth
df = data.frame(
	sle_predicted = YHAT,
	sle_true=TRUTH,
	iter = ITER
)
p.xy= ggplot(df,aes(sle_true,sle_predicted,color=iter))+geom_point(size=4)+geom_abline(xintercept = 0)+scale_colour_viridis_c()+
	theme_bw()+tt+
	#theme(legend.position="top")+
	xlab("slc  in 2100 [m SLE] - true")+ylab("slc in 2100 [m SLE] - predicted")+ggtitle("(a)")

plot_grid(p.xy,p.rr,ncol=2)

###########################################################
#### PLOT probabilistic projections
###########################################################
category = NULL
nom_case = NULL 
Temp = NULL

q3_15 =  q30_15 = q1_15 =  q10_15 = moy_15 = moy0_15 = Sd_15 = Sd0_15 = NULL
q3_2 =  q30_2 = q1_2 =  q10_2 = moy_2 = moy0_2 = med_2 = med0_2 = Sd_2 = Sd0_2 = NULL
q3_3 =  q30_3 = q1_3 =  q10_3 = moy_3 = moy0_3 = med_3 = med0_3 = Sd_3 = Sd0_3 = NULL
q3_4 =  q30_4 = q1_4 =  q10_4 = moy_4 = moy0_4 = med_4 = med0_4 = Sd_4 = Sd0_4 = NULL

c1 = c2 = c3 = c4 = 0

Prob_15 = Prob0_15 = Cat_15 = NULL
Prob_2 = Prob0_2 = Cat_2 = NULL
Prob_3 = Prob0_3 = Cat_3 = NULL
Prob_4 = Prob0_4 = Cat_4 = NULL

cc = 0
ff = list.files("./exp_pred/", pattern="RCM")
for (ii in 1:3){
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

		if (Temp[cc] == 3){
			c3 = c3 + 1
			load(paste0("./exp_pred/",ff[ii]))

			Prob0_3 = c(Prob0_3,Yte0)
			Cat_3 = c(Cat_3,rep(cas[1],length(Yte)))

			q30_3[c3] = quantile(Yte0,0.95)

			q10_3[c3] = quantile(Yte0,0.05)

			moy0_3[c3] = mean(Yte0)

			med0_3[c3] = median(Yte0)

			Sd0_3[c3] = sd(Yte0)
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
	sle=c(Prob0_2,Prob0_3,Prob0_4),
	temp=c(rep("2°C",10000),rep("3°C",10000),rep("4°C",10000))
)
df$temp = as.factor(df$temp)

TXT2.med = paste0("Median = ",round(median(Prob0_2),3))
TXT2.q05 = paste0("[",round(quantile(Prob0_2,0.05),3)," ; ",round(quantile(Prob0_2,0.95),3), "]")

TXT3.med = paste0("Median = ",round(median(Prob0_3),3))
TXT3.q05 = paste0("[",round(quantile(Prob0_3,0.05),3)," ; ",round(quantile(Prob0_3,0.95),3), "]")

TXT4.med = paste0("Median = ",round(median(Prob0_4),3))
TXT4.q05 = paste0("[",round(quantile(Prob0_4,0.05),3)," ; ",round(quantile(Prob0_4,0.95),3), "]")

colo = brewer.pal(n = 8, name = "Dark2")

#### PLOT probabilistic projections
p.sle <- ggplot(df, aes(x = sle,fill=temp,color=temp))+
        xlab("slc in 2100 [m SLE]")+
	  #ylab("Frequency")+
 	  geom_histogram(aes(y=..density..), alpha=0.5, 
                position="identity")+
 	  geom_density(alpha=.2,linewidth=1.25) +
        #geom_bar( position = position_dodge(width = 0.8))+
	  theme_bw()+tt+
	  scale_color_manual(values=c(colo[1],colo[6],colo[2]))+
	  scale_fill_manual(values=c(colo[1],colo[6],colo[2]))+
	  annotate("text", x = .125, y = 50, label = TXT2.med,size = 6,color=colo[1])+
	  annotate("text", x = .125, y = 47.5, label = TXT2.q05,size = 6,color=colo[1])+
	  annotate("text", x = .125, y = 45, label = TXT3.med,size = 6,color=colo[6])+
	  annotate("text", x = .125, y = 42.5, label = TXT3.q05,size = 6,color=colo[6])+
	  annotate("text", x = .125, y = 40, label = TXT4.med,size = 6,color=colo[2])+
	  annotate("text", x = .125, y = 37.5, label = TXT4.q05,size = 6,color=colo[2])
	  #ggtitle("GSAT diff. [°C]")#+facet_wrap(~temp)

