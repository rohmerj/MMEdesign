library(ggplot2)
library(RColorBrewer)
library(plyr)
library(cowplot)

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
		legend.position="none"
		)

## colors
colo=brewer.pal(9,name="Spectral")
colo[5] = "gold2"

## quantile level
niv1 = 0.95
niv2 = 0.05

#################################################################
##### DATA FORMAT
#################################################################
graphics.off()

## cases
CClist = c("RCP","RCM","KAPPA","ISM")

category = NULL
nom_case = NULL 
Temp = NULL

q3_15 =  q30_15 = q1_15 =  q10_15 = moy_15 = moy0_15 = Sd_15 = Sd0_15 = NULL
q3_2 =  q30_2 = q1_2 =  q10_2 = moy_2 = moy0_2 = med_2 = med0_2 = Sd_2 = Sd0_2 = NULL
q3_3 =  q30_3 = q1_3 =  q10_3 = moy_3 = moy0_3 = med_3 = med0_3 = Sd_3 = Sd0_3 = NULL
q3_4 =  q30_4 = q1_4 =  q10_4 = moy_4 = moy0_4 = med_4 = med0_4 = Sd_4 = Sd0_4 = NULL

c1 = c2 = c3 = c4 = 0

Prob_15 = Prob0_15 = Cat_15 = Cat0_15 = NULL
Prob_2 = Prob0_2 = Cat_2 = Cat0_2 = NULL
Prob_3 = Prob0_3 = Cat_3 = Cat0_3 = NULL
Prob_4 = Prob0_4 = Cat_4 = Cat0_4 = NULL

cc = 0
for (i in 1:length(CClist)){

	EXPR = CClist[i]
	ff = list.files("./exp_pred/", pattern=EXPR)

for (ii in 1:length(ff)){
	cc = cc + 1
	cas = strsplit(strsplit(ff[ii],split=".RData")[[1]],split="Test")
	category[cc] = strsplit(cas[[1]][1],split="_")[[1]][2]
	cas = strsplit(cas[[1]][2],"GSAT")[[1]]
	if(length(cas) == 2){
		nom_case[cc] = print(cas[1])
		Temp[cc] = as.numeric(cas[2])
		if (Temp[cc] == 1.5){
			c1 = c1 + 1
			load(paste0("./exp_pred/",ff[ii]))

			Prob_15 = c(Prob_15,Yte)
			Prob0_15 = c(Prob0_15,Yte0)
			Cat_15 = c(Cat_15,rep(cas[1],length(Yte)))
			Cat0_15 = c(Cat0_15,rep(category[cc],length(Yte)))

			q3_15[c1] = quantile(Yte,niv1)
			q30_15[c1] = quantile(Yte0,niv1)

			q1_15[c1] = quantile(Yte,niv2)
			q10_15[c1] = quantile(Yte0,niv2)

			moy_15[c1] = mean(Yte)
			moy0_15[c1] = mean(Yte0)

			med_15[c1] = median(Yte)
			med0_15[c1] = median(Yte0)

			Sd_15[c1] = sd(Yte)
			Sd0_15[c1] = sd(Yte0)
		}

		if (Temp[cc] == 2){
			c2 = c2 + 1
			load(paste0("./exp_pred/",ff[ii]))

			Prob_2 = c(Prob_2,Yte)
			Prob0_2 = c(Prob0_2,Yte0)
			Cat_2 = c(Cat_2,rep(cas[1],length(Yte)))
			Cat0_2 = c(Cat0_2,rep(category[cc],length(Yte)))

			q3_2[c2] = quantile(Yte,niv1)
			q30_2[c2] = quantile(Yte0,niv1)

			q1_2[c2] = quantile(Yte,niv2)
			q10_2[c2] = quantile(Yte0,niv2)

			moy_2[c2] = mean(Yte)
			moy0_2[c2] = mean(Yte0)

			med_2[c2] = median(Yte)
			med0_2[c2] = median(Yte0)

			Sd_2[c2] = sd(Yte)
			Sd0_2[c2] = sd(Yte0)
		}

		if (Temp[cc] == 3){
			c3 = c3 + 1
			load(paste0("./exp_pred/",ff[ii]))

			Prob_3 = c(Prob_3,Yte)
			Prob0_3 = c(Prob0_3,Yte0)
			Cat_3 = c(Cat_3,rep(cas[1],length(Yte)))
			Cat0_3 = c(Cat0_3,rep(category[cc],length(Yte)))

			q3_3[c3] = quantile(Yte,niv1)
			q30_3[c3] = quantile(Yte0,niv1)

			q1_3[c3] = quantile(Yte,niv2)
			q10_3[c3] = quantile(Yte0,niv2)

			moy_3[c3] = mean(Yte)
			moy0_3[c3] = mean(Yte0)

			med_3[c3] = median(Yte)
			med0_3[c3] = median(Yte0)

			Sd_3[c3] = sd(Yte)
			Sd0_3[c3] = sd(Yte0)
		}

		if (Temp[cc] == 4){
			c4 = c4 + 1
			load(paste0("./exp_pred/",ff[ii]))

			Prob_4 = c(Prob_4,Yte)
			Prob0_4 = c(Prob0_4,Yte0)
			Cat_4 = c(Cat_4,rep(cas[1],length(Yte)))
			Cat0_4 = c(Cat0_4,rep(category[cc],length(Yte)))

			q3_4[c4] = quantile(Yte,niv1)
			q30_4[c4] = quantile(Yte0,niv1)

			q1_4[c4] = quantile(Yte,niv2)
			q10_4[c4] = quantile(Yte0,niv2)

			moy_4[c4] = mean(Yte)
			moy0_4[c4] = mean(Yte0)

			med_4[c4] = median(Yte)
			med0_4[c4] = median(Yte0)

			Sd_4[c4] = sd(Yte)
			Sd0_4[c4] = sd(Yte0)
		}
		
	}
}

}

f = which(Temp == 2)
df.plt = data.frame(
	Mean = c(moy_2,moy0_2[1],moy_3,moy0_3[1],moy_4,moy0_4[1]),
	Median = c(med_2,med0_2[1],med_3,med0_3[1],med_4,moy0_4[1]),
	Sd = c(Sd_2,Sd0_2[1],Sd_3,Sd0_3[1],Sd_4,Sd0_4[1]),
	Q1 = c(q1_2,q10_2[1],q1_3,q10_3[1],q1_4,q10_4[1]),
	Q3 = c(q3_2,q30_2[1],q3_3,q30_3[1],q3_4,q30_4[1]),
	case = rep(c(nom_case[f],"0riginal"),3),
	temp = c(rep(2,length(f)+1),rep(3,length(f)+1),rep(4,length(f)+1))	
	)

#################################################################
##### PLOT FIGURE PERCENTILE CHANGES
#################################################################
f = which(Temp == 2)
df.plt = data.frame(
	Median = c((med_2-med0_2[1])/med0_2[1]*100,(med_3-med0_3[1])/med0_3[1]*100,(med_4-moy0_4[1])/moy0_4[1]*100),
	Q1 = c((q1_2-q10_2[1])/q10_2[1]*100,(q1_3-q10_3[1])/q10_3[1]*100,(q1_4-q10_4[1])/q10_4[1]*100),
	Q3 = c((q3_2-q30_2[1])/q30_2[1],(q3_3-q30_3[1])/q30_3[1],(q3_4-q30_4[1])/q30_4[1]*100),
	case = rep(c(nom_case[f]),3),
	category = rep(c(category[f]),3),
	temp = c(rep(2,length(f)),rep(3,length(f)),rep(4,length(f)))	
	)

df.plt0 = df.plt
df.plt0$category = factor(df.plt0$category)
## Rename
df.plt0$category = revalue(df.plt0$category,
			c(
				"CasRCP-" = "RCP",
				"CasRCM-" = "RCM",
				"CasKAPPA-" = "kappa",
				"CasISM-" = "ISM"
			)
			)
df.plt0$case = factor(df.plt0$case,levels=unique(df.plt0$case)) 
df.plt0$case = plyr::revalue(df.plt0$case,
			c(
				"SSP126_245_" = "woSSP585",
				"SSP126_585_" = "woSSP245",
				"SSP245_585_" = "woSS126",
				"MAR_" = "MAR",
				"woMAR_" = "woMAR",
				"CISM_" = "CISM",
				"woCISM_" = "woCISM",
				"Kappa-int_" = " Med. & extr. Kappa",
				"Kappa-ext_" = " Narrow Kappa"
			)
			)

df.plt0$temp = factor(df.plt0$temp)
df.plt0$temp = plyr::revalue(df.plt0$temp,
			c(
				"2" = "GSAT change = 2°C",
				"3" = "GSAT change = 3°C",
				"4" = "GSAT change = 4°C"
			)
			)

df.plt0 = df.plt0[(df.plt0$case != "woMAR"),]##extract noMAR

L = 60
plt1 = ggplot(df.plt0,aes(y=case,x=Median,fill=case))+
		geom_bar(stat = "identity")+
		scale_fill_manual(values=colo)+
		theme_bw()+tt+ylab("")+xlab("Rel. difference [%]")+xlim(-L,L)+facet_wrap(~temp)
plt3 = ggplot(df.plt0,aes(y=case,x=(Q3),fill=case))+
		geom_bar(stat = "identity")+
		scale_fill_manual(values=colo)+
		theme_bw()+tt+ylab("")+xlab("Rel. difference [%]")+xlim(-L,L)+facet_wrap(~temp)
plt2 = ggplot(df.plt0,aes(y=case,x=Q1,fill=case))+
		geom_bar(stat = "identity")+
		scale_fill_manual(values=colo)+
		theme_bw()+tt+ylab("")+xlab("Rel. difference [%]")+xlim(-L,L)+facet_wrap(~temp)
plot_grid(plt1,plt2,plt3,ncol=1,labels=c("Median"," Q5%"," Q95%"))

#################################################################
##### PLOT FIGURE FULL DISTRIBUTION
#################################################################
df.plt = data.frame(
	sle = c(Prob_2,Prob0_2[1:10000],Prob_3,Prob0_3[1:10000],Prob_4,Prob0_4[1:10000]),
	case = c(Cat_2,rep("init",10000),Cat_3,rep("init",10000),Cat_4,rep("init",10000)),
	category = c(Cat0_2,rep("original",10000),Cat_3,rep("original",10000),Cat0_4,rep("original",10000)),
	temp = c(rep(2,length(Prob_2)+10000),rep(3,length(Prob_3)+10000),rep(4,length(Prob_4)+10000))
	)
df.plt0 = df.plt
df.plt0$category = factor(df.plt0$category)
df.plt0$category = plyr::revalue(df.plt0$category,
			c(
				"CasRCP-" = "RCP",
				"CasRCM-" = "RCM",
				"CasKAPPA-" = "kappa",
				"CasISM-" = "ISM"
			)
			)
df.plt0$case = factor(df.plt0$case,levels=unique(df.plt0$case)) 
##rename
df.plt0$case = revalue(df.plt0$case,
			c(
				"init" = "original",
				"SSP126_245_" = "woSSP585",
				"SSP126_585_" = "woSSP245",
				"SSP245_585_" = "woSSP126",
				"MAR_" = "MAR",
				"woMAR_" = "woMAR",
				"CISM_" = "CISM",
				"woCISM_" = "woCISM",
				"Kappa-int_" = " Med. & extr. Kappa",
				"Kappa-ext_" = " Narrow Kappa"
			)
			)

df.plt0$temp = factor(df.plt0$temp)
df.plt0$temp = plyr::revalue(df.plt0$temp,
			c(
				"2" = "GSAT change = 2°C",
				"3" = "GSAT change = 3°C",
				"4" = "GSAT change = 4°C"
			)
			)

median_IQR <- function(x) {
  data.frame(y = median(x), # Median
             ymin = quantile(x,0.05), # 1st quartile
             ymax = quantile(x,0.95))  # 3rd quartile
}

df.plt2 = df.plt0
p.sle <- ggplot(df.plt2[(df.plt2$case != "woMAR"),], aes(x = sle,color=case,y=case))+
        xlab("slc in 2100 [m SLE]")+
	  theme_bw()+tt+
	facet_wrap(~temp)+
	geom_violin(trim = FALSE,linewidth=1.05) + 
  stat_summary(
    fun.data = "median_IQR", 
    geom = "pointrange", color = "black",linewidth=1.5,size=1.5
    )+
  scale_color_manual(values=colo)+
  theme(legend.position = "none")+ylab("")#+scale_colour_manual("black")
print(p.sle)
