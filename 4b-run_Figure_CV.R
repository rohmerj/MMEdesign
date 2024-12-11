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
		legend.position="none"
		)

#### COLORS
colo = brewer.pal(9,name="Spectral")
colo[5] = "gold2"

#################################################################
##### POST-TREATMENT - FORMAT
#################################################################
graphics.off()

## List of cases
CClist = c("RCP","RCM","KAPPA","ISM")

nom_case = type=  NULL 
Iter = NULL

RAE = RAE0 = QQ = QQ0 = NULL

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

	RAE[cc] = mean(abs(Yte-Y.te.BB)/Y.te.BB)
	QQ[cc] = 1-sum((Y.te.BB-Yte)^2)/sum((Y.te.BB-mean(Y.te.BB))^2)
}

}##Clist

### REFERENCE SOLUTION
for (bb in 1:25){

	load(paste0("./exp_cv/init/init_",bb,".RData"))

	RAE0[bb] = mean(abs(Yte0-Y.te.BB)/Y.te.BB)
	QQ0[bb] = 1-sum((Y.te.BB-Yte0)^2)/sum((Y.te.BB-mean(Y.te.BB))^2)

}

RAE0M = QQ0M= NULL
for (i in 1:25){
	f = which(Iter==i)
	RAE0M = c(RAE0M,mean(RAE0[i]))
	QQ0M = c(QQ0M,mean(QQ0[i]))
}

df.plt0 = data.frame(
		case = c(nom_case,rep("0riginal",length(QQ0M))),
		category = c(type,rep("0riginal",length(QQ0M))),
		rae = c(RAE,RAE0M),
		qq = c(QQ,QQ0M)		
)

###############################################################################
#### DEVIATION from REFERENCE SOLUTION
###############################################################################
f = which(df.plt0$category %in% c("0riginal"))
ref = df.plt0[f,4]
ref.rae = df.plt0[f,3]
qq = rae= cas =  NULL
cc = unique(df.plt0$case)[-length(unique(df.plt0$case))]
for (i in cc){
	f = which(df.plt0$case %in% i)
	qq = c(qq,(df.plt0[f,4]-ref)/ref)
	rae = c(rae,(df.plt0[f,3]-ref.rae)/ref.rae)
	cas = c(cas,rep(i,length(f)))
}

df.plt00 = data.frame(
	qq,
	rae,
	cas,
	cat = df.plt0$category[1:length(cas)]
)
df.plt00$cas = factor(df.plt00$cas,levels=unique(df.plt00$cas)) 
### rename some values
df.plt00$cat = plyr::revalue(df.plt00$cat,
			c(
				"CasRCP" = "RCP",
				"CasISM" = "ISM",
				"CasKAPPA" = paste0(expression(Kappa)),
				"CasRCM" = "RCM"
			)
			)

df.plt00$cas = plyr::revalue(df.plt00$cas,
			c(
				"SSP126 245" = "woSSP585",
				"SSP126 585" = "woSSP245",
				"SSP245 585" = "woSS126",
				"Kappa-int" = " Med. & extr. Kappa",
				"Kappa-ext" = " Narrow Kappa"
			)
			)

f = which(df.plt00$cas != "woMAR")## extract case woMAR
plt.rae = ggplot(df.plt00[f,],aes(x=as.factor(cas),y=rae*100,fill=as.factor(cas)))+geom_boxplot()+
			scale_fill_manual(values=colo)+
	geom_jitter()+theme_bw()+tt+xlab("")+ylab("RAE rel. diff. [%]")+ggtitle("")+
	theme(legend.position="none",legend.text=element_text(size=14))
print(plt.rae)

