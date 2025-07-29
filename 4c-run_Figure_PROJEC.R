library(ggplot2)
library(ggstatsplot)

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

colo=RColorBrewer::brewer.pal(9,name="Spectral")
colo[5] = "gold2"

#########################################################
##### FUNCTION
#########################################################
niv1 = 0.17
niv3 = 0.83
data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(median = median(x[[col]], na.rm=TRUE),
      q1 = quantile(x[[col]], na.rm=TRUE,niv1),
      q3 = quantile(x[[col]], na.rm=TRUE,niv3)
	)
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  return(data_sum)
}

median_IQR <- function(x) {
  data.frame(y = median(x), # Median
             ymin = quantile(x,0.17), # 1st quartile
             ymax = quantile(x,0.83))  # 3rd quartile
}

#################################################################
##### PROPAGTE EMULATOR UNC
#################################################################
### UNCOMMENT FOR LAUNCHING
'
graphics.off()

CClist = c("RCP","RCM","KAPPA","ISM")

dfrand = NULL
for (irand in 1:100){

category = NULL
nom_case = NULL 
Temp = NULL

c1 = c2 = c3 = c4 = 0

Prob_2 = Prob0_2 = Cat_2 = Cat0_2 = NULL
Prob_4 = Prob0_4 = Cat_4 = Cat0_4 = NULL

Prob_2u = Prob0_2u = Prob_4u = Prob0_4u = NULL

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
		#load(paste0("./exp_pred/",ff[ii]))

		if (Temp[cc] == 2){
			c2 = c2 + 1
			load(paste0("./exp_pred/",ff[ii]))

			Prob_2 = c(Prob_2,Yte)
			Prob0_2 = c(Prob0_2,Yte0)

			rr = runif(10000)
			rand = rand0 = NULL
			for(k in 1:10000){
				rand[k] = approx(x=seq(0,1,by=0.05),y=Qte[k,],xout=rr[k],rule=2)$y
				rand0[k] = approx(x=seq(0,1,by=0.05),y=Qte0[k,],xout=rr[k],rule=2)$y
			}
			Prob_2u = c(Prob_2u,rand)			
			Prob0_2u = c(Prob0_2u,rand0)			

			Cat_2 = c(Cat_2,rep(cas[1],length(Yte)))
			Cat0_2 = c(Cat0_2,rep(category[cc],length(Yte)))
		}

		if (Temp[cc] == 4){
			c4 = c4 + 1
			load(paste0("./exp_pred/",ff[ii]))

			Prob_4 = c(Prob_4,Yte)
			Prob0_4 = c(Prob0_4,Yte0)

			rr = runif(10000)
			rand = rand0 = NULL
			for(k in 1:10000){
				rand[k] = approx(x=seq(0,1,by=0.05),y=Qte[k,],xout=rr[k],rule=2)$y
				rand0[k] = approx(x=seq(0,1,by=0.05),y=Qte0[k,],xout=rr[k],rule=2)$y
			}
			Prob_4u = c(Prob_4u,rand)			
			Prob0_4u = c(Prob0_4u,rand0)			

			Cat_4 = c(Cat_4,rep(cas[1],length(Yte)))
			Cat0_4 = c(Cat0_4,rep(category[cc],length(Yte)))
		}
		
	}
}

}

Prob = Prob0 = Probu = Prob0u = list()
Prob[["2°C"]] = Prob_2
Prob0[["2°C"]] = Prob0_2
Prob[["4°C"]] = Prob_4
Prob0[["4°C"]] = Prob0_4
Probu[["2°C"]] = Prob_2u
Prob0u[["2°C"]] = Prob0_2u
Probu[["4°C"]] = Prob_4u
Prob0u[["4°C"]] = Prob0_4u
f = which(Temp == 2)

###########################################################################
nsim = 10000
temp = c("2°C","4°C")
df000 = TEMP = NULL
plt = list()

for (itemp in 1:2){

df.plt = data.frame(
	Prob = c(Probu[[itemp]],Prob0u[[itemp]][1:nsim]),
	case = c(
			rep(nom_case[f][1],nsim),
			rep(nom_case[f][2],nsim),
			rep(nom_case[f][3],nsim),
			rep(nom_case[f][4],nsim),
			rep(nom_case[f][5],nsim),
			rep(nom_case[f][6],nsim),
			rep(nom_case[f][7],nsim),
			rep(nom_case[f][8],nsim),
			rep(nom_case[f][9],nsim),
			rep("0riginal",nsim)
		)
	)

df.plt$case = as.factor(df.plt$case)
df0 = data_summary(df.plt,"Prob",c("case"))
#df0 = df0[-1,]
df00 = df0[1:(nrow(df0)),]
for (i in 1:nrow(df00)){
	for (j in 1:3){
		df00[i,j+1] = (df00[i,j+1]-df0[1,j+1])/df0[1,j+1]*100
	}
}

df000 = rbind(df000,df00)
TEMP = c(TEMP,rep(temp[itemp],nrow(df00)))

save(df.plt,file=paste0("./exp_pred/wUNC_META/","propag_mean_",itemp,"_",irand,".RData"))

}##temp

df000$temp = TEMP
names(df000)[3]= "Q1"
names(df000)[4]= "Q3"
df000$temp = as.factor(df000$temp)
df000$irand = rep(irand,nrow(df000))
dfrand = rbind(dfrand,df000)

}## random

save(dfrand,file=paste0("./exp_pred/wUNC_META/","propag_mean.RData"))
'

#########################################################################
################# RESTART HERE
#########################################################################

##### GRAPHICS - FORMAT
dodge <- position_dodge(width=0.9)
L1 = 30
L0 = 40

niv1 = 0.025
niv3 = 0.975
data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(median = median(x[[col]], na.rm=TRUE),
      q1 = quantile(x[[col]], na.rm=TRUE,niv1),
      q3 = quantile(x[[col]], na.rm=TRUE,niv3)
	)
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  return(data_sum)
}

##### LOAD PRECALCULATED UNC. PROPAGATION
load(paste0("./exp_pred/wUNC_META/","propag_mean.RData"))

##### median
dfrand0 = data_summary(dfrand,"median",c("case","temp"))
names(dfrand0) = c("case","temp","median","mini","maxi")
dfrand0$case = plyr::revalue(dfrand0$case,
			c(
				"SSP126_245_" = "woSSP585",
				"SSP126_585_" = "woSSP245",
				"SSP245_585_" = "woSSP126",
				"MAR_" = "MAR",
				"woMAR_" = "woMAR",
				"CISM_" = "CISM",
				"woCISM_" = "woCISM",
				"Extrem_" = " Med. & extr. Kappa",
				"Narrow_" = " Narrow Kappa",
				"0riginal" = "reference"
			)
			)
f = which(dfrand0$case == "reference")
dfrand0 = dfrand0[-f,]

dfrand0$case = factor(dfrand0$case, c("woSSP585","woSSP245","woSSP126","CISM","woCISM"," Med. & extr. Kappa"," Narrow Kappa","MAR","woMAR"))

colo=RColorBrewer::brewer.pal(9,name="Spectral")
colo[5] = "gold2"

plt2 = ggplot(dfrand0,aes(y=case,x=median,fill=as.factor(case)))+
		#geom_col(position = dodge)+
		geom_bar(stat = "identity")+
		geom_errorbar(aes(xmin = mini, xmax = maxi),position = dodge,width = 0.25)+
		scale_fill_manual(values=colo)+
		theme_bw()+tt+ylab("")+xlab("Rel. difference [%]")+xlim(-L0,L1)+facet_wrap(~temp)
##### Q1
dfrand0 = data_summary(dfrand,"Q1",c("case","temp"))
names(dfrand0) = c("case","temp","median","mini","maxi")
dfrand0$case = plyr::revalue(dfrand0$case,
			c(
				"SSP126_245_" = "woSSP585",
				"SSP126_585_" = "woSSP245",
				"SSP245_585_" = "woSSP126",
				"MAR_" = "MAR",
				"woMAR_" = "woMAR",
				"CISM_" = "CISM",
				"woCISM_" = "woCISM",
				"Extrem_" = " Med. & extr. Kappa",
				"Narrow_" = " Narrow Kappa",
				"0riginal" = "reference"
			)
			)
f = which(dfrand0$case == "reference")
dfrand0 = dfrand0[-f,]
dfrand0$case = factor(dfrand0$case, c("woSSP585","woSSP245","woSSP126","CISM","woCISM"," Med. & extr. Kappa"," Narrow Kappa","MAR","woMAR"))

plt1 = ggplot(dfrand0,aes(y=case,x=median,fill=case))+
		#geom_col(position = dodge)+
		geom_bar(stat = "identity")+
		geom_errorbar(aes(xmin = mini, xmax = maxi),position = dodge,width = 0.25)+
		scale_fill_manual(values=colo)+
		theme_bw()+tt+ylab("")+xlab("Rel. difference [%]")+xlim(-L0,L1)+facet_wrap(~temp)
##### Q3
dfrand0 = data_summary(dfrand,"Q3",c("case","temp"))
names(dfrand0) = c("case","temp","median","mini","maxi")
dfrand0$case = plyr::revalue(dfrand0$case,
			c(
				"SSP126_245_" = "woSSP585",
				"SSP126_585_" = "woSSP245",
				"SSP245_585_" = "woSSP126",
				"MAR_" = "MAR",
				"woMAR_" = "woMAR",
				"CISM_" = "CISM",
				"woCISM_" = "woCISM",
				"Extrem_" = " Med. & extr. Kappa",
				"Narrow_" = " Narrow Kappa",
				"0riginal" = "reference"
			)
			)
f = which(dfrand0$case == "reference")
dfrand0 = dfrand0[-f,]
dfrand0$case = factor(dfrand0$case, c("woSSP585","woSSP245","woSSP126","CISM","woCISM"," Med. & extr. Kappa"," Narrow Kappa","MAR","woMAR"))

plt3 = ggplot(dfrand0,aes(y=case,x=median,fill=case))+
		#geom_col(position = dodge)+
		geom_bar(stat = "identity")+
		geom_errorbar(aes(xmin = mini, xmax = maxi),position = dodge,width = 0.25)+
		scale_fill_manual(values=colo)+
		theme_bw()+tt+ylab("")+xlab("Rel. difference [%]")+xlim(-L0,L1)+facet_wrap(~temp)

############ FINAL FIGURE ############
cowplot::plot_grid(plt2,plt1,plt3,ncol=1,labels=c("Median"," Q17%"," Q83%"))





