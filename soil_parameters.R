#package um .xls einzulesen
library(readxl)
#plot package
library(ggplot2)

#pfad des Datei
soilpfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/bodenparameter/"
plotpfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/abbildungen/plots/"

#sheet 3 der .xls einlesen
soil.xls<-read_xls(paste0(soilpfad,"Soil physical data Hartheim.xls"),sheet = 3)
#sheet 2 der .xls einlesen
soil.xls2<-read_xls(paste0(soilpfad,"Soil physical data Hartheim.xls"),sheet = 2)


#aggregieren der Horizonte
soil<-aggregate(soil.xls[,4:41],list(soil.xls$Horizon),function(x) mean(x,na.rm = T))
#bulk density in g/cm3
soil$Dichte

###########################################
#funktion um Bodenretentionskurve zu fitten
muafit<-function(data,alpha=NULL,n=NULL){
  if(is.null(alpha)&is.null(n)){
  #fitten der Parameter alpha und n der Mualem van genuchten gleichung
  mua<-nls(th_norm~(1+(-alpha*psi)^n)^-(1-1/n),data=data,start = list(alpha=0.02,n=1.2))
  #ausgabe der Parameter
  alpha<-coef(mua)[1]
  n<-coef(mua)[2]}
  #psi sequenz um gefittete Werte zu berechnen
  psi<-seq(min(data$psi),0,by=1)
  #mit gefitteten Alpha und n theta_werte berechnen
  fit<-(1+(-alpha*psi)^n)^-(1-1/n)
  return(list(data.frame(psi,th_mod=fit),c(alpha,n)))}

###############################
#Parameter für Ah1 fitten
#############################
#alle Proben aus Ah1 auswählen sheet 2
Ah1<-subset(soil.xls2,Horizon=="Ah1"&pf!=7)

#alle Proben aus Ah1 auswählen sheet 3
ths_Ah1<-subset(soil.xls,Horizon=="Ah1")
#ths entspricht dem gesamten Porenvolumen
ths_Ah1$ths<-ths_Ah1$PV/100

#theta ist der soil Water content (swc) /100
Ah1$th<-Ah1$swc/100

#thr ist der residuale Wassergehalt bei pf=4.2
thr_Ah1<-Ah1[Ah1$pf==4.2,c(8,10)]
#Spaltennamen anpassen
colnames(thr_Ah1)<-c("MG_ID","thr")
#thr an den Datensatz anfügen
Ah1<-merge(Ah1,thr_Ah1)

#ths an den dAtensatz anfügen
Ah1<-merge(Ah1,ths_Ah1[,c(1,42)])

sat_Ah1<-data.frame(th=ths_Ah1$ths,ths=ths_Ah1$ths,MG_ID=ths_Ah1$MG_ID,pf=0,thr=ths_Ah1$swc_15000hPa/100)
sat_Ah1<-sat_Ah1[!is.na(sat_Ah1$thr),]
Ah1<-merge(Ah1,sat_Ah1,all=T)

Ah1$th_norm<-(Ah1$th-Ah1$thr)/(Ah1$ths-Ah1$thr)
Ah1$psi<--10^Ah1$pf

fit_Ah1_list<-vector("list",length(unique(Ah1$MG_ID)))
coef_Ah1_list<-vector("list",length(unique(Ah1$MG_ID)))

for (i in 1:length(unique(Ah1$MG_ID))){
  
fit_Ah1_list[[i]]<-muafit(subset(Ah1,MG_ID==unique(Ah1$MG_ID)[i]))[[1]]
colnames(fit_Ah1_list[[i]])<-c("psi",unique(Ah1$MG_ID)[i])
coef_Ah1_list[[i]]<-muafit(subset(Ah1,MG_ID==unique(Ah1$MG_ID)[i]))[[2]]
}

coef_Ah1<-do.call("rbind",coef_Ah1_list)
fit_Ah1<-Reduce(merge,fit_Ah1_list)
fit_Ah1<-data.table::melt(fit_Ah1,id=1)
colnames(fit_Ah1)<-c("psi","MG_ID","th_mod")

ranges_Ah1<-apply(coef_Ah1,2,range)

RMSE<-function(obs,mod){
  rmse<-sqrt(mean((obs-mod)^2))
  return(rmse)
}

psi<-sort(unique(Ah1$psi))

th_norm<-tapply(Ah1$th_norm,Ah1$psi,mean)
n_parseq<-100
alpha<-rep(seq(0.05,4,len=n_parseq),n_parseq)
n<-rep(seq(1.1,2,len=n_parseq),each=n_parseq)
fit<-matrix(NA,length(n),length(psi))
for (i in 1:length(psi)){
fit[,i]<-(1+(-alpha*psi[i])^n)^-(1-1/n)
}

rmse<-apply(fit,1,function(x) RMSE(obs=th_norm,mod=x))

rmsegood<-which(rmse<=0.1)
bestrmse<-which.min(rmse)


alpha_range_Ah1<-range(alpha[rmsegood])
n_range_Ah1<-range(n[rmsegood])

fit_min<-muafit(Ah1,alpha = min(alpha[rmsegood]),n=min(n[rmsegood]))[[1]]
fit_max<-muafit(Ah1,alpha = max(alpha[rmsegood]),n=max(n[rmsegood]))[[1]]
fit_best<-muafit(Ah1,alpha = max(alpha[bestrmse]),n=max(n[bestrmse]))[[1]]

fit_range<-rbind(fit_min,fit_max[order(fit_max$psi,decreasing = T),])

Ah1_agr<-aggregate(Ah1$th_norm,list(Ah1$pf),mean)
colnames(Ah1_agr)<-c("pf","th_norm")

Ah1plot<-ggplot()+geom_polygon(data=fit_range,aes(th_mod,log10(-psi),fill="RMSE<0.1"),alpha=0.5)+geom_path(data=Ah1,aes(th_norm,pf,col="obs",linetype=MG_ID),show.legend = F)+geom_line(data=fit_best,aes(th_mod,log10(-psi),col="best fit"),size=1.4)+labs(x=expression(theta[norm]),y="pF")+theme_classic()+scale_colour_manual(name="",values = c("red",rep(grey(0.3),11)),labels=c("best fit",rep("obs",11)))+scale_fill_manual(name="",values = "grey")+scale_linetype_manual(values=rep(1,11))
Ah1plot


Ah1plot+ggsave(paste0(plotpfad,"muafit.pdf"),width=7,height=5)

########################################
#paramter für Ah2 fitten
#######################################
Ah2<-subset(soil.xls2,Horizon=="Ah2"&pf!=7)
ths_Ah2<-subset(soil.xls,Horizon=="Ah2")
ths_Ah2$ths<-ths_Ah2$PV/100

Ah2$th<-Ah2$swc/100
thr_Ah2<-Ah2[Ah2$pf==4.2,c(8,10)]
colnames(thr_Ah2)<-c("MG_ID","thr")
Ah2<-merge(Ah2,thr_Ah2)

Ah2<-merge(Ah2,ths_Ah2[,c(1,42)])
sat_Ah2<-data.frame(th=ths_Ah2$ths,ths=ths_Ah2$ths,MG_ID=ths_Ah2$MG_ID,pf=0,thr=ths_Ah2$swc_15000hPa/100)
sat_Ah2<-sat_Ah2[!is.na(sat_Ah2$thr),]
Ah2<-merge(Ah2,sat_Ah2,all=T)

Ah2$th_norm<-(Ah2$th-Ah2$thr)/(Ah2$ths-Ah2$thr)
Ah2$psi<--10^Ah2$pf

fit_Ah2_list<-vector("list",length(unique(Ah2$MG_ID)))
coef_Ah2_list<-vector("list",length(unique(Ah2$MG_ID)))

for (i in 1:length(unique(Ah2$MG_ID))){
  
  fit_Ah2_list[[i]]<-muafit(subset(Ah2,MG_ID==unique(Ah2$MG_ID)[i]))[[1]]
  colnames(fit_Ah2_list[[i]])<-c("psi",unique(Ah2$MG_ID)[i])
  coef_Ah2_list[[i]]<-muafit(subset(Ah2,MG_ID==unique(Ah2$MG_ID)[i]))[[2]]
}

coef_Ah2<-do.call("rbind",coef_Ah2_list)
fit_Ah2<-Reduce(merge,fit_Ah2_list)
fit_Ah2<-data.table::melt(fit_Ah2,id=1)
colnames(fit_Ah2)<-c("psi","MG_ID","th_mod")

ranges_Ah2<-apply(coef_Ah2,2,range)

psi<-sort(unique(Ah2$psi))

th_norm<-tapply(Ah2$th_norm,Ah2$psi,mean)
n_parseq<-100
alpha<-rep(seq(0.05,4,len=n_parseq),n_parseq)
n<-rep(seq(1.1,2,len=n_parseq),each=n_parseq)
fit<-matrix(NA,length(n),length(psi))
for (i in 1:length(psi)){
  fit[,i]<-(1+(-alpha*psi[i])^n)^-(1-1/n)
}

rmse<-apply(fit,1,function(x) RMSE(obs=th_norm,mod=x))

rmsegood<-which(rmse<=0.1)
bestrmse<-which.min(rmse)

alpha_range_Ah2<-range(alpha[rmsegood])

n_range_Ah2<-range(n[rmsegood])

fit_min<-muafit(Ah2,alpha = min(alpha[rmsegood]),n=min(n[rmsegood]))[[1]]
fit_max<-muafit(Ah2,alpha = max(alpha[rmsegood]),n=max(n[rmsegood]))[[1]]
fit_best<-muafit(Ah2,alpha = max(alpha[bestrmse]),n=max(n[bestrmse]))[[1]]

fit_range<-rbind(fit_min,fit_max[order(fit_max$psi,decreasing = T),])

Ah2plot<-ggplot()+geom_polygon(data=fit_range,aes(th_mod,log10(-psi)),fill="grey",alpha=0.5)+geom_line(data=Ah2,aes(th_norm,pf,col=MG_ID))+geom_line(data=fit_best,aes(th_mod,log10(-psi)))+labs(x=expression(theta[norm]),y="pF")+theme_classic()

Ah2plot
gridExtra::grid.arrange(Ah1plot,Ah2plot,ncol=2)

thr<-range(Ah1$thr)
thr2<-range(Ah2$thr)
ths<-range(Ah1$ths)
ths2<-range(Ah2$ths)

alpha<-ranges_Ah1[,1]
alpha2<-ranges_Ah2[,1]

n<-ranges_Ah1[,2]
n2<-ranges_Ah2[,2]
#The free-air diffusivity of CO2 is 0.152 cm2 s−1
D0<-0.152*60

eps<-range(soil.xls$air_15000hPa[soil.xls$Horizon%in%c("Ah1","Ah2")],na.rm = T)/100

DispA<-1.5*eps^2.74*D0

bulks<-t(aggregate(soil.xls$Dichte,list(soil.xls$Horizon),function(x) range(x,na.rm = T))[1:2,][2])

#tabellenwerte für ks matrix cm/min von silt loam bis sandy loam
#carsel parrish 1988
ks_range<-c(0.45/60,4.42/60)

#daten einladen
load("C:/Users/ThinkPad/Documents/Masterarbeit/daten/all.R")
r<-7.5#cm Radius
A<-pi*r^2#cm2 area
lapply(all_list,function(x) max(x$q,na.rm=T)/A)#cm3/min

#zeitdifferenz zwischen der negativsten steigung von theta in tiefe -14 und -10 
#zur abschätzung der gesättigten wasserleitfähigkeit
ks_peak_diff<-lapply(all_list,function(x) 4/as.numeric(difftime(x$date[x$tiefe==-14][which.min(diff(x$theta[x$tiefe==-14]))],x$date[x$tiefe==-10][which.min(diff(x$theta[x$tiefe==-10]))],units="min")))[1:4]

#die ranges sind ungefähr im selben Werte beirech
range(ks_peak_diff)#cm/min
ks_range#cm/min

realistic_bulk<-as.data.frame(bulks)
colnames(realistic_bulk)<-c("bulk","bulk2")



realistic_ranges<-data.frame(alpha=alpha_range_Ah1,alpha2=alpha_range_Ah2,alpha3=alpha_range_Ah2,n=n_range_Ah1,n2=n_range_Ah2,n3=n_range_Ah2,p_opt=c(0.00016,0.00022),DispA,ks=ks_range,ks2=ks_range)

#params<-data.frame(alpha=colMeans(alpha),n=colMeans(n),ths=colMeans(ths),thr=colMeans(thr),hseep=-100,l=0.5,ks=0.09)
save(realistic_ranges,realistic_bulk,file=paste0(soilpfad,"ranges.R"))

library(xtable)
tabelle1<-cbind(0.75,0.11,realistic_ranges[c(1,4,7,8,9)],realistic_bulk$bulk)
tabelle2<-cbind(0.64,0.13,realistic_ranges[c(2,5,7,8,9)],realistic_bulk$bulk2)
print(xtable((tabelle1)),include.rownames = F)
print(xtable((tabelle2)),include.rownames = F)
xtable(realistic_bulk)
