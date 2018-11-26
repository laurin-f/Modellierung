#package um .xls einzulesen
library(readxl)
#pfad des Datei
soilpfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/bodenparameter/"

#sheet 3 der .xls einlesen
soil.xls<-read_xls(paste0(soilpfad,"Soil physical data Hartheim.xls"),sheet = 3)
#sheet 2 der .xls einlesen
soil.xls2<-read_xls(paste0(soilpfad,"Soil physical data Hartheim.xls"),sheet = 2)

#aggregieren der Bodenparameter je Horizont
pf<-aggregate(soil.xls2[,3:7],list(soil.xls2$Horizon,soil.xls2$hPa),function(x) mean(x,na.rm = T))
#nur Ah1 und Ah2 werden gebraucht
pfs<-subset(pf,Group.1 %in% c("Ah1","Ah2")&pf!=7)

#aggregieren der Horizonte
soil<-aggregate(soil.xls[,4:41],list(soil.xls$Horizon),function(x) mean(x,na.rm = T))
#bulk density in g/cm3
soil$Dichte

###########################################
#funktion um Bodenretentionskurve zu fitten
muafit<-function(data){
  #fitten der Parameter alpha und n der Mualem van genuchten gleichung
  mua<-nls(th_norm~(1+(-alpha*psi)^n)^-(1-1/n),data=data,start = list(alpha=0.02,n=1.2))
  #ausgabe der Parameter
  alpha<-coef(mua)[1]
  n<-coef(mua)[2]
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
#ths ist 
ths_Ah1$ths<-ths_Ah1$PV/100

Ah1$th<-Ah1$swc/100
thr_Ah1<-Ah1[Ah1$pf==4.2,c(8,10)]
colnames(thr_Ah1)<-c("MG_ID","thr")
Ah1<-merge(Ah1,thr_Ah1)

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
alpha<-mean(ranges_Ah1[,1])*10

library(ggplot2)
ggplot()+geom_line(data=Ah1,aes(th_norm,pf,col=MG_ID))+geom_line(data=fit_Ah1,aes(th_mod,log10(-psi),col=MG_ID))

ggplot()+geom_line(data=Ah1,aes(th,pf,col=MG_ID))

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

library(ggplot2)
ggplot()+geom_line(data=Ah2,aes(th_norm,pf,col=MG_ID))+geom_line(data=fit_Ah2,aes(th_mod,log10(-psi),col=MG_ID))


thr<-range(Ah1$thr)
thr2<-range(Ah2$thr)
ths<-range(Ah1$ths)
ths2<-range(Ah2$ths)

alpha<-ranges_Ah1[,1]
alpha2<-ranges_Ah2[,1]

n<-ranges_Ah1[,2]
n2<-ranges_Ah2[,2]
#The free-air diffusivity of CO2 is 0.152 cm2 s−1
0.152*60
plot(Ah1$pf,Ah1$Ds)
plot()
realistic_ranges<-data.frame(alpha,alpha2,thr,thr2,ths,ths2,n,n2)

params<-data.frame(alpha=colMeans(alpha),n=colMeans(n),ths=colMeans(ths),thr=colMeans(thr),hseep=-100,l=0.5,ks=0.09)
save(params,realistic_ranges,file=paste0(soilpfad,"params.R"))
