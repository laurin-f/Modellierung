library(readxl)
soilpfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/bodenparameter/"

soil.xls<-read_xls(paste0(soilpfad,"Soil physical data Hartheim.xls"),sheet = 3)
soil.xls2<-read_xls(paste0(soilpfad,"Soil physical data Hartheim.xls"),sheet = 2)
pf<-aggregate(soil.xls2[,3:7],list(soil.xls2$Horizon,soil.xls2$hPa),function(x) mean(x,na.rm = T))
pfs<-subset(pf,Group.1 %in% c("Ah1","Ah2")&pf!=7)
soil<-aggregate(soil.xls[,4:41],list(soil.xls$Horizon),function(x) mean(x,na.rm = T))


muafit<-function(data){
  mua<-nls(th_norm~(1+(-alpha*psi)^n)^-(1-1/n),data=data,start = list(alpha=0.02,n=1.2))
  alpha<-coef(mua)[1]
  n<-coef(mua)[2]
  psi<-seq(min(data$psi),0,by=1)
  fit<-(1+(-alpha*psi)^n)^-(1-1/n)
  return(list(data.frame(psi,th_mod=fit),c(alpha,n)))}

Ah1<-subset(soil.xls2,Horizon=="Ah1"&pf!=7)
ths_Ah1<-subset(soil.xls,Horizon=="Ah1")
ths_Ah1$ths<-ths_Ah1$PV/100

Ah1$th<-Ah1$swc/100
thr_Ah1<-Ah1[Ah1$pf==4.2,c(8,10)]
colnames(thr_Ah1)<-c("MG_ID","thr")
Ah1<-merge(Ah1,thr_Ah1)

Ah1<-merge(Ah1,ths_Ah1[,c(1,42)])
sat_Ah1<-data.frame(th=ths_Ah1$ths,ths=ths_Ah1$ths,MG_ID=ths_Ah1$MG_ID,pf=0,thr=ths_Ah1$swc_15000hPa)
sat_Ah1<-sat_Ah1[!is.na(sat_Ah1$thr),]
Ah1<-merge(Ah1,sat_Ah1,all=T)

Ah1$th_norm<-(Ah1$th-Ah1$thr)/(Ah1$ths-Ah1$thr)
Ah1$psi<--10^Ah1$pf

mua_Ah1<-muafit(Ah1)
fit_Ah1<-mua_Ah1[[1]]
coef_Ah1<-mua_Ah1[[2]]

library(ggplot2)
ggplot()+geom_line(data=Ah1,aes(th_norm,pf,col=MG_ID))+geom_line(data=fit_Ah1,aes(th_mod,log10(-psi)))


Ah2<-subset(soil.xls2,Horizon=="Ah2"&pf!=7)
ths_Ah2<-subset(soil.xls,Horizon=="Ah2")
ths_Ah2$ths<-ths_Ah2$PV/100

Ah2$th<-Ah2$swc/100
thr_Ah2<-Ah2[Ah2$pf==4.2,c(8,10)]
colnames(thr_Ah2)<-c("MG_ID","thr")
Ah2<-merge(Ah2,thr_Ah2)

Ah2<-merge(Ah2,ths_Ah2[,c(1,42)])
sat_Ah2<-data.frame(th=ths_Ah2$ths,ths=ths_Ah2$ths,MG_ID=ths_Ah2$MG_ID,pf=0,thr=ths_Ah2$swc_15000hPa)
sat_Ah2<-sat_Ah2[!is.na(sat_Ah2$thr),]
Ah2<-merge(Ah2,sat_Ah2,all=T)

Ah2$th_norm<-(Ah2$th-Ah2$thr)/(Ah2$ths-Ah2$thr)
Ah2$psi<--10^Ah2$pf

mua_Ah2<-muafit(Ah2)
fit_Ah2<-mua_Ah2[[1]]
coef_Ah2<-mua_Ah2[[2]]

library(ggplot2)
ggplot()+geom_line(data=Ah2,aes(th_norm,pf,col=MG_ID))+geom_line(data=fit_Ah2,aes(th_mod,log10(-psi)))

alpha<-c(coef_Ah1[1],coef_Ah2[1])
n<-c(coef_Ah1[2],coef_Ah2[2])

thr<-pfs$swc[pfs$pf==4.2]/100
ths<-soil$PV[1:2]/100
th<-c(pfs$swc/100,ths)
th_norm<-(th-thr)/(ths-thr)

th_norm<-th
pf<-c(pfs$pf,0,0)
psi<--10^pf
Horizon<-c(pfs$Group.1,"Ah1","Ah2")
plot(th,pf,col=ifelse(Horizon=="Ah1",1,2),log="y")


mua_data<-data.frame(th_norm=th_norm,psi=psi,Horizon)


mua_ah1<-nls(th_norm~(1+(-alpha*psi)^n)^-(1-1/n),data=subset(mua_data,Horizon=="Ah1"),start = list(alpha=0.02,n=1.2))
mua_ah2<-nls(th_norm~(1+(-alpha*psi)^n)^-(1-1/n),data=subset(mua_data,Horizon=="Ah2"),start = list(alpha=0.02,n=1.2))


psi_seq<-seq(min(psi),max(psi))
fit_ah1<-muafit(subset(mua_data,Horizon=="Ah1"))[[1]]
fit_ah2<-muafit(subset(mua_data,Horizon=="Ah2"))[[1]]

plot(th_norm,-psi,log="y")
lines(fit_ah1[,2],-fit_ah1[,1],col=2)
lines(fit_ah2[,2],-fit_ah2[,1],col=3)

muafit(subset(mua_data,Horizon=="Ah1"))[[2]]
muafit(subset(mua_data,Horizon=="Ah2"))[[2]]

#alpha<-c(coef(mua_ah1)[1],coef(mua_ah2)[1])
#n<-c(coef(mua_ah1)[2],coef(mua_ah2)[2])
ths<-soil$PV[1:2]/100
thr<-soil$swc_15000hPa[1:2]/100

params<-data.frame(alpha,n,ths,thr)
save(params,file=paste0(soilpfad,"params.R"))
