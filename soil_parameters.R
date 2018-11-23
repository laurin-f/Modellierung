library(readxl)
soilpfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/bodenparameter/"

soil.xls<-read_xls(paste0(soilpfad,"Soil physical data Hartheim.xls"),sheet = 3)
soil.xls2<-read_xls(paste0(soilpfad,"Soil physical data Hartheim.xls"),sheet = 2)
pf<-aggregate(soil.xls2[,3:7],list(soil.xls2$Horizon,soil.xls2$hPa),function(x) mean(x,na.rm = T))
pfs<-subset(pf,Group.1 %in% c("Ah1","Ah2")&pf!=7)
soil<-aggregate(soil.xls[,4:41],list(soil.xls$Horizon),function(x) mean(x,na.rm = T))

soil$Dichte

#muafit<-function(data){
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


thr<-cbind(range(Ah1$thr),range(Ah2$thr))
ths<-cbind(range(Ah1$ths),range(Ah2$ths))

alpha<-cbind(ranges_Ah1[,1],ranges_Ah2[,1])
n<-cbind(ranges_Ah1[,2],ranges_Ah2[,2])

#alpha<-c(coef(mua_ah1)[1],coef(mua_ah2)[1])
#n<-c(coef(mua_ah1)[2],coef(mua_ah2)[2])
#ths<-soil$PV[1:2]/100
#thr<-soil$swc_15000hPa[1:2]/100

params<-data.frame(alpha=colMeans(alpha),n=colMeans(n),ths=colMeans(ths),thr=colMeans(thr),hseep=-100,l=0.5,ks=0.09)
save(params,file=paste0(soilpfad,"params.R"))
