source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/durchf-hrung/read_all.R")

library(scales)
load("C:/Users/ThinkPad/Documents/Masterarbeit/daten/all.R")


schlauchinhalt_ml_cm<-8.4/80#ml/cm
schlauchlänge_cm<-10#cm
lf_kammer<-5#ml
schlauch<-schlauchinhalt_ml_cm*schlauchlänge_cm#+lf_kammer#ml

delay<-schlauch/all$q_interpol#ml/ml*min->min
13.4/0.00

summary(all$q_interpol)
summary(delay)

plot(delay,ylim=c(0,1000))
okt15<-read_all("15.10","09:21")
tiefe2_15.10<-read_vaisala("tiefe2_15.10.2","C:/Users/ThinkPad/Documents/Masterarbeit/daten/co2/",aggregate = T,temp_line = 31,Sonde=2)
tiefe2_15.10$datum<-round_date(tiefe2_15.10$date,"min")
okt15<-merge(okt15,tiefe2_15.10,all=T)
okt15<-subset(okt15,date>="2018-10-15 09:06:31 CEST")

okt18<-read_all(datum="18.10",start = "09:30")

okt22<-read_all(datum="22.10",start = "13:36")


lf_co2_sub<-function(data){
zeiten<-data$date[!is.na(data$lf)&data$lf>200]
zeiten<-zeiten[zeiten<"2018-10-28 02:00:00 CEST"|zeiten>"2018-10-28 03:00:00 CEST"]
sub<-data[data$tiefe==-14&data$date%in%zeiten,-(8:15)]
sub2<-data[data$tiefe==-17&data$date%in%zeiten,(8:15)]
sub<-cbind(sub,sub2)
#date<-data$date[data$tiefe==-14&data$date%in%zeiten]
#t_min<-data$t_min[data$tiefe==-14&data$date%in%zeiten]
#int<-data$treatment[data$tiefe==-14&data$date%in%zeiten]
#lf<-data$lf[data$tiefe==-14&data$date%in%zeiten]
return(sub)
  #data.frame(co2=co2sub,lf=lfsub,date,t_min,int))
}


sub15<-lf_co2_sub(okt15)
sub<-lf_co2_sub(okt18)
sub22<-lf_co2_sub(okt22)
sub<-lf_co2_sub(all)

delay<-schlauch/sub$q_interpol
lf<-sub$lf
date<-sub$date

datedel<-date-delay*60

par(mfrow=c(1,1))
plot(date,lf)
points(datedel,lf,col=2)


#mischung
#u<-2:length(lf)
#lf_unmix<-c(0,(lf[u]-lf[u-1]*((lf_kammer-sub$q_interpol[u])/lf_kammer))/(sub$q_interpol[u]/lf_kammer))

#lf_mix<-c(0,(lf[u]*(sub$q_interpol[u]/lf_kammer)+lf[u-1]*((lf_kammer-sub$q_interpol[u])/lf_kammer)))

#plot(date,lf)
#points(date,lf_mix)

sub<-sub[!is.na(sub$co2),]
co2lffm<-glm(lf~log(co2-min(sub$co2)+1)+int,data=sub)
sub$preds<-predict(co2lffm)

library(ggplot2)
ggplot(sub)+geom_point(aes(co2,lf,col=date))+facet_wrap(~int)+
  geom_point(aes(co2,preds),size=0.1)
+ scale_color_gradient(trans = time_trans(), low="blue", high="green")
par(mfrow=c(1,1))
plot(sub$co2,sub$lf)
points(sub$co2,preds,col=2)

par(mfrow=c(2,1))
plot(sub$t_min,sub$co2)
plot(sub$t_min,sub$lf)
lines(sub$date,preds)

sub18<-sub18[!is.na(sub18$co2),]
co2lffm<-glm(lf~log(co2-min(sub18$co2)+1),data=sub18)
preds<-predict(co2lffm)

par(mfrow=c(1,1))
plot(sub18$co2,sub18$lf)
lines(sub18$co2,preds)

par(mfrow=c(2,1))
plot(sub18$date,sub18$co2)
plot(sub18$date,sub18$lf)
lines(sub18$date,preds)


sub22<-sub22[!is.na(sub22$co2),]
co2lffm<-glm(lf~log(co2-min(sub22$co2)+1),data=sub22)
preds2<-predict(co2lffm)


par(mfrow=c(1,1))
plot(sub22$co2,sub22$lf)
lines(sub22$co2,preds2)

par(mfrow=c(2,1))
plot(sub22$date,sub22$co2)
plot(sub22$date,sub22$lf)
lines(sub22$date,preds2)

plot(okt15$date[okt15$tiefe==-14],okt15$CO2_raw[okt15$tiefe==-14],ylim=c(0,max(okt15$CO2_raw,na.rm = T)))
lines(okt15$date[okt15$tiefe==-17],okt15$lf[okt15$tiefe==-17])
