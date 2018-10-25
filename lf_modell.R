source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/durchf-hrung/read_all.R")

okt15<-read_all("15.10","09:21")
tiefe2_15.10<-read_vaisala("tiefe2_15.10.2","C:/Users/ThinkPad/Documents/Masterarbeit/daten/co2/",aggregate = T,temp_line = 31,Sonde=2)
tiefe2_15.10$datum<-round_date(tiefe2_15.10$date,"min")
okt15<-merge(okt15,tiefe2_15.10,all=T)
okt15<-subset(okt15,date>="2018-10-15 09:06:31 CEST")

okt18<-read_all(datum="18.10",start = "09:30")

okt22<-read_all(datum="22.10",start = "13:36")

lf_co2_sub<-function(data){
zeiten<-data$date[!is.na(data$lf)&data$lf>200]
co2sub<-data$CO2_raw[data$tiefe==-14&data$date%in%zeiten]
lfsub<-data$lf[data$tiefe==-17&data$date%in%zeiten]
date<-data$date[data$tiefe==-14&data$date%in%zeiten]
return(data.frame(co2=co2sub,lf=lfsub,date))
}

sub15<-lf_co2_sub(okt15)
sub18<-lf_co2_sub(okt18)
sub22<-lf_co2_sub(okt22)

plot(sub15)
plot(sub18)
plot(sub22)

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

okt10$CO2[okt$tiefe==-14]
plot(okt15$date[okt15$tiefe==-14],okt15$CO2_raw[okt15$tiefe==-14],ylim=c(0,max(okt15$CO2_raw,na.rm = T)))
lines(okt15$date[okt15$tiefe==-17],okt15$lf[okt15$tiefe==-17])
