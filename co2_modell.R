source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/durchf-hrung/read_all.R")

load("C:/Users/ThinkPad/Documents/Masterarbeit/daten/all.R")

okt18<-read_all(datum="18.10",start = "09:30")
bf<-okt18$theta[okt18$tiefe==-14]
co2<-okt18$CO2_raw[okt18$tiefe==-14]

plot(bf)
bf<-bf[!is.na(bf)]
co2<-co2[!is.na(bf)]
offset<-which.max(co2)-which.max(bf)


bf<-all$theta[all$tiefe==-14]
co2<-all$CO2_raw[all$tiefe==-14]
date<-all$date[all$tiefe==-14]



#co2mod<-c(rep(bf[1],10000),bf,rep(tail(bf,1),10000))*16000
co2mod<-bf
co2mod<-zoo::na.approx(co2mod)
co2<-co2[-(1:offset)]
date<-date[-(1:offset)]

for(j in 1:500){
n<-101
co2mod<-as.numeric(filter(co2mod,rep(1/n,n),circular = T))
#co2mod-zoo::rollapply(co2mod,100,mean,fill=NA)
}
co2mod<-co2mod[1:(length(co2mod)-offset)]
co2fm<-glm(co2~co2mod)
nas<-which(is.na(co2))
data<-data.frame(date,co2)
data<-data[-nas,]

preds<-predict(co2fm)

data$co2_mod<-preds


ggplot(data)+geom_line(aes(date,co2))+geom_line(aes(date,co2_mod),col=2)
