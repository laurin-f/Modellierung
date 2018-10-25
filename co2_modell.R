source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/durchf-hrung/read_all.R")

okt18<-read_all(datum="18.10",start = "09:30")
bf<-okt18$theta[okt18$tiefe==-14]
co2<-okt18$CO2_raw[okt18$tiefe==-14]
plot(bf)
bf<-bf[!is.na(bf)]
co2<-co2[!is.na(bf)]
offset<-which.max(co2)-which.max(bf)
plot(bf*13000)
lines(co2[-(1:offset)])
co2mod<-co2
for (i in 1:(length(co2)-offset)){

co2mod[i+offset+1]<-co2mod[i+offset]+((bf[i+1]-bf[i])*1000)-0.0001*i^1.1
}
plot(co2mod)
lines(co2)

n<-10000
bfmod<-filter(bf,rep(1/n,n))
plot(bf,type="l")
offset<-0:(length(bf)-1000)
rsq<-offset
for (i in 1:length(offset)){
bf_off<-bf[offset[i]:length(bf)]
co22<-co2[1:length(bf_off)]
co2fm<-glm(co22~bf_off)
rsq[i]<-1-co2fm$deviance/co2fm$null.deviance
if((i/length(offset)*100)%%5==0){
print(round(i/length(offset)*100))}
}
plot(rsq)
bf_off<-bf[offset[which.max(rsq)]:length(bf)]
co22<-co2[1:length(bf_off)]
co2fm<-glm(co22~bf_off)
preds<-predict(co2fm)
plot(preds)
lines(co2)
lines(bfmod)
0.1%%0.1
