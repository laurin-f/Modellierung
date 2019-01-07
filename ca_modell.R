capath<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/ca/"
load(file=paste0(capath,"cafm.R"))

load("C:/Users/ThinkPad/Documents/Masterarbeit/daten/all.R")

#subset von tiefe -17 da nur hier die lf gemessen wurde 
sub<-subset(all,tiefe==-17)

#packages laden
library(ggplot2)
library(gridExtra)

#übersicht
qp<-ggplot(sub)+geom_line(aes(date,q_interpol))
lfp<-ggplot(sub)+geom_line(aes(date,lf))
grid.arrange(qp,lfp)

#vektor mit startzeitpunkten der event 
eventstarts<-c(which(sub$t_min==0),nrow(sub))

#data.frame erstellen für die aufsummierten Werte von Ca [mg] 
ca_sum<-data.frame(ca_transf=eventstarts[-1],datum=eventstarts[-1],tiefe=-17)

#Schleife um Ca concentration aufzusummieren und die gesamtkonzentration nach 
#den jeweiligen Events zu berechnen
for(i in 1:nrow(ca_sum)){
#die Calciumkonzentration [mg/l] wird durch die summe der Calciummenge [mg] 
#geteilt durch die  gesamt Wassermenge in l berechnet = mg/l
ca_sum$ca_transf[i]<-sum(sub$ca_mg[eventstarts[i]:(eventstarts[i+1]-1)],na.rm = T)/max(sub$wasser[eventstarts[i]:(eventstarts[i+1]-1)],na.rm = T)*1000
#datum des Events anfügen
ca_sum$datum[i]<-format(sub$date[eventstarts[i]],"%d.%m")
}#ede schleife

ca_sum

#mit den Messungen der IC zusammenfügen
ic<-merge(ic,ca_sum,all=T)
#plot der IC werte und der über die LF berechneten
#zwei aussreißer da hier die LF nicht bis zum ende gemessen wurde
ggplot(subset(ic,!is.na(ca_transf)))+geom_point(aes(ca,ca_transf,col=datum))+geom_abline(slope=1,intercept=0)



