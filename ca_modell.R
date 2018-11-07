capath<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/ca/"
load(file=paste0(capath,"cafm.R"))

load("C:/Users/ThinkPad/Documents/Masterarbeit/daten/all.R")

sub<-subset(all,tiefe==-17)


# #berechnung der Ca2+ Konzentration mit dem Modell
# sub$ca_conc<-predict(cafm,data.frame(lf=sub$lf))
# #es gibt keine Konzentrationen unter Null
# sub$ca_conc[sub$ca_conc<0]<-0
# #berechnung der Menge an Calcium in mg die pro Zeitschritt transportiert wird 
# sub$ca_mg<-sub$ca_conc*sub$q_interpol/1000#mg/l*ml/min<-mg


library(ggplot2)
library(gridExtra)


qp<-ggplot(sub)+geom_line(aes(date,q_interpol))
lfp<-ggplot(sub)+geom_line(aes(date,lf))
grid.arrange(qp,lfp)


ggplot(sub)+geom_line(aes(date,ca_mg),na.rm = T)

ggplot(sub)+geom_line(aes(date,ca_conc),na.rm = T)
eventstarts<-c(which(sub$t_min==0),nrow(sub))
ca_sum<-data.frame(ca_transf=eventstarts[-1],datum=eventstarts[-1],tiefe=-17)
for(i in 1:nrow(ca_sum)){

ca_sum$ca_transf[i]<-sum(sub$ca_mg[eventstarts[i]:(eventstarts[i+1]-1)],na.rm = T)/max(sub$wasser[eventstarts[i]:(eventstarts[i+1]-1)],na.rm = T)*1000
ca_sum$datum[i]<-format(sub$date[eventstarts[i]],"%d.%m")

}
ca_sum
ic<-merge(ic,ca_sum,all=T)
ggplot(subset(ic,!is.na(ca_transf)))+geom_point(aes(ca,ca_transf,col=datum))+geom_abline(slope=1,intercept=0)
