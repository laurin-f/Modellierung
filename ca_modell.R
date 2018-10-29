capath<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/ca/"
load(file=paste0(capath,"cafm.R"))

okt18$ca_conc<-predict(cafm,data.frame(lf=okt18$lf))
okt18$ca_conc[okt18$ca_conc<0]<-0
okt18$ca_mg<-okt18$ca_conc*okt18$q/1000#mg/l*ml/min<-mg

sum(okt18$ca_mg,na.rm = T)/max(okt18$wasser,na.rm = T)*1000
