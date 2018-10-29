########################################################
#Funktion um log-regression an q-Werte zu fitten
q_modell<-function(datum,#datum der Messung
                   start,#startzeit der Kamera
                   mov_avg=61,#zellenweite des gleitenden mittels für die Modellierten Werte
                   q_filter=1){#zellenweite des gleitenden mittels für den Input
  
  #skript für read_waage funktion ausführen
  source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/durchf-hrung/waage.R")
  source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/durchf-hrung/event.R")

  #daten einlesen  
  q<-read_waage(datum,start,q_filter=q_filter,na_interpolation = F)
  max(which(q$q==0))
  sub<-q[-(1:max(which((q$wasser)<1))),]
  #q$q<-zoo::rollapply(q$q,filter,mean,na.rm=T,fill=NA)
    #Subset der daten bei denen q nicht null ist
  sub<-subset(sub,!is.na(q))
  events<-event()
  begin<-events$start[events$datum==datum]
  
  offset0<-as.numeric(min(difftime(sub$date,begin,units = "min")))
  #Vektor mit offsets
  x.offset<-seq(1-offset0,1000,1)
  #Vektor für R² werte anlegen
  rsq<-x.offset

  #schleife um besten Offset zu finden
  for (i in 1:length(x.offset)){
    #id als zeitlicher Abstand vom beginn des Abflusses in Minuten
    #i unterschiedliche Offsets verwenden um log-regression daran zu fitten
    id<-as.numeric(difftime(sub$date,begin,units = "min"))+x.offset[i]
    #fitten des Modells
    fm<-glm(sub$wasser~log(id))
    #berechnen des R² des Modells
    rsq[i]<-1-fm$deviance/fm$null.deviance}
  plot(rsq)
  #Offset des Modells mit dem besten fit wird verwendet
  best.x.offset<-x.offset[which.max(rsq)]
  
  id<-as.numeric(difftime(sub$date,begin,units = "min"))+best.x.offset
  
  #modell wird damit gefittet
  fm<-glm(sub$wasser~log(id))

  date<-seq(begin,max(sub$date),60)
  id_min<-as.numeric(difftime(date,begin,units = "min"))+best.x.offset
  
  #Modellierte Werte in Minutenabständen
  preds<-predict(fm,data.frame(id=id_min))
  
 
  #offset der q-Kurve von den Modellierten Werten bestimmen
  #Vektor mit offsets anlegen
  y.offset<-seq(0,0.05,len=1000)
  #Vektor für RMSE anlegen
  rmse<-y.offset
  #Schleife um besten Offset zu finden
  for (i in 1:length(y.offset)){
    #modellierte Wassermenge-Werte werden über die Zeit abgeleitet und der i-te Offset abgezogen
    qmod<-c(0,diff(predict(fm))/as.numeric(diff(sub$date))-y.offset[i])
    #RMSE zwischen modellierten und gemessenen Werten berechnen
    rmse[i]<-sqrt(mean((qmod-sub$q)^2))}
  
  #Zeitsequenz in Minutenabständen vom start des Events an
  #der Offset mit dem besten RMSE wird verwendet um die modellierten Werte an die gemessenen anzugleichen
  best.y.offset<-y.offset[which.min(rmse)]
  #unterschied der länge vom Zeitvektor und der modellierten Werte
  lag<-as.numeric(min(difftime(sub$date,begin,units = "min")))
  #die modellierten q-Werte werden durch die Ableitung der Wassermenge-Werte minus y.offset bestimmt.
  #an den Anfang werden soviele 0 Werte geschrieben, dass die Zeitreihe am beginn der Beregnung beginnt
  preds[preds<0]<-0
  qpreds<-c(0,diff(preds)-best.y.offset)
  #Werte unter Null gibt es nicht
  qpreds[qpreds<0]<-0

  #gleitendes Mittel über die modellierten Werte
  q_filter<-zoo::rollapply(qpreds,mov_avg,mean,na.rm=T,fill=NA)
  #startwert ist Null
  q_filter[1]<-0  
  
  length(id_min)
  #Modellierte Werte in einen Datensatz
  mod<-data.frame(date=date,q_mod=qpreds,wasser_mod=preds,q_filter=q_filter,id=id_min)
  #Modellierte und gemessene Werte in einen Datensatz
  qs<-merge(q,mod,all=T)
  #Modellparameter in einen Datensatz
  params<-data.frame(datum,best.x.offset,best.y.offset,lag,Intercept=fm$coefficients[1],log_id=fm$coefficients[2])
  
  #Output
  return(list(qs,params))}

qmod22<-q_modell(datum="22.10",start="14:06",q_filter = 1)
qmod15<-q_modell(datum="15.10",start="09:21",q_filter = 1)

qmod18<-q_modell(datum="18.10",start = "09:30",q_filter = 1)
qmod26<-q_modell(datum="26.10",start = "10:03",q_filter = 1)

q22<-qmod22[[1]]
q15<-qmod15[[1]]
q18<-qmod18[[1]]

q26<-qmod26[[1]]

offsets<-rbind(qmod15[[2]],qmod18[[2]],qmod22[[2]],qmod26[[2]])
offsets

#qs$q[qs$date>q$date[8]&qs$date<q$date[9]]<-qs$q_mod[qs$date>q$date[8]&qs$date<q$date[9]]

###############################
#plots
events<-event()
library(ggplot2)

ggplot()+
  geom_point(data=q22,aes(date,q),na.rm = T)+
  geom_line(data=q22,aes(date,q_filter),na.rm = T)+
  theme_classic()+
  geom_rect(data=subset(events,datum=="22.10"),aes(xmin=start,xmax=stop,ymin = -Inf, ymax = Inf), alpha = 0.15,fill="blue")

ggplot()+
  geom_point(data=q26,aes(date,q),na.rm = T)+
  geom_line(data=q26,aes(date,q_filter),na.rm = T)+
  theme_classic()+
  geom_rect(data=subset(events,datum=="26.10"),aes(xmin=start,xmax=stop,ymin = -Inf, ymax = Inf), alpha = 0.15,fill="blue")

ggplot()+
  geom_point(data=q18,aes(date,q),na.rm = T)+
  geom_line(data=q18,aes(date,q_filter),na.rm = T)+
  theme_classic()+
  geom_rect(data=subset(events,datum=="18.10"),aes(xmin=start,xmax=stop,ymin = -Inf, ymax = Inf), alpha = 0.15,fill="blue")

ggplot()+
  geom_point(data=q15,aes(date,q),na.rm = T)+
  geom_line(data=q15,aes(date,q_filter),col=2,na.rm = T)+
  theme_classic()+
  geom_rect(data=subset(events,datum=="15.10"),aes(xmin=start,xmax=stop,ymin = -Inf, ymax = Inf), alpha = 0.15,fill="blue")

plot(q22$date,q22$wasser)
lines(q22$date,q22$wasser_mod)

plot(q26$date,q26$wasser)
lines(q26$date,q26$wasser_mod)

plot(q22$date,q22$q)
lines(q22$date,q22$q_mod)

plot(q15$date,q15$wasser)
lines(q15$date,q15$wasser_mod)

plot(q15$date,q15$q)
lines(q15$date,q15$q_mod)

plot(q18$date,q18$wasser)
lines(q18$date,q18$wasser_mod)

plot(q18$date,q18$q)
lines(q18$date,q18$q_mod)

plot(q)
qmod18<-q_modell(datum="18.10",start = "09:30",q_filter = 1)

datum<-c("15.10","18.10","22.10","26.10")
start<-c("09:21","09:30","14:06","10:03")
########################################################
#Funktion um log-regression an q-Werte zu fitten
q_modell2<-function(datum,#datum der Messung
                   start,#startzeit der Kamera
                   mov_avg=61,#zellenweite des gleitenden mittels für die Modellierten Werte
                   q_filter=3){#zellenweite des gleitenden mittels für den Input
  
  #skript für read_waage funktion ausführen
  source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/durchf-hrung/waage.R")
  source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/durchf-hrung/event.R")
  
  q<-vector("list", length(datum))
  min_seq<-vector("list", length(datum))
  lag<-vector("list", length(datum))
  events<-event()
  for (i in 1:length(datum)){
  #daten einlesen  
  dat<-read_waage(datum[i],start[i],q_filter=1,na_interpolation = F)
  dat$datum<-datum[i]
  dat<-dat[-(1:max(which((dat$wasser)<1))),]
  
  
  begin<-events$start[events$datum==datum[i]]
  dat$id<-as.numeric(difftime(dat$date,begin,units = "min"))
  
  dat$rain_mm_h<-events$rain_mm_h[events$datum==datum[i]]
  
  lag[[i]]<-as.numeric(min(difftime(dat$date,begin,units = "min")))

  #Zeitsequenz in Minutenabständen vom start des Events an
  date<-seq(begin,max(dat$date),60)
  id_min<-as.numeric(difftime(date,begin,units = "min"))
  int<-rep(events$rain_mm_h[events$datum==datum[i]],length(date))
  min_seq[[i]]<-data.frame(date,id_min,int)
  
  q[[i]]<-dat}

  sub<-do.call("rbind",q)
  mod.inp<-do.call("rbind",min_seq)
  lag<-do.call("c",lag)


  #q$q<-zoo::rollapply(q$q,filter,mean,na.rm=T,fill=NA)
  #Subset der daten bei denen q nicht null ist
  sub<-subset(sub,!is.na(q))
  rain_mm_h<-sub$rain_mm_h
  
  #Vektor mit offsets
  x.offset<-seq(1-min(lag),1000,1)
  #Vektor für R² werte anlegen
  rsq<-x.offset
  
  #schleife um besten Offset zu finden
  for (i in 1:length(x.offset)){
    #id als zeitlicher Abstand vom beginn des Abflusses in Minuten
    #i unterschiedliche Offsets verwenden um log-regression daran zu fitten
    id<-sub$id+x.offset[i]
    #fitten des Modells
    fm<-glm(sub$wasser~log(id)*rain_mm_h)
    #berechnen des R² des Modells
    rsq[i]<-1-fm$deviance/fm$null.deviance}
  
  plot(rsq)
  #Offset des Modells mit dem besten fit wird verwendet
  best.x.offset<-x.offset[which.max(rsq)]
  id<-sub$id+best.x.offset
  #modell wird damit gefittet
  fm<-glm(sub$wasser~log(id)*rain_mm_h)
  #Modellierte Werte in Minutenabständen
  preds<-predict(fm,data.frame(id=mod.inp$id_min,rain_mm_h=mod.inp$int))
  

  #offset der q-Kurve von den Modellierten Werten bestimmen
  #Vektor mit offsets anlegen
  y.offset<-seq(-0.01,0.01,len=100)
  #Vektor für RMSE anlegen
  rmse<-y.offset
  #Schleife um besten Offset zu finden
  for (i in 1:length(y.offset)){
    #modellierte Wassermenge-Werte werden über die Zeit abgeleitet und der i-te Offset abgezogen
    qmod<-c(0,diff(predict(fm))/as.numeric(diff(sub$date))-y.offset[i])
    #RMSE zwischen modellierten und gemessenen Werten berechnen
    rmse[i]<-sqrt(mean((qmod-sub$q)^2))
    }
  

  #plot(rmse)
  #der Offset mit dem besten RMSE wird verwendet um die modellierten Werte an die gemessenen anzugleichen
  best.y.offset<-y.offset[which.min(rmse)]
  #unterschied der länge vom Zeitvektor und der modellierten Werte
  #die modellierten q-Werte werden durch die Ableitung der Wassermenge-Werte minus y.offset bestimmt.
  #an den Anfang werden soviele 0 Werte geschrieben, dass die Zeitreihe am beginn der Beregnung beginnt
  preds[preds<0]<-0

  qpreds<-c(0,diff(preds)-best.y.offset)

  #Werte unter Null gibt es nicht
  qpreds[qpreds<0]<-0
  
  #gleitendes Mittel über die modellierten Werte
  q_filter<-zoo::rollapply(qpreds,mov_avg,mean,na.rm=T,fill=NA)
  #startwert ist Null
  q_filter[1]<-0  
  
  #auch der Wassermenge Vektor wird am anfang mit Nullen verlängert um mit dem Event zu beginnen
  #preds<-c(rep(0,lag),preds)
  
  
  #Modellierte Werte in einen Datensatz
  mod<-data.frame(date=mod.inp$date,q_mod=qpreds,wasser_mod=preds,q_filter=q_filter,id_min=mod.inp$id_min,int=round(mod.inp$int))
  #Modellierte und gemessene Werte in einen Datensatz
  qs<-merge(sub,mod,all=T)
  #Modellparameter in einen Datensatz
  params<-data.frame(datum,best.x.offset,best.y.offset,lag,Intercept=fm$coefficients[1],log_id=fm$coefficients[2])
  
  #Output
  return(list(qs,params))}


qs<-q_modell2(datum,start)
q<-qs[[1]]
qs[[2]]

library(ggplot2)
ggplot(q)+geom_line(aes(id_min,q_mod,col=as.factor(int)))+
  geom_point(aes(id_min,q))
+
  facet_wrap(~int)
