########################################################
#Funktion um log-regression an q-Werte zu fitten
q_modell<-function(datum,#datum der Messung
                   start,#startzeit der Kamera
                   mov_avg=31,#zellenweite des gleitenden mittels für die Modellierten Werte
                   q_filter=3){#zellenweite des gleitenden mittels für den Input
  
  #skript für read_waage funktion ausführen
  source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/durchf-hrung/waage.R")
  source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/durchf-hrung/event.R")

  #daten einlesen  
  q<-read_waage(datum,start,q_filter,na_interpolation = F)
  #q$q<-zoo::rollapply(q$q,filter,mean,na.rm=T,fill=NA)
    #Subset der daten bei denen q nicht null ist
  sub<-subset(q,q!=0)
  events<-event()
  begin<-events$start[events$datum==datum]
  
  offset0<-as.numeric(min(difftime(sub$date,begin,units = "min")))
  #Vektor mit offsets
  offset<-seq(1-offset0,1000,1)
  #Vektor für R² werte anlegen
  rsq<-offset

  #schleife um besten Offset zu finden
  for (i in 1:length(offset)){
    #id als zeitlicher Abstand vom beginn des Abflusses in Minuten
    #i unterschiedliche Offsets verwenden um log-regression daran zu fitten
    id<-as.numeric(difftime(sub$date,begin,units = "min"))+offset[i]
    #fitten des Modells
    fm<-glm(sub$wasser~log(id))
    #berechnen des R² des Modells
    rsq[i]<-1-fm$deviance/fm$null.deviance}

  #Offset des Modells mit dem besten fit wird verwendet
  best.offset<-offset[which.max(rsq)]
  id<-as.numeric(difftime(sub$date,begin,units = "min"))+best.offset
  #modell wird damit gefittet
  fm<-glm(sub$wasser~log(id))
  
  #Modellierte Werte in Minutenabständen
  preds<-predict(fm,data.frame(id=(min(id):max(id))))
  
  #offset der q-Kurve von den Modellierten Werten bestimmen
  #Vektor mit offsets anlegen
  offset2<-seq(0,0.05,len=1000)
  #Vektor für RMSE anlegen
  rmse<-offset2
  #Schleife um besten Offset zu finden
  for (i in 1:length(offset2)){
    #modellierte Wassermenge-Werte werden über die Zeit abgeleitet und der i-te Offset abgezogen
    qmod<-c(0,diff(predict(fm))/as.numeric(diff(sub$date))-offset2[i])
    #RMSE zwischen modellierten und gemessenen Werten berechnen
    rmse[i]<-sqrt(mean((qmod-sub$q)^2))}
  
  #Zeitsequenz in Minutenabständen vom start des Events an
  date<-seq(begin,max(sub$date),60)
  #der Offset mit dem besten RMSE wird verwendet um die modellierten Werte an die gemessenen anzugleichen
  best.offset2<-offset2[which.min(rmse)]
  #unterschied der länge vom Zeitvektor und der modellierten Werte
  lag<-length(date)-length(preds)
  #die modellierten q-Werte werden durch die Ableitung der Wassermenge-Werte minus offset2 bestimmt.
  #an den Anfang werden soviele 0 Werte geschrieben, dass die Zeitreihe am beginn der Beregnung beginnt
  qpreds<-c(rep(0,lag),0,diff(preds)-best.offset2)
  #Werte unter Null gibt es nicht
  qpreds[qpreds<0]<-0

  #gleitendes Mittel über die modellierten Werte
  q_filter<-zoo::rollapply(qpreds,mov_avg,mean,na.rm=T,fill=NA)
  #startwert ist Null
  q_filter[1]<-0  
  
  #auch der Wassermenge Vektor wird am anfang mit Nullen verlängert um mit dem Event zu beginnen
  preds<-c(rep(0,lag),preds)
  
  #Modellierte Werte in einen Datensatz
  mod<-data.frame(date=date,q_mod=qpreds,wasser_mod=preds,q_filter=q_filter)
  #Modellierte und gemessene Werte in einen Datensatz
  qs<-merge(q,mod,all=T)
  #Modellparameter in einen Datensatz
  params<-data.frame(datum,best.offset,best.offset2,lag,Intercept=fm$coefficients[1],log_id=fm$coefficients[2])
  
  #Output
  return(list(qs,params))}

qmod22<-q_modell(datum="22.10",start="13:27",q_filter = 1)
qmod15<-q_modell(datum="15.10",start="09:21",q_filter = 1)

qmod18<-q_modell(datum="18.10",start = "09:30",q_filter = 1)

q22<-qmod22[[1]]
q15<-qmod15[[1]]
q18<-qmod18[[1]]

offsets<-rbind(qmod15[[2]],qmod18[[2]],qmod22[[2]])
offsets

#qs$q[qs$date>q$date[8]&qs$date<q$date[9]]<-qs$q_mod[qs$date>q$date[8]&qs$date<q$date[9]]

###############################
#plots
events<-event()
library(ggplot2)

ggplot()+
  geom_point(data=q22,aes(date,q))+
  geom_line(data=q22,aes(date,q_filter))+
  theme_classic()+
  geom_rect(data=subset(events,datum=="22.10"),aes(xmin=start,xmax=stop,ymin = -Inf, ymax = Inf), alpha = 0.15,fill="blue")

ggplot()+
  geom_point(data=q18,aes(date,q))+
  geom_line(data=q18,aes(date,q_filter))+
  theme_classic()+
  geom_rect(data=subset(events,datum=="18.10"),aes(xmin=start,xmax=stop,ymin = -Inf, ymax = Inf), alpha = 0.15,fill="blue")

ggplot()+
  geom_point(data=q15,aes(date,q))+
  geom_line(data=q15,aes(date,q_filter),col=2)+
  theme_classic()+
  geom_rect(data=subset(events,datum=="15.10"),aes(xmin=start,xmax=stop,ymin = -Inf, ymax = Inf), alpha = 0.15,fill="blue")

plot(q22$date,q22$wasser)
lines(q22$date,q22$wasser_mod)

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

