########################################################
#Funktion um log-regression an q-Werte zu fitten
q_modell<-function(datum,#datum der Messung
                   start,#startzeit der Kamera
                   mov_avg=1){#zellenweite des gleitenden mittels
  
  #skript für read_waage funktion ausführen
  source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/durchf-hrung/waage.R")
  
  #daten einlesen  
  q<-read_waage(datum,start,mov_avg = mov_avg)
  #der erste q-Wert nach Zeitlücken über 2h wird entfernt 
  q$q<-ifelse(c(0,as.numeric(diff(q$date)))>3*60,NA,q$q)
  
  #Vektor mit offsets
  offset<-seq(1,1000,1)
  #Vektor für R² werte anlegen
  rsq<-offset
  #Subset der daten bei denen q nicht null ist
  sub<-subset(q,q!=0)
  
  #schleife um besten Offset zu finden
  for (i in 1:length(offset)){
    #id als zeitlicher Abstand vom beginn des Abflusses in Minuten
    #i unterschiedliche Offsets verwenden um log-regression daran zu fitten
    id<-as.integer((sub$date-min(sub$date))/60)+offset[i]
    #fitten des Modells
    fm<-glm(sub$wasser~log(id))
    #berechnen des R² des Modells
    rsq[i]<-1-fm$deviance/fm$null.deviance}
  
  #Offset des Modells mit dem besten fit wird verwendet
  best.offset<-offset[which.max(rsq)]
  id<-as.integer((sub$date-min(sub$date))/60)+best.offset
  #modell wird damit gefittet
  fm<-glm(sub$wasser~log(id))
  
  #Modellierte Werte in Minutenabständen
  preds<-predict(fm,data.frame(id=(min(id):max(id))))
  #Zeitsequenz in Minutenabständen
  date<-seq(min(sub$date),max(sub$date),60)
  
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
  
  #der Offset mit dem besten RMSE wird verwendet um die modellierten Werte an die gemessenen anzugleichen
  best.offset2<-offset2[which.min(rmse)]
  qpreds<-c(0,diff(preds)-best.offset2)
  
  #Modellierte Werte in einen Datensatz
  mod<-data.frame(date=date,q_mod=qpreds,wasser_mod=preds)
  #Modellierte und gemessene Werte in einen Datensatz
  qs<-merge(q,mod,all=T)
  #Output
  return(list(qs,cbind(datum,best.offset,best.offset2)))}

qmod22<-q_modell("22.10","13:27")
qmod15<-q_modell("15.10","09:21")
qmod18<-q_modell(datum="18.10",start = "09:30")

q22<-qmod22[[1]]
q15<-qmod15[[1]]
q18<-qmod18[[1]]

offset22<-qmod22[[2]]
offset15<-qmod15[[2]]
offset18<-qmod18[[2]]

offsets<-rbind(qmod15[[2]],qmod18[[2]],qmod22[[2]])
offsets

#qs$q[qs$date>q$date[8]&qs$date<q$date[9]]<-qs$q_mod[qs$date>q$date[8]&qs$date<q$date[9]]

###############################
#plots

library(ggplot2)

class(q22$q_mod)
ggplot()+
  geom_point(data=q22,aes(date,q))+
  geom_line(data=q22,aes(date,q_mod))+
  theme_classic()

ggplot()+
  geom_point(data=q18,aes(date,q))+
  geom_line(data=q18,aes(date,q_mod))+
  theme_classic()

ggplot()+
  geom_point(data=q15,aes(date,q))+
  geom_line(data=q15,aes(date,q_mod))+
  theme_classic()

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

