#Pfade definieren
hydruspfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/hydrus/"
projektpfad1<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/hydrus/undisturbed/"
projektpfad2<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/hydrus/undisturbed2/"
programmpfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/programme/Hydrus-1D 4.xx/"
mcpfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/hydrus/montecarlo/"
plotpfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/abbildungen/plots/mc/"

#Datensatz all laden
load("C:/Users/ThinkPad/Documents/Masterarbeit/daten/all.R")

#Skripte mit Funktionen ausführen
source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/modellierung/hydrus_input.R")
source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/modellierung/mc_out_function.R")
source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/modellierung/EET_na.R")
source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/durchf-hrung/event.R")

#Packages laden
library(ggplot2)
library(stringr)
tiefenstufen<-c(-2,-6,-10,-14)

####################################Z
#Parameterwerte die beim MC nicht varriert wurden
###################################

#ungestörter Boden
fixed<-data.frame(thr=0.11,
                  ths=0.75,
                  thr2=0.13,
                  ths2=0.64,
                  thr3=0.13,
                  ths3=0.64,
                  hseep=0,
                  l=0.5,
                  bulk=0.7561984,
                  bulk2=1.1480438,
                  difuz=0,
                  disperl=1.7,
                  cec=140,#aus scheffer schachtschabel tabelle parabraunerde KAKeff
                  calcit=0.2,
                  CaAds=500,
                  CaPrec=500,
                  DispA=9.54)

#gestörter Boden
fixed_dist<-data.frame(thr=0.067,
                       ths=0.45,
                       thr2=0.067,
                       ths2=0.45,
                       thr3=0.067,
                       ths3=0.45,
                       hseep=0,
                       l=0.5,
                       bulk=0.7561984,
                       bulk2=1.1480438,
                       difuz=0,
                       disperl=1.7,
                       cec=140,
                       calcit=0.2,
                       CaAds=500,
                       CaPrec=500,
                       DispA=9.54)


#Co2 Parameter
fixed_co2<-data.frame(act_en=6677,
                      h_crit=-10^6,
                      michaelis=0.19,
                      DispW=0.00106181,
                      Disper=5)

###############################################################
#mc_out Funktion anwenden für undist
###############################################################

#Namen der .R Dateien
loadfiles<-c("mc_55000-free_ranges","mc_55000-realistic_ranges")
#die mc_out Funktion speichert den Datensatz des besten Modellaufs ab
#dabei fügt sie entsprechend des verwendeten RMSE fit_CO2 etc. an
#also wird fit_... auch an den Vektor mit den Namen angefügt
loadfiles_undist<-paste0(c("fit_CO2-_","fit_Ca-_","fit_both-_"),rep(loadfiles,each=3))

#Matrizen anlegen um RMSE Werte und Parametersätze der besten Modellläufe
#reinzuschreiben und damit Tabellen für die Arbeit zu erstellen
rmse_norms<-matrix(NA,3,length(loadfiles_undist))
pars_tab<-matrix(NA,11,length(loadfiles_undist))
std_tab<-matrix(NA,11,length(loadfiles_undist))

#die Spaltennamen sind die Namen der jeweilig MC-Läufe
colnames(rmse_norms)<-gsub("-_mc_55000-|_"," ",loadfiles_undist)
colnames(pars_tab)<-colnames(rmse_norms)
colnames(std_tab)<-colnames(rmse_norms)

#j-Schleife um mc_out Funktion für alle MC-Dateien der ungestörten Probe anzuwenden 
#in den i-Schleife wird rmse_pos variiert und damit für jeden Lauf 
#sowohl fit CO2 fit Ca als auch fit both verwendet
for (i in 1:3){
  for (j in 1:length(loadfiles)){
  mc_out(fixed=cbind(fixed,fixed_co2),loadfile = loadfiles[j],dtmax = 10,kin_sol = T,plot = T,rmse_pos = c(1,4,5)[i],Nboot = 100,ndottys = 10000,hide_hydrus = T)
    
    #RMSE norm Werte des besten Laufs in Tabelle
    rmse_norms[,i+3*(j-1)]<-c(rmse_co2,rmse_ca,rmse_both)
    
    #Parameterwerte und deren sd in Tabelle schreiben
    std_tab[1:10,i+3*(j-1)]<-std
    pars_tab[11,i+3*(j-1)]<-c(rmse_co2,rmse_ca,rmse_both)[i]
    pars_tab[1:10,i+3*(j-1)]<-t(pars_opt)
}}

########################
#tabelle rmse norms undist
########################

#zeilennamen anpassen
rownames(rmse_norms)<-c("rmse CO2","rmse Ca","rmse both")
#fit ... aus Spaltennamen extrahieren
fit<-str_extract(colnames(rmse_norms),"fit (both|CO2|Ca)")
#fit ... aus Spaltennamen entfernen
colnames_rmse_norms<-gsub("fit (both|CO2|Ca)","",colnames(rmse_norms))
#Leerzeichen am Anfang und am Ende der Spaltennamen entfernen
colnames_rmse_norms<-gsub("(^\\s+)|(\\s+$)","",colnames_rmse_norms)
#Tabelle nach Spaltennamen und fit sortieren
rmse_norms2<-rmse_norms[,order(colnames_rmse_norms,fit)]
fit2<-fit[order(colnames(rmse_norms),fit)]

#Tabbele in LATEX format bringen
xtable::xtable(rmse_norms2)

##############################
#tabelle parameter undist
###############################

#Tabelle für Parameterwerte und jeweiligen sd Wert anlegen
par_opt_tab<-matrix(NA,11,12)
#Spaltennamen jeweils zweimal wiederholen und beim zweiten mal "sd" dranhängen
colnames(par_opt_tab)<-paste(rep(colnames(pars_tab),each=2),c("","sd"))

#die Parameterwerte und sd-Werte immer abwechselnd in Tabelle schreiben
par_opt_tab[,seq(1,11,2)]<-pars_tab
par_opt_tab[,seq(2,12,2)]<-std_tab
#Ca free und both free sollen nicht verwendet werden
par_opt_tab<-par_opt_tab[,-grep("Ca free|both free",colnames(par_opt_tab))]
#Tabelle nach Spaltennamen sortieren
par_opt_tab<-par_opt_tab[,order(colnames(par_opt_tab))]

#für jede Zeile die Werte auf 2 signifikante Nachkommastellen kürzen 
#und als character speichern damit xtable keinen Unsinn macht
par_opt_tab2<-t(apply(par_opt_tab,1,function(x)as.character(signif(x,2))))
#dabei gehen die Spaltennamen verloren also werden sie wieder drangehängt
colnames(par_opt_tab2)<-colnames(par_opt_tab)

#Zeilennamen anpassen in für LATEX passendem Format
rownames(par_opt_tab2)<-c("$\\alpha_1$","$\\alpha_2$","h$_{opt}$","K$_{S1}$","K$_{S2}$","K$_{S3}$","n$_1$","n$_2$","P$_{distr}$","P$_{opt}$","RMSE$_{norm}$")

#die Spalten mit den sd-Werten sollen grau sein
par_opt_tab2[1:10,seq(2,ncol(par_opt_tab2),2)]<-paste("\\cellcolor{lightgray}",par_opt_tab2[1:10,seq(2,ncol(par_opt_tab2),2)])

#Tabelle mittels xtable in LATEX Format bringen
#mit sanatize.... wird verhindert das xtable "\" zu "\backslash" umformatiert
print(xtable::xtable(par_opt_tab2),sanitize.rownames.function=identity,sanitize.text.function=identity)

###############################################################
#mc_out Funktion anwenden für dist
###############################################################

#Names der Dateien
loadfiles2<-c("mc_55000-dist_free_ranges","mc_55000-dist_realistic_ranges","mc_55000-dist_fit_tiefe_1-2")
#für dist wird nur fit_CO2 verwendet
loadfiles_dist<-paste0("fit_CO2-_",loadfiles2)

#Matrizen für Tabellen anlegen 
par_tab_dist<-matrix(NA,11,length(loadfiles_dist))
std_tab_dist<-matrix(NA,11,length(loadfiles_dist))

#Spaltennamen definieren
colnames(par_tab_dist)<-gsub("mc_..000-dist_"," ",loadfiles2)
colnames(std_tab_dist)<-colnames(par_tab_dist)

#Schleife um mc_out Funktion anzuwenden
for (i in 1:length(loadfiles2)){
      
    fixed_pars<-cbind(fixed_dist,fixed_co2)
    mc_out(fixed=fixed_pars,loadfile = loadfiles2[i],dtmax = 10,kin_sol = F,plot = T,rmse_pos = 1,Nboot = 100,ndottys = 10000,hide_hydrus = T,obs=alldist_s,traintime = 8000,Probe = "dist",n_best = c(1,2,2)[i])
    
    #sd- RMSE- und Parameterwerte in Matrizen schreiben
    std_tab_dist[1:10,i]<-std
    par_tab_dist[11,i]<-rmse_co2
    par_tab_dist[1:10,i]<-t(pars_opt)
  }


##############################
#tabelle parameter dist
###############################

#formatieren der Tabelle wie bei undist
par_opt_tab_dist<-matrix(NA,11,2*length(loadfiles_dist))
colnames(par_opt_tab_dist)<-paste(rep(colnames(par_tab_dist),each=2),c("","sd"))
rownames(par_opt_tab_dist)<-c(colnames_par,"RMSE")
rownames(par_tab_dist)<-rownames(par_opt_tab_dist)
rownames(std_tab_dist)<-rownames(par_opt_tab_dist)
par_opt_tab_dist[,seq(1,ncol(par_opt_tab_dist)-1,2)]<-par_tab_dist
par_opt_tab_dist[,seq(2,ncol(par_opt_tab_dist),2)]<-std_tab_dist
par_opt_tab_dist<-par_opt_tab_dist[,order(colnames(par_opt_tab_dist))]

#kürzen der Werte
par_opt_tab_dist2<-t(apply(par_opt_tab_dist,1,function(x)as.character(signif(x,2))))
colnames(par_opt_tab_dist2)<-colnames(par_opt_tab_dist)

#Zeilennamen
rownames(par_opt_tab_dist2)<-c("$\\alpha_1$","$\\alpha_2$","h$_{opt}$","K$_{S1}$","K$_{S2}$","K$_{S3}$","n$_1$","n$_2$","P$_{distr}$","P$_{opt}$","RMSE$_{norm}$")
par_opt_tab_dist2[1:10,seq(2,ncol(par_opt_tab_dist2),2)]<-paste("\\cellcolor{lightgray}",par_opt_tab_dist2[1:10,seq(2,ncol(par_opt_tab_dist2),2)])
#Tabelle erstellen
print(xtable::xtable(par_opt_tab_dist2),sanitize.rownames.function=identity,sanitize.text.function=identity)


#############################
#EE plots für  Ergebnisse undist
###############################

#die EET_plots werden in der mc_out Funktion mittels assign() in die Global Env geschrieben
#hier werden die benötigten mit get() genommen und etwas umformatiert
EE_co2_free<-get(paste0(loadfiles_undist[1],"EET_plt"))+theme(legend.position = "none")+labs(title=expression("fit CO"[2]),subtitle="free ranges")
EE_co2_real<-get(paste0(loadfiles_undist[4],"EET_plt"))+labs(y="",title="",subtitle="realistic ranges")

EE_both_free<-get(paste0(loadfiles_undist[3],"EET_plt"))+theme(legend.position = "none")+labs(title="fit both",subtitle="free ranges")
EE_both_real<-get(paste0(loadfiles_undist[6],"EET_plt"))+labs(y="",title="",subtitle="realistic ranges")

#eine Matrix zum arrangieren der Plots da der linke einen Titel hat ist er etwas höher
#der rechte hat eine Legende und ist etwas breiter
layout.mat<-matrix(2,60,20)
layout.mat[1,]<-3
layout.mat[,1:9]<-1

#Exportieren von jeweils 2 EE plot nebeneinander 
pdf(paste0(plotpfad,"EE_fit_both.pdf"),height = 3.5,width = 7)
gridExtra::grid.arrange(EE_both_free,EE_both_real,ncol=2,layout_matrix=layout.mat)
dev.off()

pdf(paste0(plotpfad,"EE_fit_CO2.pdf"),height = 3.5,width = 7)
gridExtra::grid.arrange(EE_co2_free,EE_co2_real,ncol=2,layout_matrix=layout.mat)
dev.off()


#############################
#EE plots für  Ergebnisse dist
###############################

EE_co2_free_dist<-get(paste0(loadfiles_dist[1],"EET_plt"))+theme(legend.position = "none")+labs(title="gestörte Probe",subtitle="free ranges")
EE_co2_real_dist<-get(paste0(loadfiles_dist[2],"EET_plt"))+labs(y="",title="",subtitle="realistic ranges")

pdf(paste0(plotpfad,"EE_fit_CO2_dist.pdf"),height = 3.5,width = 7)
gridExtra::grid.arrange(EE_co2_free_dist,EE_co2_real_dist,ncol=2,layout_matrix=layout.mat)
dev.off()


##################
#Tabelle CO2mean CO2q und Siq
##################
#Output des MC-Laufs laden
load(file = paste0(mcpfad,"mc_55000-realistic_ranges",".R"))
  #der Output ist in die Liste mc geschrieben
  #die einzelnen Listenelemente auspacken
  par<-mc[[2]]
  rmse<-mc[[1]]
  nse<-mc[[3]]
  pars<-cbind(par[which.min(rmse),],fixed,fixed_co2)
  
  #mit Funktion das Modell für gesamten messzeitraum (obs=all) ausführen und Output laden
  out<-hydrus(params = pars,
              UNSC=T,
              hide_hydrus=F,
              free_drain=T,
              print_times = 2000,
              dtmax = 10,
              obs=all,
              min_nrows=100,
              kin_sol=T)
data1<-out
#data1_s enthält den modelloutput nur für den Trainingsdatensatz all_s
data1_s<-get(loadfiles_undist[4])

#das erste Warm-Up-event abschneiden
#das entspricht dem zweiten-Mal das die Intensität von 0 ansteigt
ohne_warmup<-data1$t_min[!is.na(data1$rain_mm_h)][which(diff(data1$rain_mm_h[!is.na(data1$rain_mm_h)])>0)[2]]
ohne_warmup_s<-data1_s$t_min[!is.na(data1_s$rain_mm_h)][which(diff(data1_s$rain_mm_h[!is.na(data1_s$rain_mm_h)])>0)[2]]

#schauen was der Wert von p_optund p-distr ist
pars$p_opt#0.0002
pars$p_distr#0.0056
#der maximale Wert der CO2_Produktion im Output (vProd) 
#müsste eigentlich mit p_opt übereinstimmt
max(out$vProd,na.rm = T)#1.78e-05
#vProd ist jedoch deutlich niedriger

#p_distr erhöhen
pars$p_distr<-0.2
#testen ob mit hohem p_dist die höchste Produktion im Modell näher an p_opt dran ist
out<-hydrus(params = pars,
            UNSC=T,
            hide_hydrus=F,
            free_drain=T,
            print_times = 2000,
            dtmax = 10,
            obs=all,
            min_nrows=100,
            kin_sol=T)
#ja 
pars$p_opt#0.0002
max(out$vProd,na.rm = T)#0.00019
#P_distr hat also einen Einfluss auf vProd und nicht nur auf die Tiefenverteilung

#Subset des outputs ohne Warm-Up-Event
data<-subset(data1,t_min>=ohne_warmup&tiefe%in%c(-2,-6,-10,-14,-17))
data_s<-subset(data1_s,t_min>=ohne_warmup_s)

#CO2_q und SI_q berechnen um gewichtetes Mittel zu bestimmen
data$CO2_q<-data$CO2_mod*data$q_mod
data$SI_q<-data$SI*data$q_mod

#auch für die CO2-Messungen wird modelliertes q-verwendet, 
#da es für tiefe -17 keine Messungen gibt 
data$CO2_obs_q<-data$CO2_raw*data$q_mod

#Mittelwerte für jede Intensität bestimmen
#Es wird zudem der Mittelwert q2 bestimmt q2 ist der Mittelwert aller q_mod Werte bei denen
#CO2 Messwerte vorhanden sind
aggs<-aggregate(data.frame(q=data$q_mod,CO2=data$CO2_mod,CO2_q=data$CO2_q,SI=data$SI,SI_q=data$SI_q,ca_mod=data$Ca_mod,CO2_obs=data$CO2_raw,CO2_obs_q=data$CO2_obs_q,ca_obs=data$ca_conc,q2=ifelse(is.na(data$CO2_raw),NA,data$q_mod)),list(treatment=data$treatment),function(x) mean(x,na.rm=T))

#Für die Verwitterung wird nicht der Mittelwert sondern die Summe verwendet
#dazu wird der Trainingsdatensatz verwendet da hier jede Intensität nur einmal vorkommt 
ca_we_sum<-aggregate(data_s$Ca_weather,list(data_s$treatment),function(x) sum(x,na.rm=T))

#Die nach q gewichteten Mitellwerte werden durch q geteilt
aggs$CO2_q<-aggs$CO2_q/aggs$q
#CO2_obs_q wird durch q2 geteilt sonst werden q Werte berücksichtigt
#die nicht in die Gewichtung eingegangen sind
aggs$CO2_obs_q<-aggs$CO2_obs_q/aggs$q2
aggs$SI_q<-aggs$SI_q/aggs$q

#die Calcitverwitterung wird zu den aggregierten Werte hinzugefügt
aggs$ca_verwitterung<-ca_we_sum$x#meq/kg

#die Tabellen werden in LATEX Format gebracht
print(xtable::xtable(aggs[,-c(2,5,8:11)]),include.rownames = F)
print(xtable::xtable(aggs[,c(1,8:10)]),include.rownames = F)


#########################
#plot der Modellläufe zusammen undist
##########################

#runname wird für die Legende im Plot verwendet
runname<-gsub("-_mc_55000-|_"," ",loadfiles_undist)

#events laden
events<-event()

#event1 als alle events die im Zeitraum des Trainingsdatensatzes (all_s) liegen
event1<-subset(events,start>=min(all_s$date)&stop<=max(all_s$date))
#Bei event2 wird event1 für jede Tiefenstufe wiederholt
event2<-data.frame(start=rep(event1$start,4),stop=rep(event1$stop,4),tiefe=rep(c(-2,-6,-10,-14),each=nrow(event1)))

#Datensatz zum Plotten
maindata<-subset(get(loadfiles_undist[1]),tiefe%in%c(-2,-6,-10,-14))

#Namen für die Tiefenstufen im Plot
name_tiefe<-setNames(c("Tiefe = -2 cm","-6 cm","-10 cm","-14 cm"),c(2,6,10,14))

#Plots mit Messwerten anlegen
co2plt<-ggplot(data=maindata)+geom_line(aes(date,CO2_raw,linetype=""),col=1)
bfplt<-ggplot(data=maindata)+geom_line(aes(date,theta,linetype=""),col=1)
qplt<-ggplot(data=subset(get(loadfiles_undist[1]),tiefe==-17))+geom_line(aes(date,q_interpol*5,linetype=""),col=1)

#Plot von CO2-Produktion und Flux
ggplot(data=subset(get(loadfiles_undist[4]),!is.na(P_0)))+geom_line(aes(date,P_2_korr,col="3"))+geom_line(aes(date,P_4_korr,col="4"))+geom_line(aes(date,cvTop,col="1"))+geom_line(aes(date,vProd,col="2"))+
  geom_rect(data=event1,aes(xmin=start,xmax=stop,ymin = -Inf, ymax = Inf,fill=""), alpha = 0.15)+theme_classic()+
  labs(x="",y=expression("CO"[2]*" [ml  cm"^-2*" min"^-1*"]"),col="")+scale_color_discrete(labels=c("Flux 0 cm",paste("Prod <",c(0,-2,-4),"cm")))+
  theme_classic()+
  scale_fill_manual(name="Beregnung",values="blue")+
  ggsave(paste0(plotpfad,"CO2_Prod_flux.pdf"),width=7,height = 4)

#Schleife um Linien der besten Modellläufe der unterschiedlich MC-simulationen 
#zu den Messungen dazu zu plotten
for(i in (1:length(loadfiles_undist))[-grep("fit_Ca",loadfiles_undist)]){
  #Den jeweiligen Datensatz auswählen
  #Die Datensätze mit den modellierten Werten werden mittels assign() von mc_out 
  #in dei Global Env gelegt
  data<-get(loadfiles_undist[i])
  #Spalte mit runname hinzufügen
  data$run<-runname[i]
  
  #Modellierte Ganglinien zu den Plots hinzufügen
  co2plt<-co2plt+geom_line(data=subset(data,tiefe%in%c(-2,-6,-10,-14)),aes(date,CO2_mod,col=run))
  bfplt<-bfplt+geom_line(data=subset(data,tiefe%in%c(-2,-6,-10,-14)),aes(date,theta_mod,col=run))
  qplt<-qplt+geom_line(data=subset(data,tiefe==-17),aes(date,q_mod,col=run))
}

#Co2 plot exportieren
co2plt+
  geom_rect(data=event2,aes(xmin=start,xmax=stop,ymin = -Inf, ymax = Inf,fill=""), alpha = 0.15)+
  facet_wrap(~as.factor(-tiefe),labeller = as_labeller(name_tiefe),ncol = 1,scales = "free")+
  labs(title="ungestörte Probe",x="",y=expression("CO"[2]*" [ppm]"),col="mod",linetype="obs")+
  theme_classic()+
  scale_fill_manual(name="Beregnung",values="blue")+guides(color = guide_legend(order=2),linetype = guide_legend(order=1),fill = guide_legend(order=3))+
  ggsave(paste0(plotpfad,"co2_mod_undist.pdf"),width=7,height = 9)

#theta plot exportieren
bfplt+
  geom_rect(data=event2,aes(xmin=start,xmax=stop,ymin = -Inf, ymax = Inf,fill=""), alpha = 0.15)+
  facet_wrap(~as.factor(-tiefe),labeller = as_labeller(name_tiefe),ncol = 1,scales = "free")+
  labs(title="ungestörte Probe",x="",y=expression(theta*" [cm"^3/cm^3*"]"),col="mod",linetype="obs")+
  theme_classic()+
  scale_fill_manual(name="Beregnung",values="blue")+guides(color = guide_legend(order=2),linetype = guide_legend(order=1),fill = guide_legend(order=3))+
  ggsave(paste0(plotpfad,"bf_mod_undist.pdf"),width=7,height = 6)

#q plot exportieren
qplt+
  geom_rect(data=event1,aes(xmin=start,xmax=stop,ymin = -Inf, ymax = Inf,fill=""), alpha = 0.15)+
  labs(title="ungestörte Probe",x="",y=expression("q"*" [ml / min]"),col="mod",linetype="obs * 5")+
  theme_classic()+
  scale_fill_manual(name="Beregnung",values="blue")+guides(color = guide_legend(order=2),linetype = guide_legend(order=1),fill = guide_legend(order=3))+
  ggsave(paste0(plotpfad,"q_mod_undist.pdf"),width=7,height = 3)

#######################
#modelläufe zusammmen dist
########################

#runname wird für die Legende im Plot verwendet
runname<-gsub("mc_55000-dist_|_"," ",loadfiles2)

#events laden
events<-event()
#event1 als alle events die im Zeitraum des Trainingsdatensatzes (all_s) liegen
event1<-subset(events,start>=min(alldist_s$date)&stop<=max(alldist_s$date))
event2<-data.frame(start=rep(event1$start,4),stop=rep(event1$stop,4),tiefe=rep(c(-2,-6,-10,-14),each=nrow(event1)))

maindata<-subset(get(loadfiles_dist[1]),tiefe%in%c(-2,-6,-10,-14))

#Messwerte plotten
co2plt<-ggplot(data=maindata)+geom_line(aes(date,CO2_raw,linetype=""),col=1)
bfplt<-ggplot(data=maindata)+geom_line(aes(date,theta,linetype=""),col=1)
qplt<-ggplot(data=subset(get(loadfiles_dist[1]),tiefe==-17))+geom_line(aes(date,q_interpol*5,linetype=""),col=1)

#Schleife um Modellläufe zu Plots hinzuzufügen
for(i in 1:length(loadfiles_dist)){
  data<-get(loadfiles_dist[i])
  data$run<-runname[i]
  co2plt<-co2plt+geom_line(data=subset(data,tiefe%in%c(-2,-6,-10,-14)),aes(date,CO2_mod,col=run))
  bfplt<-bfplt+geom_line(data=subset(data,tiefe%in%c(-2,-6,-10,-14)),aes(date,theta_mod,col=run))
  qplt<-qplt+geom_line(data=subset(data,tiefe==-17),aes(date,q_mod,col=run))
}

#CO2 plot exportieren
co2plt+
  geom_rect(data=event2,aes(xmin=start,xmax=stop,ymin = -Inf, ymax = Inf,fill=""), alpha = 0.15)+
  facet_wrap(~as.factor(-tiefe),labeller = as_labeller(name_tiefe),ncol = 1,scales = "free")+
  labs(title="gestörte Probe",x="",y=expression("CO"[2]*" [ppm]"),col="mod",linetype="obs")+
  theme_classic()+
  scale_fill_manual(name="Beregnung",values="blue")+guides(color = guide_legend(order=2),linetype = guide_legend(order=1),fill = guide_legend(order=3))+
  ggsave(paste0(plotpfad,"co2_mod_dist.pdf"),width=7,height = 9)

#theta plot exportieren
bfplt+
  geom_rect(data=event2,aes(xmin=start,xmax=stop,ymin = -Inf, ymax = Inf,fill=""), alpha = 0.15)+
  facet_wrap(~as.factor(-tiefe),labeller = as_labeller(name_tiefe),ncol = 1,scales = "free")+
  labs(title="gestörte Probe",x="",y=expression(theta*" [cm"^3/cm^3*"]"),col="mod",linetype="obs")+
  theme_classic()+
  scale_fill_manual(name="Beregnung",values="blue")+guides(color = guide_legend(order=2),linetype = guide_legend(order=1),fill = guide_legend(order=3))+
  ggsave(paste0(plotpfad,"bf_mod_dist.pdf"),width=7,height = 6)

#q plot exportieren
qplt+
  geom_rect(data=event1,aes(xmin=start,xmax=stop,ymin = -Inf, ymax = Inf,fill=""), alpha = 0.15)+
  labs(title="gestörte Probe",x="",y=expression("q"*" [ml / min]"),col="mod",linetype="obs * 5")+
  theme_classic()+
  scale_fill_manual(name="Beregnung",values="blue")+guides(color = guide_legend(order=2),linetype = guide_legend(order=1),fill = guide_legend(order=3))+
  ggsave(paste0(plotpfad,"q_mod_dist.pdf"),width=7,height = 3)


#########################
#plot Modellläufe zusammen calcium
##########################

runname<-gsub("-_mc_55000-|_"," ",loadfiles_undist)

#event1 als alle events die im Zeitraum des Trainingsdatensatzes (all_s) liegen
event1<-subset(events,start>=min(all_s$date)&stop<=max(all_s$date))

#Messwerte plotten
caplt<-ggplot(data=subset(get(loadfiles_undist[1]),tiefe==-17))+geom_line(aes(date,ca_conc,linetype=""),col=1)

#Modellergebnisse zum Plot hinzufügen
for(i in (1:length(loadfiles_undist))[-grep("CO.+realistic|bo.+realistic",loadfiles_undist)]){
  data<-get(loadfiles_undist[i])
  data$run<-runname[i]
  data$date2<-data$date[data$t_min==0&data$tiefe==-17]+data$t_min*60
  caplt<-caplt+geom_line(data=subset(data,tiefe==-17),aes(date2,Ca_mod,col=run))
}

#Ca Plot exportieren
caplt+
  geom_rect(data=event1,aes(xmin=start,xmax=stop,ymin = -Inf, ymax = Inf,fill=""), alpha = 0.15)+
  labs(title="ungestörte Probe",x="",y=expression("Ca"^{2+""}*" [mg / l]"),col="mod",linetype="obs")+
  theme_classic()+
  scale_fill_manual(name="Beregnung",values="blue")+scale_y_continuous(limits = c(40,NA))+guides(color = guide_legend(order=2),linetype = guide_legend(order=1),fill = guide_legend(order=3))+
  ggsave(paste0(plotpfad,"ca_mod_undist.pdf"),width=7,height = 3.5)


######################
#EE output from Matlab function
#######################

#Mit der Matlab funktion EET_indices aus dem SAFE package
#wurde mi und sigma für den MC-Output nochmals zur überprüfung berechnet
#dazu muss das skript Use_EET.m im Ordner Matlab ausgeführt werden
#die mi und sigma Werte werden dort als .csv dateien gespeichert
#namen aller .csv dateien mit mi oder si im namen auslesen
misi<-list.files(mcpfad,pattern = "mi|si.*.csv")

#namen aller .R dateien die mit mc anfangen
loadfiles_both<-list.files(mcpfad,pattern = "^mc.+.R")

#am anfang der namen der mi oder sigma dateinamen das mi oder sigma abschneiden
mc_types<-unique(stringr::str_replace(misi,"^[a-z]+_*[a-z]+",""))
#die Endung entfernen
mc_types<-unique(stringr::str_replace(mc_types,".csv",""))
#außerdem das fit_both oder fit_CO2 am anfang des strings abschneiden
mc_types2<-stringr::str_replace(mc_types,"_-fit_([a-z]+|[A-Z]+)2*-_","0-")

#_ oder - im namen durch leerzeichen ersetzen
mc_name<-gsub("_|-"," ",mc_types)

#schleife um EE-Plots für Matlab output zu erstellen
for (j in 1:length(mc_types)){
  #für  den j-ten mc_type die enstrechenden mi und sigma files auswählen
  tempfile<-misi[grep(mc_types[j],misi)]
  
  #eine Matrix für die mi und sigma werte anlegen
  misi_val<-matrix(NA,10,length(tempfile))
  #mit einer schleife die Werte in die entsprechende Spalte der Matrix schreiben
  for (i in 1:length(tempfile)){
    #die i-te mi oder sigma .csv in die i-te spalte
    misi_val[,i]<-t(read.csv(paste0(mcpfad,tempfile[i]),header = F))
  }
  #die R Datei des MC Lauf laden
  loadfile<-loadfiles_both[grep(mc_types2[j],loadfiles_both)]
  load(file = paste0(mcpfad,loadfile))
  #die Parametermatrix aus der R Datei wird für die richtige Zuordnung 
  #der Parameternamen benötigt da in den mi & sigma dateien 
  #keine Parameternamen übergeben wurden
  par<-mc[[2]]
  
  library(stringr)
  #die Matrix der mi und sigam werte zu data.frame umwandeln
  EET_oct<-as.data.frame(misi_val)
  #als Spaltennamen die namen der .csv dateien ohne die MC-Endung die nach _- kommt
  colnames(EET_oct)<-gsub("_-.+","",tempfile)
  #als id werden die Parameternamen aus der MC.R datei übernommen
  EET_oct$id<-colnames(par)
  #als Parameter werden zahlen am ende der Parameternamen entfernt
  EET_oct$par<-str_replace(colnames(par),"2|3","")
  #die Horizontzuordnung entspricht den Zahlen nach dem Parameternamen
  mat<-str_extract(colnames(par),"2|3")
  #wo keine Zahl steht wird der horizont als 1 gesetzt
  EET_oct$Mat<-ifelse(is.na(mat),"1",mat)
  #Farben für die Parameter
  colors<-factor(EET_oct$par,labels = setNames(c(2,"purple",4:5,"green","orange"),unique(EET_oct$par)))
  #als Character
  colors<-as.character(colors)
  
  library(dplyr)
  #Formen für die Horizonte
  shapes<-factor(EET_oct$Mat,labels = setNames(c(16,17,15),unique(EET_oct$Mat)))
  #als numeric
  shapes<-as.numeric(as.character(shapes))
  #Namen der Parameter als Expression
  names<-c(expression(alpha[1],alpha[2],h[opt],K[S1],K[S2],K[S3],n[1],n[2],P[distr],P[opt]))

  print("saving GSA plot")
  
  library(ggplot2)
  #Export der Plots
  ggplot(EET_oct)+
    geom_rect(aes(xmin=mi_lb,xmax=mi_ub,ymin=sigma_lb,ymax=sigma_ub,fill=id),col=0,alpha=0.15,show.legend = F)+
    geom_point(aes(mi,sigma,col=id,shape=id),size=2)+
    theme_classic()+
    scale_shape_manual(name="Parameter",labels=names,values = shapes[order(colnames(par))])+
    scale_color_manual(name="Parameter",labels=names,values = colors[order(colnames(par))])+
    scale_fill_manual(name="",labels=names,values = colors[order(colnames(par))])+
    labs(title=mc_name[j],x=expression(S[i]),y="sigma")+
    ggsave(paste0(plotpfad,"/EE/","Matlab_",gsub("(mi_-|.csv)","",tempfile[1]),".pdf"),width=7,height=4)
}
