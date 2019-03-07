
loadfile<-"mc_temp"
fixed=cbind(fixed,fixed_co2)

ndottys=10000

fit.ca=F
dtmax=1
kin_sol=F
Nboot=100
Probe="undist"
obs=all_s
rmse_pos=4
###############################
#mc out function
#Funktion um die Ergebnisse der Monte Carlo Runs zu Plotten
#################################
mc_out<-function(fixed,#fixe Parameterwerte des MC-laufs
                 loadfile,#Dateiname des MC-Laufs ihne ".R" Endung
                 ndottys=10000,#die wieviel besten Modellläufe sollen bei den Dottyplots gezeigt werden
                 dtmax=10,#maximaler zeitschritt
                 obs=all_s,#Messung mit der das Modell verglichen wird
                 Probe="undist",#wurde die gestörte oder die ungestörte Probe benutzt
                 kin_sol=F,
                 Nboot=100,#Anzahl an Bootstrapping-Läufen
                 traintime=4500,
                 plot=F,
                 Mat=c(rep(1,3),rep(2,5),3),
                 rmse_pos=1,
                 crit_perc=5
                 taskkill=F){
  
  #definieren der Pfade
  mcpfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/hydrus/montecarlo/"
  plotpfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/abbildungen/plots/mc/"
  if(exists("rmse")){
    rm(rmse,envir = .GlobalEnv)
    }
  #Output des MC-Laufs laden
  load(file = paste0(mcpfad,loadfile,".R"))
  if(exists("rmse")){
  mc<-list(rmse,par,nse,rmse_ca,rmse_both)
  }
  #if(!exists("rmse")){
  #der Output ist in die Liste mc geschrieben
  #die einzelnen listenelemente auspacken
  par<-mc[[2]]
  if(rmse_pos<6){
  rmse<-mc[[rmse_pos]]
  }
  nse<-mc[[3]]
  #}#ende if exists
  if(rmse_pos==6){
    rmse_ca<-(mc[[4]]-mean(mc[[4]],na.rm = T))/sd(mc[[4]],na.rm = T)
    rmse_co2<-(mc[[1]]-mean(mc[[1 ]],na.rm = T))/sd(mc[[1]],na.rm = T)
    rmse<-(rmse_ca+rmse_co2)/2
  }
  
  print(paste(length(which(!is.na(rmse))),"Models succesfully calculated"))
  loadfile<-paste0(c("fit_CO2-_","","","fit_Ca-_","fit_both-_","fit_both2-_")[rmse_pos],loadfile)
  runname<-gsub("-_mc_55000-|_"," ",loadfile)
  #packages laden
  library(ggplot2)
  library(stringr)
  library(dplyr)
  
  #datensatz mit realistischen Grenzwerten der Parameter laden
  load("C:/Users/ThinkPad/Documents/Masterarbeit/daten/bodenparameter/ranges.R")
  
  #wenn der Datensatz für den gestörten Boden verwendet wird 
  #werden die Parameter Grenzwerte für den gestörten Boden verwendet
  if(nrow(obs)==nrow(alldist_s)){
    realistic_ranges<-realistic_ranges_dist
  }
  
  #den Datensatz realistic_ranges ins long format bringen
  #dazu eine Spalte id anfügen damit lässt sich besser melten
  realistic_ranges$id<-1:2
  #mit melt wird der Datensatz ins long format gebracht
  ranges_melt<-data.table::melt(realistic_ranges,id="id")
  
  #Vom long format jeweils min und max werte der Parameter in extra spalten schreiben
  realistic_range<-subset(ranges_melt,id==1)
  realistic_range$max<-ranges_melt$value[ranges_melt$id==2]
  realistic_ranges<-realistic_ranges[,colnames(realistic_ranges)!="id"]
  
  #####################################
  #run the model
  #####################################
  
  #Den Besten Parametersatz des MC-Laufs auswählen
  pars<-cbind(par[which.min(rmse),],fixed)
  pars_opt<-par[which.min(rmse),][order(colnames(par))]
  assign("pars_opt",pars_opt,envir = .GlobalEnv)
  assign("colnames_par",colnames(par)[order(colnames(par))],envir = .GlobalEnv)
  #mit function das Modell ausführen und output laden
  out<-hydrus(params = pars,
              UNSC=T,
              taskkill=taskkill,
              free_drain=T,
              print_times = 2000,
              dtmax = dtmax,
              obs=obs,
              min_nrows=100,
              kin_sol=kin_sol,
              traintime=traintime,
              Mat=Mat)
  
  #tiefe von factor in numeric
  out$tiefe<-as.numeric(out$tiefe)
  if(plot==T){
  #######################################
  #SAFER für Sensitivitätsanalyse
  #######################################
  if(Nboot>0){
  #X als Matrix der Parametersätze
  X<-as.matrix(par)
  #Y als Gütemaß der Modelläufe
  Y<-rmse
  #Anzahl Startpunkte beim OAT sampling
  r<-length(Y)/(ncol(par)+1)
  
  #ranges der unterschiedlichen Parameter bestimmen
  range<-apply(X,2,range)
  
  #Liste anlegen um Parameterranges reinzuschreiben 
  DistrPar<-vector("list",ncol(X))
  
  #Schleife um Liste zu füllen
  for(i in 1:ncol(X)){
    #range des i-ten Parameters in Liste
    DistrPar[[i]]<-range[,i]
  }
  
  #mc_(anzahl Läufe)_ aus dem Dateinamen löschen um den String für  die Bennenung der Dateien wiederzuverwenden
  mc_type<-stringr::str_replace(loadfile,"mc_\\d+(_|-)","")
  
  #X, Y und range als .csv abspeichern um die EE-Funktion auch in Matlab zu testen
  #write.table(X,paste0(mcpfad,"X",mc_type,".csv"),row.names = F,col.names = F,sep=",")
  #write.table(Y,paste0(mcpfad,"Y",mc_type,".csv"),row.names = F,col.names = F,sep=",")
  #write.table(range,paste0(mcpfad,"range",mc_type,".csv"),row.names = F,col.names = F,sep=",")
  
  #wenn kein Bootstrapping gemacht wird

    # Elementary Effects mit für NA's umgeschriebener Funktion  berechnen
    EETind <- EET_na(r=r,xrange= DistrPar, X=X, Y=Y, design_type="radial",Nboot = Nboot)
    #Output liste als Data.frame
    if(Nboot==1){
    EET<-as.data.frame(EETind[1:2])
    }else{
    EET<-as.data.frame(EETind[c(1:2,4:9)]) 
    }
    #Spalte mit Parameternamen anfügen
    EET$id<-colnames(par)
    #Spalte mit Parameternamen ohne tiefendiffernzierung
    EET$par<-str_replace(colnames(par),"2|3","")
    #Spalte nur mit der Tiefe der Parameter
    mat<-str_extract(colnames(par),"2|3")
    #überall wo keine tiefe angegeben ist wird 1 eingetragen
    EET$Mat<-ifelse(is.na(mat),"1",mat)

    #Farben für die  Parameter angeben
    colorscale<-c(2:6,"orange","darkgreen")[1:length(unique(EET$par))]
    colors<-factor(EET$par,labels = setNames(colorscale,unique(EET$par)))
    colors<-as.character(colors)
    
    #shapes für  die Tiefenstufen angeben  
    shapes<-factor(EET$Mat,labels = setNames(c(16,17,15),unique(EET$Mat)))
    shapes<-as.numeric(as.character(shapes))
    

    #Namen der Parameter als Expression
    names<-c(expression(alpha[1],alpha[2],D[a],h[opt],K[S1],K[S2],K[S3],n[1],n[2],P[distr],P[opt]))
    if(length(names)!=ncol(par)){
      names<-names[-3]
    }
    
    if(Nboot==1){    
      #Ergebnisse plotten
    print("saving GSA plot")
    ggplot(EET)+
      geom_point(aes(mi,sigma,col=id,shape=id),size=2)+
      theme_classic()+
      scale_shape_manual(name="Parameter",labels=names,values = shapes[order(colnames(par))])+
      scale_color_manual(name="Parameter",labels=names,values = colors[order(colnames(par))])+
      ggsave(paste0(plotpfad,"EE/EE_",loadfile,".pdf"),height = 9,width = 9)
    
    }else{#Falls Bootstrapping verwendet wird

    #Ergebnisse plotten
    print("saving GSA Boot plot")
    
    EET_plt<-ggplot(EET)+
      geom_rect(aes(xmin=mi_lb,xmax=mi_ub,ymin=sigma_lb,ymax=sigma_ub,fill=id),col=0,alpha=0.15,show.legend = F)+
      geom_point(aes(mi,sigma,col=id,shape=id),size=2)+
      theme_classic()+
      scale_shape_manual(name="Parameter",labels=names,values = shapes[order(colnames(par))])+
      scale_color_manual(name="Parameter",labels=names,values = colors[order(colnames(par))])+
      scale_fill_manual(name="",labels=names,values = colors[order(colnames(par))])+
      labs(title=runname,x=expression(S[i]),y="sigma")#+
      #ggsave(paste0(plotpfad,"EE/EE_boot",loadfile,".pdf"),height = 4,width = 7)
    assign(paste0(loadfile,"EET_plt"),EET_plt,envir = .GlobalEnv)
  }#Ende EE Plots
  }
    
  ##################################
  #standardabweichung als Unsicherheit der Parameter berechnen
  ######################################
  best.5perc<-order(rmse)[1:round(length(rmse)/100*crit_perc)]
  std<-apply(par[best.5perc,order(colnames(par))],2,sd)
  #die RMSE-werte in die Globale Environment speichern 
  assign("std",std,envir = .GlobalEnv)

  ##################################
  #export dottyplots for RMSE
  ##################################

  #Die besten n Modellläufe wählen
  #den n.t Besten RMSE Wert bestimmen
  best.100<-sort(rmse)[ndottys]
  #Alle Parametersätze die  Besser sind auswählen
  pargood<-par[rmse<best.100&!is.na(rmse),]
  #und alle RMSE-Werte die Besser sind auswählen
  rmsegood<-rmse[rmse<best.100&!is.na(rmse)]

  #Paramterwerte und RMSE zusammenfügen
  dotty_rmse<-cbind(rmsegood,pargood)
  
  #Vector mit Labels für den Plot
  lbls<-sort(paste(colnames(pargood),"best =",signif(pargood[which.min(rmsegood),],2)))
  lbls<-sub("Disp","Diff",lbls)
  lbls<-gsub("p_","P_",lbls)
  
  #Datensatz ins Long format bringen
  dotty_melt<-data.table::melt(dotty_rmse,id=1)
  dotty_melt$variable<-as.character(dotty_melt$variable)
  dotty_melt<-dotty_melt[order(dotty_melt$variable),]
  
  #Variablennamen mit  Labels versehen  
  named<-setNames(lbls,sort(unique(dotty_melt$variable)))  
  
  #Dottyplots erstellen  
  print("saving dotty plots")
  library(scales)


  if(length(grep("mc_55000-free",loadfile))==1){ 
    n2plt<-ggplot(dotty_rmse)+
      geom_rect(data=subset(realistic_range,variable=="n2"),aes(xmin=value,xmax=max,ymin=-Inf,ymax=Inf), alpha = 0.15,fill="green")+
      geom_point(aes(n2,rmsegood,col=p_opt),size=0.5)+
      scale_color_gradientn(colors=c("blue","yellow","red"))+
      theme_classic()+
      labs(x=expression(n[2]),y="RMSE",col=expression(P[opt]))

    
    poptplt<-ggplot(dotty_rmse)+
      geom_rect(data=subset(realistic_range,variable=="p_opt"),aes(xmin=value,xmax=max,ymin=-Inf,ymax=Inf), alpha = 0.15,fill="green")+
      geom_point(aes(p_opt,rmsegood,col=n2),size=0.5)+
      scale_color_gradientn(colors=c("blue","yellow","red"))+
      theme_classic()+
      labs(x=expression(P[opt]),y="",col=expression(n[2]))
    

    pdf(paste0(plotpfad,"dottyplots/RMSE/color_",loadfile,".pdf"),height = 3,width = 8)
    gridExtra::grid.arrange(n2plt,poptplt,ncol=2)
    dev.off()
  }
  
  if(length(grep("mc_55000-realistic",loadfile))==1){ 
    pdistrplt<-ggplot(dotty_rmse)+
      geom_point(aes(p_distr,rmsegood,col=p_opt),size=0.5)+
      scale_color_gradientn(colors=c("blue","yellow","red"))+
      theme_classic()+
      labs(x=expression(P[distr]),y="RMSE",col=expression(P[opt]))
    
    
    ks2plt<-ggplot(dotty_rmse)+
      geom_point(aes(ks2,rmsegood,col=p_distr),size=0.5)+
      scale_color_gradientn(colors=c("blue","yellow","red"))+
      theme_classic()+
      labs(x=expression(K[S2]),y="",col=expression(P[distr]))
    ks2plt
    
    
    pdf(paste0(plotpfad,"dottyplots/RMSE/color_",loadfile,".pdf"),height = 3,width = 8)
    gridExtra::grid.arrange(pdistrplt,ks2plt,ncol=2)
    dev.off()
  }
  
  
  ggplot()+
    geom_point(data=dotty_melt,aes(value,rmsegood),size=0.2)+
    geom_point(data=subset(dotty_melt,rmsegood==min(rmsegood)),aes(value,rmsegood),col=2)+
    geom_rect(data=realistic_range,aes(xmin=value,xmax=max,ymin=-Inf,ymax=Inf), alpha = 0.15,fill="green")+
    facet_wrap(~variable,scales = "free",ncol = 3,labeller = as_labeller(named))+
    theme_bw()+labs(x="Value",y="RMSE")+
    ggsave(paste0(plotpfad,"dottyplots/RMSE/dotty_",loadfile,".pdf"),height = 6,width = 6)
  

  ##################################
  #export dottyplots for RMSE  for most sensitve Parameters
  ##################################
  
  #4 Sensitivste Parameter aus EE entnehmen
  best4<-order(EET$mi,decreasing = T)[1:4]
  #die n  besten Modellläufe dieser Parameter auswählen
  best.100<-sort(rmse)[ndottys]
  pargood<-par[rmse<best.100&!is.na(rmse),best4]
  rmsegood<-rmse[rmse<best.100&!is.na(rmse)]
  dotty_rmse<-cbind(rmsegood,pargood)
  
  #Labels erstellen
  #lbls<-sort(paste(colnames(pargood),"best =",signif(pargood[which.min(rmsegood),],2)))
  parnames<-names#expression(alpha[1],alpha[2],D[A],h[opt],K[S1],K[S2],K[S3],n[1],n[2],P[distr],P[opt])
  lbls<-names[pmatch(sort(colnames(pargood)),sort(colnames(par)))]
  lbls2<-names[pmatch(sort(colnames(realistic_ranges)),sort(colnames(par)))]
  #Datensatz ins long-format bringen
  dotty_melt<-data.table::melt(dotty_rmse,id=1)
  dotty_melt$variable<-as.character(dotty_melt$variable)
  dotty_melt<-dotty_melt[order(dotty_melt$variable),]
  
  #Variablen labeln
  dotty_melt$label<-factor(dotty_melt$variable,levels = sort(as.character(unique(dotty_melt$variable))),labels = lbls)
  realistic_range$label<-factor(realistic_range$variable,levels = sort(as.character(realistic_range$variable)),labels = lbls2)
  #Plotten
  print("saving dotty plots best 4")

  ggplot()+
    geom_point(data=dotty_melt,aes(value,rmsegood),size=0.5)+
    geom_point(data=subset(dotty_melt,rmsegood==min(rmsegood)),aes(value,rmsegood),col=2)+
    geom_rect(data=realistic_range[realistic_range$label%in%lbls,],aes(xmin=value,xmax=max,ymin=-Inf,ymax=Inf), alpha = 0.15,fill="green")+
    facet_wrap(~label,scales = "free",ncol = 2,labeller = label_parsed)+
    theme_bw()+
    labs(x="Value",y="RMSE")+
    ggsave(paste0(plotpfad,"dottyplots/RMSE/best4_",loadfile,".pdf"),height = 4,width = 6)
  
  ##################################
  #export dottyplots for NSE
  ##################################
  
  #Analog zum RMSE wird dassselbe mit NSE gemacht
  #nur dass hier der höchste Wert der Beste ist
  best.nse<-sort(nse,decreasing = T)[ndottys]
  pargood<-par[nse>best.nse&!is.na(nse),]
  nsegood<-nse[nse>best.nse&!is.na(nse)]
  
  dotty_nse<-cbind(nsegood,pargood)
  
  #long-format
  dotty_melt<-data.table::melt(dotty_nse,id=1)
  dotty_melt$variable<-as.character(dotty_melt$variable)
  dotty_melt<-dotty_melt[order(dotty_melt$variable),]
  
  #plotten
  nse_dotty<-ggplot()+
    geom_point(data=dotty_melt,aes(value,nsegood),size=0.5)+
    geom_point(data=subset(dotty_melt,nsegood==max(nsegood)),aes(value,nsegood),col=2)+
    geom_rect(data=realistic_range,aes(xmin=value,xmax=max,ymin=-Inf,ymax=Inf), alpha = 0.15,fill="green")+
    facet_wrap(~variable,scales = "free",labeller = as_labeller(named))+
    ggsave(paste0(plotpfad,"dottyplots/NSE/dotty_",loadfile,".pdf"),height = 8,width = 10)
  
  

  #######################################
  # export  mod vs. obs data plots
  #######################################

  #Event funktion script ausführen
  source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/durchf-hrung/event.R")
  #events laden
  events<-event()
  
  #lade datensatz all.R
  load("C:/Users/ThinkPad/Documents/Masterarbeit/daten/all.R")
  #zeitspanne in der beregnet wurde
  event<-subset(events,start>=min(obs$date)&stop<=max(obs$date))
  
  #die Startzeiten der einzelnen Events in Minuten nach dem ersten Event
  #+1 da kein input bei t=0 reinkann
  event$time_start<-as.numeric(difftime(event$start,event$start[1],units = "min"))+1
  #die Endzeiten der einzelnen Events in Minuten nach dem ersten Event
  event$time_stop<-as.numeric(difftime(event$stop,event$start[1],units = "min"))+1
  
  #Vektor mit tiefenstufen
  tiefenstufen<-c(-2,-6,-10,-14)

  #Zeitreihen plots
  print("saving timeline plots")
  #thetaplot
  ggplot()+
    geom_rect(data=event,aes(xmin=time_start,xmax=time_stop,ymin = -Inf, ymax = Inf), alpha = 0.15,fill="blue")+
    geom_line(data= subset(out,tiefe%in%tiefenstufen),aes(t_min,theta,col="obs"),na.rm = T)+
    geom_line(data= subset(out,tiefe%in%tiefenstufen),aes(t_min,theta_mod,col="mod"),na.rm = T)+
    facet_wrap(~tiefe,ncol=1,scales = "free")+
    theme_classic()+
    labs(x="Zeit [min]",y=expression(theta*" [Vol %]"))+
    ggsave(paste0(plotpfad,"theta/",ifelse(kin_sol==T,"kinsol-",""),loadfile,".pdf"),height = 9,width = 9)
  
  #q plot
  ggplot()+
    geom_rect(data=event,aes(xmin=time_start,xmax=time_stop,ymin = -Inf, ymax = Inf), alpha = 0.15,fill="blue")+
    geom_line(data=subset(out,tiefe==-17),aes(t_min,q_interpol*5,col="obs"),na.rm = T)+
    geom_line(data=subset(out,tiefe==-17),aes(t_min,q_mod,col="mod"),na.rm = T)+
    theme_classic()+
    labs(x="Zeit [min]",y=expression("q [ml min"^{-1}*"]"))+ggsave(paste0(plotpfad,"q/",ifelse(kin_sol==T,"kinsol-",""),loadfile,".pdf"),height = 5,width = 9)
  
  #Co2 plot
  ggplot()+
    geom_rect(data=event,aes(xmin=time_start,xmax=time_stop,ymin = -Inf, ymax = Inf), alpha = 0.15,fill="blue")+
    geom_line(data=subset(out,tiefe%in%tiefenstufen),aes(t_min,CO2_raw,col="obs"),na.rm = T)+
    geom_line(data=subset(out,tiefe%in%tiefenstufen),aes(t_min,CO2_mod,col="mod"),na.rm = T)+
    facet_wrap(~tiefe,ncol=2,scales = "free")+
    theme_classic()+
    labs(x="Zeit [min]",y=expression("CO"[2]*" [ppm]"))+
    ggsave(paste0(plotpfad,"co2/",ifelse(kin_sol==T,"kinsol-",""),loadfile,".pdf"),height = 9,width = 9)
  
  #ca zeitreihen plot 
  ggplot(subset(out,tiefe==-17&!is.na(Ca_mod)))+
    geom_line(aes(t_min,Ca_mod,col="mod"))+geom_line(aes(t_min,ca_conc,col="obs"))+
    theme_classic()+
    labs(x="Zeit [min]",y=expression("Ca"^{2+""}*" [mg / l]"),color="Tiefe")+
    ggsave(paste0(plotpfad,"ca/",ifelse(kin_sol==T,"kinsol-",""),loadfile,".pdf"),height = 7,width = 9)
  
  
  #######################
  #caplot tiefenprofil
  #berechnung der Masse gelöstem Calciums pro zeitschritt
  out$ca_mg_mod<-out$Ca_mod*out$q_mod/1000*10#mg/l *l/min *min=mg
  
  #da das erste Event nur Warm-Up Period ist wird das zweite Event ausgewählt
  #dabei wird der Zweite wert gewählt bei dem Regen beginnt
  event2<-out$t_min[!is.na(out$rain_mm_h)][which(diff(out$rain_mm_h[!is.na(out$rain_mm_h)])>0)[2]]

  #subset des outputs ohne warm-up-event
  out2<-subset(out,t_min>=event2)
  #die calcium summen der unterschiedlichen Tiefenstufen je nach intensität bestimmen
  ca_mg_sums<-aggregate(out2$ca_mg_mod,list(out2$treatment,out2$tiefe),function(x) sum(x,na.rm=T))
  #dasselbe für den Abfluss
  q_sums<-aggregate(out2$q_mod,list(out2$treatment,out2$tiefe),function(x) sum(x,na.rm=T))
  
  #die  Ca-Konz. über die Menge Calcium durch den Abfluss berechnen
  ca_mg_sums$Ca_ml_mod<-ca_mg_sums$x/q_sums$x*1000/10#mg/l*min/min
  #spaltennamen definieren
  colnames(ca_mg_sums)<-c("treatment","tiefe","Ca_mg_mod","Ca_ml_mod")
  #in tiefe 0 entspricht die Ca-Konzentration dem niederschlagswasser
  ca_mg_sums$Ca_ml_mod[ca_mg_sums$tiefe==0]<-2.17#mg/l
  
  #gemessene Ca-Konz. Werte laden
  capath<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/ca/"
  load(file=paste0(capath,"cafm.R"))
  #nur die Werte der gestörten (oder ungestörten) Probe benutzen
  ic<-subset(ic,sample==Probe)
  #Sortieren
  ic<-ic[order(ic$treatment),]
  
  #Mittelwerte nach tiefen und treatment aggregierne
  icmean<-aggregate(ic[2:7],list(ic$treatment,ic$tiefe),mean)
  
  #Labels für Plot 
  names<-paste("Intensit\xe4t =",unique(ic$treatment),"mm/h")
  names[2:length(unique(ic$treatment))]<-paste(unique(ic$treatment)[2:length(unique(ic$treatment))],"mm/h")
  named<-setNames(names,unique(ic$treatment))

  # #plot erstellen
  # ca_tiefenplot<-ggplot()+
  #   geom_path(data=icmean[icmean$treatment%in%unique(ca_mg_sums$treatment),],aes(ca,tiefe,col="mean (obs)",linetype="mean (obs)"))+
  #   geom_point(data=subset(ic,!is.na(rain_mm_h)&treatment%in%unique(ca_mg_sums$treatment)),aes(ca,tiefe,shape="obs"))+
  #   geom_path(data=ca_mg_sums,aes(Ca_ml_mod,tiefe,col="mod",linetype="mod"))+labs(x=expression("Ca"^{"2+"}*"  [mg / l]"),y="Tiefe [cm]",col="",shape="",linetype="")+
  #   scale_linetype_manual(name="",values=2:1)+
  #   scale_color_manual(name="",values=1:2)+
  #   theme_bw()+
  #   facet_wrap(~treatment,labeller = as_labeller(named))
  
  #plot erstellen
  ca_tiefenplot<-ggplot()+
    geom_point(data=subset(ic,!is.na(rain_mm_h)&treatment%in%unique(ca_mg_sums$treatment)),aes(ca,tiefe,col=as.factor(treatment),shape="obs"))+
    geom_path(data=ca_mg_sums,aes(Ca_ml_mod,tiefe,col=as.factor(treatment),linetype="mod"))+labs(x=expression("Ca"^{"2+"}*"  [mg / l]"),y="Tiefe [cm]",col="",shape="",linetype="")+
    theme_bw()
  
  #plot speichern
  pdf(paste0(plotpfad,"ca_tiefenprofil/",ifelse(kin_sol==T,"kinsol-",""),loadfile,".pdf"),height = 6,width = 9)
  print(ca_tiefenplot)
  dev.off()
  
  #######################
  #CaCO3 verwitterungs plot
  
  #Ca_weather zu summen jeder Intensität und tiefenstufe aggregieren
  ca_we_sums<-aggregate(out2$Ca_weather,list(out2$treatment,out2$tiefe),function(x) sum(x,na.rm=T))
  
  #spaltennamen definieren
  colnames(ca_we_sums)<-c("treatment","tiefe","ca_we")
  #plot erstellen
  ca_we_tiefenplot<-ggplot()+
    geom_path(data=ca_we_sums,aes(ca_we,tiefe,col=as.factor(treatment)))+labs(x=expression("Ca"^{"2+"}*"  [mg / l]"),y="Tiefe [cm]",col="",shape="",linetype="")+
    theme_bw()
  
  #plot speichern
  pdf(paste0(plotpfad,"SI/ca_we",ifelse(kin_sol==T,"kinsol-",""),loadfile,".pdf"),height = 6,width = 9)
  print(ca_we_tiefenplot)
  dev.off()
  
  ######################
  #SI-tiefenprofil
  
  #SI plot nur wenn kinetic Solution verwendet wird da SI sonst immer 0 ist
  if(kin_sol==T){
  
  #zu normierung nach q SI mal q berechnen 
  out2$SI_q<-out2$SI*out2$q_mod#mg/l *l/min *min=mg
  
  #mean von SI_q der unterschiedlichen Tiefenstufen je nach intensität bestimmen
  SI_sums<-aggregate(data.frame(SI_q=out2$SI_q,SI=out2$SI),list(treatment=out2$treatment,tiefe=out2$tiefe),function(x) mean(x,na.rm=T))
  
  #die mittelwerte von q analog bestimmen
  q_means<-aggregate(out2$q_mod,list(treatment=out2$treatment,tiefe=out2$tiefe),function(x) mean(x,na.rm=T))
  
  #durch die q means teilen um wieder in den Wertebreich von SI zu kommen
  SI_sums$SI_q<-SI_sums$SI_q/q_means$x#mg/l*min/min
  
  #plot erstellen
  SI_tiefenplot<-ggplot()+
    geom_path(data=SI_sums,aes(SI_q,tiefe,col=as.factor(treatment),linetype="SI_q"))+geom_path(data=SI_sums,aes(SI,tiefe,col=as.factor(treatment),linetype="SI"))+labs(x=expression("SI  []"),y="Tiefe [cm]",col="",shape="",linetype="")+
    theme_bw()
  
  #plot speichern
  pdf(paste0(plotpfad,"SI/kinsol-",loadfile,".pdf"),height = 6,width = 9)
  print(SI_tiefenplot)
  dev.off()
  }#ende SI tiefenprofil
  }#ende plot schleife
  #den Modelloutput mit namen der geladenen MC-Datei in die global environment schreiben
  assign(loadfile,out,envir = .GlobalEnv)
  print(paste(loadfile,"best RMSE:",min(rmse,na.rm = T)))
  print(paste(loadfile,"best NSE:",max(nse,na.rm = T)))
}#ende 

