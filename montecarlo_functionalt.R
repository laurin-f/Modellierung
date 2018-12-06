#########################################
#Funktion für Monte carlo Runs mit Hydrus
#########################################

monte_carlo<-function(nr=100,#anzahl Modellläufe
                      #Parameter Ranges
                      ranges=data.frame(alpha=c(0.005,0.6),
                                        n=c(1.5,4),
                                        ks=c(0.01,2),
                                        alpha2=c(0.005,0.6),
                                        n2=c(1.5,4),
                                        ks2=c(0.01,2),
                                        alpha_bot=c(0.005,0.6),
                                        n_bot=c(1.2,3),
                                        ks_bot=c(0.001,0.1),
                                        p_opt=c(0.002,0.005),
                                        act_en=c(6500,7000),
                                        h_opt=c(-120,-50),
                                        h_crit=c(-10^-4,-10^-6)),
                      #Parameter die nicht variiert werden
                      fixed=data.frame(thr=0.11,
                                       ths=0.75,
                                       thr2=0.13,
                                       ths2=0.64,
                                       thr_bot=0.13,
                                       ths_bot=0.64,
                                       hseep=-100,
                                       l=0.5),
                      #pfad=projektpfad1,
                      UNSAT=T,#soll UNSATCHEM verwendet werden
                      treatm="all",#intensität oder "all"
                      sleep=1,#sleeptime für die .exe
                      #wenn T werden Paramtersätze über das Latin Hypercube Sampling gezogen
                      lhs=T,
                      #Tiefen die Benutzt werden um objective Function zu ermitteln
                      fit.tiefe=c(-2,-6,-10,-14),
                      free_drain=T){
  starttime<-Sys.time()
    #Rscript mit Hydrus Functionen ausführen
    source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/modellierung/hydrus_input.R")
  
  #projektpfad wird abhängig von UNSC == T/F gewählt
  pfad<-ifelse(UNSAT==T,"C:/Users/ThinkPad/Documents/Masterarbeit/daten/hydrus/undisturbed/","C:/Users/ThinkPad/Documents/Masterarbeit/daten/hydrus/undisturbed2/")
  
    #wenn lhs verwendet werden soll
    if(lhs==T){
      #lade nötiges Package
      library(lhs)
      #wenn nr kleiner gleich 100 ist kann optimumLHS verwendet werden
      if(nr<=100){
        lhs<-optimumLHS(n=nr,k=ncol(ranges))
        
      #bei größeren nr dauert optimumLHS zu lange und randomLHS wird verwendet
      }else{
        lhs<-randomLHS(n=nr,k=ncol(ranges))
      }
      
      #Datensatz für  die Parametersätze anlegen
      par<-as.data.frame(matrix(NA,nrow = nr,ncol = ncol(ranges)))
      #Spaltennamen von den Ranges übernehmen
      colnames(par)<-colnames(ranges)
      
      #Schleife um die lhs Parameterwerte an die Ranges anzupassen
      for (i in 1:ncol(ranges)){
        par[,i]<-ranges[1,i]+(ranges[2,i]-ranges[1,i])*lhs[,i]
      }
      
    #wenn kein lhs verwendet wird werden Parameter zufällig gezogen
    }else{
      par<-as.data.frame(apply(ranges, 2, function(x) runif(nr,x[1],x[2])))
    }
  
  #Parameter auf 3 signifikante Nachkommastellen Runden
  par<-signif(par,3)
  #file enstsprechend zu UNSC == T/F auswählen
  file<-ifelse(UNSAT==T,"undisturbed","undisturbed2")    
  
  #wenn treat ="all"
  if(treatm=="all"){
    #lade datensatz all.R
    load("C:/Users/ThinkPad/Documents/Masterarbeit/daten/all.R")
    #wird für tmax  die zeitdifferenz vom ersten zum letzten Messwert in minuten verwendet
    tmax<-as.numeric(difftime(max(all$date),min(all$date),units = "min"))
    #t_event wird dann nicht gebraucht
    t_event<-NULL
    #alle auf TRUE gesetzt
    alle<-T
    }else{#wenn treat nicht "all" ist
      #berechnung von t_event
      t_event<-round(50/treatm*60)
      #tmax wird gesetzt
      tmax<-6000
      #alle auf FALSE gesetzt
      alle<-F
    }
  
  #atmos.in funktion ausführen
  atmos.in(int=treatm,
           event=t_event,
           alle=alle,
           total_t = tmax,
           projektpfad = pfad)
  
  #profile.in funktion ausführen
  profile.in(projektpfad = pfad,Mat = c(rep(1,7),rep(2,10),3))
  
  #Vektoren für RMSE & NSE anlegen
  rmse<-rep(NA,nr)
  nse<-rep(NA,nr)
  
  #MonteCarlo Schleife mit nr durchläufen
  for (i in 1:nr){
    
    #Parametersatz i mit den fix-Parametern zusammenfügen
    pars<-cbind(par[i,],fixed)
    
    
    #selector.in funktion mit dem i-ten Parametersatz
    selector.in(params = pars,
                projektpfad = pfad,
                tmax=tmax,
                UNSC = UNSAT,
                free_drain=free_drain)
    

    #hydrus ausführen
    hydrus.exe(sleep=sleep,file = file,UNSC=UNSAT,taskkill = T)
    #kurz verschnaufen
    Sys.sleep(0.7)
    
    #output function anwenden
    out<-read_hydrus.out(treat=treatm,
                             projektpfad=pfad,
                             UNSC=UNSAT,fit.tiefe = fit.tiefe)
    
    #Objective Functions in Vektoren schreiben
    rmse[i]<-out[[2]]
    nse[i]<-out[[3]]
    
    #Fortschritt der Schleife ausgeben
    print(paste(i/nr*100,"%")) 
    print(rmse[i])
    #falls später ein Fehler auftritt speichern der Daten
    save(rmse,par,nse,file="C:/Users/ThinkPad/Documents/Masterarbeit/daten/hydrus/montecarlo/mc_temp.R") 
  }#ende Monteccarlo Schleife
  #output in Liste schreiben
  mc<-list(rmse,par,nse)
  #falls 
  if(nr>100){
  save(mc,file = paste0(mcpfad,"mc_out-nr_",nr,"-",format(Sys.time(),"%m-%d_%H.%M"),".R"))}
  
  print("calculation time:")
  print(Sys.time()-starttime)
  print(paste(length(which(!is.na(rmse)))/nr*100,"% succesfully calculated"))
  #ausgabe der Parameter & Objective Function
    return(mc)
}#Ende

#########################################
#Funktion für Monte carlo Runs mit Hydrus
#parallelisiert
#########################################

mc_parallel<-function(nr=100,#anzahl Modellläufe
                      #Parameter Ranges
                      ranges,
                      #Parameter die nicht variiert werden
                      fixed,
                      #wie oft soll das modell parallel gerechnet werden
                      n_parallel=4,
                      #pfad=projektpfad1,
                      UNSAT=T,#soll UNSATCHEM verwendet werden
                      treatm="all",#intensität oder "all"
                      sleep=5,#sleeptime für die .exe
                      #wenn T werden Paramtersätze über das Latin Hypercube Sampling gezogen
                      OAT=T,
                      #Tiefen die Benutzt werden um objective Function zu ermitteln
                      fit.tiefe=c(-2,-6,-10,-14),
                      #soll die lower Boundary free drain verwendet werden oder seepage face
                      free_drain=T,
                      #soll  die objective Function auf CO2 oder Ca gefittet werden
                      fit.calcium=F,
                      #anzahl Knoten
                      n_nodes=9,
                      #Verteilung des Bodenmaterials
                      Mat=c(rep(1,3),rep(2,5),3),
                      #anzahl Print times
                      print_times=100,
                      #maximaler zeitschritt
                      dtmax=0.1){
  starttime<-Sys.time()
  #Rscript mit Hydrus Functionen ausführen
  source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/modellierung/hydrus_input.R")
  
  #wenn lhs verwendet werden soll
  if(OAT==F){
    #lade nötiges Package
    library(lhs)
    #wenn nr kleiner gleich 100 ist kann optimumLHS verwendet werden
    if(nr<=100){
      lhs<-optimumLHS(n=nr,k=ncol(ranges))
      
      #bei größeren nr dauert optimumLHS zu lange und randomLHS wird verwendet
    }else{
      lhs<-randomLHS(n=nr,k=ncol(ranges))
    }
    
    #Datensatz für  die Parametersätze anlegen
    par<-as.data.frame(matrix(NA,nrow = nr,ncol = ncol(ranges)))
    #Spaltennamen von den Ranges übernehmen
    colnames(par)<-colnames(ranges)
    
    #Schleife um die lhs Parameterwerte an die Ranges anzupassen
    for (i in 1:ncol(ranges)){
      par[,i]<-ranges[1,i]+(ranges[2,i]-ranges[1,i])*lhs[,i]
    }
    
    #wenn kein lhs verwendet wird werden Parameter zufällig gezogen
  }else{
    M<-ncol(ranges)
    r<-round(nr/(M+1))
    nr<-r*(M+1)
    distr_par<-as.list(ranges)
    par<-SAFER::OAT_sampling(r=r,M=M,distr_fun = "unif",distr_par = distr_par,samp_strat = "lhs",des_type = "radial")
    
    par<-as.data.frame(par)
    colnames(par)<-colnames(ranges)
    
  }
  
  #Parameter auf 3 signifikante Nachkommastellen Runden
  par<-signif(par,3)
  #file enstsprechend zu UNSC == T/F auswählen
  file<-paste0("UNSC",1:n_parallel) 
  
  #wenn treat ="all"
  if(treatm=="all"){
    #lade datensatz all.R
    load("C:/Users/ThinkPad/Documents/Masterarbeit/daten/all.R")
    #wird für tmax  die zeitdifferenz vom ersten zum letzten Messwert in minuten verwendet
    tmax<-as.numeric(difftime(max(all$date),min(all$date),units = "min"))
    #t_event wird dann nicht gebraucht
    t_event<-NULL
    #alle auf TRUE gesetzt
    alle<-T
  }else{#wenn treat nicht "all" ist
    #berechnung von t_event
    t_event<-round(50/treatm*60)
    #tmax wird gesetzt
    tmax<-6000
    #alle auf FALSE gesetzt
    alle<-F
  }
  
  projektpfad<-paste0("C:/Users/ThinkPad/Documents/Masterarbeit/daten/hydrus/UNSC",1:n_parallel,"/")
  programmpfad<-paste0("C:/Users/ThinkPad/Documents/Masterarbeit/programme/Hydrus-1D_4-",1:n_parallel,"/")
  
  for (i in 1:n_parallel){
  #atmos.in funktion ausführen
  atmos.in(obs=all,
           int=treatm,
           event=t_event,
           alle=alle,
           total_t = tmax,
           projektpfad = projektpfad[i])
  
  #profile.in funktion ausführen
  profile.in(projektpfad = projektpfad[i],Mat = Mat,n_nodes = n_nodes,th=seq(0.2,0.2,len=n_nodes))
  }
  #Vektoren für RMSE & NSE anlegen
  rmse<-rep(NA,nr)
  nse<-rep(NA,nr)
  
  #MonteCarlo Schleife mit nr durchläufen
  for (i in seq(1,nr,n_parallel)){
    for (j in 1:n_parallel){
      if(nrow(par)>=(i+j-1)){
    #Parametersatz i mit den fix-Parametern zusammenfügen
    pars<-cbind(par[(i+j-1),],fixed)
    

    
    #selector.in funktion mit dem i-ten Parametersatz
    selector.in(params = pars,
                projektpfad = projektpfad[j],
                tmax=tmax,
                UNSC = UNSAT,
                free_drain=free_drain,
                print_times = print_times,
                dtmax = dtmax)
    
    
    #hydrus ausführen
    hydrus.exe(sleep=sleep,file = file[j],UNSC=UNSAT,taskkill = T,programmpfad = programmpfad[j],wait = F)
      }
    }
    #kurz verschnaufen
    Sys.sleep(sleep)
    exe_check<-shell('tasklist /FI "IMAGENAME eq H1D_UNSC.EXE"',intern = T)
    
    while(length(grep("INFORMATION",exe_check))==0){
      Sys.sleep(0.01)
      exe_check<-shell('tasklist /FI "IMAGENAME eq H1D_UNSC.EXE"',intern = T)
    }
    
    for (j in 1:n_parallel){
      if(length(rmse)>=(i+j-1)){
    #output function anwenden
      if(fit.calcium==T){
        out<-read_conc.out(projektpfad = projektpfad[j],n_nodes = n_nodes)
      }else{
        out<-read_hydrus.out(treat=treatm,
                         projektpfad=projektpfad[j],
                         UNSC=UNSAT,fit.tiefe = fit.tiefe)}
      

    
    #Objective Functions in Vektoren schreiben
    rmse[(i+j-1)]<-out[[2]]
    nse[(i+j-1)]<-out[[3]]}}
    
    exe_check<-shell('tasklist /FI "IMAGENAME eq H1D_UNSC.EXE"',intern = T)
    
    while(length(grep("INFORMATION",exe_check))==0){
      Sys.sleep(0.01)
      exe_check<-shell('tasklist /FI "IMAGENAME eq H1D_UNSC.EXE"',intern = T)
    }
    
    #Fortschritt der Schleife ausgeben
    print(paste(i/nr*100,"%")) 
    print(rmse[i:(i+3)])
    #falls später ein Fehler auftritt speichern der Daten
    save(rmse,par,nse,file="C:/Users/ThinkPad/Documents/Masterarbeit/daten/hydrus/montecarlo/mc_temp.R") 
  }#ende Monteccarlo Schleife
  #output in Liste schreiben
  mc<-list(rmse,par,nse)
  #falls 
  if(nr>100){
    filename<-paste0("mc_out-nr_",nr,"-",format(Sys.time(),"%m-%d_%H.%M"))
    save(mc,file = paste0(mcpfad,filename,".R"))
    print(paste("saved file",filename))}
  
  print("calculation time:")
  print(Sys.time()-starttime)
  print(paste(length(which(!is.na(rmse)))/nr*100,"% succesfully calculated"))
  
  #ausgabe der Parameter & Objective Function
  return(mc)
}#Ende


loadfile<-"mc_06.12"
###############################
#mc out function
#################################
mc_out<-function(fixed,
                 loadfile,
                 treat="all",
                 sleep=8,
                 ndottys=200,
                 free_drain=T,
                 fit.ca=F,
                 dtmax=10){
  mcpfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/hydrus/montecarlo/"
  plotpfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/abbildungen/plots/mc/"
  if(exists("rmse")){
  rm(rmse)}
  load(file = paste0(mcpfad,loadfile,".R"))
  if(!exists("rmse")){
  par<-mc[[2]]
  rmse<-mc[[1]]
  nse<-mc[[3]]
  }

  
  colnames(par)<-gsub("_bot","3",colnames(par))
  
  library(ggplot2)
     
   load("C:/Users/ThinkPad/Documents/Masterarbeit/daten/bodenparameter/ranges.R")
  if(fit.ca==T){
   realistic_ranges<-realistic_bulk 
  }
  
  realistic_ranges$id<-1:2
  ranges_melt<-data.table::melt(realistic_ranges,id="id")
  realistic_range<-subset(ranges_melt,id==1)
  realistic_range$max<-ranges_melt$value[ranges_melt$id==2]
  ##################################
  #export dottyplots for RMSE
  ##################################
  
  best.100<-sort(rmse)[ndottys]
  pargood<-par[rmse<best.100&!is.na(rmse),]
  rmsegood<-rmse[rmse<best.100&!is.na(rmse)]
  dotty_rmse<-cbind(rmsegood,pargood)
  
  dotty_melt<-data.table::melt(dotty_rmse,id=1)
  dotty_melt$variable<-as.character(dotty_melt$variable)
  dotty_melt<-dotty_melt[order(dotty_melt$variable),]
  
  rmse_dotty<-ggplot()+
    geom_point(data=dotty_melt,aes(value,rmsegood),size=0.5)+
    geom_point(data=subset(dotty_melt,rmsegood==min(rmsegood)),aes(value,rmsegood),col=2)
  
  if (colnames(realistic_ranges)[1]%in%colnames(par)){
    rmse_dotty<-rmse_dotty+geom_rect(data=realistic_range,aes(xmin=value,xmax=max,ymin=-Inf,ymax=Inf), alpha = 0.15,fill="green")}
  
    rmse_dotty+facet_wrap(~variable,scales = "free")+
    ggsave(paste0(plotpfad,"dottyplots/RMSE/dotty_",loadfile,".pdf"),height = 8,width = 8)
  
  
  ##################################
  #export dottyplots for NSE
  ##################################
  
  best.nse<-sort(nse,decreasing = T)[ndottys]
  pargood<-par[nse>best.nse&!is.na(nse),]
  nsegood<-nse[nse>best.nse&!is.na(nse)]
  
  dotty_nse<-cbind(nsegood,pargood)
  
  dotty_melt<-data.table::melt(dotty_nse,id=1)
  dotty_melt$variable<-as.character(dotty_melt$variable)
  dotty_melt<-dotty_melt[order(dotty_melt$variable),]
  
  nse_dotty<-ggplot()+
    geom_point(data=dotty_melt,aes(value,nsegood),size=0.5)+
    geom_point(data=subset(dotty_melt,nsegood==max(nsegood)),aes(value,nsegood),col=2)
  
  if (colnames(realistic_ranges)[1]%in%colnames(par)){
    nse_dotty<-nse_dotty+geom_rect(data=realistic_range,aes(xmin=value,xmax=max,ymin=-Inf,ymax=Inf), alpha = 0.15,fill="green")}
  
  nse_dotty+facet_wrap(~variable,scales = "free")+
    ggsave(paste0(plotpfad,"dottyplots/NSE/dotty_",loadfile,".pdf"),height = 8,width = 8)
  
  
  #######################################
  #SAFER
  #######################################
  
  X<-as.matrix(par[!is.na(rmse),])
  Y<-rmse[!is.na(rmse)]
  r<-floor(length(Y)/(ncol(par)+1))
  nr<-r*(ncol(par)+1)
  X<-X[1:nr,]
  Y<-Y[1:nr]
  range<-apply(X,2,range)
  
  DistrPar<-vector("list",ncol(par))
  for(i in 1:ncol(par)){
    DistrPar[[i]]<-range[,i]
  }
  
  # Compute Elementary Effects:
  EETind <- SAFER::EET_indices(r, DistrPar, X, Y, design_type="radial")
  
  EE <- EETind$EE
  mi <- EETind$mi
  sigma <- EETind$sigma 
  
  # Plot results in the plane (mean(EE),std(EE)):
  if(length(which(!is.na(sigma)))>0){
  par(mfrow=c(1,1))
  SAFER::EET_plot(mi, sigma,  xlab = "Mean of EEs", ylab = "Sd of EEs",  labels = colnames(par))
  }
  
  # # Use bootstrapping to derive confidence bounds:
  # 
  # Nboot <-100
  # 
  # EETind100 <- SAFER::EET_indices(r, DistrPar, X, Y, design_type="radial", Nboot)
  # 
  # EE <- EETind100$EE
  # mi <- EETind100$mi
  # sigma <- EETind100$sigma
  # mi_lb <- EETind100$mi_lb
  # mi_ub <- EETind100$mi_ub
  # sigma_lb <- EETind100$sigma_lb
  # sigma_ub <- EETind100$sigma_ub
  # 
  # # Plot bootstrapping results in the plane (mean(EE),std(EE)):
  # #EET_plot
  # 
  # 
  # EET_plot(mi, sigma, mi_lb, mi_ub, sigma_lb, sigma_ub, labels = X_labels)
  # 
  #######################################
  # export  mod vs. obs data plots
  #######################################
  
  pars<-cbind(par[which.min(rmse),],fixed)
  colnames(pars)<-gsub("_bot","3",colnames(pars))
  
  out<-hydrus(params = pars,
              UNSC=T,
              sleep = sleep,
              treat = treat,
              taskkill=F,
              free_drain=free_drain,
              print_times = 3000,
              dtmax = dtmax)

  out$tiefe<-as.numeric(out$tiefe)
  
  source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/durchf-hrung/event.R")
  events<-event()
  
  #lade datensatz all.R
  load("C:/Users/ThinkPad/Documents/Masterarbeit/daten/all.R")
  #zeitspanne in der beregnet wurde
  event<-subset(events,start>=min(all$date)&stop<=max(all$date))
  
  #die Startzeiten der einzelnen Events in Minuten nach dem ersten Event
  #+1 da kein input bei t=0 reinkann
  event$time_start<-as.numeric(difftime(event$start,event$start[1],units = "min"))+1
  #die Endzeiten der einzelnen Events in Minuten nach dem ersten Event
  event$time_stop<-as.numeric(difftime(event$stop,event$start[1],units = "min"))+1
  
  tiefenstufen<-c(-2,-6,-10,-14)
  
  ggplot()+
    geom_rect(data=event,aes(xmin=time_start,xmax=time_stop,ymin = -Inf, ymax = Inf), alpha = 0.15,fill="blue")+
    geom_point(data= subset(out,tiefe%in%tiefenstufen),aes(t_min,theta,col="obs"),na.rm = T)+
    geom_line(data= subset(out,tiefe%in%tiefenstufen),aes(t_min,theta_mod,col="mod"),na.rm = T)+
    facet_wrap(~tiefe,ncol=1)+
    theme_classic()+
    labs(x="Zeit [min]",y=expression(theta*" [Vol %]"))+
    ggsave(paste0(plotpfad,"theta/thetas_treat-",treat,"-",loadfile,".pdf"),height = 9,width = 9)
  
  ggplot()+
    geom_rect(data=event,aes(xmin=time_start,xmax=time_stop,ymin = -Inf, ymax = Inf), alpha = 0.15,fill="blue")+
    geom_point(data=subset(out,tiefe==-17),aes(t_min,q_interpol*5,col="obs"),na.rm = T)+
    geom_line(data=subset(out,tiefe==-17),aes(t_min,q_mod,col="mod"),na.rm = T)+
    theme_classic()+
    labs(x="Zeit [min]",y=expression("q [ml min"^{-1}*"]"))+ggsave(paste0(plotpfad,"q/q_treat-",treat,"-",loadfile,".pdf"),height = 5,width = 9)
  
  
  ggplot()+
    geom_rect(data=event,aes(xmin=time_start,xmax=time_stop,ymin = -Inf, ymax = Inf), alpha = 0.15,fill="blue")+
    geom_point(data=subset(out,tiefe%in%tiefenstufen),aes(t_min,CO2_raw,col="obs"),na.rm = T)+
    geom_line(data=subset(out,tiefe%in%tiefenstufen),aes(t_min,CO2_mod,col="mod"),na.rm = T)+
    facet_wrap(~tiefe,ncol=2)+
    theme_classic()+
    labs(x="Zeit [min]",y=expression("CO"[2]*" [ppm]]"))+
    ggsave(paste0(plotpfad,"co2/CO2_treat-",treat,"-",loadfile,".pdf"),height = 9,width = 9)
  
  ggplot(subset(out,tiefe==-17&!is.na(Ca_mod)))+
    geom_line(aes(t_min,Ca_mod,col="mod"))+geom_point(aes(t_min,ca_conc,col="obs"))+
    theme_classic()+
    labs(x="Zeit [min]",y=expression("Ca"^{2+""}*" [mg * l"^{-1}*"]"),color="tiefe")+
    ggsave(paste0(plotpfad,"ca/Ca_treat-",treat,"-",loadfile,".pdf"),height = 7,width = 9)
  

  #######################
  #caplot
  #berechnung der Masse gelöstem Calciums pro zeitschritt
  zeitschritt_ca<-mean(diff(out$t_min[out$tiefe==-17&!is.na(out$Ca_mod)]))
  out$ca_mg_mod<-out$Ca_mod*abs(out$q_mod)/1000*zeitschritt_ca#mg/l *l/min *min=mg
  
  ca_mg_sums<-aggregate(out[out$t_min>0,c(14)],list(out$treatment[out$t_min>0],out$tiefe[out$t_min>0]),function(x) sum(x,na.rm=T))
  q_sums<-aggregate(abs(out[!is.na(out$Ca_mod),c(10)]),list(out$treatment[!is.na(out$Ca_mod)],out$tiefe[!is.na(out$Ca_mod)]),function(x) sum(x,na.rm=T))
ca_mg_sums$Ca_ml_mod<-ca_mg_sums$x/q_sums$x*1000/zeitschritt_ca#mg/l*min/min
colnames(ca_mg_sums)<-c("treatment","tiefe","Ca_mg_mod","Ca_ml_mod")
  
    ca_means<-aggregate(out[out$t_min>0,c(2,4,13)],list(out$treatment[out$t_min>0],out$tiefe[out$t_min>0]),function(x) mean(x,na.rm=T))
  
  capath<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/ca/"
  load(file=paste0(capath,"cafm.R"))
  
  legendtitle<-expression("Intensität [mm*h"^{-1}*"]")
  ggplot()+
    geom_point(data=subset(ic,!is.na(rain_mm_h)),aes(ca,tiefe,col=as.factor(round(rain_mm_h)),shape=as.factor(round(rain_mm_h))))+
    geom_path(data=ca_means,aes(Ca_mod,tiefe,col=as.factor(treatment)),linetype="dotted")+
    geom_path(data=ca_mg_sums,aes(Ca_ml_mod,tiefe,col=as.factor(treatment)))+
    labs(x=expression("Ca"^{"2+"}*"  [mg * l"^{-1}*"]"),y="tiefe [cm]",col=legendtitle,shape=legendtitle)+theme_classic()+
    ggsave(paste0(plotpfad,"ca/Ca_tiefenprofil-",treat,"-",loadfile,".pdf"),height = 9,width = 9)
}


