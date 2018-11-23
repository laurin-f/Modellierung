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
                      free_drain=F){
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
  profile.in(projektpfad = pfad,Mat = c(rep(1,10),rep(2,7),3))
  
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
  save(mc,file = paste0(mcpfad,"mc_out-nr:",nr,"-",format(Sys.time(),"%m-%d_%H:%M"),".R"))}
  
  #ausgabe der Parameter & Objective Function
    return(mc)
}#Ende


mc_out<-function(fixed,
                 loadfile,
                 treat="all",
                 sleep=3){
  
  load(file = paste0(mcpfad,loadfile,".R"))
  
  par<-mc[[2]]
  rmse<-mc[[1]]
  #nse<-mc[[3]]
  
  pars<-cbind(par[which.min(rmse),],fixed)
  
  out<-hydrus(params = pars,
              UNSC=T,
              sleep = sleep,
              treat = treat,
              taskkill=T)[[1]]
  
  
  ggplot(subset(out,tiefe%in%tiefenstufen))+geom_point(aes(t_min,theta,col="obs"),na.rm = T)+geom_line(aes(t_min,theta_mod,col="mod"),na.rm = T)+facet_wrap(~tiefe,ncol=1)+theme_classic()+labs(x="zeit [min]",y=expression(theta*" [Vol %]"))+ggsave(paste0(plotpfad,"theta/thetas_treat-",treat,"-",loadfile,".pdf"))
  
  ggplot(subset(out,tiefe==-17))+geom_point(aes(t_min,q_interpol*5,col="obs"),na.rm = T)+geom_line(aes(t_min,q_mod,col="mod"),na.rm = T)+theme_classic()+labs(x="zeit [min]",y=expression("q [ml min"^{-1}*"]"))+ggsave(paste0(plotpfad,"q/q_treat-",treat,"-",loadfile,".pdf"))
  
  
  ggplot(subset(out,tiefe%in%tiefenstufen))+geom_point(aes(t_min,CO2_raw,col="obs"),na.rm = T)+geom_line(aes(t_min,CO2_mod,col="mod"),na.rm = T)+facet_wrap(~tiefe,ncol=2)+theme_classic()+labs(x="zeit [min]",y=expression("CO"[2]*" [ppm]]"))+ggsave(paste0(plotpfad,"co2/CO2_treat-",treat,"-",loadfile,".pdf"))
  
}
