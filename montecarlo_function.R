#########################################
#Funktion für Monte carlo Runs mit Hydrus
#########################################


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
  source("//FUHYS013/Freiberg/rcode/modellierung/hydrus_input.R")
  
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
    load("//FUHYS013/Freiberg/daten/all.R")
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
  
  projektpfad<-paste0("//FUHYS013/Freiberg/Hydrus/UNSC",1:n_parallel,"/")
  programmpfad<-paste0("//FUHYS013/Freiberg/programme/Hydrus-1D_4-",1:n_parallel,"/")
  
  system("taskkill /IM H1D_UNSC.EXE",show.output.on.console=F)
  
  for (i in 1:n_parallel){
  #atmos.in funktion ausführen
  atmos.in(obs=all,
           int=treatm,
           event=t_event,
           alle=alle,
           total_t = tmax,
           projektpfad = projektpfad[i])
  
  #profile.in funktion ausführen
  profile.in(projektpfad = projektpfad[i],Mat = Mat,n_nodes = n_nodes,th=seq(0.2,0.4,len=n_nodes))
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
    hydrus.exe(sleep=sleep,file = file[j],UNSC=UNSAT,taskkill = T,programmpfad = programmpfad[j],wait = T)
      }
    }
    #kurz verschnaufen
    Sys.sleep(1)
    
    
    check_CPU<-function(sleep2=sleep){
    tasklist<-system("wmic path Win32_PerfFormattedData_PerfProc_Process get Name,PercentProcessorTime",intern=T)
    tasksplit<-strsplit(tasklist[2:(length(tasklist)-1)]," \\s+")
    tasks<-do.call("rbind",tasksplit)
    startpoint<-Sys.time()
    if(length(grep("H1D_UNSC",tasks))>0){
    while(length(which(tasks[grep("H1D_UNSC",tasks),2]>0))>2&as.numeric(difftime(Sys.time(),startpoint,units = "sec"))<=sleep2){
      Sys.sleep(0.1)
      tasklist<-system("wmic path Win32_PerfFormattedData_PerfProc_Process get Name,PercentProcessorTime",intern=T)
      tasksplit<-strsplit(tasklist[2:(length(tasklist)-1)]," \\s+")
      tasks<-do.call("rbind",tasksplit)
    }}
    }
    
    check_CPU()
    system("taskkill /IM H1D_UNSC.EXE",show.output.on.console=F)
    
    fault_check<-shell('tasklist /FI "IMAGENAME eq WerFault.exe"',intern = T)
    if(length(grep("INFORMATION",fault_check))==0){
      system("taskkill /IM WerFault.exe",show.output.on.console=F)}
    exe_check<-shell('tasklist /FI "IMAGENAME eq H1D_UNSC.EXE"',intern = T)
    
    while(length(grep("INFORMATION",exe_check))==0){
      Sys.sleep(0.01)
      exe_check<-shell('tasklist /FI "IMAGENAME eq H1D_UNSC.EXE"',intern = T)
      system("taskkill /IM H1D_UNSC.EXE",show.output.on.console=F)
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
    
    ##############################################
    #langsamer werden wenn NA
    ##############################################
    
    dtmax_fac<-1
    dtmax2<-dtmax
    
    while(is.na(mean(rmse[i:(i+n_parallel-1)]))&dtmax2>=0.1){
    dtmax2<-  dtmax2/10
    print(paste("calculating",length(which(is.na(rmse[i:(i+n_parallel-1)]))),"models with dtmax =",dtmax2))
      for (j in 1:n_parallel){
      if(nrow(par)>=(i+j-1)){
        if(is.na(rmse[(i+j-1)])){
        #Parametersatz i mit den fix-Parametern zusammenfügen
        pars<-cbind(par[(i+j-1),],fixed)
        
        
        
        #selector.in funktion mit dem i-ten Parametersatz
        selector.in(params = pars,
                    projektpfad = projektpfad[j],
                    tmax=tmax,
                    UNSC = UNSAT,
                    free_drain=free_drain,
                    print_times = print_times,
                    dtmax = dtmax2)
        
        
        #hydrus ausführen
        hydrus.exe(sleep=sleep,file = file[j],UNSC=UNSAT,taskkill = T,programmpfad = programmpfad[j],wait = T)
      }
      }
    }
    #kurz verschnaufen
    #Sys.sleep(1)
    
    check_CPU(sleep = sleep+10*dtmax_fac)
    system("taskkill /IM H1D_UNSC.EXE",show.output.on.console=F)
    
    fault_check<-shell('tasklist /FI "IMAGENAME eq WerFault.exe"',intern = T)
    if(length(grep("INFORMATION",fault_check))==0){
    system("taskkill /IM WerFault.exe",show.output.on.console=F)}
    
    exe_check<-shell('tasklist /FI "IMAGENAME eq H1D_UNSC.EXE"',intern = T)
    
    while(length(grep("INFORMATION",exe_check))==0){
      Sys.sleep(0.01)
      exe_check<-shell('tasklist /FI "IMAGENAME eq H1D_UNSC.EXE"',intern = T)
      system("taskkill /IM H1D_UNSC.EXE",show.output.on.console=F)
    }
    
    for (j in 1:n_parallel){
      if(length(rmse)>=(i+j-1)){
        if(is.na(rmse[(i+j-1)])){
        #output function anwenden
        if(fit.calcium==T){
          out<-read_conc.out(projektpfad = projektpfad[j],n_nodes = n_nodes)
        }else{
          out<-read_hydrus.out(treat=treatm,
                               projektpfad=projektpfad[j],
                               UNSC=UNSAT,fit.tiefe = fit.tiefe)}
        
        #Objective Functions in Vektoren schreiben
        rmse[(i+j-1)]<-out[[2]]
        nse[(i+j-1)]<-out[[3]]}}}
    dtmax_fac<-dtmax_fac+1
    }
    ####################
    #ende langsamer
    #####################
    
    exe_check<-shell('tasklist /FI "IMAGENAME eq H1D_UNSC.EXE"',intern = T)
    
    while(length(grep("INFORMATION",exe_check))==0){
      Sys.sleep(0.01)
      exe_check<-shell('tasklist /FI "IMAGENAME eq H1D_UNSC.EXE"',intern = T)
      system("taskkill /IM H1D_UNSC.EXE",show.output.on.console=F)
    }
    
    
    
    #Fortschritt der Schleife ausgeben
    print(paste(i/nr*100,"%"))
    print(rmse[i:(i+n_parallel-1)])
    #falls später ein Fehler auftritt speichern der Daten
    save(rmse,par,nse,file="//FUHYS013/Freiberg/Hydrus/montecarlo/mc_temp.R") 
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

mc_parallel2<-function(nr=100,#anzahl Modellläufe
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
  source("//FUHYS013/Freiberg/rcode/modellierung/hydrus_input.R")
  
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
    load("//FUHYS013/Freiberg/daten/all.R")
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
  
  projektpfad<-paste0("//FUHYS013/Freiberg/Hydrus/UNSC",1:n_parallel,"/")
  programmpfad<-paste0("//FUHYS013/Freiberg/programme/Hydrus-1D_4-",1:n_parallel,"/")
  
  system("taskkill /IM H1D_UNSC.EXE",show.output.on.console=F)

  for (i in 1:n_parallel){
    #atmos.in funktion ausführen
    atmos.in(obs=all,
             int=treatm,
             event=t_event,
             alle=alle,
             total_t = tmax,
             projektpfad = projektpfad[i])
    
    #profile.in funktion ausführen
    profile.in(projektpfad = projektpfad[i],Mat = Mat,n_nodes = n_nodes,th=seq(0.2,0.4,len=n_nodes))
  }
  #Vektoren für RMSE & NSE anlegen
  rmse<-rep(NA,nr)
  nse<-rep(NA,nr)
  print("start of mc loop")
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
        hydrus.exe(sleep=sleep,file = file[j],UNSC=UNSAT,taskkill = T,programmpfad = programmpfad[j],wait = T)
      }
    }
    #kurz verschnaufen
    Sys.sleep(1)
    
    
    check_CPU<-function(sleep2=sleep){
      tasklist<-system("wmic path Win32_PerfFormattedData_PerfProc_Process get Name,PercentProcessorTime",intern=T)
      tasksplit<-strsplit(tasklist[2:(length(tasklist)-1)]," \\s+")
      tasks<-do.call("rbind",tasksplit)
      startpoint<-Sys.time()
      if(length(grep("H1D_UNSC",tasks))>0){
        while(length(which(tasks[grep("H1D_UNSC",tasks),2]>0))>2&as.numeric(difftime(Sys.time(),startpoint,units = "sec"))<=sleep2){
          Sys.sleep(0.1)
          tasklist<-system("wmic path Win32_PerfFormattedData_PerfProc_Process get Name,PercentProcessorTime",intern=T)
          tasksplit<-strsplit(tasklist[2:(length(tasklist)-1)]," \\s+")
          tasks<-do.call("rbind",tasksplit)
        }}
    }
    
    check_CPU()
    system("taskkill /IM H1D_UNSC.EXE",show.output.on.console=F)
    
    fault_check<-shell('tasklist /FI "IMAGENAME eq WerFault.exe"',intern = T)
    if(length(grep("INFORMATION",fault_check))==0){
      system("taskkill /IM WerFault.exe",show.output.on.console=F)}
    exe_check<-shell('tasklist /FI "IMAGENAME eq H1D_UNSC.EXE"',intern = T)
    
    while(length(grep("INFORMATION",exe_check))==0){
      Sys.sleep(0.01)
      exe_check<-shell('tasklist /FI "IMAGENAME eq H1D_UNSC.EXE"',intern = T)
      system("taskkill /IM H1D_UNSC.EXE",show.output.on.console=F)
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
      system("taskkill /IM H1D_UNSC.EXE",show.output.on.console=F)
    }
    
    
    
    #Fortschritt der Schleife ausgeben
    print(paste(i/nr*100,"%"))
    print(rmse[i:(i+n_parallel-1)])
    #falls später ein Fehler auftritt speichern der Daten
    save(rmse,par,nse,file="//FUHYS013/Freiberg/Hydrus/montecarlo/mc_temp.R") 
    gc()
  }#ende Monteccarlo Schleife
  
  ##############################################
  #langsamer werden wenn NA
  ##############################################
  
  dtmax_fac<-1
  dtmax2<-dtmax
  
  while(is.na(mean(rmse))&dtmax2>=0.1){
    dtmax2<-  dtmax2/10
    rmse_na<-rmse[is.na(rmse)]
    print(paste("recalculating",length(rmse_na)," NA models with dtmax =",dtmax2))
    
    nse_na<-nse[is.na(rmse)]
    
    for (i in seq(1,length(rmse_na),n_parallel)){
    for (j in 1:n_parallel){
      if(nrow(par[is.na(rmse),])>=(i+j-1)){
          #Parametersatz i mit den fix-Parametern zusammenfügen
          pars<-cbind(par[is.na(rmse),][(i+j-1),],fixed)
          
          
          
          #selector.in funktion mit dem i-ten Parametersatz
          selector.in(params = pars,
                      projektpfad = projektpfad[j],
                      tmax=tmax,
                      UNSC = UNSAT,
                      free_drain=free_drain,
                      print_times = print_times,
                      dtmax = dtmax2)
          
          
          #hydrus ausführen
          hydrus.exe(sleep=sleep,file = file[j],UNSC=UNSAT,taskkill = T,programmpfad = programmpfad[j],wait = T)
        }#ende if nrow par
      }#ende for j
      
    #kurz verschnaufen
    #Sys.sleep(1)
    
    check_CPU(sleep = sleep+10*dtmax_fac)
    system("taskkill /IM H1D_UNSC.EXE",show.output.on.console=F)
    
    fault_check<-shell('tasklist /FI "IMAGENAME eq WerFault.exe"',intern = T)
    if(length(grep("INFORMATION",fault_check))==0){
      system("taskkill /IM WerFault.exe",show.output.on.console=F)}
    
    exe_check<-shell('tasklist /FI "IMAGENAME eq H1D_UNSC.EXE"',intern = T)
    
    while(length(grep("INFORMATION",exe_check))==0){
      Sys.sleep(0.01)
      exe_check<-shell('tasklist /FI "IMAGENAME eq H1D_UNSC.EXE"',intern = T)
      system("taskkill /IM H1D_UNSC.EXE",show.output.on.console=F)
    }
    
    for (j in 1:n_parallel){
      if(length(rmse_na)>=(i+j-1)){
          #output function anwenden
          if(fit.calcium==T){
            out<-read_conc.out(projektpfad = projektpfad[j],n_nodes = n_nodes)
          }else{
            out<-read_hydrus.out(treat=treatm,
                                 projektpfad=projektpfad[j],
                                 UNSC=UNSAT,fit.tiefe = fit.tiefe)}
          
          #Objective Functions in Vektoren schreiben
          rmse_na[(i+j-1)]<-out[[2]]
          nse_na[(i+j-1)]<-out[[3]]
      }#ende if length rmse
    }#ende for j 
    #Fortschritt der Schleife ausgeben
    print(paste(i/length(rmse_na)*100,"%"))
    print(rmse_na[i:(i+n_parallel-1)])
    #falls später ein Fehler auftritt speichern der Daten
    save(rmse,par,nse,file="//FUHYS013/Freiberg/Hydrus/montecarlo/mc_temp.R") 
    }#ende monte carlo NA
    dtmax_fac<-dtmax_fac+1
    rmse[is.na(rmse)]<-rmse_na
    nse[is.na(nse)]<-nse_na
  }#ende while na
  ####################
  #ende langsamer
  #####################
  
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

###############################
#mc out function
#################################
mc_out<-function(fixed,
                 loadfile,
                 treat="all",
                 sleep=3,
                 ndottys=200,
                 free_drain=T,
                 fit.ca=F){
  mcpfad<-"//FUHYS013/Freiberg/Hydrus/montecarlo/"
  plotpfad<-"//FUHYS013/Freiberg/abbildungen/plots/mc/"
  load(file = paste0(mcpfad,loadfile,".R"))
  
  par<-mc[[2]]
  rmse<-mc[[1]]
  nse<-mc[[3]]
  
  library(ggplot2)
     
   load("//FUHYS013/Freiberg/daten/bodenparameter/ranges.R")
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
  
  par(mfrow=c(1,1))
  SAFER::EET_plot(mi, sigma,  xlab = "Mean of EEs", ylab = "Sd of EEs",  labels = colnames(par))
  
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
  
  out<-hydrus(params = pars,
              UNSC=T,
              sleep = sleep,
              treat = treat,
              taskkill=T,
              free_drain=free_drain,
              print_times = 1000)
  
  
  out$tiefe<-as.numeric(out$tiefe)
  
  source("//FUHYS013/Freiberg/rcode/durchf-hrung/event.R")
  events<-event()
  
  #lade datensatz all.R
  load("//FUHYS013/Freiberg/daten/all.R")
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
  
  ca_means<-aggregate(out[out$t_min>0,c(1:4,13)],list(out$treatment[out$t_min>0],out$tiefe[out$t_min>0]),function(x) mean(x,na.rm=T))
  #######################
  #caplot
  
  capath<-"//FUHYS013/Freiberg/daten/ca/"
  load(file=paste0(capath,"cafm.R"))
  
  legendtitle<-expression("Intensität [mm*h"^{-1}*"]")
  ggplot()+
    geom_point(data=subset(ic,!is.na(rain_mm_h)),aes(ca,tiefe,col=as.factor(round(rain_mm_h)),shape=as.factor(round(rain_mm_h))))+
    geom_path(data=ca_means,aes(Ca_mod,tiefe,col=as.factor(treatment)))+
    labs(x=expression("Ca"^{"2+"}*"  [mg * l"^{-1}*"]"),y="tiefe [cm]",col=legendtitle,shape=legendtitle)+theme_classic()+
    ggsave(paste0(plotpfad,"ca/Ca_tiefenprofil-",treat,"-",loadfile,".pdf"),height = 9,width = 9)
}

