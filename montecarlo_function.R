#########################################
#Funktion für Monte carlo Runs mit Hydrus
#########################################


#########################################
#Funktion für Monte carlo Runs mit Hydrus
#parallelisiert
#########################################


mc_parallel2<-function(nr=100,#anzahl Modellläufe
                      #Parameter Ranges
                      ranges,
                      #Parameter die nicht variiert werden
                      fixed,
                      #wie oft soll das modell parallel gerechnet werden
                      n_parallel=20,
                      #pfad=projektpfad1,
                      UNSAT=T,#soll UNSATCHEM verwendet werden
                      sleep=5,#sleeptime für die .exe
                      #wenn T werden Paramtersätze über das Latin Hypercube Sampling gezogen
                      #Tiefen die Benutzt werden um objective Function zu ermitteln
                      fit.tiefe=c(-2,-6,-10,-14),
                      #soll die lower Boundary free drain verwendet werden oder seepage face
                      free_drain=T,
                      #soll  die objective Function auch für Ca gefittet werden
                      fit.calcium=T,
                      #anzahl Knoten
                      n_nodes=9,
                      #Verteilung des Bodenmaterials
                      Mat=c(rep(1,3),rep(2,5),3),
                      #anzahl Print times
                      print_times=100,
                      #maximaler zeitschritt
                      dtmax=10,
                      #länge der Warmup-Periode
                      traintime=4500,
                      #soll kinetci solution verwendet werden?
                      kin_sol=T,
                      #sollen die nicht konvergierten Modellruns mit niedrigerem dtmax erneut berechnet werden?
                      recalc=T,
                      #minimal akzeptierte länge des modelloutputs die akzeptiert wird
                      #wenn das modell nach einer weile abgebrochen hat wird sonst der RMSE berechnet obwohl 
                      #möglicherweise nicht alle Intensitäten representiert werden
                      min_nrows=2500,
                      #welche messungen werden als referenz verwendet
                      obs=all_s){
  #startzeit wird gespeichert um später die gesamtzeit des MC-laufs ausgeben zu können
  starttime<-Sys.time()
  #Rscript mit Hydrus Functionen ausführen
  source("//FUHYS013/Freiberg/rcode/modellierung/hydrus_input.R")
  

    
    #M als anzahl Parameter
    M<-ncol(ranges)
    #für  EE Funktion muss nr  =  r*(M+1) also wird nr angepasst
    r<-round(nr/(M+1))
    nr<-r*(M+1)
    #für OAT function müssen parameterranges als Liste vorliegen
    distr_par<-as.list(ranges)
    #Paramatersätze mit  OAT-sampling und lhs ziehen 
    par<-SAFER::OAT_sampling(r=r,M=M,distr_fun = "unif",distr_par = distr_par,samp_strat = "lhs",des_type = "radial")
    
    #Parametersätze als data.frame und mit Parameternamen die übergeben wurden
    par<-as.data.frame(par)
    colnames(par)<-colnames(ranges)

  #lade datensatz all.R
  load("//FUHYS013/Freiberg/daten/all.R")
  #für tmax wird  die zeitdifferenz vom ersten zum letzten Messwert in minuten verwendet
  tmax<-as.numeric(difftime(max(obs$date),min(obs$date),units = "min"))

    
  #Vektoren für die parallelisiert aufgerufenen Hydrus-Ordner und dateinamen
  file<-paste0("UNSC",1:n_parallel) 
  projektpfad<-paste0("//FUHYS013/Freiberg/Hydrus/UNSC",1:n_parallel,"/")
  programmpfad<-paste0("//FUHYS013/Freiberg/programme/Hydrus-1D_4-",1:n_parallel,"/")
  
  #falls Hydrus gerade noch ausgeführt wird dier es jetzt gestoppt, 
  #da sonst die input dateien nicht bearbeitet werden können
  system("taskkill /IM H1D_UNSC.EXE",show.output.on.console=F)
  Sys.sleep(2)
  #schleife um inputs für alle parallelisierten Ordner zu schreiben
  for (i in 1:n_parallel){
    #atmos.in funktion ausführen
    atmos.in(obs=obs,
             total_t = tmax,
             projektpfad = projektpfad[i],mainpath ="//FUHYS013/Freiberg/")
    
    #profile.in funktion ausführen
    profile.in(projektpfad = projektpfad[i],Mat = Mat,n_nodes = n_nodes,th=seq(0.2,0.4,len=n_nodes))
  }
  #Vektoren für RMSE & NSE anlegen
  rmse<-rep(NA,nr)
  rmse_ca<-rep(NA,nr)
  rmse_both<-rep(NA,nr)
  nse<-rep(NA,nr)
  print("start of mc loop")
  #MonteCarlo Schleife mit nr durchläufen in n_parallel abständen,  
  #da bei jedem i n_parallel modellläufe gerechnet werden
  for (i in seq(1,nr,n_parallel)){
    
    #zweite schleife für parallelisierung
    for (j in 1:n_parallel){
      #falls die anzahl MC-Läufe nicht durch n_parallel teilbar ist 
      #werden die überschüssigen parallelisierten Läufe nicht durchgeführt
      if(nrow(par)>=(i+j-1)){
        
        #Parametersatz i+j-1 mit den fix-Parametern zusammenfügen
        pars<-cbind(par[(i+j-1),],fixed)
        
        #selector.in funktion mit dem i-ten Parametersatz
        selector.in(params = pars,
                    projektpfad = projektpfad[j],
                    tmax=tmax,
                    UNSC = UNSAT,
                    free_drain=free_drain,
                    print_times = print_times,
                    dtmax = dtmax,
                    kin_sol = kin_sol)
        
        
        #hydrus ausführen
        hydrus.exe(file = file[j],UNSC=UNSAT,hide_hydrus = T,programmpfad = programmpfad[j],wait = T,scriptpath = "//FUHYS013/Freiberg/Hydrus/")
      }
    }
    #kurz verschnaufen
    Sys.sleep(1)
    
    #interne Function um CPU der Prozesse abzufragen
    #dabei gibt "sleep" die maximal Rechenzeit an die hydrus gegeben wird 
    check_CPU<-function(sleep2=sleep){
      #cmd-line abfrage für liste aller tasks mit CPU angabe
      tasklist<-system("wmic path Win32_PerfFormattedData_PerfProc_Process get Name,PercentProcessorTime",intern=T)
      #diese liste an stellen mit mindestens zwei leerzeichen zerschneiden
      tasksplit<-strsplit(tasklist[2:(length(tasklist)-1)]," \\s+")
      #listenelemnte aneinander hängen
      tasks<-do.call("rbind",tasksplit)
      #aktuelle  Uhrzeit speichern
      startpoint<-Sys.time()
      #wenn in der taskliste H1D_UNSC vorkommt, also Hydrus gerade ausgeführt wird...
      if(length(grep("H1D_UNSC",tasks))>0){
        #...while schleife starten in der abgefragt wird ob Hydrus noch mehr als 2 mal eine CPU über 0 braucht
        #und ob die schleife schon länger läuft als die maximal erlaubte zeit
        while(length(which(tasks[grep("H1D_UNSC",tasks),2]>0))>2&as.numeric(difftime(Sys.time(),startpoint,units = "sec"))<=sleep2){
          #kurz verschnaufen
          Sys.sleep(0.1)
          #taskliste aktualisieren
          tasklist<-system("wmic path Win32_PerfFormattedData_PerfProc_Process get Name,PercentProcessorTime",intern=T)
          tasksplit<-strsplit(tasklist[2:(length(tasklist)-1)]," \\s+")
          tasks<-do.call("rbind",tasksplit)
        }#ende while schleife
        print(difftime(Sys.time(),startpoint,units = "sec"))
        }#ende if-schleife
    }#ende check CPU-funktion
    
    #CPU checken
    check_CPU()
    #wenn CPU auf 0 ist oder zeit überschritten ist das modell beenden
    system("taskkill /IM H1D_UNSC.EXE",show.output.on.console=F)
    
    #da immer wieder ein fehlerfenster auftritt das man wegklicken muss
    #wird hier geprüft ob das fenster schon wieder da ist
    fault_check<-shell('tasklist /FI "IMAGENAME eq WerFault.exe"',intern = T)
    #wenn es da ist...
    if(length(grep("INFORMATION",fault_check))==0){
      #wird es einfach geschlossen
      system("taskkill /IM WerFault.exe",show.output.on.console=F)}
    
    #eventuell nicht mehr nötig aber redundanz schadet nie
    #also nochmal checken ob hydrus noch offen ist, da sonst function abbricht
    exe_check<-shell('tasklist /FI "IMAGENAME eq H1D_UNSC.EXE"',intern = T)
    
    #wenn es offen ist dann wird es jetzt geschlossen
    while(length(grep("INFORMATION",exe_check))==0){
      Sys.sleep(0.01)
      exe_check<-shell('tasklist /FI "IMAGENAME eq H1D_UNSC.EXE"',intern = T)
      system("taskkill /IM H1D_UNSC.EXE",show.output.on.console=F)
    }
    
    #parallelisierte Schleife um in allen hydrus ordnern die outputs einzulesen und den modelfit zu berechnen 
    for (j in 1:n_parallel){
      #falls die anzahl MC-Läufe nicht durch n_parallel teilbar ist 
      #werden die überschüssigen parallelisierten Läufe nicht durchgeführt
      if(length(rmse)>=(i+j-1)){
        

          #die calcium und die co2 output function anwenden
        if(fit.calcium==T){
          outca<-read_conc.out(projektpfad = projektpfad[j],obs=obs,min_nrows=min_nrows)
        }else{
          outca<-list(NA,NA,NA,NA)
          }
          out<-read_hydrus.out(projektpfad=projektpfad[j],
                               UNSC=UNSAT,fit.tiefe = fit.tiefe,
                               traintime=traintime,min_nrows=min_nrows,obs=obs)
          #mit den outputs wird der normierte RMSE berechnet
          ca_vals<-outca[[1]]
          co2_vals<-out[[1]]
          nse[(i+j-1)]<-out[[3]]
          #checken ob die outputs nicht NAs sind
          if(!is.na(out[[2]])&!is.na(outca[[2]])){
            #RMSE norm durch teilen durch sd berechnen und dann mittel wischen co2 und calcium RMSE bilden
          rmse_both[(i+j-1)]<-(out[[2]]/sd(co2_vals$CO2_raw,na.rm = T)+outca[[2]]/sd(ca_vals$ca_conc,na.rm = T))/2
          }#ende if schleife

        #Objective Functions in Vektoren schreiben
        rmse[(i+j-1)]<-out[[2]]
        rmse_ca[(i+j-1)]<-outca[[2]]
        nse[(i+j-1)]<-out[[3]]
      }#ende length rmse
      }#ende j-parallel
    
    #Fortschritt der Schleife ausgeben
    print(paste(i/nr*100,"%"))
    #RMSE werte der parallelen modelruns ausgeben
    print(rmse[i:(i+n_parallel-1)])
    #falls später ein Fehler auftritt speichern der Daten
    mc<-list(rmse,par,nse,rmse_ca,rmse_both)
    save(mc,file="//FUHYS013/Freiberg/Hydrus/montecarlo/mc_temp.R") 
    #müllabfuhr
    gc()
  }#ende Monteccarlo Schleife
  
  ##############################################
  #langsamer werden wenn NA
  ##############################################
  
  #faktor um den die Wartezeit bei check CPU verlängert wird
  sleep_fac<-1
  #neuer dtmax
  dtmax2<-dtmax
  #faktor um den dtmax verringert wird
  dtmax_fac<-1
  
  #while schleife in der abgefragt wird ob
  #NAs in RMSE vorkommen
  #dtmax2 größer gleich 0.1 ist
  #und ob dtmax überhaupt verkleinert werden soll (recalc=T)
  while(is.na(mean(rmse))&dtmax2>=0.1&recalc==T){
    #der faktor fac wird erhöht
    dtmax_fac<-10*dtmax_fac
    #dtmax2 wird durch fac geteilt
    dtmax2<-  dtmax2/dtmax_fac
    #alle NAs werden aus RMSE-vektor ausgeschnitten
    rmse_na<-rmse[is.na(rmse)]
    rmse_ca_na<-rmse_ca[is.na(rmse)]
    rmse_both_na<-rmse_both[is.na(rmse)]
    
    #ausgabe wie viele NAs nochmal berechnet werden
    print(paste("recalculating",length(rmse_na)," NA models with dtmax =",dtmax2))
    
    #alle NAs werden aus NSE-vektor ausgeschnitten
    nse_na<-nse[is.na(rmse)]
    
    #monte-carlo schlefe schleife nochmal über NAs laufen lassen
    for (i in seq(1,length(rmse_na),n_parallel)){
      #parallelisierungsschleife
    for (j in 1:n_parallel){
      #falls die anzahl MC-Läufe nicht durch n_parallel teilbar ist 
      #werden die überschüssigen parallelisierten Läufe nicht durchgeführt
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
          hydrus.exe(file = file[j],UNSC=UNSAT,hide_hydrus = T,programmpfad = programmpfad[j],wait = T,scriptpath = "//FUHYS013/Freiberg/Hydrus/")
        }#ende if nrow par
      }#ende for j
    Sys.sleep(1)
    #CPU checken
    check_CPU(sleep = sleep+15*sleep_fac)
    #wenn modelle fertig sind oder zeit überschritten ist wird Hydrus geschlossen
    system("taskkill /IM H1D_UNSC.EXE",show.output.on.console=F)
    
    #schauen ob die Fehlermeldung da ist
    fault_check<-shell('tasklist /FI "IMAGENAME eq WerFault.exe"',intern = T)
    #wenn ja die meldung schließen
    if(length(grep("INFORMATION",fault_check))==0){
      system("taskkill /IM WerFault.exe",show.output.on.console=F)}
    
    #nochmal schauen ob hydrus wirklich zu ist
    exe_check<-shell('tasklist /FI "IMAGENAME eq H1D_UNSC.EXE"',intern = T)
    #wenn nein hydrus schließen
    while(length(grep("INFORMATION",exe_check))==0){
      Sys.sleep(0.01)
      exe_check<-shell('tasklist /FI "IMAGENAME eq H1D_UNSC.EXE"',intern = T)
      system("taskkill /IM H1D_UNSC.EXE",show.output.on.console=F)
    }
    
    #schleife um modellfit zu berechnen
    for (j in 1:n_parallel){
      if(length(rmse_na)>=(i+j-1)){

          #co2 und calcium-werte mittels funktion einlesen 
        if(fit.calcium==T){
          outca<-read_conc.out(projektpfad = projektpfad[j],obs=obs,min_nrows=min_nrows)
      }else{
        outca<-list(NA,NA,NA,NA)
      }
          out<-read_hydrus.out(projektpfad=projektpfad[j],
                               UNSC=UNSAT,fit.tiefe = fit.tiefe,
                               traintime=traintime,min_nrows=min_nrows,obs=obs)
          #RMSE norm berechnen
          ca_vals<-outca[[1]]
          co2_vals<-out[[1]]
          nse_na[(i+j-1)]<-out[[3]]
          #schauen ob der RMSE der über die funktionen berechnet wurde kein NA ist
          if(!is.na(out[[2]])&!is.na(outca[[2]])){
            #RMSE norm als mittel von CO2 und CA fit
            rmse_both_na[(i+j-1)]<-(out[[2]]/sd(co2_vals$CO2_raw,na.rm = T)+outca[[2]]/sd(ca_vals$ca_conc,na.rm = T))/2
            }#ende if
          
          #Objective Functions in Vektoren schreiben
          rmse_na[(i+j-1)]<-out[[2]]
          rmse_ca_na[(i+j-1)]<-outca[[2]]
          nse_na[(i+j-1)]<-out[[3]]
      }#ende if length rmse
    }#ende for j 
    #Fortschritt der Schleife ausgeben
    print(paste(i/length(rmse_na)*100,"%"))
    #RMSE werte ausgeben
    print(rmse_na[i:(i+n_parallel-1)])
    #falls später ein Fehler auftritt speichern der Daten
    mc<-list(rmse,par,nse,rmse_ca,rmse_both)
    save(mc,file="//FUHYS013/Freiberg/Hydrus/montecarlo/mc_temp.R") 
    }#ende monte carlo NA
    
    #sleep_fac um 2 erhöhen
    sleep_fac<-sleep_fac+2
    #die neu berechneten RMSE werte in die stellen des RMSE-vektors schreiben an denen NAs waren
    rmse[is.na(rmse)]<-rmse_na
    rmse_ca[is.na(rmse)]<-rmse_ca_na
    rmse_both[is.na(rmse)]<-rmse_both_na
    #dasselbe für NSE
    nse[is.na(nse)]<-nse_na
  }#ende while NA schleife
  ####################
  #ende langsamer
  #####################
  
  #output in Liste schreiben
  mc<-list(rmse,par,nse,rmse_ca,rmse_both)
  #falls mehr als 100 modellläufe gemacht wurden
  if(nr>100){
    #eine datei mit uhrzeit und Datum im namen speichern um überschreiben zu verhindern
    filename<-paste0("mc_",nr,"-",format(Sys.time(),"%m-%d_%H.%M"))
    save(mc,file = paste0(mcpfad,filename,".R"))
    #ausgeben welche datei gespeichert wurde
    print(paste("saved file",filename))}
  
  #ausgeben wielange der MC-lauf insgesamt gedauert hat
  print("calculation time:")
  print(Sys.time()-starttime)
  #ausgeben wieviel prozent  nicht NAs  waren
  print(paste(length(which(!is.na(rmse)))/nr*100,"% succesfully calculated"))
  
  #ausgabe der Parameter & Objective Function
  return(mc)
}#Ende



