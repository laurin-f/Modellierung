
#############################
#function to execute hydrus

hydrus.exe<-function(file="undisturbed",#auf welche datei soll hydrus zugreifen
                     #pfad zum powershell script in dem auf die .exe zugegriffen wird
                     scriptpath="C:/Users/ThinkPad/Documents/Masterarbeit/daten/hydrus/",
                     #pause zwischen programm starten und schließen
                     sleep=3,
                     #wenn UNSC=T wird das modul UNSATCHEM zur modellierung von CO2 
                     #und Hauptionen verwendet
                     UNSC=T,
                     #wenn taskkill=T dann wird das fenster von hydrus nicht geöffnet 
                     #und das modell wird nach der gesetzten sleeptime abgebrochen, 
                     #wichtig für Monte Carlo
                     taskkill=F,  
                     #pfade definieren
                     #hydruspfad="C:/Users/ThinkPad/Documents/Masterarbeit/daten/hydrus/",
                     programmpfad="C:/Users/ThinkPad/Documents/Masterarbeit/programme/Hydrus-1D_4/",
                     #wenn Inverse=T wird die inverse Parameterschätzung verwendet 
                     #funktioniert nicht so gut)
                     Inverse=F,
                     wait=T){


  
  #wenn die Hydrus.exe von extern ausgeführt wird verwendet si die datei die in der Datei
  #LEVEL_01.dir steht
  
  #Level_01.dir einlesen
  level_01<-readLines(paste0(scriptpath,"Level_01.dir"))
  #filename reinschreiben
  level_01<-sub("file",file,level_01)
  #geänderte Datei schreiben
  writeLines(level_01,paste0(programmpfad,"Level_01.dir"))
  
  #wenn taskkill==T wird das Powershellscript mit dem eingebauten taskkill befehl verwendet
  if(taskkill==T){
    #.txt mit vorlage für Powershell-Code lesen
    script<-readLines(paste0(scriptpath,"hydrus_exe.txt"))  
    #im Powershell-Code wird die gewünschte sleeptime eingefügt
    script<-sub("secs",sleep,script) 
  }else{
    #wenn kein taskkill gewünscht dann wird die .txt ohne taskkill genommen
    script<-readLines(paste0(scriptpath,"hydrus_exe2.txt"))
  }

  #im Powershell-Code wird die gewünschte Pfad eingefügt
  script<-sub("programmpfad",gsub("/","\\\\\\\\",programmpfad),script)
  #wenn kein UNSATCHEM verwendet werden soll...
  if(UNSC==F){
    #wird im Code UNSC durch CALC ersetzt 
    #da die .exe dateien von Hydrus UNSC.EXE und CALC.EXE heißen
    script<-sub("UNSC","CALC",script)}
  if(Inverse==T){
    #für Inverse Solution wird CALC durch CLCI ersetzt (CALCInverse)
    script<-sub("CALC","CLCI",script)
  }
  
  scriptname<-ifelse(UNSC==T,"hydrus_UNSC_exe.ps1","hydrus_CALC_exe.ps1")
  #der Powershell-Code wird in einer scriptdatei .ps1 gespeichert
  #writeLines(script,paste0(scriptpath,scriptname))
  writeLines(script,paste0(programmpfad,scriptname))
  
  #über  shell wird der Commandline übergeben, dass powershell das script ausführen soll
  #-executionpoloicy bypass wird verwendet, 
  #da im powershell im default keine scripte ausführen darf
  shell(paste0("powershell.exe -noprofile -executionpolicy bypass -file ",programmpfad,scriptname),wait=wait)}#ende function


######################################
#function to set atmospheric input

atmos.in<-function(int=0,#Niederschlagsintensität in mm/h
                   event=0,#länge des Events in min 
                   total_t=4000,#zu simulierender Gesamtzeitraum in min
                   alle=F,#wenn alle = T dann werden alle Events verwendet
                   obs=all,#messungen um eventzeitraum festzulegen
                   projektpfad=projektpfad1){#pfad zum hydrus projekt
  #wenn alle ==T...
  if(alle==T){
    #einlesen der von hydrus erstellten Armospären Input Datei
    lines<-readLines(paste0(projektpfad,"ATMOSPH.IN"))
    
    #den Anfang der Datei ausschneiden
    head<-lines[1:grep("tAtm",lines)]
    
    #das Ende der Datei ausschneiden
    tail<-lines[grep("END OF INPUT",lines)]
    
    ########################
    #events aus datensatz extrahieren
    #t_min als Zeit nach Event 1 in Minuten
    event1<-min(which(obs$rain_mm_h>0))
    obs$t_min<-as.numeric(difftime(obs$date,obs$date[event1],units = "min"))
    
    starts<-which(diff(obs$rain_mm_h[!is.na(obs$rain_mm_h)])!=0)

    time<-obs$t_min[!is.na(obs$rain_mm_h)][sort(c(starts,starts+1))]+2
    int<-obs$rain_mm_h[!is.na(obs$rain_mm_h)][sort(c(starts,starts+1))]
    # #Intensitäten der Events /600 um von mm/h in cm/min umzurechnen
    int<-int/600
    
    #endzeitpunkt mit niederschlag=0 anhängen
    time<-c(time,total_t)
    int<-c(int,0)

    
    #Verdunstung wird aus der .csv mit den Messungen ermittelt und als konstant angenommen
    evaps<-read.csv("C:/Users/ThinkPad/Documents/Masterarbeit/daten/events/verdunstung.csv",sep=";")
    #radius des Messbechers der für die Messung verwendet wurde
    radius<-9/2
    #der Verdunstung ausgesetzte Fläche
    area<-pi*radius^2
    #berechnung des verdunsteten Wassers pro minute
    evap<-mean((evaps$vorher-evaps$nachher)/evaps$t_min)#ml/min = cm3/min
    #Verdunstung auf die fläche normen
    evap<-evap/area#cm/min

    #die Werte für den Input in einen Vektor mit Nullen für andere Paramter 
    #die in Hydrus möglich sind aber hier nicht verwendet werden
    vals<-paste("  ",format(time,width=7,digits=2),format(int,width=7,digits=2),format(evap,width = 7,digits = 2),"           0      100000           0           0           0           0           0           0           0           0 ")
    
    #im Kopf der Input Datei wird die Anzahl der übergebenen Werte eingetragen 
    head[grep("MaxAL",head)+1]<-format(length(time),width = 7)
    
    #Anfang Werte und Ende des Inputs werden wieder zusammengefügt
    lines<-c(head,vals,tail)
    
    }else{#wenn alle==F...
  
  #die übergeben intensität in cm/min umrechnen
  int_cm_min<-int/10/60
  
  #die Vorlage für die Input datei wird eingelesen
  lines<-readLines("C:/Users/ThinkPad/Documents/Masterarbeit/daten/hydrus/ATMOSPHtemp.IN")
  #an der Stelle event wird die Länge des Events eingetragen
  lines<-sub("event",event,lines)
  #stop ist eine minute nach dem Event
  lines<-sub("stop",event+1,lines)
  #der Gesamtzeitraum wird eingefügt
  lines<-sub("total_t",total_t,lines)
  #die Intensität wird übergeben
  lines<-gsub("int",int_cm_min,lines)}#ende if else Schleife "alle"
  
  #die Inputdatei wird in den Ordner der Projektes geschrieben
  writeLines(lines,paste0(projektpfad,"ATMOSPH.IN"))}

######################################
#function to set Soil Parameters
#######################################

selector.in<-function(params,#Boden parameter als data.frame mit Parameternamen als colname
                      UNSC=T,#soll UNSATCHEM verwendet werden?
                      projektpfad=projektpfad1,
                      tmax=4000,#Gesamtzeitraum
                      print_times=100,#anzahl an ausgaben der Calcium conc.
                      #soll als lower Boundary Free Drain oder Seepage Face verwendet werden
                      free_drain=F,
                      kin_sol=F,
                      dtmin=0.0001,#kleinster Zeitschritt
                      dtmax=10){#größter Zeitschritt
  
  #die Selector.IN datei wird eingelesen
  lines<-readLines(paste0(projektpfad,"SELECTOR.IN"))
  
  #####################################
  #Wasser Parameter
  #####################################
  
  #die Paramter der ersten schicht werden zusammengefügt
  vals<-paste(params$thr,params$ths,params$alpha,params$n,params$ks,params$l)
  
  #falls keine Parameter für die zweite schicht angegeben wurden entspricht diese der ersten
  if(length(params$ths2)==0){
    vals2<-vals
  }else{#sonst...
  #die Paramter von Schicht 2 werden zusammgefügt
  vals2<-paste(params$thr2,params$ths2,params$alpha2,params$n2,params$ks2,params$l)
  }
  #die Paramter von Schicht 3 werden zusammgefügt
  vals3<-paste(params$thr3,params$ths3,params$alpha2,params$n2,params$ks3,params$l)
  
  #Position der Bodenphydikalischen Paramter im Input suchen
  soil_param_pos<-(grep("thr",lines)+1)
  
  #Parameter der 3 Materialien einfügen
  lines[soil_param_pos]<-vals[1]
  lines[soil_param_pos+1]<-vals2[1]
  lines[grep("thr",lines)+3]<-vals3[1] 
  
  ################################
  #set CO2 parameter
  ################################
  
  #wenn UNSATCHEM verwendet wird
  if(UNSC==T){
    #position der CO2 paramter in der Inputdatei werden bestimmt
    co2_pos1<-grep("GamS0",lines)+1
    #CO2 Paramter Optimale Produktionsrate wird angepasst
    lines[co2_pos1]<-paste("   0.000  ",params$p_opt,"      1250           0")
    
    #weitere CO2 Produktions-Paramter  werden übergeben
    co2_pos2<-grep("P0c        P50c",lines)+1
    lines[co2_pos2]<-paste("     6014        ",params$act_en,"       0.14        ",params$michaelis,"        ",params$h_opt,params$h_crit,"           3        1000")
    
    #CO2-Transport Papramter werden übergeben 
    co2_pos3<-grep("DispA",lines)+1
    lines[co2_pos3:(co2_pos3+2)]<-paste(" ",params$DispA," ",params$DispW," ",params$Disper)
    
    co2_pos4<-grep(" Alpha",lines)+1
    lines[co2_pos4]<-format(params$p_distr,width = 13)

  #####################################
  #Solute Parameter
  #####################################
    #Werte für Solute Parameter in Vektor
    #Gapon exchange constants aus Alterra 2003 für Löss: 
    #K[Ca/Mg]=0.27; K[Ca/Na]=-0.6; K[ca/K]=-1.6
    ca_vals<-paste(" ",params$bulk,params$difuz,params$disperl,params$cec,params$calcit," 0  0  0.27  -0.6  -1.6")
    ca_vals2<-paste(" ",params$bulk2,params$difuz,params$disperl,params$cec,params$calcit," 0  0  0.27  -0.6  -1.6")
    
    #Position der Solute Parameter im Input finden
    ca_pos1<-grep("Bulk.d.",lines)+1
    #Werte einsetzen
    lines[ca_pos1]<-ca_vals
    lines[(ca_pos1+1):(ca_pos1+2)]<-ca_vals2
    
    #Calcite Adsorbiert und Precipitated werte in input
    ca_pos2<-grep("Calcite      Gypsum",lines)
    lines[(ca_pos2-1)]<-paste(" ",params$CaAds," 0 0 0")
    lines[(ca_pos2+1)]<-paste(" ",params$CaPrec," 0 0 0 0 0")
    
    }#ende if UNSATCHEM 
  #####################################
  #lower boundary
  #####################################
  
  #wenn free drain verwendet werden soll
  if(free_drain==T){
    #in der datei ein t an die stelle die für free drain steht
    lines[grep("hSeep",lines)+1]<-" f     f     t     f     -1      f   0"
  }else{
    #sonst ein t bei hSeep und der Wert des hSeep parameters
  lines[grep("hSeep",lines)+1]<-paste0(" f     f     f     t     -1      f   ",params$hseep[1])
  }#ende if free_drain
    
  #den Gesamtzeitraum der Simulation in die nötige Stelle schreiben
  lines[grep(" tMax",lines)+1]<-paste("          0       ",tmax)
  
  #########################################
  #kinetic Solution
  #########################################
  #wenn kinetic solution verwendet wird kommt ein "t" an die entsprechende stelle im input
  lines[grep("lRate",lines)+1]<-ifelse(kin_sol==T," t      f         0         1"," f      f         0         1")
  
  #########################################
  #print times
  #########################################
  
  #print times, dtmin & dtmax an die nötige Stelle
  lines[grep("MPL",lines)+1]<-paste("      0.001         ",dtmin,"          ",dtmax,"     1.3     0.7     3     7    ",print_times)
  
  #sequenz mit der Länge der Print times vom anfang bis zum ende des Gesamtzeitraums
  p_seq<-round(seq(tmax/print_times,tmax,tmax/print_times))
  #auf zehner runden damit am ende alles in zehnerschritten zusammenpasst
  p_seq[1:(length(p_seq)-1)]<-round(p_seq[1:(length(p_seq)-1)]/10)*10
  #ans ende noch ein paar Leerzeichen
  p_seq<-c(p_seq,rep(" ",5))
  #Vektor für die Printtimes im nötigen Format für den Input
  prints<-1:round(print_times/6)
  j<-1#counter
  #schleife um jeweils 6 printtimes zusammenzufügen
  for(i in seq(1,print_times,6)){
    #die sechserpackete an die j-te stelle des prints Vektors
  prints[j]<-paste("",p_seq[i:(i+5)],collapse = " ")
  #j wird bei jedem durchlauf um eins erhöht
  j<-j+1}
  
  #Input vor den print_times als  head
  head<-lines[1:grep("TPrint",lines)]
  
  #Input nach den Printtimes als tail
  if(length(grep("CARBON",lines))==0){
    #wenn kein CARBON vorhanden kommt danach END
    tail<-lines[grep("END",lines):length(lines)]
  }else{
    #sonst ab CARBON bis Ende
    tail<-lines[grep("CARBON",lines):length(lines)]
  }
  
  #head printtimes und tail zusammenfügen
  lines<-c(head,prints,tail)
  
  #die Printtimes müssen auch in der HYDRUS1D.DAT datei angepasst werden
  #einlesen der Datei
  hydrus1d.dat<-readLines(paste0(projektpfad,"HYDRUS1D.DAT"))
  #Printtimes anpassen
  hydrus1d.dat[grep("PrintTimes=",hydrus1d.dat)]<-paste0("PrintTimes=",print_times)
  #schreiben der Datei
  writeLines(hydrus1d.dat,paste0(projektpfad,"HYDRUS1D.DAT"))
  
  #neue SELECTOR.IN Datei in Projektordner schreiben
  writeLines(lines,paste0(projektpfad,"SELECTOR.IN"))}


######################################
#function to set Profile Parameters
####################################

profile.in<-function(n_nodes=18,#Anzahl Knoten
                     th=seq(0.11,0.2,len=18),#initial theta werte
                     Mat=1,#Tiefenschichtung der unterschiedlichen Materialien
                     Temp=20,#Temperatur
                     Conc=0.0004,#CO2 Concentration in der Atmosphäre cm3/cm3
                     projektpfad=projektpfad1){
  #Vektor mit tiefenstufen
  tiefenstufen<-c(-2,-6,-10,-14,-17)
  #einlesen der PROFILE.DAT inputdatei
  lines<-readLines(paste0(projektpfad,"PROFILE.DAT"))
  
  if (n_nodes==9){
    vals<-paste0(format(1:n_nodes,width = 5)," ",sprintf("-%.6e",c(seq(0,14,by=2),17)),"  ",sprintf("%.6e",th)," "," "," "," ",Mat,"    1  0.000000e+00    1    1    1  ",sprintf("%.6e",Temp),"  ",sprintf("%.6e",Conc))
  }else{
  #Werte in für hydrus geeignetem Format in einen Vektor
  vals<-paste0(format(1:n_nodes,width = 5)," ",sprintf("-%.6e",seq(0,17,len=n_nodes)),"  ",sprintf("%.6e",th)," "," "," "," ",Mat,"    1  0.000000e+00    1    1    1  ",sprintf("%.6e",Temp),"  ",sprintf("%.6e",Conc))}
  
  #observation nodes in den untersuchten Tiefenstufen einfügen
  #obs_nodes<-substr(vals[substr(vals,7,19)%in%sprintf("%.6e",tiefenstufen)],3,6)
  obs_nodes<-rep(NA,length(tiefenstufen))
  for (i in 1:length(tiefenstufen)){
  obs_nodes[i]<-format(which.min(abs(seq(0,17,len=n_nodes)+tiefenstufen[i])),width=5)
  }
  #Format an hydrus anpassen
  vals<-gsub("e\\+","e+0",vals)
  vals<-gsub("e-","e-0",vals)
  
  
  #ende der Input datei enthält anzahl der Observation Nodes ...
  tail1<-format(5,width = 5)
  #und Postion der observation nodes
  tail2<-paste0(" ",paste0(" ",obs_nodes,collapse = ""))
  
  #Kopf des Inputs ausschneiden
  head<-lines[1:grep("Mat",lines)]
  #anzahl der Knoten anpassen
  head[5]<-paste0(format(n_nodes,width=5),substr(head[5],6,nchar(head[5])))
  
  #Anfang Werte und Ende zusammenfügen
  lines<-c(head,vals,tail1,tail2)
  
  #auch in der HYDRUS1D.DAT muss anzahl Nodes angepasst werden
  hydrus1d.dat<-readLines(paste0(projektpfad,"HYDRUS1D.DAT"))
  hydrus1d.dat[grep("NumberOfNodes=",hydrus1d.dat)]<-paste0("NumberOfNodes=",n_nodes)
  writeLines(hydrus1d.dat,paste0(projektpfad,"HYDRUS1D.DAT"))
 
  #Profile.dat in den Projektordner schreiben
  writeLines(lines,paste0(projektpfad,"PROFILE.DAT"))
}



######################################
#function to read Hydrus outputfile

#Nash Sutcliff Efficiency function
NSE<-function(obs,mod){
  nse<-1-sum((obs-mod)^2,na.rm = T)/sum((obs-mean(obs,na.rm=T))^2,na.rm = T)
  return(nse)
}


read_hydrus.out<-function(obs=all,#Messungen
                          treat=17,#Regenintensität oder "all"
                          projektpfad=projektpfad1,
                          UNSC=T,#wurde UNSATCHEM benutzt oder nicht
                          #Tiefen die Benutzt werden um objective Function zu ermitteln
                          fit.tiefe=c(-2,-6,-10,-14),
                          traintime=4500,
                          min_nrows=2200){
  #überprüfen ob outpufile existiert
  if(file.exists(paste0(projektpfad,"Obs_Node.out"))){
    #wenn ja spalten Zählen
    fields<-count.fields(paste0(projektpfad,"Obs_Node.out"),blank.lines.skip = F,skip=10)
    #erwartete Spaltenzahl für output mit oder ohne UNSATCHEM modul
    ncols<-ifelse(UNSC==T,21,16)
    #wenn Spaltenzahl teilweise von der erwarteten abweicht ist 
    #nrows die Reihe mit der ersten Abweichung von ncols -2
    #sonst entspricht nrow der Reihenzahl der .out datei 
    nrows<-ifelse(length(which(fields!=ncols))!=0,which(fields!=ncols)[1]-2,length(fields))
    #wenn nrows größer ist als 300...
  if(nrows>min_nrows){
    #wird die .out datei eingelesen
    obs_node<-read.table(paste0(projektpfad,"Obs_Node.out"),skip=10,nrows = nrows,header = T)
    
    #Vektor mit tiefenstufen
    tiefenstufen<-c(-2,-6,-10,-14,-17)
    #radius der Bodensäule
    r<-7.5#cm Radius
    #Fläche
    A<-pi*r^2#cm2 area
    
    #Anzahl der Variablen
    n_vars<-(ncol(obs_node)-1)/5
    #Spaltennamen mit tiefenstufen überschreiben
    colnames(obs_node)<-c("t_min",rep(tiefenstufen,each=n_vars))
    
    #position der Spalten mit thetawerten + zeitspalte
    th_pos<-c(1,seq(3,ncol(obs_node)-n_vars+2,by=n_vars))
    #thetawerte auschneiden
    theta_node<-obs_node[,th_pos]
    #mit melt thetawerte ins long-format bringen
    theta_mod<-data.table::melt(theta_node,id=1)
    
    #Position der Spalte mit Q wert
    q_pos<-th_pos[2:length(th_pos)]+1
    #Q-Werte und zeitspalte ausschneiden
    q_node<-obs_node[,c(1,q_pos)]
    #mit melt q-werte ins long-format bringen
    q_mod<-data.table::melt(q_node,id=1)
    #Spaltennamen anpassen
    colnames(q_mod)<-c("t_min","tiefe","q_mod")
    #einheit von cm3/cm2/min in cm3/min umrechnen
    q_mod$q_mod<--q_mod$q_mod*A#cm3/min
    
    #wenn UNSATCHEM verwendet wird
    if(UNSC==T){
      #Spalten in denen die CO2 werte stehen auschneiden
      CO2_node<-obs_node[,c(1,5,9,13,17,21)]
      
      #ins long-format bringen
      CO2_mod<-data.table::melt(CO2_node,id=1)
      #Einheit von cm3/cm3 in ppm umrechnen
      CO2_mod$value<-CO2_mod$value*10^6#ppm
      #modellierte CO2 und theta Werte Mergen
      vals_mod<-merge(CO2_mod,theta_mod,by=c("t_min","variable"))
      #Spaltennamen anpassen
      colnames(vals_mod)<-c("t_min","tiefe","CO2_mod","theta_mod")
    }else{#wenn kein UNSATCHEM verwendet wird werden nur theta Werte genommen
      vals_mod<-theta_mod
      #Spaltennamen anpassen
      colnames(vals_mod)<-c("t_min","tiefe","theta_mod")
    }
    #die Modellierten Werte werden auch mit den Abflusswerten zusammengeführt
    vals_mod<-merge(vals_mod,q_mod,all=T)
    
    #wenn alle Events verwendet werden
    if(treat=="all"){
      #relevante spalten des Datensatzes der Messungen werden ausgeschnitten
      sub<-obs[,c(1:3,6,11,15,17)]
      
      
      #t_min als Zeit nach Event 1 in Minuten
      event1<-min(which(sub$rain_mm_h>0))
      sub$t_min<-as.numeric(difftime(sub$date,sub$date[event1],units = "min"))
      
      
    }else{#wenn treat nicht "all" ist
      #wird die Messungen mit der gewünschten Niederschlagsintensität verwendet
      sub<-subset(obs,treatment==treat&tiefe%in%tiefenstufen)}
    
    #Zeitintervall auf 10 minuten Werte kürzen um rechenzeit zu sparen
    sub<-sub[sub$t_min%%10==0,]
    #gemessene und modellierte Werte zusammenführen
    sub<-merge(sub,vals_mod,by=c("t_min","tiefe"),all=T)
    
    #falls gewünscht können mit fit.tiefe nur gewisse tiefen 
    #für die berechnung der Objecitve Funktion verwendet werden
    sub2<-subset(sub,tiefe %in% fit.tiefe&t_min>traintime)
      

    #RootMeanSquaresError RMSE berechnen
    rmse_theta<-sqrt(mean((sub2$theta-sub2$theta_mod)^2,na.rm = T))
    rmse_q<-sqrt(mean((sub$q_interpol*5-sub$q_mod)^2,na.rm = T))
      
    #NSE berchnen
    nse_theta<-NSE(sub2$theta,sub2$theta_mod)
    nse_q<-NSE(sub2$q_interpol,sub2$q_mod)
      
    if(UNSC==T){
      #wenn UNSC==T wird der RMSE und NSE für CO2 verwendet
      rmse_CO2<-sqrt(mean((sub2$CO2_raw-sub2$CO2_mod)^2,na.rm = T))
      rmse<-rmse_CO2
      
      nse<-NSE(sub2$CO2_raw,sub2$CO2_mod)
    }else{
      #ohne UNSC wird theta verwendet
      rmse<-rmse_theta#+rmse_q
      nse<-nse_theta
      #rmse<-rmse_theta/sd(sub$theta,na.rm = T)^2+rmse_q/sd(sub$q_interpol*5,na.rm = T)^2
    }#ende if UNSC==T
    #ausgeben der modellierten Werte und der Objective Function
    return(list(sub,rmse,nse))
  }else{#wenn nrow kleiner 300
    #NA ausgeben
    return(list(NA,NA,NA))
    }
  }else{#wenn die Datei nicht existiert
    #NA ausgeben
      return(list(NA,NA,NA))
    }
  }


######################################
#function to set read Hydrus concentration outputfile

read_conc.out<-function(projektpfad=projektpfad1,
                           obs=all_s){
  #Anzahl Felder im Output zählen
  fields<-count.fields(paste0(projektpfad,"Obs_Node_Ch.out"),blank.lines.skip = F,skip=5)
  #die letzte Reihe die Eingelesen werden soll ist die letzte die 51 Elemente hat 
  #-2 weil auch eine Reihe beim Header draufgeht 
  nrows<-which(fields!=51)-2
  #Obs_Node_Ch.out datei einlesen
  obs_node_ch<-read.table(paste0(projektpfad,"Obs_Node_Ch.out"),nrows = nrows,skip=5,header = T)
  
  #nur Calcium-Werte auswählen
  Ca_vals<-obs_node_ch[,grep("Ca|time",colnames(obs_node_ch))]
  #Spaltennamen als Tiefenstufe
  colnames(Ca_vals)<-c("t_min",-2,-6,-10,-14,-17)
  #daten ins long-format melten
  vals<-data.table::melt(Ca_vals,id=1,value.name="Ca_meq",variable.name="tiefe")
  
  #nur Alkalinity-Werte auswählen
  Alk_vals<-obs_node_ch[,grep("Alk|time",colnames(obs_node_ch))]
  #Spaltennamen als Tiefenstufe
  colnames(Alk_vals)<-c("t_min",-2,-6,-10,-14,-17)
  #daten ins long-format melten
  vals2<-data.table::melt(Alk_vals,id=1,value.name="Alk_mod",variable.name="tiefe")
  vals<-merge(vals,vals2)
  
  #Einheit anpassen
  #Molare Masse Calcium
  Ca_g_pro_mol<-40.1#g/mol
  #Ladung Calcium
  Ca_z<-2
  #Einheit von meq/l in mg/l
  vals$Ca_mod<-vals$Ca_meq*Ca_g_pro_mol/Ca_z#meq/l*g/mol/z  ->mg/l
  
  #t_min als Zeit nach Event 1 in Minuten
  event1<-min(which(obs$rain_mm_h>0))
  obs$t_min<-as.numeric(difftime(obs$date,obs$date[event1],units = "min"))
  
  ############################
  #messungen mit modellierten Werte zusammenführen
  #nur Werte mit identischem t_min
  sub<-subset(obs,t_min%in%vals$t_min)
  #relevante Spalten auswählen
  sub<-sub[,c(1:2,13:14,16:17)]
  #Ca-Konz. unter 30 sind durch nicht vollständig gefüllte LF-Kammer entstanden
  sub$ca_conc[sub$ca_conc<=30]<-NA
  
  #obs und mod mergen
  merged<-merge(sub,vals,all=T)
  #reihenfolge nach t_min
  out<-merged[order(merged$t_min),]
  #RMSE bestimmen
  RMSE<-sqrt(mean((out$ca_conc-out$Ca_mod)^2,na.rm = T))
  #NSE bestimmen
  nse<-NSE(obs=out$ca_conc,out$Ca_mod)
  #ausgabe der Werte
  return(list(out,RMSE,nse))
  }#Ende

#Funktion um CO2 Produktion CO2 fluss sowiw  CaCO3 verwitterung und IAP vom Modell-Output einzulesen
read_Nod_inf.out<-function(projektpfad=projektpfad1,
                           n_nodes=9,
                           obs=all_s){#anzahl Knoten
 
  
  #Bestimmen bis zu welcher zeile in der CO2_inf.out datei werte stehen
  #da in der letzteb zeile der Datei "END" steht wird nur bis zu einer Zeile davor eingelesen 
  #sonst beschwert sich read.csv
  nrows<-which(count.fields(paste0(projektpfad,"CO2_inf.out"),skip=4)!=12)-1
  #die Spaltennamen seperat einlesen da dazwischen noch eine Zeile mit einheiten steht
  colnames<-read.table(paste0(projektpfad,"CO2_inf.out"),skip=1,nrows = 1,stringsAsFactors = F)
  #Time in t_min umbenennen
  colnames[1]<-"t_min"
  #CO2_inf.out einlesen
  CO2_inf<-read.table(paste0(projektpfad,"CO2_inf.out"),skip=4,nrows = nrows,col.names = colnames)
  #hier werden nur die Spalten mit der Zeit dem CO2_Fluss und Produktion an der Oberflläche benötigt
  CO2_inf<-CO2_inf[,c(1:2,9)]
  #Tiefe ist an der Oberfläche 0
  CO2_inf$tiefe<-0
  
  #Nod_Inf.out einlesen
  lines<-readLines(paste0(projektpfad,"Nod_Inf.out"))
  #Zeitpunkte der Messungen aus der Datei entnehmen
  time<-as.numeric(substr(lines[grep("Time:",lines)[-1]],7,19))
  
  #Equil.out datei einlesen
  Equil.out<-readLines(paste0(projektpfad,"Equil.out"))
  #Zeitpunkte der Messungen aus der Datei entnehmen
  time2<-as.numeric(substr(Equil.out[grep("Time:",Equil.out)],7,19))
  
  #solid.out einlesen
  solid.out<-readLines(paste0(projektpfad,"solid.out"))
  #Zeitpunkte der Messungen aus der Datei entnehmen
  time3<-as.numeric(substr(solid.out[grep("Time:",solid.out)],7,19))
  
  #alle haben gleiche Printtimes
  #trotzdem überprüfen
  which(time-time2!=0)
  which(time-time3!=0)
  #passt
  
  #da in den Dateien zwischen jeder Printtime unterbrechungen mit extra header in der Datei sind
  #werden nur diejenigen Zeilen ausgewählt die die richtige Anzahl Zeichen haben
  pos_vals<-which(nchar(lines)==nchar(lines[(grep("Node",lines)+3)[1]]))
  pos_vals2<-which(nchar(Equil.out)==nchar(Equil.out[(grep("aHCO3",Equil.out)+3)[1]]))
  pos_vals3<-which(nchar(solid.out)==nchar(solid.out[(grep("Node",solid.out)+2)[1]]))
  
  #Die position der gewünschten Variablen im string suchen
  #im header überprüfen ob der Variablenname passt
  substr(Equil.out[pos_vals2-3][1],87,92)
  #Werte aus dem String ausschneiden
  pIAPc<-as.numeric(substr(Equil.out[pos_vals2],87,92))
  
  #dassselbe für die gewünschten anderen Variablen
  substr(Equil.out[pos_vals2-3][1],55,62)
  pH<-as.numeric(substr(Equil.out[pos_vals2],58,61))
  
  substr(solid.out[pos_vals3-2][1],17,24)
  Ca_solid<-as.numeric(substr(solid.out[pos_vals3],17,24))

  substr(solid.out[pos_vals3-2][1],70,80)
  Ca_surf<-as.numeric(substr(solid.out[pos_vals3],71,78))
    
  tiefe<-na.omit(as.numeric(substr(lines[pos_vals],8,10)))
  
  chars<-nchar(lines[pos_vals])[1]
  P_mod<-na.omit(as.numeric(substr(lines[pos_vals],chars-9,chars)))
  
  #im Modell wird pIAPc also der negativ dekadische logarithmus des IAP ausgegeben
  #pIAP  zu IAP umrechnen
  IAP<-10^(-pIAPc)
  #Sättigungsindex berechnen
  #das Sättigungsprodukt von calcium beträgt 10^-845
  SI<-log10(IAP/10^-8.453)
  #ein paar ausreißer die  zu NAs 
  SI[SI>2]<-NA
  #die Variablen in einen Dataframe zusammenfügen
  #da jede variable für jeden zeitschritt in 9 tiefen ausgegeben wird 
  #wird jeder zeitschritt 9-mal in den Datensatz geschrieben
  vals<-data.frame(t_min=rep(time,each=9),tiefe,P_mod,pH,SI,Ca_solid,Ca_surf)
  #Spalte für CaCO3 verwitterungsrate anlegen
  vals$Ca_weather<-NA
  #Schleife um CaCO3 Verwitterungrate für  jede tiefe pro zeitschritt zu bestimmen
  for(i in unique(tiefe)){
    #CaCO3 Verwitterungrate entspricht der veränderung des Calcitgehalts pro Zeitschritt 
    vals$Ca_weather[vals$tiefe==i]<-c(0,diff(vals$Ca_solid[vals$tiefe==i])/diff(vals$t_min[vals$tiefe==i]))
  }#ende schleife
  
  #Spalte  P_4 und P_2 als summe der Produktion unterhalb -4 cm bzw. -2 cm
  #wird nur zum leichteren gemeinsamen plot in tiefe 0 geschrieben
  vals$P_4<-NA
  vals$P_4[vals$tiefe==0]<-tapply(vals$P_mod[vals$tiefe<=-4],vals$t_min[vals$tiefe<=-4],sum)

  vals$P_2<-NA
  vals$P_2[vals$tiefe==0]<-tapply(vals$P_mod[vals$tiefe<=-2],vals$t_min[vals$tiefe<=-2],sum)

  #Spalte P_0 müsste vProd aus CO2_inf.out entsprechen
  #weicht jedoch etwas ab, da nur produktion einzelner tiefenstufen summiert 
  #wird und nicht das integral über die gesamte tiefe genommen werden kann
  vals$P_0<-NA
  vals$P_0[vals$tiefe==0]<-tapply(vals$P_mod[vals$tiefe<=0],vals$t_min[vals$tiefe<=0],sum)

  #Werte mit CO2_inf.out werten mergen
  vals<-merge(vals,CO2_inf,all=T)
  #P_0 P_2 und P_4 auf vProd werte normieren um die abweichung des absoluten werts zu kompensieren
  vals$P_0_korr<-vals$P_0+max(vals$vProd,na.rm = T)-max(vals$P_0,na.rm = T)
  vals$P_2_korr<-vals$P_2+max(vals$vProd,na.rm = T)-max(vals$P_0,na.rm = T)
  vals$P_4_korr<-vals$P_4+max(vals$vProd,na.rm = T)-max(vals$P_0,na.rm = T)
  #output
  return(vals)}#ende

#####################################################
#function to run all above functions


hydrus<-function(params,
                 treat="all",#intensität oder "all"
                 UNSC=T,#UNSATCHEM modul an/aus
                 obs=all,#Messwerte
                 sleep=3,#sleeptime für die .exe
                 read=T,#soll der output gelesen werden
                 free_drain=T,#soll free drainage lower boundary condition sein
                 taskkill = F,
                 inverse=F,#soll Inverse Solution verwendet werden dann =T
                 dtmin=0.0001,
                 dtmax=10,
                 n_nodes=9,
                 Mat=c(rep(1,3),rep(2,5),3),
                 print_times = 2000,
                 kin_sol=F,
                 min_nrows=2200,
                 traintime=4500){
  #wenn treat ="all"
  if(treat=="all"){
    #wird für tmax  die zeitdifferenz vom ersten zum letzten Messwert in minuten verwendet
    tmax<-as.numeric(difftime(max(obs$date),min(obs$date),units = "min"))
    #t_event wird dann nicht gebraucht
    t_event<-NULL
    #alle auf TRUE gesetzt
    alle<-T
    
  }else{#wenn treat nicht "all" ist
    #berechnung von t_event
    t_event<-round(50/treat*60)
    #tmax wird gesetzt
    tmax<-6000
    #alle auf FALSE gesetzt
    alle<-F
  }
  
  #projektpfad wird abhängig von UNSC == T/F gewählt
  pfad<-ifelse(UNSC==T,"C:/Users/ThinkPad/Documents/Masterarbeit/daten/hydrus/undisturbed/","C:/Users/ThinkPad/Documents/Masterarbeit/daten/hydrus/undisturbed2/")
  
  #profile.in funktion ausführen
  profile.in(projektpfad = pfad,Mat = Mat,n_nodes = n_nodes,th=seq(0.2,0.2,len=n_nodes))
  
  #atmos.in funktion ausführen
  #hydruspfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/hydrus/"
  atmos.in(int=treat,event = t_event,total_t = tmax,alle=alle,projektpfad = pfad,obs=obs)
  
  #selector.in funktion ausführen
  selector.in(params=params,
              projektpfad = pfad,
              UNSC=UNSC,
              tmax = tmax,
              free_drain = free_drain,
              dtmin = dtmin,
              dtmax=dtmax,
              print_times = print_times,
              kin_sol = kin_sol)
  
  #file enstsprechend zu UNSC == T/F auswählen
  file<-ifelse(UNSC==T,"undisturbed","undisturbed2")
  
  #hydrus.exe ausführen
  hydrus.exe(file = file,UNSC=UNSC,sleep = sleep,Inverse = inverse,taskkill = taskkill)
  
  #wenn kein Taskkill verwendet werden soll
  if(taskkill==F){
    Sys.sleep(3)
    #funktion um CPU auslastung von hydrus zu bestimmen
  check_CPU<-function(){
    #liste abfragen in der der Name und die CPU aller laufender Prozesse steht
    tasklist<-system("wmic path Win32_PerfFormattedData_PerfProc_Process get Name,PercentProcessorTime",intern=T)
    #die strings in der liste bei mindestens zwei leerzeichen auseinanderschneiden
    tasksplit<-strsplit(tasklist[2:(length(tasklist)-1)]," \\s+")
    #die auseinandergeschnittenen strings zu einer matrix zusammenfügen
    tasks<-do.call("rbind",tasksplit)
    
    #abfragen ob in der Taskliste H1D_UNSC vorkommt
    if(length(grep("H1D_UNSC",tasks))>0){
      #while schleife wiederholen solange die CPU von H1D größer als 0 ist
      while(length(which(tasks[grep("H1D_UNSC",tasks),2]>0))>0){
        #kurz warten
        Sys.sleep(1)
        #aktuelle Taskliste abfragen und wie  gehabt formatieren
        tasklist<-system("wmic path Win32_PerfFormattedData_PerfProc_Process get Name,PercentProcessorTime",intern=T)
        tasksplit<-strsplit(tasklist[2:(length(tasklist)-1)]," \\s+")
        tasks<-do.call("rbind",tasksplit)
      }#ende while schleife
      }#ende if length H1D >0
  }#ende check_CPU funktion
  
  check_CPU()
  Sys.sleep(1)
  system("taskkill /IM H1D_UNSC.EXE",show.output.on.console=F)
  
}#ende if taskkill==F
  #wenn read  = TRUE den output einlesen ...
  if(read==T){
  co2_out<-read_hydrus.out(treat = treat,projektpfad = pfad,UNSC=UNSC,obs=obs,min_nrows=min_nrows,traintime = traintime)
  out<-co2_out[[1]]
  rmse_co2<-co2_out[[2]]/sd(out$CO2_raw,na.rm = T)
  ca_out<-read_conc.out(projektpfad = pfad,obs=obs)
  out_ca<-ca_out[[1]]
  rmse_ca<-ca_out[[2]]/sd(out_ca$ca_conc,na.rm = T)
  rmse_both<-(rmse_co2+rmse_ca)/2
  
  assign("rmse_co2",rmse_co2,envir = .GlobalEnv)
  assign("rmse_ca",rmse_ca,envir = .GlobalEnv)
  assign("rmse_both",rmse_both,envir = .GlobalEnv)
  
  out_P<-read_Nod_inf.out(projektpfad = pfad,obs=obs)
  out2<-merge(out,out_ca,all=T)
  out2<-merge(out2,out_P,all=T)
  #und ausgeben
  return(out2)}
}#ende