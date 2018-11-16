
#############################
#function to execute hydrus

hydrus.exe<-function(file="undisturbed",scriptpath="C:/Users/ThinkPad/Documents/Masterarbeit/daten/hydrus/",sleep=3,UNSC=T,taskkill=F,Inverse=F){
  level_01<-readLines(paste0(hydruspfad,"Level_01.dir"))
  level_01<-sub("file",file,level_01)
  writeLines(level_01,paste0(programmpfad,"Level_01.dir"))
  
  if(taskkill==T){
    script<-readLines(paste0(scriptpath,"hydrus_exe.txt")) 
  }else{
    script<-readLines(paste0(scriptpath,"hydrus_exe2.txt"))
  }
  script<-sub("secs",sleep,script)
  if(UNSC==F){
    script<-sub("UNSC","CALC",script)}
  if(Inverse==T){
    script<-sub("CALC","CLCI",script)
  }
  writeLines(script,paste0(scriptpath,"hydrus_exe2.ps1"))
  shell(paste0("powershell.exe -noprofile -executionpolicy bypass -file ",scriptpath,"hydrus_exe2.ps1"))}


######################################
#function to set atmospheric input
atmos.in<-function(int=0,event=0,total_t=4000,alle=F,projektpfad=projektpfad1){
  if(alle==T){
    lines<-readLines(paste0(projektpfad,"ATMOSPH.IN"))
    
    head<-lines[1:grep("tAtm",lines)]
    
    
    tail<-lines[grep("END OF INPUT",lines)]
    
    source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/durchf-hrung/event.R")
    events<-event()
    events<-events[-1,]
    events<-subset(events,stop<=max(all$date))
    
    time_start<-as.numeric(difftime(events$start,events$start[1],units = "min"))
    time_stop<-as.numeric(difftime(events$stop,events$start[1],units = "min"))
    time<-sort(c(time_start,time_start-1,time_stop,time_stop+1,total_t))[-1]
    time[1]<-1
    
    evaps<-read.csv("C:/Users/ThinkPad/Documents/Masterarbeit/daten/events/verdunstung.csv",sep=";")
    radius<-9/2
    area<-pi*radius^2
    evap<-mean((evaps$vorher-evaps$nachher)/evaps$t_min)#ml/min = cm3/min
    
    evap<-evap/area
    int_paste<-paste(events$rain_mm_h/10/60,events$rain_mm_h/10/60,0,0)
    int<-do.call("c",strsplit(int_paste,split=" "))
    vals<-paste(time,int,evap,"           0      100000           0           0           0           0           0           0           0           0 ")
    
    head[grep("MaxAL",head)+1]<-format(length(time),width = 7)
    lines<-c(head,vals,tail)
    
    }else{
  int_cm_min<-int/10/60
  lines<-readLines(paste0(hydruspfad,"ATMOSPHtemp.IN"))
  lines<-sub("event",event,lines)
  lines<-sub("stop",event+5,lines)
  lines<-sub("total_t",total_t,lines)
  lines<-gsub("int",int_cm_min,lines)}
  writeLines(lines,paste0(projektpfad,"ATMOSPH.IN"))}



######################################
#function to set Soil Parameters

selector.in<-function(params,
                      co2_params=NULL,
                      projektpfad=projektpfad1,
                      tmax=4000,
                      print_times=100,
                      free_drain=F){
  lines<-readLines(paste0(projektpfad,"SELECTOR.IN"))
  
  vals<-paste(params$thr,params$ths,params$alpha,params$n,params$ks,params$l)
  if(length(params$ths2)==0){
    vals2<-vals
  }else{
  vals2<-paste(params$thr2,params$ths2,params$alpha2,params$n2,params$ks2,params$l)
  }
  vals_bot<-paste(params$thr_bot,params$ths_bot,params$alpha_bot,params$n_bot,params$ks_bot,params$l)
  
  if(length(co2_params)>0){
    co2_pos1<-grep("GamS0",lines)+1
    lines[co2_pos1]<-paste("   0.000  ",co2_params$p_opt,"      1250           0")
    
    co2_pos2<-grep("P0c        P50c",lines)+1
    lines[co2_pos2]<-paste("     6014        ",co2_params$act_en,"       0.14        0.19        ",co2_params$h_opt,co2_params$h_crit,"           3        1000")
    
  }
  soil_param_pos<-(grep("thr",lines)+1)
  lines[soil_param_pos]<-vals[1]
  lines[soil_param_pos+1]<-vals2[1]
  lines[grep("thr",lines)+3]<-vals_bot[1]
 
  if(free_drain==T){
    lines[grep("hSeep",lines)+1]<-" f     f     t     f     -1      f   0"
  }else{
  lines[grep("hSeep",lines)+1]<-paste0(" f     f     f     t     -1      f   ",params$hseep[1])
  }
    
  lines[grep(" tMax",lines)+1]<-paste("          0       ",tmax)
 
  lines[grep("MPL",lines)+1]<-paste("      0.001         0.1          10     1.3     0.7     3     7    ",print_times)
  
  p_seq<-seq(tmax/print_times,tmax,tmax/print_times)
  p_seq<-c(p_seq,rep(" ",5))
  prints<-1:round(print_times/6)
  j<-1
  for(i in seq(1,print_times,6)){
  prints[j]<-paste("",p_seq[i:(i+5)],collapse = " ")
  j<-j+1}
  
  head<-lines[1:grep("TPrint",lines)]
  if(length(grep("CARBON",lines))==0){
    tail<-lines[grep("END",lines):length(lines)]
  }else{
    tail<-lines[grep("CARBON",lines):length(lines)]
  }
  
  lines<-c(head,prints,tail)
  
  
  hydrus1d.dat<-readLines(paste0(projektpfad,"HYDRUS1D.DAT"))
  hydrus1d.dat[grep("PrintTimes=",hydrus1d.dat)]<-paste0("PrintTimes=",print_times)
  writeLines(hydrus1d.dat,paste0(projektpfad,"HYDRUS1D.DAT"))
  
  writeLines(lines,paste0(projektpfad,"SELECTOR.IN"))}


######################################
#function to set Profile Parameters

profile.in<-function(n_nodes=18,
                     th=seq(0.11,0.2,len=length(n_nodes)),
                     Mat=1,
                     Temp=20,
                     Conc=0.0004,
                     projektpfad=projektpfad1){
  tiefenstufen<-c(-2,-6,-10,-14,-17)
  lines<-readLines(paste0(projektpfad,"PROFILE.DAT"))
  
  vals<-paste0(format(1:n_nodes,width = 5)," ",sprintf("-%.6e",seq(0,17,len=n_nodes)),"  ",sprintf("%.6e",th)," "," "," "," ",Mat,"    1  0.000000e+00    1    1    1  ",sprintf("%.6e",Temp),"  ",sprintf("%.6e",Conc))
  obs_nodes<-substr(vals[substr(vals,7,19)%in%sprintf("%.6e",tiefenstufen)],3,6)
  vals<-gsub("e\\+","e+0",vals)
  vals<-gsub("e-","e-0",vals)
  
  
  tail1<-format(5,width = 5)
  tail2<-paste0(" ",paste0(" ",obs_nodes,collapse = ""))
  head<-lines[1:grep("Mat",lines)]
  head[5]<-paste0(format(n_nodes,width=5),substr(head[5],6,nchar(head[5])))
  lines<-c(head,vals,tail1,tail2)
  
  hydrus1d.dat<-readLines(paste0(projektpfad,"HYDRUS1D.DAT"))
  hydrus1d.dat[grep("NumberOfNodes=",hydrus1d.dat)]<-paste0("NumberOfNodes=",n_nodes)
  writeLines(hydrus1d.dat,paste0(projektpfad,"HYDRUS1D.DAT"))
 
  writeLines(lines,paste0(projektpfad,"PROFILE.DAT"))
}


############################################
#function to write obs data ta fit.in

fit.in<-function(obs=all,treat=17,projektpfad=projektpfad2,params,q_fit=T){
  
  lines<-readLines(paste0(projektpfad,"FIT.IN"))
  head<-lines[1:grep("ITYPE",lines)]
  
  vals<-paste(params$thr,params$ths,params$alpha,params$n,params$ks,params$l)
  if(length(params$ths2)==0){
    vals2<-vals
  }else{
    vals2<-paste(params$thr2,params$ths2,params$alpha2,params$n2,params$ks2,params$l)
  }
  vals_bot<-paste(params$thr_bot,params$ths_bot,params$alpha_bot,params$n_bot,params$ks_bot,params$l)
  
  head[grep("thr",head)+1][1]<-vals
  head[grep("thr",head)+1][2]<-vals2
  head[grep("thr",head)+1][3]<-vals_bot
  
  tail<-lines[grep("END",lines)]
  if(treat=="all"){
    sub<-all
    source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/durchf-hrung/event.R")
    events<-event()
    sub$t_min<-as.numeric(difftime(all$date,events$start[2],units = "min"))
    sub<-sub[sub$t_min%%100==0&sub$t_min>0,]
  }else{
  sub<-subset(obs,treatment%in%treat)
  sub<-sub[sub$t_min%%10==0&sub$t_min>0,]}
  sub_th<-subset(sub,!is.na(theta))
  sub_q<-subset(sub,!is.na(q_interpol)&q_interpol>0)
  vals_th<-paste(sub_th$t_min,sub_th$theta,2,(-sub_th$tiefe+2)/4,1)
  if(q_fit==T){
    vals_q<-paste(sub_q$t_min,sub_q$q_interpol*5,3,2,1)
    head[grep("NOBB",head)+1]<-paste(" ",length(c(vals_q,vals_th)) ,"     30       2")
    lines<-c(head,vals_th,vals_q,tail)
  }else{
    head[grep("NOBB",head)+1]<-paste(" ",length(vals_th) ,"     30       2")
    lines<-c(head,vals_th,tail)
  }

  
  writeLines(lines,paste0(projektpfad,"FIT.IN"))
}

########################################
#read fit.out

fit.out<-function(projektpfad=projektpfad2){
  lines<-readLines(paste0(projektpfad,"Fit.out"))
  results<-grep("final results",lines)
  res_list<-strsplit(lines[(results+4):(results+12)],"\\s+")
  pars<-as.data.frame(do.call("cbind",res_list))[3,]
  colnames(pars)<-paste0(rep(c("alpha","n","ks"),3),rep(c("","2","_bot"),each=3))
  return(pars)
}
######################################
#function to set read Hydrus outputfile

read_hydrus.out<-function(obs=all,treat=17,projektpfad=projektpfad1,UNSC=T){
  
  fields<-count.fields(paste0(projektpfad,"Obs_Node.out"),blank.lines.skip = F,skip=10)
  ncols<-ifelse(UNSC==T,21,16)
  if(length(which(fields==ncols))>300&which(fields!=ncols)[1]-2>300){
    obs_node<-read.table(paste0(projektpfad,"Obs_Node.out"),skip=10,nrows = which(fields!=ncols)[1]-2,header = T)
    
    tiefenstufen<-c(-2,-6,-10,-14,-17)
    r<-7.5#cm Radius
    A<-pi*r^2#cm2 area
    
    n_vars<-(ncol(obs_node)-1)/5
    colnames(obs_node)<-c("t_min",rep(tiefenstufen,each=n_vars))
    
    th_pos<-c(1,seq(3,ncol(obs_node)-n_vars+2,by=n_vars))
    theta_node<-obs_node[,th_pos]
    theta_mod<-data.table::melt(theta_node,id=1)
    
    q_col<-ifelse(UNSC==T,20,16)
    q_mod<-obs_node[,c(1,q_col)]
    q_mod$tiefe<-as.factor(-17)
    colnames(q_mod)<-c("t_min","q_mod","tiefe")
    q_mod$q_mod<--q_mod$q_mod*A#cm3/min
    
    if(UNSC==T){
      CO2_node<-obs_node[,c(1,5,9,13,17)]
      CO2_mod<-data.table::melt(CO2_node,id=1)
      CO2_mod$value<-CO2_mod$value*10^6#ppm
      vals_mod<-merge(CO2_mod,theta_mod,by=c("t_min","variable"))
      colnames(vals_mod)<-c("t_min","tiefe","CO2_mod","theta_mod")
    }else{
      vals_mod<-theta_mod
      colnames(vals_mod)<-c("t_min","tiefe","theta_mod")
    }
    
    vals_mod<-merge(vals_mod,q_mod,all=T)
    
    # t_min<-1:max(vals_mod$t_min)
    # mod_list<-vector("list",4)
    # for (i in 1:4){
    #   mod<-as.data.frame(apply(vals_mod[vals_mod$tiefe==tiefenstufen[i],1:(ncol(vals_mod)-1)],2,function(x) approx(vals_mod$t_min[vals_mod$tiefe==tiefenstufen[i]],x,xout=t_min)$y))
    #   mod_list[[i]]<-mod}
    # 
    # modelled_approx<-do.call("rbind",mod_list)
    # 
    # q_approx<-as.data.frame(apply(vals_mod[vals_mod$tiefe==tiefenstufen[5],c(1:2,ncol(vals_mod))],2,function(x) approx(vals_mod$t_min[vals_mod$tiefe==tiefenstufen[5]],x,xout=t_min)$y))
    
    
    if(treat=="all"){
      sub<-all[,c(1:3,6,11,17)]
      source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/durchf-hrung/event.R")
      events<-event()
      sub$t_min<-as.numeric(difftime(all$date,events$start[2],units = "min"))
      
    }else{
    sub<-subset(obs,treatment==treat&tiefe%in%tiefenstufen)}
    sub<-sub[sub$t_min%%10==0,]
    sub<-merge(sub,vals_mod,by=c("t_min","tiefe"),all=T)
    #sub<-merge(sub,q_approx,by=c("t_min","tiefe"),all=T)
    
    rmse_theta<-sqrt(mean((sub$theta-sub$theta_mod)^2,na.rm = T))
    rmse_q<-sqrt(mean((sub$q_interpol*5-sub$q_mod)^2,na.rm = T))
    if(UNSC==T){
      rmse_CO2<-sqrt(mean((sub$CO2_raw-sub$CO2_mod)^2,na.rm = T))
      #rmse<-rmse_theta/sd(sub$theta,na.rm = T)^2+rmse_CO2/sd(sub$CO2_raw,na.rm = T)^2+rmse_q/sd(sub$q_interpol*5,na.rm = T)^2
      rmse<-rmse_CO2
      #rmse<-rmse_theta+rmse_CO2+rmse_q
    }else{
      #rmse<-rmse_theta+rmse_q
      rmse<-rmse_theta/sd(sub$theta,na.rm = T)^2+rmse_q/sd(sub$q_interpol*5,na.rm = T)^2
    }
    return(list(sub,rmse))}else{
      return(list(NA,NA))
    }
  }


######################################
#function to set read Hydrus concentration outputfile

read_conc.out<-function(projektpfad=projektpfad1,
                        n_nodes=18){
  lines<-readLines(paste0(projektpfad,"Conc.out"))
  time<-as.numeric(substr(lines[grep("Time:",lines)],7,19))
  names<-strsplit(lines[grep("Node",lines)][1],"\\s+")[[1]]
  names[1]<-"time"
  
  vals<-vector("list",length(time))
  
  for (i in 1:length(time)){
    pos_vals<-(grep("Node",lines)+2)[i]:(grep("Node",lines)+1+n_nodes)[i]
    list_vals<-strsplit(lines[pos_vals],"\\s+")
    vals[[i]]<-as.data.frame(do.call("rbind",list_vals))
    vals[[i]][,1]<-time[i]
  }
  
  vals<-do.call("rbind",vals)
  colnames(vals)<-names
  
  vals<-as.data.frame(apply(vals, 2, function(x) as.numeric(as.character(x))))
  return(vals)}
