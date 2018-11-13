
#############################
#function to execute hydrus

hydrus.exe<-function(file="undisturbed",scriptpath="C:/Users/ThinkPad/Documents/Masterarbeit/daten/hydrus/",sleep=3,UNSC=T){
  level_01<-readLines(paste0(hydruspfad,"Level_01.dir"))
  level_01<-sub("file",file,level_01)
  writeLines(level_01,paste0(programmpfad,"Level_01.dir"))
  
  script<-readLines(paste0(scriptpath,"hydrus_exe2.txt"))
  script<-sub("secs",sleep,script)
  if(UNSC==F){
    script<-sub("UNSC","CALC",script)}
  writeLines(script,paste0(scriptpath,"hydrus_exe2.ps1"))
  shell(paste0("powershell.exe -noprofile -executionpolicy bypass -file ",scriptpath,"hydrus_exe2.ps1"))}


######################################
#function to set atmospheric input
atmos.in<-function(int,event,total_t,projektpfad=projektpfad1){
  int_cm_min<-int/10/60
  lines<-readLines(paste0(hydruspfad,"ATMOSPHtemp.IN"))
  lines<-sub("event",event,lines)
  lines<-sub("stop",event+5,lines)
  lines<-sub("total_t",total_t,lines)
  lines<-gsub("int",int_cm_min,lines)
  writeLines(lines,paste0(projektpfad,"ATMOSPH.IN"))}


######################################
#function to set Soil Parameters

selector.in<-function(params,
                      n_mats=2,
                      projektpfad=projektpfad1){
  lines<-readLines(paste0(projektpfad,"SELECTOR.IN"))
  vals<-paste(params$thr,params$ths,params$alpha,params$n,params$ks,params$l)
  soil_param_pos<-(grep("thr",lines)+1):(grep("thr",lines)+n_mats)
  lines[soil_param_pos]<-vals
  
  lines[grep("hSeep",lines)+1]<-paste0(" f     f     f     t     -1      f   ",params$hseep[1])
  writeLines(lines,paste0(projektpfad,"SELECTOR.IN"))}


######################################
#function to set Profile Parameters

profile.in<-function(n_nodes=19,
                     th=seq(0.1,0.2,len=length(n_nodes)),
                     Mat=1,
                     Temp=20,
                     Conc=0.0004,
                     projektpfad=projektpfad1){
  tiefenstufen<-c(-2,-6,-10,-14)
  lines<-readLines(paste0(projektpfad,"PROFILE.DAT"))
  
  vals<-paste0(format(1:n_nodes,width = 5)," ",sprintf("-%.6e",seq(0,18,len=n_nodes)),"  ",sprintf("%.6e",th)," "," "," "," ",Mat,"    1  0.000000e+00    1    1    1  ",sprintf("%.6e",Temp),"  ",sprintf("%.6e",Conc))
  obs_nodes<-substr(vals[substr(vals,7,19)%in%sprintf("%.6e",tiefenstufen)],3,6)
  vals<-gsub("e\\+","e+0",vals)
  vals<-gsub("e-","e-0",vals)
  
  
  tail1<-format(4,width = 5)
  tail2<-paste0(" ",paste0(" ",obs_nodes,collapse = ""))
  head<-lines[1:grep("Mat",lines)]
  head[5]<-paste0(format(n_nodes,width=5),substr(head[5],6,nchar(head[5])))
  lines<-c(head,vals,tail1,tail2)
  
  hydrus1d.dat<-readLines(paste0(projektpfad,"HYDRUS1D.DAT"))
  hydrus1d.dat[grep("NumberOfNodes=",hydrus1d.dat)]<-paste0("NumberOfNodes=",n_nodes)
  writeLines(hydrus1d.dat,paste0(projektpfad,"HYDRUS1D.DAT"))
 
  writeLines(lines,paste0(projektpfad,"PROFILE.DAT"))
}

######################################
#function to set read Hydrus outputfile

read_hydrus.out<-function(obs=all,treat=17,projektpfad=projektpfad1,UNSC=T){
  
  fields<-count.fields(paste0(projektpfad,"Obs_Node.out"),blank.lines.skip = F,skip=10)
  ncols<-ifelse(UNSC==T,17,13)
  if(length(which(fields==ncols))>300){
    obs_node<-read.table(paste0(projektpfad,"Obs_Node.out"),skip=10,nrows = which(fields!=ncols)[1]-2,header = T)
    
    tiefenstufen<-c(-2,-6,-10,-14)
    
    
    n_vars<-(ncol(obs_node)-1)/4
    colnames(obs_node)<-c("t_min",rep(tiefenstufen,each=n_vars))
    
    th_pos<-c(1,seq(3,ncol(obs_node)-n_vars+2,by=n_vars))
    theta_node<-obs_node[,th_pos]
    theta_mod<-data.table::melt(theta_node,id=1)
    
    if(UNSC==T){
      CO2_node<-obs_node[,c(1,5,9,13,17)]
      CO2_mod<-data.table::melt(CO2_node,id=1)
      vals_mod<-merge(CO2_mod,theta_mod,by=c("t_min","variable"))
      colnames(vals_mod)<-c("t_min","tiefe","CO2_mod","theta_mod")
    }else{
      vals_mod<-theta_mod
      colnames(vals_mod)<-c("t_min","tiefe","theta_mod")
    }
    
    t_min<-1:max(vals_mod$t_min)
    mod_list<-vector("list",4)
    for (i in 1:4){
      mod<-as.data.frame(apply(vals_mod[vals_mod$tiefe==tiefenstufen[i],][,2:ncol(vals_mod)],2,function(x) approx(vals_mod$t_min[vals_mod$tiefe==tiefenstufen[i]],x,xout=t_min)$y))
      mod$t_min<-t_min
      mod_list[[i]]<-mod}
    modelled_approx<-do.call("rbind",mod_list)
    
    
    sub<-subset(obs,treatment==treat&tiefe%in%tiefenstufen)
    sub<-merge(sub,modelled_approx,by=c("t_min","tiefe"),all="T")
    
    rmse_theta<-sqrt(mean((sub$theta-sub$theta_mod)^2,na.rm = T))
    if(UNSC==T){
      rmse_CO2<-sqrt(mean((sub$CO2_raw-sub$CO2_mod)^2,na.rm = T))
      rmse<-c(rmse_theta,rmse_CO2)
    }else{
      rmse<-rmse_theta
    }
    return(list(sub,rmse))}else{
      return(list(NA,NA))
    }}


######################################
#function to set read Hydrus concentration outputfile

read_conc.out<-function(projektpfad=projektpfad1,
                        n_nodes=19){
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
