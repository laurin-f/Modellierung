hydruspfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/hydrus/"
projektpfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/hydrus/undisturbed/"
programmpfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/programme/Hydrus-1D 4.xx/"

hydrus_gui<-function(file,path=hydruspfad){
  path<-gsub("/","\\\\\\\\",path)
  
  scriptpath<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/hydrus/"
  
  script<-readLines(paste0(scriptpath,"hydrus_psinput.txt"))
  
  writeLines(sub("pfad",paste0(path,file),script),
             paste0(scriptpath,"psinput.ps1"))
  
  shell(paste0("powershell.exe -noprofile -executionpolicy bypass -file ",scriptpath,"psinput.ps1"))}


hydrus.exe<-function(file="undisturbed",scriptpath="C:/Users/ThinkPad/Documents/Masterarbeit/daten/hydrus/",sleep=3){
  level_01<-readLines(paste0(hydruspfad,"Level_01.dir"))
  level_01<-sub("file",file,level_01)
  writeLines(level_01,paste0(programmpfad,"Level_01.dir"))
  
  script<-readLines(paste0(scriptpath,"hydrus_exe2.txt"))
  
  writeLines(sub("secs",sleep,script),
             paste0(scriptpath,"hydrus_exe2.ps1"))
  shell(paste0("powershell.exe -noprofile -executionpolicy bypass -file ",scriptpath,"hydrus_exe2.ps1"))}


load("C:/Users/ThinkPad/Documents/Masterarbeit/daten/all.R")

atmos.in<-function(int,event,total_t,projektpfad=projektpfad){
lines<-readLines(paste0(hydruspfad,"ATMOSPHtemp.IN"))
lines<-sub("event",event,lines)
lines<-sub("stop",event+5,lines)
lines<-sub("total_t",total_t,lines)
lines<-gsub("int",0.26,lines)
writeLines(lines,paste0(projektpfad,"ATMOSPH.IN"))}

max(all$theta,na.rm=T)

selector.in<-function(thr,
                      ths,
                      alfa,
                      n,
                      ks,
                      l,
                      n_mats=3,
                      projektpfad=projektpfad){
  lines<-readLines(paste0(projektpfad,"SELECTOR.IN"))
  vals<-paste(thr,ths,alfa,n,ks,l)
  soil_param_pos<-(grep("thr",lines)+1):(grep("thr",lines)+n_mats)
  lines[soil_param_pos]<-vals
  writeLines(lines,paste0(projektpfad,"SELECTOR.IN"))}

profile.in<-function(th=seq(0.1,0.2,len=10),
                     Mat=1,
                     Temp=20,
                     Conc=0.0004,
                     projektpfad=projektpfad){
  lines<-readLines(paste0(projektpfad,"PROFILE.DAT"))
  n_nodes<-length(lines)-7
  #vals<-paste(1:n_nodes,-(1:n_nodes)+1,th,Mat,"1  0.000000e+000    1    1    1",Temp,Conc)
  vals<-paste0(format(1:n_nodes,width = 5)," ",sprintf("-%.6e",seq(0,18,len=n_nodes)),"  ",sprintf("%.6e",th)," "," "," "," ",Mat,"    1  0.000000e+00    1    1    1  ",sprintf("%.6e",Temp),"  ",sprintf("%.6e",Conc))
  obs_nodes<-substr(vals[substr(vals,7,19)%in%sprintf("%.6e",tiefenstufen)],3,6)
  vals<-gsub("e\\+","e+0",vals)
  vals<-gsub("e-","e-0",vals)
  lines[length(lines)-1]<-format(4,width = 5)
  lines[length(lines)]<-paste0(" ",paste0(" ",obs_nodes,collapse = ""))
  soil_profile_pos<-(grep("Mat",lines)+1):(grep("Mat",lines)+n_nodes)
  lines[soil_profile_pos]<-vals
  writeLines(lines,paste0(projektpfad,"PROFILE.DAT"))}



read_hydrus.out<-function(treat=17,projektpfad=projektpfad){lines_node<-readLines(paste0(projektpfad,"Obs_Node.out"))
obs_node<-read.table(paste0(projektpfad,"Obs_Node.out"),skip=10,nrows = length(lines_node)-12,header = T)
tiefenstufen<-c(-2,-6,-10,-14)
colnames(obs_node)<-c("t_min",rep(tiefenstufen,each=4))
CO2_node<-obs_node[,c(1,5,9,13,17)]
theta_node<-obs_node[,c(1,3,7,11,15)]
CO2_mod<-data.table::melt(CO2_node,id=1)
theta_mod<-data.table::melt(theta_node,id=1)

vals_mod<-merge(CO2_mod,theta_mod,by=c("t_min","variable"))
colnames(vals_mod)<-c("t_min","tiefe","CO2_mod","theta_mod")
max(vals_mod$t_min)
t_min<-1:max(vals_mod$t_min)
mod_list<-vector("list",4)
for (i in 1:4){
mod<-as.data.frame(apply(vals_mod[vals_mod$tiefe==tiefenstufen[i],][,2:4],2,function(x) approx(vals_mod$t_min[vals_mod$tiefe==tiefenstufen[i]],x,xout=t_min)$y))
mod$t_min<-t_min
mod_list[[i]]<-mod}
modelled_approx<-do.call("rbind",mod_list)


sub<-subset(all,treatment==treat&tiefe%in%tiefenstufen)
sub<-merge(sub,modelled_approx,by=c("t_min","tiefe"),all="T")

rmse_theta<-sqrt(mean((sub$theta-sub$theta_mod)^2,na.rm = T))
rmse_CO2<-sqrt(mean((sub$CO2_raw-sub$CO2_mod)^2,na.rm = T))
return(list(sub,c(rmse_theta,rmse_CO2)))}


library(ggplot2)
selector.in(thr=0.068,
            ths=0.61,
            alfa=0.075,
            n=1.89,
            ks=0.0273,
            l=0.5)
atmos.in(0.0026,180,4000)
profile.in()
hydrus.exe(sleep = 8)
sub<-read_hydrus.out(treat=17)[[1]]


ggplot(sub)+geom_point(aes(t_min,theta,col="obs"),na.rm = T)+geom_line(aes(t_min,theta_mod,col="mod"),na.rm = T)+facet_wrap(~tiefe,ncol=1)


ggplot(sub)+geom_point(aes(t_min,CO2_raw,col="obs"),na.rm = T)+geom_line(aes(t_min,CO2_mod*1000000,col="mod"),na.rm = T)+facet_wrap(~tiefe,ncol=1)

ggplot()+geom_path(data=sub,aes(t_min,theta,col=as.factor(tiefe)))+geom_line(data=vals_mod,aes(t_min,theta_mod,col=as.factor(tiefe)))
+facet_wrap(~tiefe,ncol=1)

ggplot()+geom_line(data=vals_mod,aes(t_min,theta_mod,col=1))+geom_line(data=modelled_approx,aes(t_min,theta_mod,col=2))+facet_wrap(~tiefe)


plot(sub$t_min,sub$theta,type="l")
lines(obs_node$time,obs_node$theta.1,col=2)
min(sub$theta,na.rm=T)

plot(sub$t_min,sub$CO2_raw)
plot(obs_node$time,obs_node$CO2.2*200000,col=2)

1/60
0.0004/400
1000000*0.0004
