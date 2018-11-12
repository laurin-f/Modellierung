hydruspfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/hydrus/"
projektpfad1<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/hydrus/undisturbed/"
projektpfad2<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/hydrus/undisturbed2/"
programmpfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/programme/Hydrus-1D 4.xx/"


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


load("C:/Users/ThinkPad/Documents/Masterarbeit/daten/all.R")

atmos.in<-function(int,event,total_t,projektpfad=projektpfad1){
  int_cm_min<-int/10/60
  lines<-readLines(paste0(hydruspfad,"ATMOSPHtemp.IN"))
  lines<-sub("event",event,lines)
  lines<-sub("stop",event+5,lines)
  lines<-sub("total_t",total_t,lines)
  lines<-gsub("int",int_cm_min,lines)
  writeLines(lines,paste0(projektpfad,"ATMOSPH.IN"))}

max(all$theta,na.rm=T)

selector.in<-function(thr,
                      ths,
                      alfa,
                      n,
                      ks,
                      l=0.5,
                      n_mats=3,
                      projektpfad=projektpfad1){
  lines<-readLines(paste0(projektpfad,"SELECTOR.IN"))
  vals<-paste(thr,ths,alfa,n,ks,l)
  soil_param_pos<-(grep("thr",lines)+1):(grep("thr",lines)+n_mats)
  lines[soil_param_pos]<-vals
  writeLines(lines,paste0(projektpfad,"SELECTOR.IN"))}

profile.in<-function(n_nodes=19,
                     th=seq(0.1,0.2,len=length(n_nodes)),
                     Mat=1,
                     Temp=20,
                     Conc=0.0004,
                     projektpfad=projektpfad1){
  tiefenstufen<-c(-2,-6,-10,-14)
  lines<-readLines(paste0(projektpfad,"PROFILE.DAT"))
  #n_nodes<-length(lines)-7
  #vals<-paste(1:n_nodes,-(1:n_nodes)+1,th,Mat,"1  0.000000e+000    1    1    1",Temp,Conc)
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
  # lines[length(lines)-1]<-format(4,width = 5)
  # lines[length(lines)]<-paste0(" ",paste0(" ",obs_nodes,collapse = ""))
  # soil_profile_pos<-(grep("Mat",lines)+1):(grep("Mat",lines)+n_nodes)
  # lines[soil_profile_pos]<-vals

  writeLines(lines,paste0(projektpfad,"PROFILE.DAT"))
  }



read_hydrus.out<-function(treat=17,projektpfad=projektpfad1,UNSC=T){
  lines_node<-readLines(paste0(projektpfad,"Obs_Node.out"))
obs_node<-read.table(paste0(projektpfad,"Obs_Node.out"),skip=10,nrows = length(lines_node)-12,header = T)
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


sub<-subset(all,treatment==treat&tiefe%in%tiefenstufen)
sub<-merge(sub,modelled_approx,by=c("t_min","tiefe"),all="T")

rmse_theta<-sqrt(mean((sub$theta-sub$theta_mod)^2,na.rm = T))
if(UNSC==T){
rmse_CO2<-sqrt(mean((sub$CO2_raw-sub$CO2_mod)^2,na.rm = T))
rmse<-c(rmse_theta,rmse_CO2)
}else{
  rmse<-rmse_theta
}
return(list(sub,rmse))}


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


ggplot(vals)+geom_line(aes(time,Ca,col=as.factor(Depth)))
ggplot(vals)+geom_line(aes(Ca,Depth,col=as.factor(time)))
ggplot(vals)+geom_tile(aes(time,Depth,fill=Ca))
######################################################
#run modell

library(ggplot2)

selector.in(thr=0.068,
            ths=c(0.6,0.51,0.5),
            alfa=c(0.075,0.075,0.075),
            n=c(2.0,2,1.5),
            ks=c(0.07,0.05,0.02),
            l=0.5)
atmos.in(17,180,4000)
profile.in(n_nodes = 19,Mat = c(rep(1,7),rep(2,10),3,3))
hydrus.exe(sleep = 5)

sub<-read_hydrus.out(treat=17)[[1]]
vals<-read_conc.out(projektpfad = projektpfad1)

ggplot(sub)+geom_point(aes(t_min,theta,col="obs"),na.rm = T)+geom_line(aes(t_min,theta_mod,col="mod"),na.rm = T)+facet_wrap(~tiefe,ncol=1)


ggplot(sub)+geom_point(aes(t_min,CO2_raw,col="obs"),na.rm = T)+geom_line(aes(t_min,CO2_mod*1000000,col="mod"),na.rm = T)+facet_wrap(~tiefe,ncol=1)

ggplot(vals)+geom_line(aes(time,Ca,col=as.factor(Depth)))
ggplot(vals)+geom_line(aes(Ca,Depth,col=as.factor(time)))
ggplot(vals)+geom_tile(aes(time,Depth,fill=Ca))


####################################
#Monte Carlo


mc_params<-

monte_carlo<-function(nr=100,
                      par=data.frame(thr=runif(nr,0.05,0.1),
                      ths=runif(nr,0.4,0.7),
                      alfa=runif(nr,0.005,0.075),
                      n=runif(nr,1.5,4.5),
                      ks=runif(nr,0.01,0.5))){
  rmse<-rep(NA,nr)
for (i in 1:nr){
  selector.in(par$thr[i],
              par$ths[i],
              par$alfa[i],
              par$n[i],
              par$ks[i])
  hydrus.exe(sleep=5)
  rmse[i]<-read_hydrus.out(treat=17)[[2]][1]
  print(paste(i/nr*100,"%"))
}
  return(list(rmse,par))
}

mc<-monte_carlo(nr=300)
plot(par$ks,mc)
#######################################
#without co2
selector.in(thr=0.068,
            ths=c(0.6,0.51,0.2),
            alfa=c(0.075,0.035,0.005),
            n=c(4.0,4,1.5),
            ks=c(0.07,0.07,0.02),
            l=0.5,
            projektpfad = projektpfad2)
atmos.in(0.26,180,4000,
         projektpfad = projektpfad2)
profile.in(projektpfad = projektpfad2,Mat = c(rep(1,7),rep(2,10),3,3))
hydrus.exe(sleep = 2,file = "undisturbed2",UNSC=T)

sub<-read_hydrus.out(treat=17,projektpfad = projektpfad2,UNSC=T)[[1]]

ggplot(sub)+geom_point(aes(t_min,theta,col="obs"),na.rm = T)+geom_line(aes(t_min,theta_mod,col="mod"),na.rm = T)+facet_wrap(~tiefe,ncol=1)




ggplot()+geom_path(data=sub,aes(t_min,theta,col=as.factor(tiefe)))+geom_line(data=vals_mod,aes(t_min,theta_mod,col=as.factor(tiefe)))
+facet_wrap(~tiefe,ncol=1)

ggplot()+geom_line(data=vals_mod,aes(t_min,theta_mod,col=1))+geom_line(data=modelled_approx,aes(t_min,theta_mod,col=2))+facet_wrap(~tiefe)


plot(sub$t_min,sub$theta,type="l")
lines(obs_node$time,obs_node$theta.1,col=2)
min(sub$theta,na.rm=T)

plot(sub$t_min,sub$CO2_raw)
plot(obs_node$time,obs_node$CO2.2*200000,col=2)

