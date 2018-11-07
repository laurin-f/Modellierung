hydruspfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/hydrus/"
projektpfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/hydrus/undisturbed/"

hydrus_gui<-function(file,path=hydruspfad){
  path<-gsub("/","\\\\\\\\",path)
  
  scriptpath<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/hydrus/"
  
  script<-readLines(paste0(scriptpath,"hydrus_psinput.txt"))
  
  writeLines(sub("pfad",paste0(path,file),script),
             paste0(scriptpath,"psinput.ps1"))
  
  shell(paste0("powershell.exe -noprofile -executionpolicy bypass -file ",scriptpath,"psinput.ps1"))}





load("C:/Users/ThinkPad/Documents/Masterarbeit/daten/all.R")

atmos.in<-function(int,event,total_t){
lines<-readLines(paste0(hydruspfad,"ATMOSPHtemp.IN"))
lines<-sub("event",event,lines)
lines<-sub("stop",event+5,lines)
lines<-sub("total_t",total_t,lines)
lines<-gsub("int",0.26,lines)
writeLines(lines,paste0(projektpfad,"ATMOSPH.IN"))}

atmos.in(0.26,180,4000)

hydrus_gui(file="undisturbed.h1d")


lines_node<-readLines(paste0(projektpfad,"Obs_Node.out"))


obs_node<-read.table(paste0(projektpfad,"Obs_Node.out"),skip=10,nrows = length(lines_node)-12,header = T)

sub<-subset(all,tiefe==-10)

plot(sub$t_min,sub$theta,type="l")
lines(obs_node$time,obs_node$theta.1,col=2)


plot(sub$t_min,sub$CO2_raw,type="l")
lines(obs_node$time,obs_node$CO2.2*200000,col=2)
