if(!"caTools"%in%installed.packages()){
install.packages("caTools")}
if(!"SAFER"%in%installed.packages()){
saferpath<-"C:/Users/ThinkPad/Documents/Masterarbeit/programme/safer_1.1/safer_1.1"
install.packages(paste0(saferpath,"/calibrater_0.51.tar.gz"), repos = NULL, type = "source")
install.packages(paste0(saferpath,"/SAFER_1.1.tar.gz"), repos = NULL, type = "source")}

library(SAFER)
