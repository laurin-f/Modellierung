phreeqc<-shell(paste('"start "" C:/Program Files/USGS/phreeqc-3.4.0-12927-x64/bin/phreeqci.exe" '))



phreeqc<-function(input){shell(paste0('cd C:/Users/ThinkPad/Documents/Masterarbeit/daten/PhreeqC/phreeqc-3.4.0-12927-x64/bin && phreeqc C:\\Users\\ThinkPad\\Documents\\Masterarbeit\\daten\\PhreeqC\\',input))}
phreeqc("calcium")
