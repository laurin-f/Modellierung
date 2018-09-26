phreeqc<-function(input){shell(paste0('cd C:/Users/ThinkPad/Documents/Masterarbeit/daten/PhreeqC/phreeqc-3.4.0-12927-x64/bin && phreeqc C:\\Users\\ThinkPad\\Documents\\Masterarbeit\\daten\\PhreeqC\\',input))}

phreeqc("calcium")

#sendKeys 
phreeqc_gui<-function(file,path){
  path<-gsub("/","\\\\\\\\",path)
script<-readLines("C:/Users/ThinkPad/Documents/Masterarbeit/daten/PhreeqC/psinput",encoding = "utf8")
writeLines(sub("pfad",paste0(path,file),script),
           "C:/Users/ThinkPad/Documents/Masterarbeit/daten/PhreeqC/psinput.ps1")
#shell("powershell.exe -noprofile -executionpolicy bypass -file C:/Users/ThinkPad/Documents/Masterarbeit/daten/PhreeqC/phreeqc.ps1")
shell("powershell.exe -noprofile -executionpolicy bypass -file C:/Users/ThinkPad/Documents/Masterarbeit/daten/PhreeqC/psinput.ps1")
print(paste0(path,file))}

phreeqc_gui("calcium.pqi","C:/Users/ThinkPad/Documents/Masterarbeit/daten/PhreeqC/")

