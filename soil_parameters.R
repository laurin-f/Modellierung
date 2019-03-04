#package um .xls einzulesen
library(readxl)
#plot package
library(ggplot2)

#pfad des Datei
soilpfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/bodenparameter/"
plotpfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/abbildungen/plots/"

#sheet 3 der .xls einlesen
soil.xls<-read_xls(paste0(soilpfad,"Soil physical data Hartheim.xls"),sheet = 3)
#sheet 2 der .xls einlesen
soil.xls2<-read_xls(paste0(soilpfad,"Soil physical data Hartheim.xls"),sheet = 2)


#aggregieren der Horizonte
soil<-aggregate(soil.xls[,4:41],list(soil.xls$Horizon),function(x) mean(x,na.rm = T))
#bulk density in g/cm3
soil$Dichte

###########################################
#funktion um Bodenretentionskurve zu fitten
muafit<-function(data,alpha=NULL,n=NULL){
  if(is.null(alpha)&is.null(n)){
  #fitten der Parameter alpha und n der Mualem van genuchten gleichung
  mua<-nls(th_norm~(1+(-alpha*psi)^n)^-(1-1/n),data=data,start = list(alpha=0.02,n=1.2))
  #ausgabe der Parameter
  alpha<-coef(mua)[1]
  n<-coef(mua)[2]}
  #psi sequenz um gefittete Werte zu berechnen
  psi<-seq(min(round(data$psi)),-1,by=1)
  #mit gefitteten Alpha und n theta_werte berechnen
  fit<-(1+(-alpha*psi)^n)^-(1-1/n)
  return(list(data.frame(psi,th_mod=fit),c(alpha,n)))}

###############################
#Horizont Ah1
#Bodenfeuchte (th), Sättigungswassergehalt (ths) und residualer Wassergehalt (thr)
#sowie theta norm
#der unterschiedlichen Proben bestimmen
#############################

#alle Proben aus Ah1 auswählen sheet 2
#sheet 2 ist der datensatz im long format
Ah1<-subset(soil.xls2,Horizon=="Ah1"&pf!=7)

#alle Proben aus Ah1 auswählen sheet 3
Ah1_sheet3<-subset(soil.xls,Horizon=="Ah1")
#ths entspricht dem gesamten Porenvolumen
Ah1_sheet3$ths<-Ah1_sheet3$PV/100

#theta ist der soil Water content (swc) /100
Ah1$th<-Ah1$swc/100

#thr ist der residuale Wassergehalt bei pf=4.2
thr_Ah1<-Ah1[Ah1$pf==4.2,c(8,10)]
#Spaltennamen anpassen
colnames(thr_Ah1)<-c("MG_ID","thr")
#thr an den Datensatz anfügen
Ah1<-merge(Ah1,thr_Ah1)

#ths an den Datensatz anfügen
Ah1<-merge(Ah1,Ah1_sheet3[,c(1,42)])

#Da keine Messungen für den Wassergesättigten boden vorliegen werden spalten mit th=ths bei pf=0 angefügt
sat_Ah1<-data.frame(th=Ah1_sheet3$ths,ths=Ah1_sheet3$ths,MG_ID=Ah1_sheet3$MG_ID,pf=0,thr=Ah1_sheet3$swc_15000hPa/100)
#NAs ausschneiden
sat_Ah1<-sat_Ah1[!is.na(sat_Ah1$thr),]
#die Werte für pf=0 werden dem Datensatz angefügt
Ah1<-merge(Ah1,sat_Ah1,all=T)

#th_norm wird berechnet
Ah1$th_norm<-(Ah1$th-Ah1$thr)/(Ah1$ths-Ah1$thr)

#pf ist der negative dekadische log der saugspannung psi
#psi berechnen
Ah1$psi<--10^Ah1$pf

########################################
#Paramter alpha und n für Ah1 fitten
#######################################

#RMSE funktion erstellen
RMSE<-function(obs,mod){
  rmse<-sqrt(mean((obs-mod)^2))
  return(rmse)
}

#aufsteigende sequenz der psi-werte 
psi<-sort(unique(Ah1$psi))
pf<-log10(-psi)
#mittelwerte von th_norm für jedes psi berechnen
th_norm<-tapply(Ah1$th_norm,Ah1$psi,mean)
th_norm_dpf<-c(0,-diff(th_norm)/diff(pf),0)
dpf<-c(4.2,pf[1:length(pf)-1]+diff(pf)/2,0)
pore_distr<-data.frame(dpf=dpf,th_norm_dpf=th_norm_dpf)

#länge der Sequenz für alpha und n
n_parseq<-300
#aufsteigende sequenz von alpha und n werten
#dabei wird die alpha sequenz werte 300-mal wiederholt
#und bei der n sequenz jeder wert 300-mal wiederholt
#sodass alle kombinationen der Parameter vorkommen
alpha<-rep(seq(0.05,4,len=n_parseq),n_parseq)
n<-rep(seq(1.1,2,len=n_parseq),each=n_parseq)

#leere matrix für RMSE-Werte der unterschiedlichen Rententionskurve
fit<-matrix(NA,length(n),length(psi))
#schleife um matrix zu füllen
for (i in 1:length(psi)){
  #Berechnung der van Genuchten Gleichung für i-ten Psi-Wert
fit[,i]<-(1+(-alpha*psi[i])^n)^-(1-1/n)
}

#berechnung des RMSE für jeden Parametersatz
rmse<-apply(fit,1,function(x) RMSE(obs=th_norm,mod=x))

#critischer RMSE wert auf dem der realistische Wertebereich der Parameter begrenzt wird
crit<-0.085
#alle RMSE-Werte die besser sind als crit
rmsegood<-which(rmse<=crit)
#bester RMSE
bestrmse<-which.min(rmse)

#Parameterranges für Ah1 bestimmen
alpha_range_Ah1<-range(alpha[rmsegood])
n_range_Ah1<-range(n[rmsegood])

############################################
#plot der Retentionskurve für Ah1
############################################

#die Retentionskurve beim Grenzwert für realistische alpha und n Werte mit 
#für höher aufgelöstes psi berechnen damit die Kurve im Plot schön glat ist
fit_min<-muafit(Ah1,alpha = min(alpha[rmsegood]),n=min(n[rmsegood]))[[1]]
fit_max<-muafit(Ah1,alpha = max(alpha[rmsegood]),n=max(n[rmsegood]))[[1]]
#die Retentionskurve des besten fits
fit_best<-muafit(Ah1,alpha = max(alpha[bestrmse]),n=max(n[bestrmse]))[[1]]

#um ein Polygon zu plotten wird fit_max rückwärts an fit_min gehängt
#dann wird jeweils das obere und das untere Ende der Linien richtig verbunden
fit_range<-rbind(fit_min,fit_max[order(fit_max$psi,decreasing = T),])

#plot der gemessenen Retentionskurven un der gefitteten Kurve 
Ah1plot<-ggplot()+
  geom_polygon(data=fit_range,aes(th_mod,log10(-psi),fill=""),alpha=0.5)+
  geom_path(data=Ah1,aes(th_norm,pf,col="obs",linetype=MG_ID),show.legend = F)+
  geom_line(data=fit_best,aes(th_mod,log10(-psi),col="best fit"),size=1.4)+
  labs(x=expression(S[e]),y="pF")+theme_classic()+
  scale_colour_manual(name="",values = c("red",rep(grey(0.3),11)),labels=c("best fit",rep("obs",11)))+scale_fill_manual(name=paste0("RMSE<",crit),values = "grey")+scale_linetype_manual(values=rep(1,11))+annotate("text",x=0.8,y=c(3.15,3,2.85),label=c("best fit",paste(c("alpha","n")," = ",signif(c(alpha[bestrmse],n[bestrmse]),2))))+scale_y_continuous(limits = c(0,4.2))
summary(fit_range)
range(log10(-fit_range$psi))
fit_best$pf<-log10(-fit_best$psi)
th_mod_dpf<--diff(fit_best$th_mod)/diff(fit_best$pf)
dpf_mod<-fit_best$pf[1:length(fit_best$pf)-1]+diff(fit_best$pf)/2
pore_distr_mod<-data.frame(th_mod_dpf=th_mod_dpf,dpf_mod=dpf_mod)
#porengrößenverteilung
pore_plt<-ggplot(pore_distr)+geom_path(aes(th_norm_dpf,dpf),col=grey(0.3),size=1)+geom_path(data=pore_distr_mod,aes(th_mod_dpf,dpf_mod),col="red",size=1.1)+theme_classic()+scale_y_continuous(limits = c(0,4.2))+labs(x="Porengrößenverteilung [dSe/dpF]",y="pF")
pdf(paste0(plotpfad,"muafit_poresize.pdf"),width=8,height=4.5)
gridExtra::grid.arrange(pore_plt,Ah1plot,layout_matrix=rbind(c(1,rep(2,2))))
dev.off()
#plot speichern
Ah1plot+ggsave(paste0(plotpfad,"muafit.pdf"),width=6,height=4.5)

###############################
#Horizont Ah2
#Bodenfeuchte (th), Sättigungswassergehalt (ths) und residualer Wassergehalt (thr)
#sowie theta norm
#der unterschiedlichen Proben bestimmen
#############################

#alle Proben aus Ah1 auswählen sheet 2
#sheet 2 ist der datensatz im long format
Ah2<-subset(soil.xls2,Horizon=="Ah2"&pf!=7)
#sheet 3
ths_Ah2<-subset(soil.xls,Horizon=="Ah2")

#theta_s ist das Porenvolumen/100
ths_Ah2$ths<-ths_Ah2$PV/100

#theta ist der soilwatercontent/100
Ah2$th<-Ah2$swc/100

#theta_r ist theta bei pf 4.2
thr_Ah2<-Ah2[Ah2$pf==4.2,c(8,10)]
#spaltenname setzen
colnames(thr_Ah2)<-c("MG_ID","thr")
#theta_r an Datensatz anfügen
Ah2<-merge(Ah2,thr_Ah2)
#theta_s an Datensatz anfügen
Ah2<-merge(Ah2,ths_Ah2[,c(1,42)])

#Da keine Messungen für den Wassergesättigten boden vorliegen werden spalten mit th=ths bei pf=0 angefügt
sat_Ah2<-data.frame(th=ths_Ah2$ths,ths=ths_Ah2$ths,MG_ID=ths_Ah2$MG_ID,pf=0,thr=ths_Ah2$swc_15000hPa/100)
#NAs ausschneiden
sat_Ah2<-sat_Ah2[!is.na(sat_Ah2$thr),]
#die Werte für pf=0 werden dem Datensatz angefügt
Ah2<-merge(Ah2,sat_Ah2,all=T)

#Berechnung von theta_norm
Ah2$th_norm<-(Ah2$th-Ah2$thr)/(Ah2$ths-Ah2$thr)
#Berechnung von psi
Ah2$psi<--10^Ah2$pf

########################################
#Paramter alpha und n für Ah1 fitten
#######################################

#aufsteigende sequenz von psi
psi<-sort(unique(Ah2$psi))

#Mittelwerte von theta norm für jedes psi
th_norm<-tapply(Ah2$th_norm,Ah2$psi,mean)

#sequenzen von alpha und n mit allen Kombinationen wie bei Ah1
alpha<-rep(seq(0.05,4,len=n_parseq),n_parseq)
n<-rep(seq(1.1,2,len=n_parseq),each=n_parseq)

#Matrix für Retentionskurven
fit<-matrix(NA,length(n),length(psi))
#Schleife um Kurven zu berechnen
for (i in 1:length(psi)){
  #Berechnung der Van Genuchten Gleichung für i-ten Psi-Wert
  fit[,i]<-(1+(-alpha*psi[i])^n)^-(1-1/n)
}

#Berechnung des RMSE
rmse<-apply(fit,1,function(x) RMSE(obs=th_norm,mod=x))

#RMSE besser als crit auswähen
rmsegood<-which(rmse<=crit)
#bester RMSE
bestrmse<-which.min(rmse)

#realistischer Wertebereich von alpha und n im Ah2
alpha_range_Ah2<-range(alpha[rmsegood])
n_range_Ah2<-range(n[rmsegood])

############################################
#plot der Retentionskurve für Ah2
############################################

#die Retentionskurve beim Grenzwert für realistische alpha und n Werte mit 
#für höher aufgelöstes psi berechnen damit die Kurve im Plot schön glat ist
fit_min<-muafit(Ah2,alpha = min(alpha[rmsegood]),n=min(n[rmsegood]))[[1]]
fit_max<-muafit(Ah2,alpha = max(alpha[rmsegood]),n=max(n[rmsegood]))[[1]]
fit_best<-muafit(Ah2,alpha = max(alpha[bestrmse]),n=max(n[bestrmse]))[[1]]

#um ein Polygon zu plotten wird fit_max rückwärts an fit_min gehängt
#dann wird jeweils das obere und das untere Ende der Linien richtig verbunden
fit_range<-rbind(fit_min,fit_max[order(fit_max$psi,decreasing = T),])

#plot
Ah2plot<-ggplot()+geom_polygon(data=fit_range,aes(th_mod,log10(-psi)),fill="grey",alpha=0.5)+geom_line(data=Ah2,aes(th_norm,pf,col=MG_ID))+geom_line(data=fit_best,aes(th_mod,log10(-psi)))+labs(x=expression(theta[norm]),y="pF")+theme_classic()

Ah2plot

##########################################
#realistische Wertebereich weiterer Bodenparameter
##################################

###########
#theta r und theta s
#Grenzwerte von thr und ths für Ah1 und Ah2 entspricht dem Wertebereich der Messungen
thr<-range(Ah1$thr)
thr2<-range(Ah2$thr)
ths<-range(Ah1$ths)
ths2<-range(Ah2$ths)

############
#Diffusivität

#Die Diffusivität in freier Luft von  CO2 ist 0.152 cm2 s−1 also 9.12 cm2/min
D0<-0.152*60

#epsilon als Luftgehalt bei trockenem Boden pf=4.2
eps<-range(soil.xls$air_15000hPa[soil.xls$Horizon%in%c("Ah1","Ah2")],na.rm = T)/100

#Zusammenhang zwischen epsilon und DS/D0 aus Maier et al. 2012
DispA<-1.5*eps^2.74*D0
#In Hydrus heißt Diffusionskoeffizient von CO2 in Luft DispA
#nicht klar ob effektive Diffusivität im Boden (DS) oder Diffusivität in freier Luft (D0)
#laut Hilfe funktion aber eher zweiteres demnach:
DispA<-9.54

#Pedotransferfunktion von Moldrup et al. 2001 für die Berechnung von DS
ths_dist<-0.45
Ds_max<-ths_dist^1.5*D0

####################
#KS Wert

#tabellenwerte für ks der Bodenmatrix in cm/min für silt loam bis sandy loam
#carsel parrish (1988)
ks_range<-c(0.45/60,4.42/60)


##############################################
#einschätzen ob KS realistisch ist 
#über den zeitlichen Versatz der Bodenfeuchtepeaks mit der Tiefe

#daten einladen
load("C:/Users/ThinkPad/Documents/Masterarbeit/daten/all.R")
r<-7.5#cm Radius
A<-pi*r^2#cm2 area

############
#1. Ansatz
#zeitdifferenz zwischen der negativsten steigung von theta in tiefe -14 und -10 
#zur abschätzung der gesättigten wasserleitfähigkeit

#mehrere plots nebeneinander ermöglichen
par(mfrow=c(2,2))
#vektor um die Zeitdifferenz zu speichern
ks_peak_diff<-1:4
#schleife um Zeitdifferenz für unterschiedliche Events zu berechnen
#die ersten vier Events haben eine hohen Intensität und eignen sich dafür
 for (i in 1:4){
   #subset der unteren beiden Tiefenstufen
   x<-subset(all_list[[i]],tiefe%in%c(-10,-14))
   #zeitpunkt der höchsten Steigung in tiefe -14
   peak14cm<-x$date[x$tiefe==-14][which.min(diff(x$theta[x$tiefe==-14]))-1]
   #zeitpunkt der höchsten Steigung in tiefe -10
   peak10cm<-x$date[x$tiefe==-10][which.min(diff(x$theta[x$tiefe==-10]))-1]
   
   #Zeitdifferenz zwischen den Peaks bestimmen
   ks_peak_diff[i]<-4/as.numeric(difftime(peak14cm,peak10cm,units = "min"))
   #plot von theta
   plot(x$date,x$theta,xlab=i)
   #visualisierung der ausgewählten punkte
   points(peak10cm,x$theta[x$date==peak10cm&x$tiefe==-10],col=2)
   points(peak14cm,x$theta[x$date==peak14cm&x$tiefe==-14],col=3)}
#die negativste Steigung eignet sich bei den ersten beiden Events um den Zeitversatz auszuwählen

############
#2. Ansatz
#zeitdifferenz zwischen erstem wert mit negativerer steigung als 0.001 der höchstens 
#0.1 kleiner ist als der peak

#vektor um die Zeitdifferenz zu speichern
ks_peak_diff2<-1:4

#schleife um Zeitdifferenz mit dem zweiten Ansatz zu berechnen
for (i in 1:4){
  #subset der unteren beiden Tiefenstufen
  x<-subset(all_list[[i]],tiefe%in%c(-10,-14)&!is.na(theta))
  #subsets mit jeweils nur einer tiefenstufe
  x10<-subset(x,tiefe==-10)
  x14<-subset(x,tiefe==-14)
  
  #Zeitpunkte an dem die voraussetzungen erfüllt sind für -14 cm
  peak14cm<-x14$date[c(0,diff(x14$theta))<(-0.001)&x14$theta-max(x14$theta)>-0.1][1]
  #und für -10 cm
  peak10cm<-x10$date[c(0,diff(x10$theta))<(-0.001)&x10$theta-max(x10$theta)>-0.1][1]
  #Zeitdifferenz zwischen den Peaks bestimmen
  ks_peak_diff2[i]<-4/as.numeric(difftime(peak14cm,peak10cm,units = "min"))
  #visualisieren um zu prüfen ob das auswahlkriterium gut ist
  plot(x$date,x$theta,xlab=i)
  points(peak10cm,x10$theta[x10$date==peak10cm],col=2)
  points(peak14cm,x14$theta[which(x14$date==peak14cm)],col=3)
}
#auswahl ist bei allen Events recht passend
par(mfrow=c(1,1))

#die ranges vom KS aus carsel & Parish sind ungefähr im selben Werte Bereich 
#wie die über den Peakversatz bestimmten
range(ks_peak_diff)#cm/min
range(ks_peak_diff2)#cm/min
ks_range#cm/min

###########################################
#Datensatz mit realistischen Wertebereichen
#
#für den ungestörten Boden
#Bestimmung von p_opt siehe script respiration.R
realistic_ranges<-data.frame(alpha=alpha_range_Ah1,alpha2=alpha_range_Ah2,n=n_range_Ah1,n2=n_range_Ah2,p_opt=c(0.00015,0.00022),ks=ks_range,ks2=ks_range)


#und für den gestörten Boden
realistic_ranges_dist<-data.frame(alpha=c(0.02,0.075),alpha2=c(0.02,0.075),n=c(1.41,1.89),n2=c(1.41,1.89),p_opt=c(0.00019,0.00026),ks=ks_range,ks2=ks_range)
#DispA=c(0.5,Ds_max)

#speichern des Datensatzes
save(realistic_ranges,realistic_bulk,realistic_ranges_dist,file=paste0(soilpfad,"ranges.R"))

#tabelle für LATEX erstellen
library(xtable)
tabelleAh1<-cbind(th2=0.75,thr=0.11,realistic_ranges[,c(1,3,5:7)])
tabelleAh2<-cbind(ths=0.64,thr=0.13,realistic_ranges[,c(2,4:7)])
tabelle_dist<-cbind(ths=0.45,thr=0.067,realistic_ranges_dist[,c(1,3,5:6)])

print(xtable((tabelleAh1)),include.rownames = F)
print(xtable((tabelleAh2)),include.rownames = F)
print(xtable((tabelle_dist)),include.rownames = F)
