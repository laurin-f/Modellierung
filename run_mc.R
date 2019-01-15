hydruspfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/hydrus/"
projektpfad1<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/hydrus/undisturbed/"
projektpfad2<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/hydrus/undisturbed2/"
programmpfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/programme/Hydrus-1D 4.xx/"
mcpfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/hydrus/montecarlo/"
plotpfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/abbildungen/plots/mc/"

load("C:/Users/ThinkPad/Documents/Masterarbeit/daten/all.R")

source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/modellierung/hydrus_input.R")
source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/modellierung/montecarlo_function.R")
source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/modellierung/EET_na.R")

library(ggplot2)
library(stringr)
tiefenstufen<-c(-2,-6,-10,-14)
####################################Z
#Monte Carlo
###################################
fixed<-data.frame(thr=0.11,
                  ths=0.75,
                  thr2=0.13,
                  ths2=0.64,
                  thr3=0.13,
                  ths3=0.64,
                  hseep=-100,
                  l=0.5,
                  bulk=0.7561984,
                  bulk2=1.1480438,
                  difuz=0,
                  disperl=1.7,
                  cec=0,
                  calcit=0.2,
                  CaAds=500,
                  CaPrec=500)

fixed_dist<-data.frame(thr=0.067,
                       ths=0.45,
                       thr2=0.067,
                       ths2=0.45,
                       thr3=0.067,
                       ths3=0.45,
                       hseep=0,
                       l=0.5,
                       bulk=0.7561984,
                       bulk2=1.1480438,
                       difuz=0,
                       disperl=1.7,
                       cec=0,
                       cec2=0,
                       calcit=0.2,
                       CaAds=500,
                       CaPrec=500)

fixed_co2<-data.frame(act_en=6677,
                      h_crit=-10^6,
                      michaelis=0.19,
                      DispW=0.00106181,
                      Disper=5)

fixedca<-data.frame(thr=0.11,
                    ths=0.75,
                    thr2=0.13,
                    ths2=0.64,
                    thr3=0.13,
                    ths3=0.64,
                    hseep=0,
                    l=0.5)
###############################################################
#co2 with changing water paramters
###############################################################


loadfiles<-list.files(mcpfad,pattern = ".R")
loadfiles<-loadfiles[loadfiles!="mc_temp.R"]
loadfiles<-loadfiles[-grep("ca",loadfiles)]
loadfiles<-substr(loadfiles,1,nchar(loadfiles)-2)
loadfiles_undist<-loadfiles[-grep("dist",loadfiles)]
loadfiles_dist<-loadfiles[grep("dist",loadfiles)]

for(i in 1:length(loadfiles_undist)){
  mc_out(fixed=cbind(fixed,fixed_co2),loadfile = loadfiles_undist[i],treat = "all",ndottys = 1000,sleep = 5,dtmax = 1,Nboot = 100)
}


for(i in 1:length(loadfiles_dist)){
  mc_out(fixed=cbind(fixed_dist,fixed_co2),loadfile = loadfiles_dist[i],treat = "all",ndottys = 1000,sleep = 5,dtmax = 10,obs=alldist_s,min_nrows = 2200,Probe = "dist",Nboot = 100)
}

#########################
#plot modellläufe zusammen undist
##########################

runname<-str_extract(loadfiles_undist,"-.+")
runname<-substr(runname,2,nchar(runname)) 
runname<-gsub("_"," ",runname)

#events laden
events<-event()

#zeitspanne ausschneiden
event<-subset(events,start>=min(all_s$date)&stop<=max(all_s$date))
event2<-data.frame(start=rep(event$start,4),stop=rep(event$stop,4),tiefe=rep(c(-2,-6,-10,-14),each=nrow(event)))

maindata<-subset(get(loadfiles_undist[1]),tiefe%in%c(-2,-6,-10,-14))

name_tiefe<-setNames(c("Tiefe = -2 cm","-6 cm","-10 cm","-14 cm"),c(2,6,10,14))
co2plt<-ggplot(data=maindata)+geom_line(aes(date,CO2_raw,linetype=""),col=1)
bfplt<-ggplot(data=maindata)+geom_line(aes(date,theta,linetype=""),col=1)
qplt<-ggplot(data=subset(get(loadfiles_undist[1]),tiefe==-17))+geom_line(aes(date,q_interpol*5,linetype=""),col=1)

for(i in 1:length(loadfiles_undist)){
  data<-get(loadfiles_undist[i])
  data$run<-runname[i]
co2plt<-co2plt+geom_line(data=subset(data,tiefe%in%c(-2,-6,-10,-14)),aes(date,CO2_mod,col=run))
bfplt<-bfplt+geom_line(data=subset(data,tiefe%in%c(-2,-6,-10,-14)),aes(date,theta_mod,col=run))
qplt<-qplt+geom_line(data=subset(data,tiefe==-17),aes(date,q_mod,col=run))

}
co2plt+
  geom_rect(data=event2,aes(xmin=start,xmax=stop,ymin = -Inf, ymax = Inf,fill=""), alpha = 0.15)+
  facet_wrap(~as.factor(-tiefe),labeller = as_labeller(name_tiefe),ncol = 1,scales = "free")+
  labs(title="ungestörte Probe",x="Zeit [Tage]",y=expression("CO"[2]*" [ppm]"),col="mod",linetype="obs")+
  theme_classic()+
  scale_fill_manual(name="Beregnung",values="blue")+
  ggsave(paste0(plotpfad,"co2_mod_undist.pdf"),width=7,height = 9)

bfplt+
  geom_rect(data=event2,aes(xmin=start,xmax=stop,ymin = -Inf, ymax = Inf,fill=""), alpha = 0.15)+
  facet_wrap(~as.factor(-tiefe),labeller = as_labeller(name_tiefe),ncol = 1,scales = "free")+
  labs(title="ungestörte Probe",x="Zeit [Tage]",y=expression("CO"[2]*" [ppm]"),col="mod",linetype="obs")+
  theme_classic()+
  scale_fill_manual(name="Beregnung",values="blue")+
  ggsave(paste0(plotpfad,"bf_mod_undist.pdf"),width=7,height = 9)

qplt+
  geom_rect(data=event,aes(xmin=start,xmax=stop,ymin = -Inf, ymax = Inf,fill=""), alpha = 0.15)+
  labs(title="ungestörte Probe",x="Zeit [Tage]",y=expression("CO"[2]*" [ppm]"),col="mod",linetype="obs")+
  theme_classic()+
  scale_fill_manual(name="Beregnung",values="blue")+
  ggsave(paste0(plotpfad,"q_mod_undist.pdf"),width=7,height = 9)

#######################
#modelläufe zusammmen dist
########################

runname<-str_extract(loadfiles_dist,"-.+")
runname<-substr(runname,2,nchar(runname)) 
runname<-gsub("_"," ",runname)

#events laden
events<-event()

#zeitspanne ausschneiden
event<-subset(events,start>=min(alldist_s$date)&stop<=max(alldist_s$date))
event2<-data.frame(start=rep(event$start,4),stop=rep(event$stop,4),tiefe=rep(c(-2,-6,-10,-14),each=nrow(event)))

maindata<-subset(get(loadfiles_dist[1]),tiefe%in%c(-2,-6,-10,-14))

co2plt<-ggplot(data=maindata)+geom_line(aes(date,CO2_raw,linetype=""),col=1)
bfplt<-ggplot(data=maindata)+geom_line(aes(date,theta,linetype=""),col=1)
qplt<-ggplot(data=subset(get(loadfiles_dist[1]),tiefe==-17))+geom_line(aes(date,q_interpol*5,linetype=""),col=1)

for(i in 1:length(loadfiles_dist)){
  data<-get(loadfiles_dist[i])
  data$run<-runname[i]
  co2plt<-co2plt+geom_line(data=subset(data,tiefe%in%c(-2,-6,-10,-14)),aes(date,CO2_mod,col=run))
  bfplt<-bfplt+geom_line(data=subset(data,tiefe%in%c(-2,-6,-10,-14)),aes(date,theta_mod,col=run))
  qplt<-qplt+geom_line(data=subset(data,tiefe==-17),aes(date,q_mod,col=run))
}
co2plt+
  geom_rect(data=event2,aes(xmin=start,xmax=stop,ymin = -Inf, ymax = Inf,fill=""), alpha = 0.15)+
  facet_wrap(~as.factor(-tiefe),labeller = as_labeller(name_tiefe),ncol = 1,scales = "free")+
  labs(title="gestörte Probe",x="Zeit [Tage]",y=expression("CO"[2]*" [ppm]"),col="mod",linetype="obs")+
  theme_classic()+
  scale_fill_manual(name="Beregnung",values="blue")+
  ggsave(paste0(plotpfad,"co2_mod_dist.pdf"),width=7,height = 9)

bfplt+
  geom_rect(data=event2,aes(xmin=start,xmax=stop,ymin = -Inf, ymax = Inf,fill=""), alpha = 0.15)+
  facet_wrap(~as.factor(-tiefe),labeller = as_labeller(name_tiefe),ncol = 1,scales = "free")+
  labs(title="ungestörte Probe",x="Zeit [Tage]",y=expression("CO"[2]*" [ppm]"),col="mod",linetype="obs")+
  theme_classic()+
  scale_fill_manual(name="Beregnung",values="blue")+
  ggsave(paste0(plotpfad,"bf_mod_dist.pdf"),width=7,height = 9)

qplt+
  geom_rect(data=event,aes(xmin=start,xmax=stop,ymin = -Inf, ymax = Inf,fill=""), alpha = 0.15)+
  labs(title="ungestörte Probe",x="Zeit [Tage]",y=expression("CO"[2]*" [ppm]"),col="mod",linetype="obs")+
  theme_classic()+
  scale_fill_manual(name="Beregnung",values="blue")+
  ggsave(paste0(plotpfad,"q_mod_dist.pdf"),width=7,height = 9)

######################################################################


loadfile<-"mc_60000-realistic"
load(file = paste0(mcpfad,loadfile,".R"))

par<-mc[[2]]
rmse<-mc[[1]]
fix_pars<-cbind(par[which.min(rmse),],fixedca,fixed_co2)

mc_out(fixed=fix_pars,loadfile = "mc_90000-ca_realistic" ,treat = "all",ndottys = 1000,sleep = 5,dtmax = 10,fit.ca = T)

mc_out(fixed=cbind(fixed,fixed_co2),loadfile = "mc_120000-free_ranges",treat = "all",ndottys = 1000,sleep = 5,dtmax = 1)


mc_out(fixed=cbind(fixed,fixed_co2),loadfile = "mc_60000-realistic_free_ks",treat = "all",ndottys = 1000,sleep = 5,dtmax = 10)



mc_out(fixed=cbind(fixed_dist,fixed_co2),loadfile = "mc_120000-free_dist",treat = "all",ndottys = 1000,sleep = 5,dtmax = 10,obs=alldist_s,min_nrows = 2200)

get("mc_120000-free-ranges")

###############################################################
#co2 with changing water paramters realistic ranges
###############################################################

fixed_co2<-data.frame(act_en=6677,
                      h_crit=-10^6,
                      michaelis=0.19,
                      DispW=0.00106181,
                      Disper=5)

load("C:/Users/ThinkPad/Documents/Masterarbeit/daten/bodenparameter/ranges.R")
ranges<-cbind(realistic_ranges,data.frame(ks3=c(0.0001,0.01),
                                          h_opt=c(-80,-10),
                                          p_distr=c(0.001,0.2)))

mc<-mc_parallel(nr=11,sleep=5,ranges=ranges,
                 fixed=cbind(fixed,fixed_co2),
                n_nodes = 9,Mat = c(rep(1,3),rep(2,5),3))


#save(mc,file = paste0(mcpfad,"mc_wp_co2-",Sys.Date(),".R"))
loadfile<-"mc_out-nr_20000-11-25_12.02"
#load(file = paste0(mcpfad,loadfile,".R"))

loadfile<-"mc_temp"
#load(file = paste0(mcpfad,loadfile,".R"))

mc_out(fixed=cbind(fixed,fixed_co2),loadfile = "mc_out-nr_20000-11-29_07.47",treat = "all",ndottys = 300)

#######################
#caplot

capath<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/ca/"
load(file=paste0(capath,"cafm.R"))

out$tiefe<-as.numeric(out$tiefe)
library(ggplot2)
legendtitle<-expression("Intensität [mm*h"^{-1}*"]")
ggplot()+
  geom_point(data=subset(ic,!is.na(rain_mm_h)),aes(ca,tiefe,col=as.factor(round(rain_mm_h)),shape=as.factor(round(rain_mm_h))))+
  geom_point(data=subset(out,tiefe%in%tiefenstufen&!is.na(Ca_mod)),aes(Ca_mod,tiefe))+
  labs(x=expression("Ca"^{"2+"}*"  [mg * l"^{-1}*"]"),y="tiefe [cm]",col=legendtitle,shape=legendtitle)+theme_classic()

  ggplot()+geom_point(data=subset(out,tiefe%in%tiefenstufen&!is.na(Ca_mod)),aes(Ca_mod,tiefe))

######################
#calcium mc
######################

fixed<-data.frame(thr=0.11,
                  ths=0.75,
                  thr2=0.13,
                  ths2=0.64,
                  thr3=0.13,
                  ths3=0.64,
                  hseep=0,
                  l=0.5,
                  cec=0,
                  cec2=0)
fixed_co2<-data.frame(act_en=6677,
                      h_crit=-10^6,
                      michaelis=0.19,
                      DispW=0.00106181,
                      Disper=5)
loadfile<-"mc_out-nr_4000-11-29_15.50"
load(file = paste0(mcpfad,loadfile,".R"))

par<-mc[[2]]
rmse<-mc[[1]]
nse<-mc[[3]]
fix_pars<-cbind(par[which.min(rmse),],fixed,fixed_co2)


parca<-mcca[[2]]
rmseca<-mcca[[1]]
nseca<-mcca[[3]]
plot(rmseca)

parsca<-cbind(parca[which.min(rmseca),],fix_pars)

outca<-hydrus(params=parsca,)

mc_out(fixed=fix_pars,loadfile = "mc_out-nr_10000-11-30_02.26",treat = "all",ndottys = 300,fit.ca=T)



######################
#EE outpput from Matlab function
misi<-list.files(mcpfad,pattern = "mi|si.*.csv")
mc_types<-stringr::str_replace(loadfiles,"mc_\\d+(_|-)","")
mc_types<-paste0("_-",mc_types,"\\.")

for (j in 1:length(mc_types)){
tempfile<-misi[grep(mc_types[j],misi)]

misi_val<-matrix(NA,11,length(tempfile))
for (i in 1:length(tempfile)){

misi_val[,i]<-t(read.csv(paste0(mcpfad,tempfile[i]),header = F))
}

load(file = paste0(mcpfad,loadfiles[j],".R"))
  par<-mc[[2]]
  
library(stringr)
EET_oct<-as.data.frame(misi_val)
colnames(EET_oct)<-gsub("_-.+","",tempfile)
EET_oct$id<-colnames(par)
EET_oct$par<-str_replace(colnames(par),"2|3","")
mat<-str_extract(colnames(par),"2|3")
EET_oct$Mat<-ifelse(is.na(mat),"1",mat)
colors<-factor(EET_oct$par,labels = setNames(c(2:6,"orange","darkgreen"),unique(EET_oct$par)))
colors<-as.character(colors)
library(dplyr)
shapes<-factor(EET_oct$Mat,labels = setNames(c(16,17,15),unique(EET_oct$Mat)))
shapes<-as.numeric(as.character(shapes))
names<-c(expression(alpha[1],alpha[2],D[a],h[opt],K[S1],K[S2],K[S3],n[1],n[2],P[distr],P[opt]))

# Plot results in the plane (mean(EE),std(EE)):
print("saving GSA plot")

library(ggplot2)

ggplot(EET_oct)+geom_rect(aes(xmin=mi_lb,xmax=mi_ub,ymin=sigma_lb,ymax=sigma_ub,fill=id),col=0,alpha=0.15,show.legend = F)+geom_point(aes(mi,sigma,col=id,shape=id),size=2)+theme_classic()+scale_shape_manual(name="Parameter",labels=names,values = shapes[order(colnames(par))])+scale_color_manual(name="Parameter",labels=names,values = colors[order(colnames(par))])+scale_fill_manual(name="",labels=names,values = colors[order(colnames(par))])+ggsave(paste0(plotpfad,"/EE/","M_",gsub("(mi_-|.csv)","",tempfile[1]),".pdf"),width=7,height=4)
}

