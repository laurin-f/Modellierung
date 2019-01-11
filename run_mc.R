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


###############################################################
#co2 with changing water paramters
###############################################################
fixed_co2<-data.frame(act_en=6677,
                      h_crit=-10^6,
                      michaelis=0.19,
                      DispW=0.00106181,
                      Disper=5)

loadfiles<-list.files(mcpfad,pattern = ".R")
loadfiles<-loadfiles[loadfiles!="mc_temp.R"]
loadfiles<-substr(loadfiles,1,nchar(loadfiles)-2)
loadfiles_undist<-loadfiles[-grep("dist",loadfiles)]
loadfiles_dist<-loadfiles[grep("dist",loadfiles)]

for(i in 1:length(loadfiles_undist)){
  mc_out(fixed=cbind(fixed,fixed_co2),loadfile = loadfiles_undist[i],treat = "all",ndottys = 1000,sleep = 5,dtmax = 1,Nboot = 100)
}


for(i in 1:length(loadfiles_dist)){
  mc_out(fixed=cbind(fixed_dist,fixed_co2),loadfile = loadfiles_dist[i],treat = "all",ndottys = 1000,sleep = 5,dtmax = 10,obs=alldist_s,min_nrows = 2200,Probe = "dist",Nboot = 100)
}

runname<-str_extract(loadfiles_undist,"-.+")
runname<-substr(runname,2,nchar(runname)) 
runname<-gsub("_"," ",runname)

#events laden
events<-event()

#zeitspanne ausschneiden
event<-subset(events,start>=min(all_s$date)&stop<=max(all_s$date))
event2<-data.frame(start=rep(event$start,4),stop=rep(event$stop,4),tiefe=rep(c(-2,-6,-10,-14),each=nrow(event)))

maindata<-subset(get(loadfiles_undist[1]),tiefe%in%c(-2,-6,-10,-14))
#maindata$start<-ymd_hm(NA)
#maindata$stop<-ymd_hm(NA)
# 
# for (i in c(-2,-6,-10,-14)){
# maindata$start[maindata$tiefe==i][1:nrow(event)]<-event$start
# maindata$stop[maindata$tiefe==i][1:nrow(event)]<-event$stop}

co2plt<-ggplot(data=maindata)+geom_line(aes(date,CO2_raw,linetype=""),col=1)

for(i in 1:length(loadfiles_undist)){
  data<-get(loadfiles_undist[i])
  data$run<-runname[i]
co2plt<-co2plt+geom_line(data=subset(data,tiefe%in%c(-2,-6,-10,-14)),aes(date,CO2_mod,col=run))
}
co2plt+
  geom_rect(data=event2,aes(xmin=start,xmax=stop,ymin = -Inf, ymax = Inf,fill=""), alpha = 0.15)+
  facet_wrap(~as.factor(tiefe),ncol = 1,scales = "free")+
  labs(x="Zeit [Tage]",y=expression("CO"[2]*" [ppm]"),col="mod",linetype="obs")+
  theme_classic()+
  scale_fill_manual(name="Beregnung",values="blue")+
  ggsave(paste0(plotpfad,"co2_mod_undist.pdf"),width=7,height = 9)




mc_out(fixed=cbind(fixed,fixed_co2),loadfile = ,treat = "all",ndottys = 1000,sleep = 5,dtmax = 10)

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

mcca<-mc_parallel(nr=10,ranges = data.frame(calcit=c(0.1,2),
                                           calcit2=c(0.1,2),
                                           difuz=c(0,0.001),
                                           difuz2=c(0,0.001),
                                           disperl=c(1.5,2),
                                           disperl2=c(1.5,2),
                                           bulk=c(0.7,1),
                                           bulk2=c(0.9,1.2),
                                           CaAds=c(50,3000),
                                           CaPrec=c(50,3000),
                                           cec=c(0,3000),
                                           cec2=c(0,3000)),
                fixed=fix_pars,
                sleep=9,
                fit.calcium = F,
                print_times = 1000)
parca<-mcca[[2]]
rmseca<-mcca[[1]]
nseca<-mcca[[3]]
plot(rmseca)

parsca<-cbind(parca[which.min(rmseca),],fix_pars)

outca<-hydrus(params=parsca,)

mc_out(fixed=fix_pars,loadfile = "mc_out-nr_10000-11-30_02.26",treat = "all",ndottys = 300,fit.ca=T)
