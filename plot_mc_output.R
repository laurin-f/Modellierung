#pfade definieren
hydruspfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/hydrus/"
projektpfad1<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/hydrus/undisturbed/"
projektpfad2<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/hydrus/undisturbed2/"
programmpfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/programme/Hydrus-1D 4.xx/"
mcpfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/hydrus/montecarlo/"
plotpfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/abbildungen/plots/mc/"

#datensatz all laden
load("C:/Users/ThinkPad/Documents/Masterarbeit/daten/all.R")

#skripte mit Funktionen ausführen
source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/modellierung/hydrus_input.R")
source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/modellierung/mc_out_function.R")
source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/modellierung/EET_na.R")
source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/durchf-hrung/event.R")

#packages laden
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
                  hseep=0,
                  l=0.5,
                  bulk=0.7561984,
                  bulk2=1.1480438,
                  difuz=0,
                  disperl=1.7,
                  cec=140,#aus scheffer schachtschabel tabelle parabraunerde KAKeff
                  calcit=0.2,
                  CaAds=500,
                  CaPrec=500,
                  DispA=9.54)

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
                       cec=140,
                       calcit=0.2,
                       CaAds=500,
                       CaPrec=500,
                       DispA=9.54)



fixed_co2<-data.frame(act_en=6677,
                      h_crit=-10^6,
                      michaelis=0.19,
                      DispW=0.00106181,
                      Disper=5)

###############################################################
#mc_out funktion anwenden für undist
###############################################################

loadfiles<-c("mc_55000-free_ranges","mc_55000-realistic_ranges")
loadfiles_undist<-paste0(c("fit_CO2-_","fit_Ca-_","fit_both-_"),rep(loadfiles,each=3))


rmse_norms<-matrix(NA,3,length(loadfiles_undist))
pars_tab<-matrix(NA,11,length(loadfiles_undist))
std_tab<-matrix(NA,11,length(loadfiles_undist))

colnames(rmse_norms)<-gsub("-_mc_55000-|_"," ",loadfiles_undist)
colnames(pars_tab)<-colnames(rmse_norms)
colnames(std_tab)<-colnames(rmse_norms)

for (i in 1:3){
  for (j in 1:length(loadfiles)){
  mc_out(fixed=cbind(fixed,fixed_co2),loadfile = loadfiles[j],dtmax = 10,kin_sol = T,plot = T,rmse_pos = c(1,4,5)[i],Nboot = 100,ndottys = 10000,hide_hydrus = T)
    rmse_norms[,i+3*(j-1)]<-c(rmse_co2,rmse_ca,rmse_both)
    
    std_tab[1:10,i+3*(j-1)]<-std
    pars_tab[11,i+3*(j-1)]<-c(rmse_co2,rmse_ca,rmse_both)[i]
    pars_tab[1:10,i+3*(j-1)]<-t(pars_opt)
}}

########################
#tabelle rmse norms
########################
rownames(rmse_norms)<-c("rmse CO2","rmse Ca","rmse both")
fit<-str_extract(colnames(rmse_norms),"fit (both|CO2|Ca)")
colnames_rmse_norms<-gsub("fit (both|CO2|Ca)","",colnames(rmse_norms))
colnames_rmse_norms<-gsub("(^\\s+)|(\\s+$)","",colnames_rmse_norms)
rmse_norms2<-rmse_norms[,order(colnames_rmse_norms,fit)]
fit2<-fit[order(colnames(rmse_norms),fit)]

xtable::xtable(rmse_norms2)

##############################
#tabelle parameter
###############################

par_opt_tab<-matrix(NA,11,12)
colnames(par_opt_tab)<-paste(rep(colnames(pars_tab),each=2),c("","sd"))
rownames(par_opt_tab)<-c(colnames_par,"RMSE")
rownames(pars_tab)<-rownames(par_opt_tab)
rownames(std_tab)<-rownames(par_opt_tab)
par_opt_tab[,seq(1,11,2)]<-pars_tab
par_opt_tab[,seq(2,12,2)]<-std_tab
par_opt_tab<-par_opt_tab[,-grep("Ca free|both free",colnames(par_opt_tab))]
par_opt_tab<-par_opt_tab[,order(colnames(par_opt_tab))]

par_opt_tab2<-t(apply(par_opt_tab,1,function(x)as.character(signif(x,2))))
colnames(par_opt_tab2)<-colnames(par_opt_tab)
rownames(par_opt_tab2)<-c("$\\alpha_1$","$\\alpha_2$","h$_{opt}$","K$_{S1}$","K$_{S2}$","K$_{S3}$","n$_1$","n$_2$","P$_{distr}$","P$_{opt}$","RMSE$_{norm}$")
par_opt_tab2[1:10,seq(2,ncol(par_opt_tab2),2)]<-paste("\\cellcolor{lightgray}",par_opt_tab2[1:10,seq(2,ncol(par_opt_tab2),2)])
print(xtable::xtable(par_opt_tab2),sanitize.rownames.function=identity,sanitize.text.function=identity)

###############################################################
#mc_out funktion anwenden für dist
###############################################################

loadfiles2<-c("mc_55000-dist_free_ranges","mc_55000-dist_realistic_ranges","mc_55000-dist_fit_tiefe_1-2")
loadfiles_dist<-paste0("fit_CO2-_",loadfiles2)

par_tab_dist<-matrix(NA,11,length(loadfiles_dist))
std_tab_dist<-matrix(NA,11,length(loadfiles_dist))

colnames(par_tab_dist)<-gsub("mc_..000-dist_"," ",loadfiles2)
colnames(std_tab_dist)<-colnames(par_tab_dist)

  for (i in 1:length(loadfiles2)){
      
    fixed_pars<-cbind(fixed_dist,fixed_co2)
    mc_out(fixed=fixed_pars,loadfile = loadfiles2[i],dtmax = 10,kin_sol = F,plot = T,rmse_pos = 1,Nboot = 100,ndottys = 10000,hide_hydrus = T,obs=alldist_s,traintime = 8000,Probe = "dist",n_best = c(1,2,2,1)[i])
    
      std_tab_dist[1:10,i]<-std
      par_tab_dist[11,i]<-rmse_co2
      par_tab_dist[1:10,i]<-t(pars_opt)
  }


##############################
#tabelle parameter
###############################

par_opt_tab_dist<-matrix(NA,11,2*length(loadfiles_dist))
colnames(par_opt_tab_dist)<-paste(rep(colnames(par_tab_dist),each=2),c("","sd"))
rownames(par_opt_tab_dist)<-c(colnames_par,"RMSE")
rownames(par_tab_dist)<-rownames(par_opt_tab_dist)
rownames(std_tab_dist)<-rownames(par_opt_tab_dist)
par_opt_tab_dist[,seq(1,ncol(par_opt_tab_dist)-1,2)]<-par_tab_dist
par_opt_tab_dist[,seq(2,ncol(par_opt_tab_dist),2)]<-std_tab_dist
par_opt_tab_dist<-par_opt_tab_dist[,order(colnames(par_opt_tab_dist))]

par_opt_tab_dist2<-t(apply(par_opt_tab_dist,1,function(x)as.character(signif(x,2))))
colnames(par_opt_tab_dist2)<-colnames(par_opt_tab_dist)
rownames(par_opt_tab_dist2)<-c("$\\alpha_1$","$\\alpha_2$","h$_{opt}$","K$_{S1}$","K$_{S2}$","K$_{S3}$","n$_1$","n$_2$","P$_{distr}$","P$_{opt}$","RMSE$_{norm}$")
par_opt_tab_dist2[1:10,seq(2,ncol(par_opt_tab_dist2),2)]<-paste("\\cellcolor{lightgray}",par_opt_tab_dist2[1:10,seq(2,ncol(par_opt_tab_dist2),2)])
print(xtable::xtable(par_opt_tab_dist2),sanitize.rownames.function=identity,sanitize.text.function=identity)


#############################
#EE plots für  Ergebnisse undist
###############################
EE_co2_free<-get(paste0(loadfiles_undist[1],"EET_plt"))+theme(legend.position = "none")+labs(title=expression("fit CO"[2]),subtitle="free ranges")
EE_co2_real<-get(paste0(loadfiles_undist[4],"EET_plt"))+labs(y="",title="",subtitle="realistic ranges")

EE_both_free<-get(paste0(loadfiles_undist[3],"EET_plt"))+theme(legend.position = "none")+labs(title="fit both",subtitle="free ranges")
EE_both_real<-get(paste0(loadfiles_undist[6],"EET_plt"))+labs(y="",title="",subtitle="realistic ranges")
layout.mat<-matrix(2,60,20)
layout.mat[1,]<-3
layout.mat[,1:9]<-1

pdf(paste0(plotpfad,"EE_fit_both.pdf"),height = 3.5,width = 7)
gridExtra::grid.arrange(EE_both_free,EE_both_real,ncol=2,layout_matrix=layout.mat)
dev.off()

pdf(paste0(plotpfad,"EE_fit_CO2.pdf"),height = 3.5,width = 7)
gridExtra::grid.arrange(EE_co2_free,EE_co2_real,ncol=2,layout_matrix=layout.mat)
dev.off()


#############################
#EE plots für  Ergebnisse dist
###############################

EE_co2_free_dist<-get(paste0(loadfiles_dist[1],"EET_plt"))+theme(legend.position = "none")+labs(title="gestörte Probe",subtitle="free ranges")
EE_co2_real_dist<-get(paste0(loadfiles_dist[2],"EET_plt"))+labs(y="",title="",subtitle="realistic ranges")




pdf(paste0(plotpfad,"EE_fit_CO2_dist.pdf"),height = 3.5,width = 7)
gridExtra::grid.arrange(EE_co2_free_dist,EE_co2_real_dist,ncol=2,layout_matrix=layout.mat)
dev.off()


################
#tabelle co2mean
#Output des MC-Laufs laden
load(file = paste0(mcpfad,"mc_55000-realistic_ranges",".R"))
  #der Output ist in die Liste mc geschrieben
  #die einzelnen listenelemente auspacken
  par<-mc[[2]]
  rmse<-mc[[1]]
  nse<-mc[[3]]
  pars<-cbind(par[which.min(rmse),],fixed,fixed_co2)
  
  #mit function das Modell ausführen und output laden
  out<-hydrus(params = pars,
              UNSC=T,
              hide_hydrus=F,
              free_drain=T,
              print_times = 2000,
              dtmax = 10,
              obs=all,
              min_nrows=100,
              kin_sol=T)
data1<-out
data1_s<-get(loadfiles_undist[4])
ohne_warmup<-data1$t_min[!is.na(data1$rain_mm_h)][which(diff(data1$rain_mm_h[!is.na(data1$rain_mm_h)])>0)[2]]
ohne_warmup_s<-data1_s$t_min[!is.na(data1_s$rain_mm_h)][which(diff(data1_s$rain_mm_h[!is.na(data1_s$rain_mm_h)])>0)[2]]
pars$p_opt
pars$p_distr
max(out$vProd,na.rm = T)

pars$p_distr<-0.2
#testen ob mit hohem p_dist die höchsten Prod-werte im modell näher an p_opt dran sind

out<-hydrus(params = pars,
            UNSC=T,
            hide_hydrus=F,
            free_drain=T,
            print_times = 2000,
            dtmax = 10,
            obs=all,
            min_nrows=100,
            kin_sol=T)
#ja 
pars$p_opt
pars$p_distr
max(out$vProd,na.rm = T)

plot(data1$vProd)
#subset des outputs ohne warm-up-event
data<-subset(data1,t_min>=ohne_warmup&tiefe%in%c(-2,-6,-10,-14,-17))
data_s<-subset(data1_s,t_min>=ohne_warmup_s)

data$CO2_q<-data$CO2_mod*data$q_mod
data$SI_q<-data$SI*data$q_mod

data$CO2_obs_q<-data$CO2_raw*data$q_mod


data$CO2_theta<-data$CO2_mod*data$theta_mod
data$SI_theta<-data$SI*data$theta_mod

aggs<-aggregate(data.frame(q=data$q_mod,CO2=data$CO2_mod,CO2_q=data$CO2_q,SI=data$SI,SI_q=data$SI_q,ca_mod=data$Ca_mod,CO2_obs=data$CO2_raw,CO2_obs_q=data$CO2_obs_q,ca_obs=data$ca_conc,q2=ifelse(is.na(data$CO2_raw),NA,data$q_mod)),list(treatment=data$treatment),function(x) mean(x,na.rm=T))

ca_we_sum<-aggregate(data_s$Ca_weather,list(data_s$treatment),function(x) sum(x,na.rm=T))

aggs$CO2_q<-aggs$CO2_q/aggs$q
aggs$CO2_obs_q<-aggs$CO2_obs_q/aggs$q2
aggs$SI_q<-aggs$SI_q/aggs$q

aggs$ca_verwitterung<-ca_we_sum$x#meq/kg

print(xtable::xtable(aggs[,-c(2,5,8:11)]),include.rownames = F)
print(xtable::xtable(aggs[,c(1,8:10)]),include.rownames = F)


#########################
#plot modellläufe zusammen undist
##########################


#runname<-str_extract(loadfiles_undist,"-.+")
#runname<-substr(runname,2,nchar(runname)) 
#runname<-gsub("_"," ",runname)
runname<-gsub("-_mc_55000-|_"," ",loadfiles_undist)


#events laden
events<-event()

#zeitspanne ausschneiden
event1<-subset(events,start>=min(all_s$date)&stop<=max(all_s$date))
event2<-data.frame(start=rep(event1$start,4),stop=rep(event1$stop,4),tiefe=rep(c(-2,-6,-10,-14),each=nrow(event1)))

maindata<-subset(get(loadfiles_undist[1]),tiefe%in%c(-2,-6,-10,-14))

name_tiefe<-setNames(c("Tiefe = -2 cm","-6 cm","-10 cm","-14 cm"),c(2,6,10,14))

co2plt<-ggplot(data=maindata)+geom_line(aes(date,CO2_raw,linetype=""),col=1)
bfplt<-ggplot(data=maindata)+geom_line(aes(date,theta,linetype=""),col=1)
qplt<-ggplot(data=subset(get(loadfiles_undist[1]),tiefe==-17))+geom_line(aes(date,q_interpol*5,linetype=""),col=1)

pHplt<-ggplot(data=subset(get(loadfiles_undist[4]),!is.na(pH)))+geom_line(aes(t_min,pH,linetype="",col=tiefe))

caplt<-ggplot(data=subset(get(loadfiles_undist[4])))+geom_line(aes(t_min,Ca_mod,col=as.factor(tiefe)))


SIplt<-ggplot(data=subset(get(loadfiles_undist[4]),!is.na(SI)&tiefe%in%c(-2,-6,-10,-14,-17)))+geom_line(aes(t_min,SI,col=as.factor(tiefe)))


ggplot(data=subset(get(loadfiles_undist[4]),!is.na(P_0)))+geom_line(aes(date,P_2_korr,col="3"))+geom_line(aes(date,P_4_korr,col="4"))+geom_line(aes(date,cvTop,col="1"))+geom_line(aes(date,vProd,col="2"))+
  geom_rect(data=event1,aes(xmin=start,xmax=stop,ymin = -Inf, ymax = Inf,fill=""), alpha = 0.15)+theme_classic()+
  labs(x="",y=expression("CO"[2]*" [ml  cm"^-2*" min"^-1*"]"),col="")+scale_color_discrete(labels=c("Flux 0 cm",paste("Prod <",c(0,-2,-4),"cm")))+
  theme_classic()+
  scale_fill_manual(name="Beregnung",values="blue")+
  ggsave(paste0(plotpfad,"CO2_Prod_flux.pdf"),width=7,height = 4)


for(i in (1:length(loadfiles_undist))[-grep("fit_Ca",loadfiles_undist)]){
  data<-get(loadfiles_undist[i])
  data$run<-runname[i]
co2plt<-co2plt+geom_line(data=subset(data,tiefe%in%c(-2,-6,-10,-14)),aes(date,CO2_mod,col=run))
bfplt<-bfplt+geom_line(data=subset(data,tiefe%in%c(-2,-6,-10,-14)),aes(date,theta_mod,col=run))
qplt<-qplt+geom_line(data=subset(data,tiefe==-17),aes(date,q_mod,col=run))

}
co2plt+
  geom_rect(data=event2,aes(xmin=start,xmax=stop,ymin = -Inf, ymax = Inf,fill=""), alpha = 0.15)+
  facet_wrap(~as.factor(-tiefe),labeller = as_labeller(name_tiefe),ncol = 1,scales = "free")+
  labs(title="ungestörte Probe",x="",y=expression("CO"[2]*" [ppm]"),col="mod",linetype="obs")+
  theme_classic()+
  scale_fill_manual(name="Beregnung",values="blue")+guides(color = guide_legend(order=2),linetype = guide_legend(order=1),fill = guide_legend(order=3))+
  ggsave(paste0(plotpfad,"co2_mod_undist.pdf"),width=7,height = 9)

bfplt+
  geom_rect(data=event2,aes(xmin=start,xmax=stop,ymin = -Inf, ymax = Inf,fill=""), alpha = 0.15)+
  facet_wrap(~as.factor(-tiefe),labeller = as_labeller(name_tiefe),ncol = 1,scales = "free")+
  labs(title="ungestörte Probe",x="",y=expression(theta*" [Vol %]"),col="mod",linetype="obs")+
  theme_classic()+
  scale_fill_manual(name="Beregnung",values="blue")+guides(color = guide_legend(order=2),linetype = guide_legend(order=1),fill = guide_legend(order=3))+
  ggsave(paste0(plotpfad,"bf_mod_undist.pdf"),width=7,height = 6)

qplt+
  geom_rect(data=event1,aes(xmin=start,xmax=stop,ymin = -Inf, ymax = Inf,fill=""), alpha = 0.15)+
  labs(title="ungestörte Probe",x="",y=expression("q"*" [ml / min]"),col="mod",linetype="obs * 5")+
  theme_classic()+
  scale_fill_manual(name="Beregnung",values="blue")+guides(color = guide_legend(order=2),linetype = guide_legend(order=1),fill = guide_legend(order=3))+
  ggsave(paste0(plotpfad,"q_mod_undist.pdf"),width=7,height = 3)

#######################
#modelläufe zusammmen dist
########################

runname<-gsub("mc_55000-dist_|_"," ",loadfiles2)


#events laden
events<-event()
#zeitspanne ausschneiden
event1<-subset(events,start>=min(alldist_s$date)&stop<=max(alldist_s$date))
event2<-data.frame(start=rep(event1$start,4),stop=rep(event1$stop,4),tiefe=rep(c(-2,-6,-10,-14),each=nrow(event1)))

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
  labs(title="gestörte Probe",x="",y=expression("CO"[2]*" [ppm]"),col="mod",linetype="obs")+
  theme_classic()+
  scale_fill_manual(name="Beregnung",values="blue")+guides(color = guide_legend(order=2),linetype = guide_legend(order=1),fill = guide_legend(order=3))+
  ggsave(paste0(plotpfad,"co2_mod_dist.pdf"),width=7,height = 9)

bfplt+
  geom_rect(data=event2,aes(xmin=start,xmax=stop,ymin = -Inf, ymax = Inf,fill=""), alpha = 0.15)+
  facet_wrap(~as.factor(-tiefe),labeller = as_labeller(name_tiefe),ncol = 1,scales = "free")+
  labs(title="gestörte Probe",x="",y=expression(theta*" [Vol %]"),col="mod",linetype="obs")+
  theme_classic()+
  scale_fill_manual(name="Beregnung",values="blue")+guides(color = guide_legend(order=2),linetype = guide_legend(order=1),fill = guide_legend(order=3))+
  ggsave(paste0(plotpfad,"bf_mod_dist.pdf"),width=7,height = 6)

qplt+
  geom_rect(data=event1,aes(xmin=start,xmax=stop,ymin = -Inf, ymax = Inf,fill=""), alpha = 0.15)+
  labs(title="gestörte Probe",x="",y=expression("q"*" [ml / min]"),col="mod",linetype="obs * 5")+
  theme_classic()+
  scale_fill_manual(name="Beregnung",values="blue")+guides(color = guide_legend(order=2),linetype = guide_legend(order=1),fill = guide_legend(order=3))+
  ggsave(paste0(plotpfad,"q_mod_dist.pdf"),width=7,height = 3)

######################################################################


#########################
#plot modellläufe zusammen calcium
##########################

runname<-gsub("-_mc_55000-|_"," ",loadfiles_undist)

#zeitspanne ausschneiden
event1<-subset(events,start>=min(all_s$date)&stop<=max(all_s$date))

caplt<-ggplot(data=subset(get(loadfiles_undist[1]),tiefe==-17))+geom_line(aes(date,ca_conc,linetype=""),col=1)

for(i in (1:length(loadfiles_undist))[-grep("CO.+realistic|bo.+realistic",loadfiles_undist)]){
  data<-get(loadfiles_undist[i])
  data$run<-runname[i]
  data$date2<-data$date[data$t_min==0&data$tiefe==-17]+data$t_min*60
  caplt<-caplt+geom_line(data=subset(data,tiefe==-17),aes(date2,Ca_mod,col=run))
  
}
caplt+
  geom_rect(data=event1,aes(xmin=start,xmax=stop,ymin = -Inf, ymax = Inf,fill=""), alpha = 0.15)+
  labs(title="ungestörte Probe",x="",y=expression("Ca"^{2+""}*" [mg / l]"),col="mod",linetype="obs")+
  theme_classic()+
  scale_fill_manual(name="Beregnung",values="blue")+scale_y_continuous(limits = c(40,NA))+guides(color = guide_legend(order=2),linetype = guide_legend(order=1),fill = guide_legend(order=3))+
  ggsave(paste0(plotpfad,"ca_mod_undist.pdf"),width=7,height = 4)

