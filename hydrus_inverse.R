hydruspfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/hydrus/"
projektpfad1<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/hydrus/undisturbed/"
projektpfad2<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/hydrus/undisturbed2/"
programmpfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/programme/Hydrus-1D 4.xx/"
mcpfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/hydrus/montecarlo/"

load("C:/Users/ThinkPad/Documents/Masterarbeit/daten/all.R")
load("C:/Users/ThinkPad/Documents/Masterarbeit/daten/bodenparameter/params.R")


source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/modellierung/hydrus_input.R")


tmax_all<-as.numeric(difftime(max(all$date),min(all$date),units = "min"))

atmos.in(17,180,6000,projektpfad = projektpfad2)
load(file = paste0(mcpfad,"mc","2018-11-15",".R"))

par<-mc[[2]]
rmse<-mc[[1]]

#pars<-cbind(par[which.min(rmse),],fixed)
pars<-par[which.min(rmse),]

selector.in(params = pars,
            projektpfad = projektpfad2,
            tmax= 6000)

# selector.in(params = data.frame(thr=0.11,
#                                 ths=0.75,
#                                 alpha=0.231464,
#                                 n=c(1.8,1.8),
#                                 ks=c(1,1),
#                                 thr_bot=c(0.05),
#                                 ths_bot=c(0.6),
#                                 alpha_bot=c(0.6),
#                                 n_bot=c(1.8),
#                                 ks_bot=c(0.2),
#                                 l=0.5,
#                                 hseep = -100),
#             tmax = 4000,
#             projektpfad = projektpfad2)


profile.in(th=seq(0.11,0.2,len=18),Mat = c(rep(1,10),rep(2,7),3),projektpfad = projektpfad2)
fit.in(params = pars,q_fit = T)

hydrus.exe(sleep=20,file = "undisturbed2",UNSC=F,Inverse = T)


out<-read_hydrus.out(treat=17,
                     projektpfad=projektpfad2,
                     UNSC=F)[[1]]


library(ggplot2)
tiefenstufen<-c(-2,-6,-10,-14)
ggplot(subset(out,tiefe%in%tiefenstufen))+geom_point(aes(t_min,theta,col="obs"),na.rm = T)+geom_point(aes(t_min,theta_mod,col="mod"),na.rm = T)+facet_wrap(~tiefe,ncol=1)

ggplot(subset(out,tiefe==-17))+geom_point(aes(t_min,q_interpol*5,col="obs"),na.rm = T)+geom_point(aes(t_min,q_mod,col="mod"),na.rm = T)


fixed<-data.frame(thr=0.11,
                  ths=0.75,
                  thr2=0.13,
                  ths2=0.64,
                  thr_bot=0.13,
                  ths_bot=0.64,
                  hseep=-100,
                  l=0.5)

pars_inv<-cbind(fit.out(),fixed)



selector.in(params = pars_inv,
            projektpfad = projektpfad2,
            tmax= tmax_all)
atmos.in(alle=T,total_t = tmax_all,tmax_all,projektpfad = projektpfad2)

profile.in(th=seq(0.11,0.2,len=18),Mat = c(rep(1,10),rep(2,7),3),projektpfad = projektpfad2)
hydrus.exe(sleep=1,file = "undisturbed2",UNSC=F,Inverse = F)


out<-read_hydrus.out(treat="all",
                     projektpfad=projektpfad2,
                     UNSC=F)[[1]]


selector.in(params = pars_inv,co2_params = data.frame(p_opt=0.001,
                                                      act_en=6677,
                                                      h_opt=-50,
                                                      h_crit=-10^6),
            projektpfad = projektpfad1,
            tmax= tmax_all)
atmos.in(alle=T,total_t = tmax_all,tmax_all,projektpfad = projektpfad1)

profile.in(th=seq(0.11,0.2,len=18),Mat = c(rep(1,10),rep(2,7),3),projektpfad = projektpfad1)
hydrus.exe(sleep=1,file = "undisturbed",UNSC=T,Inverse = F)


out<-read_hydrus.out(treat="all",
                     projektpfad=projektpfad2,
                     UNSC=F)[[1]]


library(ggplot2)
tiefenstufen<-c(-2,-6,-10,-14)
ggplot(subset(out,tiefe%in%tiefenstufen))+geom_point(aes(t_min,theta,col="obs"),na.rm = T)+geom_point(aes(t_min,theta_mod,col="mod"),na.rm = T)+facet_wrap(~tiefe,ncol=1)

ggplot(subset(out,tiefe==-17&q_mod>0))+geom_point(aes(t_min,q_interpol*5,col="obs"),na.rm = T)+geom_point(aes(t_min,q_mod,col="mod"),na.rm = T)



###################################
#all
#####################################
atmos.in(alle=T,total_t = tmax_all,projektpfad = projektpfad2)

load(file = paste0(mcpfad,"mc","2018-11-15",".R"))

par<-mc[[2]]
rmse<-mc[[1]]

#pars<-cbind(par[which.min(rmse),],fixed)
pars<-par[which.min(rmse),]

selector.in(params = pars,
            projektpfad = projektpfad2,
            tmax= tmax_all)

profile.in(th=seq(0.11,0.2,len=18),Mat = c(rep(1,10),rep(2,7),3),projektpfad = projektpfad2)
fit.in(params = pars,q_fit = F,treat = "all")

hydrus.exe(sleep=1,file = "undisturbed2",UNSC=F,Inverse = T)


out<-read_hydrus.out(treat=17,
                     projektpfad=projektpfad2,
                     UNSC=F)[[1]]


library(ggplot2)
tiefenstufen<-c(-2,-6,-10,-14)
ggplot(subset(out,tiefe%in%tiefenstufen))+geom_point(aes(t_min,theta,col="obs"),na.rm = T)+geom_point(aes(t_min,theta_mod,col="mod"),na.rm = T)+facet_wrap(~tiefe,ncol=1)

ggplot(subset(out,tiefe==-17))+geom_point(aes(t_min,q_interpol*5,col="obs"),na.rm = T)+geom_point(aes(t_min,q_mod,col="mod"),na.rm = T)


pars_inv<-fit.out()
