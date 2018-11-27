hydruspfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/hydrus/"
projektpfad1<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/hydrus/undisturbed/"
projektpfad2<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/hydrus/undisturbed2/"
programmpfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/programme/Hydrus-1D 4.xx/"
mcpfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/hydrus/montecarlo/"

load("C:/Users/ThinkPad/Documents/Masterarbeit/daten/all.R")
load("C:/Users/ThinkPad/Documents/Masterarbeit/daten/bodenparameter/params.R")

source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/modellierung/hydrus_input.R")
source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/modellierung/montecarlo_function.R")



tmax_all<-as.numeric(difftime(max(all$date),min(all$date),units = "min"))
######################################################
#run modell

library(ggplot2)



params<-data.frame(alpha=0.65,
           n=1.8,
           ks=0.05,
           alpha_bot=0.6,
           n_bot=1.8,
           ks_bot=0.002,
           thr=0.11,
           ths=0.75,
           thr2=0.13,
           ths2=0.64,
           thr_bot=0.13,
           ths_bot=0.64,
           hseep=-100,
           l=0.5,
           p_opt=0.001,
           act_en=6677,
           h_opt=-50,
           h_crit=-10^6)


sub<-hydrus(params = data.frame(thr=0.11,
                           ths=0.75,
                           alpha=0.65,
                           n=1.8,
                           ks=0.1,
                           thr_bot=0.05,
                           ths_bot=0.6,
                           alpha_bot=0.6,
                           n_bot=1.8,
                           ks_bot=0.002,
                           l=0.5,
                           hseep = -100,
                           p_opt=0.001,
                              act_en=6677,
                              h_opt=-50,
                              h_crit=-10^6,
                           michaelis=0.19,
                           DispA=0.1,
                           DispW=0.00106181),
       treat = 17,UNSC=T,sleep=5)[[1]]

ggplot(subset(sub,tiefe%in%c(-10,-14)))+geom_point(aes(t_min,CO2_raw,col="obs"),na.rm = T)+geom_point(aes(t_min,CO2_mod,col="mod"),na.rm = T)




vals<-read_conc.out(projektpfad = projektpfad1)[[1]]

ggplot(sub)+geom_point(aes(t_min,theta,col="obs"),na.rm = T)+geom_line(aes(t_min,theta_mod,col="mod"),na.rm = T)+facet_wrap(~tiefe,ncol=1)

ggplot(subset(sub,tiefe==-17))+geom_point(aes(t_min,q_interpol,col="obs"),na.rm = T)+geom_line(aes(t_min,q_mod,col="mod"),na.rm = T)

sum(sub$q_mod,na.rm = T)/2/5
sum(sub$q_interpol,na.rm = T)/2


#+facet_wrap(~tiefe,ncol=1)

ggplot(subset(vals,tiefe%in%c(tiefenstufen,-17)))+geom_line(aes(t_min,Ca_mod,col=as.factor(tiefe)))
ggplot(subset(vals,tiefe%in%seq(-14,-17)))+geom_line(aes(t_min,Ca_mod,col=as.factor(tiefe)))+geom_point(aes(t_min,ca_conc,col="obs (-17)"))

ggplot(vals)+geom_path(aes(Ca_mod,tiefe,col=as.factor(t_min)))
ggplot(vals)+geom_tile(aes(t_min,tiefe,fill=Ca_mod))


which(is.na(vals$t_min))
summary(vals$Ca_mod[vals$tiefe==-17])
#####################################################
#modell all


selector.in(params = data.frame(thr=0.08,
                                ths=c(0.75,0.63),
                                alpha=c(0.65,0.55),
                                n=c(1.8,1.8),
                                ks=c(1,1),
                                thr_bot=c(0.05),
                                ths_bot=c(0.6),
                                alpha_bot=c(0.6),
                                n_bot=c(1.8),
                                ks_bot=c(0.001),
                                l=0.5,
                                hseep = -100),
            co2_params =data.frame(p_opt=0.001,
                                   act_en=6677,
                                   h_opt=-50,
                                   h_crit=-10^6),
            tmax = tmax_all)


#selector.in(params = params,n_mats = 2)
atmos.in(17,180,4000)
atmos.in(alle=T,total_t = tmax_all)
profile.in(n_nodes = 18,Mat = c(rep(1,10),rep(2,7),3))
hydrus.exe(sleep = 5)

sub<-read_hydrus.out(treat="all")[[1]]
rmse<-read_hydrus.out(treat=17)[[2]]
vals<-read_conc.out(projektpfad = projektpfad1)

ggplot(sub)+geom_point(aes(t_min,theta,col="obs"),na.rm = T)+geom_line(aes(t_min,theta_mod,col="mod"),na.rm = T)+facet_wrap(~tiefe,ncol=1)

ggplot(subset(sub,tiefe==-17))+geom_point(aes(t_min,q_interpol*5,col="obs"),na.rm = T)+geom_line(aes(t_min,q_mod,col="mod"),na.rm = T)

sum(sub$q_mod,na.rm = T)/5/5
sum(sub$q_interpol,na.rm = T)/5

ggplot(subset(sub,tiefe%in%c(-10,-14)))+geom_point(aes(t_min,CO2_raw,col="obs"),na.rm = T)+geom_point(aes(t_min,CO2_mod,col="mod"),na.rm = T)
#+facet_wrap(~tiefe,ncol=1)

ggplot(vals)+geom_line(aes(time,Ca,col=as.factor(Depth)))
ggplot(vals)+geom_line(aes(Ca,Depth,col=as.factor(time)))
ggplot(vals)+geom_tile(aes(time,Depth,fill=Ca))





