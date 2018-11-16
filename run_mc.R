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

####################################
#Monte Carlo

fixed<-data.frame(thr=0.11,
                  ths=0.75,
                  thr2=0.13,
                  ths2=0.64,
                  thr_bot=0.13,
                  ths_bot=0.64,
                  hseep=-100,
                  l=0.5)


atmos.in(alle=T,total_t = tmax_all, projektpfad = projektpfad2)
#atmos.in(17,180,4000, projektpfad = projektpfad2)

profile.in(n_nodes = 18,
           th=seq(0.11,0.2,len=18),
           Mat = c(rep(1,10),rep(2,6),3,3),
           projektpfad = projektpfad2)

mc<-monte_carlo(nr=100,ranges=data.frame(alpha=c(0.05,0.75),
                                         n=c(1.5,4.5),
                                         ks=c(0.01,1),
                                         alpha2=c(0.005,0.075),
                                         n2=c(1.5,4.5),
                                         ks2=c(0.01,1),
                                         alpha_bot=c(0.06,0.5),
                                         n_bot=c(1.2,1.9),
                                         ks_bot=c(0.001,0.002)),
                pfad = projektpfad2,
                UNSAT = F,
                File = "undisturbed2",
                sleep = 0.7,
                treatm = "all",
                total_t = tmax_all)

#save(mc,file = paste0(mcpfad,"mc",Sys.Date(),".R"))
#load(file = paste0(mcpfad,"mc","2018-11-15",".R"))

par<-mc[[2]]
rmse<-mc[[1]]

pargood<-par[rmse<1,]
rmsegood<-rmse[rmse<1]
par(mfrow=c(3,3),mar=c(3,4,2,1))
for(i in 1:9) plot(pargood[,i],rmsegood,main = colnames(par)[i])
par(mfrow=c(1,1))

pars<-cbind(par[which.min(rmse),],fixed)
#pars<-par[which.min(rmse),]

selector.in(params = pars,
            projektpfad = projektpfad2,
            tmax= tmax_all)

hydrus.exe(sleep=1,file = "undisturbed2",UNSC=F)

out<-read_hydrus.out(treat="all",
                     projektpfad=projektpfad2,
                     UNSC=F)[[1]]







############################################################
#output with mc water parameters
########################################################

atmos.in(alle=T,total_t = tmax_all, projektpfad = projektpfad1)

selector.in(params = pars,
            co2_params =data.frame(p_opt=0.001,
                                   act_en=6677,
                                   h_opt=-50,
                                   h_crit=-10^6),
            projektpfad = projektpfad1,tmax=tmax_all)

profile.in(n_nodes = 18,
           th=seq(0.11,0.2,len=18),
           Mat = c(rep(1,10),rep(2,6),3,3),
           projektpfad = projektpfad1)
hydrus.exe(sleep=1,file = "undisturbed",UNSC=T)

out<-read_hydrus.out(treat="all",
                     projektpfad=projektpfad1,
                     UNSC=T)[[1]]

read_hydrus.out(treat="all",
                projektpfad=projektpfad1,
                UNSC=T)[[2]]

tiefenstufen<-c(-2,-6,-10,-14)
ggplot(subset(out,tiefe%in%tiefenstufen))+geom_point(aes(t_min,theta,col="obs"),na.rm = T)+geom_line(aes(t_min,theta_mod,col="mod"),na.rm = T)+facet_wrap(~tiefe,ncol=1)

ggplot(subset(out,tiefe==-17))+geom_point(aes(t_min,q_interpol*5,col="obs"),na.rm = T)+geom_line(aes(t_min,q_mod,col="mod"),na.rm = T)


ggplot(subset(out,tiefe%in%c(-10,-14)))+geom_point(aes(t_min,CO2_raw,col="obs"),na.rm = T)+geom_point(aes(t_min,CO2_mod,col="mod"),na.rm = T)


#####################################
#co2 with fixed water parameters
######################################

mc<-monte_carlo(nr=300,sleep=7,total_t = tmax_all,ranges = NULL,fixed=pars,
                co2_ranges=data.frame(p_opt=c(0.0001,0.01),
                                      act_en=c(4500,8000),
                                      h_opt=c(-200,-10),
                                      h_crit=c(-10^7,-10^3)),treatm = "all")
par<-mc[[2]]
rmse<-mc[[1]]

pargood<-par[rmse<4000,]
rmsegood<-rmse[rmse<4000]
par(mfrow=c(2,2),mar=c(3,4,2,1))
for(i in 1:4) plot(pargood[,i],rmsegood,main = colnames(par)[i])
par(mfrow=c(1,1))

co2_pars<-par[which.min(rmse),]


atmos.in(alle=T,total_t = tmax_all, projektpfad = projektpfad1)

selector.in(params = pars,
            co2_params =co2_pars,
            projektpfad = projektpfad1,tmax=tmax_all)

profile.in(n_nodes = 18,
           th=seq(0.11,0.2,len=18),
           Mat = c(rep(1,10),rep(2,6),3,3),
           projektpfad = projektpfad1)

hydrus.exe(sleep=1,file = "undisturbed",UNSC=T)

out<-read_hydrus.out(treat="all",
                     projektpfad=projektpfad1,
                     UNSC=T)[[1]]
ggplot(subset(out,tiefe%in%tiefenstufen))+geom_point(aes(t_min,theta,col="obs"),na.rm = T)+geom_line(aes(t_min,theta_mod,col="mod"),na.rm = T)+facet_wrap(~tiefe,ncol=1)

ggplot(subset(out,tiefe==-17))+geom_point(aes(t_min,q_interpol*5,col="obs"),na.rm = T)+geom_line(aes(t_min,q_mod,col="mod"),na.rm = T)


ggplot(subset(out,tiefe%in%c(-10,-14)))+geom_point(aes(t_min,CO2_raw,col="obs"),na.rm = T)+geom_point(aes(t_min,CO2_mod,col="mod"),na.rm = T)


###############################################################
#with changing water paramters
###############################################################

mc<-monte_carlo(nr=100,sleep=7,total_t = tmax_all,ranges=data.frame(alpha=c(0.05,0.75),
                                                                    n=c(1.5,4.5),
                                                                    ks=c(0.01,1),
                                                                    alpha2=c(0.005,0.075),
                                                                    n2=c(1.5,4.5),
                                                                    ks2=c(0.01,1),
                                                                    alpha_bot=c(0.06,0.5),
                                                                    n_bot=c(1.2,1.9),
                                                                    ks_bot=c(0.001,0.002)),fixed=fixed,
                co2_ranges=data.frame(p_opt=c(0.0001,0.01),
                                      act_en=c(4500,8000),
                                      h_opt=c(-200,-10),
                                      h_crit=c(-10^7,-10^3)),treatm = "all")
par<-mc[[3]]
par_co2<-mc[[2]]
rmse<-mc[[1]]

pargood<-par[rmse<4000,]
rmsegood<-rmse[rmse<4000]
par(mfrow=c(3,3),mar=c(3,4,2,1))
for(i in 1:9) plot(pargood[,i],rmsegood,main = colnames(par)[i])
par(mfrow=c(1,1))



par_co2good<-par_co2[rmse<4000,]
rmsegood<-rmse[rmse<4000]
par(mfrow=c(2,2),mar=c(3,4,2,1))
for(i in 1:4) plot(par_co2good[,i],rmsegood,main = colnames(par_co2)[i])
par(mfrow=c(1,1))

co2_pars<-par_co2[which.min(rmse),]
pars<-cbind(par[which.min(rmse),],fixed)


atmos.in(alle=T,total_t = tmax_all, projektpfad = projektpfad1)

selector.in(params = pars,
            co2_params =co2_pars,
            projektpfad = projektpfad1,tmax=tmax_all)

profile.in(n_nodes = 18,
           th=seq(0.11,0.2,len=18),
           Mat = c(rep(1,10),rep(2,6),3,3),
           projektpfad = projektpfad1)

hydrus.exe(sleep=1,file = "undisturbed",UNSC=T)

out<-read_hydrus.out(treat="all",
                     projektpfad=projektpfad1,
                     UNSC=T)[[1]]
ggplot(subset(out,tiefe%in%tiefenstufen))+geom_point(aes(t_min,theta,col="obs"),na.rm = T)+geom_line(aes(t_min,theta_mod,col="mod"),na.rm = T)+facet_wrap(~tiefe,ncol=1)

ggplot(subset(out,tiefe==-17))+geom_point(aes(t_min,q_interpol*5,col="obs"),na.rm = T)+geom_line(aes(t_min,q_mod,col="mod"),na.rm = T)


ggplot(subset(out,tiefe%in%c(-10,-14)))+geom_point(aes(t_min,CO2_raw,col="obs"),na.rm = T)+geom_point(aes(t_min,CO2_mod,col="mod"),na.rm = T)

############################################################
#everything changes
########################################################

mc<-monte_carlo(nr=100,sleep=7,total_t = tmax_all,ranges=data.frame(alpha=c(0.05,0.75),
                                                                    n=c(1.5,4.5),
                                                                    ks=c(0.01,1),
                                                                    alpha2=c(0.005,0.075),
                                                                    n2=c(1.5,4.5),
                                                                    ks2=c(0.01,1),
                                                                    alpha_bot=c(0.06,0.5),
                                                                    n_bot=c(1.2,1.9),
                                                                    ks_bot=c(0.001,0.002),
                                                                    thr=c(0.05,0.11),
                                                                    thr2=c(0.05,0.13),
                                                                    thr_bot=c(0.05,0.13),
                                                                    ths=c(0.6,0.8),
                                                                    ths2=c(0.5,0.7),
                                                                    ths_bot=c(0.5,0.7),
                                                                    hseep=c(-100,-20)
                                                                    ),
                fixed=data.frame(l=0.5),
                co2_ranges=data.frame(p_opt=c(0.0001,0.01),
                                      act_en=c(4500,8000),
                                      h_opt=c(-200,-10),
                                      h_crit=c(-10^7,-10^3)),treatm = "all")
par<-mc[[3]]
par_co2<-mc[[2]]
rmse<-mc[[1]]

pargood<-par[rmse<4000,]
rmsegood<-rmse[rmse<4000]
par(mfrow=c(3,3),mar=c(3,4,2,1))
for(i in 1:9) plot(pargood[,i],rmsegood,main = colnames(par)[i])
par(mfrow=c(1,1))



par_co2good<-par_co2[rmse<4000,]
rmsegood<-rmse[rmse<4000]
par(mfrow=c(2,2),mar=c(3,4,2,1))
for(i in 1:4) plot(par_co2good[,i],rmsegood,main = colnames(par_co2)[i])
par(mfrow=c(1,1))

co2_pars<-par_co2[which.min(rmse),]
pars<-cbind(par[which.min(rmse),],fixed)


atmos.in(alle=T,total_t = tmax_all, projektpfad = projektpfad1)

selector.in(params = pars,
            co2_params =co2_pars,
            projektpfad = projektpfad1,tmax=tmax_all)

profile.in(n_nodes = 18,
           th=seq(0.11,0.2,len=18),
           Mat = c(rep(1,10),rep(2,6),3,3),
           projektpfad = projektpfad1)

hydrus.exe(sleep=1,file = "undisturbed",UNSC=T)

out<-read_hydrus.out(treat="all",
                     projektpfad=projektpfad1,
                     UNSC=T)[[1]]
ggplot(subset(out,tiefe%in%tiefenstufen))+geom_point(aes(t_min,theta,col="obs"),na.rm = T)+geom_line(aes(t_min,theta_mod,col="mod"),na.rm = T)+facet_wrap(~tiefe,ncol=1)

ggplot(subset(out,tiefe==-17))+geom_point(aes(t_min,q_interpol*5,col="obs"),na.rm = T)+geom_line(aes(t_min,q_mod,col="mod"),na.rm = T)


ggplot(subset(out,tiefe%in%c(-10,-14)))+geom_point(aes(t_min,CO2_raw,col="obs"),na.rm = T)+geom_point(aes(t_min,CO2_mod,col="mod"),na.rm = T)
