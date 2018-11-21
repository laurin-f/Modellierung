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

tiefenstufen<-c(-2,-6,-10,-14)
####################################
#Monte Carlo
###################################
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
           Mat = c(rep(1,10),rep(2,7),3),
           projektpfad = projektpfad2)

#########################################################
#mc mit wasserparametern
########################################################

mc<-monte_carlo(nr=10000,ranges=data.frame(alpha=c(0.2,0.75),
                                         n=c(1.5,4.5),
                                         ks=c(1,3),
                                         alpha2=c(0.2,0.75),
                                         n2=c(2.5,5),
                                         ks2=c(0.01,3),
                                         alpha_bot=c(0.06,0.75),
                                         n_bot=c(1.2,1.9),
                                         ks_bot=c(0.001,0.01)),
                pfad = projektpfad2,
                UNSAT = F,
                sleep = 0.7,
                treatm = "all")

#save(mc,file = paste0(mcpfad,"mc_wp",Sys.Date(),".R"))
#load(file = paste0(mcpfad,"mc","2018-11-15",".R"))
#load(file = paste0(mcpfad,"mc","2018-11-20",".R"))
#load(file = paste0(mcpfad,"mc_wp2018-11-21",".R"))


par<-mc[[2]]
rmse<-mc[[1]]
nse<-mc[[3]]

best.100<-sort(rmse)[300]
pargood<-par[rmse<best.100,]
rmsegood<-rmse[rmse<best.100]
par(mfrow=c(3,3),mar=c(3,4,2,1))
for(i in 1:9) plot(pargood[,i],rmsegood,main = colnames(par)[i])
par(mfrow=c(1,1))

pargood<-par[nse<0,]
nsegood<-nse[nse<0]
par(mfrow=c(3,3),mar=c(3,4,2,1))
for(i in 1:9) plot(pargood[,i],nsegood,main = colnames(par)[i])
par(mfrow=c(1,1))

pars<-cbind(par[which.min(rmse),],fixed)
min(rmse,na.rm = T)

out<-hydrus(params = pars,UNSC=F,sleep = 2)[[1]]



############################################################
#output with mc water parameters
########################################################

co2_params <- data.frame(p_opt=0.001,
                         act_en=6677,
                         h_opt=-50,
                         h_crit=-10^6,
                         michaelis=0.19,
                         DispA=1,
                         DispW=0.00106181)



co2_pars<-cbind(pars,co2_params)

out<-hydrus(params = co2_pars,UNSC=T,sleep = 7)[[1]]



ggplot(subset(out,tiefe%in%tiefenstufen))+geom_point(aes(t_min,theta,col="obs"),na.rm = T)+geom_line(aes(t_min,theta_mod,col=as.factor(tiefe)),na.rm = T)+facet_wrap(~tiefe,ncol=1)

ggplot(subset(out,tiefe==-17))+geom_point(aes(t_min,q_interpol*5,col="obs"),na.rm = T)+geom_line(aes(t_min,q_mod,col="mod"),na.rm = T)


ggplot(subset(out,tiefe%in%c(-10,-14)))+geom_point(aes(t_min,CO2_raw,col="obs"),na.rm = T)+geom_point(aes(t_min,CO2_mod,col="mod"),na.rm = T)


############################################################
#change all water parameters
###############################################################

mc<-monte_carlo(nr=5,ranges=data.frame(thr=c(0.05,0.11),
                                           ths=c(0.5,0.8),
                                           thr2=c(0.07,0.13),
                                           ths2=c(0.4,0.7),
                                           thr_bot=c(0.05,0.2),
                                           ths_bot=c(0.5,0.7),
                                           hseep=c(-100,-20),
                                           alpha=c(0.05,0.75),
                                           n=c(2,5),
                                           ks=c(0.1,3),
                                           alpha2=c(0.05,0.75),
                                           n2=c(1.5,4.5),
                                           ks2=c(0.01,1),
                                           alpha_bot=c(0.06,0.75),
                                           n_bot=c(1.2,1.9),
                                           ks_bot=c(0.001,0.1)),
                fixed = data.frame(l=0.5),
                pfad = projektpfad2,
                UNSAT = F,
                sleep = 0.7,
                treatm = "all")

#save(mc,file = paste0(mcpfad,"mc_alle_wp",Sys.Date(),".R"))

load(file = paste0(mcpfad,"mc_alle_wp2018-11-21",".R"))


par<-mc[[2]]
rmse<-mc[[1]]
nse<-mc[[3]]

best.100<-sort(rmse)[300]
pargood<-par[rmse<best.100,]
rmsegood<-rmse[rmse<best.100]
par(mfrow=c(4,4),mar=c(3,4,2,1))
for(i in 1:16) plot(pargood[,i],rmsegood,main = colnames(par)[i])
par(mfrow=c(1,1))

best.nse<-sort(nse,decreasing = T)[300]
pargood<-par[nse>best.nse,]
nsegood<-nse[nse>best.nse]
par(mfrow=c(4,4),mar=c(3,4,2,1))
for(i in 1:16) plot(pargood[,i],nsegood,main = colnames(par)[i])
par(mfrow=c(1,1))

pars<-par[which.min(rmse),]
pars$l<-0.5

min(rmse,na.rm = T)

out<-hydrus(params = pars,UNSC=F,sleep = 2)[[1]]


############################################################
#output with mc water parameters
########################################################

co2_params <- data.frame(p_opt=0.001,
                         act_en=6677,
                         h_opt=-50,
                         h_crit=-10^6,
                         michaelis=0.19,
                         DispA=9,
                         DispW=0.00106181)

co2_pars<-cbind(pars,co2_params)

out<-hydrus(params = co2_pars,UNSC=T,sleep = 7)[[1]]


ggplot(subset(out,tiefe%in%tiefenstufen))+geom_point(aes(t_min,theta,col="obs"),na.rm = T)+geom_line(aes(t_min,theta_mod,col="mod"),na.rm = T)+facet_wrap(~tiefe,ncol=1)

ggplot(subset(out,tiefe==-17))+geom_point(aes(t_min,q_interpol*5,col="obs"),na.rm = T)+geom_line(aes(t_min,q_mod,col="mod"),na.rm = T)


ggplot(subset(out,tiefe%in%c(-10,-14)))+geom_point(aes(t_min,CO2_raw,col="obs"),na.rm = T)+geom_point(aes(t_min,CO2_mod,col="mod"),na.rm = T)


#####################################
#co2 with fixed water parameters
######################################
load(file = paste0(mcpfad,"mc_wp2018-11-21",".R"))

par<-mc[[2]]
rmse<-mc[[1]]

fixpars<-cbind(par[which.min(rmse),],fixed)

hydrus(params= co2_pars,sleep = 2,treat ="all",read=F)

mc<-monte_carlo(nr=1000,sleep=5,
                ranges = data.frame(p_opt=c(0.0001,0.005),
                                      act_en=c(6000,7000),
                                      h_opt=c(-100,-20),
                                      h_crit=c(-10^7,-10^5),
                                      michaelis=c(0.1,0.3),
                                      DispA=c(0.1,10),
                                      DispW=c(0.001,0.002))
                ,fixed=fixpars,
                treatm = "all",fit.tiefe = c(-10,-14))

#save(mc,file = paste0(mcpfad,"mc_co2-",Sys.Date(),".R"))

load(file = paste0(mcpfad,"mc_co2-2018-11-21",".R"))

par<-mc[[2]]
rmse<-mc[[1]]
nse<-mc[[3]]


best.100<-sort(rmse)[300]
pargood<-par[rmse<best.100,]
rmsegood<-rmse[rmse<best.100]
par(mfrow=c(3,4),mar=c(3,4,2,1))
for(i in 1:ncol(pargood)) plot(pargood[,i],rmsegood,main = colnames(par)[i])
par(mfrow=c(1,1))

best.nse<-sort(nse,decreasing = T)[300]
pargood<-par[nse>best.nse,]
nsegood<-nse[nse>best.nse]
par(mfrow=c(3,4),mar=c(3,4,2,1))
for(i in 1:ncol(pargood)) plot(pargood[,i],nsegood,main = colnames(par)[i])
par(mfrow=c(1,1))

pars<-cbind(par[which.min(rmse),],fixpars)

out<-hydrus(params=pars,sleep = 5,treat ="all")[[1]]


ggplot(subset(out,tiefe%in%c(-10,-14)))+geom_point(aes(t_min,CO2_raw,col="obs"),na.rm = T)+geom_point(aes(t_min,CO2_mod,col="mod"),na.rm = T)


###############################################################
#with changing water paramters
###############################################################

fixed_co2<-data.frame(act_en=6677,
                      h_opt=-50,
                      h_crit=-10^6,
                      michaelis=0.19,
                      DispW=0.00106181)



mc<-monte_carlo(nr=1000,sleep=7,ranges=data.frame(alpha=c(0.05,0.75),
                                                                    n=c(1.5,4.5),
                                                                    ks=c(0.1,3),
                                                                    alpha2=c(0.05,0.75),
                                                                    n2=c(1.5,4.5),
                                                                    ks2=c(0.1,3),
                                                                    alpha_bot=c(0.06,0.5),
                                                                    n_bot=c(1.2,1.9),
                                                                    ks_bot=c(0.001,0.002),
                                                                    p_opt=c(0.0001,0.01),
                                                                    DispA=c(0.1,9)),
                fixed=cbind(fixed,fixed_co2),
                treatm = "all",
                fit.tiefe = c(-10,-14))

save(mc,file = paste0(mcpfad,"mc_wp_co2-",Sys.Date(),".R"))

load(file = paste0(mcpfad,"mc_wp_co2-2018-11-21",".R"))

par<-mc[[2]]
rmse<-mc[[1]]

par<-mc[[2]]
rmse<-mc[[1]]
nse<-mc[[3]]

best.100<-sort(rmse)[300]
pargood<-par[rmse<best.100,]
rmsegood<-rmse[rmse<best.100]
par(mfrow=c(3,4),mar=c(3,4,2,1))
for(i in 1:ncol(pargood)) plot(pargood[,i],rmsegood,main = colnames(par)[i])
par(mfrow=c(1,1))

best.nse<-sort(nse,decreasing = T)[300]
pargood<-par[nse>best.nse,]
nsegood<-nse[nse>best.nse]
par(mfrow=c(3,4),mar=c(3,4,2,1))
for(i in 1:ncol(pargood)) plot(pargood[,i],nsegood,main = colnames(par)[i])
par(mfrow=c(1,1))

pars<-cbind(par[which.min(rmse),],fixed,fixed_co2)


out<-hydrus(params=pars)[[1]]

ggplot(subset(out,tiefe%in%tiefenstufen))+geom_point(aes(t_min,theta,col="obs"),na.rm = T)+geom_line(aes(t_min,theta_mod,col="mod"),na.rm = T)+facet_wrap(~tiefe,ncol=1)

ggplot(subset(out,tiefe==-17))+geom_point(aes(t_min,q_interpol*5,col="obs"),na.rm = T)+geom_line(aes(t_min,q_mod,col="mod"),na.rm = T)


ggplot(subset(out,tiefe%in%c(-10,-14)))+geom_point(aes(t_min,CO2_raw,col="obs"),na.rm = T)+geom_point(aes(t_min,CO2_mod,col="mod"),na.rm = T)

############################################################
#everything changes
########################################################


mc<-monte_carlo(nr=10000,sleep=8,ranges=data.frame(alpha=c(0.005,0.75),
                                                                    n=c(1.2,4.5),
                                                                    ks=c(0.01,1),
                                                                    alpha2=c(0.005,0.075),
                                                                    n2=c(1.2,4.5),
                                                                    ks2=c(0.01,1),
                                                                    alpha_bot=c(0.006,0.5),
                                                                    n_bot=c(1.2,3),
                                                                    ks_bot=c(0.001,0.2),
                                                                    thr=c(0.05,0.11),
                                                                    thr2=c(0.05,0.13),
                                                                    thr_bot=c(0.05,0.13),
                                                                    ths=c(0.6,0.8),
                                                                    ths2=c(0.5,0.7),
                                                                    ths_bot=c(0.5,0.7),
                                                                    hseep=c(-100,-20),
                                                                    p_opt=c(0.0001,0.01),
                                                                    act_en=c(4500,8000),
                                                                    h_opt=c(-200,-10),
                                                                    h_crit=c(-10^7,-10^3),
                                                                    michaelis=c(0.1,0.3)
                                                                    ),
                fixed=data.frame(l=0.5),treatm = "all",fit.tiefe = c(-10,-14))

if(length(mc)==0){
  load("C:/Users/ThinkPad/Documents/Masterarbeit/daten/hydrus/montecarlo/mc_temp.R")
}else{
par<-mc[[2]]
rmse<-mc[[1]]
}

pargood<-par[rmse<4000,]
rmsegood<-rmse[rmse<4000]
par(mfrow=c(5,5),mar=c(3,4,2,1))
for(i in 1:ncol(par)) plot(pargood[,i],rmsegood,main = colnames(par)[i])
par(mfrow=c(1,1))


pars<-cbind(par[which.min(rmse),],fixed)


out<-hydrus(params = pars,sleep = 6)[[1]]

ggplot(subset(out,tiefe%in%c(-10,-14)))+geom_point(aes(t_min,CO2_raw,col="obs"),na.rm = T)+geom_point(aes(t_min,CO2_mod,col="mod"),na.rm = T)

ggplot(subset(out,tiefe%in%tiefenstufen))+geom_point(aes(t_min,theta,col="obs"),na.rm = T)+geom_line(aes(t_min,theta_mod,col="mod"),na.rm = T)+facet_wrap(~tiefe,ncol=1)

ggplot(subset(out,tiefe==-17))+geom_point(aes(t_min,q_interpol*5,col="obs"),na.rm = T)+geom_line(aes(t_min,q_mod,col="mod"),na.rm = T)


