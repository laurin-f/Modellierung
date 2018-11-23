load("C:/Users/ThinkPad/Documents/Masterarbeit/daten/all.R")
source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/modellierung/montecarlo_function.R")

mcpfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/hydrus/montecarlo/"
library(ggplot2)
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

#########################################################
#mc mit wasserparametern
########################################################

mc<-monte_carlo(nr=2,ranges=data.frame(alpha=c(0.2,0.75),
                                           n=c(1.5,4.5),
                                           ks=c(1,3),
                                           alpha2=c(0.2,0.75),
                                           n2=c(2.5,5),
                                           ks2=c(0.01,3),
                                           alpha_bot=c(0.06,0.75),
                                           n_bot=c(1.2,1.9),
                                           ks_bot=c(0.001,0.01)),
                UNSAT = F,
                sleep = 0.7,
                treatm = "all",
                free_drain = T)

save(mc,file = paste0(mcpfad,"mc_wp_fd",Sys.Date(),".R"))

#load(file = paste0(mcpfad,"mc_wp_fd2018-11-22",".R"))


par<-mc[[2]]
rmse<-mc[[1]]
nse<-mc[[3]]

best.100<-sort(rmse)[100]
pargood<-par[rmse<best.100,]
rmsegood<-rmse[rmse<best.100]
par(mfrow=c(3,3),mar=c(3,4,2,1))
for(i in 1:9) plot(pargood[,i],rmsegood,main = colnames(par)[i])
par(mfrow=c(1,1))

best.nse<-sort(nse,decreasing = T)[300]
pargood<-par[nse>best.nse,]
nsegood<-nse[nse>best.nse]
par(mfrow=c(3,3),mar=c(3,4,2,1))
for(i in 1:9) plot(pargood[,i],nsegood,main = colnames(par)[i])
par(mfrow=c(1,1))

pars<-cbind(par[which.min(rmse),],fixed)
min(rmse,na.rm = T)

out<-hydrus(params = pars,UNSC=F,sleep = 2,free_drain=T)[[1]]

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

#out<-hydrus(params = co2_pars,UNSC=T,sleep = 3)[[1]]



ggplot(subset(out,tiefe%in%tiefenstufen))+geom_point(aes(t_min,theta,col="obs"),na.rm = T)+geom_line(aes(t_min,theta_mod,col=as.factor(tiefe)),na.rm = T)+facet_wrap(~tiefe,ncol=1)

ggplot(subset(out,tiefe==-17))+geom_point(aes(t_min,q_interpol*5,col="obs"),na.rm = T)+geom_line(aes(t_min,q_mod,col="mod"),na.rm = T)


#ggplot(subset(out,tiefe%in%c(-10,-14)))+geom_point(aes(t_min,CO2_raw,col="obs"),na.rm = T)+geom_point(aes(t_min,CO2_mod,col="mod"),na.rm = T)


############################################################
#change all water parameters
###############################################################

mc<-monte_carlo(nr=500,ranges=data.frame(thr=c(0.05,0.11),
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
                UNSAT = F,
                sleep = 0.7,
                treatm = "all",
                free_drain = T)

#save(mc,file = paste0(mcpfad,"mc_alle_wp_fd",Sys.Date(),".R"))

#load(file = paste0(mcpfad,"mc_alle_wp2018-11-21",".R"))


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

