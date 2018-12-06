hydruspfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/hydrus/"
projektpfad1<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/hydrus/undisturbed/"
projektpfad2<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/hydrus/undisturbed2/"
programmpfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/programme/Hydrus-1D 4.xx/"
mcpfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/hydrus/montecarlo/"
plotpfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/abbildungen/plots/mc/"
load("C:/Users/ThinkPad/Documents/Masterarbeit/daten/all.R")
#load("C:/Users/ThinkPad/Documents/Masterarbeit/daten/bodenparameter/params.R")

source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/modellierung/hydrus_input.R")
source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/modellierung/montecarlo_function.R")

library(ggplot2)


tiefenstufen<-c(-2,-6,-10,-14)
####################################
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
                  difuz2=0,
                  disperl=1.7,
                  disperl2=1.7,
                  cec=0,
                  cec2=0,
                  calcit=0.2,
                  calcit2=0.2,
                  CaAds=500,
                  CaPrec=500)


#########################################################
#mc mit wasserparametern
########################################################

mc<-monte_carlo(nr=1000,ranges=data.frame(alpha=c(0.2,0.75),
                                         n=c(1.5,4.5),
                                         ks=c(1,3),
                                         alpha2=c(0.2,0.75),
                                         n2=c(2.5,5),
                                         ks2=c(0.01,3),
                                         alpha3=c(0.06,0.75),
                                         n3=c(1.2,1.9),
                                         ks3=c(0.001,0.01)),
                #pfad = projektpfad2,
                UNSAT = F,
                sleep = 0.7,
                treatm = "all")

#save(mc,file = paste0(mcpfad,"mc_wp",Sys.Date(),".R"))
loadfile<-"mc_wp2018-11-21"
#load(file = paste0(mcpfad,loadfile,".R"))


par<-mc[[2]]
rmse<-mc[[1]]
nse<-mc[[3]]

best.100<-sort(rmse)[300]
pargood<-par[rmse<best.100,]
rmsegood<-rmse[rmse<best.100]
par(mfrow=c(3,3),mar=c(3,4,2,1))
for(i in 1:9) plot(pargood[,i],rmsegood,main = colnames(par)[i])
par(mfrow=c(1,1))

best.nse<-sort(nse,decreasing = T)[300]
pargood<-par[nse>best.nse&!is.na(nse),]
nsegood<-nse[nse>best.nse&!is.na(nse)]
par(mfrow=c(3,3),mar=c(3,4,2,1))
for(i in 1:9) plot(pargood[,i],nsegood,main = colnames(par)[i])
par(mfrow=c(1,1))

pars<-cbind(par[which.min(rmse),],fixed)
min(rmse,na.rm = T)

out<-hydrus(params = pars,UNSC=F,sleep = 2)[[1]]
mc_out(fixed=cbind(fixed,co2_params),loadfile = "mc_wp2018-11-21",free_drain = F,sleep = 5)


############################################################
#output with mc water parameters
########################################################

co2_params <- data.frame(p_opt=0.0001,
                         act_en=6677,
                         h_opt=-50,
                         h_crit=-10^6,
                         michaelis=0.19,
                         DispA=4,
                         DispW=0.00106181,
                         Disper=5)

pars$ks<-0.01
pars$ks2<-0.01

co2_pars<-cbind(pars,co2_params)

out<-hydrus(params = co2_pars,UNSC=T,sleep = 3,treat = 1,
            dtmin=0.0001,dtmax=10,free_drain = T)[[1]]


ggplot(subset(out,tiefe%in%tiefenstufen))+geom_point(aes(t_min,theta,col="obs"),na.rm = T)+geom_line(aes(t_min,theta_mod,col="mod"),na.rm = T)+facet_wrap(~tiefe,ncol=1)+theme_classic()+labs(x="zeit [min]",y=expression(theta*" [Vol %]"))
#+ggsave(paste0(plotpfad,"thetas_",loadfile,".pdf"))

ggplot(subset(out,tiefe==-17))+geom_point(aes(t_min,q_interpol*5,col="obs"),na.rm = T)+geom_line(aes(t_min,q_mod,col="mod"),na.rm = T)+theme_classic()+labs(x="zeit [min]",y=expression("q [ml min"^{-1}*"]"))#+ggsave(paste0(plotpfad,"q_",loadfile,".pdf"))


ggplot(subset(out,tiefe%in%c(-10,-14)))+geom_point(aes(t_min,CO2_raw,col="obs"),na.rm = T)+geom_point(aes(t_min,CO2_mod,col="mod"),na.rm = T)+theme_classic()+labs(x="zeit [min]",y=expression("CO"[2]*" [ppm]]"))#+ggsave(paste0(plotpfad,"CO2_",loadfile,".pdf"))


############################################################
#change all water parameters
###############################################################

mc<-monte_carlo(nr=5,ranges=data.frame(thr=c(0.05,0.11),
                                           ths=c(0.5,0.8),
                                           thr2=c(0.07,0.13),
                                           ths2=c(0.4,0.7),
                                           thr3=c(0.05,0.2),
                                           ths3=c(0.5,0.7),
                                           hseep=c(-100,-20),
                                           alpha=c(0.05,0.75),
                                           n=c(2,5),
                                           ks=c(0.1,3),
                                           alpha2=c(0.05,0.75),
                                           n2=c(1.5,4.5),
                                           ks2=c(0.01,1),
                                           alpha3=c(0.06,0.75),
                                           n3=c(1.2,1.9),
                                           ks3=c(0.001,0.1)),
                fixed = data.frame(l=0.5),
                #pfad = projektpfad2,
                UNSAT = F,
                sleep = 0.7,
                treatm = "all")

#save(mc,file = paste0(mcpfad,"mc_alle_wp",Sys.Date(),".R"))
loadfile<-"mc_alle_wp2018-11-21"
load(file = paste0(mcpfad,loadfile,".R"))


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
pargood<-par[nse>best.nse&!is.na(nse),]
nsegood<-nse[nse>best.nse&!is.na(nse)]
par(mfrow=c(4,4),mar=c(3,4,2,1))
for(i in 1:16) plot(pargood[,i],nsegood,main = colnames(par)[i])
par(mfrow=c(1,1))

pars<-par[which.min(rmse),]
#pars$l<-0.5

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
                         DispW=0.00106181,
                         Disper=5,
                         p_distr=0.105)
fixed_soil<-data.frame(hseep=-100,
                  l=0.5,
                  bulk=0.7561984,
                  bulk2=1.1480438,
                  difuz=0,
                  difuz2=0,
                  disperl=1.7,
                  disperl2=1.7,
                  cec=0,
                  cec2=0,
                  calcit=0.2,
                  calcit2=0.2)

co2_pars<-cbind(pars,co2_params)

mc_out(cbind(co2_params,fixed_soil),loadfile = "mc_alle_wp2018-11-21",treat = "all",free_drain = F)

out<-hydrus(params = cbind(co2_params,fixed_soil,pars),UNSC=T,sleep = 3,treat = 17)[[1]]


ggplot(subset(out,tiefe%in%tiefenstufen))+geom_point(aes(t_min,theta,col="obs"),na.rm = T)+geom_line(aes(t_min,theta_mod,col="mod"),na.rm = T)+facet_wrap(~tiefe,ncol=1)+theme_classic()+labs(x="zeit [min]",y=expression(theta*" [Vol %]"))#+ggsave(paste0(plotpfad,"thetas_",loadfile,".pdf"))

ggplot(subset(out,tiefe==-17))+geom_point(aes(t_min,q_interpol*5,col="obs"),na.rm = T)+geom_line(aes(t_min,q_mod,col="mod"),na.rm = T)+theme_classic()+labs(x="zeit [min]",y=expression("q [ml min"^{-1}*"]"))#+ggsave(paste0(plotpfad,"q_",loadfile,".pdf"))


ggplot(subset(out,tiefe%in%c(-10,-14)))+geom_point(aes(t_min,CO2_raw,col="obs"),na.rm = T)+geom_point(aes(t_min,CO2_mod,col="mod"),na.rm = T)+theme_classic()+labs(x="zeit [min]",y=expression("CO"[2]*" [ppm]]"))#+ggsave(paste0(plotpfad,"CO2_",loadfile,".pdf"))


#####################################
#co2 with fixed water parameters
######################################

loadfile<-"mc_alle_wp2018-11-21"
load(file = paste0(mcpfad,loadfile,".R"))
par<-mc[[2]]
rmse<-mc[[1]]

fixpars<-cbind(par[which.min(rmse),],fixed)

#hydrus(params= co2_pars,sleep = 2,treat ="all",read=F)

mc<-monte_carlo(nr=1000,sleep=5,
                ranges = data.frame(p_opt=c(0.0001,0.005),
                                      act_en=c(6000,7000),
                                      h_opt=c(-100,-20),
                                      h_crit=c(-10^7,-10^5),
                                      michaelis=c(0.1,0.3),
                                      DispA=c(0.1,10),
                                      DispW=c(0.001,0.002),
                                    Disper=c(1,10))
                ,fixed=fixpars,
                treatm = "all",fit.tiefe = c(-10,-14))

#save(mc,file = paste0(mcpfad,"mc_co2-",Sys.Date(),".R"))

loadfile<-"mc_co2-2018-11-21"
load(file = paste0(mcpfad,loadfile,".R"))

par<-mc[[2]]
rmse<-mc[[1]]
nse<-mc[[3]]


best.100<-sort(rmse)[300]
pargood<-par[rmse<best.100,]
rmsegood<-rmse[rmse<best.100]
par(mfrow=c(3,3),mar=c(3,4,2,1))
for(i in 1:ncol(pargood)) plot(pargood[,i],rmsegood,main = colnames(par)[i])
par(mfrow=c(1,1))

best.nse<-sort(nse,decreasing = T)[300]
pargood<-par[nse>best.nse,]
nsegood<-nse[nse>best.nse]
par(mfrow=c(3,3),mar=c(3,4,2,1))
for(i in 1:ncol(pargood)) plot(pargood[,i],nsegood,main = colnames(par)[i])
par(mfrow=c(1,1))

pars<-cbind(par[which.min(rmse),],fixpars)
pars$Disper<-5
out<-hydrus(params=pars,sleep = 5,treat ="all")[[1]]
mc_out(pars,loadfile = "mc_co2-2018-11-21",treat = "all")

ggplot(subset(out,tiefe%in%c(-10,-14)))+geom_point(aes(t_min,CO2_raw,col="obs"),na.rm = T)+geom_point(aes(t_min,CO2_mod,col="mod"),na.rm = T)


###############################################################
#co2 with changing ks paramters
###############################################################

fixed_co2<-data.frame(act_en=6677,
                      h_crit=-10^6,
                      michaelis=0.19,
                      DispW=0.00106181,
                      Disper=5)

fixed_wp<-data.frame(alpha=0.7,
                     n=2,
                     alpha2=0.7,
                     n2=1.5,
                     alpha3=0.4,
                     n3=1.8)

mc<-monte_carlo(nr=200,sleep=5,ranges=data.frame(ks=c(0.001,2),
                                                 ks2=c(0.001,0.05),
                                                 ks3=c(0.0001,0.01),
                                                 p_opt=c(0.00001,0.0002),
                                                 DispA=c(0.1,6),
                                                 h_opt=c(-80,-10),
                                                 p_distr=c(0.01,0.2)),
                                                 
                fixed=cbind(fixed,fixed_co2,fixed_wp),
                treatm = "all",
                free_drain = T,
                fit.tiefe = tiefenstufen)


#save(mc,file = paste0(mcpfad,"mc_wp_co2-",Sys.Date(),".R"))
#loadfile<-"mc_out-nr_700-11-23_14.20"
#load(file = paste0(mcpfad,loadfile,".R"))

par<-mc[[2]]
rmse<-mc[[1]]
nse<-mc[[3]]

##################################
#dottyplots for RMSE
##################################

best.100<-sort(rmse)[100]
pargood<-par[rmse<best.100&!is.na(rmse),]
rmsegood<-rmse[rmse<best.100&!is.na(rmse)]
dotty_rmse<-cbind(rmsegood,pargood)

dotty_melt<-data.table::melt(dotty_rmse,id=1)
dotty_melt$variable<-as.character(dotty_melt$variable)
dotty_melt<-dotty_melt[order(dotty_melt$variable),]

ggplot()+geom_point(data=dotty_melt,aes(value,rmsegood))+geom_point(data=subset(dotty_melt,rmsegood==min(rmsegood)),aes(value,rmsegood),col=2)+facet_wrap(~variable,scales = "free")

##################################
#dottyplots for NSE
##################################

best.nse<-sort(nse,decreasing = T)[100]
pargood<-par[nse>best.nse&!is.na(nse),]
nsegood<-nse[nse>best.nse&!is.na(nse)]

dotty_nse<-cbind(nsegood,pargood)

dotty_melt<-data.table::melt(dotty_nse,id=1)
dotty_melt$variable<-as.character(dotty_melt$variable)
dotty_melt<-dotty_melt[order(dotty_melt$variable),]

ggplot()+geom_point(data=dotty_melt,aes(value,nsegood))+geom_point(data=subset(dotty_melt,nsegood==max(nsegood)),aes(value,nsegood),col=2)+facet_wrap(~variable,scales = "free")


pars<-cbind(par[which.min(rmse),],fixed,fixed_co2,fixed_wp)
parsnse<-cbind(par[which.max(nse),],fixed,fixed_co2,fixed_wp)


mc_out(cbind(fixed,fixed_co2,fixed_wp),loadfile = "mc_out-nr_200-11-23_16.53",treat = "all",ndottys = 100,free_drain = T)

out<-hydrus(params=pars,sleep = 4,dtmin = 0.0001,dtmax = 10)[[1]]
#out<-hydrus(params=parsnse,sleep = 10,dtmin = 0.0001,dtmax = 10)[[1]]
sort(out$tiefe)

ggplot(subset(out,tiefe%in%tiefenstufen))+geom_point(aes(t_min,theta,col="obs"),na.rm = T)+geom_line(aes(t_min,theta_mod,col="mod"),na.rm = T)+facet_wrap(~tiefe,ncol=1)

ggplot(subset(out,tiefe==-17))+geom_point(aes(t_min,q_interpol*5,col="obs"),na.rm = T)+geom_line(aes(t_min,q_mod,col="mod"),na.rm = T)


ggplot(subset(out,tiefe%in%tiefenstufen))+geom_point(aes(t_min,CO2_raw,col="obs"),na.rm = T)+geom_point(aes(t_min,CO2_mod,col="mod"),na.rm = T)+facet_wrap(~tiefe)

###############################################################
#co2 with changing water paramters
###############################################################

fixed_co2<-data.frame(act_en=6677,
                      h_crit=-10^6,
                      michaelis=0.19,
                      DispW=0.00106181,
                      Disper=5)


mc<-mc_parallel(nr=20000,sleep=6,ranges=data.frame(alpha=c(0.1,1),
                                               n=c(1.1,4.5),
                                               ks=c(0.001,1),
                                               alpha2=c(0.1,1),
                                               n2=c(1.1,1.9),
                                               ks2=c(0.0001,0.1),
                                               alpha3=c(0.1,1),
                                               n3=c(1.2,1.9),
                                               ks3=c(0.0001,0.01),
                                               p_opt=c(0.0000001,0.0005),
                                               DispA=c(0.01,5),
                                               h_opt=c(-80,-10),
                                               p_distr=c(0.001,0.2)),
                fixed=cbind(fixed,fixed_co2),fit.calcium = F,
                dtmax = 10)

test<-data.frame(alpha=c(0.1,1),
n=c(1.1,4.5),
ks=c(0.001,1),
alpha2=c(0.1,1),
n2=c(1.1,1.9),
ks2=c(0.0001,0.1),
alpha3=c(0.1,1),
n3=c(1.2,1.9),
ks3=c(0.0001,0.01))
test<-as.data.frame(t(colMeans(test)))
mc<-mc_parallel(nr=400,sleep=5,ranges=data.frame(p_opt=c(0.0000001,0.0005),
                                                 DispA=c(0.01,5),
                                                 h_opt=c(-80,-10),
                                                 p_distr=c(0.001,0.2)),
                fixed=cbind(fixed,fixed_co2,test),fit.calcium = F)

#save(mc,file = paste0(mcpfad,"mc_wp_co2-",Sys.Date(),".R"))
loadfile<-"mc_out-nr_994-12-03_10.10"
#load(file = paste0(mcpfad,loadfile,".R"))

par<-mc[[2]]
rmse<-mc[[1]]
nse<-mc[[3]]

best.100<-sort(rmse)[200]
pargood<-par[rmse<best.100,]
rmsegood<-rmse[rmse<best.100]
par(mfrow=c(4,4),mar=c(3,4,2,1))
for(i in 1:ncol(pargood)){ plot(pargood[,i],rmsegood,main = colnames(par)[i])
points(par[which.min(rmse),i],rmse[which.min(rmse)],col=2)}
par(mfrow=c(1,1))

best.nse<-sort(nse,decreasing = T)[100]
pargood<-par[nse>best.nse,]
nsegood<-nse[nse>best.nse]
par(mfrow=c(4,4),mar=c(3,4,2,1))
for(i in 1:ncol(pargood)){ plot(pargood[,i],nsegood,main = colnames(par)[i])
points(par[which.max(nse),i],nse[which.max(nse)],col=2)}
par(mfrow=c(1,1))

pars<-cbind(par[which.min(rmse),],fixed,fixed_co2)
parsnse<-cbind(par[which.max(nse),],fixed,fixed_co2)

mc_out(fixed=cbind(fixed,fixed_co2),loadfile = "mc_06.12",treat = "all",ndottys = 400,sleep = 5,dtmax = 10)

out<-hydrus(params=pars,sleep = 5,dtmin = 0.0001,dtmax = 10,free_drain = T,taskkill = T)


ggplot(subset(out,tiefe%in%tiefenstufen))+geom_point(aes(t_min,theta,col="obs"),na.rm = T)+geom_line(aes(t_min,theta_mod,col="mod"),na.rm = T)+facet_wrap(~tiefe,ncol=1)

ggplot(subset(out,tiefe==-17))+geom_point(aes(t_min,q_interpol*5,col="obs"),na.rm = T)+geom_line(aes(t_min,q_mod,col="mod"),na.rm = T)


ggplot(subset(out,tiefe%in%tiefenstufen))+geom_point(aes(t_min,CO2_raw,col="obs"),na.rm = T)+geom_line(aes(t_min,CO2_mod,col="mod"),na.rm = T)+facet_wrap(~tiefe)

ggplot(subset(out,tiefe==-17))+geom_point(aes(t_min,ca_conc,col="obs"),na.rm = T)+geom_point(aes(t_min,Ca_mod,col="mod"),na.rm = T)

###############################################################
#co2 with changing water paramters realistic ranges
###############################################################

fixed_co2<-data.frame(act_en=6677,
                      h_crit=-10^6,
                      michaelis=0.19,
                      DispW=0.00106181,
                      Disper=5)

load("C:/Users/ThinkPad/Documents/Masterarbeit/daten/bodenparameter/ranges.R")
ranges<-cbind(realistic_ranges,data.frame(ks=c(0.001,1),
                                          ks2=c(0.001,0.05),
                                          ks3=c(0.0001,0.01),
                                          h_opt=c(-80,-10),
                                          p_distr=c(0.001,0.2)))

mc<-mc_parallel(nr=20000,sleep=5,ranges=ranges,
                 fixed=cbind(fixed,fixed_co2),
                n_nodes = 9,Mat = c(rep(1,3),rep(2,5),3))


#save(mc,file = paste0(mcpfad,"mc_wp_co2-",Sys.Date(),".R"))
loadfile<-"mc_out-nr_20000-11-25_12.02"
#load(file = paste0(mcpfad,loadfile,".R"))

loadfile<-"mc_temp"
#load(file = paste0(mcpfad,loadfile,".R"))

par<-mc[[2]]
rmse<-mc[[1]]
nse<-mc[[3]]


best.100<-sort(rmse)[200]
pargood<-par[rmse<best.100,]
rmsegood<-rmse[rmse<best.100]
par(mfrow=c(4,4),mar=c(3,4,2,1))
for(i in 1:ncol(pargood)){ plot(pargood[,i],rmsegood,main = colnames(par)[i])
  points(par[which.min(rmse),i],rmse[which.min(rmse)],col=2)}
par(mfrow=c(1,1))

best.nse<-sort(nse,decreasing = T)[100]
pargood<-par[nse>best.nse,]
nsegood<-nse[nse>best.nse]
par(mfrow=c(4,4),mar=c(3,4,2,1))
for(i in 1:ncol(pargood)){ plot(pargood[,i],nsegood,main = colnames(par)[i])
  points(par[which.max(nse),i],nse[which.max(nse)],col=2)}
par(mfrow=c(1,1))

pars<-cbind(par[which.min(rmse),],fixed,fixed_co2)
parsnse<-cbind(par[which.max(nse),],fixed,fixed_co2)

mc_out(fixed=cbind(fixed,fixed_co2),loadfile = "mc_out-nr_20000-11-29_07.47",treat = "all",ndottys = 300)
y<-colMeans(ranges)


realistic_pars<-cbind(as.data.frame(t(colMeans(ranges))),fixed,fixed_co2)

out<-hydrus(params=pars,sleep = 30,dtmin = 0.0001,dtmax = 0.02,n_nodes = 9,Mat = c(rep(1,3),rep(2,5),3))

out<-hydrus(params=realistic_pars,sleep = 5,dtmin = 0.0001,dtmax = 10,n_nodes = 9,Mat = c(rep(1,2),rep(2,6),3))




ggplot(subset(out,tiefe%in%tiefenstufen))+geom_point(aes(t_min,theta,col="obs"),na.rm = T)+geom_line(aes(t_min,theta_mod,col="mod"),na.rm = T)+facet_wrap(~tiefe,ncol=1)

ggplot(subset(out,tiefe==-17))+geom_point(aes(t_min,q_interpol*5,col="obs"),na.rm = T)+geom_line(aes(t_min,q_mod,col="mod"),na.rm = T)


ggplot(subset(out,tiefe%in%tiefenstufen))+geom_point(aes(t_min,CO2_raw,col="obs"),na.rm = T)+geom_line(aes(t_min,CO2_mod,col="mod"),na.rm = T)+facet_wrap(~tiefe)

ggplot(subset(out,tiefe==-17&!is.na(Ca_mod)))+
  geom_line(aes(t_min,Ca_mod,col="mod"))+geom_point(aes(t_min,ca_conc,col="obs"))+
  theme_classic()+
  labs(x="Zeit [min]",y=expression("Ca"^{2+""}*" [mg * l"^{-1}*"]"),color="tiefe")

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

mcca<-mc_parallel(nr=10000,ranges = data.frame(calcit=c(0.1,2),
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
