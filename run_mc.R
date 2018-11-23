hydruspfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/hydrus/"
projektpfad1<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/hydrus/undisturbed/"
projektpfad2<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/hydrus/undisturbed2/"
programmpfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/programme/Hydrus-1D 4.xx/"
mcpfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/hydrus/montecarlo/"
plotpfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/abbildungen/plots/mc/"
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
                #pfad = projektpfad2,
                UNSAT = F,
                sleep = 0.7,
                treatm = "all")

#save(mc,file = paste0(mcpfad,"mc_wp",Sys.Date(),".R"))
loadfile<-"mc_wp2018-11-21"
load(file = paste0(mcpfad,loadfile,".R"))


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
mc_out(fixed=cbind(fixed,co2_params),loadfile = "mc_wp2018-11-21")


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
                         Disper=5)

co2_pars<-cbind(pars,co2_params)

mc_out(co2_pars,loadfile = "mc_alle_wp2018-11-21",treat = "all")

out<-hydrus(params = co2_pars,UNSC=T,sleep = 3,treat = 17)[[1]]


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
#with changing water paramters
###############################################################

fixed_co2<-data.frame(act_en=6677,
                      h_opt=-50,
                      h_crit=-10^6,
                      michaelis=0.19,
                      DispW=0.00106181)



mc<-monte_carlo(nr=100,sleep=7,ranges=data.frame(alpha=c(0.05,0.75),
                                                                    n=c(1.5,4.5),
                                                                    ks=c(0.1,3),
                                                                    alpha2=c(0.05,0.75),
                                                                    n2=c(1.5,4.5),
                                                                    ks2=c(0.1,3),
                                                                    alpha_bot=c(0.06,0.5),
                                                                    n_bot=c(1.2,1.9),
                                                                    ks_bot=c(0.001,0.002),
                                                                    p_opt=c(0.0001,0.01),
                                                                    DispA=c(0.1,9),
                                                  Disper=c(1,10)),
                fixed=cbind(fixed,fixed_co2),
                treatm = "all",
                fit.tiefe = c(-10,-14))

#save(mc,file = paste0(mcpfad,"mc_wp_co2-",Sys.Date(),".R"))

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
pars$Disper<-5

mc_out(cbind(fixed,fixed_co2),loadfile = "mc_wp_co2-2018-11-21",treat = 17)

out<-hydrus(params=pars)[[1]]

ggplot(subset(out,tiefe%in%tiefenstufen))+geom_point(aes(t_min,theta,col="obs"),na.rm = T)+geom_line(aes(t_min,theta_mod,col="mod"),na.rm = T)+facet_wrap(~tiefe,ncol=1)

ggplot(subset(out,tiefe==-17))+geom_point(aes(t_min,q_interpol*5,col="obs"),na.rm = T)+geom_line(aes(t_min,q_mod,col="mod"),na.rm = T)


ggplot(subset(out,tiefe%in%c(-10,-14)))+geom_point(aes(t_min,CO2_raw,col="obs"),na.rm = T)+geom_point(aes(t_min,CO2_mod,col="mod"),na.rm = T)

############################################################
#everything changes
########################################################
2000*17/3600
mc<-monte_carlo(nr=3000,sleep=15,ranges=data.frame(alpha=c(0.005,0.75),
                                                                    n=c(1.2,4.5),
                                                                    ks=c(0.01,3),
                                                                    alpha2=c(0.005,0.75),
                                                                    n2=c(1.2,4.5),
                                                                    ks2=c(0.01,3),
                                                                    alpha_bot=c(0.006,0.5),
                                                                    n_bot=c(1.2,3),
                                                                    ks_bot=c(0.001,1),
                                                                    thr=c(0.05,0.11),
                                                                    thr2=c(0.05,0.13),
                                                                    thr_bot=c(0.05,0.13),
                                                                    ths=c(0.6,0.8),
                                                                    ths2=c(0.5,0.7),
                                                                    ths_bot=c(0.5,0.7),
                                                                    hseep=c(-100,-20),
                                                                    p_opt=c(0.00001,0.01),
                                                                    act_en=c(4500,8000),
                                                                    h_opt=c(-200,-10),
                                                                    h_crit=c(-10^7,-10^3),
                                                                    michaelis=c(0.1,0.3),
                                               DispA=c(0.01,9),
                                               DispW=c(0.001,0.01),
                                               Disper=c(1,10),
                                               bulk=c(0.6,1),
                                               bulk2=c(0.6,1.3),
                                               difuz=c(0,0.01),
                                               difuz2=c(0,0.01),
                                               disperl=c(1,2),
                                               disperl2=c(1,2),
                                               cec=c(0,0),
                                               cec2=c(0,0),
                                               calcit=c(0.02,1),
                                               calcit2=c(0.02,1)
                                               ),
                fixed=data.frame(l=0.5),treatm = "all",fit.tiefe = c(-10,-14))

if(length(mc)==0){
  load("C:/Users/ThinkPad/Documents/Masterarbeit/daten/hydrus/montecarlo/mc_temp.R")
}else{
par<-mc[[2]]
rmse<-mc[[1]]
}
  #save(mc,file = paste0(mcpfad,"mc_alle_wp_co2-",Sys.Date(),".R"))

mc_out(data.frame(l=0.5),loadfile = "mc_alle_wp_co2-2018-11-22",treat = "all")
  
  loadfile<-"mc_alle_wp_co2-2018-11-22"
  load(file = paste0(mcpfad,loadfile,".R"))
  
  par<-mc[[2]]
  rmse<-mc[[1]]
  
best.100<-sort(rmse)[200]
pargood<-par[rmse<best.100,]
rmsegood<-rmse[rmse<best.100]
par(mfrow=c(5,5),mar=c(3,4,2,1))
for(i in 1:ncol(pargood)) plot(pargood[,i],rmsegood,main = colnames(par)[i])
par(mfrow=c(1,1))

best.nse<-sort(nse,decreasing = T)[300]
pargood<-par[nse>best.nse,]
nsegood<-nse[nse>best.nse]
par(mfrow=c(3,4),mar=c(3,4,2,1))
for(i in 1:ncol(pargood)) plot(pargood[,i],nsegood,main = colnames(par)[i])
par(mfrow=c(1,1))


pars<-cbind(par[which.min(rmse),])
pars$l<-0.5



###################################################
#
out<-hydrus(params = pars,sleep = 3)[[1]]

ggplot(subset(out,tiefe%in%c(-10,-14)))+geom_point(aes(t_min,CO2_raw,col="obs"),na.rm = T)+geom_point(aes(t_min,CO2_mod,col="mod"),na.rm = T)

ggplot(subset(out,tiefe%in%tiefenstufen))+geom_point(aes(t_min,theta,col="obs"),na.rm = T)+geom_line(aes(t_min,theta_mod,col="mod"),na.rm = T)+facet_wrap(~tiefe,ncol=1)

ggplot(subset(out,tiefe==-17))+geom_point(aes(t_min,q_interpol*5,col="obs"),na.rm = T)+geom_line(aes(t_min,q_mod,col="mod"),na.rm = T)


