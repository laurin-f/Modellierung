
packages<-c("zoo","lubridate","readxl","data.table","reshape2" ,"ggplot2","devtools","caTools")
for (i in packages){
if(!i%in%installed.packages()){
  install.packages(i)}}
if(!"SAFER"%in%installed.packages()){
  saferpath<-"//FUHYS013/Freiberg/programme/safer_1.1/safer_1.1"
  install.packages(paste0(saferpath,"/calibrater_0.51.tar.gz"), repos = NULL, type = "source")
  install.packages(paste0(saferpath,"/SAFER_1.1.tar.gz"), repos = NULL, type = "source")}

mcpfad<-"//FUHYS013/Freiberg/Hydrus/montecarlo/"
hydruspfad<-"//FUHYS013/Freiberg/Hydrus/"
projektpfad1<-"//FUHYS013/Freiberg/Hydrus/undisturbed/"
projektpfad2<-"//FUHYS013/Freiberg/Hydrus/undisturbed2/"
programmpfad<-"//FUHYS013/Freiberg/programme/Hydrus-1D 4.xx/"
load("//FUHYS013/Freiberg/daten/all.R")
#load("C:/Users/ThinkPad/Documents/Masterarbeit/daten/bodenparameter/params.R")
source("//FUHYS013/Freiberg/rcode/modellierung/hydrus_input.R")
source("//FUHYS013/Freiberg/rcode/modellierung/montecarlo_function.R")

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
#co2 with changing water paramters
###############################################################


#free ranges
mc<-mc_parallel2(nr=55000,sleep=23,ranges=data.frame(alpha=c(0.27,1.89),
                                               n=c(1.17,4.5),
                                               ks=c(0.007,0.8),
                                               alpha2=c(0.19,1.85),
                                               n2=c(1.16,1.8),
                                               ks2=c(0.007,0.75),
                                               ks3=c(0.0001,0.1),
                                               p_opt=c(0.00001,0.0003),
                                               #DispA=c(0.5,5),
                                               h_opt=c(-80,-10),
                                               p_distr=c(0.005,0.2)),
                fixed=cbind(fixed,fixed_co2),fit.calcium = F,
                dtmax = 10,
                n_parallel = 16,obs=all_s,kin_sol = T)

# #free ranges fit both
# mc<-mc_parallel2(nr=60000,sleep=15,ranges=data.frame(alpha=c(0.27,1.89),
#                                                  n=c(1.17,4.5),
#                                                  ks=c(0.001,0.8),
#                                                  alpha2=c(0.19,1.85),
#                                                  n2=c(1.16,1.8),
#                                                  ks2=c(0.0001,0.75),
#                                                  ks3=c(0.0001,0.1),
#                                                  p_opt=c(0.0000001,0.0003),
#                                                  #DispA=c(0.5,5),
#                                                  h_opt=c(-80,-10),
#                                                  p_distr=c(0.05,0.2)),
#                  fixed=cbind(fixed,fixed_co2),fit.calcium = "both",
#                  dtmax = 10,
#                  n_parallel = 16,obs=all_s)
 
#dist free ranges
mc<-mc_parallel2(nr=120000,sleep=11,ranges=data.frame(alpha=c(0.005,1),
                                                      n=c(1,3),
                                                      ks=c(0.001,0.5),
                                                      alpha2=c(0.005,1),
                                                      n2=c(1,3),
                                                      ks2=c(0.0001,0.5),
                                                      ks3=c(0.0001,0.5),
                                                      p_opt=c(0.000001,0.0003),
                                                      #DispA=c(0.1,3),
                                                      h_opt=c(-80,-10),
                                                      p_distr=c(0.05,0.2)),
                 fixed=cbind(fixed_dist,fixed_co2),fit.calcium = F,
                 dtmax = 10,
                 n_parallel = 16,
                 traintime = 8000,min_nrows = 2200,obs=alldist_s)

#dist free ranges fit.tiefe 1-2
mc<-mc_parallel2(nr=60000,sleep=11,ranges=data.frame(alpha=c(0.005,1),
                                                      n=c(1,3),
                                                      ks=c(0.001,0.5),
                                                      alpha2=c(0.005,1),
                                                      n2=c(1,3),
                                                      ks2=c(0.0001,0.5),
                                                      ks3=c(0.0001,0.5),
                                                      p_opt=c(0.000001,0.0003),
                                                      #DispA=c(0.1,3),
                                                      h_opt=c(-80,-10),
                                                      p_distr=c(0.05,0.2)),
                 fixed=cbind(fixed_dist,fixed_co2),fit.calcium = F,
                 dtmax = 10,
                 n_parallel = 16,
                 traintime = 8000,min_nrows = 2200,obs=alldist_s,fit.tiefe = c(-2,-6))


###############################################################
#co2 with changing water paramters realistic ranges
###############################################################


load("//FUHYS013/Freiberg/daten/bodenparameter/ranges.R")
ranges<-cbind(realistic_ranges,data.frame(ks3=c(0.0001,0.1),
                                          h_opt=c(-80,-10),
                                          p_distr=c(0.0001,0.105)))


mc<-mc_parallel2(nr=55000,sleep=11,ranges=ranges,
                 fixed=cbind(fixed,fixed_co2),n_parallel = 16)

#mc dist
ranges<-cbind(realistic_ranges_dist,data.frame(ks3=c(0.0001,0.1),
                                          h_opt=c(-80,-10),
                                          p_distr=c(0.0001,0.105)))#,
                                          #DispA=c(0.5,2.75)))


mc<-mc_parallel2(nr=55000,sleep=11,ranges=ranges,
                 fixed=cbind(fixed_dist,fixed_co2),min_nrows = 2200,
                 traintime = 8000,n_parallel = 16,obs=alldist_s)


###############################################################
#co2 with changing water paramters realistic ranges changing ks
###############################################################


load("//FUHYS013/Freiberg/daten/bodenparameter/ranges.R")
ranges<-cbind(realistic_ranges,data.frame(ks3=c(0.0001,0.1),
                                          h_opt=c(-80,-10),
                                          p_distr=c(0.00005,0.02)))

ranges$ks<-c(0.0075,1)
ranges$ks2<-c(0.0075,0.4)

mc<-mc_parallel2(nr=22,sleep=11,ranges=ranges,
                 fixed=cbind(fixed,fixed_co2),n_parallel = 16)

mc<-mc_parallel2(nr=55000,sleep=23,ranges=ranges,
                 fixed=cbind(fixed,fixed_co2),n_parallel = 16,fit.calcium = "both",kin_sol = T)


mc<-mc_parallel2(nr=55000,sleep=23,ranges=ranges,
                 fixed=cbind(fixed,fixed_co2),n_parallel = 16,fit.calcium = T,kin_sol = T)


mc<-mc_parallel2(nr=55000,sleep=11,ranges=ranges,
                 fixed=cbind(fixed,fixed_co2),n_parallel = 16,fit.calcium = T)

#fix p_distr
load("//FUHYS013/Freiberg/daten/bodenparameter/ranges.R")
ranges<-cbind(realistic_ranges,data.frame(ks3=c(0.0001,0.3),
                                          h_opt=c(-80,-10),
                                          p_distr=c(0.09,0.105)))

ranges$ks<-c(0.1,1.5)
ranges$ks2<-c(0.1,1.5)

mc<-mc_parallel2(nr=55000,sleep=23,ranges=ranges,
                 fixed=cbind(fixed,fixed_co2),n_parallel = 16,kin_sol = T)

#mc dist
ranges<-cbind(realistic_ranges_dist,data.frame(ks3=c(0.0001,0.1),
                                               h_opt=c(-80,-10),
                                               p_distr=c(0.0001,0.2)))

ranges$ks<-c(0.0075,0.8)
ranges$ks2<-c(0.0075,0.8)

mc<-mc_parallel2(nr=55000,sleep=11,ranges=ranges,
                 fixed=cbind(fixed_dist,fixed_co2),min_nrows = 2200,n_parallel = 16,obs=alldist_s)



########################
#
loadfile<-"mc_60000_dist-realistic"
obs<-alldist_s
plot(alldist_s$CO2_raw)
load(file = paste0(mcpfad,loadfile,".R"))
par<-mc[[2]]
rmse<-mc[[1]]
pars<-cbind(par[which.min(rmse),],fixed_dist,fixed_co2)
tmax<-as.numeric(difftime(max(obs$date),min(obs$date),units = "min"))

n_parallel=20
file<-paste0("UNSC",1:n_parallel) 
projektpfad<-paste0("//FUHYS013/Freiberg/Hydrus/UNSC",1:n_parallel,"/")
programmpfad<-paste0("//FUHYS013/Freiberg/programme/Hydrus-1D_4-",1:n_parallel,"/")

system("taskkill /IM H1D_UNSC.EXE",show.output.on.console=F)

selector.in(params = pars,
            projektpfad = projektpfad[1],
            tmax=tmax,
            UNSC =T,
            free_drain=T,
            print_times = 2000,
            dtmax = 10,
            kin_sol = F)

atmos.in(obs=obs,
         int="all",
         event=NULL,
         alle=T,
         total_t = tmax,
         projektpfad = projektpfad[1])
#hydrus ausführen
hydrus.exe(sleep=10,file = file[1],UNSC=T,taskkill = T,programmpfad = programmpfad[1],wait = T)
out<-read_hydrus.out(projektpfad=projektpfad[1],obs = obs,min_nrows = 100,traintime = 4500)
out[[2]]
min(rmse,na.rm=T)
