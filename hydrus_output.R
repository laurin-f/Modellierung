hydruspfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/hydrus/"
projektpfad1<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/hydrus/undisturbed/"
projektpfad2<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/hydrus/undisturbed2/"
programmpfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/programme/Hydrus-1D 4.xx/"


load("C:/Users/ThinkPad/Documents/Masterarbeit/daten/all.R")
load("C:/Users/ThinkPad/Documents/Masterarbeit/daten/bodenparameter/params.R")

source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/modellierung/hydrus_input.R")

######################################################
#run modell

library(ggplot2)
params

params$hseep<--50
params$ks<-0.7
params$l<-0.5
params$alpha<-0.07
params$n<-1.5
params$thr<-0.06

selector.in(params = data.frame(thr=0.08,
                   ths=c(0.75,0.63),
                   alpha=c(0.65,0.55),
                   n=c(1.8,1.8),
                   ks=c(1,1),
                   thr_bot=c(0.05),
                   ths_bot=c(0.6),
                   alpha_bot=c(0.6),
                   n_bot=c(1.8),
                   ks_bot=c(0.2),
                   l=0.5,
                   hseep = -100),
            co2_params =data.frame(p_opt=0.001,
                       act_en=6677,
                       h_opt=-50,
                       h_crit=-10^6),
            tmax = 4000,
            free_drain = T,
            print_times = 100)


#selector.in(params = params,n_mats = 2)
atmos.in(17,180,4000)
profile.in(n_nodes = 18,Mat = c(rep(1,10),rep(2,7),3))
hydrus.exe(sleep = 10)

out<-read_hydrus.out(treat=17)
sub<-out[[1]]
rmse<-out[[2]]
vals<-read_conc.out(projektpfad = projektpfad1)

ggplot(sub)+geom_point(aes(t_min,theta,col="obs"),na.rm = T)+geom_line(aes(t_min,theta_mod,col="mod"),na.rm = T)+facet_wrap(~tiefe,ncol=1)

ggplot(subset(sub,tiefe==-17))+geom_point(aes(t_min,q_interpol,col="obs"),na.rm = T)+geom_line(aes(t_min,q_mod,col="mod"),na.rm = T)

sum(sub$q_mod,na.rm = T)/2/5
sum(sub$q_interpol,na.rm = T)/2

ggplot(subset(sub,tiefe%in%c(-10,-14)))+geom_point(aes(t_min,CO2_raw,col="obs"),na.rm = T)+geom_point(aes(t_min,CO2_mod,col="mod"),na.rm = T)
#+facet_wrap(~tiefe,ncol=1)

ggplot(vals)+geom_line(aes(time,Ca,col=as.factor(Depth)))
ggplot(vals)+geom_path(aes(Ca,Depth,col=as.factor(time)))
ggplot(vals)+geom_tile(aes(time,Depth,fill=Ca))






#####################################################
#modell all

tmax_all<-as.numeric(difftime(max(all$date),min(all$date),units = "min"))
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
atmos.in(all=T,total_t = tmax_all)
profile.in(n_nodes = 18,Mat = c(rep(1,10),rep(2,7),3))
hydrus.exe(sleep = 30)

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




###################################################
#
atmos.in(6,480,4000)
profile.in(n_nodes = 18,Mat = c(rep(1,10),rep(2,5),3,3))
hydrus.exe(sleep = 1)

sub<-read_hydrus.out(treat=6)[[1]]
rmse<-read_hydrus.out(treat=6)[[2]]
vals<-read_conc.out(projektpfad = projektpfad1)

ggplot(sub)+geom_point(aes(t_min,theta,col="obs"),na.rm = T)+geom_line(aes(t_min,theta_mod,col="mod"),na.rm = T)+facet_wrap(~tiefe,ncol=1)

ggplot(subset(sub,tiefe==-17))+geom_point(aes(t_min,q_interpol,col="obs"),na.rm = T)+geom_line(aes(t_min,q_mod,col="mod"),na.rm = T)

sum(sub$q_mod,na.rm = T)/2
sum(sub$q_interpol,na.rm = T)/2

ggplot(subset(sub,tiefe%in%c(-10,-14)))+geom_point(aes(t_min,CO2_raw,col="obs"),na.rm = T)+geom_point(aes(t_min,CO2_mod,col="mod"),na.rm = T)
#+facet_wrap(~tiefe,ncol=1)

ggplot(vals)+geom_line(aes(time,Ca,col=as.factor(Depth)))
ggplot(vals)+geom_line(aes(Ca,Depth,col=as.factor(time)))
ggplot(vals)+geom_tile(aes(time,Depth,fill=Ca))


#####################################################
#
atmos.in(1,3000,4000)
profile.in(n_nodes = 18,Mat = c(rep(1,10),rep(2,5),3,3))
hydrus.exe(sleep = 1)

sub<-read_hydrus.out(treat=1)[[1]]
rmse<-read_hydrus.out(treat=1)[[2]]
vals<-read_conc.out(projektpfad = projektpfad1)

ggplot(sub)+geom_point(aes(t_min,theta,col="obs"),na.rm = T)+geom_line(aes(t_min,theta_mod,col="mod"),na.rm = T)+facet_wrap(~tiefe,ncol=1)

ggplot(subset(sub,tiefe==-17))+geom_point(aes(t_min,q_interpol,col="obs"),na.rm = T)+geom_line(aes(t_min,q_mod,col="mod"),na.rm = T)

sum(sub$q_mod,na.rm = T)/2
sum(sub$q_interpol,na.rm = T)/2

ggplot(subset(sub,tiefe%in%c(-10,-14)))+geom_point(aes(t_min,CO2_raw,col="obs"),na.rm = T)+geom_point(aes(t_min,CO2_mod,col="mod"),na.rm = T)
#+facet_wrap(~tiefe,ncol=1)

ggplot(vals)+geom_line(aes(time,Ca,col=as.factor(Depth)))
ggplot(vals)+geom_line(aes(Ca,Depth,col=as.factor(time)))
ggplot(vals)+geom_tile(aes(time,Depth,fill=Ca))



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

monte_carlo<-function(nr=100,
                      ranges=data.frame(alpha=c(0.005,0.6),
                                        n=c(1.5,4),
                                        ks=c(0.01,2),
                                        alpha2=c(0.005,0.6),
                                        n2=c(1.5,4),
                                        ks2=c(0.01,2),
                                        alpha_bot=c(0.005,0.6),
                                        n_bot=c(1.2,3),
                                        ks_bot=c(0.001,0.1)),
                      fixed=data.frame(thr=0.11,
                                       ths=0.75,
                                       thr2=0.13,
                                       ths2=0.64,
                                       thr_bot=0.13,
                                       ths_bot=0.64,
                                       hseep=-100,
                                       l=0.5),
                      co2_ranges=data.frame(p_opt=c(0.002,0.005),
                                            act_en=c(6500,7000),
                                            h_opt=c(-120,-50),
                                            h_crit=c(-10^-4,-10^-6)),
                      pfad=projektpfad1,
                      UNSAT=T,
                      File="undisturbed",
                      treatm=17,
                      sleep=1,
                      total_t=4000,
                      lhs=T){
  if(lhs==T){
    library(lhs)
    lhs<-optimumLHS(n=nr,k=ncol(ranges))
    par<-as.data.frame(matrix(NA,nrow = nr,ncol = ncol(ranges)))
    colnames(par)<-colnames(ranges)
    for (i in 1:ncol(ranges)){
      par[,i]<-ranges[1,i]+(ranges[2,i]-ranges[1,i])*lhs[,i]
      
    }
    }else{
  par<-as.data.frame(apply(ranges, 2, function(x) runif(nr,x[1],x[2])))
    }
  if(UNSAT==T){
  par_co2<-as.data.frame(apply(co2_ranges, 2, function(x) runif(nr,x[1],x[2])))
  }else{
    par_co2<-NULL
  }
  rmse<-rep(NA,nr)
  
for (i in 1:nr){
  pars<-cbind(par[i,],fixed)
  selector.in(params = pars,co2_params = par_co2[i,],
              projektpfad = pfad,tmax=total_t)
  hydrus.exe(sleep=sleep,file = File,UNSC=UNSAT,taskkill = T)
  Sys.sleep(0.5)
  rmse[i]<-read_hydrus.out(treat=treatm,
                           projektpfad=pfad,
                           UNSC=UNSAT)[[2]][1]
  print(paste(i/nr*100,"%")) 
  
}
 return(list(rmse,par))
}






atmos.in(all=T,total_t = tmax_all, projektpfad = projektpfad2)
#atmos.in(17,180,4000, projektpfad = projektpfad2)

profile.in(n_nodes = 18,
           th=seq(0.11,0.2,len=18),
           Mat = c(rep(1,10),rep(2,6),3,3),
           projektpfad = projektpfad2)

mc<-monte_carlo(nr=10000,ranges=data.frame(alpha=c(0.05,0.75),
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



#save(mc,file = paste0("C:/Users/ThinkPad/Documents/Masterarbeit/daten/hydrus/montecarlo/mc",Sys.Date(),".R"))
#load(file = paste0("C:/Users/ThinkPad/Documents/Masterarbeit/daten/hydrus/montecarlo/mc",Sys.Date(),".R"))




############################################################
#
atmos.in(all=T,total_t = tmax_all, projektpfad = projektpfad1)

selector.in(params = pars,
            co2_params =data.frame(p_opt=0.001,
                                     act_en=6677,
                                     h_opt=-50,
                                     h_crit=-10^-6),
            projektpfad = projektpfad1,tmax=tmax_all)

profile.in(n_nodes = 18,
           th=seq(0.11,0.2,len=18),
           Mat = c(rep(1,10),rep(2,6),3,3),
           projektpfad = projektpfad1)
hydrus.exe(sleep=1,file = "undisturbed",UNSC=T)

out<-read_hydrus.out(treat="all",
                         projektpfad=projektpfad1,
                         UNSC=T)[[1]]

tiefenstufen<-c(-2,-6,-10,-14)
ggplot(subset(out,tiefe%in%tiefenstufen))+geom_point(aes(t_min,theta,col="obs"),na.rm = T)+geom_line(aes(t_min,theta_mod,col="mod"),na.rm = T)+facet_wrap(~tiefe,ncol=1)

ggplot(subset(out,tiefe==-17))+geom_point(aes(t_min,q_interpol*5,col="obs"),na.rm = T)+geom_line(aes(t_min,q_mod,col="mod"),na.rm = T)


ggplot(subset(out,tiefe%in%c(-10,-14)&treatment==17))+geom_point(aes(t_min,CO2_raw,col="obs"),na.rm = T)+geom_point(aes(t_min,CO2_mod,col="mod"),na.rm = T)


#####################################
#
mc<-monte_carlo(nr=10,sleep=10)
par<-mc[[2]]
rmse<-mc[[1]]
plot(par$alpha,rmse)

