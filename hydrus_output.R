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
params<-data.frame(thr=0.068,
                   ths=c(0.6,0.51),
                   alpha=c(0.075,0.035),
                   n=c(4.0,4),
                   ks=c(0.07,0.07),
                   l=0.5,
                   hseep = 0)

params$hseep<--50
params$ks<-0.07
params$l<-0.5
params$alpha<-0.07
params$n<-4
params$thr<-0.06
selector.in(params = params)
atmos.in(17,180,4000)
profile.in(n_nodes = 19,Mat = c(rep(1,10),rep(2,9)))
hydrus.exe(sleep = 1)


sub<-read_hydrus.out(treat=17)[[1]]
vals<-read_conc.out(projektpfad = projektpfad1)

ggplot(sub)+geom_point(aes(t_min,theta,col="obs"),na.rm = T)+geom_line(aes(t_min,theta_mod,col="mod"),na.rm = T)+facet_wrap(~tiefe,ncol=1)


ggplot(sub)+geom_point(aes(t_min,CO2_raw,col="obs"),na.rm = T)+geom_line(aes(t_min,CO2_mod*1000000,col="mod"),na.rm = T)+facet_wrap(~tiefe,ncol=1)

ggplot(vals)+geom_line(aes(time,Ca,col=as.factor(Depth)))
ggplot(vals)+geom_line(aes(Ca,Depth,col=as.factor(time)))
ggplot(vals)+geom_tile(aes(time,Depth,fill=Ca))


####################################
#Monte Carlo



monte_carlo<-function(nr=100,
                      ranges=data.frame(thr=c(0.05,0.1),
                      ths=c(0.4,0.7),
                      alpha=c(0.005,0.075),
                      n=c(1.5,4.5),
                      ks=c(0.01,1),
                      hseep=c(-20,0),
                      l=c(0.5,0.5)),
                      pfad=projektpfad1,
                      UNSAT=T,
                      File="undisturbed",
                      treatm=17,
                      sleep=1){
  par<-as.data.frame(apply(ranges, 2, function(x) runif(nr,x[1],x[2])))
  rmse<-rep(NA,nr)
for (i in 1:nr){
  selector.in(params = par[i,],
              projektpfad = pfad)
  hydrus.exe(sleep=sleep,file = File,UNSC=UNSAT)
  Sys.sleep(0.5)
  rmse[i]<-read_hydrus.out(treat=treatm,
                           projektpfad=pfad,
                           UNSC=UNSAT)[[2]][1]
  print(paste(i/nr*100,"%")) 
  
}
 return(list(rmse,par))
}

mc<-monte_carlo(nr=100)
par<-mc[[2]]
rmse<-mc[[1]]
plot(par$alfa,rmse)


pars=
mc<-monte_carlo(nr=1000,ranges=data.frame(thr=c(0.05,0.1),
               ths=c(0.4,0.7),
               alpha=c(0.05,0.1),
               n=c(3,5),
               ks=c(0.01,1),
               hseep=round(c(-50,-10),2))
               ,pfad = projektpfad2,UNSAT = F,File = "undisturbed2",sleep = 0.1)


par<-mc[[2]]
rmse<-mc[[1]]

par(mfrow=c(3,2),mar=c(3,4,2,1))
for(i in 1:6) plot(par[,i],rmse[1:100],main = colnames(par)[i])
par(mfrow=c(1,1))

rmse[56]
pars<-par[which.min(rmse),]
selector.in(params = pars,
            projektpfad = projektpfad2)

hydrus.exe(sleep=1,file = "undisturbed2",UNSC=F)
out<-read_hydrus.out(treat=treatm,
                         projektpfad=pfad,
                         UNSC=UNSAT)[[1]]


ggplot(out)+geom_point(aes(t_min,theta,col="obs"),na.rm = T)+geom_line(aes(t_min,theta_mod,col="mod"),na.rm = T)+facet_wrap(~tiefe,ncol=1)

#######################################
#without co2
selector.in(thr=0.068,
            ths=c(0.6,0.51,0.2),
            alpha=c(0.075,0.035,0.005),
            n=c(4.0,4,1.5),
            ks=c(0.07,0.07,0.02),
            l=0.5,
            hseep = 0,
            projektpfad = projektpfad2)
atmos.in(0.26,180,4000,
         projektpfad = projektpfad2)
profile.in(projektpfad = projektpfad2,Mat = c(rep(1,7),rep(2,10),3,3))
hydrus.exe(sleep = 2,file = "undisturbed2",UNSC=T)

sub<-read_hydrus.out(treat=17,projektpfad = projektpfad2,UNSC=T)[[1]]

ggplot(sub)+geom_point(aes(t_min,theta,col="obs"),na.rm = T)+geom_line(aes(t_min,theta_mod,col="mod"),na.rm = T)+facet_wrap(~tiefe,ncol=1)




ggplot()+geom_path(data=sub,aes(t_min,theta,col=as.factor(tiefe)))+geom_line(data=vals_mod,aes(t_min,theta_mod,col=as.factor(tiefe)))
+facet_wrap(~tiefe,ncol=1)

ggplot()+geom_line(data=vals_mod,aes(t_min,theta_mod,col=1))+geom_line(data=modelled_approx,aes(t_min,theta_mod,col=2))+facet_wrap(~tiefe)


plot(sub$t_min,sub$theta,type="l")
lines(obs_node$time,obs_node$theta.1,col=2)
min(sub$theta,na.rm=T)

plot(sub$t_min,sub$CO2_raw)
plot(obs_node$time,obs_node$CO2.2*200000,col=2)

