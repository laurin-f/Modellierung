# Respiration ####

#create function to calculate soil respiration for different depths
novak <- function(depth, #forest type
                  l_s=30, #constant describing decrease of S_c with depth [cm]
                  temp=10,#temperature in ?C
                  theta,
                  fw_model="pumpanen"){
  
  #parameters for spruce & beech forest (vesterdal et al. 2011)
  #1.Q10 = change of s_c with temp change of 10 ?C
  #2.t_ref =  reference temperature [?C]
  #3.res_t_ref = respiration at t_ref [mymol/m?/s]
  params <- c(Q10=3.8,t_ref=9.5,res_t_ref=1.27 )
  
  # formula for temperature dependent respiration (vesterdal et al. 2011)
  resp <- 10^(((log10(params[1])*(temp-params[2]))/10) + log10(params[3]))#mymol/m2/s
  
  #chage unit from mymol/m2/s to cm3/cm2/s with ideal gas law
  #universal gas constant R
  R<-8.314#J/mol/K=Nm/mol/K
  #amount of molecules n
  n<-resp/1e6/1e4#mol/cm2/s
  #pressure p
  p<-101.3*1000#Pa#N/m2
  #calculate Volume by converting p*V=n*R*t 
  V<-n*R*(temp+273.15)/p #m3/cm2/s
  #respiration rate vco2
  vco2<-V*1e6#cm3/cm2/s
  
  #formula for depths dependent respiration rate in soil (novak 2007)
  #calculate s_10 (source term co2 at surface)
  s_10 <-  vco2/l_s#cm3/cm3/s = 1/s
  #create matrix for s_c results
  s_c<-matrix(nrow=length(depth),ncol=length(temp))
  #loop for s_c with changing temperatures 




  if(fw_model=="fang"){
########################################
#f(w) PATCIS model Fang & Moncrieff (1999)
a<-7.5
c<-0.15
W<-seq(0,1,0.01)
fw<-1-exp(-a*W+c)
plot(W,fw)
}

#fO2<-
 
if(fw_model=="jassal"){ 
##########################################
#f(W) Jassal et al. (2004)

thetaR<-seq(0,1,0.01)

Ws<-c(seq(0,0.1,0.01),seq(0.1,0.3,0.01),seq(0.3,0.8,0.01),seq(0.8,1,0.01))
fws<-c(seq(0,0.6,len=11),seq(0.6,1,len=21),rep(1,51),seq(1,0.5,len=21))
fw<-approx(Ws,fws,xout=thetaR)
plot(fw)
}


if(fw_model=="pumpanen"){
##########################################
#f(W) Pumpanen et al. (2003)

#E0<-max(theta)
thetav<-seq(0,0.61,0.01)
a<-3.83
b<-4.43
g<-0.854
d<-1.25

fw<-thetav
x<-1:100
y<-x^(1/3)
plot(x,y) 
lines(x,log(x))
E0<-max(thetav)
for (i in 1:length(thetav)){
fw[i]<-min(a*thetav[i]^d,b*(E0-thetav[i])^g,1)}#ende j-loop
  0.6*0.6
  plot(thetav/E0,fw)
  abline(v=0.55)
  }#ende pumpanen
  if(fw_model==""){
    fw<-matrix(1,1,length(depth))
  }
for (i in 1:length(depth)){
  #claculate s_c (depth dependent source term co2)
  s_c[i,] <- s_10 *exp(-depth[i]/l_s)*params[1]^((temp - 10)/10)*fw[,i]
}

return(s_c)  #modell output is s_c
#[vol/(vol*s)]=kPa/s
}#ende novak funcktion

novak(1:10,l_s=100,fw_model = "")
##############################################################
#Diffusion function
##############################################################

co2_soil_depth <- function(theta_t,#soil moisture [vol/vol]
                           timestep=1,#timestep for calculation [s] 
                           max_depth=17, #maximal depth for calculation [cm]
                           z=1, #depth for each cell [cm]
                           ambient=0.04, #pco2 kPa
                           temp=20, #temperature [?C] (without depth gradient)
                           outputresolution=60, #in sec = one minute
                           total_t= nrow(epsilon_t), #time of interest in minutes
                           fw_model ="pumpanen",
                           layer1=6,
                           resp_factor=3,
                           dif_factor=0.01,
                           diff_mod="schacki",
                           ls=100#constant describing decrease of S_c with depth [cm]
                          ){
  
  epsilon_t<-theta_t
  E0<-rep(0,ncol(epsilon_t))
  for (j in 1:layer1){
    E0[j]<-max(theta_t[,j])+0.3
    epsilon_t[,j]<-E0[j]-theta_t[,j]}
  for (j in layer1:ncol(theta_t)){
    E0[j]<-max(theta_t[,j])
    epsilon_t[,j]<-E0[j]-theta_t[,j]}
  #epsilon_t<-max(theta_t)-theta_t
  #calculate CO2 production for each cell center (z/2 = 5 cm) and temperature
  p_t <- novak(depth = seq(z,max_depth,z)-z/2, temp=temp,l_s=ls,theta = theta_t,fw_model = fw_model)

  min<-1:nrow(epsilon_t)
  #set number of iteration steps -> J
  #total_t<-nrow(epsilon_t) #time of interest in minutes
  J<-total_t*60/timestep
  #set number of cells -> n
  n <- max_depth/z
  #create output matrix with output for each depth cell after each day
  output<-matrix(nrow=J*timestep/outputresolution,ncol=n) #result matrix
  #set initial values for calculation
  result <- rep(ambient, n)
  #set initial for resultcounter
  resultcounter<-1
  #calculate Ds for 20?C
  
  if(diff_mod=="schacki"){
  Ds20<- 0.496*epsilon_t^1.661  #transfer-function (Schack-Kirchner et al. 2011) 
  #temperature correction of Ds Currie ..
  Ds_t<-Ds20*((temp+273)/293)^1.72

  }
  
  if(diff_mod=="PATCIS"){

    a<-seq(0,1,len=10000)
    epsilon<-seq(0,0.99,by=0.01)
    aopt<-epsilon
    for (i in 1:length(epsilon)){
      eins<-epsilon[i]^(2*a)+(1-epsilon[i])^a
      aopt[i]<-a[which.min(abs(1-eins))]
    }
    
    a<-epsilon_t
    
    for (i in 1:ncol(epsilon_t)){
    a[,i]<-approx(epsilon,aopt,xout=epsilon_t[,i])$y
    }
    range(a)
    eps<-epsilon_t^(2*a)*(epsilon_t/E0)^2
    nco2<-1.75#for c02
    Dg0<-1.39*10^(-5)#m2s-1 for co2
    T0<-273.2#K
    TK<-temp+T0
    P0<-101.3
    P<-P0
    Dg<-Dg0*(TK/T0)^nco2*(P/P0)
    Ds_t<-eps*Dg
    range(Ds_t)

  }

  #time-loop
  for (j in 1:J){
    #calculate time of current step
    time<-j*timestep
    i<-which.min(abs(time/60-min))
    #select CO2 production for temperature at current day
    p<-p_t[,i]*resp_factor
    #select Ds for temperature at current day
    Ds<-Ds_t[i,]*dif_factor
    epsilon<-rep(1,length(epsilon_t[i,]))
    

    #calculate CO2-concentration for top layer with Dirichlet boundary
    c1 <- result[1]  + ifelse(epsilon[1]==0,0,timestep * Ds[1]/epsilon[1]*
      (result[2]- 3*result[1]+2*ambient)/(.75*z^2) + p[1]*z *timestep/epsilon[1])
    #calculate CO2-concentration for cells 2:(n-1)
    c2 <- result[2:(n-1)] + ifelse(epsilon[2:(n-1)]==0,0,timestep * Ds[2:(n-1)] /epsilon[2:(n-1)] *
      (result[3:n]- 2*result[2:(n-1)]+result[1:(n-2)])/(.75*z^2) + 
      p[2:(n-1)]*timestep*z/epsilon[2:(n-1)] )
    #calculate CO2-concentration for top layer with Neumann boundary
    c3 <- result[n] + ifelse(epsilon[n]==0,0,timestep * Ds[n]/epsilon[n]*
      (0- result[n] + result [n-1])/(z^2) + p[n] *timestep*z/epsilon[n])
    #overwrite previous conditions with new result   
    result <- c(c1,c2,c3)
    
    #store result once a day
    if(time%%outputresolution==0){
      output[resultcounter,]<-result
      resultcounter<-resultcounter+1}
    #print progress of calculation
    if ((j/J*100)%%10==0){
      print(paste((j/J)*100,"% complete"))}
  }
  return(output)}

load("C:/Users/ThinkPad/Documents/Masterarbeit/daten/all.R")

tiefenstufen<--seq(2,14,by=4)
thetas<-vector("list",4)
for (i in 1:4){
thetas[[i]]<-all[all$tiefe==tiefenstufen[i],c(1,6)]
colnames(thetas[[i]])<-c("date",paste0("tiefe",i))
}

bfs<-Reduce(function(x,y) merge(x,y,by="date"),thetas)

co2s<-vector("list",4)
for (i in 1:4){
  co2s[[i]]<-all[all$tiefe==tiefenstufen[i],c(1,3)]
  colnames(co2s[[i]])<-c("date",paste0("tiefe",i))
}

co2s<-Reduce(function(x,y) merge(x,y,by="date"),co2s)

temp<-all[all$tiefe==-6,c(1,5)]
bfs<-merge(bfs,temp)
zeitseq<-seq(min(bfs[,1]),max(bfs[,1]),by=60)

bfs_interpol<-apply(bfs[,2:6],2,function(x) approx(x=bfs[,1],y=x,xout = zeitseq,rule=2)$y)

co2s_interpol<-apply(co2s[,2:5],2,function(x) approx(x=co2s[,1],y=x,xout = zeitseq,rule=2)$y)

bfmat<-matrix(NA,nrow(bfs_interpol),17)


for (i in 1:4){
bfmat[,-tiefenstufen[i]]<-bfs_interpol[,i]}
temp<-bfs_interpol[,5]
#nas<-apply(bfmat,1,function(x) length(which(!is.na(x))))
#bfmat<-bfmat[(nas==4),]
bfmat<-t(apply(bfmat,1,function(y) approx(y,xout = 1:17,rule = 2)$y))
#image(bfmat)

#epsilonmat<-max(bfmat)-bfmat


#temp<-rep(20,nrow(bfmat))
co2_mod<-co2_soil_depth(theta_t = bfmat,temp=temp,diff_mod = "PATCIS",dif_factor = 1)
co2_mod_dif_const<-co2_mod
co2_mod2<-co2_soil_depth(theta_t = bfmat,temp=temp,dif_factor = seq(0.4,0.002,len=17))
                         #,total_t = 60*24*3)
co2_mod<-co2_mod2

#co2_mod_ohne_fw<-co2_soil_depth(theta_t = bfmat,temp=temp,fw_model = "")
co2_mod<-co2_mod*10000
#co2_mod_ohne_fw<-co2_mod_ohne_fw*10000

#image(epsilonmat)
#image(co2_mod)

library(reshape)
co2_mod_data.frame<-as.data.frame(co2_mod[,(-tiefenstufen)])
colnames(co2_mod_data.frame)<-tiefenstufen
co2_mod_data.frame$date<-zeitseq#[1:(60*24*3)]
co2_mod_melted<-data.table::melt(co2_mod_data.frame,id=5)
colnames(co2_mod_melted)<-c("date","tiefe","CO2_mod")
source("C:/Users/ThinkPad/Documents/Masterarbeit/rcode/durchf-hrung/event.R")
event<-event()
event<-event[-1,]

library(ggplot2)
ggplot()+
  geom_line(data=co2_mod_melted,aes(date,CO2_mod,col="modelled"))+
  geom_line(data=subset(all,tiefe%in%tiefenstufen),aes(date,CO2_raw,col="measured"))+
  facet_wrap(~tiefe,scales = "free",ncol=1)+
  geom_rect(data=event,aes(xmin=start,xmax=stop,ymin = -Inf, ymax = Inf), alpha = 0.15,fill="blue")


matplot((co2_mod[,(-tiefenstufen)]),type="l",ylim=c(min(c(co2_mod,co2s_interpol)),max(c(co2_mod,co2s_interpol))),lty=2)
matlines(co2s_interpol,type="l",lty=1,alpha=0.1)

matplot((bfmat[,(-tiefenstufen)]),type="l",lty=2)

#matplot((co2_mod_ohne_fw[,(-tiefenstufen)]),type="l",ylim=c(min(c(co2_mod_ohne_fw,co2s_interpol)),max(c(co2_mod_ohne_fw,co2s_interpol))),lty=2)
#matlines(co2s_interpol,type="l",lty=1,alpha=0.1)
