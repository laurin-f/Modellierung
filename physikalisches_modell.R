#############
#CO2 L?sung


caroll<-function(t){
  tk<-t+273.15
  lnH_Mpa<--6.8346+1.2817*10^4/tk-3.7668*10^6/tk^2+2.997*10^8/tk^3
  #kH = 1/H = 1/exp(ln(H))
  kh_Mpa<- 1/exp(lnH_Mpa)#mol/mol/MPa
  kh_mol_l<-kh_Mpa/18*1000 #/ 18 g/mol * 1000 g/l -> mol/l/MPa
  kh<-kh_mol_l/1000 #/1000 -> mol/l/kPa
  return(kh)
}

# Respiration ####

#create function to calculate soil respiration for different depths
novak <- function(l_s=30, #constant describing decrease of S_c with depth [cm]
                  depth, #forest type
                  temp=10){#temperature in ?C
  
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
  for (i in 1:length(depth)){
    #claculate s_c (depth dependent source term co2)
    s_c[i,] <- s_10 *exp(-depth[i]/l_s)*params[1]^((temp - 10)/10)
  }
  #modell output is s_c
  return(s_c)#[vol/(vol*s)]=kPa/s
}

#######################
#calcium l?sung

f_ca<-function(tx=20,pco2=.5,ph=7,gamma=rep(1,6)){
  logkh<-log10(caroll(tx))
  h<-10^-ph
  ca<-10^(8.28-logkh)*h^2/(pco2*gamma[1])
  cahco3<-10^3.02*h/gamma[2]
  hakt<-h/gamma[3]
  hco3<-10^(-6.36+logkh)*pco2/(h*gamma[4])
  co3<-10^(-16.69+logkh)*pco2/(h^2*gamma[5])
  oh<-10^-14/(h*gamma[6])
  return(cbind(ca,cahco3,hakt,hco3,co3,oh))#mol/l
}
plot(seq(4,12,0.1),log(f_ca(ph=seq(4,12,0.1))[,1]))
cas<-f_ca()

wasser<-18/1000#kg/mol#l/mol
ca<-26*1000#g/mol
(cas[1]*ca)/wasser
####################
#ph-wert

#############
#bisection method

bisec<-function(phact=7,interv=14/2,balance=-1,times=0,crit=10E-8,pco2=.5,tx=20,gamma=rep(1,6)){
  while (abs(balance) > crit){ #convergence criterion: stop when balance is lower (close to zero)
    
    interv<-interv/2
    #evaluate equation #14
    cas<-f_ca(ph=phact,pco2=pco2,tx=tx,gamma=gamma)
    balance<-2*cas[1]+sum(cas[2:3])-cas[4]-2*cas[5]-cas[6]
    
    
    phact<- ifelse(balance>0, phact<-phact+interv,phact<-phact-interv)
    times<-times+1
    if (times>1000) {
      phact<-NA 
      times<-NA
      break}
  }
  
  return(cbind(phact))} 
bisec()
##########################
#activity 
aktivity<-function(tx=20,pco2=.5,d=c(6,3,9,4,4.5,3.5)*10^-8,Z=c(2,1,1,1,2,1),gamma=rep(1,6),ph=7,crit=1e-3){
  phneu<-ph
  phalt<-ph-1
  times<-0
  AB<-matrix(c(0,0.4883,0.3241E8,5 ,0.4921, 0.3249E8,
               10, 0.4961, 0.3258E8,
               15, 0.5002, 0.3267E8,
               20, 0.5046, 0.3276E8,
               25, 0.5092, 0.3286E8),6,3,byrow = T)
  AB<-as.data.frame(AB)
  colnames(AB)<-c("temp","A","B")
  
  while (abs(phalt-phneu)>crit) {
    phalt<-phneu
    phneu<-bisec(phact = phalt,tx=tx,pco2 = pco2,gamma=gamma)[1]
    cas<-f_ca(tx=tx,pco2=pco2,ph=phneu,gamma=gamma)
    mu<-0.5*(sum(cas)+3*cas[1]+3*cas[5])
    #loggamma<--AB$A[which.min(abs(AB$temp-tx))]*Z^2*mu^0.5/(1+AB$B[which.min(abs(AB$temp-tx))]*d*mu^0.5)
    loggamma<--approx(AB$temp,AB$A,tx)$y*Z^2*mu^0.5/(1+approx(AB$temp,AB$B,tx)$y*d*mu^0.5)
    gamma<-10^loggamma
    times<-times+1
  }
  return(list(gamma,phneu,cas))}

aktivity(pco2 = 1)

##############################################################
#Diffusion function
##############################################################

co2_soil_depth <- function(epsilon_t=0.2,#air filled soil [vol/vol]
                           timestep=1,#timestep for calculation [s] 
                           max_depth=17, #maximal depth for calculation [cm]
                           z=1, #depth for each cell [cm]
                           ambient=0.04, #pco2 kPa
                           temp=20, #temperature [?C] (without depth gradient)
                           outputresolution=60, #in sec = one minute
                           total_t= 48, #time of interest in hours
                           ls=30,#constant describing decrease of S_c with depth [cm]
                           sec=1:ncol(epsilon_t)){
  
  #calculate CO2 production for each cell center (z/2 = 5 cm) and temperature
  p_t <- novak(depth = seq(z,max_depth,z)-z/2, temp=temp,l_s=ls)
  
  #set number of iteration steps -> J
  J<-total_t*60*60/timestep
  #set number of cells -> n
  n <- max_depth/z
  #create output matrix with output for each depth cell after each day
  output<-matrix(nrow=J*timestep/outputresolution,ncol=n) #result matrix
  #set initial values for calculation
  result <- rep(ambient, n)
  #set initial for resultcounter
  resultcounter<-1
  #calculate Ds for 20?C
  Ds20<- 0.496*epsilon_t^1.661  #transfer-function (Schack-Kirchner et al. 2011) 
  #temperature correction of Ds Currie ..
  Ds_t<-Ds20*((temp+273)/293)^1.72 
  
  
  #time-loop
  for (j in 1:J){
    #calculate time of current step
    time<-j*timestep
    i<-which.min(abs(sec-time))
    #select CO2 production for temperature at current day
    p<-p_t[,i]
    #select Ds for temperature at current day
    Ds<-Ds_t[i,] 
    epsilon<-epsilon_t[i,]
    
    #calculate CO2-concentration for top layer with Dirichlet boundary
    c1 <- result[1]  + timestep * Ds[1]/epsilon[1]*
      (result[2]- 3*result[1]+2*ambient)/(.75*z^2) + p[1]*z *timestep/epsilon[1]
    #calculate CO2-concentration for cells 2:(n-1)
    c2 <- result[2:(n-1)] + timestep * Ds[2:(n-1)] /epsilon[2:(n-1)] *
      (result[3:n]- 2*result[2:(n-1)]+result[1:(n-2)])/(.75*z^2) + 
      p[2:(n-1)]*timestep*z/epsilon[2:(n-1)] 
    #calculate CO2-concentration for top layer with Neumann boundary
    c3 <- result[n] + timestep * Ds[n]/epsilon[n]*
      (0- result[n] + result [n-1])/(z^2) + p[n] *timestep*z/epsilon[n]
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

#calcium function
ca_soil_depth<-function(pco2=co2_vals,tx=t){
  pco2_cal<-seq(min(pco2),max(pco2),length.out = 50)
  t_cal<-seq(min(tx),max(tx),.5)
  pco2_l<-rep(pco2_cal,length(t_cal))
  t_l<-rep(t_cal,each=length(pco2_cal))
  ca_cal<-rep(0,length(pco2_l))
  for(i in 1:length(pco2_l)){
    ca_cal[i]<-   aktivity(tx=t_l[i],pco2=pco2_l[i])[[3]][1]
    if ((i/length(pco2_l)*100)%%10==0){
      print(paste((i/length(pco2_l))*100,"% complete"))}}
  
  ca_fm<-glm(ca_cal~log(pco2_l)*t_l+pco2_l*t_l)
  ca<-apply(co2_vals, 2, function(x) predict(ca_fm,data.frame(pco2_l=x,t_l=20)))
  molare_masse_calcium<-40*1000#mg/mol
  ca_mg<-(ca*molare_masse_calcium)#mg/l
  return(ca_mg)
}
ca_mg<-ca_soil_depth()

#####
#tests

epsilon<-matrix(0.1,60*60,18)
#for (i in 1:(60*60)){ epsilon[i,]<-seq(0.01+i*0.0005,0.8-i*0.0005,length.out = 60*60)[i]}
image(epsilon)
t<-rep(20,3600)

co2_vals<-co2_soil_depth(temp=t,epsilon_t = epsilon)

matplot(co2_vals[1:(60*2),],type="l")
image(co2_vals)
plot(co2_vals[2880,])
pco2_cal<-seq(min(co2_vals),max(co2_vals),length.out = 100)
t_cal<-seq(min(t),max(t),0.25)
pco2_l<-rep(pco2_cal,length(t_cal))
t_l<-rep(t_cal,each=length(pco2_cal))
ca_cal<-rep(0,length(pco2_l))
for(i in 1:length(pco2_l)){
  ca_cal[i]<-   aktivity(tx=t_l[i],pco2=pco2_l[i])[[3]][1]
  
  if ((i/length(pco2_l)*100)%%10==0){
    print(paste((i/length(pco2_l))*100,"% complete"))}}

ca_fm<-glm(ca_cal~log(pco2_l)*t_l+pco2_l*t_l)

library(mgcv)

ca_gam<-gam(ca_cal~s(pco2_l)+s(t_l)+s(pco2_l,by=t_l)+s(t_l,by=pco2_l))
plot(ca_cal~pco2_l)
points(predict(ca_fm)~pco2_l,col=2,pch=".")
points(predict(ca_gam)~pco2_l,col=3,pch=".")

summary(ca_gam)
summary(ca_fm)
ca<-apply(co2_vals, 2, function(x) predict(ca_fm,data.frame(pco2_l=x,t_l=20)))
molare_masse_calcium<-40*1000#mg/mol
ca_mg<-(ca*molare_masse_calcium)#mg/l
points(ca~co2_vals,col=2)
