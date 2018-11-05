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
plot(fw)}

#fO2<-
 
if(fw_model=="jassal"){ 
##########################################
#f(W) Jassal et al. (2004)

thetaR<-seq(0,1,0.01)

Ws<-c(seq(0,0.1,0.01),seq(0.1,0.3,0.01),seq(0.3,0.8,0.01),seq(0.8,1,0.01))
fws<-c(seq(0,0.6,len=11),seq(0.6,1,len=21),rep(1,51),seq(1,0.5,len=21))
fw<-approx(Ws,fws,xout=thetaR)
plot(fw)}


if(fw_model=="pumpanen"){
##########################################
#f(W) Pumpanen et al. (2003)

#E0<-max(theta)
thetav<-theta
a<-3.83
b<-4.43
g<-0.854
d<-1.25

fw<-thetav
for (j in 1:ncol(thetav)){
  E0<-max(thetav[,j])
for (i in 1:nrow(thetav)){
fw[i,j]<-min(a*thetav[i,j]^d,b*(E0-thetav[i,j])^g,1)}}#ende j-loop
  }#ende pumpanen
for (i in 1:length(depth)){
  #claculate s_c (depth dependent source term co2)
  s_c[i,] <- s_10 *exp(-depth[i]/l_s)*params[1]^((temp - 10)/10)*fw[,i]
}

return(s_c)  #modell output is s_c
#[vol/(vol*s)]=kPa/s
}#ende novak funcktion


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
                           total_t= 48*60, #time of interest in hours
                           ls=30#constant describing decrease of S_c with depth [cm]
                          ){
  
  epsilon_t<-theta_t
  for (j in 1:ncol(theta_t)){
    E0<-max(theta_t[,j])
    epsilon_t<-E0-theta_t[,j]}
  epsilon_t<-max(theta_t)-theta_t
  #calculate CO2 production for each cell center (z/2 = 5 cm) and temperature
  p_t <- novak(depth = seq(z,max_depth,z)-z/2, temp=temp,l_s=ls,theta = theta_t)

  min<-1:nrow(epsilon_t)
  #set number of iteration steps -> J
  total_t<-nrow(epsilon_t) #time of interest in minutes
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
  Ds20<- 0.496*epsilon_t^1.661  #transfer-function (Schack-Kirchner et al. 2011) 
  #temperature correction of Ds Currie ..
  Ds_t<-Ds20*((temp+273)/293)^1.72 
j<-1
  #time-loop
  for (j in 1:J){
    #calculate time of current step
    time<-j*timestep
    i<-which.min(abs(time/60-min))
    #select CO2 production for temperature at current day
    p<-p_t[,i]
    #select Ds for temperature at current day
    Ds<-Ds_t[i,] 
    epsilon<-epsilon_t[i,]
    

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
zeitseq<-seq(min(bfs[,1]),max(bfs[,1]),by=60)

bfs_interpol<-apply(bfs[,2:5],2,function(x) approx(x=bfs[,1],y=x,xout = zeitseq,rule=2)$y)


bfmat<-matrix(NA,nrow(bfs_interpol),17)


for (i in 1:4){
bfmat[,-tiefenstufen[i]]<-bfs_interpol[,i]}

#nas<-apply(bfmat,1,function(x) length(which(!is.na(x))))
#bfmat<-bfmat[(nas==4),]
bfmat<-t(apply(bfmat,1,function(y) approx(y,xout = 1:17,rule = 2)$y))
image(bfmat)
#temp<-all$temp[all$tiefe==-6]
#epsilonmat<-max(bfmat)-bfmat


temp<-rep(20,nrow(bfmat))
co2_mod<-co2_soil_depth(theta_t = bfmat,temp=temp)
co2_mod<-co2_mod*10000

#image(epsilonmat)
image(co2_mod)
plot(co2_mod[,17])
matplot((co2_mod[seq(1,nrow(co2_mod),100),(-tiefenstufen)]),type="l")
matplot(bfmat[seq(1,nrow(co2_mod),100),(-tiefenstufen)],type="l")
