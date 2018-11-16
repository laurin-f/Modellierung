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
  if(length(ranges)>0){
    if(lhs==T){
      library(lhs)
      if(nr<=100){
        lhs<-optimumLHS(n=nr,k=ncol(ranges))
      }else{
        lhs<-randomLHS(n=nr,k=ncol(ranges))
      }
      
      par<-as.data.frame(matrix(NA,nrow = nr,ncol = ncol(ranges)))
      colnames(par)<-colnames(ranges)
      for (i in 1:ncol(ranges)){
        par[,i]<-ranges[1,i]+(ranges[2,i]-ranges[1,i])*lhs[,i]
        
      }
    }else{
      par<-as.data.frame(apply(ranges, 2, function(x) runif(nr,x[1],x[2])))
    }}
  if(UNSAT==T){
    par_co2<-as.data.frame(apply(co2_ranges, 2, function(x) runif(nr,x[1],x[2])))
  }else{
    par_co2<-NULL
  }
  rmse<-rep(NA,nr)
  
  for (i in 1:nr){
    if(length(ranges)>0){
      pars<-cbind(par[i,],fixed)
    }else{
      pars<-fixed
    }
    
    selector.in(params = pars,co2_params = par_co2[i,],
                projektpfad = pfad,tmax=total_t)
    hydrus.exe(sleep=sleep,file = File,UNSC=UNSAT,taskkill = T)
    Sys.sleep(0.5)
    rmse[i]<-read_hydrus.out(treat=treatm,
                             projektpfad=pfad,
                             UNSC=UNSAT)[[2]][1]
    print(paste(i/nr*100,"%")) 
    
  }
  if(UNSAT==T){
    return(list(rmse,par_co2,par))
  }else{
    return(list(rmse,par))}
}


