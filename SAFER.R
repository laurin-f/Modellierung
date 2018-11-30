if(!"caTools"%in%installed.packages()){
install.packages("caTools")}
if(!"SAFER"%in%installed.packages()){
saferpath<-"C:/Users/ThinkPad/Documents/Masterarbeit/programme/safer_1.1/safer_1.1"
install.packages(paste0(saferpath,"/calibrater_0.51.tar.gz"), repos = NULL, type = "source")
install.packages(paste0(saferpath,"/SAFER_1.1.tar.gz"), repos = NULL, type = "source")}

library(SAFER)
mcpfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/daten/hydrus/montecarlo/"
plotpfad<-"C:/Users/ThinkPad/Documents/Masterarbeit/abbildungen/plots/SAFER/"
loadfile<-"mc_out-nr_20000-11-25_12.02"
load(file = paste0(mcpfad,loadfile,".R"))

par<-mc[[2]]
rmse<-mc[[1]]
nse<-mc[[3]]

X<-as.matrix(par)
Y<-rmse
r<-length(nse)/(ncol(par)+1)
range<-apply(par,2,range)

DistrPar<-vector("list",ncol(par))
for(i in 1:ncol(par)){
  DistrPar[[i]]<-range[,i]
}
# Compute Elementary Effects:
EETind <- EET_indices(r, DistrPar, X, Y, design_type="radial")

EE <- EETind$EE
mi <- EETind$mi
sigma <- EETind$sigma 

# Plot results in the plane (mean(EE),std(EE)):

dev.new()
EET_plot(mi, sigma,  xlab = "Mean of EEs", ylab = "Sd of EEs",  labels = X_labels)

# Use bootstrapping to derive confidence bounds:

Nboot <-100

EETind100 <- EET_indices(r, DistrPar, X, Y, design_type, Nboot)

EE <- EETind100$EE
mi <- EETind100$mi
sigma <- EETind100$sigma
mi_lb <- EETind100$mi_lb
mi_ub <- EETind100$mi_ub
sigma_lb <- EETind100$sigma_lb
sigma_ub <- EETind100$sigma_ub

# Plot bootstrapping results in the plane (mean(EE),std(EE)):
#EET_plot

dev.new()
EET_plot(mi, sigma, mi_lb, mi_ub, sigma_lb, sigma_ub, labels = X_labels)