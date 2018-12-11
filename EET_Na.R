EET_na<- function (r, xrange, X, Y, design_type, Nboot = 0, alfa = 0.05) 
{
  library("assertthat", lib.loc="~/R/win-library/3.3")
  stopifnot(is.scalar(r), r >= 0, r == floor(r))
  n <- nrow(X)
  M <- ncol(X)
  Dr <- sapply(xrange, diff)
  stopifnot(n == r * (M + 1), nrow(Y) == n, length(Y) == n, 
            Dr > 0, length(Dr) == M, design_type %in% c("radial", 
                                                        "trajectory"))
  stopifnot(is.scalar(Nboot), Nboot >= 0, Nboot == floor(Nboot), 
            is.numeric(alfa), alfa <= 1, alfa >= 0)
  EE <- matrix(NA, r, M)
  k <- 1
  ki <- 1
  for (i in 1:r) {
    for (j in 1:M) {
      if (design_type == "radial") {
        EE[i, j] <- abs(Y[k + 1] - Y[ki])/abs(X[k + 
                                                  1, j] - X[ki, j]) * Dr[j]
      }
      else {
        idx <- which(abs(X[k + 1, ] - X[k, ]) > 0)
        EE[i, idx] <- abs(Y[k + 1] - Y[k])/abs(X[k + 
                                                   1, idx] - X[k, idx]) * Dr[idx]
      }
      k = k + 1
    }
    k = k + 1
    ki = k
  }
  if (Nboot > 1) {
    B <- matrix(sample.int(r, r * Nboot, replace = TRUE), 
                r, Nboot)
    mi_all <- apply(B, 2, function(b) colMeans(EE[b, ]))
    sigma_all <- apply(B, 2, function(b) apply(EE[b, ], 
                                               2, sd))
    mi <- rowMeans(mi_all)
    mi_sd <- apply(mi_all, 1, sd)
    mi_lb <- apply(mi_all, 1, sort)
    mi_lb <- mi_lb[max(1, round(Nboot * alfa/2)), ]
    mi_ub <- apply(mi_all, 1, sort)
    mi_ub <- mi_ub[round(Nboot * (1 - alfa/2)), ]
    sigma <- rowMeans(sigma_all)
    sigma_sd <- apply(sigma_all, 1, sd)
    sigma_lb <- apply(sigma_all, 1, sort)
    sigma_lb <- sigma_lb[max(1, round(Nboot * alfa/2)), 
                         ]
    sigma_ub <- apply(sigma_all, 1, sort)
    sigma_ub <- sigma_ub[round(Nboot * (1 - alfa/2)), ]
    cat(sprintf("\n\t mean(EE) std(EE)\n"))
    cat(sprintf("X%d:\t %2.3f\t %2.3f\n", 1:M, mi, sigma))
    robj <- list(mi = mi, sigma = sigma, EE = EE, mi_sd = mi_sd, 
                 sigma_sd = sigma_sd, mi_lb = mi_lb, sigma_lb = sigma_lb, 
                 mi_ub = mi_ub, sigma_ub = sigma_ub, mi_all = mi_all, 
                 sigma_all = sigma_all)
  }
  else {
    EE[EE==Inf]<-NA
    mi <- colMeans(EE,na.rm = T)
    sigma <- apply(EE, 2, sd,na.rm=T)
    robj <- list(mi = mi, sigma = sigma, EE = EE)
  }
  cat(sprintf("\n\t mean(EE) std(EE)\n"))
  cat(sprintf("X%d:\t %2.3f\t %2.3f\n", 1:M, mi, sigma))
  return(robj)
}
