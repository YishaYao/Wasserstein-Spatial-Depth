
library(MASS); library(transport); library(abind)

load("~/Documents/papers/Wasserstein_depth/REAL_DATA/area1_alphebat_order/area1_final.RData")
ARR <- abind(AREA1, along=3); rm(AREA1); ARR <- ARR*0.1
n=40;  M=150;  d=12; 
samp <- array(0, dim = c(n, d, M))
for(m in 1:M){
  samp[,,m] <- t(ARR[m,,])
}

Wdist <- matrix(0, M, M)
for (m in 1:M){
  #samp[,,m] <- t(ARR[m,,])
  if (m >1){
    for (i in 1:(m-1)){
      a <- pp(samp[,,m])
      b <- pp(samp[,,i])
      Wdist[i,m] <- wasserstein(a,b,p=2, prob = TRUE)
    }
  }
}

OT <- list()
mtemp <- matrix(0, n, M-1); a <- pp(samp[,,1])
for (k in 2:M){
  b <- pp(samp[,,k])
  mtemp[,(k-1)] <- transport(a, b, p = 2, method = "auctionbf", fullreturn=FALSE, control = list(), threads=1)$to
}
OT[[1]] <- mtemp
for (m in 2:(M-1)){
  mtemp <- matrix(0, n, M-1);  a <- pp(samp[,,m])
  for (k in 1:(m-1)){
    b <- pp(samp[,,k])
    mtemp[,k] <- transport(a, b, p = 2, method = "auctionbf", fullreturn=FALSE, control = list(), threads=1)$to
  }
  for (k in (m+1):M){
    b <- pp(samp[,,k])
    mtemp[,(k-1)] <- transport(a, b, p = 2, method = "auctionbf", fullreturn=FALSE, control = list(), threads=1)$to
  }
  OT[[m]] <- mtemp
}
mtemp <- matrix(0, n, M-1);  a <- pp(samp[,,M])
for (k in 1:(M-1)){
  b <- pp(samp[,,k])
  mtemp[,k] <- transport(a, b, p = 2, method = "auctionbf", fullreturn=FALSE, control = list(), threads=1)$to
}
OT[[M]] <- mtemp

Wdepth <- rep(0, M)
for (m in 1:M){
  ind <- c(1:M);  ind <- ind[! ind %in% c(m)]
  sorce <- samp[,,m];  transmap <- OT[[m]]
  stotal <- 0
  for (i in 1:n){
    s <- 0
    for (k in ind){
      target <- samp[,,k];  cind <- k-as.integer(k>m);  target_indeces <- transmap[, cind]; id <- target_indeces[i]
      itemk <- (sorce[i,] - target[id,])/max(Wdist[m,k], Wdist[k,m])
      s <- s + itemk
    }
    s <- s/(M-1)
    L2norm <- sum(s^2)
    stotal <- stotal + L2norm
  }
  Wdepth[m] <- 1- sqrt(stotal/n)
}

save(Wdist, OT, Wdepth, file = "~/Documents/papers/Wasserstein_depth/REAL_DATA/area1_alphebat_order/temp_Wdepth.RData")


lower_bound <- quantile(Wdepth, 0.05);  oind <- which(Wdepth <= lower_bound); 
c(1874:2023)[oind]  # 1879 1929 1940 1942 1947 1956 1963 2018

