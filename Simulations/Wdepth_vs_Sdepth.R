library(MASS)
library(transport)

# Case 1&2: Draw n sample points for each of the M Gaussian distributions; each multivariate Gaussian distribution has dimension d.
n=500;  M=500;  d=10
set.seed(98)
means <- matrix(0, M, d)
alp <- 0.2;  cov <- matrix(0, d, d);  for(i in 1:d){ for(j in 1:d){ cov[i,j] <- alp^(abs(i-j)) } }
samp <- array(0, dim = c(n, d, M))
Wdist <- matrix(0, M, M)
for (m in 1:M){
  mu <- runif(d, min = -2, max = 2)
  means[m,] <- mu
  samp[,,m] <- mvrnorm(n, mu, cov)
  if (m >1){
    for (i in 1:(m-1)){
      a <- pp(samp[,,m])
      b <- pp(samp[,,i])
      Wdist[i,m] <- wasserstein(a,b,p=2, prob = TRUE)
    }
  }
}

OT <- list()
#the i-th element of OT, OT[[i]] records the mapping information of the i-th distribution to the other distributions (in ascending order),
# i.e., from i-th distribution to the first, second, ..., i-1, i+1, ..., M-th distribution.
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
# compute the spatial depths of the mean vectors
Sdepth <- rep(0, M)
for (m in 1:M){
  target_mu <- means[m,]
  ind <- c(1:M);  ind <- ind[! ind %in% c(m)]
  s <- rep(0, d)
  for (k in ind){
    other_mu <- means[k,]
    norm2 <- sqrt(sum((other_mu-target_mu)^2))
    s <- s + (target_mu-other_mu)/norm2
  }
  v <- s/(M-1)
  Sdepth[m] <- 1- sqrt(sum(v^2))
}
save(Wdepth, Sdepth, file = "~/Documents/papers/Wasserstein_depth/Wdepth_vs_Sdepth_fixed_nonidentity_cov.RData")


# Case 3: Unifs on d cubes
n=500;  M=500;  d=10
set.seed(30)
means <- matrix(0, M, d)
samp <- array(0, dim = c(n, d, M))
Wdist <- matrix(0, M, M)
for (m in 1:M){
  means[m,] <- mu <- mvrnorm(1, rep(0,d), diag(d))
  for (i in 1:n){
    for (j in 1:d){ samp[i,j,m] <- runif(1, mu[j]-0.5, mu[j]+0.5) }
  }
  if (m >1){
    for (k in 1:(m-1)){
      a <- pp(samp[,,m])
      b <- pp(samp[,,k])
      Wdist[k,m] <- wasserstein(a,b,p=2, prob = TRUE)
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
      target <- samp[,,k];  cind <- k-as.integer(k>m);  id <- transmap[i, cind] 
      itemk <- (sorce[i,] - target[id,])/max(Wdist[m,k], Wdist[k,m])
      s <- s + itemk
    }
    s <- s/(M-1)
    L2norm <- sum(s^2)
    stotal <- stotal + L2norm
  }
  Wdepth[m] <- 1- sqrt(stotal/n)
}
# compute the spatial depths of the mean vectors
Sdepth <- rep(0, M)
for (m in 1:M){
  target_mu <- means[m,]
  ind <- c(1:M);  ind <- ind[! ind %in% c(m)]
  s <- rep(0, d)
  for (k in ind){
    other_mu <- means[k,]
    norm2 <- sqrt(sum((other_mu-target_mu)^2))
    s <- s + (target_mu-other_mu)/norm2
  }
  v <- s/(M-1)
  Sdepth[m] <- 1- sqrt(sum(v^2))
}
save(Wdepth, Sdepth, file = "~/Documents/papers/Wasserstein_depth/Wdepth_vs_Sdepth_unif.RData")


# Case 4: double exponential with varying centers    
n=500;  M=500
set.seed(60)
means <- rep(0, M); samp <- matrix(0, M, n); Wdist <- matrix(0, M, M)
for (m in 1:M){
  means[m] <- rnorm(1, 0, 1)   #rbeta(1, 2,2) 
  samp[m,] <- (2*rbinom(n,1,0.5)-1)*rexp(n, rate = 1) + means[m]  
  if (m >1){
    for (k in 1:(m-1)){
      a <- samp[m,]; b <- samp[k,]
      Wdist[k,m] <- sqrt(sum((sort(a)-sort(b))^2)/n)
    }
  }
}
OT <- list()
for (m in 1:M){
  mtemp <- matrix(0, n, M);  a <- samp[m,]
  for (k in 1:M){
    b <- samp[k,]
    mtemp[,k] <- order(b)[order(order(a))]
  }
  OT[[m]] <- mtemp
}
Wdepth <- rep(0, M)
for (m in 1:M){
  ind <- c(1:M);  ind <- ind[! ind %in% c(m)]
  sorce <- samp[m,];  transmap <- OT[[m]]
  stotal <- 0
  for (i in 1:n){
    s <- 0
    for (k in ind){
      target <- samp[k,];  id <- transmap[i,k]
      ite <- (sorce[i] - target[id])/max(Wdist[k,m], Wdist[m,k])
      s <- s + ite
    }  
    s <- s/(M-1)
    L2norm <- sum(s^2)
    stotal <- stotal + L2norm
  }
  Wdepth[m] <- 1- sqrt(stotal/n)
}
# compute the spatial depths of the means
Sdepth <- rep(0, M)
for (m in 1:M){
  target_mu <- means[m]
  ind <- c(1:M);  ind <- ind[! ind %in% c(m)]
  s <- 0
  for (k in ind){
    other_mu <- means[k]
    norm2 <- abs(other_mu-target_mu)
    s <- s + (target_mu-other_mu)/norm2
  }
  v <- s/(M-1)
  Sdepth[m] <- 1- abs(v)
}
save(Wdepth, Sdepth, file = "~/Documents/papers/Wasserstein_depth/Wdepth_vs_Sdepth_doubleexp.RData")



load("~/Documents/papers/Wasserstein_depth/Wdepth_vs_Sdepth_identity_cov.RData")
W1 <- Wdepth; S1 <- Sdepth
load("~/Documents/papers/Wasserstein_depth/Wdepth_vs_Sdepth_nonidentity_cov.RData")
W2 <- Wdepth; S2 <- Sdepth
load("~/Documents/papers/Wasserstein_depth/Wdepth_vs_Sdepth_unif.RData")
W3 <- Wdepth; S3 <- Sdepth
load("~/Documents/papers/Wasserstein_depth/Wdepth_vs_Sdepth_doubleexp.RData")
W4 <- Wdepth; S4 <- Sdepth

layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
plot(S1, W1, cex.main=0.9, main = "Case 1",
     xlab = "spatial depth", ylab = "WSD", xlim=c(0, 0.8), ylim=c(0, 0.8),
     pch = 19, col = "orange", frame = FALSE)
plot(S2, W2, cex.main=0.9, main = "Case 2",
     xlab = "spatial depth", ylab = "WSD", xlim=c(0, 0.8), ylim=c(0, 0.8),
     pch = 19, col = "orange", frame = FALSE)
plot(S3, W3, cex.main=0.9, main = "Case 3",
     xlab = "spatial depth", ylab = "WSD", xlim=c(0, 0.8), ylim=c(0, 0.8),
     pch = 19, col = "orange", frame = FALSE)
plot(S4, W4, cex.main=0.9, main = "Case 4",
     xlab = "spatial depth", ylab = "WSD", xlim=c(0, 1), ylim=c(0, 1),
     pch = 19, col = "orange", frame = FALSE)


