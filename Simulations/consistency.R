library(MASS)
library(transport)

# Case 1
n=3000; rep <- 100;  M=1000
K <- 15; pots <- seq(0, 1, length.out=(K+1))[2:K]; pots <- sort(c(0.5, pots))
X <- matrix(0, K, n); Wdepth <- matrix(0, rep, K)
set.seed(1)
for (t in 1:rep){
  for (k in 1:K){ X[k,] <- rexp(n, rate = pots[k]) }
  samp <- matrix(0, M, n)
  for (m in 1:M){lams <- rbeta(1, 2, 2); samp[m,] <- rexp(n, rate = lams) }
  rm(k,m)
  #the kth element of OT, OT[[k]], is an n by M matrix. The mth column of this matrix records the map from X[k,] to samp[m,].
  #This map is the permutation of the elements in the target support. The 1-st,2-nd,3-rd,...,n-th elements correspond to the target[mth column]
  OT <- list();   Wdist <- matrix(0, K, M)
  for (k in 1:K){ 
    a <- X[k,]; map <- matrix(0, n, M)
    for (m in 1:M){ b <- samp[m,]; Wdist[k,m] <- sqrt(sum((sort(a)-sort(b))^2)/n); 
    map[,m] <- order(b)[order(order(a))]
    }
    OT[[k]] <- map
  }
  # compute the Wdepths
  for (k in 1:K){
    sorce <- X[k,]; transmap <- OT[[k]]
    stotal <- 0
    for (i in 1:n){
      s <- 0
      for (m in 1:M){
        target <- samp[m,];  id <- transmap[i,m]
        ite <- (sorce[i] - target[id])/Wdist[k,m]
        s <- s + ite
      }  
      L2norm <- sum((s/M)^2)
      stotal <- stotal + L2norm
    }
    Wdepth[t,k] <- 1- sqrt(stotal/n)
  }
}
save(Wdepth, file = "~/Documents/papers/Wasserstein_depth/consistency/consistency_case1_n3000_M1000.RData")


# Case 2
n=400; rep <- 100;  M <- 500
K <- 2; pots <- c(2,1); X <- matrix(0, K, n); Wdepth <- matrix(0, rep, K)
set.seed(20)
for (t in 1:rep){
  samp <- matrix(0, M, n)   
  for (m in 1:M){ shp <- rbinom(1,1,1/2)+1; samp[m,] <- rweibull(n, shp, scale = 1) }
  OT <- list();   Wdist <- matrix(0, K, M)
  #the kth element of OT, OT[[k]], is an n by M matrix. The mth column of this matrix records the map from X[k,] to samp[m,].
  #This map is the permutation of the elements in the target support. The 1-st,2-nd,3-rd,...,n-th elements correspond to the target[mth column]
  for (k in 1:K){ 
    X[k,] <- rweibull(n, pots[k], scale = 1) 
    a <- X[k,]; map <- matrix(0, n, M)
    for (m in 1:M){ b <- samp[m,]; Wdist[k,m] <- sqrt(sum((sort(a)-sort(b))^2)/n); 
    map[,m] <- order(b)[order(order(a))]
    }
    OT[[k]] <- map
  }
  rm(k,m)
  # compute the Wdepths
  for (k in 1:K){
    sorce <- X[k,]; transmap <- OT[[k]]
    stotal <- 0
    for (i in 1:n){
      s <- 0
      for (m in 1:M){
        target <- samp[m,];  id <- transmap[i,m]
        ite <- (sorce[i] - target[id])/Wdist[k,m]
        s <- s + ite
      }  
      L2norm <- sum((s/M)^2)
      stotal <- stotal + L2norm
    }
    Wdepth[t,k] <- 1- sqrt(stotal/n)
  }
}
save(Wdepth, file = "~/Documents/papers/Wasserstein_depth/consistency/consistency_case2_weibull_n400_M500.RData")


# Case 3
n=500; rep <- 100;  M <- 500 #theoretical value is (3-sqrt(2))/4
K <- 4; X <- array(0, dim=c(n,2,K)); Wdepth <- matrix(0, rep, K)
centers <- cbind(c(-1,0), c(1,0), c(0,-1), c(0,1)) 
set.seed(3)
for (t in 1:rep){
  for (k in 1:K){ X[,,k] <- mvrnorm(n, centers[,k], diag(2)) }
  samp <- array(0, dim = c(n,2,M))
  for (m in 1:M){ 
    id <- sample(c(1:4), 1, replace = TRUE); 
    samp[,,m] <- mvrnorm(n, centers[,id], diag(2)) 
  }
  Wdist <- matrix(0, K, M); OT <- list()
  for (k in 1:K){ 
    a <- pp(X[,,k]); ottem <- matrix(0, n, M)
    for (m in 1:M){ 
      b <- pp(samp[,,m]); Wdist[k,m] <- wasserstein(a,b,p=2, prob = TRUE) 
      ottem[,m] <- transport(a, b, p = 2, method = "auctionbf", fullreturn=FALSE, control = list(), threads=1)$to
    }
    OT[[k]] <- ottem
  }
  rm(k,m)
  # compute Wdepths
  for (k in 1:K){
    sorce <- X[,,k];  transmap <- OT[[k]]  #transmap is an n by M matrix
    stotal <- 0
    for (i in 1:n){
      s <- 0
      for (m in 1:M){
        target <- samp[,,m];  idth <- transmap[i,m]
        ite <- (sorce[i,] - target[idth,])/Wdist[k,m]
        s <- s + ite
      }  
      L2norm <- sum((s/M)^2)
      stotal <- stotal + L2norm
    }
    Wdepth[t,k] <- 1- sqrt(stotal/n)
  }
}
save(Wdepth, file = "~/Documents/papers/Wasserstein_depth/consistency/consistency_case3_n500_M500.RData")


# Case 4
n=1000; rep <- 100;  M <- 1000
K <- 11; pots <- seq(1,2, length.out=K)
X <- array(0, dim=c(n,2,K)); Wdepth <- matrix(0, rep, K)
set.seed(4)
for (t in 1:rep){
  for (k in 1:K){ X[,1,k] <- runif(n, min=0, max=pots[k]); X[,2,k] <- runif(n, min=0, max=pots[k]) }
  samp <- array(0, dim = c(n,2,M))
  for (m in 1:M){ u <- runif(1, min=1, max=2); samp[,1,m] <- runif(n, min=0, max=u); samp[,2,m] <- runif(n, min=0, max=u) }
  Wdist <- matrix(0, K, M); OT <- list()
  for (k in 1:K){ 
    a <- pp(X[,,k]); ottem <- matrix(0, n, M)
    for (m in 1:M){ 
      b <- pp(samp[,,m]); Wdist[k,m] <- wasserstein(a,b,p=2, prob = TRUE) 
      ottem[,m] <- transport(a, b, p = 2, method = "auctionbf", fullreturn=FALSE, control = list(), threads=1)$to
    }
    OT[[k]] <- ottem
  }
  # compute Wdepths
  for (k in 1:K){
    sorce <- X[,,k];  transmap <- OT[[k]]  #transmap is an n by M matrix
    stotal <- 0
    for (i in 1:n){
      s <- 0
      for (m in 1:M){
        target <- samp[,,m];  id <- transmap[i,m]
        ite <- (sorce[i,] - target[id,])/Wdist[k,m]
        s <- s + ite
      }  
      L2norm <- sum((s/M)^2)
      stotal <- stotal + L2norm
    }
    Wdepth[t,k] <- 1- sqrt(stotal/n)
  }
}
save(Wdepth, file = "~/Documents/papers/Wasserstein_depth/consistency/consistency_case4_n1000_M1000.RData")


