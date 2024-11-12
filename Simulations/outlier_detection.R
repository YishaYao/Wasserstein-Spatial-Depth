#Draw n sample points from each of the M Gaussian distributions. 
#each multivariate Gaussian distribution has dimension d.
#there are 6 outlier distributions
library(MASS)
library(transport)

n=500;  M=500;  d=10;  no= 6 # no is the number of outliers

# Case 1  each Gaussian distribution has mean vector from (normal(0,1)^d and identity covariance 
set.seed(47)
means <- matrix(0, M, d)
samp <- array(0, dim = c(n, d, M+no))
Wdist <- matrix(0, M+no, M+no)
for (m in 1:M){
  mu <- rnorm(d, 0, 1); means[m,] <- mu
  samp[,,m] <- mvrnorm(n, mu, diag(d))
  if (m >1){
    for (i in 1:(m-1)){
      a <- pp(samp[,,m])
      b <- pp(samp[,,i])
      Wdist[i,m] <- wasserstein(a,b,p=2, prob = TRUE)
    }
  }
}
# outlier distributions
samp[,, M+1] <- mvrnorm(n, rep(5, d), diag(d))
Cova <- matrix(0, d, d);  for(i in 1:d){ for (j in 1:d){ Cova[i,j] <- 0.5^(abs(i-j)) } }; samp[,, M+2] <- mvrnorm(n, rep(5, d), Cova)
for (i in 1:n){ samp[i,, M+3] <- rgamma(d, shape = 3, scale = 2) }
for (i in 1:n){ samp[i,, M+4] <- rmultinom(1, 2*d, prob = c(0.25, 0.25, 0.15, 0.15, 0.15, 0.01, 0.01, 0.01, 0.01, 0.01)) }
for (i in 1:n){ samp[i,, M+5] <- runif(d, min = -6, max = 6) }
for (i in 1:n){ samp[i,, M+6] <- rpois(d, lambda = 6)  }
#for (i in 1:n){ samp[i,, M+6] <- rexp(d,2) }
#for (j in 1:n){ samp[i,, M+6] <- rbeta(d, 0.1, 0.1)  } 
#for (i in 1:n){ samp[i,, M+3] <- rbinom(d, 1, 0.5) }
rm(m,i,j)

for (m in (M+1):(M+no)){
  for (i in 1:(m-1)){
    a <- pp(samp[,,m])
    b <- pp(samp[,,i])
    Wdist[i,m] <- wasserstein(a,b,p=2, prob = TRUE)
  }
}
#here run the COMMON CODE 
save(Wdist, OT, Wdepth, file = "~/Documents/papers/Wasserstein_depth/outlier_detection_1.RData")


# Case 2  each distribution is d random variables iid uniform[0,up]
set.seed(68)
samp <- array(0, dim = c(n, d, M+no))
Wdist <- matrix(0, M+no, M+no)
for (m in 1:M){
  up <- runif(1, min = 1, max = 2)
  for (i in 1:n){ samp[i,,m] <- runif(d, min =0, max = up) }
  if (m >1){
    for (k in 1:(m-1)){
      a <- pp(samp[,,m])
      b <- pp(samp[,,k])
      Wdist[k,m] <- wasserstein(a,b,p=2, prob = TRUE)
    }
  }
}
# outlier distributions
samp[,, M+1] <- mvrnorm(n, rep(3, d), diag(d))
Cova <- matrix(0, d, d);  for(i in 1:d){ for (j in 1:d){ Cova[i,j] <- 0.5^(abs(i-j)) } }; samp[,, M+2] <- mvrnorm(n, rep(-1, d), Cova)
for (i in 1:n){ samp[i,, M+3] <- rpois(d, lambda = 3) }
for (i in 1:n){ samp[i,, M+4] <- rbinom(d, 1, 0.2) }
for (i in 1:n){ samp[i,, M+5] <- rmultinom(1, 2*d, prob = c(0.25, 0.15, 0.1, 0.1, 0.15, 0.05, 0.05, 0.05, 0.05, 0.05)) }
for (i in 1:n){ samp[i,, M+6] <- rchisq(d, df = 10) }

for (m in (M+1):(M+no)){
  for (k in 1:(m-1)){
    a <- pp(samp[,,m])
    b <- pp(samp[,,k])
    Wdist[k,m] <- wasserstein(a,b,p=2, prob = TRUE)
  }
}
#here run the COMMON CODE
save(Wdist, OT, Wdepth, file = "~/Documents/papers/Wasserstein_depth/outlier_detection_2.RData")


# below is the COMMON CODE for Case 1 and Case 2.
OT <- list()
mtemp <- matrix(0, n, M+no-1); a <- pp(samp[,,1])
for (k in 2:(M+no)){
  b <- pp(samp[,,k])
  mtemp[,(k-1)] <- transport(a, b, p = 2, method = "auctionbf", fullreturn=FALSE, control = list(), threads=1)$to
}
OT[[1]] <- mtemp
for (m in 2:(M+no-1)){
  mtemp <- matrix(0, n, M+no-1);  a <- pp(samp[,,m])
  for (k in 1:(m-1)){
    b <- pp(samp[,,k])
    mtemp[,k] <- transport(a, b, p = 2, method = "auctionbf", fullreturn=FALSE, control = list(), threads=1)$to
  }
  for (k in (m+1):(M+no)){
    b <- pp(samp[,,k])
    mtemp[,(k-1)] <- transport(a, b, p = 2, method = "auctionbf", fullreturn=FALSE, control = list(), threads=1)$to
  }
  OT[[m]] <- mtemp
}
mtemp <- matrix(0, n, M+no-1);  a <- pp(samp[,,M+no])
for (k in 1:(M+no-1)){
  b <- pp(samp[,,k])
  mtemp[,k] <- transport(a, b, p = 2, method = "auctionbf", fullreturn=FALSE, control = list(), threads=1)$to
}
OT[[M+no]] <- mtemp

Wdepth <- rep(0, M+no)
for (m in 1:(M+no)){
  ind <- c(1:(M+no));  ind <- ind[! ind %in% c(m)]
  sorce <- samp[,,m];  transmap <- OT[[m]]
  stotal <- 0
  for (i in 1:n){
    s <- 0
    for (k in ind){
      target <- samp[,,k];  cind <- k-as.integer(k>m);  target_indeces <- transmap[, cind]; id <- target_indeces[i]
      itemk <- (sorce[i,] - target[id,])/max(Wdist[m,k], Wdist[k,m])
      s <- s + itemk
    }
    s <- s/(M+no-1)
    L2norm <- sum(s^2)
    stotal <- stotal + L2norm
  }
  Wdepth[m] <- 1- sqrt(stotal/n)
}


# below is the code for the plots
load("~/Documents/papers/Wasserstein_depth/outlier_detection_1.RData")
lower_bound <- quantile(Wdepth, 0.01);  which(Wdepth <= lower_bound)
df1 <- cbind(Wdepth, c(rep(1, M), rep(2, no)))
df1 <- df1[sample(nrow(df1)),]
df1 <- as.data.frame(cbind(c(1:(M+no)), df1))

load("~/Documents/papers/Wasserstein_depth/outlier_detection_2.RData")
lower_bound <- quantile(Wdepth, 0.01);  which(Wdepth <= lower_bound)
df2 <- cbind(Wdepth, c(rep(1, M), rep(2, no)))
df2 <- df2[sample(nrow(df2)),]
df2 <- as.data.frame(cbind(c(1:(M+no)), df2))

layout(matrix(c(1,2), 1, 2, byrow = TRUE))
plot(df1$V1, df1$Wdepth, pch=16, col=df1$V3, xlab = "", xaxt="n", ylab = "Wasserstein depth", ylim = c(0, 1), main="Case 1")
plot(df2$V1, df2$Wdepth, pch=16, col=df2$V3, xlab = "", xaxt="n", ylab = "Wasserstein depth", ylim = c(0, 1), main="Case 2")

