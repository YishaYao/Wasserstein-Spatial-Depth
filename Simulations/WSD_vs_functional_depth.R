library(MASS)
library(depthTools)
library(fda)       # Functional Data Analysis
library(fda.usc)   # Contains depth functions for functional data
library(transport)

# Function to compute RKHS mean embedding
mean_embedding <- function(X, sigma = 0.3) {
  m <- max(dim(X))
  K <- matrix(0, m, m)
  for (i in 1:m) {
    for (j in 1:m) {
      K[i,j] <- exp(-sum((X[i,] - X[j,])^2)/(2*sigma^2))
    }
  }
  mean_embed <- colMeans(K)  # Compute the mean embedding
  return(mean_embed)
}

n <- 100; M <- 200; no <- 4; d <- 3 # n is the number of distributions, M is the number of data points
samp <- array(0, dim = c(n+no, M, d))

set.seed(49)
for (i in 1:n){
  mu <- rnorm(d, 0, 1); sd <- runif(1, min=0.8, max=1)
  samp[i,,] <- mvrnorm(M, mu, sd^2*diag(d))
}
#for (m in 1:M){ samp[n+1,m,] <- rgamma(d, shape = 5, scale = 1) } #works for MBD
for (m in 1:M){ samp[n+1,m,] <- sample(c(-3.5, -2.5, 2.5, 3.5), d, replace = TRUE) }
for (m in 1:M){ samp[n+2,m,] <- rgamma(d, shape = 3, scale = 2) }
#for (m in 1:M){ samp[n+2,m,] <- runif(d, min = -5.5, max = 5.5) } works for MBD
for (m in 1:M){ samp[n+3,m,] <- rweibull(d, 2, scale = 1) + 3*sample(c(-1,1), d, replace = TRUE) } 
#for (m in 1:M){ for (j in 1:d){ samp[n+3,m,j] <- sample(3:4, 1, replace = TRUE)*sample(c(-1,1), 1, replace = TRUE) } }
Cova <- matrix(0, d, d);  for(i in 1:d){ for (j in 1:d){ Cova[i,j] <- 0.5^(abs(i-j)) } }
for (m in 1:M){ samp[n+4,m,] <- mvrnorm(1, c(-3,3,3), Cova) }

# conduct kernel mean embedding via Gaussian kernel
fembed <- matrix(0, n+no, M)
for (i in 1:(n+no)){ fembed[i,] <- mean_embedding(samp[i,,], sigma=0.3) }
# compute the modified band depth
f_MBD <- MBD(fembed,plotting=FALSE)$MBD
lob_mbd <- quantile(f_MBD, 0.03);  which(f_MBD <= lob_mbd)
# compute the functional spatial depth
fX <- fdata(fembed) # Convert data into functional data object (fdata)
f_FSD <- depth.FSD(fX,trim=0.1,draw=FALSE)$dep
lob_fsd <- quantile(f_FSD, 0.03);  which(f_FSD <= lob_fsd)
#f_Modal <- depth.mode(fX,trim=0.1,draw=FLASE)$dep
#lob_modal <- quantile(f_Modal, 0.03);  which(f_Modal <= lob_modal)


OT <- list()
mtemp <- matrix(0, M, (n+no)); a <- pp(samp[1,,]); mtemp[,1] <- c(1:M)
for (k in 2:(n+no)){
  b <- pp(samp[k,,])
  mtemp[,k] <- transport(a, b, p = 2, method = "auctionbf", fullreturn=FALSE, control = list(), threads=1)$to
}
OT[[1]] <- mtemp
for (i in 2:(n+no-1)){
  mtemp <- matrix(0, M, (n+no));  a <- pp(samp[i,,])
  for (k in 1:(i-1)){
    prev <- OT[[k]]
    mtemp[,k] <- order(prev[,i])
  }
  mtemp[,i] <- c(1:M)
  for (k in (i+1):(n+no)){
    b <- pp(samp[k,,])
    mtemp[,k] <- transport(a, b, p = 2, method = "auctionbf", fullreturn=FALSE, control = list(), threads=1)$to
  }
  OT[[i]] <- mtemp
}
mtemp <- matrix(0, M, (n+no)); mtemp[,(n+no)] <- c(1:M)
for (k in 1:(n+no-1)){
  prev <- OT[[k]]
  mtemp[,k] <- order(prev[,(n+no)])
}
OT[[n+no]] <- mtemp
rm(i,k)

WdistX <- matrix(0, (n+no), (n+no))
for (i in 2:(n+no)){
  src <- samp[i,,];  OTmap <- OT[[i]]
  for (l in 1:(i-1)){
    target <- samp[l,,];  ind <- OTmap[,l]; 
    v <- src - target[ind,]; WdistX[l,i] <- sqrt(sum(v^2)/M)
  }
}
WdepthX <- rep(0, (n+no))
for (i in 1:(n+no)){
  ind <- c(1:(n+no));  ind <- ind[! ind %in% c(i)]
  srce <- samp[i,,];  transmap <- OT[[i]]
  stotal <- 0
  for (m in 1:M){
    s <- 0
    for (k in ind){
      target <- samp[k,,];  id <- transmap[m, k]
      itemk <- (srce[m,] - target[id,])/max(WdistX[i,k], WdistX[k,i])
      s <- s + itemk
    }
    s <- s/(n+no-1)
    L2norm <- sum(s^2)
    stotal <- stotal + L2norm
  }
  WdepthX[i] <- 1- sqrt(stotal/M)
}
lob_w <- quantile(WdepthX, 0.03);  which(WdepthX <= lob_w)


library(scatterplot3d)
Xm <- matrix(0,0,3)
for (i in 1:n){ Xm <- rbind(Xm, samp[i,,]) }
Ym <- matrix(0,0,3)
for (i in (n+1):(n+no)){ Ym <- rbind(Ym, samp[i,,]) }

Xms <- Xm[sample(1:(M*n), 8*M, replace = FALSE),]
plotdata <- data.frame(rbind(Xms, Ym))
colnames(plotdata) <- c("x", "y", "z")
grp = factor(c(rep(1, 8*M), rep(2, 4*M)));  plotdata$group <- grp
colors <- c("#66BD63","darkorange") #"aquamarine3"
group_colors <- colors[as.numeric(plotdata$group)]
xmin <- min(plotdata$x);xmax <- max(plotdata$x);  ymin <- min(plotdata$y);ymax <- max(plotdata$y);  zmin <- min(plotdata$z);  zmax <- max(plotdata$z); 

df1 <- cbind(WdepthX, c(rep(1, n), rep(2, no)))
df1 <- df1[sample(nrow(df1)),]
df1 <- as.data.frame(cbind(c(1:(n+no)), df1))

df2 <- cbind(as.vector(f_MBD), c(rep(1, n), rep(2, no)))
df2 <- df2[sample(nrow(df2)),]
df2 <- as.data.frame(cbind(c(1:(n+no)), df2))

df3 <- cbind(as.vector(f_FSD), c(rep(1, n), rep(2, no)))
df3 <- df3[sample(nrow(df3)),]
df3 <- as.data.frame(cbind(c(1:(n+no)), df3))

colv1 <- colors[as.numeric(df1$V3)]; colv2 <- colors[as.numeric(df2$V3)]; colv3 <- colors[as.numeric(df3$V3)]

#layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
layout(matrix(c(1,2,1,2,3,4, 3, 4), 4, 2, byrow = TRUE))
par(mar = c(3, 5, 3, 2))
#par(mfrow = c(2, 2), mar = c(3, 7, 2, 3))
plot.new()
plot(df1$V1, df1$WdepthX, pch=16, col=colv1, xlab = "", xaxt="n", ylab = "WSD", ylim = c(0, 0.9))
plot(df2$V1, df2$V2, pch=16, col=colv2, xlab = "", xaxt="n", ylab = "MBD", ylim = c(0, 0.6))
plot(df3$V1, df3$V2, pch=16, col=colv3, xlab = "", xaxt="n", ylab = "FSD", ylim = c(0, 0.6))

par(mfrow = c(1, 1))
scatterplot3d(plotdata$x, plotdata$y, plotdata$z, 
              color = group_colors, pch = 17, cex.symbols = 0.5,
              #main = "3D Data Clouds",
              xlim = c(floor(xmin), ceiling(xmax)), ylim = c(floor(ymin), ceiling(ymax)), zlim = c(floor(zmin), ceiling(zmax)),
              xlab = "", ylab = "", zlab = "")

