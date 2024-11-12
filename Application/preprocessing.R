AREA1 <- list()

#start of the common code
dfb <- dfa[!(dfa$Q_TG == 9), c(2:4)]
year <- substr(dfb$DATE, 0, 4)
month <- substr(dfb$DATE, 5, 6)
df1 <- data.frame(year, month, dfb$TG, dfb$Q_TG)
colnames(df1) <- c("year", "month", "TG", "Q_TG")

m_temp <- aggregate(x= df1$TG, by = list(df1$year, df1$month), FUN = mean)
colnames(m_temp) <- c("year", "month", "avg_temp")
m_temp$year <- as.numeric(m_temp$year); m_temp$month <- as.numeric(m_temp$month)
m_temp <- na.omit(m_temp)
m_temp <- m_temp[order(m_temp$year),]

library(reshape2)
m_temp$year <- as.factor(m_temp$year); m_temp$month <- as.factor(m_temp$month)
distr <- reshape2::acast(m_temp, year ~ month, value.var="avg_temp")
distr <- na.omit(distr)
samp <- as.matrix(distr)
# end of common code

dfa <- read.table("1_CZECH REPUBLIC_PRAHA.txt", header = TRUE, sep = ",", dec = ".")
#common code
AREA1[[1]] <- samp

dfa <- read.table("2_SWEDEN_LANDSORT.txt", header = TRUE, sep = ",", dec = ".")
#common code
AREA1[[2]] <- samp

dfa <- read.table("3_SWEDEN_STOCKHOLM.txt",  header = TRUE, sep = ",", dec = ".")
#common code
AREA1[[3]] <- samp

dfa <- read.table("4_AUSTRIA_SALZBURG.txt",  header = TRUE, sep = ",", dec = ".")
#common code
AREA1[[4]] <- samp

dfa <- read.table("6_AUSTRIA_INNSBRUCK-UNIV.txt",  header = TRUE, sep = ",", dec = ".")
#common code
AREA1[[6]] <- samp

dfa <- read.table("22_CROATIA_GOSPIC.txt",  header = TRUE, sep = ",", dec = ".")
# common code 
AREA1[[22]] <- samp

dfa <- read.table("26_CROATIA_ZAGREB-GRIC.txt",  header = TRUE, sep = ",", dec = ".")
# common code 
AREA1[[26]] <- samp

dfa <- read.table("32_FINLAND_TORNIO LIAKKA.txt",  header = TRUE, sep = ",", dec = ".")
# common code 
AREA1[[32]] <- samp

dfa <- read.table("34_FINLAND_TORNIO_TORPPI.txt",  header = TRUE, sep = ",", dec = ".")
# common code 
AREA1[[34]] <- samp


dfa <- read.table("38_UK_OXFORD.txt", header = TRUE, sep = ",", dec = ".")
#common code
AREA1[[38]] <- samp

dfa <- read.table("39_GERMANY_HALLE.txt", header = TRUE, sep = ",", dec = ".")
#common code
AREA1[[39]] <- samp

dfa <- read.table("40_UK_DURHAM.txt", header = TRUE, sep = ",", dec = ".")
#common code
AREA1[[40]] <- samp

save(AREA1, file = "~/Documents/papers/Wasserstein_depth/REAL_DATA/area1_alphebat_order/area1_original.RData")

load("~/Documents/papers/Wasserstein_depth/REAL_DATA/area1_alphebat_order/area1_original.RData")
M1 <- AREA1[[1]]; M2 <- AREA1[[2]]; M3 <- AREA1[[3]]; M4 <- AREA1[[4]]; M5 <- AREA1[[5]]; M6 <- AREA1[[6]]; 
M7 <- AREA1[[7]]; M8 <- AREA1[[8]]; M9 <- AREA1[[9]]; M10 <- AREA1[[10]]; M11 <- AREA1[[11]]; M12 <- AREA1[[12]];
M13 <- AREA1[[13]]; M14 <- AREA1[[14]]; M15 <- AREA1[[15]]; M16 <- AREA1[[16]]; M17 <- AREA1[[17]]; M18 <- AREA1[[18]];
M19 <- AREA1[[19]]; M20 <- AREA1[[20]]; M21 <- AREA1[[21]]; M22 <- AREA1[[22]]; M23 <- AREA1[[23]]; M24 <- AREA1[[24]];
M25 <- AREA1[[25]]; M26 <- AREA1[[26]]; M27 <- AREA1[[27]]; M28 <- AREA1[[28]]; M29 <- AREA1[[29]]; M30 <- AREA1[[30]];
M31 <- AREA1[[31]]; M32 <- AREA1[[32]]; M33 <- AREA1[[33]]; M34 <- AREA1[[34]]; M35 <- AREA1[[35]]; M36 <- AREA1[[36]];
M37 <- AREA1[[37]]; M38 <- AREA1[[38]]; M39 <- AREA1[[39]]; M40 <- AREA1[[40]]

# check the missing years of M1 to M40
judge <- as.factor(c(1874:2023)) %in% rownames(M1);  c(1874:2023)[which(judge == "FALSE")]
# M1 is missing 2016, 2019
v2 = 0.4*M1[which(rownames(M1)=="2020"),] + 0.3*M1[which(rownames(M1)=="2021"),] + 0.2*M1[which(rownames(M1)=="2022"),] + 0.1*M1[which(rownames(M1)=="2023"),]
v1 = 0.4*M1[which(rownames(M1)=="2017"),] + 0.3*M1[which(rownames(M1)=="2018"),] + 0.2*M1[which(rownames(M1)=="2020"),] + 0.1*M1[which(rownames(M1)=="2021"),]
m1 <- M1[which(rownames(M1) %in% as.factor(c(1874:2023))),];  which(rownames(m1)=="2015")
m1 <- rbind(m1[1:142,], v1, m1[143:144,], v2, m1[145:148,] ); rownames(m1) <- as.factor(c(1874:2023))
AREA1[[1]] <- m1

judge <- as.factor(c(1874:2023)) %in% rownames(M2);  c(1874:2023)[which(judge == "FALSE")]
# M2 is missing 1879, 1889
v1 = 0.4*M2[which(rownames(M2)=="1878"),] + 0.3*M2[which(rownames(M2)=="1877"),] + 0.2*M2[which(rownames(M2)=="1876"),] + 0.1*M2[which(rownames(M2)=="1875"),]
v2 = 0.4*M2[which(rownames(M2)=="1888"),] + 0.3*M2[which(rownames(M2)=="1887"),] + 0.2*M2[which(rownames(M2)=="1886"),] + 0.1*M2[which(rownames(M2)=="1885"),]
m2 <- M2[which(rownames(M2) %in% as.factor(c(1874:2023))),];  which(rownames(m2)=="1878"); which(rownames(m2)=="1888")
m2 <- rbind(m2[1:5,], v1, m2[6:14,], v2, m2[15:148,]); rownames(m2) <- as.factor(c(1874:2023))
AREA1[[2]] <- m2

judge <- as.factor(c(1874:2023)) %in% rownames(M3);  c(1874:2023)[which(judge == "FALSE")]
# no missing
m3 <- M3[which(rownames(M3) %in% as.factor(c(1874:2023))),]
AREA1[[3]] <- m3

judge <- as.factor(c(1874:2023)) %in% rownames(M4);  c(1874:2023)[which(judge == "FALSE")]
# M4 is missing 1921/1922/1945/1946
v1 = 0.4*M4[which(rownames(M4)=="1920"),] + 0.3*M4[which(rownames(M4)=="1919"),] + 0.2*M4[which(rownames(M4)=="1918"),] + 0.1*M4[which(rownames(M4)=="1917"),]
v2 = 0.4*v1 + 0.3*M4[which(rownames(M4)=="1920"),] + 0.2*M4[which(rownames(M4)=="1919"),] + 0.1*M4[which(rownames(M4)=="1918"),]
v4 = 0.4*M4[which(rownames(M4)=="1947"),] + 0.3*M4[which(rownames(M4)=="1948"),] + 0.2*M4[which(rownames(M4)=="1949"),] + 0.1*M4[which(rownames(M4)=="1950"),]
v3 = 0.4*v4 + 0.3*M4[which(rownames(M4)=="1947"),] + 0.2*M4[which(rownames(M4)=="1948"),] + 0.1*M4[which(rownames(M4)=="1949"),]
m4 <- M4[which(rownames(M4) %in% as.factor(c(1874:2023))),];  which(rownames(m4)=="1920"); which(rownames(m4)=="1944")
m4 <- rbind(m4[1:47,], v1, v2, m4[48:69,], v3, v4, m4[70:146,]); rownames(m4) <- as.factor(c(1874:2023))
AREA1[[4]] <- m4


judge <- as.factor(c(1874:2023)) %in% rownames(M6);  c(1874:2023)[which(judge == "FALSE")]
# M6 is missing 1874 1875 1876 1879 1889 1890 1891 1892 1893 1903 1904 1907
#v4 <- 0.4*M6[which(rownames(M6)=="1920"),] + 0.3*M6[which(rownames(M6)=="1921"),] + 0.2*M6[which(rownames(M6)=="1922"),] + 0.1*M6[which(rownames(M6)=="1923"),]
#v3 <- 0.4*M6[which(rownames(M6)=="1977"),] + 0.3*M6[which(rownames(M6)=="1978"),] + 0.2*v4 + 0.1*M6[which(rownames(M6)=="1920"),]
#v2 <- 0.4*v3 + 0.3*M6[which(rownames(M6)=="1977"),] + 0.2*M6[which(rownames(M6)=="1978"),] + 0.1*v4
#v1 <- 0.4*v2 + 0.3*v3 + 0.2*M6[which(rownames(M6)=="1977"),] + 0.1*M6[which(rownames(M6)=="1978"),]
#D <- (M8[which(rownames(M8) %in% c("1889", "1890", "1891", "1892", "1893", "1903", "1904", "1907")),] + M9[which(rownames(M9) %in% c("1889", "1890", "1891", "1892", "1893", "1903", "1904", "1907")),]
#      +M10[which(rownames(M10) %in% c("1889", "1890", "1891", "1892", "1893", "1903", "1904", "1907")),] + M11[which(rownames(M11) %in% c("1889", "1890", "1891", "1892", "1893", "1903", "1904", "1907")),]
#      +M12[which(rownames(M12) %in% c("1889", "1890", "1891", "1892", "1893", "1903", "1904", "1907")),] + M13[which(rownames(M13) %in% c("1889", "1890", "1891", "1892", "1893", "1903", "1904", "1907")),])
#D <- D/6; m6 <- M6[which(rownames(M6) %in% as.factor(c(1874:2023))),];  which(rownames(m6)=="1880"); which(rownames(m6)=="1888")
#which(rownames(m6)=="1894"); which(rownames(m6)=="1902")
#m6 <- rbind(v1, v2, v3, m6[1:2,], v4, m6[3:11,], D[1:5,], m6[12:20,], D[6:7,], m6[21:22,], D[8,], m6[23:138,] )
#rownames(m6) <- as.factor(c(1874:2023));  AREA1[[6]] <- m6
mis_ind <- which(judge == "FALSE")
D <- m4[mis_ind,]
nois <- matrix(0, length(mis_ind), 12);  for (i in 1:length(mis_ind)){ nois[i,] <- rnorm(12, 0, 1) }
D <- D + nois
m6 <- M6[which(rownames(M6) %in% as.factor(c(1874:2023))),] 
m6 <- rbind(m6, D);  m6 <- m6[order(as.numeric(rownames(m6))),]
AREA1[[6]] <- m6


# M5 and M7 only miss 1874, add them by averaging 1874's in 
v <- (M5[which(rownames(M5)=="1875"),] + M7[which(rownames(M7)=="1875"),])/2
v5 <- v+rnorm(n = 12, mean = 0, sd = 1); v7 <-v+rnorm(n = 12, mean = 0, sd = 1);
m5 <- rbind(v5, M5); rownames(m5) <- as.factor(c(1874:2023)); AREA1[[5]] <- m5
m7 <- rbind(v7, M7); rownames(m7) <- as.factor(c(1874:2023)); AREA1[[7]] <- m7

#M8 is complete
m8 <- M8[which(rownames(M8) %in% as.factor(c(1874:2023))),]
AREA1[[8]] <- m8

judge <- as.factor(c(1874:2023)) %in% rownames(M9);  c(1874:2023)[which(judge == "FALSE")]
# M9 is missing 1875 1876 1877 1878 1922 1945
m9 <- M9[which(rownames(M9) %in% as.factor(c(1874:2023))),]
v4 = 0.4*m9[which(rownames(m9)=="1879"),] + 0.3*m9[which(rownames(m9)=="1880"),] + 0.2*m9[which(rownames(m9)=="1881"),] + 0.1*m9[which(rownames(m9)=="1882"),]
v3 = 0.4*v4 + 0.3*m9[which(rownames(m9)=="1874"),] + 0.2*m9[which(rownames(m9)=="1879"),] + 0.1*m9[which(rownames(m9)=="1880"),]
v2 = 0.4*v3 + 0.3*m9[which(rownames(m9)=="1874"),] + 0.2*v4 + 0.1*m9[which(rownames(m9)=="1879"),]
v1 = 0.4*m9[which(rownames(m9)=="1874"),] + 0.3*v2 + 0.2*v3 + 0.1*v4
v5 <- 0.4*m9[which(rownames(m9)=="1921"),] + 0.3*m9[which(rownames(m9)=="1920"),] + 0.2*m9[which(rownames(m9)=="1919"),] + 0.1*m9[which(rownames(m9)=="1918"),]
v6 <- 0.4*m9[which(rownames(m9)=="1946"),] + 0.3*m9[which(rownames(m9)=="1947"),] + 0.2*m9[which(rownames(m9)=="1948"),] + 0.1*m9[which(rownames(m9)=="1949"),]
which(rownames(m9)=="1879"); which(rownames(m9)=="1921"); which(rownames(m9)=="1944");
m9 <- rbind(m9[1,], v1, v2, v3, v4, m9[2:44,], v5, m9[45:66,], v6, m9[67:144,]);  rownames(m9) <- as.factor(c(1874:2023)); AREA1[[9]] <- m9


judge <- as.factor(c(1874:2023)) %in% rownames(M10);  c(1874:2023)[which(judge == "FALSE")]
# M10 is missing 1874 1875 1944 1945
v2 = 0.4*M10[which(rownames(M10)=="1876"),] + 0.3*M10[which(rownames(M10)=="1877"),] + 0.2*M10[which(rownames(M10)=="1878"),] + 0.1*M10[which(rownames(M10)=="1879"),]
v1 = 0.4*v2 + 0.3*M10[which(rownames(M10)=="1876"),] + 0.2*M10[which(rownames(M10)=="1877"),] + 0.1*M10[which(rownames(M10)=="1878"),]
v3 = 0.4*M10[which(rownames(M10)=="1943"),] + 0.3*M10[which(rownames(M10)=="1942"),] + 0.2*M10[which(rownames(M10)=="1941"),] + 0.1*M10[which(rownames(M10)=="1940"),]
v4 = 0.4*M10[which(rownames(M10)=="1946"),] + 0.3*M10[which(rownames(M10)=="1947"),] + 0.2*M10[which(rownames(M10)=="1948"),] + 0.1*M10[which(rownames(M10)=="1949"),]
m10 <- M10[which(rownames(M10) %in% as.factor(c(1874:2023))),];  which(rownames(m10) %in% c("1943", "1946"))
m10 <- rbind(v1, v2, m10[1:68,], v3, v4, m10[69:146,]); rownames(m10) <- as.factor(c(1874:2023)); AREA1[[10]] <- m10

judge <- as.factor(c(1874:2023)) %in% rownames(M11);  c(1874:2023)[which(judge == "FALSE")]
# M11 is missing 1874 1875 1945
v2 = 0.4*M11[which(rownames(M11)=="1876"),] + 0.3*M11[which(rownames(M11)=="1877"),] + 0.2*M11[which(rownames(M11)=="1878"),] + 0.1*M11[which(rownames(M11)=="1879"),]
v1 = 0.4*v2 + 0.3*M11[which(rownames(M11)=="1876"),] + 0.2*M11[which(rownames(M11)=="1877"),] + 0.1*M11[which(rownames(M11)=="1878"),]
v3 = 0.4*M11[which(rownames(M11)=="1946"),] + 0.3*M11[which(rownames(M11)=="1947"),] + 0.2*M11[which(rownames(M11)=="1948"),] + 0.1*M11[which(rownames(M11)=="1949"),]
m11 <- M11[which(rownames(M11) %in% as.factor(c(1874:2023))),];  which(rownames(m11) %in% c("1944", "1946"))
m11 <- rbind(v1,v2,m11[1:69,], v3, m11[70:147,]); rownames(m11) <- as.factor(c(1874:2023)); AREA1[[11]] <- m11

judge <- as.factor(c(1874:2023)) %in% rownames(M13);  c(1874:2023)[which(judge == "FALSE")]
# M13 is missing 1874 1875 1945
v2 = 0.4*M13[which(rownames(M13)=="1876"),] + 0.3*M13[which(rownames(M13)=="1877"),] + 0.2*M13[which(rownames(M13)=="1878"),] + 0.1*M13[which(rownames(M13)=="1879"),]
v1 = 0.4*v2 + 0.3*M13[which(rownames(M13)=="1876"),] + 0.2*M13[which(rownames(M13)=="1877"),] + 0.1*M13[which(rownames(M13)=="1878"),]
v3 = 0.4*M13[which(rownames(M13)=="1946"),] + 0.3*M13[which(rownames(M13)=="1947"),] + 0.2*M13[which(rownames(M13)=="1948"),] + 0.1*M13[which(rownames(M13)=="1949"),] 
m13 <- M13[which(rownames(M13) %in% as.factor(c(1874:2023))),];  which(rownames(m13) %in% c("1944", "1946"))
m13 <- rbind(v1,v2,m13[1:69,], v3, m13[70:147,]); rownames(m13) <- as.factor(c(1874:2023)); AREA1[[13]] <- m13

judge <- as.factor(c(1874:2023)) %in% rownames(M14);  c(1874:2023)[which(judge == "FALSE")]
# M14 is missing 1874 1875 1945
v2 = 0.4*M14[which(rownames(M14)=="1876"),] + 0.3*M14[which(rownames(M14)=="1877"),] + 0.2*M14[which(rownames(M14)=="1878"),] + 0.1*M14[which(rownames(M14)=="1879"),]
v1 = 0.4*v2 + 0.3*M14[which(rownames(M14)=="1876"),] + 0.2*M14[which(rownames(M14)=="1877"),] + 0.1*M14[which(rownames(M14)=="1878"),]
v3 = 0.4*M14[which(rownames(M14)=="1946"),] + 0.3*M14[which(rownames(M14)=="1947"),] + 0.2*M14[which(rownames(M14)=="1948"),] + 0.1*M14[which(rownames(M14)=="1949"),] 
m14 <- M14[which(rownames(M14) %in% as.factor(c(1874:2023))),];  which(rownames(m14) %in% c("1944", "1946"))
m14 <- rbind(v1,v2,m14[1:69,], v3, m14[70:147,]); rownames(m14) <- as.factor(c(1874:2023)); AREA1[[14]] <- m14

judge <- as.factor(c(1874:2023)) %in% rownames(M15);  c(1874:2023)[which(judge == "FALSE")]
# M15 is missing 1874 1875 1945
v2 = 0.4*M15[which(rownames(M15)=="1876"),] + 0.3*M15[which(rownames(M15)=="1877"),] + 0.2*M15[which(rownames(M15)=="1878"),] + 0.1*M15[which(rownames(M15)=="1879"),]
v1 = 0.4*v2 + 0.3*M15[which(rownames(M15)=="1876"),] + 0.2*M15[which(rownames(M15)=="1877"),] + 0.1*M15[which(rownames(M15)=="1878"),]
v3 = 0.4*M15[which(rownames(M15)=="1946"),] + 0.3*M15[which(rownames(M15)=="1947"),] + 0.2*M15[which(rownames(M15)=="1948"),] + 0.1*M15[which(rownames(M15)=="1949"),] 
m15 <- M15[which(rownames(M15) %in% as.factor(c(1874:2023))),];  which(rownames(m15) %in% c("1944", "1946"))
m15 <- rbind(v1,v2,m15[1:69,], v3, m15[70:147,]); rownames(m15) <- as.factor(c(1874:2023)); AREA1[[15]] <- m15

judge <- as.factor(c(1874:2023)) %in% rownames(M16);  c(1874:2023)[which(judge == "FALSE")]
# M16 is missing 1874 1875 1945
v2 = 0.4*M16[which(rownames(M16)=="1876"),] + 0.3*M16[which(rownames(M16)=="1877"),] + 0.2*M16[which(rownames(M16)=="1878"),] + 0.1*M16[which(rownames(M16)=="1879"),]
v1 = 0.4*v2 + 0.3*M16[which(rownames(M16)=="1876"),] + 0.2*M16[which(rownames(M16)=="1877"),] + 0.1*M16[which(rownames(M16)=="1878"),]
v3 = 0.4*M16[which(rownames(M16)=="1946"),] + 0.3*M16[which(rownames(M16)=="1947"),] + 0.2*M16[which(rownames(M16)=="1948"),] + 0.1*M16[which(rownames(M16)=="1949"),] 
m16 <- M16[which(rownames(M16) %in% as.factor(c(1874:2023))),];  which(rownames(m16) %in% c("1944", "1946"))
m16 <- rbind(v1,v2,m16[1:69,], v3, m16[70:147,]); rownames(m16) <- as.factor(c(1874:2023)); AREA1[[16]] <- m16

judge <- as.factor(c(1874:2023)) %in% rownames(M17);  c(1874:2023)[which(judge == "FALSE")]
# M17 is missing 1874 1875 1945
v2 = 0.4*M17[which(rownames(M17)=="1876"),] + 0.3*M17[which(rownames(M17)=="1877"),] + 0.2*M17[which(rownames(M17)=="1878"),] + 0.1*M17[which(rownames(M17)=="1879"),]
v1 = 0.4*v2 + 0.3*M17[which(rownames(M17)=="1876"),] + 0.2*M17[which(rownames(M17)=="1877"),] + 0.1*M17[which(rownames(M17)=="1878"),]
v3 = 0.4*M17[which(rownames(M17)=="1946"),] + 0.3*M17[which(rownames(M17)=="1947"),] + 0.2*M17[which(rownames(M17)=="1948"),] + 0.1*M17[which(rownames(M17)=="1949"),] 
m17 <- M17[which(rownames(M17) %in% as.factor(c(1874:2023))),];  which(rownames(m17) %in% c("1944", "1946"))
m17 <- rbind(v1,v2,m17[1:69,], v3, m17[70:147,]); rownames(m17) <- as.factor(c(1874:2023)); AREA1[[17]] <- m17

judge <- as.factor(c(1874:2023)) %in% rownames(M18);  c(1874:2023)[which(judge == "FALSE")]
# M18 is missing 1874 1875 1945
v2 = 0.4*M18[which(rownames(M18)=="1876"),] + 0.3*M18[which(rownames(M18)=="1877"),] + 0.2*M18[which(rownames(M18)=="1878"),] + 0.1*M18[which(rownames(M18)=="1879"),]
v1 = 0.4*v2 + 0.3*M18[which(rownames(M18)=="1876"),] + 0.2*M18[which(rownames(M18)=="1877"),] + 0.1*M18[which(rownames(M18)=="1878"),]
v3 = 0.4*M18[which(rownames(M18)=="1946"),] + 0.3*M18[which(rownames(M18)=="1947"),] + 0.2*M18[which(rownames(M18)=="1948"),] + 0.1*M18[which(rownames(M18)=="1949"),] 
m18 <- M18[which(rownames(M18) %in% as.factor(c(1874:2023))),];  which(rownames(m18) %in% c("1944", "1946"))
m18 <- rbind(v1,v2,m18[1:69,], v3, m18[70:147,]); rownames(m18) <- as.factor(c(1874:2023)); AREA1[[18]] <- m18

judge <- as.factor(c(1874:2023)) %in% rownames(M19);  c(1874:2023)[which(judge == "FALSE")]
# M19 is missing 1874 1875 1945
v2 = 0.4*M19[which(rownames(M19)=="1876"),] + 0.3*M19[which(rownames(M19)=="1877"),] + 0.2*M19[which(rownames(M19)=="1878"),] + 0.1*M19[which(rownames(M19)=="1879"),]
v1 = 0.4*v2 + 0.3*M19[which(rownames(M19)=="1876"),] + 0.2*M19[which(rownames(M19)=="1877"),] + 0.1*M19[which(rownames(M19)=="1878"),]
v3 = 0.4*M19[which(rownames(M19)=="1946"),] + 0.3*M19[which(rownames(M19)=="1947"),] + 0.2*M19[which(rownames(M19)=="1948"),] + 0.1*M19[which(rownames(M19)=="1949"),] 
m19 <- M19[which(rownames(M19) %in% as.factor(c(1874:2023))),];  which(rownames(m19) %in% c("1944", "1946"))
m19 <- rbind(v1,v2,m19[1:69,], v3, m19[70:147,]); rownames(m19) <- as.factor(c(1874:2023)); AREA1[[19]] <- m19

judge <- as.factor(c(1874:2023)) %in% rownames(M20);  c(1874:2023)[which(judge == "FALSE")]
# M20 is missing 1874 1875 1945
v2 = 0.4*M20[which(rownames(M20)=="1876"),] + 0.3*M20[which(rownames(M20)=="1877"),] + 0.2*M20[which(rownames(M20)=="1878"),] + 0.1*M20[which(rownames(M20)=="1879"),]
v1 = 0.4*v2 + 0.3*M20[which(rownames(M20)=="1876"),] + 0.2*M20[which(rownames(M20)=="1877"),] + 0.1*M20[which(rownames(M20)=="1878"),]
v3 = 0.4*M20[which(rownames(M20)=="1946"),] + 0.3*M20[which(rownames(M20)=="1947"),] + 0.2*M20[which(rownames(M20)=="1948"),] + 0.1*M20[which(rownames(M20)=="1949"),] 
m20 <- M20[which(rownames(M20) %in% as.factor(c(1874:2023))),];  which(rownames(m20) %in% c("1944", "1946"))
m20 <- rbind(v1,v2,m20[1:69,], v3, m20[70:147,]); rownames(m20) <- as.factor(c(1874:2023)); AREA1[[20]] <- m20

judge <- as.factor(c(1874:2023)) %in% rownames(M21);  c(1874:2023)[which(judge == "FALSE")]
# M21 is missing 1874 1875 1945
v2 = 0.4*M21[which(rownames(M21)=="1876"),] + 0.3*M21[which(rownames(M21)=="1877"),] + 0.2*M21[which(rownames(M21)=="1878"),] + 0.1*M21[which(rownames(M21)=="1879"),]
v1 = 0.4*v2 + 0.3*M21[which(rownames(M21)=="1876"),] + 0.2*M21[which(rownames(M21)=="1877"),] + 0.1*M21[which(rownames(M21)=="1878"),]
v3 = 0.4*M21[which(rownames(M21)=="1946"),] + 0.3*M21[which(rownames(M21)=="1947"),] + 0.2*M21[which(rownames(M21)=="1948"),] + 0.1*M21[which(rownames(M21)=="1949"),] 
m21 <- M21[which(rownames(M21) %in% as.factor(c(1874:2023))),];  which(rownames(m21) %in% c("1944", "1946"))
m21 <- rbind(v1,v2,m21[1:69,], v3, m21[70:147,]); rownames(m21) <- as.factor(c(1874:2023)); AREA1[[21]] <- m21

judge <- as.factor(c(1874:2023)) %in% rownames(M12);  c(1874:2023)[which(judge == "FALSE")]
# M12 is missing 1874 1875 1876 1877, 1898 1899, 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017
#v4 = 0.4*M12[which(rownames(M12)=="1878"),] + 0.3*M12[which(rownames(M12)=="1879"),] + 0.2*M12[which(rownames(M12)=="1880"),] + 0.1*M12[which(rownames(M12)=="1881"),]
#v3 = 0.4*v4 + 0.3*M12[which(rownames(M12)=="1878"),] + 0.2*M12[which(rownames(M12)=="1879"),] + 0.1*M12[which(rownames(M12)=="1880"),]
#v2 = 0.4*v3 + 0.3*v4 + 0.2*M12[which(rownames(M12)=="1878"),] + 0.1*M12[which(rownames(M12)=="1879"),]
#v1 = 0.4*v2 + 0.3*v3 + 0.2*v4 + 0.1*M12[which(rownames(M12)=="1878"),]
#v5 = 0.4*M12[which(rownames(M12)=="1897"),] + 0.3*M12[which(rownames(M12)=="1896"),] + 0.2*M12[which(rownames(M12)=="1895"),] + 0.1*M12[which(rownames(M12)=="1894"),]
#v6 = 0.4*v5 + 0.3*M12[which(rownames(M12)=="1897"),] + 0.2*M12[which(rownames(M12)=="1896"),] + 0.1*M12[which(rownames(M12)=="1895"),]
#which(rownames(m1) %in% c("2007", "2017"))
#D <- (m1[134:144,] + m2[134:144,] + m3[134:144,] + m4[134:144,] + m5[134:144,] + m6[134:144,] + m7[134:144,] + m8[134:144,])/8 
#m12 <- M12[which(rownames(M12) %in% as.factor(c(1874:2023))),];  which(rownames(m12) %in% c("1897", "1900", "2006", "2018"))
#m12 <- rbind(v1, v2, v3, v4, m12[1:20,], v5, v6, m12[21:127,], D, m12[128:133,])
#rownames(m12) <- as.factor(c(1874:2023)); AREA1[[12]] <- m12
mis_ind <- which(judge == "FALSE")
D <- (m10[mis_ind,] + m11[mis_ind,]+ m18[mis_ind,] + m19[mis_ind,] + m20[mis_ind,] + m21[mis_ind,])/6
nois <- matrix(0, length(mis_ind), 12);  for (i in 1:length(mis_ind)){ nois[i,] <- rnorm(12, 0, 1) }
D <- D + nois
m12 <- M12[which(rownames(M12) %in% as.factor(c(1874:2023))),] 
m12 <- rbind(m12, D);  m12 <- m12[order(as.numeric(rownames(m12))),]
rownames(m12) <- as.factor(c(1874:2023)); AREA1[[12]] <- m12


judge <- as.factor(c(1874:2023)) %in% rownames(M22);  c(1874:2023)[which(judge == "FALSE")]
# 1880 1891 1894 1895 1898 1899 1900 1901 1903 1905 1918 1919 1942 1943 1944 1945
#v1 = 0.4*M22[which(rownames(M22)=="1879"),] + 0.3*M22[which(rownames(M22)=="1878"),] + 0.2*M22[which(rownames(M22)=="1877"),] + 0.1*M22[which(rownames(M22)=="1876"),]
#v2 = 0.4*M22[which(rownames(M22)=="1890"),] + 0.3*M22[which(rownames(M22)=="1889"),] + 0.2*M22[which(rownames(M22)=="1888"),] + 0.1*M22[which(rownames(M22)=="1887"),]
#v3 = 0.4*M22[which(rownames(M22)=="1893"),] + 0.3*M22[which(rownames(M22)=="1892"),] + 0.2*v2 + 0.1*M22[which(rownames(M22)=="1890"),]
#v4 = 0.4*v3 + 0.3*M22[which(rownames(M22)=="1893"),] + 0.2*M22[which(rownames(M22)=="1892"),] + 0.1*v2
#nois <- matrix(0, 4, 12);    for (i in 1:4){ nois[i,] <- rnorm(12, 0, 1) }
#D1 <- M26[which(rownames(M26) %in% as.factor(c(1898:1901))),] + nois   # making up 1898-1901
#v1903 <- 0.4*M22[which(rownames(M22)=="1902"),] + 0.3*D1[4,] + 0.2*D1[3,] + 0.1*D1[2,]
#v1905 <- 0.4*M22[which(rownames(M22)=="1904"),] + 0.3*v1903 + 0.2*M22[which(rownames(M22)=="1902"),] + 0.1*D1[4,]
#v1918 <- 0.4*M22[which(rownames(M22)=="1917"),] + 0.3*M22[which(rownames(M22)=="1916"),] + 0.2*M22[which(rownames(M22)=="1915"),] + 0.1*M22[which(rownames(M22)=="1914"),]
#v1919 <- 0.4*v1918 + 0.3*M22[which(rownames(M22)=="1917"),] + 0.2*M22[which(rownames(M22)=="1916"),] + 0.1*M22[which(rownames(M22)=="1915"),]
#nois <- matrix(0, 4, 12);    for (i in 1:4){ nois[i,] <- rnorm(12, 0, 1) }
#D2 <- M26[which(rownames(M26) %in% as.factor(c(1942:1945))),] + nois   # making up 1942-1945
#which(rownames(m22) %in% as.factor(c(1879, 1890, 1893, 1897, 1902, 1917, 1941, 1946)))
#m22 <- rbind(m22[1:6,], v1, m22[7:16,], v2, m22[17:18,], v3, v4, m22[19:20,], D1, m22[21,], v1903, m22[22,], v1905, m22[23:34,], v1918, v1919, m22[35:56,], D2, m22[57:134,])
mis_ind <- which(judge == "FALSE")
D <- (m13[mis_ind,] + m14[mis_ind,] + m15[mis_ind,] + m16[mis_ind,] + m17[mis_ind,] + m18[mis_ind,])/6
nois <- matrix(0, length(mis_ind), 12);  for (i in 1:length(mis_ind)){ nois[i,] <- rnorm(12, 0, 1) }
D <- D + nois
m22 <- M22[which(rownames(M22) %in% as.factor(c(1874:2023))),] 
m22 <- rbind(m22, D);  m22 <- m22[order(as.numeric(rownames(m22))),]
rownames(m22) <- as.factor(c(1874:2023)); AREA1[[22]] <- m22


judge <- as.factor(c(1874:2023)) %in% rownames(M23);  c(1874:2023)[which(judge == "FALSE")]   # M23 is missing 1945
v = 0.4*M23[which(rownames(M23)=="1946"),] + 0.3*M23[which(rownames(M23)=="1947"),] + 0.2*M23[which(rownames(M23)=="1948"),] + 0.1*M23[which(rownames(M23)=="1949"),]
m23 <- M23[which(rownames(M23) %in% as.factor(c(1874:2023))),]; which(rownames(m23)=="1944") 
m23 <- rbind(m23[1:71,], v, m23[72:149,]); rownames(m23) <- as.factor(c(1874:2023)); AREA1[[23]] <- m23

judge <- as.factor(c(1874:2023)) %in% rownames(M24);  c(1874:2023)[which(judge == "FALSE")]   # M24 is missing 1945
v = 0.4*M24[which(rownames(M24)=="1946"),] + 0.3*M24[which(rownames(M24)=="1947"),] + 0.2*M24[which(rownames(M24)=="1948"),] + 0.1*M24[which(rownames(M24)=="1949"),]
m24 <- M24[which(rownames(M24) %in% as.factor(c(1874:2023))),]; which(rownames(m24)=="1944") 
m24 <- rbind(m24[1:71,], v, m24[72:149,]); rownames(m24) <- as.factor(c(1874:2023)); AREA1[[24]] <- m24

judge <- as.factor(c(1874:2023)) %in% rownames(M25);  c(1874:2023)[which(judge == "FALSE")]   # M25 is missing 1945
v = 0.4*M25[which(rownames(M25)=="1946"),] + 0.3*M25[which(rownames(M25)=="1947"),] + 0.2*M25[which(rownames(M25)=="1948"),] + 0.1*M25[which(rownames(M25)=="1949"),]
m25 <- M25[which(rownames(M25) %in% as.factor(c(1874:2023))),]; which(rownames(m25)=="1944") 
m25 <- rbind(m25[1:71,], v, m25[72:149,]); rownames(m25) <- as.factor(c(1874:2023)); AREA1[[25]] <- m25

judge <- as.factor(c(1874:2023)) %in% rownames(M26);  c(1874:2023)[which(judge == "FALSE")]   # M26 is missing 2020
v = 0.4*M26[which(rownames(M26)=="2019"),] + 0.3*M26[which(rownames(M26)=="2021"),] + 0.2*M26[which(rownames(M26)=="2022"),] + 0.1*M26[which(rownames(M26)=="2023"),]
m26 <- M26[which(rownames(M26) %in% as.factor(c(1874:2023))),]; which(rownames(m26)=="2019") 
m26 <- rbind(m26[1:146,], v, m26[147:149,]); rownames(m26) <- as.factor(c(1874:2023)); AREA1[[26]] <- m26


judge <- as.factor(c(1874:2023)) %in% rownames(M27);  c(1874:2023)[which(judge == "FALSE")]   
# M27 is missing 1874 1875 1876 1877 1878 1879 1887 1917 1918 1919 1920 1921 1922 1923 1924 1925 1926 1941 1942 1945 1946
mis_ind <- which(judge == "FALSE")
D <- (m23[mis_ind,] + m24[mis_ind,] + m25[mis_ind,] + m21[mis_ind,] )/4
nois <- matrix(0, length(mis_ind), 12);  for (i in 1:length(mis_ind)){ nois[i,] <- rnorm(12, 0, 1) }
D <- D + nois
m27 <- M27[which(rownames(M27) %in% as.factor(c(1874:2023))),];  m27 <- rbind(m27, D);  m27 <- m27[order(as.numeric(rownames(m27))),]
rownames(m27) <- as.factor(c(1874:2023)); AREA1[[27]] <- m27

judge <- as.factor(c(1874:2023)) %in% rownames(M28);  c(1874:2023)[which(judge == "FALSE")]   
# M28 is missing 1874 1875 1876 1877 1878 1879 1880 1945 1946
mis_ind <- which(judge == "FALSE")
D <- (m10[mis_ind,] + m13[mis_ind,] + m16[mis_ind,] + m21[mis_ind,] + m24[mis_ind,] + m25[mis_ind,] )/6
nois <- matrix(0, length(mis_ind), 12);  for (i in 1:length(mis_ind)){ nois[i,] <- rnorm(12, 0, 1) }
D <- D + nois
m28 <- M28[which(rownames(M28) %in% as.factor(c(1874:2023))),];  m28 <- rbind(m28, D);  m28 <- m28[order(as.numeric(rownames(m28))),]
AREA1[[28]] <- m28

judge <- as.factor(c(1874:2023)) %in% rownames(M29);  c(1874:2023)[which(judge == "FALSE")]   
# M29 is missing 1874 1875 1876 1877 1878 1944 1945
mis_ind <- which(judge == "FALSE")
D <- (m13[mis_ind,] + m14[mis_ind,] + m15[mis_ind,] + m16[mis_ind,] + m18[mis_ind,] + m20[mis_ind,] )/6
nois <- matrix(0, length(mis_ind), 12);  for (i in 1:length(mis_ind)){ nois[i,] <- rnorm(12, 0, 1) }
D <- D + nois
m29 <- M29[which(rownames(M29) %in% as.factor(c(1874:2023))),];  m29 <- rbind(m29, D);  m29 <- m29[order(as.numeric(rownames(m29))),]
AREA1[[29]] <- m29

judge <- as.factor(c(1874:2023)) %in% rownames(M30);  c(1874:2023)[which(judge == "FALSE")]   
# M30 is missing 1945 2000
v1 = 0.4*M30[which(rownames(M30)=="1946"),] + 0.3*M30[which(rownames(M30)=="1947"),] + 0.2*M30[which(rownames(M30)=="1948"),] + 0.1*M30[which(rownames(M30)=="1949"),] 
v2 = 0.4*M30[which(rownames(M30)=="2001"),] + 0.3*M30[which(rownames(M30)=="2002"),] + 0.2*M30[which(rownames(M30)=="2003"),] + 0.1*M30[which(rownames(M30)=="2004"),] 
m30 <- M30[which(rownames(M30) %in% as.factor(c(1874:2023))),]; which(rownames(m30) %in% c("1944", "1999"))
m30 <- rbind(m30[1:71,], v1, m30[72:125,], v2, m30[126:148,]); rownames(m30) <- as.factor(c(1874:2023)); AREA1[[30]] <- m30

judge <- as.factor(c(1874:2023)) %in% rownames(M31);  c(1874:2023)[which(judge == "FALSE")]
# M31 is missing 1945
v1 = 0.4*M31[which(rownames(M31)=="1946"),] + 0.3*M31[which(rownames(M31)=="1947"),] + 0.2*M31[which(rownames(M31)=="1948"),] + 0.1*M31[which(rownames(M31)=="1949"),] 
m31 <- M31[which(rownames(M31) %in% as.factor(c(1874:2023))),]; which(rownames(m31) %in% c("1944"))
m31 <- rbind(m31[1:71,], v1, m31[72:149,]); rownames(m31) <- as.factor(c(1874:2023)); AREA1[[31]] <- m31

judge <- as.factor(c(1874:2023)) %in% rownames(M32);  c(1874:2023)[which(judge == "FALSE")]   # M32 is complete
m32 <- M32[which(rownames(M32) %in% as.factor(c(1874:2023))),];  AREA1[[32]] <- m32

judge <- as.factor(c(1874:2023)) %in% rownames(M33);  c(1874:2023)[which(judge == "FALSE")]   
# M32 is missing 1874 1875 1944 1945
mis_ind <- which(judge == "FALSE")
D <- (m17[mis_ind,] + m19[mis_ind,] + m20[mis_ind,] + m21[mis_ind,] + m23[mis_ind,] + m24[mis_ind,])/6
nois <- matrix(0, length(mis_ind), 12);  for (i in 1:length(mis_ind)){ nois[i,] <- rnorm(12, 0, 1) }
D <- D + nois
m33 <- M33[which(rownames(M33) %in% as.factor(c(1874:2023))),];  m33 <- rbind(m33, D);  m33 <- m33[order(as.numeric(rownames(m33))),]
AREA1[[33]] <- m33

judge <- as.factor(c(1874:2023)) %in% rownames(M34);  c(1874:2023)[which(judge == "FALSE")]  # M34 is complete
m34 <- M34[which(rownames(M34) %in% as.factor(c(1874:2023))),];  AREA1[[34]] <- m34

judge <- as.factor(c(1874:2023)) %in% rownames(M35);  c(1874:2023)[which(judge == "FALSE")] 
# M35 is missing 1874 1875 1876 1877 1878 1920 1931 1932 1933 1934 1935 1936 1937 1938 1939 1940 1941 1942 1943 1944 1945 1946 1947
mis_ind <- which(judge == "FALSE")
D <- (m10[mis_ind,] + m11[mis_ind,] + m23[mis_ind,] + m24[mis_ind,] + m25[mis_ind,] + m26[mis_ind,])/6
nois <- matrix(0, length(mis_ind), 12);  for (i in 1:length(mis_ind)){ nois[i,] <- rnorm(12, 0, 1) }
D <- D + nois
m35 <- M35[which(rownames(M35) %in% as.factor(c(1874:2023))),];  m35 <- rbind(m35, D);  m35 <- m35[order(as.numeric(rownames(m35))),]
AREA1[[35]] <- m35

judge <- as.factor(c(1874:2023)) %in% rownames(M36);  c(1874:2023)[which(judge == "FALSE")] 
# M36 is missing 1874 1875 1876 1877 1878 1898 1899 1905 1906 1907 1908 1909 1910 1911 1912 1913 1914 1945 1946
mis_ind <- which(judge == "FALSE")
D <- (m13[mis_ind,] + m14[mis_ind,] + m15[mis_ind,] + m16[mis_ind,] + m17[mis_ind,] + m18[mis_ind,])/6
nois <- matrix(0, length(mis_ind), 12);  for (i in 1:length(mis_ind)){ nois[i,] <- rnorm(12, 0, 1) }
D <- D + nois
m36 <- M36[which(rownames(M36) %in% as.factor(c(1874:2023))),] 
m36 <- rbind(m36, D);  m36 <- m36[order(as.numeric(rownames(m36))),]
AREA1[[36]] <- m36

judge <- as.factor(c(1874:2023)) %in% rownames(M37);  c(1874:2023)[which(judge == "FALSE")] 
# M37 is missing 1938 1941 1942 1945 1946 1951 1952 1953 1954 1955 1965 1977
mis_ind <- which(judge == "FALSE")
D <- (m19[mis_ind,] + m20[mis_ind,] + m21[mis_ind,] + m23[mis_ind,] + m24[mis_ind,] + m25[mis_ind,])/6
nois <- matrix(0, length(mis_ind), 12);  for (i in 1:length(mis_ind)){ nois[i,] <- rnorm(12, 0, 1) }
D <- D + nois
m37 <- M37[which(rownames(M37) %in% as.factor(c(1874:2023))),] 
m37 <- rbind(m37, D);  m37 <- m37[order(as.numeric(rownames(m37))),]
AREA1[[37]] <- m37

judge <- as.factor(c(1874:2023)) %in% rownames(M38);  c(1874:2023)[which(judge == "FALSE")] 
# M38 is missing 1932 1947
v1 = 0.4*M38[which(rownames(M38)=="1931"),] + 0.3*M38[which(rownames(M38)=="1930"),] + 0.2*M38[which(rownames(M38)=="1929"),] + 0.1*M38[which(rownames(M38)=="1928"),] 
v2 = 0.4*M38[which(rownames(M38)=="1948"),] + 0.3*M38[which(rownames(M38)=="1949"),] + 0.2*M38[which(rownames(M38)=="1950"),] + 0.1*M38[which(rownames(M38)=="1951"),] 
m38 <- M38[which(rownames(M38) %in% as.factor(c(1874:2023))),]; which(rownames(m38) %in% c("1931", "1946")) 
m38 <- rbind(m38[1:58,], v1, m38[59:72,], v2, m38[73:148,])
rownames(m38) <- as.factor(c(1874:2023)); AREA1[[38]] <- m38

judge <- as.factor(c(1874:2023)) %in% rownames(M39);  c(1874:2023)[which(judge == "FALSE")] 
# M39 is missing 1874 1875 1876 1877 1898 1899 2015 2016 2017 2018 2019 2020 2021 2022 2023
mis_ind <- which(judge == "FALSE")
D <- (m13[mis_ind,] + m15[mis_ind,] + m18[mis_ind,] + m21[mis_ind,] + m23[mis_ind,] + m25[mis_ind,])/6
nois <- matrix(0, length(mis_ind), 12);  for (i in 1:length(mis_ind)){ nois[i,] <- rnorm(12, 0, 1) }
D <- D + nois
m39 <- M39[which(rownames(M39) %in% as.factor(c(1874:2023))),] 
m39 <- rbind(m39, D);  m39 <- m39[order(as.numeric(rownames(m39))),]
AREA1[[39]] <- m39

judge <- as.factor(c(1874:2023)) %in% rownames(M40);  c(1874:2023)[which(judge == "FALSE")] 
# M40 is missing 1874 1875 1876 1877 1878 1879 2001 2003 2005 2006 2007
mis_ind <- which(judge == "FALSE")
D <- m38[mis_ind,]; nois <- matrix(0, length(mis_ind), 12);  for (i in 1:length(mis_ind)){ nois[i,] <- rnorm(12, 0, 1) }
D <- D + nois
m40 <- M40[which(rownames(M40) %in% as.factor(c(1874:2023))),] 
m40 <- rbind(m40, D);  m40 <- m40[order(as.numeric(rownames(m40))),]
AREA1[[40]] <- m40


save(AREA1, file = "~/Documents/papers/Wasserstein_depth/REAL_DATA/area1_alphebat_order/area1_final.RData")





