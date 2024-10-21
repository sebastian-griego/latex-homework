setwd("C:/Users/sebas/OneDrive/Desktop/Homework/LinAlg")
Pda<-read.table("PSTANDdarwin.txt", header=F)
dim(Pda) 
Pda
pdaDec<-Pda[,13] #Darwin Dec standardized SLP anomalies data
pdaDec

Pta<-read.table("PSTANDtahiti.txt", header=F)
ptaDec=Pta[,13] #Tahiti Dec standardized SLP anomalies
ptada1 = rbind(pdaDec, ptaDec) #space-time data matrix
ptada1
#Space-time data format
colnames(ptada1) <- 1951:2015

rownames(ptada1)<-c("Darwin", "Tahiti")
ptada1

dim(ptada1)
svdptd = svd(ptada1)

svdptd

U=round(svdptd$u, digits=2)
U

D = round(diag(svdptd$d), digits = 2)
D

V =round(svdptd$v, digits=2)
t(V)

eof1 = U[, 1]
eof2 = U[, 2]

PC1 = V[, 1]
PC2 = V[, 2]


x = c(eof1[1], eof2[1])
y = c(eof1[2], eof2[2])

# Plot EOFs with different colors
plot(x,y, col = c("blue", "red"), pch = 16,)

years = 1951:2015

# Plot PC over time
plot(years, PC1, type = 'l', col = 'blue', xlab = 'Year', ylab = 'Principal Component Value', 
     main = 'Principal Components over Time', ylim = range(c(PC1, PC2)))
lines(years, PC2, type = 'l', col = 'red')
legend('topright', legend = c('PC1', 'PC2'), col = c('blue', 'red'), lty = 1)



# Problem 2
setwd("C:/Users/sebas/OneDrive/Desktop/Homework/LinAlg")


SF = c(214.9, 107.7, 233.4, 337.5, 359.1)
SB = c(695.9, 389.7, 391.2, 446.8, 768.3)
SD = c(496.2, 475.1, 490.7, 685.7, 664.2)

mSF = mean(SF)
mSB = mean(SB)
mSD = mean(SD)

aSF = (SF - mSF)/sd(SF)
aSB = (SB - mSB)/sd(SB)
aSD = (SD - mSD)/sd(SD)

A = rbind(aSF, aSB, aSD)

C = A %*% t(A) / 5

C

invC = solve(C)

invC

eigC = eigen(C)

eigC

X = svd(A)

X

2.9560380^2/5
1.7084803^2/5
0.5856059^2/5


pc1 = X$v[,1]
pc2 = X$v[,2]
pc3 = X$v[,3]

years = 2001:2005

plot(years, pc1, type='b', col='red', ylim=range(c(pc1,pc2,pc3)), 
     xlab='Year', ylab='PC Value', main='Principal Component Time Series')
lines(years, pc2, col='blue')
lines(years, pc3, col='green')
legend('topright', legend=c('PC1', 'PC2', 'PC3'), 
       col=c('red', 'blue', 'green'), lty=1, pch=1)

#Next problem

A3 = matrix(c(0,1,0,0,0,1,2,-2,-1), nrow=3, byrow=TRUE)

b3 = c(4,6,0)

solve(A3,b3)



# Next problem
setwd("C:/Users/sebas/OneDrive/Desktop/Homework/LinAlg")

noaa_data <- read.csv("NOAAGlobalT.csv", header = TRUE)

noaa1 = noaa_data[1777,]
noaa2 = noaa_data[1778,]
noaa3 = noaa_data[1779,]
noaa4 = noaa_data[1780,]

# Adjust DecDat to use data from 2001 to 2008
DecIndex = seq(1467, 1551, 12)
DecDat = noaa1[DecIndex]
DecDat2 = noaa2[DecIndex]
DecDat3 = noaa3[DecIndex]
DecDat4 = noaa4[DecIndex]

# Verify the data
DecDat

#rbind some stuff
Xm = rbind(DecDat, DecDat2, DecDat3, DecDat4)

rownames(Xm) = c('Lat 237.5/Lon 32.5', 'Lat 242.5/Lon 32.5', 'Lat 247.5/Lon 32.5', 'Lat 252.5/Lon 32.5')
colnames(Xm) = 2001:2008
Xm

#Next problem
A1x = matrix(c(1.7, -0.7, 1.3, -1.6, -1.4, 0.4, -1.5, -0.3, 0.6), nrow=3, byrow=TRUE)
solve(A1x)

#Next problem
A2x = matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 0), nrow=3, byrow=TRUE)
b2 = c(-1, 0, 1)
solve(A2x, b2)


#next question
A3 = matrix(c(0, 1, 0, 0, 0, 1, 2, -2, -1), nrow=3, byrow=TRUE)
b3 = c(4, 6, 0)
solve(A3, b3)

#next question
A = matrix(c(0, 4, -2, -7), nrow=2, byrow=TRUE)
eigen_result = eigen(A)
eigenvectors = eigen_result$vectors

unit_eigenvectors = apply(eigenvectors, 2, function(v) v / sqrt(sum(v^2)))

unit_eigenvectors
