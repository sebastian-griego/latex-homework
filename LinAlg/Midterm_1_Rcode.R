setwd("C:/Users/sebas/OneDrive/Desktop/Homework/LinAlg")

data <- read.csv("NOAAGlobalT.csv", header = TRUE)

#I will find gridboxed for San Francisco, Chicago, London, and Tokyo
#SF (37.7749 N, 122.4194 W)
#Chicago (41.8781 N, 87.6298 W)
#London (51.5072 N, 0.1276 W)
#Tokyo (35.6764 N, 139.6500 E)
# The dataset uses longitude 0 to 360
# For San Francisco, 122.4194 W corresponds to 237.5806 (= 360 - 122.4194)
# For Chicago, 87.6298 W corresponds to 272.3702 (= 360 - 87.6298)
# For London, 0.1276 W corresponds to 359.8724 (= 360 - 0.1276)
# For Tokyo, 139.6500 E stays because it is east

#Find the rows
row_sf = which(data$LAT > 35 & data$LAT < 40 & data$LON > 235 & data$LON < 240)
row_ch = which(data$LAT > 40 & data$LAT < 45 & data$LON > 270 & data$LON < 275)
row_l = which(data$LAT > 50 & data$LAT < 55 & data$LON > 355 & data$LON < 360)
row_t = which(data$LAT > 35 & data$LAT < 40 & data$LON > 135 & data$LON < 140)

#Get the data and combine it
data4 = data[c(row_sf, row_ch, row_l, row_t), ]
data4[, 1:8]

#I want to find the data from 1991 to 1996
#data4[,4] corresponds to January 1880
#4 + 12 * (1991-1880)= 1336

#check
data4[1336:1341]
#this gives 1991.1 - 1996.6

#get 6 years of data 1336 + 72 = 1408
data6years = data4[,1336:1408]

#check
data6years[, 66:68]
#works

JuneOnly = data6years[ ,seq(6, 66, by = 12)]

#check
JuneOnly
#     X1991.6 X1992.6 X1993.6 X1994.6 X1995.6 X1996.6
# 1848 -1.4826  0.7795  0.9494  0.2273 -0.2399  0.6615
# 1927  2.0285 -1.4971 -1.0721  0.4447  1.3542 -0.4503
# 2088 -1.2829  1.2994  0.4849 -0.0177  0.3695  0.0444
# 1828  1.0840 -0.1687 -0.4509  0.2126 -0.4990 -0.1933

rownames(JuneOnly) = c("San Francisco", "Chicago", "London", "Tokyo")
colnames(JuneOnly) = seq(1991, 1996, by = 1)

W = JuneOnly
W
#                 1991    1992    1993    1994    1995    1996
# San Francisco -1.4826  0.7795  0.9494  0.2273 -0.2399  0.6615
# Chicago        2.0285 -1.4971 -1.0721  0.4447  1.3542 -0.4503
# London        -1.2829  1.2994  0.4849 -0.0177  0.3695  0.0444
# Tokyo          1.0840 -0.1687 -0.4509  0.2126 -0.4990 -0.1933



#Problem 2a

# Transpose the matrix
W_t = t(W)

# Plot the matrix
image(W_t, col = topo.colors(64), 
      xlab = "Time (Years)", 
      ylab = "Locations", 
      main = "Space-Time Data Matrix",
      axes = FALSE)

# Add axis labels
axis(1, at = seq(0, 1, length = 6), 
     labels = rownames(W_t))
axis(2, at = seq(0, 1, length= 4), 
     labels = colnames(W_t), las = 2)


#Problem 2b

svdW = svd(W)

U = svdW$u
D = diag(svdW$d)
V = svdW$v

U
D
V

#> U
#           [,1]       [,2]       [,3]      [,4]
#[1,]  0.4813186 -0.1546374  0.5825904 0.6364024
#[2,] -0.7339319 -0.5973775  0.1198705 0.3001918
#[3,]  0.4140915 -0.5527022 -0.6975263 0.1910643
#[4,] -0.2412562  0.5601318 -0.3995954 0.6843766
#> D
#         [,1]     [,2]      [,3]      [,4]
#[1,] 4.108776 0.000000 0.0000000 0.0000000
#[2,] 0.000000 1.311814 0.0000000 0.0000000
#[3,] 0.000000 0.000000 0.7559272 0.0000000
#[4,] 0.000000 0.000000 0.0000000 0.5158252
#> V
#            [,1]        [,2]        [,3]        [,4]
#[1,] -0.72896255  0.25440333 -0.21020047  0.31436362
#[2,]  0.49959597 -0.02963954 -0.74647769  0.34793405
#[3,]  0.37806583 -0.02053120  0.35260708  0.12877717
#[4,] -0.06707504 -0.13106736  0.14964596  0.81474541
#[5,] -0.20345850 -0.95714823 -0.04732287 -0.03307095
#[6,]  0.17375061  0.02583673  0.49962155  0.31405427


#Problem 2c

eof1 = U[,1]
eof1
library(maps)
library(mapdata)

par(mar = c(0,0,0,0))
map(database = "world2Hires", ylim = c(-70, 70), mar = c(0,0,0,0))
grid(nx = 12, ny = 6)
points(237.5806, 37.7749, pch = 16, col = "red", cex = 2)
text(237.5806, 25, labels = "San Francisco 0.4813186", col = "red")
points(272.3702, 41.8781, pch = 16, col = "blue", cex = 3)
text(272.3702, 32, labels = "Chicago -0.7339319", col = "blue")
points(359.8724, 51.5072, pch = 16, col = "red", cex = 1.9)
text(359.8724, 39, labels = "London 0.4140915", col = "red")
points(139.6500, 35.6764, pch = 16, col = "blue", cex = 1.5)
text(139.6500, 23, labels = "Tokyo -0.2412562", col = "blue")



# Problem 3a

setwd("C:/Users/sebas/OneDrive/Desktop/Homework/LinAlg")
earth_temp <- read.csv('EarthTemperatureData.csv', header = TRUE)

#Verify data
dim(earth_temp)
earth_temp[1:3, 1:6]

# Extract monthly data
monthly_temp = earth_temp[, 2:13]

# Verify data
monthly_temp[1:2, 1:4]
monthly_temp[1,]

# Transpose the matrix and convert to a vector
vector_temp = c(t(monthly_temp))

# Compare with original data
vector_temp[1:24]
earth_temp[1:2,]

#> vector_temp[1:24]
# [1] -0.702 -0.284 -0.732 -0.570 -0.325 -0.213 -0.128 -0.233 -0.444 -0.452
# [11] -0.190 -0.268 -0.303 -0.362 -0.485 -0.445 -0.302 -0.189 -0.215 -0.153
# [21] -0.108 -0.063 -0.030 -0.067
#> earth_temp[1:2,]
#  YEAR    JAN    FEB    MAR    APR    MAY    JUN    JUL    AUG    SEP    OCT
# 1 1850 -0.702 -0.284 -0.732 -0.570 -0.325 -0.213 -0.128 -0.233 -0.444 -0.452
# 2 1851 -0.303 -0.362 -0.485 -0.445 -0.302 -0.189 -0.215 -0.153 -0.108 -0.063
#    NOV    DEC ANNUAL
# 1 -0.19 -0.268 -0.375
# 2 -0.03 -0.067 -0.223

# Check the length of the vector
length(vector_temp)
#1992

#Problem 3b

#The data is from 1850 to 2015, but we want 1901 to 2000.
#1901 corresponds to the 52nd year, so we need to remove the first 51 years or 612 data points.
#2000 corresponds to the 150th year, so we need to remove the last 15 years or 180 data points.

#Remove the first 612 data points and the last 180 data points
new_vector_temp = vector_temp[613:1812]

length(new_vector_temp)
#1200

#Verify
new_vector_temp[1:5]
earth_temp[52, 1:5 ]

#> new_vector_temp[1:5]
#[1] -0.182 -0.270 -0.246 -0.193 -0.197
#> earth_temp[52, 1:5 ]
#   YEAR    JAN   FEB    MAR    APR
#52 1901 -0.182 -0.27 -0.246 -0.193

new_vector_temp[1189:1200]
earth_temp[151, ]

#> new_vector_temp[1189:1200]
#[1] 0.227 0.455 0.382 0.479 0.280 0.275 0.262 0.358 0.307 0.222 0.162 0.151
#> earth_temp[151, ]
#    YEAR   JAN   FEB   MAR   APR  MAY   JUN   JUL   AUG   SEP   OCT   NOV   DEC
#151 2000 0.227 0.455 0.382 0.479 0.28 0.275 0.262 0.358 0.307 0.222 0.162 0.151
#    ANNUAL
#151  0.295

#We have the correct data for the years 1901 to 2000.


time = seq(1901, 2000, len = length(new_vector_temp))

plot(time, new_vector_temp, type = "l", col = "blue", lwd = 2,
     xlab = "Time (Years)",
     ylab = "Temperature Anomaly (Celsius)",
     main = "Temperature Anomaly over Time")


#Problem 5b

H = matrix(c(2,3,4,4,6,0,0,0,1), nrow = 3, byrow = TRUE)
H

det(H)
#[1] 0

#The determinant of H is 0, so it is not invertible.

#Problem 5c
B = matrix(c(1, 2, 3, 4, 4, 5, 6, 0, 7, 1, 9, 0, 0, 1, 0, 8),
     nrow = 4,
     byrow = TRUE)
B

solve(B)
#> solve(B)
#            [,1]        [,2]        [,3]        [,4]
#[1,] -0.86666667  0.23333333  0.13333333  0.43333333
#[2,] -0.13333333  0.26666667 -0.13333333  0.06666667
[3,]  0.68888889 -0.21111111  0.02222222 -0.34444444
[4,]  0.01666667 -0.03333333  0.01666667  0.11666667


#Problem 5d

det(B)
#[1] -360


#Problem 5e

eigenB = eigen(B)
eigenB

evectorsB = eigenB$vectors
evectorsB

u1 = evectorsB[,1]
u1
#[1] -0.3147967 -0.6507493 -0.6795365 -0.1251344

norm(u1, type = "2")
#[1] 1

u2 = evectorsB[,2]
u2
#[1]  0.1563868 -0.5525792 -0.3132630  0.7563503

norm(u2, type = "2")
#[1] 1

u3 = evectorsB[,3]
u3
#[1] -0.07853434 -0.93494531  0.27385363  0.21145639

norm(u3, type = "2")
#[1] 1

u4 = evectorsB[,4]
u4
#[1]  0.818962684  0.027010169 -0.573202985 -0.002985091

norm(u4, type = "2")
#[1] 1

#All of the eigenvectors are normalized to 1, so eigenB gives unit eigenvectors and eigenvalues

eigenB
#eigen() decomposition
#$values
#[1] 13.200401  7.269414  3.578543 -1.048358

#$vectors
#           [,1]       [,2]        [,3]         [,4]
#[1,] -0.3147967  0.1563868 -0.07853434  0.818962684
#[2,] -0.6507493 -0.5525792 -0.93494531  0.027010169
#[3,] -0.6795365 -0.3132630  0.27385363 -0.573202985
#[4,] -0.1251344  0.7563503  0.21145639 -0.002985091

#These are the eigenvalues their corresponding eigenvectors of B


#Problem 5f

library(geometry)
dot(u1, u2)
#[1] 0.4285886

#The dot product of u1 and u2 is not 0, so they are not orthogonal.