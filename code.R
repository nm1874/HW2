#1.a

library(plotrix)
#The magnitude of the determinant is the area of the image of the unit square.
#Start by creating a matrix M.
#v2 <- c(2,3); M <-cbind(v1,v2); M; det(M) #determinant of M is 4

#Set up the screen with narrow margins and aspect ratio 1:1
pars<-par(mar=c(1,1,1,1)+0.1, pch=20)  #set up narrow margins
plot(NULL, xlim = c(0,25), ylim = c(-1,25), xlab = "", ylab= "", asp = 1, axes = FALSE)
axis(1,pos = 0); axis(2,pos = 0) #set up axes at left asnd bottom

arrows(0, 0, 30*cos(32*pi/180), 30*sin(32*pi/180), col = "green")
arrows(0, 0, 25*cos(22*pi/180), 25*sin(22*pi/180), col="red")
v1 <- c(25*cos(22*pi/180), 25*sin(22*pi/180))
v2 <- c(8*cos(60*pi/180), 8*sin(60*pi/180))
v3 <- v1+v2
arrows(v1[1], v1[2], v3[1], v3[2], col="red")


#distance: 
v0 <- c(30*cos(32*pi/180), 30*sin(32*pi/180))
v <- v0-v3
v


#Dot product operator
"%.%" <- function(x,y) sum(x*y)

Norm<- function(v) sqrt(v %.% v)

Norm(v)

#distance = 1.782646


#1.b

ref_a <- cbind(c(cos(2*40*pi/180), sin(2*40*pi/180)), c(sin(2*40*pi/180), -cos(2*40*pi/180)))
ref_b <- cbind(c(cos(2*30*pi/180), sin(2*30*pi/180)), c(sin(2*30*pi/180), -cos(2*30*pi/180)))
ref_c <- cbind(c(cos(2*80*pi/180), sin(2*80*pi/180)), c(sin(2*80*pi/180), -cos(2*80*pi/180)))
ref_all <- cbind(c(cos(2*(40+80-30)*pi/180), sin(2*(40+80-30)*pi/180)), c(sin(2*(40+80-30)*pi/180), -cos(2*(40+80-30)*pi/180)))

pars<-par(mar=c(1,1,1,1)+0.1, pch=20)  #set up narrow margins
plot(NULL, xlim = c(0,3), ylim = c(-2,3), xlab = "", ylab= "", asp = 1, axes = FALSE)
axis(1,pos = 0); axis(2,pos = 0) #set up axes at left asnd bottom

ref_a_result <- ref_a%*%c(1,0)
ref_b_result <- ref_b%*%ref_a_result
ref_c_result <- ref_c%*%ref_b_result
ref_all_result <- ref_all%*%c(1,0)

arrows(0, 0, 1, 0, col="green")
text(.6, .2, expression("original"), cex=1, col="green")
arrows(0, 0, ref_a_result[1], ref_a_result[2], col="red")
text(.8, .8, expression("reflect over 40"), cex=1, col="red")
arrows(0, 0, ref_b_result[1], ref_b_result[2], col="gray")
text(.6, -.5, expression("reflect over 40 then 30"), cex=1, col="gray")
arrows(0, 0, ref_c_result[1], ref_c_result[2], col="black")
text(-1.5, .2, expression("reflect over 40 then 30 then 80"), cex=1, col="black")
arrows(0, 0, ref_all_result[1], ref_all_result[2], col="black")
text(-1.5, -.2, expression("reflect over 40+80-30"), cex=1, col="black")





#2.a 

#You can look up latitudes and longitudes on latlong.com
#Boston is at latitude 42.35, longitude -71.05
source("1.2L-VectorLibrary.R")
latB <- 42.36; longB <- -71.06
latN <- 40.84; longN <- 14.26
latD <- 53.35; longD <- 6.26
#Now calculate the unit vector for each city
#The capitalized trig functions take angles in degrees
Boston <- c(Cos(latB)*Cos(longB),Cos(latB)*Sin(longB),Sin(latB)); Boston #unit vector for Boston
Naples <- c(Cos(latN)*Cos(longN),Cos(latN)*Sin(longN),Sin(latN)); Naples
Dublin <- c(Cos(latD)*Cos(longD),Cos(latD)*Sin(longD),Sin(latD)); Dublin 





#How long is the trip?
#Work out the distance in degrees from Boston to Oslo
degrees_bd <- angleBetween(Boston,Dublin); degrees_bd #50.40199
#By definition, 90 degrees on the Earth equals 10,000 kilometers
kilometers_bd <- 10000*degrees_bd/90
kilometers_bd   #the trip is 5600 km

degrees_dn <- angleBetween(Dublin, Naples); degrees_dn #13.62378
#By definition, 90 degrees on the Earth equals 10,000 kilometers
kilometers_dn <- 10000*degrees_dn/90
kilometers_dn   #the trip is 1513 km

kilometers_bd + kilometers_dn
#full trip is 7113 km


degrees_bn <- angleBetween(Boston,Naples); degrees_bn #60.90669
#By definition, 90 degrees on the Earth equals 10,000 kilometers
kilometers_bn <- 10000*degrees_bn/90
kilometers_bn
#trip is 6767.41




#2.c 

new_city <- Boston + new_city/2


"%x%" <- function(v,w) c(v[2]*w[3]-v[3]*w[2],v[3]*w[1]-v[1]*w[3], v[1]*w[2]-v[2]*w[1])

new_city %x% Boston
new_city %x% Naples
new_city %x% Dublin
