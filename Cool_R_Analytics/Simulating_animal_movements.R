
library (raster)  
library (dismo)  

###function for the go
go <- function (sp, env, n, sigma, theta_x, alpha_x, theta_y, alpha_y) {  
  track <- data.frame()  
  track[1,1] <- sp@x  
  track[1,2] <- sp@y  
  for (step in 2:n) {  
    neig <- adjacent(env,   
                     cellFromXY(env, matrix(c(track[step-1,1],  
                                              track[step-1,2]), 1,2)),   
                     directions=8, pairs=FALSE )  
    options <- data.frame()  
    for (i in 1:length(neig)){  
      options[i,1]<-neig[i]  
      options[i,2]<- sp@opt - env[neig[i]]  
    }  
    option <- c(options[abs(na.omit(options$V2)) == min(abs(na.omit(options$V2))), 1 ],   
                options[abs(na.omit(options$V2)) == min(abs(na.omit(options$V2))), 1 ])  
    new_cell <- sample(option,1)  
    new_coords <- xyFromCell(env,new_cell)  
    lon_candidate<--9999  
    lat_candidate<--9999  
    
    while ( is.na(extract(env, matrix(c(lon_candidate,lat_candidate),1,2)))) {  
      lon_candidate <- new_coords[1]+ (sigma * rnorm(1)) + (alpha_x * ( theta_x - new_coords[1]))  
      lat_candidate <- new_coords[2]+ (sigma * rnorm(1)) + (alpha_y * ( theta_y - new_coords[2]))  
    }  
    track[step,1] <- lon_candidate  
    track[step,2] <- lat_candidate  
  }  
  return(track)  
}  


#https://www.r-bloggers.com/simulating-animal-movements-and-habitat-use/
#The raster package provides classes and functions to manipulate geographic (spatial) data in 'raster' format. 
#Raster data divides space into cells (rectangles; pixels) of equal size (in units of the coordinate reference system). 
tc <- raster(nrows=100, ncols=100, xmn=0, xmx=100, ymn=0,ymx=100)
#runif generates random deviates (n,min,max)
tc[] <- runif(10000, -80, 180)
#Calculate focal ("moving window") weight matrix.. Concentratwa the areas for higher density
tc <- focal(tc, w=matrix(1, 5, 5), mean)  
tc <- focal(tc, w=matrix(1, 5, 5), mean)  
plot(tc)  

#Set species
species <- setClass("species", slots=c(x="numeric", y="numeric", opt="numeric"))  

#Setting location and optimal tree cover
Red_deer <- species(x= 50, y =50, opt= 90)  
Egyptian_mongoose <- species(x= 50, y =50, opt= 30)

#go function -> sp=species, env = enviromental variable, n = # of interations
#sigma = Brownian motion parameter, theta = geographical optimum, alpha = attraction strength
#path <- go (sp, env, n, sigma, theta_x, alpha_x, theta_y, alpha_y) 

deer_simul <- go (Red_deer, tc, 100, 2, 90, 0, 90, 0)  
mongoose_simul <- go (Egyptian_mongoose, tc, 100, 2, 90, 0, 90, 0)  

#plots the paths
plot(tc)  
lines(deer_simul, lwd=1.5, col="red")  
points(deer_simul, cex=0.3, col="red")  
lines(mongoose_simul, lwd=1.5, col="blue")  
points(mongoose_simul, cex=0.3, col="blue")  
legend("topleft", legend=c("deer","mongoose"), col=c("red","blue"),  
       lty=c(1,1), lwd=c(2,2))  

#plots the density by tree cover
plot(density(extract(tc, deer_simul)),lwd=3, col="red", xlim=c(20,80),   
     ylim=c(0,max(c(density(extract(tc, deer_simul))$y,  
                    density(extract(tc, mongoose_simul))$y))),  
     main="locations density distribution", xlab="tree cover")  
lines(density(extract(tc, mongoose_simul)),lwd=3, col="blue")  
legend("topleft", legend=c("deer","mongoose"), col=c("red","blue"),  
       lty=c(1,1), lwd=c(3,3)) 

