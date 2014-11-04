rm(list=ls())
#require(DCluster)
#?moranI.stat

setwd("~/github/RCompBio/cholera-activity")
list.files()
pumps.tb = read.csv("Pumps.csv")

death.tb = read.csv("Deaths.csv")
num.of.deaths = length(death.tb[,1])
summary(death.tb)

head(death.tb)
plot( death.tb$y ~ death.tb$x, col="blue" )

plot( pumps.tb$y ~ pumps.tb$x, col='green', pch=19, xlim=c(8,20))
points( death.tb$x, death.tb$y, pch=18, col='blue')
points( pumps.tb$x[7], pumps.tb$y[7], col='red', pch=19)

#We hypothesize that the red pump is the source of epidemics
red.x = pumps.tb$x[7]
red.y = pumps.tb$y[7]

#calculate distances of deaths to the "red pump". 
dist.obs = 
  sum(sqrt( (death.tb$x - red.x)^2 + (death.tb$y - red.y)^2 ))/num.of.deaths 

#use a function to do this
cal.avg.dist = function( in.tb, ref.x, ref.y) {
  dist = sum(sqrt( (in.tb$x - red.x)^2 + (in.tb$y - red.y)^2 ))/length(in.tb[,1])
  return(dist)
}

#double-check
cal.avg.dist(death.tb, red.x, red.y )

#####randomly put the death events on the map
random.death.tb = death.tb #just set up an empty table
#Assign random numbers to the x and y of random death events
random.death.tb$x = runif( num.of.deaths, min=min(death.tb$x), max=max(death.tb$x) )
random.death.tb$y = runif( num.of.deaths, min=min(death.tb$y), max=max(death.tb$y) )

dist.random1 = cal.avg.dist( random.death.tb, red.x, red.y )

dist.random1  #average distance of random deaths to the red pump
dist.obs  #average distance of actual death to the red pump

#Question: How do we know it is significant? We need to estimate a p-value. 
# We need to generate a random distribution for the so-called null hypothesis.

runs =1E5
dist.random.deaths = numeric( length=runs)
for( i in 1:runs){
  random.death.tb$x = runif( num.of.deaths, min=min(death.tb$x), max=max(death.tb$x) )
  random.death.tb$y = runif( num.of.deaths, min=min(death.tb$y), max=max(death.tb$y) )
  dist.random.deaths[i] = cal.avg.dist( random.death.tb, red.x, red.y )  
}

hist(dist.random.deaths, xlim=c(2.0, 5), main="Evaluation of signifance using random distributions", 
     xlab="average distance to the potential leaked pump")
arrows(dist.obs, runs/10, dist.obs, 0, col="red")
text( dist.obs, runs/8, "observation" )

#Question, what is the p-value? 



