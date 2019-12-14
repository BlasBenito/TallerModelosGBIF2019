setwd("/home/blas/Desktop/DYNAMIC_MODELS_R")
library(raster)

#LOADING INPUT DATA
####################
load("inputData.RData")

#copy to keep the original
presence.original<-presence

#SOME GRAPHICAL PARAMETERS
##########################
#colors
sdm.colors=gray.colors(99)
#zoom
zoom=extent(c(463000, 475000, 4100000, 4113000))


#PARAMETERS
###########
#TIME
years=2000:2100
years.length=length(years)

#POPULATION PARAMETERS
max.dispersal.distance=1000
max.age=100
reproductive.age=5
fecundity=4
max.mortality=10

#STARTING POPULATION
presence<-presence.original
#subset of individuals to make the simulation faster
presence=presence[sample(1:nrow(presence), size=100), ]
presence$presencia=NULL
#AGE
presence$age=sample(1:max.age, size=nrow(presence), replace=TRUE)
#FECUNDITY
presence$suitability=NA


#POP SIZE PER YEAR
pop.size=vector()
years.vector=vector()

#SIMULATION
#############################################################################
#LOOPING THROUGH YEARS
for (year.index in 1:years.length){

  #getting the current year
  year=years[year.index]
  cat(paste("Simulating year", year, sep=" "), sep="\n")

  #POPULATION SIZE PER YEAR
  pop.size[year.index]=nrow(presence)
  years.vector[year.index]=year

  #PLOTTING SPATIAL DATA
  #######################
  par(mfrow=c(1,2))
  plot(sdm[[year.index]], col=sdm.colors, breaks=seq(min(minValue(sdm[[year.index]])), max(maxValue(sdm[[year.index]])), length.out=100), legend=FALSE, main=year, ext=zoom)
  #presence
  points(presence[which(presence$age < reproductive.age), "x"], presence[which(presence$age < reproductive.age), "y"], cex=0.5, pch=20, col="green")
  points(presence[which(presence$age >= reproductive.age), "x"], presence[which(presence$age >= reproductive.age), "y"], cex=0.5, pch=20, col="red")
  #pop.size
  plot(years.vector, pop.size, xlab="Year", ylab="Population size", type="l")
  ########################
  
  #INCREASE AGE
  presence$age=presence$age+1
  
  #HABITAT SUITABILITY CHANGE
  presence$suitability=as.vector(extract(sdm[[year.index]], presence[, c("x", "y")]))
  
  #SIMULATING MORTALITY
  if (nrow(presence) > 0){
    presence=presence[which(presence$age <= max.age), ]
    }
    
  #MORTALITY BY CLIMATE (AGE PROVIDES RESISTANCE)
  if (nrow(presence) > 0){
    presence=presence[which((presence$suitability + presence$age) > sample(1:100, size=nrow(presence), replace=TRUE)), ]
    }
  
  #MORTALITY BY CHANCE (PLAGUES AND THE LIKES)
  if (nrow(presence) > 0){
    mortality=round((sample(1:max.mortality, size=1, replace=TRUE)))
    presence=presence[-sample(1:nrow(presence), size=mortality, replace=TRUE), ]
  }
    
    #REPRODUCTION AND DISPERSAL
    #selecting reproductives
    reproductives=presence[which(presence$age >= reproductive.age), ]
  
    if (nrow(reproductives) >= 1){
  
      #producing seeds
      seeds=reproductives[rep(row.names(reproductives), fecundity), ]
      seeds$age=0
  
      #dispersal
      seeds$x=seeds$x + sample(-max.dispersal.distance:max.dispersal.distance, size=nrow(seeds), replace=TRUE)
      seeds$y=seeds$y + sample(-max.dispersal.distance:max.dispersal.distance, size=nrow(seeds), replace=TRUE)
  
      #joining seeds to the population
      presence=rbind(presence, seeds)
    
      } 
    
      if (nrow(presence)==0) {
      message("Population crashed, end of simulation.")
      break #end of simulation
      }
    
    Sys.sleep(1)

}

