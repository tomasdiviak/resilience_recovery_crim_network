###### stationary SAOM analysis - micro-macro paper ##########
# tomas.diviak@manchester.ac.uk

library(RSiena)
library(openxlsx)

set.seed(42)

# loading the data
net1 <- as.matrix(read.xlsx("LONDON_GANG.xlsx", colNames = TRUE, rowNames = TRUE, 
                           sheet = 1))
net2 <- as.matrix(read.xlsx("LONDON_GANG.xlsx", colNames = TRUE, rowNames = TRUE, 
                            sheet = 2))
dcov <- as.matrix(read.xlsx("LONDON_GANG.xlsx", colNames = TRUE, rowNames = TRUE, 
                            sheet = 3))
att <- read.xlsx("LONDON_GANG_ATTR.xlsx", colNames = TRUE, sheet = 1)

# transformations
att$ethnic <- att$Birthplace
att$ethnic[att$ethnic == 4] <- 0

# defining RSiena variables
nets  <- sienaNet(array(c(net1, net2), dim = c(54, 54, 2)), allowOnly = FALSE)
age <- coCovar(att[,2])
resid <- coCovar(att[,3])
ethnic <- coCovar(att[,10])
prison <- coCovar(att[,7])
arrests <- coCovar(att[,4])
rank <- coCovar(att[,9])
kin <- coDyadCovar(dcov)

gangNet <- sienaDataCreate(nets, age, resid, ethnic, prison, arrests, rank, kin)
gangNet

print01Report(gangNet, modelname = 'gangNet_rep')

# choosing algorithms and effects
myAlgo2 <- sienaAlgorithmCreate(projname = "gangNet", useStdInits = FALSE,
                                modelType = c(nets = 2), cond = FALSE)
myAlgo3 <- sienaAlgorithmCreate(projname = "gangNet", useStdInits = FALSE,
                                modelType = c(nets = 3), cond = FALSE)

# specifying the model
?includeEffects
myeff <- getEffects(gangNet)
effectsDocumentation(myeff)
myeff <- setEffect(myeff, Rate, fix = TRUE, test = FALSE, period = 1,
                   type = "rate", initialValue = 20)
myeff <- includeEffects(myeff, degPlus) 
myeff <- includeEffects(myeff, gwesp)
myeff <- includeEffects(myeff, between)
myeff <- includeEffects(myeff, egoX, interaction1 = "age")
myeff <- includeEffects(myeff, egoX, interaction1 = "resid", include = FALSE)
myeff <- includeEffects(myeff, egoX, interaction1 = "prison")
myeff <- includeEffects(myeff, egoX, interaction1 = "arrests")
myeff <- includeEffects(myeff, simX, interaction1 = "age")
myeff <- includeEffects(myeff, simX, interaction1 = "rank")
myeff <- includeEffects(myeff, sameX, interaction1 = "ethnic")
myeff <- includeEffects(myeff, sameX, interaction1 = "resid", include = FALSE)
myeff

# running the model
model2 <- siena07(myAlgo2, data = gangNet, effects = myeff, returnDeps = TRUE,
                  prevAns = model2) 
model2

model3 <- siena07(myAlgo3, data = gangNet, effects = myeff, returnDeps = TRUE,
                  prevAns = model3)
model3

# assessing goodness of fit
gof2.dd <- sienaGOF(model2, verbose = TRUE, varName = "nets", IndegreeDistribution)
gof2.dd
plot(gof2.dd)

gof3.dd <- sienaGOF(model3, verbose = TRUE, varName = "nets", IndegreeDistribution)
gof3.dd
plot(gof3.dd)

# GOF for geodesic distances  requires a function to get GDs from simulations
GeodesicDistribution <- function(i, data, sims, period, groupName,
                                 varName, levls=c(1:5,Inf), cumulative=TRUE, ...) {
  x <- networkExtraction(i, data, sims, period, groupName, varName)
  require(sna)
  a <- sna::geodist(symmetrize(x))$gdist
  if (cumulative)
  {
    gdi <- sapply(levls, function(i){ sum(a <= i) })
  }
  else
  {
    gdi <- sapply(levls, function(i){ sum(a == i) })
  }
  names(gdi) <- as.character(levls)
  gdi
}

gof2.gd <- sienaGOF(model2, verbose = TRUE, varName = "nets", GeodesicDistribution)
gof2.gd
plot(gof2.gd)

gof3.gd <- sienaGOF(model3, verbose = TRUE, varName = "nets", GeodesicDistribution)
gof3.gd
plot(gof3.gd)

# GOF for triad census requires a function to get triads from simulations
TriadCensus <- function(i, data, sims, wave, groupName, varName, levls=1:16){
  
  unloadNamespace("igraph") # to avoid package clashes
  
  require(sna)
  
  require(network)
  
  x <- networkExtraction(i, data, sims, wave, groupName, varName)
  
  if (network.edgecount(x) <= 0){x <- symmetrize(x)}
  
  # because else triad.census(x) will lead to an error
  
  tc <- sna::triad.census(x, mode = "graph")
  
  # triad names are transferred automatically
  
  tc
  
}

gof2.tc <- sienaGOF(model2, verbose = TRUE, varName = "nets", TriadCensus)
gof2.tc
plot(gof2.tc, scale = TRUE, center = TRUE)

gof3.tc <- sienaGOF(model3, verbose = TRUE, varName = "nets", TriadCensus)
gof3.tc
plot(gof3.tc, scale = TRUE, center = TRUE)
