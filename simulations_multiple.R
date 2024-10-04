###### stationary SAOM simulations - multiple networks ##########
# tomas.diviak@manchester.ac.uk

set.seed(42)
getwd()
#setwd("C:/Users/tomas/OneDrive - Filozofická fakulta, 
#Univerzita Karlova/práce/postdoc/micro-macro/London Gang/data")

library(tidyverse)
library(openxlsx)
library(RSiena)
library(purrr)
library(igraph)

# loading more datasets at once ---------------------------------------------
data_names <- list.files(pattern = "xlsx") 
data_names <- str_extract(data_names, "[^\\.]+") 
data_names

# all data paths
data_paths <- list.files(pattern = ".xlsx", full.names = TRUE) 

# custom function for reading multiple sheets from multiple files -----------
read_network <- function(name, sheet) {
  as.matrix(read.xlsx(name,
                      colNames = TRUE, 
                      rowNames = TRUE, 
                      sheet = sheet))
}

# map from purrr: for each file, load given sheet
net1 <- map(data_paths, ~read_network(.x, sheet = 1))
net2 <- map(data_paths, ~read_network(.x, sheet = 2))
attr <- map(data_paths, ~read_network(.x, sheet = 3))


# Creating RSiena variables -----------------------------------------------
# map for creating RSiena variables
nets <- map2(net1, net2, 
             ~sienaNet(array(c(.x, .y), dim = c(dim(.x), 2)), allowOnly = FALSE))

# covariates - first col = names
age <- map(seq(nets), ~coCovar(attr[[.x]][,1]))
ethnic <- map(seq(nets), ~coCovar(attr[[.x]][,2])) 
prison <- map(seq(nets), ~coCovar(attr[[.x]][,6]))
arrests <- map(seq(nets), ~coCovar(attr[[.x]][,4]))
rank <- map(seq(nets), ~coCovar(attr[[.x]][,8])) 

# pmap a function on 3 arguments to create RSiena data
gangNets <- pmap(
  list(nets, age, ethnic, prison, arrests, rank),
  function(nets, age, ethnic, prison, arrests, rank) {
    sienaDataCreate(nets, age, ethnic, prison, arrests, rank)
  }
)

# choosing algorithms and effects for estimation
gangAlgo <- map2(data_names, nets, 
                ~sienaAlgorithmCreate(projname = .x, useStdInits = FALSE,
                                      modelType = c(.y = 2), cond = FALSE, 
                                      simOnly = TRUE, nsub = 0))

# in map 2, Model Type is set to ".y" instead of the name of the net
# this is a workaround, so siena07 works.
for (i in 1:length(gangAlgo)) {
  names(gangAlgo[[i]]$modelType) <- "nets"
}

# specifying the model with estimates from a sSAOM ----------------------------
# update values accordingly!!!
# relative in/decrease = theta +/- 0.5(or0.2)*theta
# absolute in/decrease = theta +/- 0.5(or1)
myEf <- map(gangNets, ~getEffects(.x))
myEf <- map(myEf, ~setEffect(.x, Rate, fix = TRUE, test = FALSE, period = 1,
                             type = "rate", initialValue = 8)) ## 2; 8
myEf <- map(myEf, ~setEffect(.x, density, initialValue = -1.6754)) 
myEf <- map(myEf, ~setEffect(.x, degPlus, initialValue = 0.0167 - 0.5)) ## 
myEf <- map(myEf, ~setEffect(.x, gwesp, initialValue = 1.408)) ## 
myEf <- map(myEf, ~setEffect(.x, between, initialValue = -0.312))  ## 
myEf <- map(myEf, ~setEffect(.x, egoX, interaction1 = "age", initialValue = 0.0674))
myEf <- map(myEf, ~setEffect(.x, simX, interaction1 = "age", initialValue = 0.7921)) ## 
myEf <- map(myEf, ~setEffect(.x, sameX, interaction1 = "ethnic", initialValue = 0.3244)) ## 
myEf <- map(myEf, ~setEffect(.x, egoX, interaction1 = "prison", initialValue = -0.0975))
myEf <- map(myEf, ~setEffect(.x, egoX, interaction1 = "arrests", initialValue = 0.0135))
myEf <- map(myEf, ~setEffect(.x, simX, interaction1 = "rank", initialValue = 0.1735))
myEf

# Simulating from the models -------------------------------------------------
# minimal example
trial <- siena07(gangAlgo[[1]], data = gangNets[[1]], effects = myEf[[1]], returnDeps = T)

# simulating all the models using pmap
simsMulti <- pmap(list(gangAlgo, gangNets, myEf),
                  function(gangAlgo, gangNets, myEf)
                    siena07(gangAlgo, data = gangNets, effects = myEf, returnDeps = T))

# sticking back names
names(simsMulti) <- data_names

# model results
simsMulti
summary(simsMulti[[1]])

# Reading all the edgelists into n x n adjacency matrix -------------------
edgelistToMatrix <- function(x, n)
{
  x <- x[[1]][[1]][[1]] 
  admat <- matrix(0, n, n) 
  admat[x[, 1:2]] <- x[, 3] 
  admat 
}

# extracting the simulations
simsELs <- map(simsMulti, ~.x[["sims"]])
length(simsELs)

# Automating the sizes extraction
nrow(net1[[1]])
order <- map_dbl(net1, ~nrow(.x))

# map2 for each 1k simulations of a given size
simsMats <- map2(simsELs, order, ~map2(.x, .y, ~edgelistToMatrix(x = .x, n = .y)))
simsMats[[3]][[1]]

# converting to matrices
simsAdMats <- map(simsMats, as.matrix)
simsAdMats[[2]][[1]] 

# saving the matrices
saveRDS(simsAdMats, file = "prefAttRate8By05SimsAdMats.rds") # update name!!!

# in case of recalculating indices on the matrices, load them
#simsMats <- readRDS("baseRate8SimsAdMats.rds") # skip as necessary

# converting matrices to graphs 
simsGs <- map(simsMats, ~map(.x, ~graph_from_adjacency_matrix(.x, mode = "undirected")))
length(simsGs)
simsGs[[2]][[1]]

# Calculating compactness -------------------------------------------------

# calculating standardized compactness with a constant denominator
compactnessNorm <- function(g, n) {
  gra.geo <- distances(g) 
  gra.rdist <- 1 / gra.geo  
  diag(gra.rdist) <- NA   
  gra.rdist[gra.rdist == Inf] <- 0 
  comp.igph <- sum(gra.rdist, na.rm = TRUE)/(n*(n - 1)) 
  return(comp.igph)
}

# mapping compactness over the list of graphs
compactGs <- map(simsGs, ~map(.x, compactnessNorm, n = 54))
str(compactGs)

# calculating non-standardized compactness
compactness <- function(g) {
  gra.geo <- distances(g) 
  gra.rdist <- 1/gra.geo  
  diag(gra.rdist) <- NA   
  gra.rdist[gra.rdist == Inf] <- 0 
  # Compactness = mean of reciprocal distances
  comp.igph <- mean(gra.rdist, na.rm=TRUE) 
  return(comp.igph)
}

# mapping compactness over the list of graphs
compactnessGs <- map(simsGs, ~map(.x, compactness))
str(compactnessGs)

# Binding compactness into a dataframe ------------------------------------
# create a dataframe with the network type and its compactness
compactDF <- map(compactGs, ~map_df(.x, as_tibble)) %>%
  bind_rows(.id = "id")
view(compactDF)

# saving the dataframe
# legend: recovery(1of6)By(Thetavalue)Compactness(norm/ornot)SimsDF
saveRDS(compactDF, file = "prefAttRate8By05CompNormSimsDF.rds") # update name!!!
