############ descriptive analyses - Disruption-Recovery paper ##############
# tomas.diviak@manchester.ac.uk

getwd()

library(tidyverse)
library(openxlsx)

#----- loading more datasets at once -----
# extracting names of the files
data_names <- list.files(pattern = ".xlsx") # all names of xlsx files in WD
data_names <- str_extract(data_names, "[^\\.]+") # without "."
data_names

data_paths <- list.files(pattern = ".xlsx", full.names = TRUE) # all data paths

# lapply to loop read.xlsx over all files with headers and row names
data_list <- lapply(data_paths, read.xlsx, 
                    colNames = TRUE, rowNames = TRUE, sheet = 1) 
names(data_list) <- data_names # giving them names extracted above

data_list # list of all the matrices (as lists) with their names
typeof(data_list[[1]])

# matrix and igraph conversion 
library(igraph)

disruptedMats <- lapply(data_list, as.matrix)
disruptedGs <- lapply(disruptedMats, graph_from_adjacency_matrix, 
                      mode = "undirected")
disruptedGs

#----- looping over more networks and saving results into a dataframe -----
# defining functions for looping
avgdeg <- function(x) {
  avgdeg <- mean(degree(x, mode = "all"))
  print(avgdeg)
}

sddeg <- function(x) {
  sddeg <- sd(degree(x, mode = "all"))
  print(sddeg)
}

# a function for calculating the dependent var - compactness
compactness <- function(g) {
  gra.geo <- distances(g) ## get geodesics
  gra.rdist <- 1 / gra.geo  ## get reciprocal of geodesics
  diag(gra.rdist) <- NA   ## assign NA to diagonal
  gra.rdist[gra.rdist == Inf] <- 0 ## replace infinity with 0
  # Compactness = mean of reciprocal distances
  comp.igph <- mean(gra.rdist, na.rm = TRUE) 
  return(comp.igph)
}

# compactness with a constant denominator (for different sizes)
compactnessNorm <- function(g, n) {
  gra.geo <- distances(g) ## get geodesics
  gra.rdist <- 1 / gra.geo  ## get reciprocal of geodesics
  diag(gra.rdist) <- NA   ## assign NA to diagonal
  gra.rdist[gra.rdist == Inf] <- 0 ## replace infinity with 0
  # Compactness = mean of reciprocal distances -> including removed nodes too
  comp.igph <- sum(gra.rdist, na.rm = TRUE)/(n*(n-1))
  return(comp.igph)
}

# using lapply to loop over the graphs and creating vectors of results
size <- lapply(disruptedGs, gorder) # TRY USING SAPPLY/VAPPLY INSTEAD!!!!!!!!
avg_deg <- lapply(disruptedGs, avgdeg)
sd_deg <- lapply(disruptedGs, sddeg)
avg_dist <- lapply(disruptedGs, mean_distance)
diamet <- lapply(disruptedGs, diameter)
clust_coeff <- lapply(disruptedGs, transitivity)
deg_assort <- lapply(disruptedGs, assortativity_degree)
compact_orig <- lapply(disruptedGs, compactness)
compact_norm <- lapply(disruptedGs, compactnessNorm, n = 54)
dense <- lapply(disruptedGs, edge_density)

# problem with centralization - produces a list
central <- sapply(disruptedGs, centr_degree) # sapply simplifies output
deg_cent <- central[seq(2, length(central), 3)] # extracting each 3rd element starting with the 2nd

# binding the columns together
dis_type <- c("margin11", "margin5", "margin8", "top2rank", "top3btwn", "top3dgr", 
          "top6btwn", "top6dgr", "topdgr&btwn") # mind the order!
disrupted <- data.frame(cbind(dis_type, size, avg_deg, sd_deg, dense, deg_cent, avg_dist, 
                     diamet, clust_coeff, deg_assort, compact_orig, compact_norm))
str(disrupted) # need to save it as a tibble/dataframe ideally
typeof(disrupted)
View(disrupted)
disruptedDF <- as_tibble(as.matrix(disrupted)) 
View(disruptedDF) # still a list - WHY?!?!?!
typeof(disruptedDF)
write.xlsx(disruptedDF, "disruptedNets_descriptives_v2.xlsx") #rowNames = TRUE

# visualisation with ggplot
library(ggplot2)

df <- read.xlsx("disruptedNets_descriptives_v2.xlsx", colNames = TRUE, sheet = 1)
View(df) # NEED TO FIX THE VALUES IN EXCEL TO NUMBERS!!!

# need to reorder in ascending or descending order
# line plot
ggplot(data = df, 
       aes(x = dis_type, y = compact_norm, group = 1)) +
  geom_line() +
  geom_hline(yintercept = 0.57, color = "red") + # yintercept = obs value
  geom_point(size = 3) +
  labs(title = "Effect of disruption strategies on compactness", 
       y = "Compactness (standardized)", x = "Type of disruption") +
  theme_bw()

# bar plot
ggplot(df, aes(x = dis_type, y = compact_norm, group = 1)) +
  geom_bar(fill = "steelblue", stat = "identity") +
  labs(title = "Effect of disruption strategies on compactness", 
       y = "Compactness (standardized)", 
       x = "Type of disruption") +
  geom_hline(yintercept = 0.57, color = "red") +
  theme_bw()
