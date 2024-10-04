############ descriptive analysis - micro-macro paper ##############
# tomas.diviak@manchester.ac.uk

getwd()

# packages
library(igraph)
library(openxlsx)
library(ggraph)

# reading in the data
dat <- as.matrix(read.xlsx("LONDON_GANG.xlsx", 
                           colNames = TRUE, rowNames = TRUE, sheet = 1))
View(dat)

# igraph format
net <- graph_from_adjacency_matrix(dat, mode = "undirected")
summary(net)
plot(net)

# whole network measures
edge_density(net, loops = FALSE)
centr_degree(net, mode = "all")
mean_distance(net, directed = FALSE)
diameter(net, directed = FALSE)
transitivity(net)
assortativity_degree(net, directed = FALSE)

library(statnet)
network <- network(dat, matrix.type = "adjacency", directed = FALSE)
network
connectedness(network)

compactness <- function(g) {
  gra.geo <- distances(g) ## get geodesics
  gra.rdist <- 1/gra.geo  ## get reciprocal of geodesics
  diag(gra.rdist) <- NA   ## assign NA to diagonal
  gra.rdist[gra.rdist == Inf] <- 0 ## replace infinity with 0
  # Compactness = mean of reciprocal distances
  comp.igph <- mean(gra.rdist, na.rm=TRUE) 
  return(comp.igph)
}

# compactness with a constant denominator (for different sizes)
compactnessNorm <- function(g, n) {
  gra.geo <- distances(g) ## get geodesics
  gra.rdist <- 1 / gra.geo  ## get reciprocal of geodesics
  diag(gra.rdist) <- NA   ## assign NA to diagonal
  gra.rdist[gra.rdist == Inf] <- 0 ## replace infinity with 0
  # Compactness = mean of reciprocal distances -> including removed nodes too
  comp.igph <- sum(gra.rdist, na.rm = TRUE)/(n*(n - 1)) 
  return(comp.igph)
}

compactness(net)
compactnessNorm(net, n = 54)

detach("package:statnet", unload = TRUE) # conflicts with igraph
detach("package:sna", unload = TRUE)

# centrality measures
deg <- degree(net, mode = "all")
head(sort(deg, decreasing = TRUE), 10)
head(sort(deg, decreasing = FALSE), 12)

# degree-based whole network measures 
hist(deg)

mean(deg)
sd(deg)

# betweenness 
btwns <- betweenness(net)
head(sort(btwns, decreasing = TRUE), 10)
head(sort(btwns, decreasing = FALSE), 12)

# eigenvector
eigenv <- eigen_centrality(net)$vector

# subgroups & modularity
eb <- cluster_edge_betweenness(net)
modularity(eb)

im <- cluster_infomap(net)
modularity(im)

lv <- cluster_louvain(net)
modularity(lv)

fg <- cluster_fast_greedy(net)
modularity(fg)



# attribute-based measures
att <- read.xlsx("LONDON_GANG_ATTR.xlsx", sheet = 1, colNames = TRUE)
str(att)

# descriptives
sapply(att[,6:9], table)
sapply(att[,3:4], table)
summary(att$Age)
summary(att$Arrests)
summary(att$Convictions)

# attaching the attribute to the igraph object
netatt <- set_vertex_attr(net, "age", index = V(net), att$Age)
netatt
netatt <- set_vertex_attr(net, "arrests", index = V(net), att$Arrests)
netatt <- set_vertex_attr(net, "convictions", index = V(net), att$Convictions)
summary(netatt)

# numeric assortativity
assortativity(netatt, types1 = V(netatt)$age)
assortativity(netatt, types1 = V(netatt)$arrests)
assortativity(netatt, types1 = V(netatt)$convictions)

# nominal assortativity
netatt <- set_vertex_attr(net, "ethnic", index = V(net), att$Birthplace)
netatt
assortativity_nominal(netatt, types = V(netatt)$ethnic)

# residence, prison, and music need to be integers starting with 1
netatt <- set_vertex_attr(net, "music", index = V(net), att$Music)
assortativity_nominal(netatt, types = V(netatt)$music)

netatt <- set_vertex_attr(net, "prison", index = V(net), att$Prison)
assortativity_nominal(netatt, types = V(netatt)$prison)

netatt <- set_vertex_attr(net, "ranking", index = V(net), att$Ranking)
assortativity_nominal(netatt, types = V(netatt)$ranking)

netatt <- set_vertex_attr(net, "resid", index = V(net), att$Residence)
assortativity_nominal(netatt, types = V(netatt)$ethnic)

# correlations with centralities
rankRev <- 6 - att$Ranking 

cor(rankRev, deg, method = "spearman")
cor(rankRev, btwns, method = "spearman")
cor(rankRev, eigenv, method = "spearman")

# descriptives with sna & network
library(sna)
library(network)

net <- network(net1, matrix.type = "adjacency", directed = FALSE) 
summary(net)

network.size(net)
gden(net)
network.density(net)
centralization(net, degree, mode = "graph")
connectedness(net)
gdnet <- geodist(net)
gdnet
mean(gdnet$gdist)
max(gdnet$gdist)
mean(degree(net, gmode = "graph")) 
sd(degree(net, gmode = "graph"))
gtrans(net, mode = "graph")

# network visualization with ggraph
my_palette <- c("#1A5878", "#C44237", "#AD8941", "#E99093", "#50594B")

ggraph(netatt, layout = 'kk') + 
  geom_edge_link(alpha = 0.25) + 
  scale_fill_brewer(palette = "Set1") +
  geom_node_point(aes(size = betweenness(netatt), fill = as.character(ranking)), 
                      shape = 21) + 
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_graph() +
  theme(legend.position = "bottom") +
  labs(fill = "ranking", size = "betweenness")

# visualisation with ggplot
library(ggplot2)

df <- read.xlsx("results.xlsx", colNames = TRUE, sheet = 2)
str(df)

ggplot(data = df, aes(x = reorder(type, -compactness), y = compactness, group = 1)) +
  geom_line() +
  geom_hline(yintercept = 0.57, color = "red") +
  geom_point(size = 3) +
  labs(title = "Effect of disruption strategies on compactness", 
       y = "Compactness", x = "Type of disruption") +
  theme_bw()


