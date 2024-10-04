########## visualisations of simulation results ##########

getwd()

library(ggplot2)

# reading in the DF created by another script
# this would be automated with map in the future
simsDF <- readRDS("homoEthRate8By05CompNormSimsDF.rds") # update DF accordingly!!!

# remove the redundant string from the id
simsDF$id <- gsub("gang_disrupted_", "", simsDF$id)

# visualization with ggplot
ggplot(simsDF, aes(x = id, y = value)) + 
  geom_violin() +
  geom_boxplot(width = 0.1) + 
  theme_bw() + 
  geom_hline(yintercept = 0.57, linetype = "dashed", color = "red") +
  labs(title = "Long-term recovery (rate = 8) - ethnic homophily increase (+0.5)", # update title accordingly!!!
       y = "Compactness", x = "Type of disruption")

