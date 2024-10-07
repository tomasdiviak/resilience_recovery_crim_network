# **Disruption and recovery of a criminal network: a simulation study** 
 Code used for analyses and simulations in Diviák, T. Structural resilience and recovery of a criminal network after disruption: a simulation study. *J Exp Criminol* 20, 883–911 (2024). [https://doi.org/10.1007/s11292-023-09563-z](https://doi.org/10.1007/s11292-023-09563-z)

**descriptives.R**: simple descriptive analysis of a network focusing on its structure, central nodes, and individual attributes

**stationarySAOM.R**: using so-call stationary stochastic actor-oriented models (SAOM) for cross-sectional network data (unlike the usual usage of SAOM) to get parameter estimates that are used in the subsequent simulations

Both **descriptives.R** and **stationarySAOM.R** use the data contained in *LONDON_GANG.xlsx* and *LONDON_GANG_ATTR.xlsx*, which is the the original data collected and analysed by Grund and Densley (2012; 2015) and it is taken from the Mitchell Centre covert network database [here](https://sites.google.com/site/ucinetsoftware/datasets/covert-networks/london-gang?authuser=0). The network data have been dichotomised for the purposes of this study.

**descriptives_multiple.R**: looping descriptive analyses over multiple networks yielded by removing certain actors based on the description of the original network

**simulations_multiple.R**: using stationary SAOM to simulate potential recovery of each variant of the disrupted network, the simulations are based on using and manipulating the parameter estimates in the previous analysis

Both **descriptives_multiple.R** and **simulations_multiple.R** use the data contained in *LondonGang_disruptedAttr.xlsx* and *LondonGang_disruptedWithIsolates.xlsx*, which is both based on removing certain actors from the network based on their centralities or attributes.

