#------------------------------------------------------------------------------#
# Code to run simulations and generate Figures
# ** make.R last run on 2018-06-18 ** 
#------------------------------------------------------------------------------#
rm(list = ls())   

source("load.R")

# generate Figure 2: Exploitation and run time series

source("figure_2.R")
#------------------------------------------------------------------------------#
# Fit age-structures state-space stock-recruit model to data
#------------------------------------------------------------------------------#

source("bayes_sr_model_fit.R")

# generate Figure 3: SR relationship adn productivity time series

source("figure 3.R")

#------------------------------------------------------------------------------#
# Run simulations to quantify prospects for recovery over next twenty years 
#------------------------------------------------------------------------------#
	
source("forward_sims.R")

# generate Figure 4: stock trajectories and probabilities of exceeding recovery goals

source("figure_4.R")

#------------------------------------------------------------------------------#
# Run harvest monitoring simulations 
#------------------------------------------------------------------------------#
	
source("monitoring_simulations.R")

# generate Figure 5: consequences of alternative catch monitoring effort

source("figure_5.R")
