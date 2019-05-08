#------------------------------------------------------------------------------#
# Code to run simulations and generate Figures
# ** make.R last run on 2019-04-30 ** 
#------------------------------------------------------------------------------#
rm(list = ls())   

source("load.R")

# generate Figure 2: Exploitation and run time series

source("figure_2.R")

# generate Figure 2: Exploitation and run time series

source("figure_3.R")

#------------------------------------------------------------------------------#
# Fit age-structures state-space stock-recruit model to data
#------------------------------------------------------------------------------#

source("bayes_sr_model_fit.R")

# generate Figure 4: SR relationship and productivity time series

source("figure 4.R")

#------------------------------------------------------------------------------#
# Run simulations to quantify prospects for recovery over next twenty years 
#------------------------------------------------------------------------------#
	
source("forward_sims.R")

# generate Figure 4: stock trajectories and probabilities of exceeding recovery goals

source("figure_5.R")

#------------------------------------------------------------------------------#
# Run harvest monitoring simulations and generate figures
#------------------------------------------------------------------------------#
	
source("samp_sims_figure6_7.R")

