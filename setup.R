#------------------------------------------------
# This script sets out to set up everything
# needed to run the project
#------------------------------------------------

#--------------------------------------
# Author: Trent Henderson, 4 March 2021
#--------------------------------------

library(tidyverse)
library(scales)
library(brms)
library(bayesplot)

# Create an output and data folder if none exists:

if(!dir.exists('output')) dir.create('output')
if(!dir.exists('data')) dir.create('data')