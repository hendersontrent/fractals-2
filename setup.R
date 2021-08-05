#------------------------------------------------
# This script sets out to set up everything
# needed to run the project
#------------------------------------------------

#--------------------------------------
# Author: Trent Henderson, 4 March 2021
#--------------------------------------

library(dplyr)
library(magrittr)
library(tidyr)
library(readr)
library(ggplot2)
library(scales)
library(tibble)
library(janitor)
library(brms)
library(bayesplot)
library(janitor)
library(Cairo)

# Create an output, data, and functions folder if none exists

if(!dir.exists('output')) dir.create('output')
if(!dir.exists('data')) dir.create('data')
if(!dir.exists('R')) dir.create('R')

# Load any functions in the R folder

r_files <- list.files("R", full.names = TRUE, pattern = "\\.[Rr]")
for(f in r_files){
  source(f)
}
