## ---------------------------
##
## Script name: 
##
## Purpose of script: 
##
## Author: Esther Amores Gago
##
## Copyright (c) Esther Amores Gago, 2022
##
## ---------------------------
##
## Notes:
##
##
## ---------------------------

###############
##
## Sources
##
###############

source("settings.R")
load(here(rdata_dir, "features_train.RData"))
load(here(rdata_dir, "features_valid.RData"))

## ---------------------------

install.packages("tensorflow")
library(tensorflow)

install.packages("keras")
library(keras)

##############
library(keras)
library(tfruns)
library(tfestimators)


model_from_json("cmi_mbam01.json")
model <- load_model_weights_hdf5("cmi_mbam01.h5")

summary(model)

for(layer in get_layer(model)[-5]) {
  get_output_at(layer)
  
}

#################################




