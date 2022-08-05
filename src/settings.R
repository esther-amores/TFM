## ---------------------------
##
## Script name: settings.R
##
## Purpose of script: Set main settings
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
## Packages
##
###############

## If a package is already installed, it will be loaded.
## If any are not, the missing package(s) will be installed from CRAN and then loaded.

# Package names
packages <- c(
  "data.table",
  "fftw", # sudo apt-get install fftw3 fftw3-dev pkg-config
  "tuneR",
  "seewave",
  "warbleR", # sudo apt-get install cmake
  "soundgen",
  "here",
  "reticulate",
  "tidyverse", # sudo apt-get install libxml2-dev libssl-dev libcurl4-openssl-dev
  "jpeg",
  "Sim.DiffProc",
  "memuse",
  "factoextra",
  "ggpubr",
  "corrplot",
  "RColorBrewer",
  "gridExtra",
  "dynaSpec", # sudo apt-get install -y libavfilter-dev
  "viridis",
  "tidytext",
  "parallel",
  "glmnet",
  "cvms",
  "pROC",
  "nnet",
  "e1071",
  "boot",
  "knitr",
  "kableExtra",
  "xtable",
  "igraph"
)

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(
  packages,
  library,
  character.only = TRUE,
  verbose = TRUE
))

## ---------------------------

###############
##
## Working directory
##
###############

## Set working directory for files
data_dir <- here("..", "data")

processed_dir <- here("..", "data", "processed")
rdata_dir <- here("..", "data", "processed", "rdata")

warblrb10k_dir <- here("..", "data", "warblrb10k")
warblrb10k_wav_dir <- here("..", "data", "warblrb10k", "wav")

xc_dir <- here("..", "data", "xeno-canto")
xc_mp3_dir <- here("..", "data", "xeno-canto", "mp3")
xc_wav_dir <- here("..", "data", "xeno-canto", "wav")

train_dir <- here("..", "data", "train")
test_dir <- here("..", "data", "test")

models_dir <- here("..", "models")

reports_dir <- here("..", "reports")
figures_dir <- here("..", "reports", "figures")
spec_dir <- here("..", "reports", "figures", "spec")
snr_spec_dir <- here("..", "reports", "figures", "snr_spec")
spec_clean_dir <- here("..", "reports", "figures", "spec_clean")
plots_dir <- here("..", "reports", "figures", "plots")
tables_dir <- here("..", "reports", "tables")

src_dir <- here("..", "src")


## Create new folders if don't exist
if (!dir.exists(data_dir)) {
  dir.create(data_dir)
}

if (!dir.exists(processed_dir)) {
  dir.create(processed_dir)
}

if (!dir.exists(rdata_dir)) {
  dir.create(rdata_dir)
}

if (!dir.exists(warblrb10k_dir)) {
  dir.create(warblrb10k_dir)
}

if (!dir.exists(warblrb10k_wav_dir)) {
  dir.create(warblrb10k_wav_dir)
}

if (!dir.exists(xc_dir)) {
  dir.create(xc_dir)
}

if (!dir.exists(xc_mp3_dir)) {
  dir.create(xc_mp3_dir)
}

if (!dir.exists(xc_wav_dir)) {
  dir.create(xc_wav_dir)
}

if (!dir.exists(train_dir)) {
  dir.create(train_dir)
}

if (!dir.exists(test_dir)) {
  dir.create(test_dir)
}

if (!dir.exists(models_dir)) {
  dir.create(models_dir)
}

if (!dir.exists(reports_dir)) {
  dir.create(reports_dir)
}

if (!dir.exists(figures_dir)) {
  dir.create(figures_dir)
}

if (!dir.exists(spec_dir)) {
  dir.create(spec_dir)
}

if (!dir.exists(snr_spec_dir)) {
  dir.create(snr_spec_dir)
}

if (!dir.exists(spec_clean_dir)) {
  dir.create(spec_clean_dir)
}

if (!dir.exists(plots_dir)) {
  dir.create(plots_dir)
}

if (!dir.exists(tables_dir)) {
  dir.create(tables_dir)
}

if (!dir.exists(src_dir)) {
  dir.create(src_dir)
}

## ---------------------------
