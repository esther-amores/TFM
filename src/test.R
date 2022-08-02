## ---------------------------
##
## Script name: signal_param.R
##
## Purpose of script: Signal parametrization for test data
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
source("signal_param_func.R")
load(here(rdata_dir, "mcng.RData"))

## ---------------------------

ncores <- detectCores()

###############
##
## MCNG: Test data
##
###############

# Select those wavs paths which are not null
files_wav_mcng <- mcng %>%
  filter(str_detect(path_wav, pattern = "Xevi"))

mem <- c()

for (f in files_wav_mcng$path_wav) {
  if (sum(mem) >= 5000) {
    break
  }
  wav <- readWave(f)
  mem <- append(x = mem, values = memuse(wav, prefix = "SI")@size)
  stop <- length(mem)
}

# Set the parameters to pass to signal_parametrization function
params_mcng <- data.frame(
  bottom_bp = 0.15,
  top_bp = 15,
  threshold = NA,
  mindur = 0.01,
  maxdur = 3,
  thinning = 0.5,
  ssmooth = 700,
  wl = NA,
  wn = NA,
  mar = 0.01,
  ovlp = NA,
  numcep = 12,
  nbands = NA
)

# Execute signal_parametrization function
features_mcng <- signal_parametrization(files_list = files_wav_mcng$filename_wav[1:stop],
                                        files_path = "/media/esther/TFM_Esther/Xevi/", 
                                        params = params_mcng)

# Save the features_mcng object into an .RData file
save(features_mcng, file = here(rdata_dir, "features_mcng.RData"))
dim(features_mcng)


###############
##
## Test data
##
###############

# Unite both sound.files and select variables and turn them into rownames
test <- features_mcng %>%
  unite(col = id, sound.files, selec, sep = "_") %>%
  column_to_rownames(var = "id")

# Input by variable mean in NA values
for(i in 1:ncol(test)) {
  test[is.na(test[, i]), i] <- mean(test[, i], na.rm = TRUE)
}

# Input by 0 all NA values
test[is.na(test)] <- 0

