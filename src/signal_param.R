## ---------------------------
##
## Script name: signal_param.R
##
## Purpose of script: Signal parametrization for all train and test data
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

source("split_data.R")
source("signal_param_func.R")

## ---------------------------

ncores <- detectCores()

###############
##
## Train data
##
###############

# Read the species.txt file
species <- fread(file = here(processed_dir, "species.txt"), 
                 sep = "\n", header = FALSE) %>%
  pull()

# The definitive parameters of the audios extracted from the 14 species of 
# xeno-canto birds can be found in the file values.csv
values <- fread(file = here(processed_dir, "values.csv"), 
                sep = ",", header = TRUE)

# Now, we select those wavs paths which are not null
files_wav_train <- train %>%
  filter(!is.na(path_wav))

# Now we can run auto_detec() on all the recordings considering
# the specific parameters for each specie.
signals_train <- lapply(species, function(x) {
  val <- values %>%
    filter(Specie == x)
  
  auto_detec(
    flist = files_wav_train %>%
      filter(Specie == x) %>%
      select(filename_wav) %>%
      pull(),
    wl = 512,
    bp = c(val$bottom_freq, val$top_freq),
    threshold = val$threshold,
    mindur = val$mindur,
    maxdur = val$maxdur,
    thinning = val$thinning,
    ssmooth = val$ssmooth,
    path = train_dir,
    parallel = ncores
  ) 
})

# Make one dataframe from a list of many
signals_train <- rbindlist(signals_train) 

save(signals_train, file = here(rdata_dir, "signals_train.RData"))

# Remove the NA values in signals_train
signals_train <- na.omit(signals_train)

# Detect frequency range iteratively
signals2_train <- freq_range(
  X = signals_train,
  wl = 512,
  wn = "hanning",
  ovlp = 50,
  img = FALSE,
  path = train_dir,
  parallel = ncores
)

# Use signal-to-noise ratio (SNR) to filter automatically selected signals
set.seed(999)
signals_sub2_train <- signals_train %>% 
  group_by(Specie = str_replace(
    string = str_extract(sound.files, pattern = "[:alpha:]+_[:alpha:]+"),
    pattern = "\\_",
    replacement = " "
  )
  )%>%
  group_by(Specie) %>% 
  slice_sample(n = 5) %>% 
  ungroup()

# Calculate SNR on subset of signals since we are just playing
# around with argument settings until we are satisfied with the margins
# for measuring noise
snr_spectrograms(
  X = signals_sub2_train,
  flim = c(0.15, 15),
  wl = 512,
  ovlp = 50,
  snrmar = 0.01,
  wn = "hanning",
  it = "jpeg",
  title = TRUE,
  path = train_dir,
  parallel = ncores
)

# As these images are saved automatically in train_dir directory,
# we move the files from train_dir to snr_spec_dir
move_imgs(
  from = train_dir,
  to = snr_spec_dir,
  it = "jpeg",
  cut = TRUE,
  parallel = ncores
)

# Now we can calculate SNR on all signals
snr_train <- sig2noise(
  X = signals_train, 
  mar = 0.01,
  path = train_dir,
  parallel = ncores
)

# Set as numeric snr variable
snr_train$SNR <- as.numeric(snr_train$SNR)

# Compute acoustic parameters for the recordings
spectro_train <- spectro_analysis(
  X = signals_train,
  bp = c(0.15, 15),
  wl = 512,
  fast = FALSE,
  ovlp = 50,
  wn = "hanning",
  harmonicity = FALSE,
  path = train_dir,
  parallel = ncores
) 

spectro_train <- spectro_train %>% 
  select(-sound.files) %>% 
  mutate_if(is.character, as.numeric) %>% 
  mutate(sound.files = spectro_train$sound.files, .before = selec) 

# Calculate descriptive statistics on MFCC
mfcc_train <- mfcc_stats(
  X = signals_train,
  ovlp = 50,
  wl = 512,
  bp = c(0.15, 15),
  numcep = 12,
  path = train_dir,
  parallel = ncores
)

# Merge all data frames
features_train <- signals_train %>%
  left_join(signals2_train) %>%
  left_join(snr_train) %>%
  left_join(spectro_train) %>%
  left_join(mfcc_train) 

features_train <- features_train %>%
  left_join(train, by = c("sound.files" = "filename_wav")) %>%
  select(-path_wav) %>% 
  unite(col = id, sound.files, selec, sep = "_") %>% 
  column_to_rownames(var = "id")

# Save the features_train object into an .RData file
save(features_train, file = here(rdata_dir, "features_train.RData"))
dim(features_train)

# See how many rows have at least one NA
sum(apply(features_test, 1, function(x) sum(is.na(x))) != 0)


###############
##
## Test data
##
###############

# Now, we select those wavs paths which are not null
files_wav_test <- test %>%
  filter(!is.na(path_wav))

# Now we can run auto_detec() on all the recordings considering
# the specific parameters for each specie.
signals_test <- lapply(species, function(x) {
  val <- values %>%
    filter(Specie == x)
  
  auto_detec(
    flist = files_wav_test %>%
      filter(Specie == x) %>%
      select(filename_wav) %>%
      pull(),
    wl = 512,
    bp = c(val$bottom_freq, val$top_freq),
    threshold = val$threshold,
    mindur = val$mindur,
    maxdur = val$maxdur,
    thinning = val$thinning,
    ssmooth = val$ssmooth,
    path = test_dir,
    parallel = ncores
  ) 
})

# Make one dataframe from a list of many
signals_test <- rbindlist(signals_test) 

save(signals_test, file = here(rdata_dir, "signals_test.RData"))

# Set the parameters to pass to signal_parametrization function
params_test <- data.frame(
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
features_test <- signal_parametrization(files_list = test$filename_wav,
                                         files_path = test_dir,
                                         signals = signals_test,
                                         params = params_test)

features_test <- features_test %>%
  left_join(test, by = c("sound.files" = "filename_wav")) %>%
  select(-path_wav) %>% 
  unite(col = id, sound.files, selec, sep = "_") %>%
  column_to_rownames(var = "id")

# Save the features_test object into an .RData file
save(features_test, file = here(rdata_dir, "features_test.RData"))
dim(features_test)

# See how many rows have at least one NA
sum(apply(features_test, 1, function(x) sum(is.na(x))) != 0)

