## ---------------------------
##
## Script name: split_data.R
##
## Purpose of script: Split data into train and test datasets
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
load(here(rdata_dir, "xenocanto.RData"))
load(here(rdata_dir, "warblrb10k.RData"))

## ---------------------------

###############
##
## Train and test data 
##
###############

# Combine both xenocanto and warblrb10k in one dataframe
all_data <- rbind(xenocanto %>% select(names(warblrb10k)), warblrb10k) %>% 
  na.omit()

str(all_data)

# Stratified sampling: train (80%) and test (20%)
# Unite both sound.files and select variables and turn them into rownames
set.seed(999)
ss <- sample(1:2, size = nrow(all_data), replace = TRUE, prob = c(0.8, 0.2))

train <- all_data[ss == 1, ]
test <- all_data[ss == 2, ] 

# Copy train wavs to train_dir
file.copy(from = train$path_wav,
          to = train_dir)

# Copy test wavs to test_dir
file.copy(from = test$path_wav,
          to = test_dir)

# See the train files total duration (in hours)
sound_lengths <- c()

for (f in train$path_wav) {
  wav <- readWave(f)
  sound_length <- length(wav@left) / wav@samp.rate
  sound_lengths <- append(x = sound_lengths, values = sound_length)
}

sum(sound_lengths) / 3600

# See the test files total duration (in hours)
sound_lengths <- c()

for (f in test$path_wav) {
  wav <- readWave(f)
  sound_length <- length(wav@left) / wav@samp.rate
  sound_lengths <- append(x = sound_lengths, values = sound_length)
}

sum(sound_lengths) / 3600

