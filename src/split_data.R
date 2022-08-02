## ---------------------------
##
## Script name: split_data.R
##
## Purpose of script: Split data into train and validation datasets
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
## Train and validation data 
##
###############

# Combine both xenocanto and warblrb10k in one dataframe
all_data <- rbind(xenocanto %>% select(names(warblrb10k)), warblrb10k) %>% 
  na.omit()

str(all_data)

# Stratified sampling: train (80%) and valid (20%)
# Unite both sound.files and select variables and turn them into rownames
set.seed(999)
ss <- sample(1:2, size = nrow(all_data), replace = TRUE, prob = c(0.8, 0.2))

train <- all_data[ss == 1, ] 
  # unite(col = id, sound.files, selec, sep = "_") %>% 
  # column_to_rownames(var = "id") 

valid <- all_data[ss == 2, ] 
  # unite(col = id, sound.files, selec, sep = "_") %>% 
  # column_to_rownames(var = "id") 

# Copy train wavs to train_dir
file.copy(from = train$path_wav,
          to = train_dir)

# Copy valid wavs to valid_dir
file.copy(from = valid$path_wav,
          to = valid_dir)


# ###############
# ##
# ## Test data 
# ##
# ###############
# 
# # Check if both .RData objects have the same variables
# # identical(names(xenocanto), names(features_mcng))
# 
# # Unite both sound.files and select variables and turn them into rownames
# test <- features_mcng %>%
#   unite(col = id, sound.files, selec, sep = "_") %>% 
#   column_to_rownames(var = "id")
# 
# # Input by variable mean in NA values
# for(i in 1:ncol(test)) {
#   test[is.na(test[, i]), i] <- mean(test[, i], na.rm = TRUE)
# }
# 
# # Input by 0 all NA values
# # test[is.na(test)] <- 0

  
