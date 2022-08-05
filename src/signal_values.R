## ---------------------------
##
## Script name: signal_values.R
##
## Purpose of script: Signal detection for some .wav files to extract best setting values
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

## ---------------------------

###############
##
## xeno-canto: Bird sounds (hasbird == 1)
##             Train and validation data
##
###############

# Select one call file and one song file per each specie
set.seed(999)
files_wav_sub_xenocanto <- xenocanto %>% 
  mutate_at(.vars = vars(Vocalization_type), .funs = str_to_lower) %>% 
  unnest_tokens(output = Vocalization_type, input = Vocalization_type) %>% 
  filter(!is.na(path_wav),
         str_detect(Vocalization_type, pattern = "(^call$|^song$)")) %>% 
  group_by(Specie, Vocalization_type) %>% 
  slice_sample(n = 1) %>% 
  ungroup()

# Run auto_detec() on both song and call files per each specie since we are just playing
# around with argument settings until we are satisfied with the detection.
auto_detec(
  flist = files_wav_sub_xenocanto %>%
    filter(Specie == "Lullula arborea") %>%
    select(filename_wav) %>%
    pull(),
  wl = 512,
  bp = c(0, 10),
  threshold = 15,
  mindur = 0.01,
  maxdur = 1,
  thinning = 0.8,
  ssmooth = 800,
  path = xc_wav_dir,
  parallel = ncores
) %>%
  na.omit() %>%
  full_spectrograms(path = xc_wav_dir,
                    dest.path = spec_dir,
                    parallel = ncores)

# Combine .jpeg files to a single pdf file
full_spectrogram2pdf(overwrite = TRUE,
                     path = spec_dir,
                     parallel = ncores)

# Remove .jpeg files
file.remove(list.files(
  path = spec_dir,
  pattern = ".jpeg",
  full.names = TRUE
))