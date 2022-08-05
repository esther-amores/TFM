## ---------------------------
##
## Script name: signal_param_func.R
##
## Purpose of script: Signal parametrization function
##
## Author: Esther Amores Gago
##
## Copyright (c) Esther Amores Gago, 2022
##
## ---------------------------
##
## Notes: This function is ran in preprocessing.R
##
##
## ---------------------------

signal_parametrization <- function(files_list, files_path, params, signals = NULL, ncores = detectCores()) {
  
  # Run auto_detec() on all the recordings
  if (is.null(signals)) {
    signals <- auto_detec(
      flist = files_list,
      wl = ifelse(is.na(params$wl), 512, params$wl),
      bp = c(
        ifelse(is.na(params$bottom_bp), NULL, params$bottom_bp),
        ifelse(is.na(params$top_bp), NULL, params$top_bp)
      ),
      threshold = ifelse(is.na(params$threshold), 15, params$threshold),
      mindur = ifelse(is.na(params$mindur), NULL, params$mindur),
      maxdur = ifelse(is.na(params$maxdur), NULL, params$maxdur),
      thinning = ifelse(is.na(params$thinning), 1, params$thinning),
      ssmooth = ifelse(is.na(params$ssmooth), 0, params$ssmooth),
      path = files_path,
      parallel = ncores
    ) 
  }
  
  # Remove the NA values in signals
  signals <- na.omit(signals)

  # Detect frequency range iteratively
  signals2 <- freq_range(
    X = signals,
    wl = ifelse(is.na(params$wl), 512, params$wl),
    wn = ifelse(is.na(params$wn), "hanning", params$wn),
    ovlp = ifelse(is.na(params$ovlp), 50, params$ovlp),
    img = FALSE,
    path = files_path,
    parallel = ncores
  ) 

  # Calculate SNR on all signals
  snr <- sig2noise(
    X = signals,
    mar = ifelse(is.na(params$mar), 0.05, params$mar),
    path = files_path,
    parallel = ncores
  ) 

  # Set as numeric snr variable
  snr$SNR <- as.numeric(snr$SNR)


  # Compute acoustic parameters for the recordings
  spectro <- spectro_analysis(
    X = signals,
    bp = c(
      ifelse(is.na(params$bottom_bp), NULL, params$bottom_bp),
      ifelse(is.na(params$top_bp), NULL, params$top_bp)
    ),
    wl = ifelse(is.na(params$wl), 512, params$wl),
    wn = ifelse(is.na(params$wn), "hanning", params$wn),
    ovlp = ifelse(is.na(params$ovlp), 50, params$ovlp),
    fast = FALSE,
    harmonicity = FALSE,
    path = files_path,
    parallel = ncores
  )
  
  spectro <- spectro %>% 
    select(-sound.files) %>% 
    mutate_if(is.character, as.numeric) %>% 
    mutate(sound.files = spectro$sound.files, .before = selec) 

  # Calculate descriptive statistics on MFCC
  mfcc <- mfcc_stats(
    X = signals,
    bp = c(
      ifelse(is.na(params$bottom_bp), NULL, params$bottom_bp),
      ifelse(is.na(params$top_bp), NULL, params$top_bp)
    ),
    wl = ifelse(is.na(params$wl), 512, params$wl),
    ovlp = ifelse(is.na(params$ovlp), 50, params$ovlp),
    numcep = ifelse(is.na(params$numcep), 25, params$numcep),
    nbands = ifelse(is.na(params$nbands), 40, params$nbands),
    path = files_path,
    parallel = ncores
  )

  # Merge all data frames
  features <- signals %>%
    left_join(signals2) %>%
    left_join(snr) %>%
    left_join(spectro) %>% 
    left_join(mfcc)

  return(features)

}