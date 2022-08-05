## ---------------------------
##
## Script name: spectrograms.R
##
## Purpose of script: Create dynamic and non-dynamic spectrograms
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

## ---------------------------

# files <- features_train %>% 
#   mutate(filename_wav = str_extract(rownames(.), pattern = ".*[wW][aA][vV]"),
#          path_wav = paste0(train_dir, "/", filename_wav),
#          selec = as.numeric(str_extract(rownames(.), pattern = "[0-9]*$"))) %>% 
#   na.omit() %>%
#   group_by(Specie) %>% 
#   filter(SNR == max(SNR)) %>%
#   ungroup() 

files1 <- features_train %>% 
  mutate(filename_wav = str_extract(rownames(.), pattern = ".*[wW][aA][vV]"),
         path_wav = paste0(train_dir, "/", filename_wav),
         selec = as.numeric(str_extract(rownames(.), pattern = "[0-9]*$"))) %>% 
  na.omit() 

files2 <- xenocanto %>% 
  mutate_at(.vars = vars(Vocalization_type), .funs = str_to_lower) %>% 
  unnest_tokens(output = Vocalization_type, input = Vocalization_type) %>% 
  group_by(Specie = paste(Genus, Specific_epithet)) %>% 
  select(filename_wav, Vocalization_type)

set.seed(999)
files <- files1 %>% 
  left_join(files2) %>% 
  filter(Vocalization_type == "song" | Vocalization_type == "call") %>% 
  group_by(Specie, Vocalization_type) %>% 
  slice_sample(n = 1) %>% 
  ungroup()


for (x in 1:nrow(files)) { 
  wav <- readWave(filename = files$path_wav[x], 
                  from = files$start[x] - 3, 
                  to = files$end[x] + 3, 
                  units = "seconds")

  scrolling_spectro(wave = wav,
                    file.name = here(spec_clean_dir, sprintf("%s_%d_%s.mp4", files$filename_wav[x], files$selec[x], files$Vocalization_type[x])),
                    flim = c(0, ceiling(files$top.freq[x]) + 3),
                    pal = viridis, 
                    grid = FALSE,
                    width = 1000,
                    height = 500, 
                    lower.spectro = TRUE,
                    osc = TRUE,
                    colwave = "#31688E99",
                    play = FALSE, 
                    parallel = 4)  
  
  png(
    filename = here(spec_clean_dir, sprintf("%s_%d_%s.png", files$filename_wav[x], files$selec[x], files$Vocalization_type[x])),
    res = 300,
    width = 20,
    height = 15,
    units = "cm"
  )
  spectro(wav,
          flim = c(0, ceiling(files$top.freq[x]) + 3))
  dev.off()
}

