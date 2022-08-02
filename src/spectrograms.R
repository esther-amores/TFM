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
load(here(rdata_dir, "features_xenocanto.RData"))
load(here(rdata_dir, "features_warblrb10k.RData"))

## ---------------------------

files <- features_xenocanto %>% 
  na.omit() %>%
  mutate(Specie = str_replace(
    string = str_extract(sound.files, pattern = "[:alpha:]+_[:alpha:]+"),
    pattern = "\\_",
    replacement = " "
    )
  )%>%
  group_by(Specie) %>% 
  filter(SNR == max(SNR)) %>% 
  ungroup() %>% 
  left_join(metadata, by = c("sound.files" = "filename_wav"))


for (x in 1:nrow(files)) { 
  wav <- readWave(filename = files$path_wav[x], 
                  from = files$start[x] - 2, 
                  to = files$end[x] + 2, 
                  units = "seconds")
  
  # sp <- ggspectro(wav) +
  #   stat_contour(geom = "polygon", aes(fill = ..level..), bins = 30) +
  #   scale_fill_continuous(low = "white", high = "black")
  # 
  # print(sp)
  
  # sp <- spectro(wav)
  # print(sp)
  
  scrolling_spectro(wave = wav,
                    file.name = here(spec_clean_dir, sprintf("%s_%d.mp4", files$sound.files[x], files$selec[x])),
                    # flim = c(max(0, floor(files$bottom.freq[x]) - 1), ceiling(files$top.freq[x]) + 1),
                    flim = c(0, ceiling(files$top.freq[x]) + 1),
                    pal = viridis, 
                    grid = FALSE,
                    width = 1000,
                    height = 500, 
                    lower.spectro = TRUE,
                    osc = TRUE,
                    colwave = "#31688E99",
                    play = FALSE, 
                    parallel = 4)  
  
  # sp <- ggspectro(wav) +
  #   geom_tile(aes(fill = amplitude)) +
  #   stat_contour()
  # 
  # print(sp)
}

