## ---------------------------
##
## Script name: loading_data.R
##
## Purpose of script: Data acquisition
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

## ---------------------------

ncores <- detectCores()

###############
##
## xeno-canto: Train and test data
##
###############

# Read the species.txt file
species <- fread(file = here(processed_dir, "species.txt"), 
                      sep = "\n", header = FALSE) %>%
  pull()

# Function for access recordings and metadata from xeno-canto for each specie
xeno <- function(x) {
  query_xc(qword = x, download = FALSE, path = xc_mp3_dir, parallel = ncores)
}

# Apply xeno function to all species
meta <- sapply(species, xeno)

# Make one dataframe from a list of many
meta <- rbindlist(meta, fill = TRUE)
dim(meta)

# See the frequencies distribution by specie
meta %>% 
  mutate_at(.vars = vars(Date, Uploaded), .funs = list(as.Date)) %>% 
  filter(Uploaded < "2022-07-01",
         Quality %in% c("A", "B")) %>% 
  group_by(Specie = paste(Genus, Specific_epithet)) %>% 
  count(Specie, sort = TRUE)

# Filter files uploaded before July 1st, 2022
# Filter files with quality A or B
# Subset 80 files randomly
set.seed(999)
metadata <- meta %>% 
  mutate_at(.vars = vars(Date, Uploaded), .funs = list(as.Date)) %>% 
  filter(Uploaded < "2022-07-01",
         Quality %in% c("A", "B")) %>% 
  group_by(Specie = paste(Genus, Specific_epithet)) %>% 
  slice_sample(n = 80) %>% 
  ungroup()

dim(metadata)

# Download recordings and metadata from xeno-canto for all species
query_xc(X = metadata, download = TRUE, path = xc_mp3_dir, parallel = ncores) 

# Get list .mp3 files local path
file_list_mp3 <- data.frame(path_mp3 = list.files(path = xc_mp3_dir, 
                                                  pattern = "[mM][pP]3$",
                                                  full.names = TRUE), 
                            stringsAsFactors = FALSE) %>%
  mutate(filename_mp3 = basename(path_mp3), 
         split_name = gsub(".mp3", "", filename_mp3), 
         filename_mp3_new = gsub("-", "_", filename_mp3), 
         path_mp3_new = file.path(dirname(path_mp3), filename_mp3_new))

# Rename the sound files by removing the "-" and replacing with "_".
file.rename(file_list_mp3$path_mp3, file_list_mp3$path_mp3_new)

# Parse name
file_list_mp3 <- separate(file_list_mp3, 
                          col = split_name, 
                          into = c("Genus", "Specific_epithet", "Recording_ID"), 
                          sep = "_", remove = TRUE)

# Join with the metadata from xeno-canto
metadata <- metadata %>%
  left_join(file_list_mp3, by = c("Recording_ID", "Genus", "Specific_epithet"))


###############
##
## Python
##
###############

# Convert .mp3 files to .wav by means of mp3_to_wav.py 
py_config()
py_run_file("mp3_to_wav.py")


###############
##
## xeno-canto: Train and test data
##
###############

# Get list .wav files from local path
file_list_wav <- data.frame(path_wav = list.files(path = xc_wav_dir, 
                                                  pattern = "[wW][aA][vV]$",
                                                  full.names = TRUE),
                            stringsAsFactors = FALSE) %>%
  mutate(filename_wav = basename(path_wav),
         split_name = gsub(".wav", "", filename_wav))

file_list_wav <- separate(file_list_wav, col = split_name, 
                          into = c("Genus", "Specific_epithet", "Recording_ID"), 
                          sep = "_", remove = TRUE)

# Merge metadata dataframe with .wav files that have been transformed
xenocanto <- metadata %>%
  left_join(file_list_wav, by = c("Recording_ID", "Genus", "Specific_epithet")) %>% 
  mutate(Specie = factor(x = paste(Genus, Specific_epithet), levels = c("None", species)),
         hasbird = factor(x = 1, levels = c(0, 1))) %>% 
  select(-c(filename_mp3, filename_mp3_new, path_mp3, path_mp3_new))

# Save the xenocanto object into an .RData file 
save(xenocanto, file = here(rdata_dir, "xenocanto.RData"))
dim(xenocanto)

# Save the xenocanto object as a html table
xenocanto %>% 
  kable(digits = 2, format = "html", row.names = TRUE) %>%
  kable_styling(bootstrap_options = c("striped"),
                full_width = TRUE,
                font_size = 15) %>%
  scroll_box() %>% 
  kable_paper() %>% 
  save_kable(file = here(tables_dir, "xenocanto.html"))


###############
##
## warblrb10k: Train and test data
##
###############

warblrb10k <- fread(file = here(warblrb10k_dir, "warblrb10k_public_metadata_2018.csv"), drop = "datasetid") %>% 
  mutate_at(.vars = vars(itemid), .funs = list(~ paste0(., ".wav"))) %>% 
  left_join(
    data.frame(
      filename_wav = list.files(
        path = here(warblrb10k_dir, "wav"),
        pattern = "[wW][aA][vV]$",
        full.names = FALSE
      ),
      path_wav = list.files(
        path = here(warblrb10k_dir, "wav"),
        pattern = "[wW][aA][vV]$",
        full.names = TRUE
      )
    ),
    by = c("itemid" = "filename_wav")
  ) %>% 
  rename(filename_wav = itemid) %>% 
  filter(hasbird == 0) %>% 
  mutate(Specie = factor(x = "None", levels = c("None", species)),
         hasbird = factor(x = 0, levels = c(0, 1))) 

# Save the warblrb10k object into an .RData file 
save(warblrb10k, file = here(rdata_dir, "warblrb10k.RData"))
dim(warblrb10k)

# Save the warblrb10k object as a html table
warblrb10k %>% 
  kable(digits = 2, format = "html", row.names = TRUE) %>%
  kable_styling(bootstrap_options = c("striped"),
                full_width = TRUE,
                font_size = 15) %>%
  scroll_box() %>% 
  save_kable(file = here(tables_dir, "warblrb10k.html"))
