## ---------------------------
##
## Script name: figures.R
##
## Purpose of script: Generate plots and tables 
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

# Sampling rate plot
time = seq(0, 50, 0.5)
amplitude = sin(time)
data = data.frame(time, amplitude)

ggplot(data, aes(x = time, y = amplitude)) +
  geom_line() +
  geom_point(size = 1) +
  geom_vline(xintercept = time, linetype = "dotted") + 
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank())

ggsave(
  filename = here(plots_dir, "sampling_rate.png"),
  dpi = 1000,
  width = 20,
  height = 15,
  units = "cm"
)

# Sampling rate aliasing plot
time = seq(0, 50, 0.5)
amplitude = sin(time)
aliasing = sin(time/5)
data = data.frame(time, amplitude, aliasing)
interval = seq(0, 50, 8)

ggplot(data, aes(x = time, y = amplitude)) +
  geom_line() +
  geom_line(aes(y = aliasing), col = "blue", lwd = 1) +
  geom_vline(xintercept = interval, linetype = "dotted") + 
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank())

ggsave(
  filename = here(plots_dir, "sampling_rate_aliasing.png"),
  dpi = 1000,
  width = 20,
  height = 15,
  units = "cm"
)

# Distribution of the labels in Vocalization_type variable from xenocanto dataset
vocal <- xenocanto %>% 
  mutate_at(.vars = vars(Vocalization_type), .funs = str_to_lower) %>% 
  unnest_tokens(output = Vocalization_type, input = Vocalization_type) %>% 
  group_by(Specie) %>% 
  count(Vocalization_type, sort = TRUE) %>% 
  slice_max(n = 5, order_by = n, with_ties = FALSE) %>% 
  ungroup() %>% 
  ggplot(aes(x = n, y = fct_reorder(Vocalization_type, n), fill = Specie)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ Specie, scales = "free_y", ncol = 2) +
  labs(x = "Frequency", y = NULL) +
  theme_pubclean() +
  theme(strip.text.x = element_text(face = "italic")) 

vocal

ggsave(
  filename = here(plots_dir, "vocalization_type.png"),
  dpi = 1000,
  width = 30,
  height = 20,
  units = "cm"
)

vocal_data <- vocal$data %>% 
  filter(Vocalization_type == "song" | Vocalization_type == "call") %>% 
  pivot_wider(names_from = Vocalization_type, values_from = n)

vocal_data %>%
  xtable(sanitize.colnames.function = bold,
         booktabs = TRUE,
         floating = TRUE,
         label = "tab:vocalization_type") %>%
  print(file = here(tables_dir, "vocalization_type.tex"))



####################################
# library(GGally)
# pdf(file = here(plots_dir, "pairs_plot.pdf"))
# pairs_plot <- ggpairs(train, aes(color = hasbird))
# pairs_plot
# dev.off()
# 
# library(gpairs)
# pdf(file = here(plots_dir, "corr_plot.pdf"))
# suppressWarnings(corrgram(train))
# dev.off()
####################################

# CRISP-DM project schedule with Gantt diagram
crisp_dm <- fread(file = here(processed_dir, "crisp_dm.csv"), 
                  sep = ",", header = TRUE) %>%  
  filter(wp != "Deployment") %>% 
  mutate_at(.vars = vars(start_date, end_date), .funs = ~ as.Date(., "%d/%m/%Y")) %>% 
  mutate_at(.vars = vars(wp, resources), .funs = as.factor)

library(ganttrify) # remotes::install_github("giocomai/ganttrify")

ganttrify(project = crisp_dm,
          project_start_date = "2022-04",
          by_date = TRUE)

ggsave(
  filename = here(plots_dir, "gantt.png"),
  dpi = 1000,
  width = 20,
  height = 15,
  units = "cm"
)

# Package dependencies in a network
packages <- fread(file = here(processed_dir, "packages.csv"), 
                  sep = ",", header = TRUE) 
packages_names <- unique(c(unique(packages$from), unique(packages$to)))
packages_graph <- graph_from_data_frame(d = packages, vertices = packages_names, directed = TRUE)

png(
  filename = here(plots_dir, "packages.png"),
  res = 300,
  width = 20,
  height = 15,
  units = "cm"
)

plot(packages_graph, layout = layout_as_tree, vertex.color = adjustcolor("#3A5199", alpha.f = 0.5), 
     vertex.label.color = "black", edge.color = "gray60", label.color = "black",
     vertex.size = 20, vertex.label.cex = 0.7)

dev.off()


# Bird species distribution in xenocanto dataset
world <- map_data("world")

ggplot() +
  geom_map(data = world, 
           map = world,  
           aes(long, lat, map_id = region),
           color = "white", 
           fill = "lightgray", 
           size = 0.1) +
  geom_point(data = xenocanto, 
             aes(Longitude, Latitude, color = Specie), 
             alpha = 0.7) +
  facet_wrap(~ Specie) +
  theme_void() +
  theme(strip.text = element_text(face = "italic"),
        legend.position = "none")

ggsave(
  filename = here(plots_dir, "species_distribution.png"),
  dpi = 1000,
  width = 20,
  height = 15,
  units = "cm"
)

ggplot() +
  geom_map(data = world, 
           map = world,  
           aes(long, lat, map_id = region),
           color = "white", 
           fill = "lightgray", 
           size = 0.1) +
  geom_point(data = xenocanto, 
             aes(Longitude, Latitude, color = Specie), 
             alpha = 0.7) +
  theme_void() +
  theme(legend.text = element_text(face = "italic"),
        legend.title = element_blank()) 

ggsave(
  filename = here(plots_dir, "world_distribution.png"),
  dpi = 1000,
  width = 20,
  height = 15,
  units = "cm"
)
  
