# https://epurdom.github.io/Stat131A/lectures/2018SpringLectures/04MultiVisual_forClassCode.html


library(tidytext)

vocal <- xenocanto %>% 
  mutate_at(.vars = vars(Vocalization_type), .funs = str_to_lower) %>% 
  unnest_tokens(output = Vocalization_type, input = Vocalization_type) %>% 
  group_by(Specie = paste(Genus, Specific_epithet)) %>% 
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

vocal$data %>% 
  filter(Vocalization_type == "call")

vocal$data %>% 
  filter(Vocalization_type == "song")

####################################
library(GGally)
pdf(file = here(plots_dir, "pairs_plot.pdf"))
pairs_plot <- ggpairs(train, aes(color = hasbird))
pairs_plot
dev.off()

library(gpairs)
pdf(file = here(plots_dir, "corr_plot.pdf"))
suppressWarnings(corrgram(train))
dev.off()
####################################


crisp_dm <- fread(file = here(processed_dir, "crisp_dm.csv"), 
                  sep = ",", header = TRUE) %>%  
  filter(wp != "Deployment") %>% 
  mutate_at(.vars = vars(start_date, end_date), .funs = ~ as.Date(., "%d/%m/%Y")) %>% 
  mutate_at(.vars = vars(wp, resources), .funs = as.factor)

# remotes::install_github("giocomai/ganttrify")

library(ganttrify)

names(crisp_dm) <- c("activity", "wp", "start_date", "end_date", "resources", "risks", "cost")
# names(crisp_dm) <- c("Task", "Phase", "Start", "End", "Resouces", "Risks", "Cost")

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

##################
library(igraph)

packages <- fread(file = here(processed_dir, "packages.csv"), 
                  sep = ",", header = TRUE) 

packages_names <- unique(c(unique(packages$from), unique(packages$to)))
packages_graph <- graph_from_data_frame(d = packages, vertices = packages_names, directed = TRUE)
plot(packages_graph, layout = layout_in_circle, main = "Circular")

ggsave(
  filename = here(plots_dir, "packages.png"),
  dpi = 1000,
  width = 20,
  height = 15,
  units = "cm"
)