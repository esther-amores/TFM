## ---------------------------
##
## Script name: pca.R
##
## Purpose of script: Principal Component Analysis
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
load(here(rdata_dir, "features_test.RData"))

## ---------------------------

# Input by variable mean in NA values according the hasbird group in features_train dataset
rownames_features_train <- rownames(features_train)
features_train <- features_train %>%
  group_by(hasbird) %>%
  mutate_if(is.numeric, function(x) { ifelse(is.na(x), mean(x, na.rm = TRUE), x) }) %>% 
  as.data.frame()
rownames(features_train) <- rownames_features_train

# Input by variable mean in NA values according the hasbird group in features_test dataset
rownames_features_test <- rownames(features_test)
features_test <- features_test %>%
  group_by(hasbird) %>%
  mutate_if(is.numeric, function(x) { ifelse(is.na(x), mean(x, na.rm = TRUE), x) }) %>% 
  as.data.frame()
rownames(features_test) <- rownames_features_test


###############
##
## PCA
##
###############

# Perform a Principal Component Analysis 
pca <- prcomp(features_train %>% select(-c(Specie, hasbird)), center = TRUE, scale = TRUE)
summary(pca)

# Percentage of explained variances
# (the same as variance.percent in get_eigenvalue())
pca$sdev ^ 2 / sum(pca$sdev ^ 2)

# Cumulative percentage of explained variances
# (the same as cumulative.variance.percent in get_eigenvalue())
cumsum(pca$sdev ^ 2 / sum(pca$sdev ^ 2))
eig.val <- get_eigenvalue(pca)
sel_pc <- eig.val[eig.val$cumulative.variance.percent < 71, ]

fviz_eig(pca, choice = "variance", addlabels = TRUE, ylim = c(0, 20), ncp = nrow(sel_pc)) +
  labs(title = NULL) +
  theme_pubclean()

ggsave(
  filename = here(plots_dir, "eigenvalues.png"),
  dpi = 1000,
  width = 15,
  height = 10,
  units = "cm"
)
dev.off()

# Graph of variables
var <- get_pca_var(pca)

# Variable correlation plot
head(var$coord)

fviz_pca_var(
  pca, 
  col.var = "black",
  repel = TRUE) +
  theme_pubclean()

# Quality of representation
head(var$cos2)

png(
  filename = here(plots_dir, "corrplot_cos2.png"),
  res = 300,
  width = 20,
  height = 15,
  units = "cm"
)

corrplot(
  var$cos2[, 1:nrow(sel_pc)],
  type = "upper",
  tl.col = "black",
  tl.srt = 45,
  col = brewer.pal(n = 8, name = "RdYlBu"),
  # addCoef.col = "black",
  number.cex = 0.6,
  diag = TRUE,
  is.corr = FALSE
)

dev.off()

# Visualize the quality of representation of rows/columns to PC1 and PC2
fviz_cos2(pca, choice = "var", axes = 1:2) +
  theme_pubclean() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5))

ggsave(
  filename = here(plots_dir, "quality_cos2.png"),
  dpi = 1000,
  width = 15,
  height = 10,
  units = "cm"
)

dev.off()

# Color by cos2 values: quality on the factor map
fviz_pca_var(
  pca,
  col.var = "cos2",
  gradient.cols = brewer.pal(n = 8, name = "RdYlBu"),
  repel = TRUE # Avoid text overlapping
)

ggsave(
  filename = here(plots_dir, "pca_quality_cos2.png"),
  dpi = 1000,
  width = 15,
  height = 10,
  units = "cm"
)

dev.off()

# Contributions to variables to PCs
head(var$contrib, 4)

png(
  filename = here(plots_dir, "corrplot_contrib.png"),
  res = 300,
  width = 20,
  height = 15,
  units = "cm"
)

corrplot(
  var$contrib[, 1:nrow(sel_pc)],
  type = "upper",
  tl.col = "black",
  tl.srt = 45,
  col = brewer.pal(n = 8, name = "RdYlBu"),
  # addCoef.col = "black",
  number.cex = 0.6,
  diag = TRUE,
  is.corr = FALSE
)

dev.off()

# Visualize the contributions of variables to PC1
fviz_contrib(pca, choice = "var", axes = 1, top = 20) +
  theme_pubclean() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5))

# Visualize the contributions of variables to PC2
fviz_contrib(pca, choice = "var", axes = 2, top = 20) +
  theme_pubclean() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5))

# Visualize the total contributions of variables to PC1 and PC2
fviz_contrib(pca, choice = "var", axes = 1:2) +
  theme_pubclean() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5))

ggsave(
  filename = here(plots_dir, "quality_contrib.png"),
  dpi = 1000,
  width = 15,
  height = 10,
  units = "cm"
)

dev.off()

# Color by contributions values: contributions on the factor map
fviz_pca_var(
  pca,
  col.var = "contrib",
  gradient.cols = brewer.pal(n = 8, name = "RdYlBu"),
  repel = TRUE # Avoid text overlapping
)

ggsave(
  filename = here(plots_dir, "pca_quality_contrib.png"),
  dpi = 1000,
  width = 15,
  height = 10,
  units = "cm"
)

dev.off()

# Make predictions with features_train dataset
features_train_pca <- predict(pca, newdata = features_train) %>% 
  as.data.frame() %>% 
  select(1:nrow(sel_pc)) %>% 
  mutate(hasbird = features_train$hasbird,
         Specie = features_train$Specie) 

# Save the features_train_pca object into an .RData file
save(features_train_pca, file = here(rdata_dir, "features_train_pca.RData"))

# Make predictions with features_test dataset
features_test_pca <- predict(pca, newdata = features_test) %>% 
  as.data.frame() %>% 
  select(1:nrow(sel_pc)) %>% 
  mutate(hasbird = features_test$hasbird,
         Specie = features_test$Specie) 

# Save the features_test_pca object into an .RData file
save(features_test_pca, file = here(rdata_dir, "features_test_pca.RData"))

