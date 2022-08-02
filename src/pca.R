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

source("split_data.R")
load(here(rdata_dir, "features_train.RData"))
load(here(rdata_dir, "features_valid.RData"))

## ---------------------------

# Input by variable mean in NA values according the hasbird group in features_train dataset
rownames_features_train <- rownames(features_train)
features_train <- features_train %>%
  group_by(hasbird) %>%
  mutate_if(is.numeric, function(x) { ifelse(is.na(x), mean(x, na.rm = TRUE), x) }) %>% 
  as.data.frame()
rownames(features_train) <- rownames_features_train

# Input by variable mean in NA values according the hasbird group in features_valid dataset
rownames_features_valid <- rownames(features_valid)
features_valid <- features_valid %>%
  group_by(hasbird) %>%
  mutate_if(is.numeric, function(x) { ifelse(is.na(x), mean(x, na.rm = TRUE), x) }) %>% 
  as.data.frame()
rownames(features_valid) <- rownames_features_valid

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

fviz_eig(pca, choice = "variance", addlabels = TRUE, ylim = c(0, 30), ncp = nrow(sel_pc)) +
  labs(title = "Scree plot for PCA") +
  theme_pubclean()

ggsave(
  filename = here(plots_dir, "screeplot.png"),
  dpi = 1000,
  width = 15,
  height = 10,
  units = "cm"
)

# Graph of variables
var <- get_pca_var(pca)

# Variable correlation plot
head(var$coord)

fviz_pca_var(pca, col.var = "black") +
  theme_pubclean()

# Quality of representation
head(var$cos2)

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

# Visualize the quality of representation of rows/columns
a <- fviz_cos2(pca, choice = "var", axes = 1:2) +
  theme_pubclean() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5))

# Color by cos2 values: quality on the factor map
fviz_pca_var(
  pca,
  col.var = "cos2",
  gradient.cols = brewer.pal(n = 8, name = "RdYlBu"),
  repel = TRUE # Avoid text overlapping
)

# Contributions to variables to PCs
head(var$contrib, 4)

corrplot(
  var$contrib[, 1:14],
  type = "upper",
  tl.col = "black",
  tl.srt = 45,
  col = brewer.pal(n = 8, name = "RdYlBu"),
  # addCoef.col = "black",
  number.cex = 0.6,
  diag = TRUE,
  is.corr = FALSE
)

# Visualize the contributions of variables to PC1
fviz_contrib(pca, choice = "var", axes = 1, top = 20) +
  theme_pubclean() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5))

# Visualize the contributions of variables to PC2
fviz_contrib(pca, choice = "var", axes = 2, top = 20) +
  theme_pubclean() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5))

# Visualize the total contributions of variables to PC1 and PC2
b <- fviz_contrib(pca, choice = "var", axes = 1:2) +
  theme_pubclean() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5))

# Color by contributions values: contributions on the factor map
fviz_pca_var(
  pca,
  col.var = "contrib",
  gradient.cols = brewer.pal(n = 8, name = "RdYlBu"),
  repel = TRUE # Avoid text overlapping
)

# Arrange both a and b plots
ggarrange(a, b)


# Make predictions with features_train dataset
features_train_pca <- predict(pca, newdata = features_train) %>% 
  as.data.frame() %>% 
  select(1:nrow(sel_pc)) %>% 
  mutate(hasbird = features_train$hasbird,
         Specie = features_train$Specie) 

# Save the features_train_pca object into an .RData file
save(features_train_pca, file = here(rdata_dir, "features_train_pca.RData"))

# Make predictions with features_valid dataset
features_valid_pca <- predict(pca, newdata = features_valid) %>% 
  as.data.frame() %>% 
  select(1:nrow(sel_pc)) %>% 
  mutate(hasbird = features_valid$hasbird,
         Specie = features_valid$Specie) 

# Save the features_valid_pca object into an .RData file
save(features_valid_pca, file = here(rdata_dir, "features_valid_pca.RData"))

