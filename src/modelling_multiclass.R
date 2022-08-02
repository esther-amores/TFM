## ---------------------------
##
## Script name: modelling_multiclass.R
##
## Purpose of script: 
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
source("performance_measures_func.R")
load(here(rdata_dir, "features_train.RData"))
load(here(rdata_dir, "features_valid.RData"))
load(here(rdata_dir, "features_train_pca.RData"))
load(here(rdata_dir, "features_valid_pca.RData"))


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
## Multinomial logistic regression
##
###############

# Fit a multinomial logistic regression model
time_mod_multinom <- system.time({
  mod_multinom <- multinom(Specie ~ ., data = features_train_pca %>% select(-hasbird), family = "multinomial", maxit = 1000)
})

# Summarize the multinomial logistic regression model
smod_multinom <- summary(mod_multinom)
smod_multinom

# Predict the fitted model with the features_train_pca dataset
train_pred_multinom <- predict(mod_multinom, newdata = features_train_pca %>% select(-hasbird), type = "class")

# Accuracy on features_train_pca dataset
mean(train_pred_multinom == features_train_pca$Specie)

# Predict the fitted model with the features_valid_pca dataset
valid_pred_multinom <- predict(mod_multinom, newdata = features_valid_pca %>% select(-hasbird), type = "class")

# Accuracy on features_valid_pca dataset
mean(valid_pred_multinom == features_valid_pca$Specie)

# Compute all classification metrics on valid predictions
metrics(predicted = valid_pred_multinom, actual = features_valid_pca$Specie, multiclass = TRUE)

# AUC
multiclass.roc(response = features_valid_pca$Specie, 
               predictor = predict(mod_multinom, newdata = features_valid_pca %>% select(-hasbird), type = "prob"))$auc[1]


###############
##
## SVM (linear kernel)
##
###############

time_mod_svm <- system.time({
  mod_svm <- svm(Specie ~ ., 
                 data = features_train_pca %>% select(-hasbird),
                 kernel = "linear",
                 type = "C-classification",
                 cost = 1,
                 scale = TRUE)
})

# Predict the fitted model with the features_train_pca dataset
train_pred_svm <- predict(mod_svm, newdata = features_train_pca %>% select(-hasbird), type = "response")

# Accuracy on features_train_pca dataset
mean(train_pred_svm == features_train_pca$Specie)

# Predict the fitted model with the features_valid_pca dataset
valid_pred_svm <- predict(mod_svm, newdata = features_valid_pca %>% select(-hasbird), type = "response")

# Accuracy on features_valid_pca dataset
mean(valid_pred_svm == features_valid_pca$Specie)

# Compute all classification metrics on valid predictions
metrics(predicted = valid_pred_svm, actual = features_valid_pca$Specie, multiclass = TRUE)


###############
##
## SVM (radial kernel)
##
###############

time_mod_svm2 <- system.time({
  mod_svm2 <- svm(Specie ~ ., 
                  data = features_train_pca %>% select(-hasbird),
                  kernel = "radial",
                  type = "C-classification",
                  cost = 1,
                  gamma = 1,
                  scale = TRUE)
})

# Predict the fitted model with the features_train_pca dataset
train_pred_svm2 <- predict(mod_svm2, newdata = features_train_pca %>% select(-hasbird), type = "response")

# Accuracy on features_train_pca dataset
mean(train_pred_svm2 == features_train_pca$Specie)

# Predict the fitted model with the features_valid_pca dataset
valid_pred_svm2 <- predict(mod_svm2, newdata = features_valid_pca %>% select(-hasbird), type = "response")

# Accuracy on features_valid_pca dataset
mean(valid_pred_svm2 == features_valid_pca$Specie)

# Compute all classification metrics on valid predictions
metrics(predicted = valid_pred_svm2, actual = features_valid_pca$Specie, multiclass = TRUE)


###############
##
## SVM cross-validation (radial kernel)
##
###############

time_mod_svm_cv <- system.time({
  mod_svm_cv <- tune("svm", 
                     Specie ~ ., 
                     data = features_train_pca %>% select(-hasbird),
                     kernel = "radial",
                     type = "C-classification",
                     ranges = list(
                       cost = 10 ^ (-3:1),
                       gamma = 10 ^ (-5:-1)
                     ),
                     tunecontrol = tune.control(cross = 5),
                     scale = TRUE)
})

# Save the mod_svm_cv object into an .RData file
# save(mod_svm_cv, file = here(rdata_dir, "mod_svm_cv.RData"))
dim(mod_svm_cv)

# mod_svm_cv summary
summary(mod_svm_cv)

# Plot the decision boundary in SVM
plot(mod_svm_cv, data = features_train_pca)

ggplot(data = mod_svm_cv$performances, aes(x = cost, y = error)) +
  geom_line() +
  geom_point() +
  labs(title = "Classification error vs hyperparameter C") +
  theme_pubclean()

# Save the best model
mod_svm3 <- mod_svm_cv$best.model

# Predict the fitted model with the features_train_pca dataset
train_pred_svm3 <- predict(mod_svm2, newdata = features_train_pca %>% select(-hasbird), type = "response")

# Accuracy on features_train_pca dataset
mean(train_pred_svm3 == features_train_pca$Specie)

# Predict the fitted model with the features_valid_pca dataset
valid_pred_svm3 <- predict(mod_svm2, newdata = features_valid_pca %>% select(-hasbird), type = "response")

# Accuracy on features_valid_pca dataset
mean(valid_pred_svm3 == features_valid_pca$Specie)

# Compute all classification metrics on valid predictions
metrics(predicted = valid_pred_svm3, actual = features_valid_pca$Specie, multiclass = TRUE)


###############
##
## Decision trees
##
###############

library(randomForest)
library(caret)
library(rpart)
library(rpart.plot)

time_mod_rpart <- system.time({
  mod_rpart <- rpart(Specie ~ ., 
                     data = features_train %>% select(-hasbird),
                     method = "class")
})

rpart.plot(mod_rpart)
printcp(mod_rpart)
plotcp(mod_rpart)

# Predict the fitted model with the features_train dataset
train_pred_rpart <- predict(mod_rpart, newdata = features_train %>% select(-hasbird), type = "class")

# Accuracy on features_train dataset
mean(train_pred_rpart == features_train$Specie)


mod_rpart$cptable %>%
  data.frame() %>%
  mutate(
    min_idx = which.min(mod_rpart$cptable[, "xerror"]),
    rownum = row_number(),
    xerror_cap = mod_rpart$cptable[min_idx, "xerror"] + mod_rpart$cptable[min_idx, "xstd"],
    eval = case_when(rownum == min_idx ~ "min xerror",
                     xerror < xerror_cap ~ "under cap",
                     TRUE ~ "")) %>%
  select(-rownum,-min_idx)


time_mod_rpart2 <- system.time({
  mod_rpart2 <- rpart(Specie ~ ., 
                     data = features_train %>% select(-hasbird),
                     method = "class",
                     cp = 0.01)
})

plotcp(mod_rpart2, upper = "splits")

mod_rpart2 <- prune(
  mod_rpart2,
  cp = mod_rpart2$cptable[mod_rpart2$cptable[, 2] == 3, "CP"]
)

rpart.plot(mod_rpart2)


# Predict the fitted model with the features_train dataset
train_pred_rpart2 <- predict(mod_rpart, newdata = features_train %>% select(-hasbird), type = "class")

# Accuracy on features_train dataset
mean(train_pred_rpart2 == features_train$Specie)

# Predict the fitted model with the features_valid dataset
valid_pred_rpart2 <- predict(mod_rpart, newdata = features_valid %>% select(-hasbird), type = "class")

# Accuracy on features_valid dataset
mean(valid_pred_rpart2 == features_valid$Specie)

# Compute all classification metrics on valid predictions
metrics(predicted = valid_pred_rpart2, actual = features_valid$Specie, multiclass = TRUE)


###############
##
## Random forest
##
###############

time_mod_rf_cv <- system.time({
  mod_rf_cv <- tune("randomForest", 
                    Specie ~ ., 
                    data = features_train %>% select(-hasbird),
                    kernel = "radial",
                    type = "C-classification",
                    ranges = list(
                      ntree = c(500, 1000, 1500, 2000),
                      mtry = 10:30),
                    tunecontrol = tune.control(cross = 5))
})

# Save the mod_rf_cv object into an .RData file
save(mod_rf_cv, file = here(rdata_dir, "mod_rf_cv.RData"))
dim(mod_rf_cv)

