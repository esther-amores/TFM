## ---------------------------
##
## Script name: modelling_multiclass.R
##
## Purpose of script: Multiclass classification to detect the bird species (if any)
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
load(here(rdata_dir, "features_test.RData"))
load(here(rdata_dir, "features_train_pca.RData"))
load(here(rdata_dir, "features_test_pca.RData"))

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

# Predict the fitted model with the features_test_pca dataset
test_pred_multinom <- predict(mod_multinom, newdata = features_test_pca %>% select(-hasbird), type = "class")

# Accuracy on features_test_pca dataset
mean(test_pred_multinom == features_test_pca$Specie)

# Compute all classification metrics on test predictions
metrics(predicted = test_pred_multinom, actual = features_test_pca$Specie, multiclass = TRUE)

# AUC
multiclass.roc(response = features_test_pca$Specie, 
               predictor = predict(mod_multinom, newdata = features_test_pca %>% select(-hasbird), type = "prob"))$auc[1]


###############
##
## Naive Bayes
##
###############

# Fit a Naive Bayes model
time_mod_nb <- system.time({
  mod_nb <- naiveBayes(Specie ~ ., data = features_train_pca %>% select(-hasbird))
})

# Predict the fitted model with the features_train_pca dataset
train_pred_nb <- predict(mod_nb, features_train_pca %>% select(-hasbird), type = "class") 

# Accuracy on features_train_pca dataset
mean(train_pred_nb == features_train_pca$Specie)

# Predict the fitted model with the features_test_pca dataset
test_pred_nb <- predict(mod_nb, features_test_pca %>% select(-hasbird), type = "class") 

# Accuracy on features_test_pca dataset
mean(test_pred_nb == features_test_pca$Specie)

# Compute all classification metrics on test predictions
metrics(predicted = test_pred_nb, actual = features_test_pca$Specie, multiclass = TRUE)

# AUC
multiclass.roc(response = features_test_pca$Specie, 
               predictor = predict(mod_nb, newdata = features_test_pca %>% select(-hasbird), type = "raw"))$auc[1]


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

# Save the mod_svm object into an .RData file
save(mod_svm, file = here(rdata_dir, "svm_multiclass_pca_linearkernel.RData"))
dim(mod_svm)

# Predict the fitted model with the features_train_pca dataset
train_pred_svm <- predict(mod_svm, newdata = features_train_pca %>% select(-hasbird), type = "response")

# Accuracy on features_train_pca dataset
mean(train_pred_svm == features_train_pca$Specie)

# Predict the fitted model with the features_test_pca dataset
test_pred_svm <- predict(mod_svm, newdata = features_test_pca %>% select(-hasbird), type = "response")

# Accuracy on features_test_pca dataset
mean(test_pred_svm == features_test_pca$Specie)

# Compute all classification metrics on test predictions
metrics(predicted = test_pred_svm, actual = features_test_pca$Specie, multiclass = TRUE)

# AUC
multiclass.roc(response = features_test_pca$Specie, 
               predictor = predict(mod_multinom, newdata = features_test_pca %>% select(-hasbird), type = "prob"))$auc[1]


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
                  probability = TRUE)
})

# Save the mod_svm2 object into an .RData file
save(mod_svm2, file = here(models_dir, "svm_multiclass_pca_radialkernel.RData"))
dim(mod_svm2)

# Predict the fitted model with the features_train_pca dataset
train_pred_svm2 <- predict(mod_svm2, newdata = features_train_pca %>% select(-hasbird), type = "response")

# Accuracy on features_train_pca dataset
mean(train_pred_svm2 == features_train_pca$Specie)

# Predict the fitted model with the features_test_pca dataset
test_pred_svm2 <- predict(mod_svm2, newdata = features_test_pca %>% select(-hasbird), type = "response")
test_pred_svm2_probs <- predict(mod_svm2, newdata = newdata = features_test_pca %>% select(-hasbird), probability = TRUE) %>% 
  attr("probabilities")

# Accuracy on features_test_pca dataset
mean(test_pred_svm2 == features_test_pca$Specie)

# Compute all classification metrics on test predictions
metrics(predicted = test_pred_svm2, actual = features_test_pca$Specie, multiclass = TRUE)

# Confusion matrix
ggsave(
  filename = here(plots_dir, "glm_multiclass_pca.png"),
  dpi = 1000,
  width = 20,
  height = 15,
  units = "cm"
)

# AUC
multiclass.roc(response = features_test_pca$Specie, 
               predictor = as.numeric(test_pred_svm2))$auc[1]


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
                     tunecontrol = tune.control(cross = 3),
                     scale = TRUE)
})

# Save the mod_svm_cv object into an .RData file
save(mod_svm_cv, file = here(models_dir, "svm_multiclass_pca_cv_radialkernel.RData"))

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
train_pred_svm3 <- predict(mod_svm3, newdata = features_train_pca %>% select(-hasbird), type = "response")

# Accuracy on features_train_pca dataset
mean(train_pred_svm3 == features_train_pca$Specie)

# Predict the fitted model with the features_test_pca dataset
test_pred_svm3 <- predict(mod_svm3, newdata = features_test_pca %>% select(-hasbird), type = "response")

# Accuracy on features_test_pca dataset
mean(test_pred_svm3 == features_test_pca$Specie)

# Compute all classification metrics on test predictions
metrics(predicted = test_pred_svm3, actual = features_test_pca$Specie, multiclass = TRUE)

# AUC
multiclass.roc(response = features_test_pca$Specie, 
               predictor = as.numeric(test_pred_svm3))$auc[1]


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

# Predict the fitted model with the features_test dataset
test_pred_rpart2 <- predict(mod_rpart, newdata = features_test %>% select(-hasbird), type = "class")

# Accuracy on features_test dataset
mean(test_pred_rpart2 == features_test$Specie)

# Compute all classification metrics on test predictions
metrics(predicted = test_pred_rpart2, actual = features_test$Specie, multiclass = TRUE)

# AUC
multiclass.roc(response = features_test_pca$Specie, 
               predictor = as.numeric(test_pred_rpart2))$auc[1]


###############
##
## Random forest
##
###############

time_mod_rf <- system.time({
  mod_rf <- randomForest(Specie ~ .,
                         data = features_train %>% select(-hasbird),
                         importance = TRUE)
})

# Save the mod_rf object into an .RData file
save(mod_rf, file = here(models_dir, "rf_multiclass_pca.RData"))
dim(mod_rf)

# Predict the fitted model with the features_train dataset
train_pred_rf <- predict(mod_rf, newdata = features_train %>% select(-hasbird))

# Accuracy on features_train dataset
mean(train_pred_rf == features_train$Specie)

# Predict the fitted model with the features_test dataset
test_pred_rf <- predict(mod_rf, newdata = features_test %>% select(-hasbird))

# Accuracy on features_test dataset
mean(test_pred_rf == features_test$Specie)

# Compute all classification metrics on test predictions
metrics(predicted = test_pred_rf, actual = features_test$Specie, multiclass = TRUE)

# AUC
multiclass.roc(response = features_test_pca$Specie, 
               predictor = as.numeric(test_pred_rf))$auc[1]


###############
##
## Random forest (cross-validation)
##
###############

time_mod_rf_cv <- system.time({
  mod_rf_cv <- tune("randomForest",
                    Specie ~ .,
                    data = features_train %>% select(-hasbird),
                    kernel = "radial",
                    type = "C-classification",
                    ranges = list(
                      mtry = 10:50
                    ),
                    tunecontrol = tune.control(cross = 3))
})

# Save the mod_rf_cv object into an .RData file
save(mod_rf_cv, file = here(models_dir, "rf_multiclass_pca_cv.RData"))
dim(mod_rf_cv)

# Predict the fitted model with the features_train dataset
train_pred_rf <- predict(mod_rf_cv, newdata = features_train %>% select(-hasbird))

# Accuracy on features_train dataset
mean(train_pred_rf == features_train$Specie)

# Predict the fitted model with the features_test dataset
test_pred_rf <- predict(mod_rf_cv, newdata = features_test %>% select(-hasbird))

# Accuracy on features_test dataset
mean(test_pred_rf == features_test$Specie)

# Compute all classification metrics on test predictions
metrics(predicted = test_pred_rf, actual = features_test$Specie, multiclass = TRUE)

# AUC
multiclass.roc(response = features_test_pca$Specie, 
               predictor = as.numeric(test_pred_rf))$auc[1]

