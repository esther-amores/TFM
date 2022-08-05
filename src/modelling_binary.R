## ---------------------------
##
## Script name: modelling_binary.R
##
## Purpose of script: Binary classification to detect the presence or absence of bird sounds
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

# # Input by variable mean in NA values
# for(i in 1:ncol(features_train_pca)) {
#   features_train_pca[is.na(features_train_pca[, i]), i] <- mean(features_train_pca[, i], na.rm = TRUE)
# }
# # Input by variable mean in NA values
# for(i in 1:ncol(features_test_pca)) {
#   features_test_pca[is.na(features_test_pca[, i]), i] <- mean(features_test_pca[, i], na.rm = TRUE)
# }

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
## Logistic regression
##
###############

# Fit a logistic regression model
time_mod_glm <- system.time({
  mod_glm <- glm(hasbird ~ ., data = features_train %>% select(-Specie), family = "binomial")
})

# Summarize the logistic regression model
smod_glm <- summary(mod_glm)
smod_glm

# R-squared
r_squared(mod_glm)

# Chi-squared p-value 
chi_squared_p_value(mod_glm)

# Predict the fitted model with the features_train dataset
train_pred_glm <- predict(mod_glm, newdata = features_train %>% select(-Specie), type = "response") %>% 
  sapply(function(x) { ifelse(x >= 0.5, 1, 0) })

# Accuracy on features_train dataset
mean(train_pred_glm == features_train$hasbird)

# Predict the fitted model with the features_test dataset
test_pred_glm <- predict(mod_glm, newdata = features_test %>% select(-Specie), type = "response") %>% 
  sapply(function(x) { ifelse(x >= 0.5, 1, 0) })

# Accuracy on features_test dataset
mean(test_pred_glm == features_test$hasbird)

# # Predict the fitted model with the test dataset
# test_pred_glm <- predict(mod_glm, newdata = test, type = "response") %>%
#   sapply(function(x) { ifelse(x >= 0.5, 1, 0) })
# 
# table(test_pred_glm)

# Compute all classification metrics on test predictions
metrics(predicted = test_pred_glm, actual = features_test$hasbird)


###############
##
## Logistic regression (with significant variables)
##
###############

# Obtain those explanatory variables whose confidence level is 95%
sel_vars <- rownames(smod_glm$coefficients[smod_glm$coefficients[, 4] <= 0.01, ])[-1]

# Fit a logistic regression model with the significant valus
time_mod_glm2 <- system.time({
  mod_glm2 <- glm(as.formula(sprintf("hasbird ~ %s", paste(sel_vars, collapse = "+"))),
                  data = features_train %>% select(-Specie), family = "binomial")
})

# Summarize the logistic regression model
smod_glm2 <- summary(mod_glm2)
smod_glm2

# R-squared
r_squared(mod_glm2)

# Chi-squared p-value 
chi_squared_p_value(mod_glm2)

# Predict the fitted model with the features_train dataset
train_pred_glm2 <- predict(mod_glm2, newdata = features_train %>% select(-Specie), type = "response") %>% 
  sapply(function(x) { ifelse(x >= 0.5, 1, 0) })

# Accuracy on features_train dataset
mean(train_pred_glm2 == features_train$hasbird)

# Predict the fitted model with the features_test dataset
test_pred_glm2 <- predict(mod_glm2, newdata = features_test %>% select(-Specie), type = "response") %>% 
  sapply(function(x) { ifelse(x >= 0.5, 1, 0) })

# Accuracy on features_test dataset
mean(test_pred_glm2 == features_test$hasbird)

# Compute all classification metrics on test predictions
metrics(predicted = test_pred_glm2, actual = features_test$hasbird)


###############
##
## LASSO regularization 
##
###############

train_mat <- features_train_pca %>% select(-c(Specie, hasbird)) %>% as.matrix()
test_mat <- features_test_pca %>% select(-c(Specie, hasbird)) %>% as.matrix()

# Estimate a suitable value for lambda
lambdas <- 10 ^ seq(8, -4, length = 250)

time_mod_lasso <- system.time({
  mod_lasso <- glmnet(x = train_mat, y = features_train_pca$hasbird, alpha = 1, 
                      lambda = lambdas, family = "binomial")
  cv_mod_lasso <- cv.glmnet(x = train_mat, y = features_train_pca$hasbird, alpha = 1,
                            lambda = lambdas, family = "binomial")
})

lambda_lasso <- cv_mod_lasso$lambda.min

# Examine the coefficients of the regularized model using lambda
predict(mod_lasso, type = "coefficients", s = lambda_lasso)

# Predict the fitted model with the features_train_pca dataset
train_pred_lasso <- predict(mod_lasso, 
                            s = lambda_lasso, 
                            newx = train_mat, 
                            type = "response") %>% 
  sapply(function(x) { ifelse(x >= 0.5, 1, 0) })

# Accuracy on features_train_pca dataset
mean(train_pred_lasso == features_train_pca$hasbird)

# Predict the fitted model with the features_test_pca dataset
test_pred_lasso <- predict(mod_lasso, 
                            s = lambda_lasso, 
                            newx = test_mat, 
                            type = "response") %>% 
  sapply(function(x) { ifelse(x >= 0.5, 1, 0) })

# Accuracy on features_test_pca dataset
mean(test_pred_lasso == features_test_pca$hasbird)

# # Predict the fitted model with the test dataset
# test_pred_lasso <- predict(mod_lasso, 
#                            s = lambda_lasso, 
#                            newx = test %>% as.matrix(), 
#                            type = "response") %>% 
#   sapply(function(x) { ifelse(x >= 0.5, 1, 0) })
# 
# table(test_pred_lasso)

# Compute all classification metrics on test predictions
metrics(predicted = test_pred_lasso, actual = features_test_pca$hasbird)


###############
##
## Ridge regularization 
##
###############

# Estimate a suitable value for lambda
lambdas <- 10 ^ seq(8, -4, length = 250)

time_mod_ridge <- system.time({
  mod_ridge <- glmnet(x = train_mat, y = features_train_pca$hasbird, alpha = 0, 
                      lambda = lambdas, family = "binomial")
  cv_mod_ridge <- cv.glmnet(x = train_mat, y = features_train_pca$hasbird, alpha = 0,
                            lambda = lambdas, family = "binomial")
})

lambda_ridge <- cv_mod_ridge$lambda.min

# Examine the coefficients of the regularized model using lambda
predict(mod_ridge, type = "coefficients", s = lambda_ridge)

# Predict the fitted model with the features_train_pca dataset
train_pred_ridge <- predict(mod_ridge, 
                            s = lambda_ridge, 
                            newx = features_train_pca %>% select(-c(Specie, hasbird)) %>% as.matrix(), 
                            type = "response") %>% 
  sapply(function(x) { ifelse(x >= 0.5, 1, 0) })

# Accuracy on features_train_pca dataset
mean(train_pred_ridge == features_train_pca$hasbird)

# Predict the fitted model with the features_test_pca dataset
test_pred_ridge <- predict(mod_ridge, 
                            s = lambda_ridge, 
                            newx = features_test_pca %>% select(-c(Specie, hasbird)) %>% as.matrix(), 
                            type = "response") %>% 
  sapply(function(x) { ifelse(x >= 0.5, 1, 0) })

# Accuracy on features_test_pca dataset
mean(test_pred_ridge == features_test_pca$hasbird)

# Compute all classification metrics on test predictions
metrics(predicted = test_pred_ridge, actual = features_test_pca$hasbird)


###############
##
## Naive Bayes
##
###############

# Fit a Naive Bayes model
time_mod_nb <- system.time({
  mod_nb <- naiveBayes(hasbird ~ ., data = features_train_pca %>% select(-Specie))
})

# Predict the fitted model with the features_train_pca dataset
train_pred_nb <- predict(mod_nb, features_train_pca %>% select(-Specie), type = "class") 

# Accuracy on features_train_pca dataset
mean(train_pred_nb == features_train_pca$hasbird)

# Predict the fitted model with the features_test_pca dataset
test_pred_nb <- predict(mod_nb, features_test_pca %>% select(-Specie), type = "class") 

# Accuracy on features_test_pca dataset
mean(test_pred_nb == features_test_pca$hasbird)

# Compute all classification metrics on test predictions
metrics(predicted = ifelse(test_pred_nb == 1, 1, 0), actual = features_test_pca$hasbird)


###############
##
## SVM (linear kernel)
##
###############

time_mod_svm <- system.time({
  mod_svm <- svm(hasbird ~ ., 
                 data = features_train_pca %>% select(-Specie),
                 kernel = "linear",
                 type = "C-classification",
                 cost = 1,
                 scale = TRUE)
})

save(mod_svm, file = here(models_dir, "svm_binary_pca_linearkernel.RData"))


# Predict the fitted model with the features_train_pca dataset
train_pred_svm <- predict(mod_svm, features_train_pca %>% select(-Specie), type = "class") 

# Accuracy on features_train_pca dataset
mean(train_pred_svm == features_train_pca$hasbird)

# Predict the fitted model with the features_test_pca dataset
test_pred_svm <- predict(mod_svm, features_test_pca %>% select(-Specie), type = "class") 

# Accuracy on features_test_pca dataset
mean(test_pred_svm == features_test_pca$hasbird)

# Compute all classification metrics on test predictions
metrics(predicted = ifelse(test_pred_svm == 1, 1, 0), actual = features_test_pca$hasbird)


###############
##
## SVM (radial kernel)
##
###############

time_mod_svm2 <- system.time({
  mod_svm2 <- svm(hasbird ~ ., 
                  data = features_train_pca %>% select(-Specie),
                  kernel = "radial",
                  type = "C-classification",
                  cost = 1,
                  scale = TRUE)
})

save(mod_svm2, file = here(models_dir, "svm_binary_pca_radialkernel.RData"))


# Predict the fitted model with the features_train_pca_pca dataset
train_pred_svm2 <- predict(mod_svm2, features_train_pca %>% select(-Specie), type = "class") 

# Accuracy on features_train_pca_pca dataset
mean(train_pred_svm2 == features_train_pca$hasbird)

# Predict the fitted model with the features_test_pca_pca dataset
test_pred_svm2 <- predict(mod_svm2, features_test_pca %>% select(-Specie), type = "class") 

# Accuracy on features_test_pca_pca dataset
mean(test_pred_svm2 == features_test_pca$hasbird)

# Compute all classification metrics on test predictions
metrics(predicted = ifelse(test_pred_svm2 == 1, 1, 0), actual = features_test_pca$hasbird)



###############
##
## MLP
##
###############

# Fit a Multilayer Perceptron model
time_mod_mlp <- system.time({
  mod_mlp <- nnet(hasbird ~ ., data = features_train_pca %>% select(-Specie), 
                  size = 10, maxit = 1000)
})

# Predict the fitted model with the features_train_pca dataset
train_pred_mlp <- predict(mod_mlp, features_train_pca %>% select(-Specie), type = "class") 

# Accuracy on features_train_pca dataset
mean(train_pred_mlp == features_train_pca$hasbird)

# Predict the fitted model with the features_test_pca dataset
test_pred_mlp <- predict(mod_mlp, features_test_pca %>% select(-Specie), type = "class") 

# Accuracy on features_test_pca dataset
mean(test_pred_mlp == features_test_pca$hasbird)

# Compute all classification metrics on test predictions
metrics(predicted = ifelse(test_pred_mlp == 1, 1, 0), actual = features_test_pca$hasbird)


###############
##
## MLP (cross-testation)
##
###############

# Fit a Multilayer Perceptron model
time_mod_mlp <- system.time({
  mod_mlp <- nnet(hasbird ~ ., data = features_train_pca %>% select(-Specie), 
                  size = 10, decay = 1e-5, maxit = 1000)
})

# Predict the fitted model with the features_train_pca dataset
train_pred_mlp <- predict(mod_mlp, features_train_pca %>% select(-Specie), type = "class") 

# Accuracy on features_train_pca dataset
mean(train_pred_mlp == features_train_pca$hasbird)

# Predict the fitted model with the features_test_pca dataset
test_pred_mlp <- predict(mod_mlp, features_test_pca %>% select(-Specie), type = "class") 

# Accuracy on features_test_pca dataset
mean(test_pred_mlp == features_test_pca$hasbird)

# Compute all classification metrics on test predictions
metrics(predicted = ifelse(test_pred_mlp == 1, 1, 0), actual = features_test_pca$hasbird)




# # AUC-ROC
# test_pred_glm <- predict(mod_glm, newdata = features_test_pca %>% select(-Specie), type = "response")
# ROC <- roc(response = features_test_pca$hasbird, predictor = test_pred_glm)
# ROC_rounded <- roc(response = features_test_pca$hasbird, predictor = round(test_pred_glm, 1))
# plot(ROC, print.auc = TRUE)
# lines(ROC_rounded, col = "red", type = "b")
# text(0.4, 0.43, labels = sprintf("AUC: %0.3f", auc(ROC_rounded)), col = "red")


# # Obtain those explanatory variables whose confidence level is 95%
# sel_vars <- rownames(smod_glm$coefficients[smod_glm$coefficients[, 4] <= 0.05, ])
# fml <- as.formula(sprintf("features_train_pca$hasbird ~ %s", paste(sprintf("features_train_pca$%s", sel_vars), collapse = "+")))

# # Plot a 3D point cloud with the statistically significant variables
# colors <- c("#E69F00", "#56B4E9")
# colors <- colors[features_train_pca$hasbird]
# 
# par(mfrow = c(2, 2))
# 
# library(scatterplot3d)
# scatterplot3d(
#   fml,
#   cex.main = 0.7,
#   cex.axis = 0.5,
#   cex.lab = 0.7,
#   angle = 20,
#   pch = 16,
#   type = "p",
#   highlight.3d = TRUE,
#   color = colors
#   # col.grid = "palegreen",
#   # col.axis = "blue"
# )
# 
# scatterplot3d(
#   fml,
#   cex.main = 0.7,
#   cex.axis = 0.5,
#   cex.lab = 0.7,
#   angle = -40,
#   pch = 16,
#   type = "p",
#   highlight.3d = TRUE,
#   color = colors
#   # col.grid = "palegreen",
#   # col.axis = "blue"
# )
# 
# scatterplot3d(
#   fml,
#   cex.main = 0.7,
#   cex.axis = 0.5,
#   cex.lab = 0.7,
#   angle = -120,
#   pch = 16,
#   type = "p",
#   highlight.3d = TRUE,
#   color = colors
#   # col.grid = "palegreen",
#   # col.axis = "blue"
# )
# 
# scatterplot3d(
#   fml,
#   cex.main = 0.7,
#   cex.axis = 0.5,
#   cex.lab = 0.7,
#   angle = 120,
#   pch = 16,
#   type = "p",
#   highlight.3d = TRUE,
#   color = colors
#   # col.grid = "palegreen",
#   # col.axis = "blue"
# )
# 
# legend(
#   "bottom",
#   legend = levels(features_train_pca$hasbird),
#   col = c("#E69F00", "#56B4E9"),
#   inset = -0.25,
#   xpd = TRUE,
#   horiz = TRUE
# )
# 
# # Fit a logistic regression model with the statistically significant variables
# time_mod_glm2 <- system.time({
#   mod_glm2 <- glm(as.formula(sprintf("hasbird ~ %s-1", paste(sel_vars, collapse = "+"))), 
#                   data = features_train_pca, 
#                   family = binomial)
# })
# 
# # Summarize the logistic regression model
# summary(mod_glm2)
# 
# # Predict the fitted model
# pred_glm <- predict(mod_glm2, newdata = features_test_pca, type = "response") %>% 
#   sapply(function(x) { ifelse(x >= 0.5, 1, 0) })
# 
# # Confusion matrix
# conf_mat <- table(features_test_pca$hasbird, pred_glm, dnn = c("Real class", "Predicted class"))
# conf_mat 
# 
# # Plot confusion matrix
# library(cvms)
# 
# conf_mat_df <- as_tibble(conf_mat)
# plot_confusion_matrix(conf_mat_df,
#                       target_col = "Real class",
#                       prediction_col = "Predicted class",
#                       counts_col = "n") 





time_mod_glm <- system.time({
  
  set.seed(999)
  cv_error <- c()
  k <- 5
  
  for (i in 1:k) {
    mod_glm <- glm(hasbird ~ .-1, data = features_train_pca, family = binomial)
    cv_error[i] <- cv.glm(data = features_train_pca, glmfit = mod_glm, K = k)$delta[1]
    
    # mod_glm_df <- augment(mod_glm, data = features_train_pca)
    # 
    # p1 <- ggplot(mod_glm_df, aes(x = .fitted, y = .resid)) +
    #   geom_point(size = 1, alpha = 0.4) +
    #   labs(title = sprintf("%d-fold Cross-testation", i),
    #        subtitle = "Constant variance among residuals", 
    #        x = "Predicted values",
    #        y = "Residuals")
    # 
    # p2 <- ggplot(mod_glm_df, aes(x = 1:nrow(mod_glm_df), y = .resid)) +
    #   geom_point(size = 1, alpha = 0.4) +
    #   labs(title = sprintf("%d-fold Cross-testation", i),
    #        subtitle = "Uncorrelated residuals",
    #        x = "Row ID",
    #        y = "Residuals")
    # 
    # ggarrange(p1, p2, ncol = 2) %>% 
    #   annotate_figure(top = text_grob(mod_glm$call$formula, size = 14))
  }
})

# Plot the cross-testation features_train_pca error 
ggplot(data = data.frame(fold = 1:k, train_error = cv_error), 
       aes(x = fold, y = train_error)) +
  geom_line() +
  theme_pubclean()


