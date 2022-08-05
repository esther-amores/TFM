###############
##
## Model deviance
##
###############

# R-squared 
r_squared <- function(model) {
  1 - (model$deviance / model$null.deviance)
}

# Chi-squared p-value 
chi_squared_p_value <- function(model) {
  null_df <- model$df.null
  model_df <- model$df.residual
  difference_df <- null_df - model_df
  null_deviance <- model$null.deviance
  m_deviance <- model$deviance
  difference_deviance <- null_deviance - m_deviance
  pchisq(difference_deviance, difference_df, lower.tail = FALSE)
}


###############
##
## Performance measures
##
###############

metrics <- function(predicted, actual, multiclass = FALSE) {
  if(multiclass == FALSE) {
    
    # Confusion matrix
    conf_mat <- table(predicted = predicted, actual = actual)
    
    # Plot confusion matrix
    plot_confusion_matrix(conf_mat %>% as_tibble(),
                          target_col = "actual",
                          prediction_col = "predicted",
                          counts_col = "n") %>% 
      print()
    
    # True Positive, True Negative, False Positive and False Negative
    TP <- conf_mat[1, 1]
    TN <- conf_mat[2, 2]
    FP <- conf_mat[1, 2]
    FN <- conf_mat[2, 1]
    
    # Recall / Sensitivity / True Positive Rate
    TPR <- TP / (TP + FN)
    
    # False Positive Rate
    FPR <- FP / (FP + TN)
    
    # AUC-ROC
    AUC <- roc(response = actual, predictor = predicted)$auc[1]
    
    # Precision
    precision <- TP / (TP + FP)
    
    # Specificity / True Negative Rate
    specificity <- TN / (TN + FP)
    
    # Accuracy
    accuracy <- (TP + TN) / (TP + FN + TN + FP)
    
    # F1-score
    F1 <- TP / (TP + 0.5*(FP + FN))  
    
    # Error rate
    error <- 1 - accuracy 
    
    metrics <- data.frame(TPR, FPR, AUC, precision, specificity, accuracy, F1, error)
    return(metrics)
    
  }
  
  if(multiclass == TRUE) {
    
    # Confusion matrix
    conf_mat <- evaluate(data = data.frame(predicted = as.character(predicted), 
                                           actual = as.character(actual)),
                         target_col = "actual",
                         prediction_cols = "predicted",
                         type = "multinomial")
    
    # Plot confusion matrix
    print(plot_confusion_matrix(conf_mat$`Confusion Matrix`[[1]],
                                add_counts = TRUE,
                                add_normalized = FALSE,
                                add_row_percentages = FALSE,
                                add_col_percentages = FALSE,
                                rotate_y_text = FALSE) +
            theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0))) 
    
    metrics <- data.frame(TPR = conf_mat$Sensitivity, 
                          precision = conf_mat$`Pos Pred Value`, 
                          specificity = conf_mat$Specificity,
                          accuracy = conf_mat$`Overall Accuracy`,
                          F1 = conf_mat$F1,
                          error = 1 - conf_mat$`Overall Accuracy`)
    return(metrics)
    
  }
}