---
title: An R Markdown document converted from "/Users/emmafoessing/Documents/Master/MA/Code/Master-Thesis/MLP_prediction_model.ipynb"
output: html_document
---

# MLP prediction function

### Packages


``` r
list_of_packages <- c ("synthpop", "insight", "party", "dplyr", "rpart", "rpart.plot", "randomForest", "pROC", "caret", "pracma", "here", "Hmisc", "randomForest",  "xgboost", "data.table", "RSNNS", "nnet")

# Function to check and install packages
install_if_missing <- function(p) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p, dependencies = TRUE)
  }
  library(p, character.only = TRUE)
}

# Install and load all required packages
lapply(list_of_packages, install_if_missing)
```

```
## [[1]]
##  [1] "nnet"         "RSNNS"        "Rcpp"         "data.table"   "xgboost"     
##  [6] "Hmisc"        "here"         "pracma"       "caret"        "lattice"     
## [11] "ggplot2"      "pROC"         "randomForest" "rpart.plot"   "rpart"       
## [16] "dplyr"        "party"        "strucchange"  "sandwich"     "zoo"         
## [21] "modeltools"   "stats4"       "mvtnorm"      "grid"         "insight"     
## [26] "synthpop"     "xfun"         "knitr"        "rmarkdown"    "stats"       
## [31] "graphics"     "grDevices"    "utils"        "datasets"     "methods"     
## [36] "base"        
## 
## [[2]]
##  [1] "nnet"         "RSNNS"        "Rcpp"         "data.table"   "xgboost"     
##  [6] "Hmisc"        "here"         "pracma"       "caret"        "lattice"     
## [11] "ggplot2"      "pROC"         "randomForest" "rpart.plot"   "rpart"       
## [16] "dplyr"        "party"        "strucchange"  "sandwich"     "zoo"         
## [21] "modeltools"   "stats4"       "mvtnorm"      "grid"         "insight"     
## [26] "synthpop"     "xfun"         "knitr"        "rmarkdown"    "stats"       
## [31] "graphics"     "grDevices"    "utils"        "datasets"     "methods"     
## [36] "base"        
## 
## [[3]]
##  [1] "nnet"         "RSNNS"        "Rcpp"         "data.table"   "xgboost"     
##  [6] "Hmisc"        "here"         "pracma"       "caret"        "lattice"     
## [11] "ggplot2"      "pROC"         "randomForest" "rpart.plot"   "rpart"       
## [16] "dplyr"        "party"        "strucchange"  "sandwich"     "zoo"         
## [21] "modeltools"   "stats4"       "mvtnorm"      "grid"         "insight"     
## [26] "synthpop"     "xfun"         "knitr"        "rmarkdown"    "stats"       
## [31] "graphics"     "grDevices"    "utils"        "datasets"     "methods"     
## [36] "base"        
## 
## [[4]]
##  [1] "nnet"         "RSNNS"        "Rcpp"         "data.table"   "xgboost"     
##  [6] "Hmisc"        "here"         "pracma"       "caret"        "lattice"     
## [11] "ggplot2"      "pROC"         "randomForest" "rpart.plot"   "rpart"       
## [16] "dplyr"        "party"        "strucchange"  "sandwich"     "zoo"         
## [21] "modeltools"   "stats4"       "mvtnorm"      "grid"         "insight"     
## [26] "synthpop"     "xfun"         "knitr"        "rmarkdown"    "stats"       
## [31] "graphics"     "grDevices"    "utils"        "datasets"     "methods"     
## [36] "base"        
## 
## [[5]]
##  [1] "nnet"         "RSNNS"        "Rcpp"         "data.table"   "xgboost"     
##  [6] "Hmisc"        "here"         "pracma"       "caret"        "lattice"     
## [11] "ggplot2"      "pROC"         "randomForest" "rpart.plot"   "rpart"       
## [16] "dplyr"        "party"        "strucchange"  "sandwich"     "zoo"         
## [21] "modeltools"   "stats4"       "mvtnorm"      "grid"         "insight"     
## [26] "synthpop"     "xfun"         "knitr"        "rmarkdown"    "stats"       
## [31] "graphics"     "grDevices"    "utils"        "datasets"     "methods"     
## [36] "base"        
## 
## [[6]]
##  [1] "nnet"         "RSNNS"        "Rcpp"         "data.table"   "xgboost"     
##  [6] "Hmisc"        "here"         "pracma"       "caret"        "lattice"     
## [11] "ggplot2"      "pROC"         "randomForest" "rpart.plot"   "rpart"       
## [16] "dplyr"        "party"        "strucchange"  "sandwich"     "zoo"         
## [21] "modeltools"   "stats4"       "mvtnorm"      "grid"         "insight"     
## [26] "synthpop"     "xfun"         "knitr"        "rmarkdown"    "stats"       
## [31] "graphics"     "grDevices"    "utils"        "datasets"     "methods"     
## [36] "base"        
## 
## [[7]]
##  [1] "nnet"         "RSNNS"        "Rcpp"         "data.table"   "xgboost"     
##  [6] "Hmisc"        "here"         "pracma"       "caret"        "lattice"     
## [11] "ggplot2"      "pROC"         "randomForest" "rpart.plot"   "rpart"       
## [16] "dplyr"        "party"        "strucchange"  "sandwich"     "zoo"         
## [21] "modeltools"   "stats4"       "mvtnorm"      "grid"         "insight"     
## [26] "synthpop"     "xfun"         "knitr"        "rmarkdown"    "stats"       
## [31] "graphics"     "grDevices"    "utils"        "datasets"     "methods"     
## [36] "base"        
## 
## [[8]]
##  [1] "nnet"         "RSNNS"        "Rcpp"         "data.table"   "xgboost"     
##  [6] "Hmisc"        "here"         "pracma"       "caret"        "lattice"     
## [11] "ggplot2"      "pROC"         "randomForest" "rpart.plot"   "rpart"       
## [16] "dplyr"        "party"        "strucchange"  "sandwich"     "zoo"         
## [21] "modeltools"   "stats4"       "mvtnorm"      "grid"         "insight"     
## [26] "synthpop"     "xfun"         "knitr"        "rmarkdown"    "stats"       
## [31] "graphics"     "grDevices"    "utils"        "datasets"     "methods"     
## [36] "base"        
## 
## [[9]]
##  [1] "nnet"         "RSNNS"        "Rcpp"         "data.table"   "xgboost"     
##  [6] "Hmisc"        "here"         "pracma"       "caret"        "lattice"     
## [11] "ggplot2"      "pROC"         "randomForest" "rpart.plot"   "rpart"       
## [16] "dplyr"        "party"        "strucchange"  "sandwich"     "zoo"         
## [21] "modeltools"   "stats4"       "mvtnorm"      "grid"         "insight"     
## [26] "synthpop"     "xfun"         "knitr"        "rmarkdown"    "stats"       
## [31] "graphics"     "grDevices"    "utils"        "datasets"     "methods"     
## [36] "base"        
## 
## [[10]]
##  [1] "nnet"         "RSNNS"        "Rcpp"         "data.table"   "xgboost"     
##  [6] "Hmisc"        "here"         "pracma"       "caret"        "lattice"     
## [11] "ggplot2"      "pROC"         "randomForest" "rpart.plot"   "rpart"       
## [16] "dplyr"        "party"        "strucchange"  "sandwich"     "zoo"         
## [21] "modeltools"   "stats4"       "mvtnorm"      "grid"         "insight"     
## [26] "synthpop"     "xfun"         "knitr"        "rmarkdown"    "stats"       
## [31] "graphics"     "grDevices"    "utils"        "datasets"     "methods"     
## [36] "base"        
## 
## [[11]]
##  [1] "nnet"         "RSNNS"        "Rcpp"         "data.table"   "xgboost"     
##  [6] "Hmisc"        "here"         "pracma"       "caret"        "lattice"     
## [11] "ggplot2"      "pROC"         "randomForest" "rpart.plot"   "rpart"       
## [16] "dplyr"        "party"        "strucchange"  "sandwich"     "zoo"         
## [21] "modeltools"   "stats4"       "mvtnorm"      "grid"         "insight"     
## [26] "synthpop"     "xfun"         "knitr"        "rmarkdown"    "stats"       
## [31] "graphics"     "grDevices"    "utils"        "datasets"     "methods"     
## [36] "base"        
## 
## [[12]]
##  [1] "nnet"         "RSNNS"        "Rcpp"         "data.table"   "xgboost"     
##  [6] "Hmisc"        "here"         "pracma"       "caret"        "lattice"     
## [11] "ggplot2"      "pROC"         "randomForest" "rpart.plot"   "rpart"       
## [16] "dplyr"        "party"        "strucchange"  "sandwich"     "zoo"         
## [21] "modeltools"   "stats4"       "mvtnorm"      "grid"         "insight"     
## [26] "synthpop"     "xfun"         "knitr"        "rmarkdown"    "stats"       
## [31] "graphics"     "grDevices"    "utils"        "datasets"     "methods"     
## [36] "base"        
## 
## [[13]]
##  [1] "nnet"         "RSNNS"        "Rcpp"         "data.table"   "xgboost"     
##  [6] "Hmisc"        "here"         "pracma"       "caret"        "lattice"     
## [11] "ggplot2"      "pROC"         "randomForest" "rpart.plot"   "rpart"       
## [16] "dplyr"        "party"        "strucchange"  "sandwich"     "zoo"         
## [21] "modeltools"   "stats4"       "mvtnorm"      "grid"         "insight"     
## [26] "synthpop"     "xfun"         "knitr"        "rmarkdown"    "stats"       
## [31] "graphics"     "grDevices"    "utils"        "datasets"     "methods"     
## [36] "base"        
## 
## [[14]]
##  [1] "nnet"         "RSNNS"        "Rcpp"         "data.table"   "xgboost"     
##  [6] "Hmisc"        "here"         "pracma"       "caret"        "lattice"     
## [11] "ggplot2"      "pROC"         "randomForest" "rpart.plot"   "rpart"       
## [16] "dplyr"        "party"        "strucchange"  "sandwich"     "zoo"         
## [21] "modeltools"   "stats4"       "mvtnorm"      "grid"         "insight"     
## [26] "synthpop"     "xfun"         "knitr"        "rmarkdown"    "stats"       
## [31] "graphics"     "grDevices"    "utils"        "datasets"     "methods"     
## [36] "base"        
## 
## [[15]]
##  [1] "nnet"         "RSNNS"        "Rcpp"         "data.table"   "xgboost"     
##  [6] "Hmisc"        "here"         "pracma"       "caret"        "lattice"     
## [11] "ggplot2"      "pROC"         "randomForest" "rpart.plot"   "rpart"       
## [16] "dplyr"        "party"        "strucchange"  "sandwich"     "zoo"         
## [21] "modeltools"   "stats4"       "mvtnorm"      "grid"         "insight"     
## [26] "synthpop"     "xfun"         "knitr"        "rmarkdown"    "stats"       
## [31] "graphics"     "grDevices"    "utils"        "datasets"     "methods"     
## [36] "base"        
## 
## [[16]]
##  [1] "nnet"         "RSNNS"        "Rcpp"         "data.table"   "xgboost"     
##  [6] "Hmisc"        "here"         "pracma"       "caret"        "lattice"     
## [11] "ggplot2"      "pROC"         "randomForest" "rpart.plot"   "rpart"       
## [16] "dplyr"        "party"        "strucchange"  "sandwich"     "zoo"         
## [21] "modeltools"   "stats4"       "mvtnorm"      "grid"         "insight"     
## [26] "synthpop"     "xfun"         "knitr"        "rmarkdown"    "stats"       
## [31] "graphics"     "grDevices"    "utils"        "datasets"     "methods"     
## [36] "base"        
## 
## [[17]]
##  [1] "nnet"         "RSNNS"        "Rcpp"         "data.table"   "xgboost"     
##  [6] "Hmisc"        "here"         "pracma"       "caret"        "lattice"     
## [11] "ggplot2"      "pROC"         "randomForest" "rpart.plot"   "rpart"       
## [16] "dplyr"        "party"        "strucchange"  "sandwich"     "zoo"         
## [21] "modeltools"   "stats4"       "mvtnorm"      "grid"         "insight"     
## [26] "synthpop"     "xfun"         "knitr"        "rmarkdown"    "stats"       
## [31] "graphics"     "grDevices"    "utils"        "datasets"     "methods"     
## [36] "base"
```

### Data


``` r
load(file = (paste0(here(), "/cpspop.RData")))
adult <- read.csv(file = (paste0(here(),"/adult_preprocessed.csv")))
# delete NAs
adult[adult == "?"] <- NA
adult <- na.omit(adult)

adult$workclass <- as.factor(adult$workclass)
adult$education <- as.factor(adult$education)
adult$marital_status <- as.factor(adult$marital_status)
adult$relationship <- as.factor(adult$relationship)
adult$race <- as.factor(adult$race)
adult$sex <- as.factor(adult$sex)
adult$native_country <- as.factor(adult$native_country)
adult$income <- as.factor(adult$income)
```

### Helper functions


``` r
## eval cont targets
evaluation_metrics_cont <- function(predictions, test_set){
  MAE <- mean(abs(predictions - test_set$income))
  MSE <- mean((predictions - test_set$income)^2)
  RMSE <- sqrt(MSE)

  SS_res <- sum((test_set$income - predictions)^2)
  SS_tot <- sum((test_set$income - mean(test_set$income))^2)
  R_squared <- 1 - (SS_res / SS_tot)

  MAPE <- mean(abs((test_set$income - predictions) / test_set$income)) * 100

  # Create the dataframe
  metrics_df <- data.frame(
        MAE = MAE, 
        MSE = MSE, 
        RMSE = RMSE,
        R_squared = R_squared, 
        MAPE = MAPE)

  return(metrics_df)
}
```


``` r
## eval factored targets
evaluation_metrics_factor <- function(predictions, test_set){
    # confusion matrix for the prediction on original data
    cm <- confusionMatrix(predictions, test_set$income,
                mode = "everything")

    # saving evaluation metrics
    accuracy <- cm$overall['Accuracy']
    f1 <- cm$byClass['F1']
    sens <- cm$byClass['Sensitivity']
    spec <- cm$byClass['Specificity']

    # Create the dataframe
    metrics_df <- data.frame(
        Accuracy = accuracy, 
        F1 = f1, 
        Sensitivity = sens, 
        Specificity = spec
    )
    
    return(metrics_df)
}
```


``` r
mlp_pred <- function(data, outer_folds, size_steps, decay_steps, inner_folds){
    # adjust evaluation metric to fit both numeric and factored targets
    summaryFunctionType <- if (is.numeric(data$income)) defaultSummary else multiClassSummary
    # metric: train() uses per default RSME and Accuracy for numeric and factored targets

    #  set control args
    outer_control <- trainControl(method = "cv", number = outer_folds,
                                  summaryFunction = summaryFunctionType,
                                  verboseIter = FALSE,
                                  allowParallel = TRUE)
        
    inner_control <- trainControl(method = "cv", number = inner_folds, 
                                  summaryFunction = summaryFunctionType,
                                  verboseIter = FALSE,
                                  allowParallel = TRUE)

    # Define the grid for hyperparameter tuning
    size_values <- seq(1, 10, length.out = size_steps)
    decay_values <- 10^seq(log10(0.0001), log10(0.01), length.out = decay_steps)

    # Create grid
    tunegrid <- expand.grid(size = size_values, decay = decay_values)

    # Initialize variables to store results
    outer_results <- list()

    outer_cv_folds = createFolds(data$income, k = outer_folds)
    
    # Outer loop: Cross-validation for model evaluation
    for (i in seq_along(outer_folds)) {
        
        # Split data into outer folds
        outer_test_index = outer_cv_folds[[i]]
        outer_testData = data[outer_test_index,]
        outer_trainData  = data[-outer_test_index,]
        
        # Hyperparameter tuning using inner CV
        # No need for inner loop because "train" does k-fold CV already
        mlp_model <- caret::train(income ~ ., 
                           data = outer_trainData, 
                           method = "nnet", 
                           tuneGrid = tunegrid, 
                           trControl = inner_control)#,
                           #metric = metricType)
            

        # Store the best hyperparameters
        best_hyperparameters <- mlp_model$bestTune
        print("best HP")
        print(mlp_model$bestTune)

        # Train the final model on the outer training set with the best hyperparameters
        final_model <- caret::train(income ~ ., 
                             data = outer_trainData, 
                             method = "nnet", 
                             trControl = outer_control, 
                             tuneGrid = best_hyperparameters)

        # Testing the final model on the outer test set
        predictions <- predict(final_model, newdata = outer_testData)
        
        if (is.numeric(data$income)) {
            eval <- evaluation_metrics_cont(predictions, outer_testData)
        } else if (is.factor(data$income)) {
            eval <- evaluation_metrics_factor(predictions, outer_testData)
        } else {
            stop("The predicted target has to be numeric or factor.")
        }

        # Store the evaluation metrics for this outer fold
        outer_results[[i]] <- eval
    }

    # Average the evaluation metrics over the outer folds
    eval_avg_outer_folds <- do.call(rbind, outer_results) %>%
                            summarise(across(everything(), mean, na.rm = TRUE))

    

    # Return the average evaluation metrics
    return(eval_avg_outer_folds)
}
```





