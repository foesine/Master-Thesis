
####################
# Libraries
list_of_packages <- c ("synthpop", "insight", "party", "dplyr", "rpart", "rpart.plot", "randomForest", "pROC", "caret", "pracma", "here", "Hmisc")

# Function to check and install packages
install_if_missing <- function(p) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p)
  }
  library(p, character.only = TRUE)
}

# Install and load all required packages
lapply(list_of_packages, install_if_missing)



#####################
# Load the data
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


#####################
# Functions

## tree fct with control component

build_tree <- function(data,trainsplit = 0.8, cp = 0.01, controls){ #minsplit=20, minbucket=5, maxdepth=20, 
    train_index <- sample(1:nrow(data), nrow(data)*trainsplit)
    # train dataset formation
    train_set <- data[train_index, ]
    # test dataset formation
    test_set <- data[-train_index, ]

    tree <- rpart(income ~ ., data = train_set, cp=cp, control = rpart.control(maxsurrogate = 0, maxcompete = 1))

    # Predict on the test set
    predictions <- predict(tree, test_set)#, type = "prob")

    # for factored variables this will give probabilities, so there is a need to create the actual predictions
    if (is.factor(data$income)){
        # Initialize predictions as just the probability predictions
        predictions_prob <- predictions
        predictions <- list(probabilities = predictions_prob)
        preds <- apply(predictions_prob, 1, function(row) {
            # Get the index of the max value in the row
            max_index <- which.max(row)
            # Return the column name using the index
            return(colnames(predictions_prob)[max_index])
        })
        # Add actual predictions to the predictions list
        predictions$classes <- as.factor(preds)
    }

    # plot the tree
    #rpart.plot(tree)

    # Results
    results <- list(train_set, test_set, tree, predictions)
    names(results) <- c("train_set", "test_set", "tree", "predictions")
    return(results)
}

## function for comparison

evaluation_metrics_cont <- function(predictions_orig, test_set_orig, predictions_syn, test_set_syn){

  # create table for original data
  eval_orig <- evaluation_cont_single(predictions_orig, test_set_orig)
  # create table for synthetic data
  eval_syn <- evaluation_cont_single(predictions_syn, test_set_syn)

  # Merge the two dataframes and set the first column as row names
  metrics_df <- merge(eval_orig, eval_syn, by = "Metric", suffixes = c("_orig", "_syn"))
   # Set "Metric" column as row names
  rownames(metrics_df) <- metrics_df$Metric
  
  # Remove "Metric" column
  metrics_df <- metrics_df[, -1]
  
  # Calculate the numeric difference
  metrics_df$Difference <- metrics_df$Value_syn - metrics_df$Value_orig

  return(metrics_df)
}

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
## Calculate evaluation metrics for factored targets
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

#####################
# Simulation for orig data
simulation <- function(data, nrun = 10, k_fold = 10, steps = 10){
    # create empty vector to safe trees
    tree <- list()
    # create vector to safe loss
    loss <- rep(0, steps)
    # create empty array to store cp values chosen
    cp_val <- rep(0, nrun)
    # create empty list to store evaluation dataframes
    eval_list <- list()

    # set the complexity parameters for trees
    complexity <- 10^seq(log10(0.0001), log10(0.01), length.out = steps)

    # for loss-calculation factored variables need to be converted to numeric
    if (is.factor(data$income)) {
        data$income <- as.factor(as.numeric(data$income == ">50K"))
    }

    # set inital seed
    s <- 1234
    for (i in 1:nrun){
        # vary seed with each run
        s <- s + 1

        # Randomly split the data set into k-subsets (or k-fold)
        datalist_syn <- split(data, sample(1:k_fold, nrow(gen_data$syn), replace=T)) #list of k same-sized elements that are slices of the data
        
        # leave-one-out CV for prediction
        for (j in 1:k_fold) {
            # split data in k folds
            data_val <- datalist[[j]]               # j-th of the k folds, validation set
            data_train <- bind_rows(datalist[-j])   #rest of the data without j-th of the k folds, training set

            # optional parameters to prevent overfitting: minbucket, minsplit, maxdepth
            for (l in 1:length(complexity)){
                # create income prediction tree with train data
                tree[[l]] <- rpart(income ~ ., data = data_train, cp = complexity[l], control = rpart.control(maxsurrogate = 0, maxcompete = 1))
                # Predict on the validation set
                predictions <- predict(tree[[l]], data_val)

                # safe some loss information and sum over the k-fold loops
                if (is.numeric(data$income)) {
                    # Mean Squared Error
                    loss[l] <- loss[l] + mean((predictions - data_val$income)^2)
                    }
                else if (is.factor(data$income)) {
                # Cross-Entropy Loss
                    epsilon <- 1e-15  # to prevent log(0) which is undefined
                    predicted_probs <- pmax(pmin(predictions[,2], 1 - epsilon), epsilon)
                    n <- length(predicted_probs)
                    loss[l] <- loss[l] + (-sum(as.numeric(data_val$income) * log(predicted_probs) + (1 - as.numeric(data_val$income)) * log(1 - predicted_probs)) / n)
                    }
                else {
                    break("The predicted target has to be numeric or factor.")
                    }
                }
            }
        # for which cp value was the loss the smallest
        min_loss <- which.min(loss)
        print(min_loss)
        cp_val[i] <- c(complexity[min_loss])

        tree_s <- build_tree(data = gen_data$syn, cp = cp_val[i])
    
        # evaluation metrics
        if (is.numeric(data$income)) {
            eval <- as.data.frame(evaluation_metrics_cont(tree_s$predictions, tree_s$test_set))
            }
        else if (is.factor(data$income)) {
            eval <- as.data.frame(evaluation_metrics_factor(tree_s$predictions$classes, tree_s$test_set))
            }
        else {
            break("The predicted target has to be numeric or factor.")
            }

        eval_list[[i]] <- eval
        print(c("run", i, "completed"))
        }

    # average over all runs
    sum_df <- Reduce(function(x, y) Map(`+`, x, y), eval_list)
    eval_avg <- lapply(sum_df, function(col) col / length(eval_list))

    # Convert the list back to a dataframe
    # Store row names
    rownames <- row.names(eval_list[[1]])

    # Convert the list back to a dataframe
    eval_avg <- as.data.frame(eval_avg)

    # Set back the row names
    row.names(eval_avg) <- rownames
    
    # returns
    results <- list(eval_avg = eval_avg,  cp_vals = cp_val)
    return(results)
}


#####################
# Simulation for synthetic data
simulation <- function(data, nrun = 10, k_fold = 10, steps = 10){
    # create array to save the synthetic data
    # syn_data <- array(data = NA, dim = c(k, ncol(data), nrun)) # not necessary, but in case it's wanted
    # create empty vector to safe trees
    tree <- list()
    # create vector to safe loss
    loss <- rep(0, steps)
    # create empty array to store cp values chosen
    cp_val <- rep(0, nrun)
    # create empty list to store evaluation dataframes
    eval_list <- list()

    # set the complexity parameters for trees
    complexity <- 10^seq(log10(0.0001), log10(0.01), length.out = steps)

    # for loss-calculation factored variables need to be converted to numeric
    if (is.factor(data$income)) {
        data$income <- as.factor(as.numeric(data$income == ">50K"))
    }

    # set inital seed
    s <- 1234
    for (i in 1:nrun){
        # vary seed with each run
        s <- s + 1

        # generate synthetic data
        gen_data <- syn(data = data, k = nrow(data), seed = s)
        # save the data from each run if wanted
        # syn_data[,,i] <- as.matrix(gen_data$syn)

        # Randomly split the data set into k-subsets (or k-fold)
        data_syn <- as.data.frame(gen_data$syn) # try without this?
        datalist_syn <- split(gen_data$syn, sample(1:k_fold, nrow(gen_data$syn), replace=T)) #list of k same-sized elements that are slices of the data
        
        # leave-one-out CV for prediction
        for (j in 1:k_fold) {
            # split data in k folds
            data_val <- datalist_syn[[j]]               # j-th of the k folds, validation set
            data_train <- bind_rows(datalist_syn[-j])   #rest of the data without j-th of the k folds, training set

            # optional parameters to prevent overfitting: minbucket, minsplit, maxdepth
            for (l in 1:length(complexity)){
                # create income prediction tree with train data
                tree[[l]] <- rpart(income ~ ., data = data_train, cp = complexity[l], control = rpart.control(maxsurrogate = 0, maxcompete = 1))
                # Predict on the validation set
                predictions <- predict(tree[[l]], data_val)

                # safe some loss information and sum over the k-fold loops
                if (is.numeric(data$income)) {
                    # Mean Squared Error
                    loss[l] <- loss[l] + mean((predictions - data_val$income)^2)
                    }
                else if (is.factor(data$income)) {
                    # Cross-Entropy Loss
                    epsilon <- 1e-15  # to prevent log(0) which is undefined
                    predicted_probs <- pmax(pmin(predictions[,2], 1 - epsilon), epsilon)
                    n <- length(predicted_probs)
                    loss[l] <- loss[l] + (-sum(as.numeric(data_val$income) * log(predicted_probs) + (1 - as.numeric(data_val$income)) * log(1 - predicted_probs)) / n)
                    }
                else {
                    break("The predicted target has to be numeric or factor.")
                    }
                }
            }
        # for which cp value was the loss the smallest
        min_loss <- which.min(loss)
        print(min_loss)
        cp_val[i] <- c(complexity[min_loss])

        tree_s <- build_tree(data = gen_data$syn, cp = cp_val[i])
    
        # evaluation metrics
        if (is.numeric(data$income)) {
            eval <- as.data.frame(evaluation_metrics_cont(tree_s$predictions, tree_s$test_set))
            }
        else if (is.factor(data$income)) {
            eval <- as.data.frame(evaluation_metrics_factor(tree_s$predictions$classes, tree_s$test_set))
            }
        else {
            break("The predicted target has to be numeric or factor.")
            }

        eval_list[[i]] <- eval
        print(c("run", i, "completed"))
        }

    # average over all runs
    sum_df <- Reduce(function(x, y) Map(`+`, x, y), eval_list)
    eval_avg <- lapply(sum_df, function(col) col / length(eval_list))

    # Convert the list back to a dataframe
    # Store row names
    rownames <- row.names(eval_list[[1]])

    # Convert the list back to a dataframe
    eval_avg <- as.data.frame(eval_avg)

    # Set back the row names
    row.names(eval_avg) <- rownames
    
    # returns
    results <- list(eval_avg = eval_avg,  cp_vals = cp_val)
    return(results)
}


#####################
# Run the simulation
adult_res <- simulation(adult, nrun = 20, k_fold = 10, steps = 10)
cps_res <- simulation(cpspop, nrun = 20, k_fold = 10, steps = 10)

# Save data
saveRDS(cps_res, file = (paste0(here(), "/simulation/cps_CART_res.RData")))
saveRDS(adult_res, file = (paste0(here(), "/simulation/adult_CART_res.RData")))
