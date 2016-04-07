source("src/max_idx.R");

## import the function from Github
## library(devtools);
## source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r');

FitANN <- function(N.groups, NHiddenUnits = 4, cut = 1) {
    data <- LoadData(binarize = FALSE); data$Rings <- NULL; data$Sex <- as.integer(data$Sex);
    if (cut == 1) {
        data$Class <- cut(data$Age, breaks = N.groups);  data$Age <- NULL;  ## str(data);
    } else if (cut == 2) {
        data$Class <- cut2(data$Age, g = N.groups);  data$Age <- NULL;  # str(data);
    }

    fmla <- as.formula(c("Class ~ ",
                         paste0(kAttributeNames[kAttributeNames != "Rings"],
                                collapse = " + ")));  print(fmla);

    model <- nnet(formula = fmla, data = data, size = NHiddenUnits);
    print(summary(model));
    ## str(model);
    ## plot.nnet(model);
    ## Y_test_est <- predict(object = model, newdata = data.test.k1, type = 'raw');
    ## y_test_est = max_idx(Y_test_est);
    ## Error.test.K1[k1] = 100 * sum(as.integer(data.test.cl.k1) != y_test_est) / length(data.test.cl.k1);
    plot.nnet(model);
    return(model);
}  ## End of FitANN




predictionFunction <- function(Xgriddf, model) {
    Y_test_est <- predict(object = model, newdata = Xgriddf, type = "raw");
    y_test_est = max_idx(Y_test_est);
    y_test_est;
}

FitANNCV2 <- function(N.groups, NHiddenUnits.max = 5, K1 = 2, K2 = 2, cut = 1) {
    data <- LoadData(binarize = FALSE); data$Rings <- NULL; data$Sex <- as.integer(data$Sex);
    if (cut == 1) {
        data$Class <- cut(data$Age, breaks = N.groups);  data$Age <- NULL;  ## str(data);
    } else if (cut == 2) {
        data$Class <- cut2(data$Age, g = N.groups);  data$Age <- NULL;  # str(data);
    }

    fmla <- as.formula(c("Class ~ ",
                         paste0(kAttributeNames[kAttributeNames != "Rings"],
                                collapse = " + ")));  ## print(fmla);
    NHiddenUnits <- 1:NHiddenUnits.max;

    CV.K1 <- cvFolds(n = length(data$Class),  ## number of obs to be split into groups
                  K = K1  ## number of groups in which obs should be split
                  );  ## str(CV); print(CV);

    CV.K1$TrainSize <- rep(NA, K1); # add sizes of training and test data to the list CV returned by cvFolds
    CV.K1$TestSize <- rep(NA, K1);
    NHiddenUnits.best.vec <- rep(NA, K1);

    ## Variable for classification error count
    Error.train.K1 = matrix(rep(NA, times = K1), nrow = K1);
    Error.test.K1 = matrix(rep(NA, times = K1), nrow = K1);
    min.error.rate <- rep(NA, K1);

    for (k1 in 1:K1) {  ## K1-fold splitting OUTER loop
        print(paste0(">> OUTER ", k1, " of ", K1));
        ## Extract training and test set
        data.train.k1 <- data[CV.K1$which != k1, ]; data.train.cl.k1 <- data$Class[CV.K1$which != k1];
        data.test.k1 <- data[CV.K1$which == k1, ]; data.test.k1$Class <- NULL; data.test.cl.k1 <- data$Class[CV.K1$which == k1];
        CV.K1$TrainSize[k1] <- length(data.train.cl.k1);
        CV.K1$TestSize[k1] <- length(data.test.cl.k1);
        ## str(data.train.k1); str(data.train.cl.k1);
        ## str(data.test.k1); str(data.test.cl.k1);

        CV.K2 <- cvFolds(n = length(data.train.cl.k1),  ## number of obs to be split into groups
                         K = K2  ## number of groups in which obs should be split
                         );  ## str(CV.K2); print(CV.K2);
        CV.K2$TrainSize <- rep(NA, K2);  ## train set size
        CV.K2$TestSize <- rep(NA, K2);  ## test set size
        ## Matrices for classification error count
        Error.train.K2 = matrix(rep(NA, times = K2 * length(NHiddenUnits)), nrow = K2);
        Error.test.K2 = matrix(rep(NA, times = K2 * length(NHiddenUnits)), nrow = K2);

        for (k2 in 1:K2) {  ## K2-fold splitting INNER loop
            print(paste0(">> INNER ", k2, " of ", K2));
            ## Extract training and test set
            data.train.k2 <- data.train.k1[CV.K2$which != k2, ]; data.train.cl.k2 <- data.train.cl.k1[CV.K2$which != k2];
            data.test.k2 <- data.train.k1[CV.K2$which == k2, ]; data.test.k2$Class <- NULL;
            data.test.cl.k2 <- data.train.cl.k1[CV.K2$which == k2];
            ## str(data.train.k2); str(data.train.cl.k2);
            ## str(data.test.k2); str(data.test.cl.k2);
            CV.K2$TrainSize[k2] <- length(data.train.cl.k2);
            CV.K2$TestSize[k2] <- length(data.test.cl.k2);

            for (n in 1:NHiddenUnits.max) { # For each number of hidden units
                model <- nnet(formula = fmla, data = data.train.k2, size = n);
                Y_test_est <- predict(object = model, newdata = data.test.k2, type = 'raw');
                y_test_est = max_idx(Y_test_est);
                Error.test.K2[k2, n] = 100 * sum(as.integer(data.test.cl.k2) != y_test_est);
                ## ErrorRate = sum(as.integer() != y_test_est) / length(data.test.cl);
            }  ## End of 1:NHiddenUnits loop
            ## Err.Train.K2 <- colSums(Error.train.K2) / sum(CV.K2$TrainSize);
        }  ## End of K2-fold splitting INNER loop
        Err.Test.K2 <- colSums(Error.test.K2) / sum(CV.K2$TestSize);
        ## print(Err.Test.K2);
        minErr <- min(Err.Test.K2)[1];
        NHiddenUnits.best <- NHiddenUnits[Err.Test.K2 == minErr][1];
        NHiddenUnits.best.vec[k1] <- NHiddenUnits.best;
        print(paste0(">> min(Err.Test.K2) = ", minErr,
                     " at NHiddenUnits.best = ", NHiddenUnits.best));

        model <- nnet(formula = fmla, data = data.train.k1, size = NHiddenUnits.best);
        Y_test_est <- predict(object = model, newdata = data.test.k1, type = 'raw');
        y_test_est = max_idx(Y_test_est);
        Error.test.K1[k1] = 100 * sum(as.integer(data.test.cl.k1) != y_test_est) / length(data.test.cl.k1);
    }  ## End of K1-fold splitting OUTER loop

    ## print(paste0(">> Cross-validated error rates:"));
    ## ## Err.Train.K1 <- 1 * colSums(Error.train.K1) / sum(CV.K1$TrainSize);
    ## Err.Test.K1 <- 1 * colSums(Error.test.K1) / sum(CV.K1$TestSize);
    ## ## print(Err.Train.K1);
    ## print(Err.Test.K1);
    return(list(
        k1 = 1:K1,
        NHiddenUnits.best = NHiddenUnits.best.vec,
        ErrRate = Error.test.K1
        ));

    ## print(summary(model));
    ## str(model);
    ## ## Compute results on test data
    ## ## Get the predicted output for the test data

    ## print((Y_test_est[1:20, ]));
    ## Compute the class index by finding the class with highest
    ## probability from the neural network

    ## str((y_test_est));
    ## Subtract one to have y_test_est between 0 and C-1
    ## y_test_est = y_test_est - 1;
    ## str(y_test_est);
    ## print((y_test_est[1:20]));
    ## print(summary(y_test_est));
    ## print(length(y_test_est));
    ## print(length(data.test.cl));
    ## print(as.integer(data.test.cl[1:20]));
    ## print(summary(as.integer(data.test.cl)));
    ## Compute error rate

    ## print(paste('Error rate: ', ErrorRate * 100, '%', sep=''));
    ## ## Plot results
    ## ## Display trained network
    ## plot(model);
}  ## End of FitANN


# Display decision boundaries
##dbplot(X_testdf, attributeNames, predictionFunction, y=y_test, contourLevels=0.5, contourCols='white', model=model)
