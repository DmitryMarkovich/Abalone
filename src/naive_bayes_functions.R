source("src/naiveBayes.R");
source("src/predict_naiveBayes.R");

################################################################################
TestNB <- function(N.groups, K = 10, prior = "empirical") {
    data <- LoadData(binarize = FALSE); data$Rings <- NULL;
    data$Sex <- as.integer(data$Sex);
    data$Class <- cut2(data$Age, g = N.groups); data$Age <- NULL; ## str(data);
    N <- length(data$Class);
    ## K-fold crossvalidation
    set.seed(1234) # for reproducibility
    CV <- cvFolds(n = N, K = K);
    CV$TrainSize <- rep(NA, K);  ## train set size
    CV$TestSize <- rep(NA, K);  ## test set size

    ## Parameters for naive Bayes classifier
    Distribution <- ("normal");  ## 54.4 %
    ## Distribution <- c("mvmn", rep("normal", 7)); 54.8 %
    ## Distribution <- ("mvmn");  75 %
    Prior <- prior;  ## empirical, uniform

    ## Variable for classification error
    Error <- rep(NA, times = K);

    for (k in 1:K) { # For each crossvalidation fold
    print(paste("Crossvalidation fold ", k, " of ", K));

    ## Extract training and test set
    data.train <- data[CV$which != k, ];  ## data.train$Sex <- as.integer(data.train$Sex);
    data.train$Class <- NULL;
    y.train <- data$Class[CV$which != k];
    data.test <- data[CV$which == k, ];  ## data.test$Sex <- as.integer(data.test$Sex);
    data.test$Class <- NULL;
    y.test <- data$Class[CV$which == k];
    CV$TrainSize[k] <- length(y.train);
    CV$TestSize[k] <- length(y.test);

    ## Fit naive Bayes classifier to training set
    mymod <- naiveBayes(X = data.train, y = y.train, distribution = Distribution,
                        prior = Prior);
    # Predict model on test data
    predictRes <- predict.naiveBayes(Xtest = data.test, mod = mymod);
    y.test.est <- predictRes$predictedClass;
    ## print(head(y.test.est));
    ## print(head(y.test));
    ## print(head(as.integer(y.test)));
    ## str(as.factor(y.test.est));
    ## str(y.test);
    # Compute error rate
    err <- as.integer(y.test) != y.test.est;
    err[is.na(err)] <- 1; # make test cases that could not be predicted count as errors
    Error[k] <- sum(err); # Count the number of errors
  }
    ## Print the error rate
    print(paste0('Error rate: ', sum(Error) / sum(CV$TestSize) * 100, " %"));
}  ## End of TestNB
################################################################################
