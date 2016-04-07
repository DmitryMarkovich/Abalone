source("src/dbplot.R");  ## decision boundary plot function from toolbox

predictionFunction <- function(Xgriddf, model) {
    Y_test_est <- predict(object = model, newdata = Xgriddf, type = "probs");
    y_test_est <- max_idx(Y_test_est);
    y_test_est;
}  ## End of predictionFunction

################################################################################
FitMultinomCV1 <- function(N.groups = 5, K1 = 2, cut = 1) {
    data <- LoadData(binarize = FALSE); data$Rings <- NULL; data$Sex <- as.integer(data$Sex);
    if (cut == 1) {
        data$Class <- cut(data$Age, breaks = N.groups);  data$Age <- NULL;  ## str(data);
    } else if (cut == 2) {
        data$Class <- cut2(data$Age, g = N.groups);  data$Age <- NULL;  # str(data);
    }

    fmla <- as.formula(c("Class ~ ",
                         paste0(kAttributeNames[kAttributeNames != "Rings"],
                                collapse = " + ")));  ## print(fmla);

    CV <- cvFolds(n = length(data$Class),  ## number of obs to be split into groups
                  K = K1  ## number of groups in which obs should be split
                  );  ## str(CV); print(CV);

    ## CV$TrainSize <- sum(CV$which != 1) # add sizes of training and test data to the list CV returned by cvFolds
    ## CV$TestSize <- sum(CV$which != 2)
    ## CV$TrainSize <- rep(NA, K); # add sizes of training and test data to the list CV returned by cvFolds
    ## CV$TestSize <- rep(NA, K);

    ## Variable for classification error count
    Error_rate <- rep(NA, times = K1);

    for (k in 1:K1) {  ## K1-fold splitting OUTER loop
        print(paste0(">> OUTER ", k, " of ", K1));
        ## Extract training and test set
        data.train <- data[CV$which != k, ]; data.train.cl <- data$Class[CV$which != k];
        data.test <- data[CV$which == k, ]; data.test$Class <- NULL; data.test.cl <- data$Class[CV$which == k];

        str(data.train);
        str(data.test);
        model <- multinom(formula = fmla, data = data.train);
        ## print(summary(model));
        ## Compute results on test data
        ## Get the predicted output for the test data
        data.test.est = predict(object = model, newdata = data.test, type = "probs");
        print((data.test.est[1:20, ]));
        ## print(summary(data.test.est));
        ## Compute the class index by finding the class with highest probability from the multinomial regression model
        ## y_ <- apply(data.test.est, 1, max);
        ## print(summary(y_));
        y_test_est <- apply(data.test.est, 1, which.max);
        print((y_test_est[1:20]));
        ## ## Subtract one to have y_test_est between 0 and C - 1
        ## y_test_est = data.test.est - 1;
        ## Compute error rate
        Error_rate[k] = 100 * sum(as.integer(data.test.cl) != y_test_est) / length(data.test.cl);
    }
    ## plot(Error_rate);
    ## abline(h = mean(Error_rate));
    ## print(paste("Error rate: ", mean(Error_rate), " % ", sep = ""));
    return(list(
        k1 = 1:K1,
        ErrRate = Error_rate
        ));
}  ## End of FitMultinom







##                                         # exercise 8.3.4
## rm(list=ls())
## source("setup.R")
## graphics.off()
## library(nnet) #install.packages("mlogit")

## # Load data
## library(R.matlab)
## dat <- readMat(file.path('Data', 'synth3.mat'))
## X <- dat$X
## N <- dat$N
## attributeNames <- as.vector(unlist(dat$attributeNames))
## M <- dat$M
## y <- dat$y
## C <- dat$C
## classNames <- as.vector(unlist(dat$classNames))

## X_train <- dat$X.train
## N_train <- dat$N.train
## y_train<- dat$y.train

## X_test <- dat$X.test
## N_test <- dat$N.test
## y_test <- dat$y.test
## # substitute spaces with dots to make handling of columns in data matrix easier
## attributeNames <- gsub(' ', '.', attributeNames)
## X_traindf <- data.frame(X_train)
## colnames(X_traindf) <- attributeNames
## X_testdf <- data.frame(X_test)
## colnames(X_testdf) <- attributeNames

## ## Fit multinomial regression model
## Y_train=factor(y_train)
## Y_test=factor(y_test)

## (fmla <- as.formula(paste("y_train ~ ", paste(attributeNames, collapse= "+"))))
## model <- multinom(formula=fmla, data=X_traindf)
    
## ## Compute results on test data
## # Get the predicted output for the test data
## Y_test_est = predict(object=model, newdata=X_testdf, type='probs')

## # Compute the class index by finding the class with highest probability from the multinomial regression model
## y_ <- apply(Y_test_est, 1, max)
## y_test_est <- apply(Y_test_est, 1, which.max)
## # Subtract one to have y_test_est between 0 and C-1
## y_test_est = y_test_est-1;

## # Compute error rate
## ErrorRate = sum(y_test!=y_test_est)/N_test;
## print(paste('Error rate: ', ErrorRate*100, '%', sep=''));

## ## Plot results
## # Display decision boundaries
## predictionFunction <- function(Xgriddf, model){
## Y_test_est <- predict(object=model, newdata=Xgriddf, type='probs')
## y_test_est <- max_idx(Y_test_est);
## y_test_est
## }

## dbplot(X_testdf, attributeNames, predictionFunction, y=y_test, contourLevels=0.5, contourCols='white', model=model)
