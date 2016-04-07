################################################################################
TestNNCV1 <- function(N.groups = 5, K = 10, kNN.max = 60) {
    data <- LoadData(binarize = FALSE); data$Rings <- NULL;
    ## data$Class <- cut2(data$Age, g = N.groups); data$Age <- NULL;  ## str(data);
    data$Class <- cut(data$Age, breaks = N.groups); data$Age <- NULL;  ## str(data);
    N <- length(data$Class);
    ## Leave-one-out crossvalidation
    CV <- cvFolds(n = N, K = K); ## K = N;

    ## Variable for classification error
    Error = array(rep(NA, times = K * kNN.max), dim = c(K, kNN.max));

    for (k in 1:K) { # For each crossvalidation fold
        print(paste0(">> Crossvalidation fold ", k, " of ", K));

        ## Extract training and test set
        data.train <- data[CV$which != k, ]; data.train$Sex <- as.integer(data.train$Sex);
        data.train$Class <- NULL;  ## str(data.train);
        data.train.cl <- data[CV$which != k, ]$Class;  ## str(data.train.cl);
        CV$TrainSize[k] <- length(data.train.cl);

        data.test <- data[CV$which == k, ]; data.test$Sex <- as.integer(data.test$Sex);
        data.test$Class <- NULL;  ## str(data.test);
        data.test.cl <- data[CV$which == k,]$Class; ## print(summary(data.test.cl));
        CV$TestSize[k] <- length(data.test.cl);

        for (l in 1:kNN.max) { # For each number of neighbors
            ## Use knnclassify to find the l nearest neighbors
            test.cl <- knn(train = data.train, test = data.test,
                           cl = data.train.cl, k = l,
                           prob = FALSE, algorithm = "kd_tree");
            ## Compute number of classification errors
            ## print(summary(data.test.cl));
            ## print(summary(test.cl));
            test.cl <- factor(test.cl, levels = levels(data.test.cl));
            ## print(summary(test.cl));
            Error[k, l] = sum(data.test.cl != test.cl);
        }
    }
    return(list(
        Err = Error, ErrSize = sum(CV$TestSize),
        N.groups = N.groups, K = K
        ));
}  ## End of TestNNCV1
################################################################################

################################################################################
TestNNCV2 <- function(N.groups = 5, K1 = 10, K2 = 10, kNN.max = 60, cut = 1) {
    data <- LoadData(binarize = FALSE); data$Rings <- NULL;
    if (cut == 1) {
        data$Class <- cut(data$Age, breaks = N.groups); data$Age <- NULL;  ## str(data);
    } else if (cut == 2) {
        data$Class <- cut2(data$Age, g = N.groups); data$Age <- NULL;  ## str(data);
    }

    N <- length(data$Class);
    ## Leave-one-out crossvalidation
    CV.K1 <- cvFolds(n = N, K = K1); ## K = N;
    CV.K1$TrainSize <- rep(NA, K1);  ## train set size
    CV.K1$TestSize <- rep(NA, K1);  ## test set size
    ## Variable for classification error
    Error.K1 <- rep(NA, times = K1);
    kNN.best.vec <- rep(NA, times = K1);
    ## pdf(file = paste0(project.path, "/output/k_nearest_neighbours_err_CV2.pdf"),
    ##     width = 12, height = 9, bg = "white");
    ## par(mfrow = c(2, 5));
    for (k1 in 1:K1) {  ## OUTER loop
        print(paste0(">> OUTER ", k1, " of ", K1));

        ## Extract training and test set
        data.train.k1 <- data[CV.K1$which != k1, ]; data.train.k1$Sex <- as.integer(data.train.k1$Sex);
        data.train.k1$Class <- NULL;  ## str(data.train);
        data.train.k1.cl <- data[CV.K1$which != k1, ]$Class;  ## str(data.train.cl);
        CV.K1$TrainSize[k1] <- length(data.train.k1.cl);

        data.test.k1 <- data[CV.K1$which == k1, ]; data.test.k1$Sex <- as.integer(data.test.k1$Sex);
        data.test.k1$Class <- NULL;  ## str(data.test.k1);
        data.test.k1.cl <- data[CV.K1$which == k1,]$Class; ## print(summary(data.test.k1.cl));
        CV.K1$TestSize[k1] <- length(data.test.k1.cl);

        CV.K2 <- cvFolds(n = length(data.train.k1.cl),
                         K = K2);
        CV.K2$TrainSize <- rep(NA, K2);  ## train set size
        CV.K2$TestSize <- rep(NA, K2);  ## test set size
        ## Variable for classification error
        Error.K2 = array(rep(NA, times = K2 * kNN.max), dim = c(K2, kNN.max));

        for (k2 in 1:K2) {  ## INNER loop
            print(paste0("   INNER ", k2, " of ", K2));
            ## Extract training and test set
            data.train.k2 <- data.train.k1[CV.K2$which != k2, ]; data.train.k2$Sex <- as.integer(data.train.k2$Sex);
            data.train.k2$Class <- NULL;  ## str(data.train.k2);
            data.train.k2.cl <- data.train.k1.cl[CV.K2$which != k2];  ## str(data.train.k2.cl);
            CV.K2$TrainSize[k2] <- length(data.train.k2.cl);

            data.test.k2 <- data.train.k1[CV.K2$which == k2, ]; data.test.k2$Sex <- as.integer(data.test.k2$Sex);
            data.test.k2$Class <- NULL;  ## str(data.test.k1);
            data.test.k2.cl <- data.train.k1.cl[CV.K2$which == k2]; ## print(summary(data.test.k2.cl));
            CV.K2$TestSize[k2] <- length(data.test.k2.cl);

            for (l in 1:kNN.max) { # For each number of neighbors
                ## Use knnclassify to find the l nearest neighbors
                test.cl <- knn(train = data.train.k2, test = data.test.k2,
                               cl = data.train.k2.cl, k = l,
                               prob = FALSE, algorithm = "kd_tree");
                ## Compute number of classification errors
                ## str(data.test.k2.cl);hea
                print(head(test.cl));
                test.cl <- factor(test.cl, levels = levels(data.test.k2.cl));
                print(head(test.cl));
                ## str(test.cl);
                Error.K2[k2, l] = sum(data.test.k2.cl != test.cl);
            }
        }  ## End of K2 INNER loop

        Err.K2 <- 100 * colSums(Error.K2) / sum(CV.K2$TestSize);
        Err.K2.min <- min(Err.K2)[1];
        kNN.best <- (1:kNN.max)[Err.K2 == min(Err.K2)][1];
        kNN.best.vec[k1] <- kNN.best;
        print(paste0(">> min(Err.K2) = ", min(Err.K2.min), " at kNN.best = ", kNN.best));

        ## plot(Err.K2, xlab = "Number of neighbours",
        ##      ylab = "Classification error rate, %", cex.lab = 2, cex.axis = 2,
        ##      cex.main = 2, main = paste0("k1 = ", k1, " of ", K1));
        ## abline(v = kNN.best, lty = 2);
        ## abline(h = Err.K2.min, lty = 2);

        test.cl <- knn(train = data.train.k1, test = data.test.k1,
                       cl = data.train.k1.cl, k = kNN.best[1],
                       prob = FALSE, algorithm = "kd_tree");
        ## Compute number of classification errors
        ## str(data.test.k1.cl);
        ## str(test.cl);
        test.cl <- factor(test.cl, levels = levels(data.test.k1.cl));
        Error.K1[k1] = 100 * sum(data.test.k1.cl != test.cl) / length(data.test.k1.cl);
    }  ## End of K1 OUTER loop
    ## Err.K1 <- 100 * sum(Error.K1) / sum(CV.K1$TestSize);
    ## print(Err.K1);
    ## return(list(
    ##         Err = Error, ErrSize = sum(CV$TestSize),
    ##         N.groups = N.groups, K = K
    ##         ));
    ## dev.off();
    return(list(
        k1 = 1:K1,
        kNN.best = kNN.best.vec,
        ErrRate = Error.K1
        ));
}  ## End of TestNNCV2
################################################################################
