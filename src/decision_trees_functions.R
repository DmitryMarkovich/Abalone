################################################################################
MakeDecisionTree <- function(N.groups = 5, cp = 0, minsplit = 10, cut = 1) {
    data <- LoadData(binarize = FALSE); data$Rings <- NULL;
    if (cut == 1) {
        data$Class <- cut(data$Age, breaks = N.groups); data$Age <- NULL;
    } else if (cut == 2) {
        data$Class <- cut2(data$Age, g = N.groups); data$Age <- NULL;
    }
    str(data);
    fmla <- as.formula(c("Class ~ ",
                         paste0(kAttributeNames[kAttributeNames != "Rings"],
                                collapse = " + ")));
    print(fmla);
    tree <- NULL;
    tree <- rpart(formula = fmla,
                  data = data, method = "class",
                  control = rpart.control(
                      ## minsplit - the minimum number of observations that must exist in a
                      ## node in order for a split to be attempted.
                      minsplit = minsplit,
                      ## minbucket - the minimum number of observations in any terminal
                      ## ‘<leaf>’ node.
                      minbucket = 1,
                      ## cp - complexity parameter.  Any split that does not decrease the
                      ## overall lack of fit by a factor of ‘cp’ is not attempted.
                      cp = cp,
                      ## "information", "gini"
                      parms = list(split = "gini")
                      )
                  );
    if (!is.null(tree)) {
        return(tree);
    } else {
        warning(">> returning NULL!");
        return(NULL);
    }
}  ## End of MakeDecisionTree
################################################################################

################################################################################
FindOptCPwCV1 <- function(N.groups = 5, K = 10, cp.max = 0.05, cp.N = 10,
                          minsplit = 10) {
    data <- LoadData(binarize = FALSE); data$Rings <- NULL;
    ## data$Class <- cut2(data$Age, g = N.groups);  data$Age <- NULL;  # str(data);
    data$Class <- cut(data$Age, breaks = N.groups);  data$Age <- NULL;  # str(data);
    fmla <- as.formula(c("Class ~ ",
                         paste0(kAttributeNames[kAttributeNames != "Rings"],
                                collapse = " + ")));  ## print(fmla);
    tree <- NULL;  ## object to hold a decision tree

    set.seed(1234);  ## gives reproducible results
    CV <- cvFolds(n = length(data$Class),  ## number of obs to be split into groups
                  K = K  ## number of groups in which obs should be split
                  );  ## str(CV); print(CV);
    CV$TrainSize <- rep(NA, K);  ## train set size
    CV$TestSize <- rep(NA, K);  ## test set size

    prune <- seq(from = 0, to = cp.max, length.out = cp.N);  ## vector of pruning levels

    ## Matrices for classification error count
    Error.train = matrix(rep(NA, times = K * length(prune)), nrow = K);
    Error.test = matrix(rep(NA, times = K * length(prune)), nrow = K);

    for (k in 1:K) {  ## K-fold splitting loop
        ## Extract training and test set
        data.train <- data[CV$which != k, ];
        data.test <- data[CV$which == k, ];
        CV$TrainSize[k] <- length(data.train$Class);
        CV$TestSize[k] <- length(data.test$Class);

        ## Fit the decision tree
        tree <- rpart(formula = fmla,
                      data = data.train, method = "class",
                      control = rpart.control(
                          ## minsplit - the minimum number of observations that must exist in a
                          ## node in order for a split to be attempted.
                          minsplit = minsplit,
                          ## minbucket - the minimum number of observations in any terminal
                          ## ‘<leaf>’ node.
                          minbucket = 1,
                          ## cp - complexity parameter.  Any split that does not decrease the
                          ## overall lack of fit by a factor of ‘cp’ is not attempted.
                          cp = 0.0,
                          ## "information", "gini"
                          parms = list(split = "gini"),
                          ## max depth of any node in the final tree, root - depth 0
                          ## maxdepth = 20,
                          )
                      );  ## str(tree);

        ## Compute classification error
        for (n in 1:length(prune)) { # For each pruning level
            mytree_pruned <- prune(tree = tree,
                                   cp = prune[n]  ## cp parameter to which tree will be pruned
                                   );
            pr.clss.train <- predict(mytree_pruned, newdata = data.train,
                                     type = "vector");
            pr.clss.test <- predict(mytree_pruned, newdata = data.test,
                                    type = "vector");
            Error.train[k, n] = sum(as.integer(pr.clss.train) != as.integer(data.train$Class));
            Error.test[k, n] = sum(as.integer(pr.clss.test) != as.integer(data.test$Class));
        }  ## End of for n in 1:length(prune)
    }  ## End of for k in K
    return(list(
        ErrTest = Error.test, ErrTestSize = sum(CV$TestSize),
        ErrTrain = Error.train, ErrTrainSize = sum(CV$TrainSize),
        N.groups = N.groups, K = K, minsplit = minsplit,
        cp = prune
        ));
} ## End of FindOptCPwCV1
################################################################################

################################################################################
FindOptCPwCV2 <- function(N.groups = 5, K1 = 5, K2 = 10, cp.max = 0.05, cp.N = 10,
                          minsplit = 10, cut = 1) {
    print(paste0(">> Starting CV2 with K1 = ", K1, ", K2 = ", K2, ", cp.N = ", cp.N));
    data <- LoadData(binarize = FALSE); data$Rings <- NULL;
    if (cut == 1) {
        data$Class <- cut(data$Age, breaks = N.groups);  data$Age <- NULL;  # str(data);
    } else if (cut == 2) {
        data$Class <- cut2(data$Age, g = N.groups);  data$Age <- NULL;  # str(data);
    } else {
        stop("unknown cut value!");
    }

    fmla <- as.formula(c("Class ~ ",
                         paste0(kAttributeNames[kAttributeNames != "Rings"],
                                collapse = " + ")));  ## print(fmla);

    tree <- NULL;  ## object to hold a decision tree
    cp.vec <- seq(from = 0, to = cp.max, length.out = cp.N);  ## vector of pruning levels

    set.seed(1234);  ## gives reproducible results
    CV.K1 <- cvFolds(n = length(data$Class),  ## number of obs to be split into groups
                     K = K1  ## number of groups in which obs should be split
                     );  ## str(CV.K1); print(CV.K1);
    CV.K1$TrainSize <- rep(NA, K1);  ## train set size
    CV.K1$TestSize <- rep(NA, K1);  ## test set size

    ## Matrices for classification error count
    Error.train.K1 = matrix(rep(NA, times = K1), nrow = K1);
    Error.test.K1 = matrix(rep(NA, times = K1), nrow = K1);

    ## vectors to store cp.best and min.error.rate
    cp.best.vec <- rep(NA, K1);
    min.error.rate <- rep(NA, K1);

    ## par(mfrow = CalculatePlotLayout(K1));
    ## pdf(file = paste0(project.path, "/output/decision_tree_err_CV2.pdf"),
    ##     width = 1 * 12, height = 1 * 9, bg = "white");
    ## par(mfrow = c(2, 5));
    for (k1 in 1:K1) {  ## K1-fold splitting OUTER loop
        print(paste0(">> OUTER ", k1, " of ", K1));
        ## Extract training and test set
        data.train.k1 <- data[CV.K1$which != k1, ];
        data.test.k1 <- data[CV.K1$which == k1, ];
        CV.K1$TrainSize[k1] <- length(data.train.k1$Class);
        CV.K1$TestSize[k1] <- length(data.test.k1$Class);

        CV.K2 <- cvFolds(n = length(data.train.k1$Class),  ## number of obs to be split into groups
                         K = K2  ## number of groups in which obs should be split
                         );  ## str(CV.K2); print(CV.K2);
        CV.K2$TrainSize <- rep(NA, K2);  ## train set size
        CV.K2$TestSize <- rep(NA, K2);  ## test set size
        ## Matrices for classification error count
        Error.train.K2 = matrix(rep(NA, times = K2 * cp.N), nrow = K2);
        Error.test.K2 = matrix(rep(NA, times = K2 * cp.N), nrow = K2);

        for (k2 in 1:K2) {  ## K2-fold splitting INNER loop
            print(paste0(">> INNER ", k2, " of ", K2));
            ## Extract training and test set
            data.train.k2 <- data.train.k1[CV.K2$which != k2, ];
            data.test.k2 <- data.train.k1[CV.K2$which == k2, ];
            CV.K2$TrainSize[k2] <- length(data.train.k2$Class);
            CV.K2$TestSize[k2] <- length(data.test.k2$Class);

            ## Fit the decision tree to train data
            tree <- rpart(formula = fmla,
                          data = data.train.k2, method = "class",
                          control = rpart.control(
                              ## minsplit - the minimum number of observations that must exist in a
                              ## node in order for a split to be attempted.
                              minsplit = minsplit,
                              ## minbucket - the minimum number of observations in any terminal
                              ## ‘<leaf>’ node.
                              minbucket = 1,
                              ## cp - complexity parameter.  Any split that does not decrease the
                              ## overall lack of fit by a factor of ‘cp’ is not attempted.
                              cp = 0.0,
                              ## "information", "gini"
                              parms = list(split = "gini"),
                              ## max depth of any node in the final tree, root - depth 0
                              ## maxdepth = 20,
                              )
                          );  ## str(tree);
            ## Compute classification error
            for (n in 1:cp.N) { # For each pruning level
                mytree_pruned <- prune(tree = tree,
                                       cp = cp.vec[n]  ## cp parameter to which tree will be pruned
                                       );
                pr.clss.train <- predict(mytree_pruned, newdata = data.train.k2,
                                         type = "vector");
                pr.clss.test <- predict(mytree_pruned, newdata = data.test.k2,
                                        type = "vector");
                Error.train.K2[k2, n] = sum(as.integer(pr.clss.train) != as.integer(data.train.k2$Class));
                Error.test.K2[k2, n] = sum(as.integer(pr.clss.test) != as.integer(data.test.k2$Class));
            }  ## End of for n in 1:length(cp.vec)
        }  ## End of k2 in K2 INNER loop
        ## print(paste0(">> Error.train.K2 = "));
        ## print(Error.train.K2);
        ## print(paste0(">> Error.test.K2 = "));
        ## print(Error.test.K2);
        Err.Train.K2 <- 100 * colSums(Error.train.K2) / sum(CV.K2$TrainSize);
        Err.Test.K2 <- 100 * colSums(Error.test.K2) / sum(CV.K2$TestSize);
        ## print(Err.Train.K2);
        ## print(Err.Test.K2);

        min.err <- min(Err.Test.K2)[1];
        cp.best <- cp.vec[Err.Test.K2 == min.err][1];
        print(paste0(">> min(Err.Test.K2) = ", min.err, " at cp.best = ", cp.best));
        cp.best.vec[k1] <- cp.best;
        min.error.rate[k1] <- min.err;

        ## plot(cp.vec, Err.Train.K2, col = "blue", pch = 16, type = "b",
        ##      xlab = "cp", ylab = "Error rate, %", las = 1, cex.lab = 2,
        ##      cex.axis = 2, cex.main = 2, cex = 1.5,
        ##      ylim = c(min(Err.Train.K2), max(Err.Test.K2)),
        ##      main = paste0("k1 = ", k1, " of ", K1));
        ## lines(cp.vec, Err.Test.K2, col = "red", pch = 16, type = "b", cex = 1.5);
        ## abline(v = cp.best[1], lwd = 1, lty = 3, col = "red");
        ## abline(h = min(Err.Test.K2)[1], lwd = 1, lty = 3, col = "red");
        ## Fit the decision tree to train data
        tree <- rpart(formula = fmla,
                      data = data.train.k1, method = "class",
                      control = rpart.control(
                          ## minsplit - the minimum number of observations that must exist in a
                          ## node in order for a split to be attempted.
                          minsplit = minsplit,
                          ## minbucket - the minimum number of observations in any terminal
                          ## ‘<leaf>’ node.
                          minbucket = 1,
                          ## cp - complexity parameter.  Any split that does not decrease the
                              ## overall lack of fit by a factor of ‘cp’ is not attempted.
                          cp = cp.best,
                          ## "information", "gini"
                          parms = list(split = "gini"),
                          ## max depth of any node in the final tree, root - depth 0
                          ## maxdepth = 20,
                          )
                      );  ## str(tree);
        pr.clss.train <- predict(tree, newdata = data.train.k1, type = "vector");
        pr.clss.test <- predict(tree, newdata = data.test.k1, type = "vector");
        Error.train.K1[k1] = sum(as.integer(pr.clss.train) != as.integer(data.train.k1$Class));
        Error.test.K1[k1] = 100 * sum(as.integer(pr.clss.test) != as.integer(data.test.k1$Class)) / length(data.test.k1$Class);
    }  ## End of for k1 in K1 OUTER loop
    ## print(paste0(">> Cross-validated errors:"));
    ## print(Error.train.K1);
    ## print(Error.test.K1);

    ## plot(1:K1, cp.best.vec, col = "black", pch = 16, type = "b",
    ##      xlab = "Outer loop split", ylab = NA, las = 1, cex.lab = 2,
    ##      cex.axis = 2, cex.main = 2, cex = 1.5,
    ##      ylim = c(min(Err.Train.K2), max(Err.Test.K2)),
    ##      main = paste0("k1 = ", k1, " of ", K1));

    ## print(paste0(">> Cross-validated error rates:"));
    ## Err.Train.K1 <- 100 * colSums(Error.train.K1) / sum(CV.K1$TrainSize);
    ## Err.Test.K1 <- 1 * colSums(Error.test.K1) / sum(CV.K1$TestSize);
    ## print(Err.Train.K1);
    ## print(Err.Test.K1);

    return(list(
        k1 = 1:K1,
        cp.best = cp.best.vec,
        ErrRate = Error.test.K1
        ));
    ## par(mfrow = c(1, 2));
    ## plot(1:K1, Error.test.K1);
    ## plot(1:K1, cp.best.vec);
    ## dev.off();
    ## print(Error.train.K1 / CV.K1$TrainSize);
    ## print(Error.train.K2 / CV.K2$TrainSize);
    ## return(list(
    ##     ErrTest = Error.test.K1, ErrTestSize = sum(CV$TestSize),
    ##     ErrTrain = Error.train, ErrTrainSize = sum(CV$TrainSize),
    ##     N.groups = N.groups, K = K, minsplit = minsplit,
    ##     cp = prune
    ##     ));
} ## End of FindOptCPwCV2
################################################################################
