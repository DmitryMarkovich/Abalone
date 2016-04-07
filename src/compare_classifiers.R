CompareClassifiers <- function(N.groups = 5, K = 10, cut = 1) {
    data <- LoadData(binarize = FALSE); data$Rings <- NULL; data$Sex <- as.integer(data$Sex);
    if (cut == 1) {
        data$Class <- cut(data$Age, breaks = N.groups);  data$Age <- NULL;  # str(data);
    } else if (cut == 2) {
        data$Class <- cut2(data$Age, g = N.groups);  data$Age <- NULL;  # str(data);
    }

    fmla <- as.formula(c("Class ~ ",
                         paste0(kAttributeNames[kAttributeNames != "Rings"],
                                collapse = " + ")));  ## print(fmla);
    CV <- cvFolds(length(data$Class), K = K);
    ## set up vectors that will store sizes of training and test sizes
    CV$TrainSize <- c(); CV$TestSize <- c();

    ## Initialize variables
    Error_Clsf0 = rep(NA, times = K);
    Error_Clsf1 = rep(NA, times = K);
    Error_Clsf2 = rep(NA, times = K);

    ## For each crossvalidation fold
    for (k in 1:K) {
        print(paste('Crossvalidation fold ', k, '/', K, sep = ''));

        ## Extract the training and test set
        ## data.train <- data[CV$which != k, ];
        ## data.train.cl <- data[CV$which != k, ]$Class;
        ## data.test <- data[CV$which == k, ];
        ## data.test.cl <- data[CV$which == k, ]$Class;
        ## CV$TrainSize[k] <- length(data.train.cl);
        ## CV$TestSize[k] <- length(data.test.cl);

        ## Extract training and test set
        data.train <- data[CV$which != k, ]; data.train$Sex <- as.integer(data.train$Sex);
        ## data.train$Class <- NULL;  ## str(data.train);
        data.train.cl <- data[CV$which != k, ]$Class;  ## str(data.train.cl);
        CV$TrainSize[k] <- length(data.train.cl);

        data.test <- data[CV$which == k, ]; data.test$Sex <- as.integer(data.test$Sex);
        data.test$Class <- NULL;  ## str(data.test);
        data.test.cl <- data[CV$which == k,]$Class; ## print(summary(data.test.cl));
        CV$TestSize[k] <- length(data.test.cl);

########## Classifier NULL - predict all outputs to be the largest class in training data

        ## plot(table(data.train.cl));
        ## print(table(data.train.cl));
        ft <- table(data.train.cl);
        largest.class <- names(ft[which.max(ft)]);
        ## print(largest.class);
        test.cl.pr <- factor(rep(largest.class, CV$TestSize[k]), levels = levels(data.test.cl));
        ## str(test.cl.pr);
        ## str(data.test.cl);
        Error_Clsf0[k] <- 100 * sum(data.test.cl != test.cl.pr) / CV$TestSize[k];

########## Classifier 1
        ## ANN classifier with NHiddenUnits = 4
        model <- nnet(formula = fmla, data = data.train, size = 4);
        Y_test_est <- predict(object = model, newdata = data.test, type = "raw");
        y_test_est <- max_idx(Y_test_est);
        Error_Clsf1[k] <- 100 * sum(as.integer(data.test.cl) != y_test_est) / length(data.test.cl);

########## Classifier 2
        ## MNMR classifier
        model <- multinom(formula = fmla, data = data.train);
        data.test.est = predict(object = model, newdata = data.test, type = "probs");
        y_test_est <- apply(data.test.est, 1, which.max);
        Error_Clsf2[k] = 100 * sum(as.integer(data.test.cl) != y_test_est) / length(data.test.cl);

    }  ## End of for (k in K)
    ## plot(1:K, Error_Clsf0);
    ## lines(1:K, Error_Clsf1, col = 2);
    ## lines(1:K, Error_Clsf2, col = 3);
    return(list(
        K1 = 1:K,
        LCL = Error_Clsf0,
        ANN = Error_Clsf1,
        MNMR = Error_Clsf2
        ));
    ## Determine if classifiers are significantly different
    ## mfig('Error rates');
    ## errors <- data.frame(cbind(Error_Clsf1 / CV$TestSize, Error_Clsf2 / CV$TestSize) * 100);
    ## colnames(errors) <- c('Classifier 1', 'Classifier 2');
    ## boxplot(errors, ylab = "Error rate ()%");

    ## testresult <- t.test(Error_Clsf1, Error_Clsf2);
    ## if (testresult$p.value < 0.05) {
    ##     print('Classifiers are significantly different');
    ## } else {
    ##     print('Classifiers are NOT significantly different');
    ## }
}  ## End of CompareClassifiers


    ## # Logistic regression 
    ## w_est = glm(fmla, family=binomial(link="logit"), data=Xdatframe_train);
    ## y_est = predict.glm(w_est, newdata=Xdatframe_test, type="response")
    ## Error_LogReg[k] = sum(y_test!=(y_est>.5));

    ## # Decision tree
    ##     mytree <- rpart(fmla, data=Xdatframe_train,control=rpart.control(minsplit=100, minbucket=1, cp=0), parms=list(split='gini'), method="class")
    ##     Error_DecTree[k] = sum(classNames[y_test+1] != classNames[predict(mytree, newdat=Xdatframe_test, type="vector")]);
