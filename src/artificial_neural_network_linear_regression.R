# -------------------- Preparing data for ANN -------------------------

### Exploring variables relation ###
dat<- LoadData(binarize = F)
dat$Sex<- as.integer(dat$Sex)
dat$Rings <- NULL;

## setting variables to use in the ANN
# separating response and attributes
var.names<- colnames(dat)
exclude<- c("Rings","Age")
X<- subset(dat, select = !(var.names %in% exclude))
y<- dat$Age
attributeNames<- colnames(X)
N<- length(y)

## ANN algorithm

# K-fold crossvalidation
K = 3;
#set.seed(1234) # for reproducibility
set.seed(50243) # for reproducibility
CV <- cvFolds(N, K=K)
# set up vectors that will store sizes of training and test sizes
CV$TrainSize <- c()
CV$TestSize <- c()

# Parameters for neural network classifier
NHiddenUnits = 1;  # Number of hidden units
NTrain = 2; # Number of re-trains of neural network

# Variable for classification error
Error = rep(NA, times=K)
(fmla <- as.formula(paste("log(y_train) ~ ", paste(attributeNames, collapse= "+"))))

# For each crossvalidation fmla
for(k in 1:K){
  print(paste('Crossvalidation fold ', k, '/', K, sep=''))
  
  # Extract training and test set
  X_train <- X[CV$which!=k, ];
  y_train <- y[CV$which!=k];
  X_test <- X[CV$which==k, ];
  y_test <- y[CV$which==k];
  CV$TrainSize[k] <- length(y_train)
  CV$TestSize[k] <- length(y_test)
  
  X_traindf <- data.frame(X_train)
  colnames(X_traindf) <- attributeNames
  X_testdf <- data.frame(X_test)
  colnames(X_testdf) <- attributeNames
  
  # Fit neural network to training set
  MSEBest = Inf;
  for(t in 1:NTrain){
    netwrk = neuralnet(fmla, X_traindf, hidden=NHiddenUnits, act.fct='tanh',
                       linear.output=TRUE, err.fct='sse', rep = NTrain);
    mse <- sum((unlist(netwrk$net.result)-y_train)^2)
    
    if(mse<MSEBest){
      bestnet <- netwrk
      MSEBest <- mse
    }
  }
  # Predict model on test data
  
  computeres <- compute(netwrk, X_testdf,rep = 1)
  y_test_est = unlist(computeres$net.result)
  
  # Compute error rate
  Error[k] = sum((y_test-y_test_est)^2); # Count the number of errors
  
}

# Print the error rate
print(paste('Mean Sum of Squares Error (MSSE): ', sum(Error)/sum(CV$TestSize), sep=''));

# Display the trained network (given for last cross-validation fold)
plot(bestnet)

pdf("ANN_LR_Error.pdf", width = 12, height = 9)
barplot(log(Error), xlab='Number of cross-validation split', names.arg = c("k=1","k=2","k=3"), las=1,
        main="Residual error", cex.lab = 2, cex.axis = 1, cex.main=2)
dev.off()

