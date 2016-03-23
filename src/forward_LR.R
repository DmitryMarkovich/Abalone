
# -------------------- Preliminary analysis section -------------------------

### Exploring variables relation ###
dat<- LoadData(binarize = F)

# Using a generalized additive model to explore variables relation
par(mfrow = c(3,3), mgp = c(2,0.7,0), mar = c(3,3,1,1))
lm<- gam(Age ~ s(Length)+s(Diameter)+s(Height)+s(WhlWght)+s(ShckdWght)+s(ShllWght)+s(VscrWght), data = dat )
plot(lm)
par(mfrow=c(1,1))
# gam model suggest that length, WhlWght and ShckdWght have a exponential relation with Age

# plot for report
par(mfrow = c(2,2), mgp = c(2,0.7,0), mar = c(3,3,1,1))
lm<- gam(Age ~ s(Length)+s(WhlWght)+s(ShckdWght), data = dat )
plot(lm)
par(mfrow=c(1,1))

## checkig normality with a simple linear regression
lm1<- lm( (Age)~(Sex+Length+I(Length^2)+Diameter+Height+WhlWght+I(WhlWght^2)+ShckdWght+I(ShckdWght^2)+ShllWght), data = dat )
# Check Cook's distance and normality assumptions
par(mfrow=c(2,1))
plot(lm1, which=c(1,4))
par(mfrow=c(1,1))
# Removing observation 2052 according to Cook's distance
dat1<- dat[-2052,]
# The plot shows a trompet shape, sugesting a log transformation of Age


# ------------------ Cross validation section -------------------------------

### Linear regression with Forward selection ###

## Linear function to use in inside the loop
funLinreg <- function(X_train, y_train, X_test, y_test){
  Xr <- data.frame(X_train)
  Xtest <- data.frame(X_test)
  if(dim(as.matrix(X_train))[2]!=0){
    xnam <- paste("X", 1:dim(as.matrix(X_train))[2], sep="")
    colnames(Xr) <- xnam
    colnames(Xtest) <- xnam
    (fmla <- as.formula(paste("log(y_train) ~ ", paste(xnam, collapse= "+"))))
  }else{
    xnam <- 1
    (fmla <- as.formula(paste("log(y_train) ~ ", paste(xnam, collapse= "+"))))
  }
  mod = lm(fmla, data=Xr)
  preds <- predict(mod, newdata = Xtest)
  sum((y_test-preds)^2)
}

## Setting data for the function

set.seed(23498) # for reproducibility
N<- length(dat1$Sex) # Total data
X<- dat1[,1:8]
# Includiing exponential terms
X$Length.2<- (dat1$Length)^2
X$WhlWght.2<- (dat1$WhlWght)^2
X$ShckdWght.2<- (dat1$ShckdWght)^2
attributeNames<- colnames(X)
y<- log(dat1$Age)
M<- dim(X)[2]
K<- 1 # folds in the cross validation outer level
CV <- cvFolds(N, K=K)

# set up vectors that will store sizes of training and test sizes
CV$TrainSize <- c()
CV$TestSize <- c()

# Initialize variables
Features <- matrix(rep(NA, times=K*M), nrow=K)
Error_train <- matrix(rep(NA, times=K), nrow=K)
Error_test <- matrix(rep(NA, times=K), nrow=K)
Error_train_fs <- matrix(rep(NA, times=K), nrow=K)
Error_test_fs <- matrix(rep(NA, times=K), nrow=K)

## For each crossvalidation fold (outer cross validation loop)
for(k in 1:K){
  paste('Crossvalidation fold ', k, '/', K, sep='')
  
  # Extract the training and test set
  X_train <- X[CV$which!=k, ];
  y_train <- y[CV$which!=k];
  X_test <- X[CV$which==k, ];
  y_test <- y[CV$which==k];
  CV$TrainSize[k] <- length(y_train)
  CV$TestSize[k] <- length(y_test)
  
  # Use 10-fold crossvalidation for sequential feature selection
  fsres <- forwardSelection(funLinreg, X_train, y_train, nfeats=10);
  
  # Save the selected features
  Features[k,] = fsres$binaryFeatsIncluded
  # Compute squared error without feature subset selection
  Error_train[k] = funLinreg(X_train, y_train, X_train, y_train);
  Error_test[k] = funLinreg(X_train, y_train, X_test, y_test);
  # Compute squared error with feature subset selection
  Error_train_fs[k] = funLinreg(X_train[,F], y_train, X_train[,F], y_train);
  Error_test_fs[k] = funLinreg(X_train[,F], y_train, X_test[,F], y_test);
  
  # Show variable selection history
  #    mfig(sprintf('(%d) Feature selection',k));
  I = length(fsres$costs) # Number of iterations
  par(mfrow=c(1,2))
  # Plot error criterion
  plot(fsres$costs, xlab='Iteration', ylab='Squared error (crossvalidation)', main='Value of error criterion');
  # Plot feature selection sequence
  bmplot(attributeNames, 1:I, fsres$binaryFeatsIncludedMatrix);
}

# Display results
print('Linear regression without feature selection:')
print(paste('- Training error:', sum(Error_train)/sum(CV$TrainSize)));
print(paste('- Test error:', sum(Error_test)/sum(CV$TestSize)));

print('Linear regression with sequential feature selection:');
print(paste('- Training error:', sum(Error_train_fs)/sum(CV$TrainSize)));
print(paste('- Test error:', sum(Error_test_fs)/sum(CV$TestSize)));

# Show the selected features
#dev.off()
par(mfrow=c(1,1))
bmplot(attributeNames, 1:K, Features, xlab='Crossvalidation fold', ylab='', main='Attributes selected')

