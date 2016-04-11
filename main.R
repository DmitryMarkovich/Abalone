## file: main.R
## description: main script where we work with data
rm(list = ls());  ## cleares workspace
project.path <- getwd();  ## stores current directory
source("src/packages.R");  ## loads all additional packages
source("src/constants.R");  ## loads predefined constants
source("src/data_loading_functions.R");  ## loads data loading functions
source("src/plotting_functions.R");  ## loads various plotting functions
## source("src/pca_functions.R");  ## loads pca functions
source("src/decision_trees_functions.R");  ## loads functions for decision trees
source("src/logistic_regression_functions.R");  ## loads functions for logistic regression
source("src/k_nearest_neighbours_functions.R");  ## loads functions for nearest neighbours
source("src/naive_bayes_functions.R");  ## loads functions for naive bayes
source("src/multinomial_regression_functions.R");  ## loads functions for multinomial regression
source("src/artificial_neural_network_functions.R");  ## loads functions for artificial neural network
source("src/compare_classifiers.R");  ## compares classifieers using one layer cross-validation
################################################################################
source('src/bmplot.R') # Used with the forward selection function
source('src/is.scalar.R') # Used with the forward selection function
source('src/forwardSelection.R') # Forward selection function

######################################## Main part
########### Plotting / summarizing raw data
## data <- LoadData(binarize = FALSE);
########### PCA Analysis
##data1 <- LoadData(fact = FALSE, na.rm = TRUE);  ## loads data for PCA
## SVD <- GetSVD(); P <- MakePCAProjection(SVD);
## PlotPCAVectors(SVD);
## PlotPCAVariations(SVD);
## PlotPCAProjectionParallelCoordinates(P, data);
########## Decision Tree
## tree <- MakeDecisionTree(); PlotDecisionTree(tree);
########## Logistic regression
## fit <- FitLogisticRegression(); print(summary(fit));
## predict(fit, data[31, ], type = "response");
########################################
#################### Classification part
## N.groups = 5;
####################
########## Decision trees
## PlotAgeGrouping(N.groups);
## tree <- MakeDecisionTree(N.groups, cp = 0.005, minsplit = 5, cut = 1);
## PlotDecisionTree(tree);
## res <- FindOptCPwCV1(N.groups, K = 10, cp.max = 0.01, cp.N = 20,
##                      minsplit = 5);  ## cp.best = 0.002632
## PlotErrDTwCV1(res);
## res <- FindOptCPwCV2(N.groups, K1 = 10, K2 = 10, cp.max = 0.01, cp.N = 20,
##                      minsplit = N.groups, cut = 2);
## PlotDecisionTreeCV2(res);
########## K nearest neighbours]
## res <- TestNNCV1(N.groups, K = 10, kNN.max = 60);
## PlotErrNN(res);
## res.kNN <- TestNNCV2(N.groups = 5, K1 = 10, K2 = 10, kNN.max = 60, cut = 2);
## PlotkNNCV2(res.kNN);
########## Naive Bayes
## res.NB <- FitNBCV1(N.groups, K1 = 10, prior = "empirical", cut = 1);
## PlotErrNB(res.NB);
########## Multinomial regression
## res.MNM <- FitMultinomCV1(N.groups, K = 10, cut = 2);
## PlotMNMCV1(res.MNM);
########## Artificial neural network
## FitANN(N.groups, NHiddenUnits = 4, cut = 1);
## PlotANNModel(model);
## res.ANN <- FitANNCV2(N.groups, NHiddenUnits.max = 5, K1 = 10, K2 = 10, cut = 2);
## PlotANNCV2(res.ANN);
########## Compare classifiers
## res.comp <- CompareClassifiers(N.groups, K = 10, cut = 2);
## PlotClsfComparison(res.comp);
########## JLV part
## source("src/forward_LR.R");
######################################## End of Main part
################################################################################
