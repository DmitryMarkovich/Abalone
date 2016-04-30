## file: main.R
## description: main script where we work with data
rm(list = ls());  ## cleares workspace
project.path <- getwd();  ## stores current directory
source("src/packages.R");  ## loads all additional packages
source("src/constants.R");  ## loads predefined constants
source("src/data_loading_functions.R");  ## loads data loading functions
source("src/plotting_functions.R");  ## loads various plotting functions
source("src/pca_functions.R");  ## loads pca functions
source("src/decision_trees_functions.R");  ## loads functions for decision trees
source("src/logistic_regression_functions.R");  ## loads functions for logistic regression
source("src/k_nearest_neighbours_functions.R");  ## loads functions for nearest neighbours
source("src/naive_bayes_functions.R");  ## loads functions for naive bayes
source("src/multinomial_regression_functions.R");  ## loads functions for multinomial regression
source("src/artificial_neural_network_functions.R");  ## loads functions for artificial neural network
source("src/compare_classifiers.R");  ## compares classifieers using one layer cross-validation
source('src/bmplot.R') # Used with the forward selection function
source('src/is.scalar.R') # Used with the forward selection function
source('src/forwardSelection.R') # Forward selection function
source("src/hierarchial_clustering_functions.R");  ## loads functions for HCA
source("src/gaussian_mixture_model_functions.R");  ## loads function for GMM
source("src/outlier_anomaly_detection.R");  ## loads function for outlier / anomaly detection
################################################################################
######################################## Main part
#################### Report 1
########### Plotting / summarizing raw data
## data <- LoadData(sex.binarize = TRUE, rings.rm = TRUE);
########### PCA Analysis
## data <- LoadData(sex.binarize = TRUE, rings.rm = TRUE);  ## loads data for PCA
## N.groups = 5; cut = 1;
## Class <- SplitAge(Age = data$Age, cut = cut, N.groups = N.groups);
## class.col <- as.integer(Class);
## data$Age <- NULL;
## SVD <- GetSVD(data);  P <- MakePCAProjection(SVD);
## PlotPCAVectors(SVD);
## PlotPCAVariations(SVD);
## PlotPCA1vsPCA2(P, 1, 2, Class, class.col);
## PlotPCAProjectionParallelCoordinates(P, Class, class.col);
#################### Report 2
########## Regression part
##### JLV part
## source("src/forward_LR.R");
##### Logistic regression
## fit <- FitLogisticRegression(); print(summary(fit));
## predict(fit, data[31, ], type = "response");
########## Classification part
## N.groups = 5;
####################
##### Decision trees
## tree <- MakeDecisionTree(); PlotDecisionTree(tree);
## PlotAgeGrouping(N.groups);
## tree <- MakeDecisionTree(N.groups, cp = 0.005, minsplit = 5, cut = 1);
## PlotDecisionTree(tree);
## res <- FindOptCPwCV1(N.groups, K = 10, cp.max = 0.01, cp.N = 20,
##                      minsplit = 5);  ## cp.best = 0.002632
## PlotErrDTwCV1(res);
## res <- FindOptCPwCV2(N.groups, K1 = 10, K2 = 10, cp.max = 0.01, cp.N = 20,
##                      minsplit = N.groups, cut = 2);
## PlotDecisionTreeCV2(res);
##### K nearest neighbours]
## res <- TestNNCV1(N.groups, K = 10, kNN.max = 60);
## PlotErrNN(res);
## res.kNN <- TestNNCV2(N.groups = 5, K1 = 10, K2 = 10, kNN.max = 60, cut = 2);
## PlotkNNCV2(res.kNN);
##### Naive Bayes
## res.NB <- FitNBCV1(N.groups, K1 = 10, prior = "empirical", cut = 1);
## PlotErrNB(res.NB);
##### Multinomial regression
## res.MNM <- FitMultinomCV1(N.groups, K = 10, cut = 2);
## PlotMNMCV1(res.MNM);
##### Artificial neural network
## FitANN(N.groups, NHiddenUnits = 4, cut = 1);
## PlotANNModel(model);
## res.ANN <- FitANNCV2(N.groups, NHiddenUnits.max = 5, K1 = 10, K2 = 10, cut = 2);
## PlotANNCV2(res.ANN);
##### Compare classifiers
## res.comp <- CompareClassifiers(N.groups, K = 10, cut = 2);
## PlotClsfComparison(res.comp);
#################### Report 3
N.groups = 5; cut = 1;
#### Gaussian mixture model
## TODO
## Analyze your data by the GMM and use cross-validation to estimate the number
## of clusters. (If you encounter the problem ”Ill-conditioned covariance” set
## the reg- ularization parameter Regularize of gmmdistribution.fit to a small
## constant, say 10 −6 ). Evaluate how well the clusters of the GMM model
## correspond to class labels using the cluster validity measures from last week
## by assigning observations to the cluster having highest probability.

## res <- DoGMMCV1(N.groups = N.groups, cut = cut, K.max = 20, K1 = 5);
## PlotGMMres(res);
## res.gmm <- FitGMM(N.groups = N.groups, cut = cut, K = 5);
##### Hierarchial clustering
## TODO
## Analyze your data by hierarchical clustering and try interpret the generated
## den- drogram. Use the cluster validity measures to evaluate how well the
## clusters reflect your labeled information at one of the levels of the
## dendrogram. If your data form a regression problem you can generate class
## labels by suitable thresholds of your data as you did when you analyzed the
## data as a classification problem.

## res.hca <- DoHClust(N.groups = N.groups, cut = cut, max.clust = 5);
#### Compare GMM and Hclust
## CompareGMMandHclust(res.gmm, res.hca);
#### Outlier / Anomaly detection
## TODO
## Apply the outlier scoring methods from last exercise in order to rank all the
## observations in terms of the Gaussian Kernel density (using the efficient
## leave-one-out density estimation approach), KNN density, KNN average relative
## density and dis- tance to K’th nearest neighbor for some suitable K. Discuss
## whether it seems there may be outliers in your data according to the four
## scoring methods.

res <- DoAnomalyDetection(K = 30, N.outliers = 20);
PlotOutliers(res);
######################################## End of Main part
################################################################################
