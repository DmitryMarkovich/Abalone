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
source('src/bmplot.R') # Used with the forward selection function
source('src/is.scalar.R') # Used with the forward selection function
source('src/forwardSelection.R') # Forward selection function
######################################## Main part
########### Plotting / summarizing raw data ##########
## data <- LoadData(binarize = FALSE);
########### PCA Analysis ##########
##data1 <- LoadData(fact = FALSE, na.rm = TRUE);  ## loads data for PCA
## SVD <- GetSVD(); P <- MakePCAProjection(SVD);
## PlotPCAVectors(SVD);
## PlotPCAVariations(SVD);
## PlotPCAProjectionParallelCoordinates(P, data);
########## Decision Tree ##########
## tree <- MakeDecisionTree(); PlotDecisionTree(tree);
########## Logistic regression ##########
## fit <- FitLogisticRegression(); print(summary(fit));
## predict(fit, data[31, ], type = "response");
#################### Classification part ####################
n.breaks = 5;
PlotAgeGrouping(n.breaks);
tree <- MakeDecisionTree(n.breaks); PlotDecisionTree(tree);
######################################## End of Main part
