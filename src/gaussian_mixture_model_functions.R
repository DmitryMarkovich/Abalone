source("src/gmmposterior.R");
source("src/gausKernelDensity.R");
source("src/gmmposterior.Mclust.R");
source("src/gmmposterior.mixEM.R");
source("src/logLik.Mclust.R");
source("src/logLik.mixEM.R");

################################################################################
PlotGMMData <- function(P, gmm.cl, class.col, gmm.cl.centers) {
    pdf(paste0(project.path, "/output/GMM_data.pdf"),
        width = 1 * 12, height = 9, bg = "white");
    ## mar = c(bottom, right, up, left), mgp = c(?, tick values, ticks)
    par(mar = c(4, 6, 2, 0.5), mgp = c(10, 1, 0));
    plot(P[, 1], P[, 2],  col = 9 - gmm.cl, pch = 1, lwd = 6, xlab = NA, axes = F,
         ylab = NA, cex = 2);  ## plots cluster areals
    lines(P[, 1], P[, 2], type = "p", col = class.col, pch = 20, cex = 2);

    for (k in 1:dim(gmm.cl.centers)[1]) {
        points(gmm.cl.centers[k, 1], gmm.cl.centers[k, 2], pch = 4,
               col = 9 - k, #clustercols[((k - 1) %% Ncol) + 1],
               cex = 6, lwd = 4);
    }

    legend("right", legend = paste0("Cluster ", sort(unique(gmm.cl))),
           fill = 9 - sort(unique(gmm.cl)), ncol = 1, cex = 2);
    box();
    axis(side = 1, lwd = 1, line = 0.0, cex.axis = 2.0);
    title(xlab = paste0("PCA 1"), line = 2.5, cex.lab = 2.0);
    axis(side = 2, lwd = 1, line = 0.0, cex.axis = 2.0, las = 1);
    title(ylab = paste0("PCA 2"), line = 4, cex.lab = 2.0, adj = NULL);
    title(main = paste0("PCA projection of GMM of Abalones data"), line = 0.5,
          cex.main = 2);
    dev.off();
}  ## End of PlotGMMData
################################################################################

################################################################################
FitGMM <- function(N.groups = 5, cut = 1, K = 5) {
    data <- LoadData(sex.binarize = TRUE, rings.rm = TRUE);
    Class <- SplitAge(Age = data$Age, cut = cut, N.groups = N.groups);
    class.col <- as.integer(Class);
    ## print(head(Class)); print(head(class.col));
    ## print(levels(Class)); print(sort(unique(class.col)));
    data$Age <- NULL;  ## str(data);
    data <- as.data.frame(scale(data)); str(data);
    SVD <- GetSVD(data); P <- MakePCAProjection(SVD);

    model <- Mclust(data = data, G = K); # if using the package mclust to fit the model
    print(summary(model));
    ## print((model$parameters));
    ## Get clustering
    gmm.cl <- model$classification; # using the mclust package
    ##i = max_idx(model$posterior) # using the mixtools package

    ## Get cluster centers and project them on PCA
    gmm.cl.centers <- t(model$parameters$mean); # using the mclust package
    cmeans <- colMeans(data); V <- SVD$v;
    gmm.cl.centers <- sapply(1:ncol(gmm.cl.centers),
                             function(i) gmm.cl.centers[, i] - cmeans[i]) %*% V;
    ## print(gmm.cl.centers);

    ## Plot results and clustering
    PlotGMMData(P, gmm.cl, class.col, gmm.cl.centers);
    return(list(
        Class = Class,
        GMM = gmm.cl
        ));
}  ## End of FitGMM
################################################################################

################################################################################
DoGMMCV1 <- function(N.groups = 5, cut = 1, K.max = 2 * N.groups, K1 = 10) {
    data <- LoadData(sex.binarize = TRUE, rings.rm = TRUE);
    Class <- SplitAge(Age = data$Age, cut = cut, N.groups = N.groups);
    class.col <- as.integer(Class); data$Age <- NULL;  ## str(data);
    ## print(head(Class)); print(head(class.col));
    ## print(levels(Class)); print(sort(unique(class.col)));
    data <- as.data.frame(scale(data)); str(data);
    SVD <- GetSVD(data); P <- MakePCAProjection(SVD); print(head(P));
    PCA <- 1:10;  ## 1:10
    ## Gaussian mixture model
    if (K.max < 2) stop(">> K.max has to be >= 2!");
    KRange <- 2:K.max; T <- length(KRange); ## Range of K's to try
    ## Allocate variables
    BIC <- rep(NA, times = T); AIC <- rep(NA, times = T); CVE <- rep(0, times = T);
    bic <- rep(NA, times = T);
    ## Create crossvalidation partition for evaluation
    set.seed(1234);  ## for reproducibility
    CV <- cvFolds(length(class.col), K = K1);
    CV$NumTestSets <- K1;
    ## set up vectors that will store sizes of training and test sizes
    CV$TrainSize <- c(); CV$TestSize <- c();

    ## For each model order (number of clusters)
    for (t in 1:T) {
        ## Get the current K
        K <- KRange[t];

        ## Display information
        print(paste("Fitting GMM model with K = ", K));

        model <- Mclust(data = P[, PCA], G = K); # if using the package mclust to fit the model
        print(summary(model));
        ## str(model);
        ## Get BIC and AIC
        BIC[t] <- BIC(model);  AIC[t] <- AIC(model); bic[t] <- model$bic;
        if (!is.finite(BIC[t])) BIC[t] <- NA;
        if (!is.finite(AIC[t])) AIC[t] <- NA;
        ## BIC[t] <- model$bic;  AIC[t] <- model$loglik;  ## BIC AIC(model)

        ## For each crossvalidation fold
        for (k in 1:CV$NumTestSets) {
            print(paste0("CV ", k, " of ", CV$NumTestSets));
            ## Extract the training and test set
            data.train <- P[CV$which != k, PCA]; print(head(data.train));
            data.test <- P[CV$which == k, PCA]; print(head(data.test));

            ## Fit model to training set
            model.k <- Mclust(data = data.train, G = K) ## if using the package mclust to fit the model

            ## Evaluation crossvalidation error
            res <- gmmposterior(model, data.test);
            NLOGL <- res$ll; CVE[t] = CVE[t] + NLOGL;
            if (!is.finite(CVE[t])) CVE[t] <- NA;
        }  ## End of for k
    }  ## End of for t
    return(list(
        K = KRange,
        BIC = BIC,
        AIC = AIC,
        bic = bic,
        CVE = 2 * CVE
        ));
}  ## End of DoGMMCV1

PlotGMMres <- function(res) {
    pdf(file = paste0(project.path, "/output/GMMCV1.pdf"),
        width = 12, height = 9, bg = "white");
    ## mar = c(bottom, right, up, left), mgp = c(?, tick values, ticks)
    par(mar = c(4, 8, 2, 2), mgp = c(10, 1, 0)); options(scipen = -2);
    ## Plot results
    cols <- c("blue", "darkgreen", "red", "violet");
    ylims <- c(min(c(res$BIC, res$AIC, res$CVE), na.rm = TRUE),
               max(c(res$BIC, res$AIC, res$CVE), na.rm = TRUE));
    plot(res$K, res$BIC, main = "GMM for Abalones data", col = cols[1], type = "b",
         xlab = "Number of clusters in GMM model", cex = 2, cex.lab = 2,
         cex.axis = 2, cex.main = 2, las = 1, ylim = ylims);
    title(xlab = paste0("Number of clusters in GMM model"), line = 2.5, cex.lab = 2.0);
    lines(res$K, res$AIC, col = cols[2], type = "b", cex = 2);
    lines(res$K, res$CVE, col = cols[3], type = "b", cex = 2);
    ## lines(res$K, -res$bic, col = "violet", type = "b", cex = 2);
    legend("top", legend = c("BIC", "AIC", "CVE"), fill = cols,
           cex = 1.5);
    ## abline(h = min(res$BIC, na.rm = TRUE)[1], col = cols[1]);
    dev.off();
}  ## End of PlotGMMres
