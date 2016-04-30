PlotDataBoxplot <- function(data) {
    pdf(paste0(project.path, "/output/data_boxplot.pdf"),
        width = 1 * 12, height = 9, bg = "white");
    ## mar = c(bottom, right, up, left), mgp = c(?, tick values, ticks)
    par(mar = c(5, 3, 2, 0.5), mgp = c(10, 1, 0));
    boxplot(data, las = 1, main = "Boxplot of attributes", cex = 2, lwd = 2,
            cex.axis = 2, cex.lab = 2, cex.main = 2, xaxt = "n", xlab = NA);
    labels <- kBinarizeAttributeNames[-8];
    axis(1, at = 1:length(labels), labels = FALSE);
    text(x = seq_along(labels), y = par("usr")[3] - 1, srt = 20, adj = 1,
         labels = labels, xpd = TRUE, cex = 2.0);
    dev.off();
}  ## End of PlotDataBoxplot

PlotDataHistogram <- function(data) {
    pdf(paste0(project.path, "/output/data_histogram.pdf"),
        width = 1 * 12, height = 9, bg = "white");
    ## mar = c(bottom, right, up, left), mgp = c(?, tick values, ticks)
    ## par(mar = c(5, 3, 2, 0.5), mgp = c(10, 1, 0));
    par(mfrow = c(2, 3));
    attrs <- c("Length", "Diameter", "Height", "WhlWght", "ShckdWght",
               "VscrWght", "ShllWght");
    for (i in 1:length(attrs)) {
        hist(data[[attrs[i]]], las = 1,
             main = paste0(attrs[i]), cex = 2, lwd = 2,
             cex.axis = 2, cex.lab = 2, cex.main = 2, cex.sub = 2, ylab = NA, xlab = NA);
    }
    dev.off();
}  ## End of PlotDataHistogram

################################################################################
DoAnomalyDetection <- function(K = 10, N.outliers = 20) {
    data <- LoadData(sex.binarize = TRUE, rings.rm = TRUE);
    data$Age <- NULL;
    data <- as.data.frame(scale(data)); str(data);
#### Visual inspection of data attributes
    PlotDataBoxplot(data);
    PlotDataHistogram(data);
    if (T) {
#### Gausian Kernel density estimator
    pdf(paste0(project.path, "/output/data_ranking.pdf"),
        width = 1 * 16, height = 9, bg = "white");
    ## mar = c(bottom, right, up, left), mgp = c(?, tick values, ticks)
    par(mar = c(5, 6, 2, 0.5), mgp = c(10, 1, 0));
    par(mfrow = c(2, 2));
    out.index <- 1:N.outliers; out.res <- c();
    ## cross-validate kernel width by leave-one-out-cross-validation
    ## automatically implemented in the script gausKernelDensity
    data <- as.matrix(data);
    widths = max(apply(data, 2, var)) * (2 ^ (-10:2)); # evaluate for a range of kernel widths
    logP <- rep(NA, times = length(widths));
    for (w in 1:length(widths)) {
        res <- gausKernelDensity(data, widths[w]);
        density <- res$density;
        log_density <- res$log_density;
        logP[w] = sum(log_density);
    }
    ## plot(widths, logP);
    val = max(logP); ind <- which.max(logP)
    width = widths[ind];
    print(paste("Optimal kernel width is", width));
    ## evaluate density for estimated width
    res <- gausKernelDensity(data, width); density <- res$density;
    ## Sort the densities
    sortres <- sort(density, index.return = TRUE);
    y <- sortres$x; i <- sortres$ix; out.res <- c(out.res, i[out.index]);
    ## Plot outlier scores
    barplot(y[out.index], main = "Gaussian Kernel Density: outlier score",
            las = 1, cex.names = 2, cex.main = 2, cex.axis = 2, names = i[out.index]);
    ## axis(1, at = out.index, labels = FALSE);
    ## text(x = out.index, y = par("usr")[3] - 1, srt = 20, adj = 1,
    ##      labels = i[out.index], xpd = TRUE, cex = 2.0);
    ## Plot possible outliers
    ## dev.new()
    ## par(mfrow=c(4, 5), oma=c(0, 0, 2,0))
    ## for(k in 1:20){
    ##     im <- X[i[k],]
    ##     dim(im) <- c(16,16)
    ##     image(t(im[16:1,]), col=gray((32:0)/32), main=k); 
    ## }
    ## title(main='Gaussian Kernel Density: Possible outliers', outer=TRUE)
#### K-nearest neighbor density estimator
## K = 5; Number of neighbors
## Find the k nearest neighbors
res <- get.knnx(data = data, query = data, k = K + 1);
idx <- res$nn.index; D <- res$nn.dist;
## Compute the density
density = 1/(rowSums(D[, 2:dim(D)[2]]) / K);
## Sort the densities
sortres <- sort(density, index.return = TRUE);
y <- sortres$x; i <- sortres$ix; out.res <- c(out.res, i[out.index]);

## Plot outlier scores
## dev.new()
    barplot(y[out.index], main = 'KNN density: outlier score',
            las = 1, cex.names = 2, cex.main = 2, cex.axis = 2, names = i[out.index]);
    ## axis(1, at = out.index, labels = FALSE);
    ## text(x = out.index, y = par("usr")[3] - 1, srt = 20, adj = 1,
    ##      labels = i[out.index], xpd = TRUE, cex = 2.0);
    ## Plot possible outliers
    ## dev.new()
    ## par(mfrow=c(4,5), oma=c(0,0,2,0))
    ## for(k in 1:20){
    ##    im <- X[i[k],]
    ##    dim(im) <- c(16,16)
    ##     image(t(im[16:1,]), col=gray((32:0)/32), main=k);
    ##   }
    ## title(main='KNN density: Possible outliers', outer=TRUE)

#### K-nearest neigbor average relative density
    ## Compute the average relative density
    avg_rel_density = density / (rowSums(matrix(density[idx[, 2:dim(idx)[2]]],
        nrow = dim(idx)[1])) / K);
    ## Sort the densities
    sortres <- sort(avg_rel_density, index.return = TRUE);
    y <- sortres$x; i <- sortres$ix; out.res <- c(out.res, i[out.index]);
    ## Plot outlier scores
    ## dev.new()
    barplot(y[out.index], main='KNN average relative density: outlier score',
            las = 1, cex.names = 2, cex.main = 2, cex.axis = 2, names = i[out.index]);
    ## axis(1, at = out.index, labels = FALSE);
    ## text(x = out.index, y = par("usr")[3] - 1, srt = 20, adj = 1,
    ##      labels = i[out.index], xpd = TRUE, cex = 2.0);
    ## # Plot possible outliers
    ## dev.new()
    ## par(mfrow=c(4,5), oma=c(0,0,2,0))
    ## for(k in 1:20){
    ##    im <- X[i[k],]
    ##    dim(im) <- c(16,16)
    ##     image(t(im[16:1,]), col=gray((32:0)/32), main=k);
    ##   }
    ## title(main='KNN average relative density: Possible outliers', outer=TRUE)

#### Distance to 5'th nearest neighbor outlier score
    ## Neighbor to use
    K = 10;
    ## Find the k nearest neighbors
    res <- get.knnx(data = data, query = data, k = K + 1);
    i <- res$nn.index; D <- res$nn.dist;
    ## Outlier score
    f = D[, K+1];
    ## Sort the outlier scores
    sortres <- sort(f, decreasing = TRUE, index.return = TRUE);
    y <- sortres$x; i <- sortres$ix; out.res <- c(out.res, i[out.index]);
## Plot kernel density estimate outlier scores
    barplot(y[out.index], main='Distance: outlier score',
            las = 1, cex.names = 2, cex.main = 2, cex.axis = 2, names = i[out.index]);
    ## axis(1, at = out.index, labels = FALSE);
    ## text(x = out.index, y = par("usr")[3] - 1, srt = 20, adj = 1,
    ##      labels = i[out.index], xpd = TRUE, cex = 2.0);
## # Plot possible outliers
## dev.new()
## par(mfrow=c(4,5), oma=c(0,0,2,0))
## for(k in 1:20){
##    im <- X[i[k],]
##    dim(im) <- c(16,16)
##     image(t(im[16:1,]), col=gray((32:0)/32), main=k);
##   }
    ## title(main='Distance: Possible outliers', outer=TRUE)
    dev.off();
    return(out.res);
}
}  ## End of DoAnomalyDetection
################################################################################

################################################################################
PlotOutliers <- function(out.res) {
    outlrs.ind <- as.numeric(names(sort(table(res),
                                        decreasing = T)[sort(table(res),
                                            decreasing = T) == 4]));
    print(outlrs.ind);
    data <- LoadData(sex.binarize = TRUE, rings.rm = TRUE);
    data$Age <- NULL;
    data <- as.data.frame(scale(data));
    SVD <- GetSVD(data); P <- MakePCAProjection(SVD);
    pdf(paste0(project.path, "/output/data_outliers.pdf"),
        width = 1 * 16, height = 9, bg = "white");
    ## mar = c(bottom, right, up, left), mgp = c(?, tick values, ticks)
    ## par(mar = c(5, 6, 2, 0.5), mgp = c(10, 1, 0));
    par(mfrow = c(1, 3), las = 1);
    eps.x <- 0.1; eps.y <- 0.1;
    plot(P[, 1], P[, 2], xlab = "PCA 1", ylab = "PCA 2", cex.axis = 2, cex.lab = 2);
    points(P[outlrs.ind, 1], P[outlrs.ind, 2], col = "red", lwd = 2);
    text(P[outlrs.ind, 1] - eps.x, P[outlrs.ind, 2] - eps.y, labels = outlrs.ind,
         col = 2, cex = 2);
    plot(P[, 1], P[, 3], xlab = "PCA 1", ylab = "PCA 3", cex.axis = 2, cex.lab = 2,
         cex.main = 2, main = "PCA projection of Abalones with outliers");
    points(P[outlrs.ind, 1], P[outlrs.ind, 3], col = "red", lwd = 2);
    text(P[outlrs.ind, 1] - eps.x, P[outlrs.ind, 3] - eps.y, labels = outlrs.ind,
         col = 2, cex = 2);
    plot(P[, 2], P[, 3], xlab = "PCA 2", ylab = "PCA 3", cex.axis = 2, cex.lab = 2);
    points(P[outlrs.ind, 2], P[outlrs.ind, 3], col = "red", lwd = 2);
    text(P[outlrs.ind, 2] - eps.x, P[outlrs.ind, 3] - eps.y, labels = outlrs.ind,
         col = 2, cex = 2);
    dev.off();
}  ## End of PlotOutliers
################################################################################
