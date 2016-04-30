source("src/clusterplot.R");
source("src/clusterval.R");

################################################################################
PlotHclustData <- function(P, hc.cl, class.col) {
    pdf(paste0(project.path, "/output/HClust_data.pdf"),
        width = 1 * 12, height = 9, bg = "white");
    ## mar = c(bottom, right, up, left), mgp = c(?, tick values, ticks)
    par(mar = c(4, 6, 2, 0.5), mgp = c(10, 1, 0));
    plot(P[, 1], P[, 2],  col = 9 - hc.cl, pch = 1, lwd = 6, xlab = NA, axes = F,
         ylab = NA, cex = 2);  ## plots cluster areals
    lines(P[, 1], P[, 2], type = "p", col = class.col, pch = 20, cex = 2);
    legend("right", legend = paste0("Cluster ", sort(unique(hc.cl))),
           fill = 9 - sort(unique(hc.cl)), ncol = 1, cex = 2);
    box();
    axis(side = 1, lwd = 1, line = 0.0, cex.axis = 2.0);
    title(xlab = paste0("PCA 1"), line = 2.5, cex.lab = 2.0);
    axis(side = 2, lwd = 1, line = 0.0, cex.axis = 2.0, las = 1);
    title(ylab = paste0("PCA 2"), line = 4, cex.lab = 2.0, adj = NULL);
    title(main = paste0("PCA projection of HCA of Abalones data"), line = 0.5,
          cex.main = 2);
    dev.off();
}  ## End of PlotHclustData
################################################################################

################################################################################
PlotDendrogram <- function(hc.fit, max.clust, hc.cl) {
    pdf(paste0(project.path, "/output/HClust_dendrogram.pdf"),
        width = 1 * 12, height = 9, bg = "white");
    ## mar = c(bottom, right, up, left), mgp = c(?, tick values, ticks)
    par(mar = c(1, 7, 2, 0.0), mgp = c(10, 1, 0));
    ## Plot dendrogram
    ##      plot(x, labels = NULL, hang = 0.1, check = TRUE,
    ##           axes = TRUE, frame.plot = FALSE, ann = TRUE,
    ##           main = "Cluster Dendrogram",
    ##           sub = NULL, xlab = NULL, ylab = "Height", ...)
    ##           x: an object of the type produced by ‘hclust’.

    ##     hang: The fraction of the plot height by which labels should hang
    ##           below the rest of the plot.  A negative value will cause the
    ##           labels to hang down from 0.

    ##    check: logical indicating if the ‘x’ object should be checked for
    ##           validity.  This check is not necessary when ‘x’ is known to
    ##           be valid such as when it is the direct result of ‘hclust()’.
    ##           The default is ‘check=TRUE’, as invalid inputs may crash R
    ##           due to memory violation in the internal C plotting code.

    ##   labels: A character vector of labels for the leaves of the tree.  By
    ##           default the row names or row numbers of the original data are
    ##           used.  If ‘labels = FALSE’ no labels at all are plotted.

    ## axes, frame.plot, ann: logical flags as in ‘plot.default’.

    ## main, sub, xlab, ylab: character strings for ‘title’.  ‘sub’ and ‘xlab’
    ##           have a non-NULL default when there's a ‘tree$call’.

    ##      ...: Further graphical arguments.  E.g., ‘cex’ controls the size
    ##           of the labels (if plotted) in the same way as ‘text’.

    plot(hc.fit, labels = FALSE, hang = -1, check = FALSE,
         axes = TRUE, frame.plot = FALSE, ann = TRUE,
         main = "Abalones data HCA dendrogram",
         sub = NULL, xlab = FALSE, ylab = "Height", cex = 2, cex.lab = 2,
         cex.main = 2, cex.axis = 2, las = 1, lwd = 3);
    rect.hclust(hc.fit, k = max.clust, border = 9 - sort(unique(hc.cl)),
                cluster = hc.cl);
    title(ylab = paste0("Height"), line = 5, cex.lab = 2.0, adj = NULL, density = 2);
    dev.off();
}  ## End of PlotDendrogram
################################################################################

################################################################################
PlotClusterValidityMeasures <- function(Class, hc.fit, max.clust) {
    pdf(paste0(project.path, "/output/HClust_validity.pdf"),
        width = 1 * 12, height = 9, bg = "white");
    ## mar = c(bottom, right, up, left), mgp = c(?, tick values, ticks)
    par(mar = c(4, 6, 2, 0.5), mgp = c(10, 1, 0));
    Entropy <- rep(NA, times = max.clust); Purity <- rep(NA, times = max.clust);
    Rand <- rep(NA, times = max.clust); Jaccard <- rep(NA, times = max.clust);

    for (i in 1:max.clust) {
        hc.cl <- cutree(tree = hc.fit, k = i, h = NULL);  ## str(hc.cl);
        res <- clusterval(y = Class, i = hc.cl);
        Entropy[i] <- res$Entropy; Purity[i] <- res$Purity;
        Rand[i] <- res$Rand; Jaccard[i] <- res$Jaccard;
    }

    cols <- c("blue", "green", "red", "lightblue");
    maxy <- max(c(-Entropy, Purity, Rand, Jaccard));
    miny <- min(c(-Entropy, Purity, Rand, Jaccard));
    ## plot(c(1,max.clust), c(miny, maxy), type = "n", );
    plot(1:max.clust, -Entropy, col = cols[1], type = "b", pch = 16,
         ## main = "Cluster validity", xlab = "Number of clusters", ylab = "",
         ylim = c(miny, maxy), lwd = 3, xlab = NA, axes = F,
         ylab = NA, cex = 2);
    lines(1:max.clust, Purity, col = cols[2], type = "b", pch = 16, lwd = 3, cex = 2);
    lines(1:max.clust, Rand, col = cols[3], type = "b", pch = 16, lwd = 3, cex = 2);
    lines(1:max.clust, Jaccard, col = cols[4], type = "b", pch = 16, lwd = 3, cex = 2);
    legend("left", legend = c("-Entropy", "Purity", "Rand", "Jaccard"),
           fill = cols, cex = 2.0);
    box();
    axis(side = 1, lwd = 1, line = 0.0, cex.axis = 2.0);
    title(xlab = paste0("Number of clusters"), line = 2.5, cex.lab = 2.0);
    axis(side = 2, lwd = 1, line = 0.0, cex.axis = 2.0, las = 1);
    ## title(ylab = paste0("PCA 2"), line = 4, cex.lab = 2.0, adj = NULL);
    title(main = paste0("Cluster validity of HCA of Abalones data"), line = 0.5,
          cex.main = 2);
    dev.off();
}  ## End of PlotClusterValidityMeasures
################################################################################

################################################################################
DoHClust <- function(N.groups = 5, cut = 1, max.clust  = 5) {
    data <- LoadData(sex.binarize = TRUE, rings.rm = TRUE);
    Class <- SplitAge(Age = data$Age, cut = cut, N.groups = N.groups);
    class.col <- as.integer(Class);
    ## print(head(Class)); print(head(class.col));
    ## print(levels(Class)); print(sort(unique(class.col)));
    data$Age <- NULL;  ## str(data);
    data <- as.data.frame(scale(data)); str(data);
    SVD <- GetSVD(data); P <- MakePCAProjection(SVD);

    ## Hierarchical clustering
    ## Compute hierarchical clustering
    hc.fit <- hclust(
        d = dist(x = data, method = "euclidian"),
        ## euclidean, maximum, manhattan, canberra, binary, minkowski
        method = "ward.D",
        ## ‘"ward.D"’, ‘"ward.D2"’, ‘"single"’, ‘"complete"’,
        ## ‘"average"’ (= UPGMA), ‘"mcquitty"’ (= WPGMA),
        ## ‘"median"’ (= WPGMC) or ‘"centroid"’ (= UPGMC)
        members = NULL
        ## NULL or vector with length size of ‘d’
        );
    str(hc.fit);  ## print(summary(hc.fit));

    ## Compute clustering by thresholding the dendrogram
    ## cut tree into groups of data
    ## k - desired number of groups, h - vector (scalar) of heights
    ## where the tree should be cut
    hc.cl <- cutree(tree = hc.fit, k = max.clust, h = NULL);  ## str(hc.cl);

    ## Plot data
    ## clusterplot(data, Class, i, main = "Hierarchical");
    ## PlotHclustData(P, hc.cl, class.col);

    ## Plot dendrogram
    ## PlotDendrogram(hc.fit, max.clust, hc.cl);

    ## Cluster validity measures
    PlotClusterValidityMeasures(Class, hc.fit, max.clust);
    return(list(
        Class = Class,
        HCA = hc.cl
        ));
}  ## End of DoHClust
################################################################################

################################################################################
CompareGMMandHclust <- function(res.gmm, res.hca) {
    ## str(res.gmm); str(res.hca);
    if (sum(res.gmm$Class != res.hca$Class) == 0) {
        gmm <- clusterval(y = res.gmm$Class, i = res.gmm$GMM);
        hca <- clusterval(y = res.hca$Class, i = res.hca$HCA);
    }
    str(gmm); str(hca);
    cols <- c("blue", "green", "red", "lightblue");
    pdf(paste0(project.path, "/output/Cluster_comparison.pdf"),
        width = 1 * 12, height = 9, bg = "white");
    ## mar = c(bottom, right, up, left), mgp = c(?, tick values, ticks)
    par(mar = c(2, 5, 2, 0.5), mgp = c(10, 1, 0));
    plot(x = 0:3, y = c(-1, -1, 1, 1), type = "n", axes = FALSE, ylab = NA,
         xlab = NA, main = "Cluster validity measures", cex.main = 2);
    box();
    axis(side = 1, at = 1:2, labels = c("GMM", "HCA"), cex.axis = 2);
    axis(side = 2, las = 1, cex.axis = 2);
    lines(x = rep(1, 4), y = c(-1, 1, 1, 1) * unlist(gmm), col = cols, pch = 1,
          type = "p", cex = 2, lwd = 4);
    lines(x = rep(2, 4), y = c(-1, 1, 1, 1) * unlist(hca), col = cols, pch = 2,
          type = "p", cex = 2, lwd = 4);
    legend("topright", legend = c("-Entropy", "Purity", "Rand", "Jaccard"),
           fill = cols, cex = 1.5);
    dev.off();
}  ## End of CompareGMMandHclust
################################################################################
