source("src/clusterplot.R");
source("src/clusterval.R");
DoHClust <- function(N.groups = 5, cut = 1) {
    data <- LoadData(sex.binarize = TRUE, rings.rm = TRUE);
    Class <- SplitAge(Age = data$Age, cut = cut, N.groups = N.groups);
    class.col <- as.integer(Class);
    ## print(head(Class)); print(head(class.col));
    ## print(levels(Class)); print(sort(unique(class.col)));
    data$Age <- NULL;  ## str(data);
    data <- scale(data); str(data);
    SVD <- GetSVD(data); P <- MakePCAProjection(SVD);

    ## Plot results
    layout(mat = matrix(data = c(1, 5, 2, 5, 3, 6, 4, 7), nrow = 4, ncol = 2,
               byrow = TRUE)
           ## widths = c(3, 1), heights = c(1, 2)
           );
    plot(x = 1:length(SVD$d), y = 100 * cumsum(SVD$d ^ 2) / sum(SVD$d ^ 2),
         main = "Explained variation", xlab = "N PCA", ylab = "Variation, %",
         pch = 16, type = "b", cex = 2, lwd = 4);
    legend("bottomright", legend = levels(Class),
           fill = sort(unique(class.col)), cex = 1, horiz = TRUE);
    plot(P[, 1], P[, 2], col = class.col, xlab = "PCA 1", pch = 16,
         ylab = "PCA 2");
    plot(P[, 1], P[, 3], col = class.col, xlab = "PCA 1", pch = 16,
         ylab = "PCA 3");
    plot(P[, 2], P[, 3], col = class.col, xlab = "PCA 2", pch = 16,
         ylab = "PCA 3");

    ## Hierarchical clustering
    ## Compute hierarchical clustering
    hc.fit <- hclust(d = dist(x = data, method = "euclidian"),
                 ## euclidean, maximum, manhattan, canberra, binary, minkowski
                 method = "ward.D",
                 ## ‘"ward.D"’, ‘"ward.D2"’, ‘"single"’, ‘"complete"’,
                 ## ‘"average"’ (= UPGMA), ‘"mcquitty"’ (= WPGMA),
                 ## ‘"median"’ (= WPGMC) or ‘"centroid"’ (= UPGMC)
                 members = NULL
                 ## NULL or vector with length size of ‘d’
                 );
    str(hc.fit);
    ## print(summary(hc));
    ## Maximum number of clusters
    max.clust <- 3; ## length(levels(Class));
    ## Compute clustering by thresholding the dendrogram
    ## cut tree into groups of data
    ## k - desired number of groups, h - vector (scalar) of heights
    ## where the tree should be cut
    hc.cl <- cutree(tree = hc.fit, k = max.clust, h = NULL);  ## str(hc.cl);

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
    plot(hc.fit, labels = NULL, hang = -1, check = FALSE,
         axes = TRUE, frame.plot = FALSE, ann = TRUE,

         main = "Cluster Dendrogram",
         sub = NULL, xlab = NULL, ylab = "Height");
    rect.hclust(hc.fit, k = max.clust, border = "red");

    ## Plot data
    ## clusterplot(data, Class, i, main = "Hierarchical");
    plot(P[, 1], P[, 2],  col = 9 - hc.cl, pch = 1, lwd = 6);
    lines(P[, 1], P[, 2], type = "p", col = class.col, pch = 20);
    legend("right", legend = paste0("Cluster ", sort(unique(hc.cl))),
           fill = 9 - sort(unique(hc.cl)), ncol = 2);

    ## cluster validity measures
    Entropy = rep(NA, times = max.clust);
    Purity = rep(NA, times = max.clust);
    Rand = rep(NA, times = max.clust);
    Jaccard = rep(NA, times = max.clust);

    for (i in 1:max.clust) {
        hc.cl <- cutree(tree = hc.fit, k = i, h = NULL);
        str(hc.cl);
        res <- clusterval(Class, hc.cl);
        Entropy[i] <- res$Entropy;
        Purity[i] <- res$Purity;
        Rand[i] <- res$Rand;
        Jaccard[i] <- res$Jaccard;
    }

    cols <- c("blue", "green", "red", "lightblue")
    maxy <- max(c(-Entropy, Purity, Rand, Jaccard))
    miny <- min(c(-Entropy, Purity, Rand, Jaccard))
    plot(c(1,max.clust), c(miny, maxy), type="n", main="Cluster validity",
         xlab="Number of clusters", ylab="")
    lines(1:max.clust, -Entropy, col=cols[1]);
    lines(1:max.clust, Purity, col=cols[2]);
    lines(1:max.clust, Rand, col=cols[3]);
    lines(1:max.clust, Jaccard, col=cols[4]);
    legend("left", legend=c("Negative Entropy", "Purity", "Rand", "Jaccard"),
       fill = cols, cex = 1.0)
}  ## End of DoHClust
