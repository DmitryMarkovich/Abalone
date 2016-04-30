################################################################################
GetSVD <- function(X) {
    ## X - matrix of explanatory variables
    ## X <- LoadData(sex.binarize = TRUE, rings.rm = TRUE);
    ## X$Age <- NULL; str(X);
    ## X <- X[, 2:length(X)]; ## str(X);
    ## X$M <- as.integer(X$Sex == 1); X$F <- as.integer(X$Sex == 2);
    ## X$Sex <- NULL;  ## str(X);
    return(svd(apply(X = X, MARGIN = 2,
                     FUN = function(x) (x - mean(x, na.rm = TRUE)) /
                         sd(x, na.rm = TRUE))));
}  ## End of GetSVD
################################################################################

################################################################################
PrintSVDVectors <- function(SVD, num = "all") {
    X <- SVD$v;
    colnames(X) <- paste0("PCA", 1:ncol(X));
    rownames(X) <- kAttributeNamesPCA;
    if (num == "all") {
        return(signif(X, 1));
    } else {
        return(signif(X[, 1:num], 1));
    }
}  ## End of PrintSVDVectors
################################################################################

################################################################################
MakePCAProjection <- function(SVD) {
    return(SVD$u %*% diag(SVD$d));
}  ## End of MakePCAProjection
################################################################################

GetSVD

################################################################################
PlotPCAVariations <- function(SVD) {
    N <- length(SVD$d);
    ## par(mar = c(4, 4, 0.2, 1.1), cex.lab = .95, cex.axis = .9,
    ##     mgp = c(2, .7, 0), tcl = -.3, las = 1);
    pdf(paste0(project.path, "/output/PCA_Variation.pdf"),
        width = 1 * 12, height = 9, bg = "white");
    par(mar = c(5, 5, 2, 0.5), mgp = c(10, 1, 0));
    plot(x = 1:N, y = 100 * cumsum(SVD$d ^ 2) / sum(SVD$d ^ 2),
         main = NA, xlab = NA, ylab = NA, axes = F, pch = 16, type = "b",
         cex = 2, lwd = 4);
    abline(h = 90, lty = 2); abline(v = 13, lty = 2);
    box();
    axis(side = 1, at = 1:N, labels = 1:N, cex.axis = 2.0);
    title(xlab = "Number of PCA components included", line = 3, cex.lab = 2.0);
    axis(side = 2, at = seq(20, 100, 10), labels = seq(20, 100, 10), las = 1, cex.axis = 2.0);
    title(ylab = "Explained variation, %", line = 3, cex.lab = 2.0);
    ## grid();
    dev.off();
}  ## End of PlotPCAVariations
################################################################################

################################################################################
PlotPCAProjectionParallelCoordinates <- function(P, Class, class.col) {
    ## data loaded as integers with NA's
    obs <- P[1, ]; N <- length(obs);
    pdf(paste0(project.path, "/output/PCA_parallel.pdf"),
        width = 1 * 12, height = 9, bg = "white");
    ## mar = c(bottom, right, up, left), mgp = c(?, tick values, ticks)
    par(mar = c(5, 5, 2, 0.5), mgp = c(10, 1, 0));
    plot(1:N, obs, col = class.col, pch = NA, type = "l",
         ylim = c(min(P), max(P)),  las = 1, axes = F, xlab = NA, ylab = NA,
         main = "PCA projection of Abalones data",
         cex.main = 2, cex.lab = 2);
    box();
    ## axis(side = 1, tck = -0.015, labels = NA);
    axis(side = 1, at = 1:N, labels = paste0("PCA", 1:N), cex.axis = 2, line = 0);
    title(xlab = "PCA components", line = 3.5, cex.lab = 2.5);

    axis(side = 2, las = 1, cex.axis = 2, line = 0);
    title(ylab = "Normalized PCA value", line = 2.5, cex.lab = 2.5);
    for (i in 2:length(P[, 1])) {
        obs <- P[i, ];
        lines(1:N, obs, col = class.col);
    }
    legend("topright", leg = levels(Class), col = sort(unique(class.col)),
           pch = NA, lty = 1, lwd = 4, bg = "white", cex = 1.5, horiz = FALSE);
    dev.off();
}  ## End of PlotDataParallelCoordinates
################################################################################

################################################################################
PlotPCAVectors <- function(SVD) {
    X <- SVD$v; N.pca <- ncol(X); N.attr <- nrow(X); cols <- rainbow(N.pca);
    pdf(paste0(project.path, "/output/PCA_Vectors.pdf"),
        width = 1 * 12, height = 9, bg = "white");
    ## mar = c(bottom, right, up, left), mgp = c(?, tick values, ticks)
    par(mar = c(6, 13, 1.5, 0.5), mgp = c(10, 1, 0));
    plot(X[, 1], 1:N.pca, type = "b", col = 1, xlim = c(-1, N.attr * 2 - 1),
         ylab = NA, axes = FALSE, pch = 1:N.pca, cex = 1.25);
    box();
    axis(side = 1, at = seq(0, 2 * N.pca - 1, 2), labels = FALSE);
    text(x = seq(0, 2 * N.pca - 1, 2), par("usr")[3] - 0.75, labels = paste0("PCA", 1:N.pca),
         srt = 35, pos = 1, xpd = TRUE, adj = 1, cex = 2);
    title(xlab = "Principal directions of PCA components", line = 4.5, cex.lab = 2.0);
    axis(side = 2, at = 1:N.pca, labels = FALSE);
    text(y = 1:N.pca, par("usr")[1] - 0.2, labels = kAttributeNamesPCA, srt = 0,
         pos = 2, xpd = TRUE, cex = 2);a
    title(ylab = "Attribute", line = 8.5, cex.lab = 2.0, adj = 1);
    abline(v = -1, lty = 2, lwd = 0.5); abline(v = 1, lty = 2, lwd = 0.5); abline(v = 0, lty = 1, lwd = 0.5);
    text(x = -1, y = N.pca, labels = "-1", cex = 1.5); text(x = 1, y = N.pca, labels = "1", cex = 1.5);
    text(x = 2 * N.pca - 3, y = 1, labels = "-1", cex = 1.5); text(x = 2 * N.pca - 1, y = 1, labels = "1", cex = 1.5);
    for (i in 2:N.attr) {
        lines(2 * (i - 1) + X[, i], 1:N.pca, type = "b", col = 1, pch = 1:N.pca,
              cex = 1.25);
        abline(v = 2 * (i - 1) - 1, lty = 2, lwd = 0.5);
        abline(v = 2 * (i - 1) + 1, lty = 2, lwd = 0.5);
        abline(v = 2 * (i - 1), lty = 1, lwd = 0.5);
    }
    dev.off();
}  ## End of PlotPCAVectors
################################################################################

################################################################################
PlotPCA1vsPCA2 <- function(P, PCA1 = 1, PCA2 = 2, Class, class.col) {
    pdf(paste0(project.path, "/output/PCA_Projection.pdf"),
        width = 1 * 12, height = 9, bg = "white");
    ## mar = c(bottom, right, up, left), mgp = c(?, tick values, ticks)
    par(mar = c(4, 6, 2, 0.5), mgp = c(10, 1, 0));
    plot(x = P[, PCA1], y = P[, PCA2], xlab = NA, axes = F, cex = 2,
         ylab = NA, col = class.col, pch = 16,
         ylim = c(min(P[, PCA2]), 1 * max(P[, PCA2])));
    box();
    axis(side = 1, lwd = 1, line = 0.0, cex.axis = 2.0);
    title(xlab = paste0("PCA ", PCA1), line = 2.5, cex.lab = 2.0);
    axis(side = 2, lwd = 1, line = 0.0, cex.axis = 2.0, las = 1);
    title(ylab = paste0("PCA ", PCA2), line = 4, cex.lab = 2.0, adj = NULL);
    title(main = paste0("PCA projection of Abalones data"), line = 0.5, cex.main = 2);
    legend("right", legend = levels(Class),
           fill = sort(unique(class.col)), cex = 2, ncol = 1);
    ## legend("top", legend = kAttributeLevels$Class, col = kClassColors,
    ##        pch = 16, horiz = T, cex = 2.0, seg.len = 0);
    dev.off();
}  ## End of PCA1vsPCA2
################################################################################
