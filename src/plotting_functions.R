PlotAgeGrouping <- function(n.groups = 5) {
    Age <- LoadData(binarize = FALSE)$Age;
    pdf(paste0(project.path, "/output/age_grouping.pdf"), width = 2 * 12, height = 9,
        bg = "white");
    par(mfrow = c(1, 2));

    ## par(mar = c(5, 5, 2, 0.5), mgp = c(10, 1, 0));
    barplot(table(Age),
            xlab = "Age, years", ylab = NA, las = 1,
            cex.lab = 2, cex.axis = 2, cex.names = 2, cex.main = 2,
            main = "Distribution of Age in years"
            );
    text(x = -2, 650, labels = paste0("Counts"),
         srt = 0, pos = 1, xpd = TRUE, adj = 1, cex = 2);

    ## par(mar = c(5, 5, 2, 0.5), mgp = c(10, 1, 0));
    barplot(table(cut2(x = Age, g = n.groups)),
            xlab = "Age, years", ylab = NA, las = 1,
            cex.lab = 2, cex.axis = 2, cex.names = 2, cex.main = 2,
            main = "Grouping of Age in years"
            );
    text(x = -0.2, 3250, labels = paste0("Counts"),
         srt = 0, pos = 1, xpd = TRUE, adj = 1, cex = 2);

    dev.off();
}  ## End of PlotAgeGrouping





PlotPCAVariations <- function(SVD) {
    N <- length(SVD$d);
    ## par(mar = c(4, 4, 0.2, 1.1), cex.lab = .95, cex.axis = .9,
    ##     mgp = c(2, .7, 0), tcl = -.3, las = 1);
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

}  ## End of PlotPCAVariations

PlotPCAProjectionParallelCoordinates <- function(P, data) {
    ## data loaded as integers with NA's
    obs <- P[1, ]; N <- length(obs);
    ## mar = c(bottom, right, up, left), mgp = c(?, tick values, ticks)
    par(mar = c(5, 5, 2, 0.5), mgp = c(10, 1, 0));
    plot(1:N, obs, col = kClassColors[data[1, 1]], pch = NA, type = "l",
         ylim = c(min(P), max(P)),  las = 1, axes = F, xlab = NA, ylab = NA,
         main = "PCA projection of Hepatitis data",
         cex.main = 2, cex.lab = 2);
    box();
    ## axis(side = 1, tck = -0.015, labels = NA);
    axis(side = 1, at = 1:N, labels = paste0("PCA", 1:N), cex.axis = 2, line = 0);
    title(xlab = "PCA components", line = 3.5, cex.lab = 2.5);

    axis(side = 2, las = 1, cex.axis = 2, line = 0);
    title(ylab = "Normalized PCA value", line = 2.5, cex.lab = 2.5);
    for (i in 2:length(P[, 1])) {
        obs <- P[i, ];
        lines(1:N, obs, col = kClassColors[data[i, 1]]);
    }
    legend("top", leg = kAttributeLevels$Class, col = kClassColors,
           pch = NA, lty = 1, lwd = 4, bg = "white", cex = 2, horiz = TRUE);
}  ## End of PlotDataParallelCoordinates

PlotPCAVectors <- function(SVD) {
    X <- SVD$v; N.pca <- ncol(X); N.attr <- nrow(X); cols <- rainbow(N.pca);
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
         pos = 2, xpd = TRUE, cex = 2);
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
}  ## End of PlotPCAVectors

PlotPCA1vsPCA2 <- function(P, PCA1 = 1, PCA2 = 2) {
    ## mar = c(bottom, right, up, left), mgp = c(?, tick values, ticks)
    par(mar = c(4, 5, 2, 0.5), mgp = c(10, 1, 0));
    plot(x = P[, PCA1], y = P[, PCA2], xlab = NA, axes = F, cex = 2,
         ylab = NA, col = kClassColors[data[, 1]], pch = 16,
         ylim = c(min(P[, PCA2]), 1.5 * max(P[, PCA2])));
    box();
    axis(side = 1, lwd = 1, line = 0.0, cex.axis = 2.0);
    title(xlab = paste0("PCA ", PCA1), line = 2.5, cex.lab = 2.0);
    axis(side = 2, lwd = 1, line = 0.0, cex.axis = 2.0, las = 1);
    title(ylab = paste0("PCA ", PCA2), line = 2.5, cex.lab = 2.0, adj = NULL);
    title(main = paste0("PCA projection of data"), line = 0.5, cex.main = 2);
    legend("top", legend = kAttributeLevels$Class, col = kClassColors,
           pch = 16, horiz = T, cex = 2.0, seg.len = 0);
}  ## End of PCA1vsPCA2

PlotCategoricalData <- function() {
    par(mfrow = c(2, 7))
    attr.names <- c("Class", "Sex", "Steroid", "Antivirals", "Fatigue", "Malaise",
                    "Anorexia", "LiverBig", "LiverFirm", "SpleenPalpable",
                    "Spiders", "Ascites", "Varices", "Histology");
    for (i in 1:length(attr.names)) {
        plot(data[, attr.names[i]], main = attr.names[i], las = 1, cex.axis = 2,
             cex.main = 2, cex.lab = 2, cex.names = 1.25);
    }
} ## End of PlotCategoricalData

PlotCorrelationofContinuosAttributes <- function() {
    dat <- data[, c("AlkPhosphate", "Albumin", "Age", "Bilirubin", "SGOT", "Protime")];
    cols <- data[, "Class"]; #cols <- cols[complete.cases(cols), ];
    cols <- 1 + as.integer(cols[complete.cases(dat)]);
    dat <- dat[complete.cases(dat), ];
    dat.r <- abs(cor(dat)); ## print(dat.r);
    dat.o <- order.single(dat.r);
    dat.col <- dmat.color(dat.r) # get colors
    cpairs(dat, dat.o, panel.colors = dat.col, gap = .5, main =
               "Continuous attributes ordered and colored by correlation",
           cex = 1.5, cex.axis = 2, cex.lab = 2, pch = 16, cex.main = 2, las = 1,
           col = cols);
}  ## End of PlotCorrelationofContinuosAttributes

PlotDecisionTree <- function(tree) {
    pdf(file = paste0(project.path, "/output/decision_tree.pdf"),
        width = 16, height = 9, bg = "white");
    fancyRpartPlot(model = tree, main = "Decision Tree plot", sub = NA,
                   cex = 1.05, cex.main = 2, split.cex = 1.1, under.cex = 2, nn.cex = 1.1,
                   max.auto.cex = 2, branch.lwd = 2, accept.cex = 2);
    dev.off();
}  ## End of PlotDecisionTree

PlotLogisticRegressionPrediction <- function(fit) {
    data <- fit$data[rowSums(is.na(fit$data)) > 0, ];
    plot(as.integer(data$Class), as.integer(data$Class));
    p = predict(w_est, newdata=x, type="response")
}  ## End of PlotLogisticRegressionPrediction
