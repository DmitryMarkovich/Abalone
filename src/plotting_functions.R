source("src/plot_nnet.R");
################################################################################
PlotAgeGrouping <- function(n.groups = 5) {
    Age <- LoadData(binarize = FALSE)$Age;
    pdf(paste0(project.path, "/output/age_grouping.pdf"),
        width = 2 * 12, height = 9, bg = "white");
    par(mfrow = c(1, 3));

    ## par(mar = c(5, 5, 2, 0.5), mgp = c(10, 1, 0));
    barplot(table(Age),
            xlab = "Age, years", ylab = NA, las = 1,
            cex.lab = 2, cex.axis = 2, cex.names = 2, cex.main = 2,
            main = "a) Distribution of Age in years"
            );
    text(x = -2, 650, labels = paste0("Counts"),
         srt = 0, pos = 1, xpd = TRUE, adj = 1, cex = 2);

    ## par(mar = c(5, 5, 2, 0.5), mgp = c(10, 1, 0));
    barplot(
        ## table(cut2(x = Age, g = n.groups)),
        table(cut(x = Age, breaks = n.groups)),
        xlab = "Age, years", ylab = NA, las = 1,
        cex.lab = 2, cex.axis = 2, cex.names = 2, cex.main = 2,
        main = "b) Grouping of Age in years (using cut)"
        );
    text(x = -0.2, 3150, labels = paste0("Counts"),
         srt = 0, pos = 1, xpd = TRUE, adj = 1, cex = 2);

    barplot(
        table(cut2(x = Age, g = n.groups)),
        xlab = "Age, years", ylab = NA, las = 1,
        cex.lab = 2, cex.axis = 2, cex.names = 2, cex.main = 2,
        main = "c) Grouping of Age in years (using cut2)"
        );
    text(x = -0.2, 1250, labels = paste0("Counts"),
         srt = 0, pos = 1, xpd = TRUE, adj = 1, cex = 2);

    dev.off();
}  ## End of PlotAgeGrouping
################################################################################

################################################################################
PlotDecisionTreeCV2 <- function(res.dt) {
    pdf(paste0(project.path, "/output/decision_tree_CV2.pdf"), width = 1 * 12,
        height = 9, bg = "white");
    par(mfrow = c(1, 1));
    par(mar = c(5, 3, 2, 6), mgp = c(10, 1, 0));
    plot(res.dt$k1, res.dt$ErrRate, type = "b", pch = 16, cex = 1.5, las = 1,
         cex.axis = 2, cex.lab = 2, cex.main = 2);
    grid(col = "black", lty = 2, lwd = 1);
    abline(h = mean(res.dt$ErrRate), lty = 1, lwd = 3);
    text(x = median(res.dt$k1), y = 1.01 * mean(res.dt$ErrRate),
         label = signif(mean(res.dt$ErrRate), 3), cex = 2);

    par(new = TRUE);
    plot(res.dt$k1, res.dt$cp.best, ,type = "b", pch = 17, cex = 1.5, col = "blue",
         xaxt = "n", yaxt = "n", xlab = "", ylab = "", cex.axis = 2);
    axis(side = 4, las = 1, cex.axis = 2, col = "blue",
         col.ticks = "blue", col.axis = "blue");
    abline(h = mean(res.dt$cp.best), lty = 1, lwd = 3, col = "blue");
    text(x = median(res.dt$k1), y = 1.05 * mean(res.dt$cp.best),
         label = signif(mean(res.dt$cp.best), 3), cex = 2, col = "blue");

    title(xlab = "Number of cross-validation split", line = 3, cex.lab = 2.0);
    title(main = "Two-level cross-validation for decision tree", line = 0.5,
          cex.main = 2.0);
    legend("top", legend = c("Error rate, %", "cp.best"), pch = c(16, 17),
           lty = 1, cex = 2, col = c("black", "blue"), horiz = TRUE);
    dev.off();
}  ## End of PlotDecisionTreeCV2
################################################################################o

################################################################################
PlotkNNCV2 <- function(res.kNN) {
    pdf(paste0(project.path, "/output/k_nearest_neighbors_CV2.pdf"), width = 1 * 12,
        height = 9, bg = "white");
    par(mfrow = c(1, 1));
    par(mar = c(5, 3, 2, 6), mgp = c(10, 1, 0));
    plot(res.kNN$k1, res.kNN$ErrRate, type = "b", pch = 16, cex = 1.5, las = 1,
         cex.axis = 2, cex.lab = 2, cex.main = 2);
    grid(col = "black", lty = 2, lwd = 1);
    abline(h = mean(res.kNN$ErrRate), lty = 1, lwd = 3);
    text(x = median(res.kNN$k1), y = 1.01 * mean(res.kNN$ErrRate),
         label = signif(mean(res.kNN$ErrRate), 3), cex = 2);

    par(new = TRUE);
    plot(res.kNN$k1, res.kNN$kNN.best, ,type = "b", pch = 17, cex = 1.5, col = "blue",
         xaxt = "n", yaxt = "n", xlab = "", ylab = "", cex.axis = 2);
    axis(side = 4, las = 1, cex.axis = 2, col = "blue",
         col.ticks = "blue", col.axis = "blue");
    abline(h = mean(res.kNN$kNN.best), lty = 1, lwd = 3, col = "blue");
    text(x = median(res.kNN$k1), y = 1.05 * mean(res.kNN$kNN.best),
         label = signif(mean(res.kNN$kNN.best), 3), cex = 2, col = "blue");

    title(xlab = "Number of cross-validation split", line = 3, cex.lab = 2.0);
    title(main = "Two-layer cross-validation for k nearest neighbours", line = 0.5,
          cex.main = 2.0);
    legend("topright", legend = c("Error rate, %", "kNN.best"), pch = c(16, 17),
           lty = 1, cex = 2, col = c("black", "blue"), horiz = TRUE);
    dev.off();
}  ## End of PlotkNNCV2
################################################################################

################################################################################
PlotANNCV2 <- function(res.ANN) {
    pdf(paste0(project.path, "/output/artificial_neural_network_CV2.pdf"),
        width = 1 * 12, height = 9, bg = "white");
    par(mfrow = c(1, 1));
    par(mar = c(5, 3, 2, 6), mgp = c(10, 1, 0));
    plot(res.ANN$k1, res.ANN$ErrRate, type = "b", pch = 16, cex = 1.5, las = 1,
         cex.axis = 2, cex.lab = 2, cex.main = 2);
    grid(col = "black", lty = 2, lwd = 1);
    abline(h = mean(res.ANN$ErrRate), lty = 1, lwd = 3);
    text(x = median(res.ANN$k1), y = 1.01 * mean(res.ANN$ErrRate),
         label = signif(mean(res.ANN$ErrRate), 3), cex = 2);

    par(new = TRUE);
    plot(res.ANN$k1, res.ANN$NHiddenUnits.best, ,type = "b", pch = 17, cex = 1.5, col = "blue",
         xaxt = "n", yaxt = "n", xlab = "", ylab = "", cex.axis = 2);
    axis(side = 4, las = 1, cex.axis = 2, col = "blue",
         col.ticks = "blue", col.axis = "blue");
    abline(h = mean(res.ANN$NHiddenUnits.best), lty = 1, lwd = 3, col = "blue");
    text(x = median(res.ANN$k1), y = 1.05 * mean(res.ANN$NHiddenUnits.best),
         label = signif(mean(res.ANN$NHiddenUnits.best), 3), cex = 2, col = "blue");

    title(xlab = "Number of cross-validation split", line = 3, cex.lab = 2.0);
    title(main = "Two-layer cross-validation for artificial neural network",
          line = 0.5, cex.main = 2.0);
    legend("left", legend = c("Error rate, %", "NHiddenUnits.best"), pch = c(16, 17),
           lty = 1, cex = 2, col = c("black", "blue"), ncol = 1);
    dev.off();
}  ## End of PlotANNCV2
################################################################################



################################################################################
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
################################################################################

################################################################################
PlotCorrelationofContinuosAttributes <- function() {
    dat <- data[, c("AlkPhosphate", "Albumin", "Age", "Bilirubin", "SGOT",
                    "Protime")];
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
################################################################################

################################################################################
PlotDecisionTree <- function(tree) {
    pdf(file = paste0(project.path, "/output/decision_tree.pdf"),
        width = 20, height = 10, bg = "white");
    fancyRpartPlot(model = tree, main = "Decision Tree plot", sub = NA,
                   cex = 1.05, cex.main = 2, split.cex = 1.1, under.cex = 2,
                   nn.cex = 1.1,
                   max.auto.cex = 2, branch.lwd = 2, accept.cex = 2);
    dev.off();
}  ## End of PlotDecisionTree
################################################################################

################################################################################
PlotLogisticRegressionPrediction <- function(fit) {
    data <- fit$data[rowSums(is.na(fit$data)) > 0, ];
    plot(as.integer(data$Class), as.integer(data$Class));
    p = predict(w_est, newdata=x, type="response")
}  ## End of PlotLogisticRegressionPrediction
################################################################################

################################################################################
PlotErrDTwCV1 <- function(res) {
    pdf(file = paste0(project.path, "/output/decision_tree_err_CV1.pdf"),
        width = 12, height = 9, bg = "white");
    ## mar = c(bottom, right, up, left), mgp = c(?, tick values, ticks)
    par(mar = c(4, 5, 2, 0.5), mgp = c(10, 1, 0));
    plot(res$cp, 100 * colSums(res$ErrTest) / res$ErrTestSize, col = "red",
         type = "b", pch = 16, cex = 1.5, lwd = 3,
         ylim = 100 * c(min(colSums(res$ErrTrain) / res$ErrTrainSize),
             max(colSums(res$ErrTest) / res$ErrTestSize)),
         xlab = NA, ylab = NA, las = 1, axes = FALSE
         );
    box();
    grid(col = "black", lty = 2, lwd = 1);
    axis(side = 1, lwd = 1, line = 0.0, cex.axis = 2.0);
    title(xlab = paste0("Complexity parameter"), line = 2.5, cex.lab = 2.0);
    axis(side = 2, lwd = 1, line = 0.0, cex.axis = 2.0, las = 1);
    title(ylab = paste0("Error rate, %"), line = 3, cex.lab = 2.0, adj = NULL);
    title(main = paste0("Error rates as functions of complexity parameter"),
          line = 0.5, cex.main = 2);

    err.min <- min(colSums(res$ErrTest) / res$ErrTestSize);
    print(err.min);
    cp.best <- res$cp[colSums(res$ErrTest) == min(colSums(res$ErrTest))];
    print(cp.best);
    abline(h = 100 * err.min, col = "red", lwd = 3, lty = 2);
    abline(v = cp.best, col = "red", lwd = 3, lty = 2);
    lines(res$cp, 100 * colSums(res$ErrTrain) / res$ErrTrainSize, col = "blue",
          type = "b", pch = 16, cex = 1.5);

    legend("bottomright", legend = c("Training error", "Test error"),
           fill = c("blue", "red"), cex = 2);
    dev.off();
}  ## End of PlotErrDTwCV1
################################################################################

################################################################################
PlotErrNN <- function(res) {
    pdf(file = paste0(project.path, "/output/k_nearest_neighbours_err_CV1.pdf"),
        width = 12, height = 9, bg = "white");
    ## mar = c(bottom, right, up, left), mgp = c(?, tick values, ticks)
    par(mar = c(4, 6, 2, 2), mgp = c(10, 1, 0));
    err <- 100 * colSums(res$Err) / res$ErrSize;
    plot(err, col = "black",
         type = "b", pch = 16, cex = 1.5, lwd = 3,
         xlab = NA, ylab = NA, las = 1, axes = FALSE
         );
    abline(h = min(err)[1], lwd = 2, lty = 2);
    abline(v = (1:length(err))[err == min(err)[1]], lwd = 2, lty = 2);
    box();
    print(paste0(">> Min error rate = ", min(err)[1]));
    grid(col = "black", lty = 2, lwd = 1);
    axis(side = 1, lwd = 1, line = 0.0, cex.axis = 2.0);
    title(xlab = paste0("Number of neighbours"), line = 2.5, cex.lab = 2.0);
    axis(side = 2, lwd = 1, line = 0.0, cex.axis = 2.0, las = 1);
    title(ylab = paste0("Classification error rate, %"), line = 4,
          cex.lab = 2.0, adj = NULL);
    title(main = paste0("Classification error rate as a function of number of nearest neighbours"),
          line = 0.5, cex.main = 2);
    dev.off();
}  ## End of PlotErrNN
################################################################################

################################################################################
PlotErrNB <- function(res.NB) {
    pdf(file = paste0(project.path, "/output/naive_bayes_CV1.pdf"),
        width = 12, height = 9, bg = "white");
    ## mar = c(bottom, right, up, left), mgp = c(?, tick values, ticks)
    par(mar = c(4, 6, 2, 2), mgp = c(10, 1, 0));
    plot(res.NB$k1, res.NB$ErrRate, col = "black",
         type = "b", pch = 16, cex = 1.5, lwd = 3,
         xlab = NA, ylab = NA, las = 1, axes = FALSE
         );
    abline(h = mean(res.NB$ErrRate), lwd = 2, lty = 1);
    text(x = median(res.NB$k1), y = 1.01 * mean(res.NB$ErrRate),
         label = signif(mean(res.NB$ErrRate), 3), cex = 2);
    ## abline(v = (1:length(err))[err == min(err)[1]], lwd = 2, lty = 2);
    box();
    ## print(paste0(">> Min error rate = ", min(err)[1]));
    grid(col = "black", lty = 2, lwd = 1);
    axis(side = 1, lwd = 1, line = 0.0, cex.axis = 2.0);
    title(xlab = paste0("Number of cross-validation split"), line = 2.5, cex.lab = 2.0);
    axis(side = 2, lwd = 1, line = 0.0, cex.axis = 2.0, las = 1);
    title(ylab = paste0("Error rate, %"), line = 4,
          cex.lab = 2.0, adj = NULL);
    title(main = paste0("Two-layer cross-validation for naive Bayes"),
          line = 0.5, cex.main = 2);
    dev.off();
}  ## End of PlotErrNN
################################################################################

################################################################################
PlotMNMCV1 <- function(res.MNM) {
    pdf(file = paste0(project.path, "/output/multinomial_regression_CV1.pdf"),
        width = 12, height = 9, bg = "white");
    ## mar = c(bottom, right, up, left), mgp = c(?, tick values, ticks)
    par(mar = c(4, 6, 2, 2), mgp = c(10, 1, 0));
    plot(res.MNM$k1, res.MNM$ErrRate, col = "black",
         type = "b", pch = 16, cex = 1.5, lwd = 3,
         xlab = NA, ylab = NA, las = 1, axes = FALSE
         );
    abline(h = mean(res.MNM$ErrRate), lwd = 2, lty = 1);
    text(x = median(res.MNM$k1), y = 1.01 * mean(res.MNM$ErrRate),
         label = signif(mean(res.MNM$ErrRate), 3), cex = 2);
    ## abline(v = (1:length(err))[err == min(err)[1]], lwd = 2, lty = 2);
    box();
    ## print(paste0(">> Min error rate = ", min(err)[1]));
    grid(col = "black", lty = 2, lwd = 1);
    axis(side = 1, lwd = 1, line = 0.0, cex.axis = 2.0);
    title(xlab = paste0("Number of cross-validation split"), line = 2.5, cex.lab = 2.0);
    axis(side = 2, lwd = 1, line = 0.0, cex.axis = 2.0, las = 1);
    title(ylab = paste0("Error rate, %"), line = 4,
          cex.lab = 2.0, adj = NULL);
    title(main = paste0("Two-layer cross-validation for multinomial regression"),
          line = 0.5, cex.main = 2);
    dev.off();
}  ## End of PlotErrNN
################################################################################

################################################################################
CalculatePlotLayout <- function(N) {
    i1 <- round(sqrt(N)); i2 <- ceiling(sqrt(N));
    if (i1 * i2 >= N) {
        return(c(i1, i2));
    } else {
        return(c(i1 + 1, i2));
    }
}  ## End of CalculatePlotLayout
################################################################################

################################################################################
PlotANNModel <- function(model) {
    pdf(file = paste0(project.path, "/output/ANN_model.pdf"),
        width = 12, height = 9, bg = "white");
    plot.nnet(model);
    dev.off();
}  ## End of PlotANNModel
################################################################################

################################################################################
PlotClsfComparison <- function(res.comp) {
    pdf(file = paste0(project.path, "/output/Regression_comparison.pdf"),
        width = 12, height = 9, bg = "white");
    ## mar = c(bottom, right, up, left), mgp = c(?, tick values, ticks)
    par(mar = c(4, 6, 2, 2), mgp = c(10, 1, 0));
    plot(res.comp$K1, res.comp$LCL, col = "black",
         type = "b", pch = 16, cex = 1.5, lwd = 3,
         xlab = NA, ylab = NA, las = 1, axes = FALSE,
         ylim = c(min(res.comp$ANN), max(res.comp$LCL))
         ## ylim = c(19, 30)
         );
    ## abline(h = mean(res.comp$ErrRate), lwd = 2, lty = 1);
    ## text(x = median(res.comp$k1), y = 1.01 * mean(res.comp$ErrRate),
    ##      label = signif(mean(res.comp$ErrRate), 3), cex = 2);
    ## abline(v = (1:length(err))[err == min(err)[1]], lwd = 2, lty = 2);
    box();
    ## print(paste0(">> Min error rate = ", min(err)[1]));
    grid(col = "black", lty = 2, lwd = 1);
    axis(side = 1, lwd = 1, line = 0.0, cex.axis = 2.0);
    title(xlab = paste0("Number of cross-validation split"), line = 2.5, cex.lab = 2.0);
    axis(side = 2, lwd = 1, line = 0.0, cex.axis = 2.0, las = 1);
    title(ylab = paste0("Error rate, %"), line = 4,
          cex.lab = 2.0, adj = NULL);
    title(main = paste0("Classifier comparison with one-layer cross-validation"),
          line = 0.5, cex.main = 2);
    lines(res.comp$K1, res.comp$ANN, type = "b", cex = 1.5, col = 2, pch = 16);
    lines(res.comp$K1, res.comp$MNMR, type = "b", cex = 1.5, col = 3, pch = 16);
    Cl1Cl2 <- t.test(res.comp$ANN, res.comp$MNMR)$p.value;
    Cl1Cl0 <- t.test(res.comp$ANN, res.comp$LCL)$p.value;
    Cl2Cl0 <- t.test(res.comp$MNMR, res.comp$LCL)$p.value;
    legend("right",
           legend = c("LCl", paste0("ANN (p = ", signif(Cl1Cl0, 3), ")"),
               paste0("MNMR (p = ", signif(Cl2Cl0, 3), ")"),
               paste0("ANN vs MNMR (p = ", signif(Cl1Cl2, 3), ")")),
           col = c("black", 2, 3, 1), pch = c(16, 16, 16, NA),
           lty = c(1, 1, 1, NA), cex = 1.5, ncol = 1
           );
    dev.off();
}  ## End of PlotErrNN
################################################################################

################################################################################

################################################################################
PlotClsfComparison1 <- function(res.comp) {
  pdf(file = paste0(project.path, "/output/Regression_comparison1.pdf"),
      width = 12, height = 9, bg = "white");
  ## mar = c(bottom, right, up, left), mgp = c(?, tick values, ticks)
  par(mar = c(4, 6, 2, 2), mgp = c(10, 1, 0));
  plot(res.comp$K1, res.comp$ave, col = "black",
       type = "b", pch = 16, cex = 1.5, lwd = 3,
       xlab = NA, ylab = NA, las = 1, axes = FALSE,
       ylim = c(min(res.comp$ANN), max(res.comp$FLR))
       ## ylim = c(19, 30)
  );
  ## abline(h = mean(res.comp$ErrRate), lwd = 2, lty = 1);
  ## text(x = median(res.comp$k1), y = 1.01 * mean(res.comp$ErrRate),
  ##      label = signif(mean(res.comp$ErrRate), 3), cex = 2);
  ## abline(v = (1:length(err))[err == min(err)[1]], lwd = 2, lty = 2);
  box();
  ## print(paste0(">> Min error rate = ", min(err)[1]));
  grid(col = "black", lty = 2, lwd = 1);
  axis(side = 1, lwd = 1, line = 0.0, cex.axis = 2.0);
  title(xlab = paste0("Number of cross-validation split"), line = 2.5, cex.lab = 2.0);
  axis(side = 2, lwd = 1, line = 0.0, cex.axis = 2.0, las = 1);
  title(ylab = paste0("Residuals"), line = 4,
        cex.lab = 2.0, adj = NULL);
  title(main = paste0("Models comparison with one-layer cross-validation"),
        line = 0.5, cex.main = 2);
  lines(res.comp$K1, res.comp$ANN, type = "b", cex = 1.5, col = 2, pch = 16);
  lines(res.comp$K1, res.comp$FLR, type = "b", cex = 1.5, col = 3, pch = 16);
  Cl1Cl2 <- t.test(res.comp$ANN, res.comp$ave)$p.value;
  Cl1Cl0 <- t.test(res.comp$ANN, res.comp$FLR)$p.value;
  Cl2Cl0 <- t.test(res.comp$FLR, res.comp$ave)$p.value;
  legend("right",
         legend = c("AVE", paste0("ANN (p = ", signif(Cl1Cl0, 3), ")"),
                    paste0("FLR (p = ", signif(Cl2Cl0, 3), ")"),
                    paste0("ANN vs FLR (p = ", signif(Cl1Cl2, 3), ")")),
         col = c("black", 2, 3, 1), pch = c(16, 16, 16, NA),
         lty = c(1, 1, 1, NA), cex = 1.5, ncol = 1
  );
  dev.off();
}  ## End of PlotErrNN
################################################################################
