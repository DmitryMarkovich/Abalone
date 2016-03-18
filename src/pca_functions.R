GetSVD <- function() {
    X <- LoadData(fact = FALSE, na.rm = TRUE);
    X <- X[, 2:length(X)]; ## str(X);
    X$M <- as.integer(X$Sex == 1); X$F <- as.integer(X$Sex == 2);
    X$Sex <- NULL;  ## str(X);
    return(svd(apply(X = X, MARGIN = 2,
                     FUN = function(x) (x - mean(x, na.rm = TRUE)) /
                         sd(x, na.rm = TRUE))));
}  ## End of GetSVD

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

MakePCAProjection <- function(SVD) {
    return(SVD$u %*% diag(SVD$d));
}  ## End of MakePCAProjection
