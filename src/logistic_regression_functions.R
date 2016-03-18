FitLogisticRegression <- function() {
    data <- LoadData(fact = FALSE, fact.na = FALSE,
                     na.rm = FALSE)[, c("Class", "Malaise", "Ascites",
                         "Bilirubin", "Histology")];
    str(data);
    fit <- NULL;
    fit <- glm(formula = Class ~ Malaise + Ascites + Bilirubin + Histology,
               family = binomial(link = logit), data = data);31
    if (!is.null(fit)) {
        return(fit);
    } else {
        warning(">> Returning NULL!");
        return(NULL);
    }
}  ## End of FitLogisticRegression
