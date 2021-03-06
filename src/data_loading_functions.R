## file: data_loading_functions.R
## description: defines a collection of functions used to load the data
################################################################################
LoadData <- function(sex.binarize = TRUE, rings.rm = FALSE) {
    if (file.exists(paste0("./data/", kDataFname))) {
        data <- read.table(file = paste0("./data/", kDataFname), header = FALSE,
                           sep = ",", col.names = kAttributeNames);
        if (sex.binarize) {
            ## split Sex attribute into Male, Female, Infant
            data$Male <- as.integer(data$Sex == "M");
            data$Female <- as.integer(data$Sex == "F");
            data$Infant <- as.integer(data$Sex == "I");
            data$Sex <- NULL;
        }
        data$Age <- 1.5 + data$Rings;
        if (rings.rm) {
            data$Rings <- NULL;
        }
        return(data);
    } else {
        warning(paste0(">> ", kDataFname,  " was not found, returning NULL!"));
        return(NULL);
    }
}  ## End of LoadData
################################################################################

################################################################################
SplitAge <- function(Age, cut = 1, N.groups = 5) {
    if (cut == 1) {
        return(cut(Age, breaks = N.groups));
    } else if (cut == 2) {
        return(cut2(Age, g = N.groups));
    } else {
        stop(">> Unknown cut value!");
    }
}  ## End of SplitAge
################################################################################

#################### Legacy code
## if (fact) {  ## turn integers in columns to corresponding factors, preserves NA's!
##     for (i in 1:length(kAttributeNames)) {
##         if (!any(is.na(kAttributeLevels[[kAttributeNames[[i]]]]))) {
##             if (any(is.na(data[[i]]))) {
##                 if (fact.na) {  ## NA is factorized as string "NA"
##                     data[[i]] <- factor(data[[i]],
##                                         labels = c(kAttributeLevels[[kAttributeNames[[i]]]], "NA"),
##                                         exclude = NULL);
##                 } else {  ## default treatment of NA's by R
##                     data[[i]] <- factor(data[[i]],
##                                         labels = kAttributeLevels[[kAttributeNames[[i]]]],
##                                         exclude = NA);
##                 }
##             } else {
##                 data[[i]] <- factor(data[[i]],
##                                     labels = c(kAttributeLevels[[kAttributeNames[[i]]]]));
##             }
##         }
##     }
##     return(data);
## } else if (na.rm) {  ## leaves data as integers, removes NA's!
##     for (i in 1:length(kAttributeNames)) {
##         if (is.integer(data[[i]])) {
##             if (min(data[[i]], na.rm = TRUE) == 1) {
##                 data[[i]] <- data[[i]] - 1;
##             }
##         }
##     }
##     return(data[complete.cases(data), ]);
## } else {  ## returns integers with NA's
##     for (i in 1:length(kAttributeNames)) {
##         if (is.integer(data[[i]])) {
##             if (min(data[[i]], na.rm = TRUE) == 1) {
##                 data[[i]] <- data[[i]] - 1;
##             }
##         }
##     }
##     return(data);
## }
