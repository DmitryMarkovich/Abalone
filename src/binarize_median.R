### Binarize data by median ###

BinarizeByMedian <- function(dat) {
  
  # Checking data names
  dat.names <- colnames(dat)
  
  # Creating empty data frame
  n.obs <- dim(dat)[1]
  dat.binary <- data.frame(double(n.obs))
  
  # binarize numeric attributes based on median value
  for (i in 1:length(dat.names)) {
    if (sort(unique(dat[, i]))[2] == 1) {
      col.val <- dat[, i];
      col.val <- as.data.frame(col.val);
      colnames(col.val) <- dat.names[i];
      dat.binary <- cbind(dat.binary, col.val);
      
    } else {
      col.l <- as.integer(dat[, i] < median(dat[, i]));
      col.l<- as.data.frame(col.l);
      colnames(col.l)<- paste(dat.names[i],".L", sep = "");
      dat.binary <- cbind(dat.binary, col.l);
      col.h <- as.integer(dat[, i] >= median(dat[, i]));
      col.h<- as.data.frame(col.h);
      colnames(col.h)<- paste(dat.names[i],".H", sep = "");
      dat.binary <- cbind(dat.binary, col.h);
    }  ## End of if()
  }  ## End of for()
  dat.binary <- dat.binary[,-1]
  str(dat.binary)
  return(dat.binary)
} ## End function

