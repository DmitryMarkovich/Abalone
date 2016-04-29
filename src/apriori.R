### Apriori script ### 

# upload apriori algotihm
source('Tools/Apriori/writeAprioriFile.R')
source('Tools/Apriori/runApriori.R')

AprioriResutls <- function(minSup, minConf) {
  # Loading data
  dat <- LoadData(sex.binarize = TRUE, rings.rm = TRUE);
  
  # Binarizing data
  binary.dat<- BinarizeByMedian(dat);
  
  # Saving data in the right format to use it with Apriori
  writeAprioriFile(binary.dat, "Apriori_input.txt");
  
  # Running Apriori algorithm
  results <- runApriori("Apriori_input.txt", minSup, minConf);
  
  ## Changin names
  names<- colnames(binary.dat);
  codes<- as.character(1:length(names));
  
  # Changing numbers with more than one character
  for (k in 1:2) {
    for (j in 1:length(results[[k]])) {
      for (i in 1:length(names)) {
        if(nchar(codes[i]) != 1) {
          results[[k]][j] <- gsub(codes[i], names[i], results[[k]][j]);
        } else{ 
          results
        } ## end if
      } ## end for
    } ## end for
  } ## end for
  
  # Changing one character numbers
  for (k in 1:2) {
    for (j in 1:length(results[[k]])) {
      for (i in 1:length(names)) {
        if(nchar(codes[i]) == 1) {
          results[[k]][j] <- gsub(paste0(codes[i], " "), paste0(names[i], " "), results[[k]][j]);
          results[[k]][j] <- gsub(paste0(codes[i],'\\['), paste0(names[i], '\\['), results[[k]][j]);
        } else{ 
          results
        } ## end if
      } ## end for
    } ## end for
  } ## end for
  (results);
  return(results);
} ## end function

