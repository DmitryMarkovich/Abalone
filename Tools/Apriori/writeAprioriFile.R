writeAprioriFile <- function(X,filename){

# function to write a binary matrix X into a .txt file given by filename in
# order to analyze this file by the apriori.m script
#
# Usage
#   writeAprioriFile(X,filename)
#
# Input
#   X         N x M binary data matrix
#   filename  name of the .txt file to generate
#
  
  N=dim(X)[1]
  M=dim(X)[2]
  for(n in 1:N){
    ind=which(X[n,]==1);
    if(n==1){
      cat(paste(ind),file=filename,append=FALSE,sep=",")
    } else {
      cat(paste(ind),file=filename,append=TRUE,sep=",")
    }
    cat("",file=filename,append=TRUE,sep="\n")
  }
}