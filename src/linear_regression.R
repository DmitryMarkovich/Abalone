## Linear Regression function for Abaloon data ##

LinearRegression<- function(){
  dat<- LoadData(binarize = F)
  
  # Exploring variables relation
  plot(Age~Length, data = dat)
  par(mfrow = c(2,3), mgp = c(2,0.7,0), mar = c(3,3,1,1))
  lm<- gam(Age ~ s(Length)+s(Diameter)+s(Height)+s(WhlWght)+s(ShckdWght)+s(ShllWght), data = dat )
  plot(lm)
  par(mfrow=c(1,1))
  
  # applying linear regression to predict abalon's age
  lm1<- lm( (Age)~(Sex+Length+I(Length^2)+Diameter+Height+WhlWght+I(WhlWght^2)+ShckdWght+I(ShckdWght^2)+ShllWght), data = dat )
  # checking initial model residuals
  par(mfrow=c(2,2))
  plot(lm1)
  par(mfrow=c(1,1))
  # Observation 2052 was identify like an outlier by the cook's distance and was removed
  dat1<- dat[-2052,]
  
  # Doing log transformation of the Age attribute
  lm1<- lm( log(Age)~(Sex+Length+I(Length^2)+Diameter+Height+WhlWght+I(WhlWght^2)+ShckdWght+I(ShckdWght^2)+ShllWght), data = dat1 )
  par(mfrow=c(2,2))
  plot(lm1)
  par(mfrow=c(1,1))
  
  # Reducing the model
  drop1(lm1, test = 'F')
  lm1<- update(lm1, ~. -Diameter)
  drop1(lm1, test = 'F')
  par(mfrow=c(2,2))
  plot(lm1)
  par(mfrow=c(1,1))
  
  return(lm1)

}


