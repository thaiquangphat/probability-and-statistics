#--------------------------------------------- POLYNOMIAL TDP ~ RPRICE ---------------------------------------------
ggplot(data, aes(x=tdp, y=rprice)) +
  geom_point()

data_poly <- data
data_poly <- data_poly[complete.cases(data_poly$tdp, 
                                      data_poly$rprice)
                       & !(data_poly$rprice > 3500)
                       & !(data_poly$rprice < 2000 & data_poly$tdp <= 150 & data_poly$tdp > 100)
                       & !(data_poly$rprice < 1500 & data_poly$tdp <= 100 & data_poly$tdp > 50), ]

ggplot(data_poly, aes(x=tdp, y=rprice)) +
  geom_point()

#randomly shuffle data
df.shuffled <- data_poly[sample(nrow(data_poly)),]

#define number of folds to use for k-fold cross-validation
K <- 15 

#define degree of polynomials to fit
degree <- 10

#create k equal-sized folds
folds <- cut(seq(1,nrow(df.shuffled)),breaks=K,labels=FALSE)

#create object to hold MSE's of models
mse = matrix(data=NA,nrow=K,ncol=degree)

#Perform K-fold cross validation
for(i in 1:K){
  
  #define training and testing data
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- df.shuffled[testIndexes, ]
  trainData <- df.shuffled[-testIndexes, ]
  
  #use k-fold cv to evaluate models
  for (j in 1:degree){
    fit.train = lm(rprice ~ poly(tdp,j), data=trainData)
    fit.test = predict(fit.train, newdata=testData)
    mse[i,j] = mean((fit.test-testData$rprice)^2) 
  }
}

#find MSE for each degree 
colMeans(mse)

#fit best model
poly = lm(rprice ~ poly(tdp,4, raw=T), data=data_poly)

#view summary of best model
summary(poly)

ggplot(data_poly, aes(x=tdp, y=rprice)) + 
  geom_point() +
  stat_smooth(method='lm', formula = y ~ poly(x,4), size = 1) + 
  xlab('tdp') +
  ylab('rprice')

#--------------------------------------------- POLYNOMIAL TDP ---------------------------------------------
ggplot(data, aes(x=tdp, y=rprice)) +
  geom_point()

data_poly <- data
data_poly <- data_poly[complete.cases(data_poly$tdp, 
                                      data_poly$bfreq, 
                                      data_poly$litho, 
                                      data_poly$ncore, 
                                      data_poly$temp, 
                                      data_poly$rprice), ]

ggplot(data_poly, aes(x=tdp, y=rprice)) +
  geom_point()

#randomly shuffle data
df.shuffled <- data_poly[sample(nrow(data_poly)),]

#define number of folds to use for k-fold cross-validation
K <- 15 

#define degree of polynomials to fit
degree <- 4

#create k equal-sized folds
folds <- cut(seq(1,nrow(df.shuffled)),breaks=K,labels=FALSE)

#create object to hold MSE's of models
mse = matrix(data=NA,nrow=K,ncol=degree)

#Perform K-fold cross validation
for(i in 1:K){
  
  #define training and testing data
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- df.shuffled[testIndexes, ]
  trainData <- df.shuffled[-testIndexes, ]
  
  #use k-fold cv to evaluate models
  for (j in 1:degree){
    fit.train = lm(tdp ~ poly(rprice,j) + poly(bfreq, j) + poly(litho, j) + poly(ncore, j) + poly(temp, j), data=trainData)
    fit.test = predict(fit.train, newdata=testData)
    mse[i,j] = mean((fit.test-testData$tdp)^2) 
  }
}

#find MSE for each degree 
colMeans(mse)

poly_model <- lm(tdp ~ poly(rprice, 3, raw = TRUE) + 
                   poly(bfreq, 3, raw = TRUE) + 
                   poly(litho, 3, raw = TRUE) + 
                   poly(ncore, 3, raw = TRUE) +
                   poly(temp, 3, raw = TRUE), 
                 data = data_poly)

#view summary of best model
summary(poly_model)

ggplot(data_poly, aes(x=rprice, y=tdp)) + 
  geom_point() +
  stat_smooth(method='lm', formula = y ~ poly(x,3), size = 1) + 
  xlab('rprice') +
  ylab('tdp')