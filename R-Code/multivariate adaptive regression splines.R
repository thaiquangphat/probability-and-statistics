#--------------------------------------------- MULTIVARIATE ADAPTIVE REGRESSION TDP ---------------------------------------------
library(dplyr)   #data wrangling
library(ggplot2) #plotting 
library(earth)   #fitting MARS models
library(caret)   #tuning model parameters

data_mul <- data
data_mul <- subset(data_mul, select = -c(status, market, ldate))
data_mul <- data_mul[complete.cases(data_mul$tdp, 
                                    data_mul$bfreq, 
                                    data_mul$litho, 
                                    data_mul$ncore, 
                                    data_mul$temp, 
                                    data_mul$rprice),] 

set.seed(123)
split <- createDataPartition(data_mul$tdp, p = 0.8, list = FALSE)  # Create indices for splitting
training_set <- data_mul[split, ]  # Subset of data for training
test_set <- data_mul[-split, ]     # Subset of data for testing

mars1 <- earth(
  tdp ~ .,  
  data = training_set   
)
library(dplyr)  # 
# Print model summary
print(mars1)

summary(mars1) %>% .$coefficients %>% head(10)

plot(mars1)


for (i in 1:4) {
  plot(mars1, i)
}

# create a tuning grid
hyper_grid <- expand.grid(
  degree = 1:7, 
  nprune = seq(2, 100, length.out = 10) %>% floor()
)

head(hyper_grid)

# Cross-validated model
set.seed(123)  # for reproducibility
cv_mars <- train(
  x = subset(training_set, select = -tdp),
  y = training_set$tdp,
  method = "earth",
  metric = "RMSE",
  trControl = trainControl(method = "cv", number = 10),
  tuneGrid = hyper_grid
)

# View results
cv_mars$bestTune
##    nprune degree
## 16     56      2

cv_mars$results %>%
  filter(nprune == cv_mars$bestTune$nprune, degree == cv_mars$bestTune$degree)
##   degree nprune    RMSE  Rsquared      MAE   RMSESD RsquaredSD    MAESD
## 1      2     56 26817.1 0.8838914 16439.15 11683.73 0.09785945 1678.672

ggplot(cv_mars)

cv_mars$resample

#--------------------------------------------- REGRESSION FITTED ---------------------------------------------

library(splines)
df <- data
df <- subset(df, select = -c(status, market, ldate))
df <- df[complete.cases(df$tdp, 
                        df$rprice)
         & !(df$rprice > 3500)
         & !(df$rprice < 2000 & df$tdp <= 150 & df$tdp > 100)
         & !(df$rprice < 1500 & df$tdp <= 100 & df$tdp > 50), ]

#fit sdata_xg#fit spline regression model
spline_fit <- lm(df$rprice ~ bs(df$tdp, knots=c(50, 150, 200, 225)))

#view summary of spline regression model
summary(spline_fit)

x_lim <- range(df$tdp)
x_grid <- seq(x_lim[1], x_lim[2])
preds <- predict(spline_fit, newdata=list(x=x_grid))

#create scatter plot with spline regression predictions
plot(df$tdp, df$rprice, cex=1, pch=1)

lines(x_grid, preds)

#--------------------------------------------- FIXED REGRESSION SPLINES ---------------------------------------------
df <- data
df <- subset(df, select = -c(status, market, ldate))

# Filter out incomplete cases and outliers
df <- df[complete.cases(df$tdp, df$rprice) & 
           !(df$rprice > 3500) & 
           !(df$rprice < 2000 & df$tdp <= 150 & df$tdp > 100) & 
           !(df$rprice < 1500 & df$tdp <= 100 & df$tdp > 50), ]

# Fit spline regression model
spline_fit <- lm(rprice ~ bs(tdp, knots=c(150)), data = df)

# View summary of spline regression model
summary(spline_fit)

# Generate prediction grid
x_grid <- seq(min(df$tdp), max(df$tdp), length.out = 100)
preds <- predict(spline_fit, newdata = data.frame(tdp = x_grid), se.fit = TRUE)

# Plot original data points
plot(df$tdp, df$rprice, cex = 1, pch = 16, col = "blue", xlab = "tdp", ylab = "rprice")

# Add spline regression line
lines(x_grid, preds$fit, col = "red", lwd = 2)

# Generate predictions using the fitted model
x_grid <- seq(min(df$tdp), max(df$tdp), length.out = 100)  # Generate a sequence of x values
preds <- predict(spline_fit, newdata = data.frame(tdp = x_grid), type = "response")  # Predict response for x_grid

# Create scatter plot with spline regression predictions
plot(df$tdp, df$rprice, cex = 1, pch = 1, xlab = "tdp", ylab = "rprice")  # Plot data points
lines(x_grid, preds, col = "red")  # Add spline curve to the plot