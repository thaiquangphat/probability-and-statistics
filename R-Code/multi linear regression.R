#--------------------------------------------- LINEAR REGRESSION ---------------------------------------------

data_lr <- import("cpu-clean.csv")
data_lr <- data_lr[complete.cases(data_lr$tdp, 
                                  data_lr$bfreq, 
                                  data_lr$litho, 
                                  data_lr$ncore, 
                                  data_lr$temp, 
                                  data_lr$rprice,
                                  data_lr$ldate) 
                   & data_lr$tdp < 125, ]

library(caTools)
set.seed(123)

split = sample.split(data_lr, SplitRatio = 0.8)
training_set = subset(data_lr, split == TRUE)
test_set = subset(data_lr, split == FALSE)

regressor = lm(formula = tdp ~ ldate + rprice + litho + ncore + bfreq + temp, data = training_set)
summary(regressor)

plot(regressor)

lr <- test_set['tdp']
lr['tdp_predicted'] <- as.data.frame(predict(regressor, newdata = test_set))

ggplot(lr, aes(x = tdp, y = tdp_predicted)) +
  geom_point(shape=1, color="black") +
  geom_abline(mapping=aes(intercept= 0, slope = 1), color="red") +
  labs(x = "tdp-actual", y = "tdp-predict")

MAE <- mean(abs(y_pred - test_set$tdp))
MSE <- mean((y_pred - test_set$tdp)^2)
accuracy <- sum(1-abs(lr$tdp_predicted - lr$tdp) / lr$tdp) / nrow(lr)

print(accuracy)

#residual error
ggplot(regressor, aes(x = resid(regressor))) +
  geom_histogram(binwidth = 2, fill = "paleturquoise3") +# histogram of residuals
  labs(x="residuals", y="count")

# Make predictions on the test set
y_pred <- predict(regressor, newdata = test_set)

# Create a scatter plot
plot(test_set$tdp, y_pred, xlab = "actual tdp", ylab = "predicted tdp", main = "Actual vs Predicted Values")
# Add a diagonal line representing perfect predictions
abline(0, 1, col = "red")
# Add a legend
legend("topleft", legend = "Perfect Prediction", col = "red", lty = 1)
