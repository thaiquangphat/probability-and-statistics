#--------------------------------------------- LIBRARIES ---------------------------------------------
install.packages('car')
install.packages('caret')
install.packages('dplyr')
install.packages('drc')
install.packages('ggplot2')
install.packages('lessR')
install.packages('randomForest')
install.packages('readr')
install.packages('nlme')
install.packages('nls.multstart')
install.packages('pacman')
install.packages('readxl')
install.packages('tidyverse')
install.packages("corrplot")
install.packages("purrr")
install.packages("stats")
install.packages("base")
install.packages("agricolae")
install.packages("caTools")
install.packages("xgboost")
install.packages("earth")
install.packages("plotly")
install.packages("npreg")

#--------------------------------------------- DATA PREPROCEEDING ---------------------------------------------

setwd("C:/Users/quang/OneDrive/Desktop/232/XSTK/assignment/CPU-GPU")
data <- import("./cpu-raw.csv")       # rio::import

head(data)

#Select attributes for analysing
data <- data[, c("Vertical_Segment", "Status", "Launch_Date", "Lithography",
                 "Recommended_Customer_Price", "nb_of_Cores",
                 "Processor_Base_Frequency", "TDP","T")] 

# Rename labels - easier to use
names(data) <- c("market", "status", "ldate", "litho", "rprice", "ncore", "bfreq", "tdp", "temp")
names(data)

head(data)

if (file.exists("cpu-short.csv")) {
  file.remove("cpu-short.csv")
}

export(data, "cpu-short.csv")

data[,"ldate"] <- as.yearqtr(data[,"ldate"], format="Q%q'%y")

data[,"litho"] <- gsub(" nm", "", data[, "litho"])
data[,"litho"] <- as.numeric(data[,"litho"])

data[,"rprice"] <- gsub("(^\\$(\\d)+.(\\d)+ - )", "", data[, "rprice"])
data$rprice <- ifelse(data$rprice == "N/A", NA, data$rprice)
data$rprice <- as.numeric(gsub('\\$|,', '', data$rprice))

data[,"ncore"] <- as.numeric(data[,"ncore"])

data[,"bfreq"] <- gsub("( GHz)|( MHz)", "",data[,"bfreq"])
data[,"bfreq"] <- as.numeric(data[,"bfreq"])
data<- data[!is.na(data$bfreq), ]
data$bfreq[data$bfreq > 10] <- data$bfreq[data$bfreq > 10]*0.001

data[,"tdp"] <- gsub(" W", "", data[, "tdp"])
data[,"tdp"] <- as.numeric(data[,"tdp"])

data[,"temp"] <- (gsub("[^0-9.\\-]+", ",", data[,"temp"])) 
for (i in seq_along(data[["temp"]])) {
  temp_values <- strsplit(data[i, "temp"], ",")
  temp_values <- unlist(lapply(temp_values, as.numeric))
  max_value <- max(temp_values, na.rm = TRUE)
  if (is.infinite(max_value)) {
    max_value <- NA
  }
  data[i, "temp"] <- max_value
}

if (file.exists("cpu-clean.csv")) {
  file.remove("cpu-clean.csv")
}

export(data, "cpu-clean.csv")

#--------------------------------------------- DESCRIPTIVE STATISTICS ---------------------------------------------

pacman::p_load(
  rio,
  ggplot2,
  zoo,
  car,
  FSA
)

data <- import("cpu-clean.csv") 

#--------------------------------------------- LITHOGRAPHY PLOT ---------------------------------------------

data_filtered <- data[!is.na(data$litho), ]

ggplot(data_filtered, aes(x = litho)) +
  geom_histogram(fill = "paleturquoise3", size = 2) +
  labs(title = "Histogram of Lithography",
       x = "Lithography",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5)) +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data_filtered, aes(x = litho)) +
  geom_boxplot(fill = "paleturquoise3", size = 2) +
  labs(title = "Box plot of Lithography",
       x = "Lithography",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5)) +
  theme(plot.title = element_text(hjust = 0.5))

summary(data_filtered$litho)

#--------------------------------------------- RPRICE PLOT ---------------------------------------------

data_filtered <- data[!is.na(data$rprice), ]

ggplot(data_filtered, aes(x = rprice)) +
  geom_histogram(fill = "paleturquoise3", size = 2) +
  labs(title = "Histogram of Recommended Price",
       x = "Recommended Price",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5)) +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data_filtered, aes(x = rprice)) +
  geom_boxplot(fill = "paleturquoise3", size = 2) +
  labs(title = "Box plot of Recommended Price",
       x = "Recommended Price",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5)) +
  theme(plot.title = element_text(hjust = 0.5))

#--------------------------------------------- NCORE PLOT ---------------------------------------------

data_filtered <- data[!is.na(data$ncore), ]

ggplot(data_filtered, aes(x = ncore)) +
  geom_histogram(fill = "paleturquoise3", size = 2) +
  labs(title = "Histogram of Number of Cores",
       x = "Number of Cores",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5)) +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data_filtered, aes(x = ncore)) +
  geom_boxplot(fill = "paleturquoise3", size = 2) +
  labs(title = "Box plot of Number of Cores",
       x = "Number of Cores",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5)) +
  theme(plot.title = element_text(hjust = 0.5))

summary(data_filtered$ncore)

#--------------------------------------------- BFREQ PLOT ---------------------------------------------
data_filtered <- data[!is.na(data$bfreq), ]

ggplot(data_filtered, aes(x = bfreq)) +
  geom_histogram(fill = "paleturquoise3", size = 2) +
  labs(title = "Histogram of Base Frequency",
       x = "Base Frequency",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5)) +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data_filtered, aes(x = bfreq)) +
  geom_boxplot(fill = "paleturquoise3", size = 2) +
  labs(title = "Box plot of Base Frequency",
       x = "Base Frequency",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5)) +
  theme(plot.title = element_text(hjust = 0.5))

#--------------------------------------------- TDP PLOT ---------------------------------------------
data_filtered <- data[!is.na(data$tdp), ]

ggplot(data_filtered, aes(x = tdp)) +
  geom_histogram(fill = "paleturquoise3", size = 2) +
  labs(title = "Histogram of Thermal Design Power",
       x = "Thermal Design Power",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5)) +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data_filtered, aes(x = tdp)) +
  geom_boxplot(fill = "paleturquoise3", size = 2) +
  labs(title = "Box plot of Thermal Design Power",
       x = "Thermal Design Power",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5)) +
  theme(plot.title = element_text(hjust = 0.5))

#--------------------------------------------- TEMP ---------------------------------------------
data_filtered <- data[!is.na(data$temp), ]

ggplot(data_filtered, aes(x = temp)) +
  geom_histogram(fill = "paleturquoise3", size = 2) +
  labs(title = "Histogram of Temperature",
       x = "Temperature",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5)) +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data_filtered, aes(x = temp)) +
  geom_boxplot(fill = "paleturquoise3", size = 2) +
  labs(title = "Box plot of Temperature",
       x = "Temperature",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5)) +
  theme(plot.title = element_text(hjust = 0.5))

summary(data_filtered$temp)

#-------------------------------------------- LITHOGRAPHY - LDATE --------------------------------------------

data_filtered <- data[!is.na(data$litho),]
data_filtered$litho <- as.factor(data_filtered$litho)

# Create the box plot
ggplot(data_filtered, aes(x = ldate, y = litho)) + 
  geom_boxplot(fill = "paleturquoise3") +
  labs(title = "Box plot Lithography by Launch Date",
       x = "Launch date",
       y = "Lithography") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data_filtered, aes(x = ldate, y = litho)) +
  geom_point(shape = 21,color = "paleturquoise3", fill = "paleturquoise3", size = 2) +
  labs(title = "Point plot lithography by launch date",
       x = "Launch Date", 
       y = "Lithography") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data_filtered, aes(x = ldate, fill = litho)) +
  geom_bar(fill = "paleturquoise3", size = 2) +
  labs(title = "Histogram of Lithography by Launch Date",
       x = "Launch Date",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5)) +
  theme(plot.title = element_text(hjust = 0.5))

#-------------------------------------------- TDP --------------------------------------------
ggplot(data_filtered, aes(x = tdp)) +
  geom_histogram(fill = "paleturquoise3", size = 2) +
  labs(title = "Histogram of Thermal Design Power",
       x = "Thermal Design Power",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5)) +
  theme(plot.title = element_text(hjust = 0.5))

summary(data_filtered$tdp)

#--------------------------------------------TDP - TEMPERATURE--------------------------------------------
plot_data <- data
plot_data$litho <- as.factor(plot_data$litho)

# Different trends
ggplot(plot_data, aes(x = temp, y = tdp)) +
  geom_point(color = "paleturquoise3") +
  geom_smooth(method = "lm", se = FALSE, color = "black", size = 0.5) +  # Add linear regression line
  labs(
    title = "Different trends between Temperature and TDP",
    x = "Temperature",
    y = "TDP"
  ) +
  theme(plot.title = element_text(hjust = 0.5))

#--------------------------------------------NCORE - TDP--------------------------------------------
plot_data <- data
plot_data$litho <- as.factor(plot_data$litho)

plot_data$ncore <- as.factor(plot_data$ncore)
# Different trends
ggplot(plot_data, aes(x = ncore, y = tdp)) +
  geom_point(color = "paleturquoise3") +
  geom_abline(slope = 3.5, intercept = 10, color = "black") +  
  labs(
    title = "Different Trends between Number of Cores and TDP",
    x = "Number of Cores",
    y = "TDP"
  ) +
  theme(plot.title = element_text(hjust = 0.5))

# Different trends
ggplot(plot_data, aes(x = ncore, y = tdp)) +
  geom_boxplot(color = "paleturquoise3") +
  labs(
    title = "Different trends between Number of Cores and TDP",
    x = "Number of Cores",
    y = "TDP"
  ) +
  theme(plot.title = element_text(hjust = 0.5))

#--------------------------------------------TDP - LITHO--------------------------------------------

plot_data <- data
plot_data$litho <- as.factor(plot_data$litho)

# Different trends
plot_data$litho <- as.factor(plot_data$litho)

# Different trends
ggplot(plot_data, aes(x = litho, y = tdp)) +
  geom_point(color = "paleturquoise3") +
  labs(
    title = "Different trends of Thermal Design Power in different Lithography",
    x = "Lithography",
    y = "Thermal Design Power"
  ) +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(plot_data, aes(x = litho, y = tdp)) +
  geom_boxplot(color = "paleturquoise3") +
  labs(
    title = "Different trends of Thermal Design Power in different Lithography",
    x = "Lithography",
    y = "Thermal Design Power"
  ) +
  theme(plot.title = element_text(hjust = 0.5))

#--------------------------------------------MARKET - NCORE--------------------------------------------
plot_data <- data
plot_data$litho <- as.factor(plot_data$litho)

# Different trends
ggplot(plot_data, aes(x = market, y = tdp)) +
  geom_point(color = "paleturquoise3") +
  labs(
    title = "Different trends of Number of Cores in different Markets",
    x = "Markets",
    y = "Number of Cores"
  ) +
  theme(plot.title = element_text(hjust = 0.5))

data <- import("cpu-clean.csv")
plot_data <- data

# Different trends
plot_data$litho <- as.factor(plot_data$litho)

ggplot(plot_data, aes(x = litho, y = tdp)) +
  geom_boxplot(color = "paleturquoise3") +
  labs(
    title = "Different trends of Number of Cores in different Markets",
    x = "Markets",
    y = "Number of Cores"
  ) +
  theme(plot.title = element_text(hjust = 0.5))

#--------------------------------------------SUMMARIZE DATA--------------------------------------------

data <- import("cpu-clean.csv")

data <- import("cpu-clean.csv")
data <- data[data$tdp < 150, ]
data <- data[!is.na(data$tdp), ]
data <- data[!is.na(data$bfreq), ]
data <- data[!is.na(data$litho), ]
data <- data[!is.na(data$ncore), ]
data <- data[!is.na(data$temp), ]

data <- data[complete.cases(data$tdp, 
                            data$bfreq, 
                            data$litho, 
                            data$ncore, 
                            data$temp) 
             & data$tdp < 150, ]

summary(data)

corr_data <- na.omit(data)

#-- Test with correlation

cor_matrix = cor(corr_data[, 3:ncol(corr_data)])
library(corrplot)
corrplot(cor_matrix, method="pie")
corrplot(cor_matrix,
         method = "pie",
         addCoef.col = "red",
         tl.cex = 0.8,           
         addCoefasPercent = TRUE, 
         number.cex = 0.8,      
         mar = c(0, 0, 0, 0),    
         cl.pos = "r",            
         cl.ratio = 0.2,          
         cl.offset = 1.3,)    

#--------------------------------------------- INFERENTIAL STATISTICS ---------------------------------------------

#-------------------------------------------- LITHOGRAPHY AS CPU ERA --------------------------------------------

data_rw <- import("cpu-clean.csv")

data_test <- data_rw
data_test <- data_test[complete.cases(data_test$tdp, data_test$bfreq, data_test$litho, data_test$ncore, data_test$temp) & data_test$tdp < 150, ]

data <- import("cpu-clean.csv")
data <- data[data$tdp < 150, ]
data <- data[!is.na(data$tdp), ]
data <- data[!is.na(data$bfreq), ]
data <- data[!is.na(data$litho), ]
data <- data[!is.na(data$ncore), ]
data <- data[!is.na(data$temp), ]

data <- data[complete.cases(data$tdp, 
                            data$bfreq, 
                            data$litho, 
                            data$ncore, 
                            data$temp) 
             & data$tdp < 150, ]

summary(data)

library(dplyr)
library(purrr)

data_sum <- mutate(data, litho = as.factor(litho))

retval <- data_sum %>%
  group_by(litho) %>%
  summarize(
    `5% quantile` = quantile(ldate, probs = 0.05, na.rm = TRUE),
    `95% quantile` = quantile(ldate, probs = 0.95, na.rm = TRUE),
    `STD Mean` = mean(sd(ldate, na.rm = TRUE), na.rm = TRUE),
    `Confidence Interval` = quantile(ldate, probs = 0.95, na.rm = TRUE) - quantile(ldate, probs = 0.05, na.rm = TRUE)
  ) %>%
  rename("5% quantile" = `5% quantile`, "95% quantile" = `95% quantile`)

print(retval)
View(retval)

#-------------------------------------------- ANOVA --------------------------------------------

data_litho <- data
data_litho$litho <- as.factor(data_litho$litho)

data_litho <- data_litho[data_litho$litho != 28 & data_litho$litho != 250 & data_litho$litho != 180, ]

data_litho <- data_litho[!is.na(data_litho$tdp), ]
data_litho <- data_litho[!is.na(data_litho$litho), ]

litho_anova_model <- aov(tdp ~ litho, data = data_litho)

summary(litho_anova_model)

#-------------------------------------------- QQ PLOT --------------------------------------------
qqnorm(residuals(litho_anova_model), ylab = "residuals", xlab = "quantiles", col = 'paleturquoise3')
qqline(residuals(litho_anova_model), col = "black")

library(car)
qqPlot(residuals(litho_anova_model), ylab = "residuals")


#-------------------------------------------- SHAPIRO TEST --------------------------------------------
shapiro.test(residuals(litho_anova_model))

#-------------------------------------------- BARTLETT TEST --------------------------------------------
bartlett.test(tdp ~ litho, data = data_litho)

#-------------------------------------------- KRUSKAL TEST --------------------------------------------
kruskal.test(tdp ~ litho, data = data_litho)

#-------------------------------------------- POST-HOC TEST --------------------------------------------
#-------------------------------------------- TUKEY --------------------------------------------
# Perform Tukey's HSD test
tukey_result <- TukeyHSD(litho_anova_model)
plot(tukey_result, las = 1)

# Print the results
print(tukey_result)

library(agricolae)

#perform Tukey's Test
HSD.test(litho_anova_model, "litho", console=TRUE)

#holm's
pairwise.t.test(data_litho$tdp, data_litho$litho, p.adjust="hochberg") 

#-------------------------------------------- DUNN TEST --------------------------------------------
library(FSA)
dunnTest(tdp ~ litho, data = data_litho,method = "bonferroni")


#--------------------------------------------- LINEARITY TEST --------------------------------------------
data.lr <- import("cpu-clean.csv")
data.lr <- data.lr[complete.cases(data.lr$tdp, 
                                  data.lr$bfreq, 
                                  data.lr$litho, 
                                  data.lr$ncore, 
                                  data.lr$temp, 
                                  data.lr$rprice,
                                  data.lr$ldate) 
                   & data.lr$tdp < 125, ]

li.model <- lm(tdp ~ ldate + rprice + litho + ncore + bfreq + temp, data = data.lr)

library(car)

#perform hypothesis test for hours=0 and prac_exams=0
linearHypothesis(li.model, c("ldate=0", "rprice=0", "litho=0", "ncore=0", "bfreq=0", "temp=0"))

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

#--------------------------------------------- EXTENSION ---------------------------------------------

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

#--------------------------------------------- SMOOTHING REGRESSION ---------------------------------------------
library(npreg)

data.s <- data
data.s <- subset(data.s, select = -c(status, market, ldate))
data.s <- data.s[complete.cases(data.s$tdp, 
                                data.s$bfreq, 
                                data.s$litho, 
                                data.s$ncore, 
                                data.s$temp, 
                                data.s$rprice),] 
data.ss <- subset(data.s, select = c(tdp, rprice))

mod.ss <- with(data.s, ss(rprice, tdp), lambda = 1e-15)
mod.ss <- ss(data.s$rprice, data.s$tdp, all.knots = TRUE)
mod.ss
summary(mod.ss)

# plot fit
plot(mod.ss, xlab = "rprice", ylab = "tdp")
rug(data.s$rprice)  # add rug to plot
points(data.s$rprice, data.s$tdp)

