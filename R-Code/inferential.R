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
