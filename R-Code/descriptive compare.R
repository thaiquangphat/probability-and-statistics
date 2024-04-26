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