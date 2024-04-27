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