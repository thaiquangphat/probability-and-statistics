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