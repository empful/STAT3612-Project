library(dplyr)
library(stringr)
library(ggcorrplot)
library(ggplot2)
library(ggpubr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
data = read.csv("adult.csv")
head(data)

# convert missing values "?" to N/A
data = read.csv("adult.csv", na.strings = "?")
head(data)

# convert "character" to "factor"
str(data)
data = as.data.frame(unclass(data), stringsAsFactors = TRUE)
str(data)

# summarize both numerical and categorical data
summary(data)

# identify N/A columns 
na.col = colnames(data)[apply(data, 2, anyNA)]

# remove N/A
data = na.omit(data)

# remove "education" and "relationship"
data = data %>% select(-c("education","relationship"))
col.class = sapply(data, class)
cat.names = names(col.class[col.class == "factor"])
num.names = names(col.class[col.class != "factor"])
# write.csv(data, file = "cleaned_adult.csv")

################ correlation plot for continuous variables ################
data %>%
  select(num.names) %>%
  cor() %>%
  ggcorrplot(hc.order = TRUE, type = "lower",
             lab = TRUE)

################ single categorical variable ################
bar = function(col.name) {
  col.freq = as.data.frame(sort(table(data[[col.name]]), decreasing = T))
  colnames(col.freq) = c(col.name,"freq")
  ggplot() + 
    aes(x = col.freq[[col.name]], y = col.freq[["freq"]]) + 
    geom_bar(stat = "identity", fill = "steelblue") + 
    coord_flip() +
    labs(y = "frequency", x = col.name) +
    ggtitle(paste("Barplot of", col.name))
  }
for (i in cat.names) {
  name = paste("bar",i, sep="_")
  assign(name, bar(i))
}

ggarrange(bar_workclass, bar_marital.status, bar_occupation, 
          bar_race, bar_sex,bar_income, nrow=4, ncol=2)

bar_native.country

################ single continuous variable ################
# histogram for numerical variables
par(mfrow = c(3,2))
for (i in num.names) {
  hist(data[,i], main = i, xlab = NULL)
}

################ categorical variable vs income ################
bar_grouped = function(col.name) {
  ggplot() + 
    aes(y = data[[col.name]],fill = data[["income"]]) + 
    geom_bar(position = "fill")  + 
    labs(x = "percentage", y = col.name, title = paste("barplot of", col.name, "by income")) +
    guides(fill = guide_legend(title="income"))}

for (i in cat.names[-length(cat.names)]) {
  name = paste("bar",i, "grouped",sep="_")
  assign(name, bar_grouped(i))
}

ggarrange(bar_workclass_grouped, bar_marital.status_grouped, 
          bar_occupation_grouped, bar_race_grouped, 
          bar_sex_grouped, nrow=3, ncol=2)

bar_native.country_grouped


################ continuous variable vs income  ################
# side-by-side histogram
hist_grouped = function(col.name) {
  ggplot(data = data, mapping = aes(x = data[,col.name], fill = income)) + 
    geom_histogram(alpha = 0.8) +
    facet_wrap(~income) +
    labs(x = col.name) +
    ggtitle(paste("Histogram of", col.name ,"by Income"))
}

for (i in num.names) {
  name = paste("hist",i, sep="_")
  assign(name, hist_grouped(i))
}

ggarrange(hist_age, hist_fnlwgt, hist_education.num,
          hist_capital.gain, hist_capital.loss,
          hist_hours.per.week, nrow=3, ncol=2)

# box plot
box_grouped  = function(col.name) {
  ggplot(aes(x = income, y = data[,col.name]),
         data = data) + 
    geom_boxplot(outlier.size = 0.5) +
    stat_summary(fun = mean, 
                 geom = "point", 
                 shape = 16, 
                 cex = 2, 
                 col = "red")  +
    ylab(col.name) +
    xlab("Income") +  
    ggtitle(paste("Box Plot of", col.name ,"by Income"))
}

for (i in num.names) {
  name = paste("box",i, sep="_")
  assign(name, box_grouped(i))
}

ggarrange(box_age, box_fnlwgt, box_education.num,
          box_capital.gain, box_capital.loss,
          box_hours.per.week, nrow=3, ncol=2)



