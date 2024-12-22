#install theme packages
install.packages(c("ggthemes", "ggsci", "ggpubr", "hrbrthemes"))



#install color packages
install.packages("RColorBrewer")
install.packages("colorspace")

#load package
library(tidyverse)
library(ggthemes)
library(ggsci)
library(ggpubr)
library(hrbrthemes)
library(RColorBrewer)
library(colorspace)

install.packages("readxl")
library(readxl)
df <- read_excel("C:/Users/bgbgnht/OneDrive/Desktop/AMR_KAP_Data.xlsx")


#install packages
install.packages("ggplot2")
install.packages("dplyr")

#load necessary library
library(ggplot2)
library(dplyr)

#data preparation
data <-data.frame(
  characteristic = c('Good', 'Moderate', 'poor', 'Positive', 'Negative', 'Uncertain', 'Good', 'Misuse'),
  count = c(122, 314, 268, 209, 124, 371, 250, 454),
  percentage = c(17, 45, 38, 30, 18, 53, 36, 64),
  category = c(rep('Knowledge', 3), rep('Attitude', 3), rep('practice', 2))
)

#Bar chart
ggplot(data, aes(x = Characteristic, y = Count, fill = Category)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  labs(title = 'Bar Chart of Characteristics', x = 'Characteristic', y = 'Count') +
  theme_minimal()
rlang::last_trace()
rlang::last_trace(drop = FALSE)
