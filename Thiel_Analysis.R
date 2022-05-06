library(plyr)
# library(papeR)
library(dplyr)
library(ggplot2)
library(knitr)
library(kableExtra)
library(tidyr)
# library(foreign)
# library(multcomp)
# library(broom)
# library(nlme)
library(tidyverse)
# library(stargazer)
# library(reshape2)
# library(rmarkdown)
library(psych)
# library(RColorBrewer)
# library(minpack.lm)
# library(numDeriv)
# # library(mosaic)
# library(pracma)
# library(formattable)
# library(lme4)
# library(sjPlot)
# library(lsmeans)


# Prepost data from working on Theil bodies

# read in data
Raw_theil = read.csv("Thiel Data.csv")

Raw_theil$Sub = NA

Raw_theil$Sub[2:nrow(Raw_theil)] = 1:(nrow(Raw_theil)-1)


Transfer = Raw_theil[2:nrow(Raw_theil), 1:2]
names(Transfer)[1:2]= c("Pre", "Post")
Transfer$Pre = as.numeric(Transfer$Pre)
Transfer$Post = as.numeric(Transfer$Post)
Result_Transfer=t.test( x=Transfer$Pre, y=Transfer$Post, paired = TRUE)
Result_Transfer
Transfer=gather(Transfer)

ggplot(data=Transfer, aes(x=key, y = value, fill = key)) +
  geom_bar(stat = "identity") +
  # scale_fill_manual(values = c("red", "green") ) +
  theme(legend.position="none")+
  xlab( "Transference")+
  ylab("")




## 
Confidence = Raw_theil[2:nrow(Raw_theil), 5:6]
names(Confidence)[1:2]= c("Pre", "Post")
Confidence$Pre = as.numeric(Confidence$Pre)
Confidence$Post = as.numeric(Confidence$Post)


Result_Confidence=t.test( x=Confidence$Pre, y=Confidence$Post, paired = TRUE)
Result_Confidence
Confidence=gather(Confidence)
Confidence$key = factor(Confidence$key, levels = c("Pre", "Post"))

ggplot(data=Confidence, aes(x=key, y = value, fill = key)) +
  geom_bar(stat = "identity") +
  # scale_fill_manual(values = c("red", "green") ) +
  theme(legend.position="none")+
  xlab( "Confidence")+
  ylab("")


## 
Preparedness = Raw_theil[2:nrow(Raw_theil), 9:10]
names(Preparedness)[1:2]= c("Pre", "Post")
Preparedness$Pre = as.numeric(Preparedness$Pre)
Preparedness$Post = as.numeric(Preparedness$Post)


Result_Preparedness=t.test( x=Preparedness$Pre, y=Preparedness$Post, paired = TRUE)
Result_Preparedness
Preparedness=gather(Preparedness)
Preparedness$key = factor(Preparedness$key, levels = c("Pre", "Post"))

ggplot(data=Preparedness, aes(x=key, y = value, fill = key)) +
  geom_bar(stat = "identity") +
  # scale_fill_manual(values = c("red", "green") ) +
  theme(legend.position="none")+
  xlab( "Preparedness")+
  ylab("")