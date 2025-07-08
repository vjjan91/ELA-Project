## This file is for the purposes of revising the current frequency csv to create a selection table that lays over the 4 min audio clips.  

#loading libraries 
library(stringr)
library(dplyr)
library(ggridges)
library(viridis)
library(lubridate)
library(hrbrthemes)
library(mgcv)
library(ggplot2)
library(pwr)

#read in the csv file 
ghat_birds4 <- read.csv("data/frequency-data.csv")
View(ghat_birds4)


#new begin.time and end.time, adding the number before the hyphen 
frequency_data_revised <- ghat_birds4 %>%
  mutate(
    add_time = as.numeric(str_extract(filename,"(?<=_)[0-9]+(?=-)")), 
    begin_time_in_s = begin_time_in_s + add_time, 
    end_time_in_s = end_time_in_s + add_time, 
    file_offset = begin_time_in_s
  )

#filename = filename - anything after 3rd _
frequency_data_revised <- frequency_data_revised %>%
  mutate(
    filename = sub("^([^_]*_[^_]*_[^_]*)_.*", "\\1", filename)
  )
View(frequency_data_revised)

#removing unnecessary columns 
frequency_data_revised$add_time = NULL

#writing a csv 
write.csv(frequency_data_revised, "data/frequency-data-revised.csv", row.names = FALSE)
freq <- read.csv("data/frequency-data-revised.csv")
View(freq)


