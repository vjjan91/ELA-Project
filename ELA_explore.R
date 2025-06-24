
## This file is for the purposes of exploring the frequency data, organizing it, and extracting key features to determine what sound files to explore.   

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
ghat_birds <- read.csv("data/frequency-data.csv")

#grabbing date from filename into new column "date" 
#file currently looks like this INOA03R_20201217_062000_40-50 
ghat_birds <- ghat_birds %>%
  mutate(date = str_extract(filename,"_\\d{8}_") %>%
           str_replace_all("_","") %>%
           ymd())

#grabbing site_id
ghat_birds <- ghat_birds %>%
  mutate(recorder = str_extract(filename,"^[^_]+"))

##taking the time from filename, 

#add hour to ghat_birds
ghat_birds <- ghat_birds %>%
  mutate(
    hms1 = str_extract(filename, "_\\d{8}_\\d{6}_") %>%
      str_replace_all("_", "") %>%
      substr(9, 14), 
    hms2 = format(strptime(hms1, "%H%M%S"), "%H:%M:%S"),
 recorder_date_hour = paste(recorder, date, hms2, sep = "_")  #fullcombining
  )
View(ghat_birds)


#ordering the top counts of each recorder_date_hour combo, grouped by name
high_count <- ghat_birds %>% 
  group_by(common_name,recorder_date_hour) %>%
  summarize(count = n(), .groups = "drop") %>% #add dropped group for purposes of arranging/mutates
  group_by(common_name) %>%
  #top_n(1, count) %>%  #Only includes top count per species...not what I was going for
  arrange(desc(count))
View(high_count)

#Separate by unique location (unique_location) site (43 potential sites) 
#counting species occurrences by their unique days, OG
days_names <- ghat_birds %>%
  group_by(common_name) %>% #operations in each group species
  summarize(unique_days = n_distinct(date)) %>% #n_distinct counts unique items
  arrange(desc(unique_days)) 
View(days_names)



#Isolated important columns
ghat_birds2 <- ghat_birds %>%
  distinct(common_name, date, recorder, hms2)
View(ghat_birds2)

#Unique record_date_hour per species - good to see to determine how many different hours/sp
ghat_birds3 <- ghat_birds %>%
  group_by(common_name) %>%
  summarise(recorder_date_hour = n_distinct(recorder_date_hour)) %>%
  arrange(desc(recorder_date_hour))
View(ghat_birds3)
              
#count each species with the new unique recorder-day pair -> better 
species_recorder_date <- ghat_birds2 %>%
  group_by(common_name) %>%
  summarize(unique_recorder_days = n()) %>%
  arrange(desc(unique_recorder_days))
View(species_recorder_date)


#factoring out migratory bird species 

# One-way ANOVA, typical target p = 0.8/0.9, f = effect size
pwr.anova.test(k = 30, f = .25, sig.level = 0.05, power = 0.8 )
# k = 20, n = 17.2 
