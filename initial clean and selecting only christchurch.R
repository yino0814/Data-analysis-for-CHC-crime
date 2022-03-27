library(tidyverse)
library(skimr)

crime_df<-read_csv("VICTIM_TIMEPLACE.csv")#loading the data into the creim_df variable

crime_df %>%
  skim() # helps get a general overview of the data frame

clean_df <-na.omit(crime_df) # removes all "missing" / NA data
#only removed 4 rows of data

clean_df %>%  
  skim() # double check the final number of rows is 'original number of rows' - 4


christchurch_df <- clean_df%>% # gathering rows that have christchurch city as their territorial authority and storing in canterbury_df variable
  filter(clean_df$'Territorial Authority' == 'Christchurch City.')


#######################################################################################

write.csv(christchurch_df,"christchurch-data.csv", row.names = FALSE) 
# creating a copy of only christchurch city with all columns

