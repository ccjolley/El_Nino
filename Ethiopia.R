# pull data from Ethiopia's "drought response tracker" spreadsheet

library(XLConnect)
library(plyr)
library(dplyr)

setwd("C:/Users/Craig/Desktop/Live Projects/El Nino/El_Nino")
fname <- '../Drought Response Tracker - as of 05.18.16.xlsx'
eth <- readWorksheetFromFile(fname,sheet=1,startRow=2)

# In this spreadsheet, the distinction between humanitarian and development
# programs was made by dividing the spreadsheet up with "total" lines. We'll
# need to use these to label each row according to its category, then
# delete them.

total_lines <- grep('TOTAL',eth$Activity.Name.)
# Everything above the second total line is humanitarian; everything
# below it is development.
eth$type <- 'development'
eth[1:total_lines[2],'type'] <- 'humanitarian'

# Remove rows with totals and other extras
 
eth_clean <- eth[-grep('TOTAL',eth$Activity.Name.),]
eth_clean <- eth_clean[-grep('Note: The data is updated as of',eth_clean$OFFICE),]
eth_clean <- eth_clean[rowSums(is.na(eth_clean))<ncol(eth_clean),]
names(eth_clean) <- c('office','activity_name','implementer','award','start',
                      'end','LOP','obligation_to_date','disbursement_to_date',
                      'obligation_14','obligation_15','obligation_16',
                      'disbursement_15','disbursement_16','account','sector',
                      'status','type')

# Sometimes, if a project is split over multiple sectors, it will have 
# disbursement/obligation numbers for each year and each sector, but data 
# identifying the project (office through disbursement_to_date) will be
# in merged cells that wound up getting replaced with NAs


last <- data.frame()
for (i in 1:nrow(eth_clean)) {
  if (!is.na(eth_clean[i,'activity_name'])) {  # First row of a new activity
    last <- eth_clean[i,1:6]                   # grab first 6 columns; copy to new rows
  } else {
    eth_clean[i,1:6] <- last      # if not a new activity, copy rows from last new one
  }
}

# Disbursement-to-date and obligation-to-date columns haven't been updated
# in a while; we should drop them

eth_clean <- eth_clean %>%
  select(-obligation_to_date) %>%
  select(-disbursement_to_date)

# If no sector specified, replace with 'Other'

eth_clean[is.na(eth_clean$sector),'sector'] <- 'Other'

# In the tracker spreadsheet used for other countries, there is a category
# for "Shelter and non-food." Rename "Non-Food Input" to "Shelter"

eth_clean[eth_clean$sector == 'Non-Food Input','sector'] <- 'Shelter'

# Format to be compatible with output from other scripts

get_budget <- function(t=NULL,s=NULL){
  tmp <- select(eth_clean,disbursement_15,disbursement_16,type,sector)
  if (!is.null(t)) {
    tmp <- filter(tmp,type==t)
  }
  if (!is.null(s)) {
    tmp <- filter(tmp,sector==s)
  }
  if (nrow(tmp) == 0) {
    return(0)
  }
  select(tmp,disbursement_15,disbursement_16) %>% sum(na.rm=TRUE)
}

get_budget() # new total: $757M
eth_totals <- data.frame(
  mission = 'Ethiopia',
  human_total = get_budget('humanitarian'),
  dev_total = get_budget('development'),
  food_dev = get_budget('development','Food'),
  food_hum = get_budget('humanitarian','Food'),
  nutrition_dev = get_budget('development','Nutrition'),
  nutrition_hum = get_budget('humanitarian','Nutrition'),
  wash_dev = get_budget('development','WASH'),
  wash_hum = get_budget('humanitarian','WASH'),
  ag_dev = get_budget('development','Agriculture'),
  ag_hum = get_budget('humanitarian','Agriculture'),
  health_dev = get_budget('development','Health'),
  health_hum = get_budget('humanitarian','Health'),
  ed_dev = get_budget('development','Education'),
  ed_hum = get_budget('humanitarian','Education'),
  shelter_dev = get_budget('development','Shelter'),
  shelter_hum = get_budget('humanitarian','Shelter'),
  protection_dev = get_budget('development','Protection'),
  protection_hum = get_budget('humanitarian','Protection'),
  other_dev = get_budget('development','Other'),
  other_hum = get_budget('humanitarian','Other'),
  stringsAsFactors=FALSE)
  
rm(eth,eth_clean,last,fname,i,total_lines,get_budget)
  