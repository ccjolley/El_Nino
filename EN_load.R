library(XLConnect)
library(plyr)
library(dplyr)

###############################################################################
# Load data
###############################################################################
setwd("C:/Users/Craig/Desktop/Live Projects/El Nino")
fname <- 'El Nino Data Tracking Sheet_0718.xlsx'
en <- readWorksheetFromFile(fname,sheet=1)
print(paste('Loaded data from',fname))
names(en) <- c('mission','activity','loc','disbursed','obligated',
               'humanitarian','embedded','ongoing','modified','food',
               'nutrition','wash','ag','health','ed','shelter','protection',
               'other')

###############################################################################
# Straighten out budget numbers
###############################################################################
bad_nums <- function(a) {
  # Utility function to spot malformed number values
  just_digits <- grep('^[$]?[0-9]+$',a)
  good_commas <- grep('^[$]?[0-9]{1,3}(, ?[0-9]{3})*$',a)
  blank <- grep('^ *$',a)
  nas <- which(is.na(a))
  setdiff(1:length(a),c(just_digits,good_commas,blank,nas))
}

bad_budget <- c(bad_nums(en$disbursed),bad_nums(en$obligated))
en[bad_budget,c('mission','disbursed','obligated')]

# disbursed and obligated need to be numbers, not strings
# TODO: this would look nicer formatted with pipes
en$obligated <- sub('\\$','',en$obligated)
en$obligated <- gsub(',','',en$obligated)
en$obligated <- gsub(' ','',en$obligated)
en$obligated <- as.integer(en$obligated)
en[is.na(en$obligated),'obligated'] <- 0

en$disbursed <- sub('\\$','',en$disbursed)
en$disbursed <- gsub(',','',en$disbursed)
en$disbursed <- gsub(' ','',en$disbursed)
en$disbursed <- as.integer(en$disbursed)
en[is.na(en$disbursed),'disbursed'] <- 0

# In Malawi, every project is either all disbursed or all obligated; no 
# conflicts.
en[en$mission=='Malawi',c('disbursed','obligated')]

# In Mozambique, most projects only give information about obligations.
# In the cases where both are present, go with the larger of the two.
en[en$mission=='Mozambique',c('disbursed','obligated')]

en[en$mission=='Madagascar',c('disbursed','obligated')]

# In Zimbabwe, consider the "obligated" column to be the total for the 
# project.
en[en$mission=='Zimbabwe',c('disbursed','obligated')]

en$budget <- en$disbursed + en$obligated
en[en$obligated > en$disbursed,'budget'] <- en[en$obligated > en$disbursed,'obligated']

# There's one project where the total amount in the activity description 
# is the sum of what's in the disbursed & obligated columns; not sure
# what's happening there.
odd <- grep('WFP Productive Asset',en[,2])
en[odd,'budget'] <- en[odd,'disbursed'] + en[odd,'obligated']

# Pick up numbers in the humanitarian-modified columns. If there are no other
# budget numbers, use those.
odd_budgets <- select(en,humanitarian:modified) %>% 
  na_if('X') %>%
  na_if(' ') %>%
  transmute(x = coalesce(humanitarian,embedded,ongoing,modified)) 
odd_budgets <- gsub('[, $]','',odd_budgets$x) %>% as.numeric()
odd_budgets[!is.na(odd_budgets)]
en[en$budget == 0 & !is.na(odd_budgets),'budget'] <- 
  odd_budgets[en$budget == 0 & !is.na(odd_budgets)]
rm(bad_budget,odd,odd_budgets)

# who has no budget information?
which(en$budget==0)

###############################################################################
# Format "sectoral" columns into binary variables: 1 if the string contains 
# non-whitespace; 0 otherwise
###############################################################################
tmp <- en[,6:18]
tmp[tmp==' '] <- NA
tmp[!is.na(tmp)] <- 1
tmp[is.na(tmp)] <- 0
asNumeric <- function(x) as.numeric(as.character(x))
listNumeric <- function(d) modifyList(d, lapply(d,asNumeric))
tmp <- listNumeric(tmp)
en[6:18] <- tmp
rm(tmp,asNumeric,listNumeric)

# sanity check: does each row have at least one sectoral label?
noX <- which(rowSums(en[,c('food','nutrition','wash','ag','health','ed',
                           'shelter','protection','other')]) == 0)
en[noX,'other'] <- 1
rm(noX)


###############################################################################
# Assign projects as development or humanitarian assistance
###############################################################################
# sanity check: for each row, the humanitarian-modified columns should sum
# to exactly one
which(rowSums(en[,c('humanitarian','embedded','ongoing','modified')]) > 1)
which(rowSums(en[,c('humanitarian','embedded','ongoing','modified')]) == 0)
# In cases where there is more than one, I might end up double-counting.

assign_hum <- as.numeric(en$humanitarian+en$embedded > 0)
assign_dev <- as.numeric(en$ongoing+en$modified > 0)
double_count <- assign_hum == assign_dev
assign_hum[double_count] <- 0.5
assign_dev[double_count] <- 0.5

###############################################################################
# Fix projects that have been (illogically) labeled as development food 
# assistance.
###############################################################################
not_just_food <- (en$nutrition + en$wash + en$ag + en$health + en$ed + 
                    en$shelter + en$protection + en$other) > 0
en[assign_dev > 0 & en$food == 1,] %>%
  select(humanitarian:other)
# if these are food-only activities that have been classified as both 
# humanitarian and development, let's call them humanitarian
make_hum <- which(en$food == 1 & assign_hum > 0 & assign_dev > 0 & !not_just_food)
assign_dev[make_hum] <- 0
assign_hum[make_hum] <- 1
# if activities have been assigned as development only and have other
# sectors besides food, remove the food label
en[assign_dev > 0 & assign_hum == 0 & not_just_food & en$food==1,'food'] <- 0
# inspect again
en[assign_dev > 0 & en$food == 1,] %>%
  select(humanitarian:budget)
# There are a few projects left that are genuinely ambiguous. Fix these below.

###############################################################################
# Split up budget as humanitarian or development
###############################################################################
en$human_total <- en$budget * assign_hum
en$dev_total <- en$budget * assign_dev
head(en[,c('disbursed','obligated','human_total','dev_total')],20)


###############################################################################
# Classify projects by both sector and funding source
###############################################################################
for (x in c('food','nutrition','wash','ag','health','ed','shelter',
            'protection','other')) {
  str_dev <- paste(x,'_dev',sep='')
  str_hum <- paste(x,'_hum',sep='')
  en[,str_dev] <- en[,x]*assign_dev
  en[,str_hum] <- en[,x]*assign_hum
}
en %>% select(food_dev:other_hum) %>% sum()
en %>% select(food:other) %>% sum()
# These numbers should match.

rm(double_count,str_dev,str_hum,x)
sum(en[,c('food','nutrition','wash','ag','health','ed','shelter','protection','other')])
# I've counted 154 projects, but I only have 94 rows in my data frame; need to 
# fix double-counting *within* hum/dev categories.
overcount <- rowSums(en[,10:18])
en[,22:39] <- en[,22:39] / overcount
rm(overcount)

en %>% select(food_dev:other_hum) %>% sum()
nrow(en)
# These numbers should also match.

###############################################################################
# Still trying to fix the development food thing
###############################################################################
# inspect
en %>% 
  filter(food_dev > 0) %>%
  select(food_dev:nutrition_hum)
# If there is a development nutrition component, put it there
move_to_nut <- which(en$food_dev > 0 & en$nutrition_dev > 0)
en[move_to_nut,'nutrition_dev'] <- 
  en[move_to_nut,'food_dev'] + en[move_to_nut,'nutrition_dev']
en[move_to_nut,'food_dev'] <- 0
# inspect again
en %>% 
  filter(food_dev > 0) %>%
  select(food_dev:nutrition_hum)

###############################################################################
# Ready to export
###############################################################################

en2 <- en[,c('mission','human_total','dev_total','food_dev','food_hum',
          'nutrition_dev','nutrition_hum','wash_dev','wash_hum','ag_dev',
          'ag_hum','health_dev','health_hum','ed_dev','ed_hum','shelter_dev',
          'shelter_hum','protection_dev','protection_hum',
          'other_dev','other_hum')]

# First, summarize by number of projects

geo_projects <- ddply(en2,'mission',numcolwise(sum))
write.csv(geo_projects,'projects.csv',row.names=FALSE)

# Now, summarize by total funding in each category

en_budget <- en[,c('mission','human_total','dev_total')]
for (x in names(en)[10:18]) {
  n_dev <- paste(x,'_dev',sep='')
  n_hum <- paste(x,'_hum',sep='')
#   b_dev <- en$dev_total*en[n_dev]
#   b_hum <- en$human_total*en[,n_hum]
  b_dev <- en$budget*en[n_dev]
  b_hum <- en$budget*en[n_hum]
  en_budget[,n_dev] <- b_dev
  en_budget[,n_hum] <- b_hum
}
rm(n_dev,n_hum,b_dev,b_hum)

geo_budget <- ddply(en_budget,'mission',numcolwise(sum))

# sanity check: make sure totals match
sum(geo_budget[,4:21])
sum(geo_projects[,c('human_total','dev_total')])
sum(en$budget)

nrow(en)
sum(en[,22:39])

rm(en,en2,en_budget)
write.csv(geo_budget,'budget.csv',row.names=FALSE)

###############################################################################
# Finally, adhere to USAID color palette
###############################################################################

usaid_red <- '#ba0c2f'
usaid_blue <- '#002F6C'
med_blue <- '#0067b9'
dk_red <- '#651d32'
dk_gray <- '#6c6463'
med_gray <- '#8c8985'
r_black <- '#212721'

print(paste('Total El Nino investment:',
            sum(geo_budget[,4:21])/1e6))