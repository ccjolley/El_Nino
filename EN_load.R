library(XLConnect)
library(plyr)

setwd("C:/Users/Craig/Desktop/El Nino")
en <- readWorksheetFromFile('El Nino Data Tracking Sheet.xlsx.xlsx',sheet=1)
names(en) <- c('mission','activity','loc','disbursed','obligated',
               'humanitarian','embedded','ongoing','modified','food',
               'nutrition','wash','ag','health','ed','shelter','protection',
               'other')

# disbursed and obligated need to be numbers, not strings
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

# turn the "sectoral" columns into binary variables: 1 if the string contains 
# non-whitespace; 0 otherwise

tmp <- en[,6:18]
tmp[tmp==' '] <- NA
tmp[!is.na(tmp)] <- 1
tmp[is.na(tmp)] <- 0
asNumeric <- function(x) as.numeric(as.character(x))
listNumeric <- function(d) modifyList(d, lapply(d,asNumeric))
tmp <- listNumeric(tmp)
en[6:18] <- tmp
rm(tmp,asNumeric,listNumeric)

# sanity check: for each row, the humanitarian-modified columns should sum
# to exactly one
which(rowSums(en[,c('humanitarian','embedded','ongoing','modified')]) != 1)
# In cases where there is more than one, I might end up double-counting.

# sanity check: does each row have at least one sectoral label?
noX <- which(rowSums(en[,c('food','nutrition','wash','ag','health','ed',
                           'shelter','protection','other')]) == 0)
en[noX,'other'] <- 1
rm(noX)

# now turn this into geographic data

assign_hum <- as.numeric(en$humanitarian+en$embedded > 0)
assign_dev <- as.numeric(en$ongoing+en$modified > 0)
double_count <- assign_hum == assign_dev
assign_hum[double_count] <- assign_hum[double_count]/2
assign_dev[double_count] <- assign_dev[double_count]/2

en$human_total <- rowSums(en[,c('disbursed','obligated')]) * assign_hum
en$dev_total <- rowSums(en[,c('disbursed','obligated')]) * assign_dev
head(en[,c('disbursed','obligated','human_total','dev_total')],20)

# split out activities by funding type
for (x in c('food','nutrition','wash','ag','health','ed','shelter',
            'protection','other')) {
  str_dev <- paste(x,'_dev',sep='')
  str_hum <- paste(x,'_hum',sep='')
  en[,str_dev] <- en[,x]*assign_dev
  en[,str_hum] <- en[,x]*assign_hum
}
rm(assign_dev,assign_hum,double_count,str_dev,str_hum,x)
sum(en[,c('food','nutrition','wash','ag','health','ed','shelter','protection','other')])
# I've counted 118 projects, but I only have 72 rows in my data frame; need to 
# fix double-counting *within* hum/dev categories.
overcount <- rowSums(en[,10:18])
en[,21:38] <- en[,21:38] / overcount
rm(overcount)

en2 <- en[,c('mission','human_total','dev_total','food_dev','food_hum',
          'nutrition_dev','nutrition_hum','wash_dev','wash_hum','ag_dev',
          'ag_hum','health_dev','health_hum','ed_dev','ed_hum','shelter_dev',
          'shelter_hum','protection_dev','protection_hum',
          'other_dev','other_hum')]

# First, summarize by number of projects

geo_projects <- ddply(en2,'mission',numcolwise(sum))
# rm(en,en2)
write.csv(geo_projects,'projects.csv',row.names=FALSE)

# Now, summarize by total funding in each category

en_budget <- en[,c('mission','human_total','dev_total')]
for (x in names(en)[10:18]) {
  n_dev <- paste(x,'_dev',sep='')
  n_hum <- paste(x,'_hum',sep='')
#   b_dev <- en$dev_total*en[n_dev]
#   b_hum <- en$human_total*en[,n_hum]
  b_dev <- (en$disbursed+en$obligated)*en[n_dev]
  b_hum <- (en$disbursed+en$obligated)*en[n_hum]
  en_budget[,n_dev] <- b_dev
  en_budget[,n_hum] <- b_hum
}
rm(n_dev,n_hum,b_dev,b_hum)

geo_budget <- ddply(en_budget,'mission',numcolwise(sum))

# sanity check: make sure totals match
sum(geo_budget[,2:19])
sum(geo_projects[,c('human_total','dev_total')])
sum(en[,c('disbursed','obligated')])

rm(en,en2,en_budget)
write.csv(geo_budget,'budget.csv',row.names=FALSE)

