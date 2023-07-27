
#TITLE: NYC Restaurant Inspection Results Data Set Clean-Up 
#GOAL: Create 4 separate tables (Council, Inspections, Restaurants, Violations)
#      to import into SQL in order to create a relational database

#PART 1 
NYC <- DOHMH_New_York_City_Restaurant_Inspection_Results_FINAL

#Renaming column name INSPECTION DATE to INSP_DATE
names(NYC)[names(NYC) == "INSPECTION DATE"] <- "INSP_DATE"

#Filtering data to include only 2019 dates 
NYC_2019 <- subset(NYC, format(INSP_DATE, "%Y") == '2019')

#Checking frequency of GRADE
table(NYC_2019$GRADE)

#Filtering data to include only rows which are graded
NYC_ <- subset(NYC_2019, GRADE == "A"|GRADE == "B"| GRADE == "C")
table(NYC_$GRADE)

library(dplyr)
n_distinct(NYC_$CAMIS)

#Finding 2019 violations of the same date
NYC_dates <- NYC_[, c("CAMIS", "INSP_DATE")]

first_dates <- aggregate(INSP_DATE ~ CAMIS, NYC_dates, min)

NYC_official <- merge(NYC_, first_dates, by = c("CAMIS", "INSP_DATE"))

#Renaming Violation Description variable to V_DESC
names(NYC_official)[names(NYC_official) == "VIOLATION DESCRIPTION"] <- "V_DESC"

table(NYC_official$V_DESC)
table(NYC_official$BORO)
table(NYC_official$ZIPCODE)

#Removing 0 values from boroughs of NYC
NYC_official <- NYC_official[NYC_official$BORO !=0,]
table(NYC_official$BORO)

#Renaming columns
names(NYC_official)[names(NYC_official) == "CUISINE DESCRIPTION"] <- "CUISINE"
names(NYC_official)[names(NYC_official) == "INSPECTION TYPE"] <- "INSPEC_TYPE"
names(NYC_official)[names(NYC_official) == "Council District"] <- "COUNCIL_DIST"
names(NYC_official)[names(NYC_official) == "Community Board"] <- "COMM_BOARD"
names(NYC_official)[names(NYC_official) == "GRADE DATE"] <- "GRADE_DATE"
names(NYC_official)[names(NYC_official) == "CRITICAL FLAG"] <- "CRITICAL_FLAG"
names(NYC_official)[names(NYC_official) == "CAMIS"] <- "RESTAURANT_ID"

#Removing variables
rm(NYC_Violation,NYC_Inspection, NYC_Council)

#Creating tables/dataframe (NYC_Restaurant, NYC_Violation, NYC_Inpsection, NYC_Council) for export into SQL
#1
NYC_Restaurant <- select(NYC_official1, RESTAURANT_ID, BORO, ZIPCODE, CUISINE)

#2-4 Assigning unique identifiers using mutate function to ensure each table has a Primary Key
# Rearranging columns to ensure that the PK in each table is the first column
NYC_Violation <- select(NYC_official1, RESTAURANT_ID, V_DESC, GRADE_DATE)
NYC_Violation <- NYC_Violation %>% mutate(Violation_ID = 1:nrow(NYC_Violation))
NYC_Violation <- select(NYC_Violation, Violation_ID, everything())

#3
NYC_Inspection <-select(NYC_official1, RESTAURANT_ID,INSPEC_TYPE,CRITICAL_FLAG ,SCORE, GRADE)
NYC_Inspection <- NYC_Inspection %>% mutate(Inspection_ID = 1:nrow(NYC_Inspection))
NYC_Inspection <- select(NYC_Inspection, Inspection_ID, everything())

#4
NYC_Council <- select(NYC_official1, RESTAURANT_ID, COUNCIL_DIST, COMM_BOARD)
NYC_Council <- NYC_2 %>% mutate(Council_ID = 1:nrow(NYC_2))
NYC_Council <- select(NYC_Council, Council_ID, everything())

#Exporting dataframes as csv
write.csv(NYC_Restaurant, "NYC_Restaurant.csv", row.names = FALSE)
write.csv(NYC_1, "NYC_Restaurant.csv", row.names = FALSE)
write.csv(NYC_Inspection, "NYC_Inspection.csv", row.names = FALSE)
write.csv(NYC_Violation, "NYC_Violation.csv", row.names = FALSE)
write.csv(NYC_Council, "NYC_Council.csv", row.names = FALSE)

rm(NYC_2)

#Creating only distinct restaurantID
n_distinct(NYC_official1$RESTAURANT_ID)
NYC_1 <- NYC_Restaurant %>%
  distinct(RESTAURANT_ID, .keep_all = TRUE)

n_distinct(NYC_official1$RESTAURANT_ID)
NYC_2 <- NYC_Council %>%
  distinct(RESTAURANT_ID, .keep_all = TRUE)

#PART 2
# Encountered errors when attempting to import into SQL

#Import into SQL failed due to unrecognizable symbols 
#Changing symbols into words
NYC_Violation$V_DESC <- gsub("Âº", " degrees", NYC_Violation$V_DESC)
table(NYC_Violation$V_DESC)

NYC_Violation$V_DESC <- gsub("facilityâ€™s", " facility", NYC_Violation$V_DESC)
table(NYC_Violation$V_DESC)

#Checking for NA
which(!complete.cases(NYC_Council$COUNCIL_DIST)) 
which(!complete.cases(NYC_Council$COMM_BOARD)) 

#Removing NA from comm_board and council to import into SQL
NYC_official1 <- NYC_official[complete.cases(NYC_official$COMM_BOARD,NYC_official$COUNCIL_DIST),]











