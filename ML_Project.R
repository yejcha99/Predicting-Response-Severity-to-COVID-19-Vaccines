
#Mariam Grigoryan
#Yejin Cha
#Gordon Kong
#Claire Flynn

#data pre-processing done in R

library(tidyverse) 
library(ggplot2)
library(patchwork)
library(stringr)
library(xtable)
library(tidyverse)



#Used 3 datasets form Kaggle


#read data, specify column types
symptom_dat <- read_csv("2021VAERSSYMPTOMS.csv", 
                         col_types = cols(col_integer(),
                                          col_character(),
                                          col_double(),
                                          col_character(),
                                          col_double(),
                                          col_character(),
                                          col_double(),
                                          col_character(),
                                          col_double(),
                                          col_character(),
                                          col_double()))



vaccine_dat <- read_csv("2021VAERSVAX.csv", 
                        col_types = cols(col_integer(),
                                         col_character(),
                                         col_character(),
                                         col_character(),
                                         col_character(),
                                         col_character(),
                                         col_character(),
                                         col_character()))



#format the date columns
dat <- read_csv("2021VAERSdata.csv",
                  col_types = cols(col_integer(), 
                                   col_date(),
                                   col_character(),
                                   col_double(),
                                   col_integer(),
                                   col_double(),
                                   col_character(),
                                   col_date(),
                                   col_character(), 
                                   col_character(),
                                   col_date(),
                                   col_character(),
                                   col_character(),
                                   col_character(),
                                   col_integer(),
                                   col_character(),
                                   col_character(),
                                   col_character(),
                                   col_date(),
                                   col_date(),
                                   col_integer(), 
                                   col_character(),
                                   col_character(),
                                   col_character(),
                                   col_character(),
                                   col_character(),
                                   col_character(),
                                   col_character(),
                                   col_character(),
                                   col_integer(),
                                   col_date(),
                                   col_character(),
                                   col_character(),
                                   col_character(),
                                   col_character()))


#only keep COVID rows 
vaccine_dat <- filter(vaccine_dat, VAX_TYPE == "COVID19")

# group by manufacturer, table
manufacturer_summary <- vaccine_dat %>% group_by(VAX_MANU) %>% summarize()
gender_summary <- dat %>% group_by(SEX) %>% summarize()
state_summary <- dat %>% group_by(STATE) %>% summarize()
age_summary <- dat %>% group_by(AGE_YRS) %>% summarize()


gender_summary
manufacturer_summary
state_summary
age_summary

#xtable for prettier results
print(xtable(manufacturer_summary, caption = 'Manufacturer info'),include.rownames = FALSE,
   table.placement = "H", caption.placement = 'top')


#combine all symptoms into a column matrix with the corresponding patient ID
symptom1 <- select(symptom_dat, VAERS_ID, SYMPTOM1)
symptom1 <- rename(symptom1, SYMPTOMS = SYMPTOM1)
symptom1
symptom2 <- select(symptom_dat, VAERS_ID, SYMPTOM2)
symptom2 <- rename(symptom2, SYMPTOMS = SYMPTOM2)
symptom2
symptom3 <- select(symptom_dat, VAERS_ID, SYMPTOM3)
symptom3 <- rename(symptom3, SYMPTOMS = SYMPTOM3)
symptom3
symptom4 <- select(symptom_dat, VAERS_ID, SYMPTOM4)
symptom4 <- rename(symptom4, SYMPTOMS = SYMPTOM4)
symptom4
symptom5 <- select(symptom_dat, VAERS_ID, SYMPTOM5)
symptom5 <- rename(symptom5, SYMPTOMS = SYMPTOM5)
symptom5
new_symptom_data <- bind_rows(symptom1, symptom2, symptom3, symptom4, symptom5)

view(new_symptom_data)

# get rid of NA values
new_symptom_data <- drop_na(new_symptom_data)




new_symptom_data2 <- data.frame("VAERS_ID" = integer(), "symptom_list" = character(), stringsAsFactors = FALSE)

#from stack overflow , formatting a couple strings 
for (id in unique(new_symptom_data$VAERS_ID)) {
  id_rows <- filter(new_symptom_data, VAERS_ID == id)
  symptomtxt <- paste(id_rows$symptom, collapse="', '")
  symptomtxt <- paste("'", symptomtxt, "'", sep="")
  new_symptom_data2 <- add_row(new_symptom_data2, VAERS_ID = id, symptom_list = symptomtxt)
}


l1 <- left_join(dat, new_symptom_data2, by='VAERS_ID')
l <- left_join(l1, vaccine_dat, by='VAERS_ID')


#take only COVID values
final_data <- filter(l, VAX_TYPE == "COVID19")

                

#good 

#summary on symptoms
symptoms_list <- unique(new_symptom_data$SYMPTOMS)

#grep is  agood function
#matches to argument pattern within each element of a character vector: they differ in the format of and amount of detail in the results.
#grep(value = FALSE) returns a vector of the indices of the elements of x that yielded a match (or not, for invert = TRUE). This will be an integer vector unless the input is a long vector, when it will be a double vector.
symptoms_list[grep('pain',symptoms_list,ignore.case=TRUE)]



symptoms_of_interest <- c("Appendicitis","Anaphylactic reaction","Diarrhoea","Chills",
                        "Dizziness","Nausea","Vomiting","Pain","Headache","Pyrexia",
                        "Fatigue", "Death", "Tinnitus", "Urticaria", "Injection site pain", 
                        "Injection site warmth", "Injection site pruritus", 
                        "Injection site erythema", "Injection site swelling", 
                        "Lymphadenopathy", "Arthralgia", "Thrombosis", 
                        "Abortion spontaneous", "Dyspnoea", "COVID-19","Asthenia",
                        "Malaise","Cough","Hypoaesthesia","Rash","Feeling abnormal",
                        "Chest pain","Loss of consciousness","Facial paralysis","Flushing",
                        "Muscular weakness","Throat tightness","Hyperhidrosis",
                        "Tremor")

symptoms_of_interest <- sort(symptoms_of_interest)


#make.names makes a syntactically valid names out of character vectors.
#plus tackle case-sensitive issue
for (s in symptoms_of_interest) {
  modified_symptom_name <- make.names(s) 
  #grepl returns a logical vector (match or not for each element of x).
  final_data[[modified_symptom_name]] <- grepl(s, final_data$symptom_list, ignore.case=TRUE) 
}


#cretaing the labels code

#Create a new label column
#check if any of the serious symptom columsn are True for each row and set the new column value 
#equal to 1 of yes, 0 otherwise
#What we had before
seriousSymptoms1 <- c("ER_ED_VISIT","BIRTH_DEFECT")

seriousSymptoms2 <- c("Abortion.spontaneous", "Appendicitis","Anaphylactic.reaction", "Death"
                        ,"Dyspnoea", "Facial.paralysis", "Loss.of.consciousness", 
                        "Lymphadenopathy", "Thrombosis", "Tinnitus")
                        
               
final_data$labels <- (((rowSums(final_data[, seriousSymptoms2] == TRUE, na.rm='T') > 0) | (rowSums(final_data[, seriousSymptoms1] == 'Y', na.rm='T') > 0)) * 1)


#plot the symptoms
#need columns 44 to 82
symptom_count <- sapply(final_data[,44:82],table)
barplot(symptom_count, cex.names=0.2, xlim = c(0, 13000), horiz=TRUE, col="#69b3a2", las=1)


write.csv(final_data,"VAERS_new.csv",row.names=FALSE)


#more pre-processing, added later after we decided on the new question
#need to format a couple columns to just have ones and zeros

new_data <- read_csv("VAERS_new.csv")


history_summary <- new_data %>% group_by(HISTORY) %>% summarize()
history_summary


#modify 3the following columns as 1 or 0 if have or not
#other meds
#history
#allergies

#but ths is just is.na values, there are also NA, na, N/A, and none, None
colSums(is.na(new_data))

new_data$OTHER_MEDS  #13383 NAs
new_data$HISTORY # 11058 NAs 
new_data$ALLERGIES # 14848 NAs

#https://www.tutorialspoint.com/how-to-replace-na-s-to-a-value-of-selected-columns-in-an-r-data-frame


#not teh best solution
#convert all missing value sin the following columns to 0
#new_data[,c("OTHER_MEDS","HISTORY", "ALLERGIES")][is.na(new_data[,c("OTHER_MEDS","HISTORY", "ALLERGIES")])] <- '0'
#new_data




new_data$HISTORY <- iconv(new_data$HISTORY, 'UTF-8', 'ASCII')
new_data$OTHER_MEDS <- iconv(new_data$OTHER_MEDS, 'UTF-8', 'ASCII')
new_data$ALLERGIES <- iconv(new_data$ALLERGIES, 'UTF-8', 'ASCII')


#first convert everythign to lower case in the column
#get rid of all the punctuation in the column
new_data$HISTORY = tolower(new_data$HISTORY)
new_data$ALLERGIES = tolower(new_data$ALLERGIES)
new_data$OTHER_MEDS = tolower(new_data$OTHER_MEDS)

#remove punctuation, especially the dots and slashes in ex: none.
new_data$HISTORY<-gsub(".","",as.character(new_data$HISTORY), fixed = TRUE)
new_data$OTHER_MEDS<-gsub(".","",as.character(new_data$OTHER_MEDS), fixed = TRUE)
new_data$ALLERGIES<-gsub(".","",as.character(new_data$ALLERGIES), fixed = TRUE)
new_data$HISTORY<-gsub("/","",as.character(new_data$HISTORY), fixed = TRUE)
new_data$OTHER_MEDS<-gsub("/","",as.character(new_data$OTHER_MEDS), fixed = TRUE)
new_data$ALLERGIES<-gsub("/","",as.character(new_data$ALLERGIES), fixed = TRUE)


#this takes care of NA
new_data[,c("OTHER_MEDS","HISTORY", "ALLERGIES")][is.na(new_data[,c("OTHER_MEDS","HISTORY", "ALLERGIES")])] <- '0'
#take care of na, 
new_data$HISTORY[new_data$HISTORY == 'na'] <- '0'
new_data$HISTORY[new_data$HISTORY == 'no'] <- '0'
new_data$HISTORY[new_data$HISTORY == 'none'] <- '0'
new_data$ALLERGIES[new_data$ALLERGIES == 'na'] <- '0'
new_data$ALLERGIES[new_data$ALLERGIES == 'no'] <- '0'
new_data$ALLERGIES[new_data$ALLERGIES == 'none'] <- '0'
new_data$OTHER_MEDS[new_data$OTHER_MEDS == 'na'] <- '0'
new_data$OTHER_MEDS[new_data$OTHER_MEDS == 'no'] <- '0'
new_data$OTHER_MEDS[new_data$OTHER_MEDS == 'none'] <- '0'


#check, worked
#colSums(is.na(new_data))

typeof(new_data$OTHER_MEDS)
typeof(new_data$HISTORY)
typeof(new_data$ALLERGIES)
typeof(new_data$labels)


#change every other non-zero valeus to 1 
new_data$HISTORY[new_data$HISTORY != '0'] <- '1'
new_data$ALLERGIES[new_data$ALLERGIES != '0'] <- '1'
new_data$OTHER_MEDS[new_data$OTHER_MEDS != '0'] <- '1'



#convert these columns from double to numeric
new_data$ALLERGIES <- as.double(new_data$ALLERGIES)
typeof(new_data$ALLERGIES)


new_data$HISTORY <- as.double(new_data$HISTORY)
new_data$OTHER_MEDS <- as.double(new_data$OTHER_MEDS)

typeof(new_data$HISTORY)

typeof(new_data$OTHER_MEDS)




#dosage column manipulation
#leave 0,1 and 2 as is
#convert 3 to 2 
#convert UNK to 1
new_data$VAX_DOSE_SERIES[new_data$VAX_DOSE_SERIES == '3'] <- '2'
new_data$VAX_DOSE_SERIES[new_data$VAX_DOSE_SERIES == 'UNK'] <- '0'
new_data$VAX_DOSE_SERIES[new_data$VAX_DOSE_SERIES == 'N/A'] <- '0'
new_data[,c("VAX_DOSE_SERIES")][is.na(new_data[,c("VAX_DOSE_SERIES")])] <- '0'


#from char to double
new_data$VAX_DOSE_SERIES <- as.double(new_data$VAX_DOSE_SERIES)

new_data[,c("VAX_DOSE_SERIES")][is.na(new_data[,c("VAX_DOSE_SERIES")])] <- 0
#check , good
colSums(is.na(new_data))


#birth defect column
#birth defect only has NAs, comvert them to 0
categories <- unique(new_data$BIRTH_DEFECT) 
categories
new_data[,c("BIRTH_DEFECT")][is.na(new_data[,c("BIRTH_DEFECT")])] <- 0
new_data$BIRTH_DEFECT <- as.double(new_data$BIRTH_DEFECT)
typeof(new_data$BIRTH_DEFECT) #double, good




#final check the data
view(new_data)




#final data
write.csv(new_data,"VAERS_final.csv",row.names=FALSE)




