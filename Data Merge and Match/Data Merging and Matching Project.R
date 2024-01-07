# loading / installing necessary packages
if(require("lubridate")){
  print("lubridate is loaded correctly")
} else {
  print("trying to install lubridate")
  install.packages("lubridate")
  if(require(lubridate)){
    print("lubridate installed and loaded")
  } else {
    stop("could not install lubridate")
  }
}

if(require("tidyverse")){
  print("tidyverse is loaded correctly")
} else {
  print("trying to install tidyverse")
  install.packages("tidyverse")
  if(require(tidyverse)){
    print("tidyverse installed and loaded")
  } else {
    stop("could not install tidyverse")
  }
}

if(require("sjmisc")){
  print("sjmisc is loaded correctly")
} else {
  print("trying to install sjmisc")
  install.packages("sjmisc")
  if(require(sjmisc)){
    print("sjmisc installed and loaded")
  } else {
    stop("could not install sjmisc")
  }
}

if(require("strex")){
  print("strex is loaded correctly")
} else {
  print("trying to install strex")
  install.packages("strex")
  if(require(strex)){
    print("strex installed and loaded")
  } else {
    stop("could not install strex")
  }
}



setwd("C:/Users/eelie/OneDrive - Deciphera Pharmaceuticals/Desktop/3116-01-001 files")
PC3_26NOV2023 <- read.csv("DCC-3116-01-001_210_26NOV2023.csv")
PC4_26NOV2023 <- read.csv("DCC-3116-01-001_212_26NOV2023.csv")
Source_03NOV2023 <- read.csv("FDX (3404) DCC-3116-01-001-Clinical-Tracking_03NOV2023_NKS.csv")
PC3_26NOV2023$PC3DAT <- as.Date(PC3_26NOV2023$PC3DAT, "%m/%d/%Y")
PC4_26NOV2023$PC4DAT <- as.Date(PC4_26NOV2023$PC4DAT, "%d-%b-%y")
Source_03NOV2023$Collection.Date <- as.Date(Source_03NOV2023$Collection.Date, "%d-%b-%Y")
EDC_PDWB <- full_join(PC3_26NOV2023, PC4_26NOV2023)
EDC_PDWB <- EDC_PDWB %>% select(Subject, FolderName, PC3DAT, PC3TIM, PC4DAT, PC4TIM, PC4TPT)
Source <- Source_03NOV2023 %>% select(Subject.ID, Visit, Collection.Date, Collection.Time)

##################################################################################################
# read CSV data into R
# EDC_PDWB <- read.csv("EDC_PDWB_26Nov23.csv")
# EDC_PDWB <- EDC_PDWB[,-1]
# Source <- read.csv("Source_PDWB_26Nov23.csv")
# Source <- Source[,-1]

# count missing collection dates from Source; check for "nonsense" values
sum(is.na(Source$Collection.Date))
table(Source$Collection.Date)
# 2678 blank, 1 ???, 1 "17OCT023"

# count missing collection times from Source; check for "nonsense" values
sum(is.na(Source$Collection.Time))
table(Source$Collection.Time)
# 2679 blank, 1 ???, 1 N/A

# delete rows from Source with missing collection dates
Source <- Source %>% drop_na(Collection.Date) # no longer needed

# delete rows from Source with missing/nonsense collection times
Source <- Source %>% drop_na(Collection.Time) %>% filter(Collection.Time != "", Collection.Time != "???", Collection.Time != "N/A")

# recheck
table(Source$Collection.Date)
table(Source$Collection.Time)

# fix 17OCT023 entry
Source$Collection.Date[Source$Collection.Date == "17OCT023"] <- "17-Oct-2023"


# recheck
table(Source$Collection.Date)
# Everything looks good


########################################
# Change date string formats so that Source matches EDC_PDWB
Source_Date_months <- month(as.Date(Source$Collection.Date, "%d-%b-%Y"))
Source_Date_days <- day(as.Date(Source$Collection.Date, "%d-%b-%Y"))
Source_Date_years <- year(as.Date(Source$Collection.Date, "%d-%b-%Y"))

Source_Date_month_chars <- sprintf('%02d', Source_Date_months)
Source_Date_day_chars <- sprintf('%02d', Source_Date_days)
Source_Date_year_chars <- as.character(Source_Date_years)

Source_Date_chars <- paste(Source_Date_year_chars, Source_Date_month_chars, Source_Date_day_chars, sep = "-")
head(Source_Date_chars)

Source$New.Collection.Date <- Source_Date_chars

########################################
# Compare "Visit" from Source to "FolderName" and "PC4TPT" from EDC

# Check tables for missing/nonsense values
table(Source$Visit)
# Each entry contains "C#D# Pre/4HR/2HR", "EOT", "SCR", or "Odd Cycle"

table(EDC_PDWB$FolderName)
table(EDC_PDWB$PC4TPT)

# Check unique years
unique(year(Source$New.Collection.Date))
unique(year(EDC_PDWB$PC3DAT))
unique(year(EDC_PDWB$PC4DAT))


# initialize empty data frame for final "no match" output
PDWB_Output <- data.frame()


# find matches
for (subject in unique(Source$Subject.ID)) {
  EDC_subset <- EDC_PDWB %>%
    filter(Subject == subject)
  Source_subset <- Source %>%
    filter(Subject.ID == subject)
  Source_subset$FolderName <- "None"
  Source_subset$PC4TPT <- "None"
  Source_subset$match <- NA
  Source_subset$Collection.Time <- str_remove(Source_subset$Collection.Time, "^0+")
  EDC_subset$PC3TIM <- str_remove(EDC_subset$PC3TIM, "^0+")
  EDC_subset$PC4TIM <- str_remove(EDC_subset$PC4TIM, "^0+")
  
  for (source_row1 in 1:nrow(Source_subset)) {
    if (str_contains(Source_subset$Visit[source_row1], "d", ignore.case = TRUE) &
        !str_contains(Source_subset$Visit[source_row1], "odd", ignore.case = TRUE)) {
      visit_cycle <- str_first_number(Source_subset$Visit[source_row1])
      visit_day <- str_nth_number(Source_subset$Visit[source_row1], n = 2)
      Source_subset$FolderName[source_row1] <- paste("Cycle ", as.character(visit_cycle), ", Day ", as.character(visit_day), sep = "")
      
      last_str <- str_last_non_numeric(Source_subset$Visit[source_row1])
      if (str_contains(last_str, "pre", ignore.case = TRUE)) {
        TPT <- "Pre-Dose"
      } else if (str_contains(last_str, "hr", ignore.case = TRUE)) {
        TPT <- paste(as.character(str_nth_number(Source_subset$Visit[source_row1], n = -1)), "Hours Post-Dose")
      } else {
        TPT <- "None"
      }
      
      Source_subset$PC4TPT[source_row1] <- TPT
      
    } else if (str_contains(Source_subset$Visit[source_row1], "eot", ignore.case = TRUE)) {
      Source_subset$FolderName[source_row1] <- "End of Treatment"
    } else if (str_contains(Source_subset$Visit[source_row1], "scr", ignore.case = TRUE)) {
      Source_subset$FolderName[source_row1] <- "Screening"
    } else {
      Source_subset$FolderName[source_row1] <- "None"
    }
  }
  
  EDC_subset$PC4TPT[is.na(EDC_subset$PC4TPT)] <- "None"
  
  # Check for matching strings
  for (source_row2 in 1:nrow(Source_subset)) {
    if ((!is.na(match(Source_subset[source_row2, "New.Collection.Date"], EDC_subset$PC3DAT))) &
        (match(Source_subset[source_row2, "New.Collection.Date"], EDC_subset$PC3DAT) > 0) &
        (!is.na(match(Source_subset[source_row2, "Collection.Time"], EDC_subset$PC3TIM))) &
        (match(Source_subset[source_row2, "Collection.Time"], EDC_subset$PC3TIM) %in% which(EDC_subset$PC3DAT %in% Source_subset[source_row2, "New.Collection.Date"])) &
        (!is.na(match(Source_subset[source_row2, "FolderName"], EDC_subset[match(Source_subset[source_row2, "Collection.Time"], EDC_subset$PC3TIM), "FolderName"]))) &
        (Source_subset[source_row2, "FolderName"] == EDC_subset[match(Source_subset[source_row2, "Collection.Time"], EDC_subset$PC3TIM), "FolderName"])) {
      Source_subset$match[source_row2] <- "Match PC3 Date, Time and Visit"
      
    } else if ((!is.na(match(Source_subset[source_row2, "New.Collection.Date"], EDC_subset$PC4DAT))) &
               (match(Source_subset[source_row2, "New.Collection.Date"], EDC_subset$PC4DAT) > 0) &
               (!is.na(match(Source_subset[source_row2, "Collection.Time"], EDC_subset$PC4TIM))) &
               (match(Source_subset[source_row2, "Collection.Time"], EDC_subset$PC4TIM) %in% which(EDC_subset$PC4DAT %in% Source_subset[source_row2, "New.Collection.Date"])) &
               (!is.na(match(Source_subset[source_row2, "FolderName"], EDC_subset[match(Source_subset[source_row2, "Collection.Time"], EDC_subset$PC4TIM), "FolderName"]))) &
               (Source_subset[source_row2, "FolderName"] == EDC_subset[match(Source_subset[source_row2, "Collection.Time"], EDC_subset$PC4TIM), "FolderName"]) &
               (!is.na(match(Source_subset[source_row2, "PC4TPT"], EDC_subset[match(Source_subset[source_row2, "Collection.Time"], EDC_subset$PC4TIM), "PC4TPT"]))) &
               (Source_subset[source_row2, "PC4TPT"] == EDC_subset[match(Source_subset[source_row2, "Collection.Time"], EDC_subset$PC4TIM), "PC4TPT"])) {
      Source_subset$match[source_row2] <- "Match PC4 Date, Time, Visit, and TPT"
      
    } else {
      Source_subset$match[source_row2] <- "No Match"
    }
  }
  
  PDWB_Output <- rbind(PDWB_Output, Source_subset %>% filter(match == "No Match"))
}


colnames(EDC_PDWB)[1] <- "Subject.ID"
PDWB_Output2 <- left_join(PDWB_Output, EDC_PDWB, by = c("Subject.ID", "FolderName", "PC4TPT"))
View(PDWB_Output2)
write.csv(PDWB_Output2, "PDWB_Output.csv", row.names = FALSE)