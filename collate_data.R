# specify working directory
workingdir<-"C:/Users/3567890/Documents/Uni/Year 2/Semester 2/Data Visualisation/DALT7016_Data_Visualisation/Data"

# set working directory - specified earlier
setwd(workingdir)

required_packages<-c("readxl", "httr", "tidyxl", "dplyr", "stringr")

for (package in required_packages){
  if (!(package %in% installed.packages()[, "Package"])){
    install.packages(package)
  }
}

# Load libraries
lapply(required_packages, library, character.only = TRUE)


`%notin%` <- Negate(`%in%`)

source("../functions.R")
source("../xls_to_xlsx.R")


##URLS
ashegenderpaygap<-"annualsurveyofhoursandearningsashegenderpaygaptables/2018revised/genderpaygap2018revised.zip"
disabilitypaygap<-"https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/healthandsocialcare/disability/datasets/rawpaygapsbydisability/current/rawpaygapstables.xlsx"
ethnicitypaygap<-"https://www.ons.gov.uk/file?uri=/employmentandlabourmarket/peopleinwork/earningsandworkinghours/datasets/ethnicitypaygapreferencetables/current/referencetablesrawpaygaps1.xlsx"

regions<-c("North East", "North West", "Yorkshire and The Humber", "East Midlands", "West Midlands", "East",
           "London", "South East", "South West", "Wales", "Scotland", "Northern Ireland", "United Kingdom")

occupations<-c("Managers, directors and senior officials", "Professional occupations", "Associate professional and technical occupations", "Administrative and secretarial occupations",
               "Skilled trades occupations", "Caring, leisure and other service occupations", "Sales and customer service occupations",
               "Process, plant and machine operatives","Elementary occupations", "Not Classified")

industries<-c("AGRICULTURE, FORESTRY AND FISHING", "MINING AND QUARRYING", "MANUFACTURING", "ELECTRICITY, GAS, STEAM AND AIR CONDITIONING SUPPLY",
              "WATER SUPPLY; SEWERAGE, WASTE MANAGEMENT AND REMEDIATION ACTIVITIES", "CONSTRUCTION", "WHOLESALE AND RETAIL TRADE; REPAIR OF MOTOR VEHICLES AND MOTORCYCLES",
              "TRANSPORTATION AND STORAGE", "ACCOMMODATION AND FOOD SERVICE ACTIVITIES", "INFORMATION AND COMMUNICATION", "FINANCIAL AND INSURANCE ACTIVITIES",
              "REAL ESTATE ACTIVITIES", "PROFESSIONAL, SCIENTIFIC AND TECHNICAL ACTIVITIES", "ADMINISTRATIVE AND SUPPORT SERVICE ACTIVITIES",
              "PUBLIC ADMINISTRATION AND DEFENCE; COMPULSORY SOCIAL SECURITY", "EDUCATION", "HUMAN HEALTH AND SOCIAL WORK ACTIVITIES",
              "ARTS, ENTERTAINMENT AND RECREATION", "OTHER SERVICE ACTIVITIES", "ACTIVITIES OF HOUSEHOLDS AS EMPLOYERS; UNDIFFERENTIATED GOODS-AND SERVICES-PRODUCING ACTIVITIES OF HOUSEHOLDS FOR OWN USE",
              "ACTIVITIES OF EXTRATERRITORIAL ORGANISATIONS AND BODIES", "NOT CLASSIFIED", "ALL EMPLOYEES")

subcategory<-c("occupationregion", "industry", "age")
tablename<-c("regionbyoccupation2digitsoc", "industry2digitsic", "agegroup")
tablenumber<-c("3", "4", "6")
ashetables <- data.frame(subcategory, tablename, tablenumber)


download_zips(ashetables, years=2016:2020)

if (file.exists("ethnicitypaygap.xlsx")==FALSE){
  download.file(ethnicitypaygap, "ethnicitypaygap.xlsx", mode="wb")
}
if (file.exists("disabilitypaygap.xlsx")==FALSE){
  download.file(disabilitypaygap, "disabilitypaygap.xlsx", mode="wb")
}


convert_xls_to_xlsx(delete_xls=T)

xlsx_files<-list.files(pattern = "\\.xlsx$")

major_df<-data.frame()

for (xlsx_file in xlsx_files){
  if (xlsx_file != "disabilitypaygap.xlsx" & xlsx_file != "ethnicitypaygap.xlsx") {
    new_df<-create_dataframe(xlsx_file)
    if ("Age Group" %in% names(new_df)){
      new_df<-new_df %>%
        rename(AgeGroup=`Age Group`)
    }
    if ("PROV - Age Group" %in% names(new_df)){
      new_df<-new_df %>%
        rename(AgeGroup=`PROV - Age Group`)
    }
    if ("PROV - SIC07 Industry (2) SIC2007" %in% names(new_df)){
      new_df<-new_df %>%
        rename(Industry=`PROV - SIC07 Industry (2) SIC2007`)
    }
    if ("PROV - Work Region Occupation SOC10 (2)" %in% names(new_df)){
      new_df<-new_df %>%
        rename(Occupation=`PROV - Work Region Occupation SOC10 (2)`)
    }
    if ("SIC07 Industry (2) SIC2007" %in% names(new_df)){
      new_df<-new_df %>%
        rename(Industry=`SIC07 Industry (2) SIC2007`)
    }
    if ("Work Region Occupation SOC10 (2)" %in% names(new_df)){
      new_df<-new_df %>%
        rename(Occupation=`Work Region Occupation SOC10 (2)`)
    }
    major_df<-bind_rows(major_df, new_df)
  }
}



major_df["WorkRegion"]<-""

for (i in 1:nrow(major_df)){
  if (str_split(major_df[i, "Occupation"], ", ", n=2)[[1]][1] %in% regions){
    major_df[i, "WorkRegion"]<-trimws(str_split(major_df[i, "Occupation"], ", ", n=2)[[1]][1])
    major_df[i, "Occupation"]<-trimws(str_split(major_df[i, "Occupation"], ", ", n=2)[[1]][2])
  }
}

rows_to_delete<-c()

for (i in 1:nrow(major_df)){
  if (trimws(major_df[i, "Occupation"], which = "right") %notin% occupations & !is.na(major_df[i, "Occupation"])){
    rows_to_delete<-append(rows_to_delete, i)
  }
  if (trimws(major_df[i, "Industry"], which = "right") %notin% industries & !is.na(major_df[i, "Industry"])){
    rows_to_delete<-append(rows_to_delete, i)
  }
}

major_df<-major_df[-rows_to_delete, , drop=FALSE]

major_df<-major_df %>%
  mutate(Industry=str_to_sentence(Industry)) %>%
  subset(Value!="x")


major_df[is.na(major_df)] <- "All"

major_df[major_df=="All employees"]<-"All"
major_df<-major_df[!(major_df$Industry=="ALL EMPLOYEES"),]
major_df<-major_df[!(major_df$Industry=="Not classified"),]
major_df<-major_df[!(major_df$WorkRegion=="United Kingdom"),]
major_df$Year<-as.integer(major_df$Year)
major_df$Value<-as.numeric(major_df$Value)


major_df<-major_df[, c("Year","Units","Sex","WorkingPattern","AgeGroup","WorkRegion","Occupation","Industry","Value","comment")]

paygapdata<-read.csv("paygapdata.csv", fileEncoding = "UTF-8-BOM", strip.white = TRUE) %>%
  mutate(WorkingPattern=str_to_sentence(WorkingPattern))
paygapdata$Value<-as.numeric(as.character(paygapdata$Value))


all_data<-bind_rows(major_df, paygapdata) %>%
  subset(Value!="x")

all_data<-all_data[, c("Year","Units","Sex","WorkingPattern","AgeGroup","WorkRegion","Occupation",
                       "Industry","Ethnicity", "Impairment", "Value","comment")]


all_data<-as.data.frame(sapply(all_data, function (x) gsub("^\\s+|\\s+$", "", x)))

all_data$AgeGroup<-gsub('16-17b', '16-17', all_data$AgeGroup)

all_data[is.na(all_data)] <- "All"
all_data[all_data==""]<-"All"

all_data$comment[all_data$comment=="All"]<-"precise"

all_data$WorkingPattern[all_data$WorkingPattern=="Full-Time"]<-"Full-time"
all_data$WorkingPattern[all_data$WorkingPattern=="Part-Time"]<-"Part-time"

write.csv(all_data, "final_data.csv", row.names = F)
