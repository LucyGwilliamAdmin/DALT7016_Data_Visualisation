# specify working directory
workingdir<-"C:/Users/3567890/Documents/Uni/Year 2/Semester 2/Data Visualisation/DALT7016_Data_Visualisation"

# set working directory - specified earlier
setwd(workingdir)

required_packages<-c("shiny", "shinydashboard", "shinyWidgets", "ggplot2", "dplyr", "stringr", "RColorBrewer")

for (package in required_packages){
  if (!(package %in% installed.packages()[, "Package"])){
    install.packages(package)
  }
}

# Load libraries
lapply(required_packages, library, character.only = TRUE)

# if (file.exists("Data/final_data.csv")==FALSE){
#   source("collate_data.R")
#   # reset working
#   workingdir<-"C:/Users/3567890/Documents/Uni/Year 2/Semester 2/Data Visualisation/DALT7016_Data_Visualisation"
#   setwd(workingdir)
# }

all_data<-read.csv("Data/final_data.csv", fileEncoding = "UTF-8-BOM") 

all_data$Sex <- factor(all_data$Sex,levels = c("All", "Female", "Male"))
all_data$WorkingPattern <- factor(all_data$WorkingPattern, levels = c("All", "Full-time", "Part-time"))
all_data$AgeGroup <- factor(all_data$AgeGroup, levels = c("All", "16-17", "16-19", "18-21", "20-24", "22-29", "25-29", "30-34",
                                                          "30-39", "35-39", "40-44", "40-49", "45-49", "50-54", "50-59",
                                                          "55-59", "60-64", "60+"))
all_data$Occupation <- factor(all_data$Occupation, levels = c("All", "Administrative and secretarial occupations",
                                                              "Associate professional and technical occupations",
                                                              "Caring, leisure and other service occupations",
                                                              "Elementary occupations",
                                                              "Managers, directors and senior officials",
                                                              "Process, plant and machine operatives",
                                                              "Professional occupations",
                                                              "Sales and customer service occupations",
                                                              "Skilled trades occupations"))
all_data$Ethnicity <- factor(all_data$Ethnicity, levels = c("All", "White", "Mixed", "Mixed/multiple ethnic groups", "Asian", "Indian",
                                                            "Pakistani", "Bangladeshi", "Chinese", "Any other Asian background",
                                                            "Black", "Black/African/Caribbean/Black British",
                                                            "Other ethnic group", "Other"))
all_data$WorkRegion <- factor(all_data$WorkRegion, levels = c("All", "Wales", "Scotland", "Northern Ireland", "North East",
                                                              "North West", "Yorkshire and The Humber", "East Midlands",
                                                              "West Midlands", "East", "London", "South East", "South West"))
all_data$Industry <- factor(all_data$Industry, levels = c("All", LETTERS[1:21]))

ind_map<-list("A"="(Agriculture, Forestry and Fishing)",
              "B"="(Mining and Quarrying)",
              "C"="(Manufacturing)",
              "D"="(Electricity, Gas, Steam and Air Conditioning Supply)",
              "E"="(Water Supply; Sewerage, Waste Management and Remediation Activities)",
              "F"="(Construction)",
              "G"="(Wholesale and Retail Trade; Repair of Motor Vehicles and Motorcycles)",
              "H"="(Transportation and Storage)",
              "I"="(Accommodation and Food Service Activities)",
              "J"="(Information and Communication)",
              "K"="(Financial and Insurance Activities)",
              "L"="(Real Estate Activities)",
              "M"="(Professional, Scientific and Technical Activities)",
              "N"="(Administrative and Support Service Activities)",
              "O"="(Public Administration and Defence; Compulsory Social Security)",
              "P"="(Education)",
              "Q"="(Human Health and Social Work Activities)",
              "R"="(Arts, Entertainment and Recreation)",
              "S"="(Other Service Activities)",
              "T"="(Activities of Households as Employers; Undifferentiate Goods and Services Producing Activities of Households for Own Use)",
              "U"="(Activities of Extraterritorial Organisations and Bodies)")

ind_map2<-list("All"="",
               "A"="(Agriculture, Forestry and Fishing)",
               "B"="(Mining and Quarrying)",
               "C"="(Manufacturing)",
               "D"="(Electricity, Gas, Steam and Air Conditioning Supply)",
               "E"="(Water Supply; Sewerage, Waste Management and Remediation Activities)",
               "F"="(Construction)",
               "G"="(Wholesale and Retail Trade; Repair of Motor Vehicles and Motorcycles)",
               "H"="(Transportation and Storage)",
               "I"="(Accommodation and Food Service Activities)",
               "J"="(Information and Communication)",
               "K"="(Financial and Insurance Activities)",
               "L"="(Real Estate Activities)",
               "M"="(Professional, Scientific and Technical Activities)",
               "N"="(Administrative and Support Service Activities)",
               "O"="(Public Administration and Defence; Compulsory Social Security)",
               "P"="(Education)",
               "Q"="(Human Health and Social Work Activities)",
               "R"="(Arts, Entertainment and Recreation)",
               "S"="(Other Service Activities)",
               "T"="(Activities of Households as Employers; Undifferentiate Goods and Services Producing Activities of Households for Own Use)",
               "U"="(Activities of Extraterritorial Organisations and Bodies)")

paygap_data<-all_data %>%
  filter(Units %in% c("Disability pay gap (%)", "Ethnicity pay gap (%)", "Gender pay gap (%)"))

paygap_data<-droplevels(paygap_data)

comparison_data<-all_data %>%
  filter(Units=="Median")

comparison_data<-droplevels(comparison_data)


available_breakdowns_gender<-list("Working pattern"="WorkingPattern", "Age group"="AgeGroup",
                                  "Work region"="WorkRegion", "Occupation"="Occupation")

available_breakdowns_ethnicity<-list("Ethnicity"="Ethnicity")

available_breakdowns_disability<-list("Working pattern"="WorkingPattern", "Sex"="Sex","Age group"="AgeGroup",
                                      "Work region"="WorkRegion", "Occupation"="Occupation", "Ethnicity"="Ethnicity", "Impairment")



available_breakdowns<-list("Gender pay gap (%)"=available_breakdowns_gender,
                           "Ethnicity pay gap (%)"=available_breakdowns_ethnicity,
                           "Disability pay gap (%)"=available_breakdowns_disability)

total_data<-comparison_data %>%
  filter_at(vars(names(comparison_data)[!names(comparison_data) %in% c("Year", "Value", "Units", "comment")]), all_vars(.=="All"))


shape_set<-c(rep("circle", times=7),
             rep("star-triangle-up", times=6),
             rep("square", times=6),
             rep("diamond", times=6))


colour_set<-c("#00006A", '#cd7a00', '#339966', '#9966cc', '#8d4d57', '#A33600', '#054ce6',
              '#cd7a00', '#339966', '#9966cc', '#8d4d57', '#A33600', '#054ce6',
              '#cd7a00', '#339966', '#9966cc', '#8d4d57', '#A33600', '#054ce6',
              '#cd7a00', '#339966', '#9966cc', '#8d4d57', '#A33600', '#054ce6')

gap_colour_set<-c('#00006Abf', '#cd7a00bf', '#339966bf', '#9966ccbf', '#8d4d57bf', '#A33600bf', '#054ce6bf',
                  '#cd7a00bf', '#339966bf', '#9966ccbf', '#8d4d57bf', '#A33600bf', '#054ce6bf',
                  '#cd7a00bf', '#339966bf', '#9966ccbf', '#8d4d57bf', '#A33600bf', '#054ce6bf',
                  '#cd7a00bf', '#339966bf', '#9966ccbf', '#8d4d57bf', '#A33600bf', '#054ce6bf')

size_set<-c(rep(3, times = 7), rep(4, times=6), rep(3, times=12))

sexColours <- setNames(head(colour_set, length(levels(as.factor(comparison_data[["Sex"]])))), levels(as.factor(comparison_data[["Sex"]])))
wpColours <- setNames(head(colour_set, length(levels(as.factor(comparison_data[["WorkingPattern"]])))), levels(as.factor(comparison_data[["WorkingPattern"]])))
ageColours <- setNames(head(colour_set, length(levels(as.factor(comparison_data[["AgeGroup"]])))), levels(as.factor(comparison_data[["AgeGroup"]])))
wrColours <- setNames(head(colour_set, length(levels(as.factor(comparison_data[["WorkRegion"]])))), levels(as.factor(comparison_data[["WorkRegion"]])))
occColours <- setNames(head(colour_set, length(levels(as.factor(comparison_data[["Occupation"]])))), levels(as.factor(comparison_data[["Occupation"]])))
indColours <- setNames(head(colour_set, length(levels(as.factor(comparison_data[["Industry"]])))), levels(as.factor(comparison_data[["Industry"]])))

sexShapes <- setNames(head(shape_set, length(levels(as.factor(comparison_data[["Sex"]])))), levels(as.factor(comparison_data[["Sex"]])))
wpShapes <- setNames(head(shape_set, length(levels(as.factor(comparison_data[["WorkingPattern"]])))), levels(as.factor(comparison_data[["WorkingPattern"]])))
ageShapes <- setNames(head(shape_set, length(levels(as.factor(comparison_data[["AgeGroup"]])))), levels(as.factor(comparison_data[["AgeGroup"]])))
wrShapes <- setNames(head(shape_set, length(levels(as.factor(comparison_data[["WorkRegion"]])))), levels(as.factor(comparison_data[["WorkRegion"]])))
occShapes <- setNames(head(shape_set, length(levels(as.factor(comparison_data[["Occupation"]])))), levels(as.factor(comparison_data[["Occupation"]])))
indShapes <- setNames(head(shape_set, length(levels(as.factor(comparison_data[["Industry"]])))), levels(as.factor(comparison_data[["Industry"]])))

graph_titles<-c("Gender pay gap (%)"="Gender pay gap median (comparison group = men)", "Ethnicity pay gap (%)"="Ethnicity pay gap median (comparison group = white employees)", "Disability pay gap (%)"="Disability pay gap median (comparison group = employees without a disability)")

submenu_mapping<-c("sex"="Sex", "wp"="WorkingPattern", "age"="AgeGroup", "wr"="WorkRegion", "occ"="Occupation", "ind"="Industry")

data_download<-function(variable){
  data<-comparison_data %>%
    filter(eval(parse(text=variable)) %in% unique(comparison_data[[variable]])) %>%
    filter_at(vars(names(comparison_data)[!names(comparison_data) %in% c("Year", "Units", "Value", variable, "comment")]), all_vars(.=="All")) %>%
    select(Year,Units,where(~length(unique(.))>1),comment)
  return(data)
}



sex_data_download<-data_download("Sex")
wp_data_download<-data_download("WorkingPattern")
age_data_download<-data_download("AgeGroup")
wr_data_download<-data_download("WorkRegion")
occ_data_download<-data_download("Occupation")
ind_data_download<-data_download("Industry")


download_text <- "Download chart data as CSV"
