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

if (file.exists("final_data.csv")==FALSE){
  source("collate_data.R")
  # reset working
  workingdir<-"C:/Users/3567890/Documents/Uni/Year 2/Semester 2/Data Visualisation"
  setwd(workingdir)
}



all_data<-read.csv("final_data.csv", fileEncoding = "UTF-8-BOM")

paygap_data<-read.csv("paygapdata.csv", fileEncoding = "UTF-8-BOM")
comparison_data<-read.csv("compare_dataset.csv", fileEncoding = "UTF-8-BOM")

#paygap_data[paygap_data==""]<-"Total"
#comparison_data[comparison_data==""]<-"Total"

#comparison_data$Value<-as.numeric(comparison_data$Value)

available_breakdowns_gender<-list("Working pattern"="WorkingPattern", "Age group"="AgeGroup",
                               "Work region"="WorkRegion", "Occupation"="Occupation")

available_breakdowns_ethnicity<-list("Ethnicity"="Ethnicity")

available_breakdowns_disability<-list("Working pattern"="WorkingPattern", "Sex"="Sex","Age group"="AgeGroup",
                                      "Work region"="WorkRegion", "Occupation"="Occupation", "Ethnicity"="Ethnicity", "Impairment")



available_breakdowns<-list("Gender pay gap (%)"=available_breakdowns_gender,
                           "Ethnicity pay gap (%)"=available_breakdowns_ethnicity,
                           "Disability pay gap (%)"=available_breakdowns_disability)

total_data<-comparison_data %>%
  filter_at(vars(names(comparison_data)[!names(comparison_data) %in% c("Year", "Value")]), all_vars(.=="Total"))


shape_set<-c(16,16,16,16,16,16,
             17,17,17,17,17,17,
             15,15,15,15,15,15,
             18,18,18,18,18,18)

create_shape_set<-function(dataset, colname){
  shapes<-head(shape_set, length(levels(as.factor(comparison_data[[colname]])))-1)
  names(shapes)<-levels(as.factor(comparison_data[[colname]]))[levels(as.factor(comparison_data[[colname]]))!="Total"]
  shapes[["Total"]]<-16
  shapeScale<-scale_shape_manual(name = colname ,values = shapes)
  return(shapeScale)
}


colour_set<-c('#cd7a00', '#339966', '#9966cc', '#8d4d57', '#A33600', '#054ce6',
              '#cd7a00', '#339966', '#9966cc', '#8d4d57', '#A33600', '#054ce6',
              '#cd7a00', '#339966', '#9966cc', '#8d4d57', '#A33600', '#054ce6',
              '#cd7a00', '#339966', '#9966cc', '#8d4d57', '#A33600', '#054ce6')

create_colour_set<-function(dataset, colname){
  colours<-head(colour_set, length(levels(as.factor(comparison_data[[colname]])))-1)
  names(colours)<-levels(as.factor(comparison_data[[colname]]))[levels(as.factor(comparison_data[[colname]]))!="Total"]
  colours[["Total"]]<-"#00006A"
  colScale<-scale_colour_manual(name = colname ,values = colours)
  return(colScale)
}
sexColours <- create_colour_set(comparison_data, "Sex")
ageColours <- create_colour_set(comparison_data, "AgeGroup")
wpColours <- create_colour_set(comparison_data, "WorkingPattern")
occColours <- create_colour_set(comparison_data, "Occupation")
wrColours <- create_colour_set(comparison_data, "WorkRegion")
indColours <- create_colour_set(comparison_data, "Industry")

sexShapes <- create_shape_set(comparison_data, "Sex")
ageShapes <- create_shape_set(comparison_data, "AgeGroup")
wpShapes <- create_shape_set(comparison_data, "WorkingPattern")
occShapes <- create_shape_set(comparison_data, "Occupation")
wrShapes <- create_shape_set(comparison_data, "WorkRegion")
indShapes <- create_shape_set(comparison_data, "Industry")
