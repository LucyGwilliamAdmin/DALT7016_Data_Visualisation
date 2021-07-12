library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(ggplot2)
library(dplyr)
library(stringr)
library(RColorBrewer)
library(ggpubr)
library(cowplot)
#remotes::install_github("coolbutuseless/ggpattern")
library(ggpattern)
library(forcats)

all_data<-read.csv("AppData/final_data.csv", fileEncoding = "UTF-8-BOM")

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
all_data$Industry <- factor(all_data$Industry, levels = c("All",
                                                          "Accommodation and food service activities",
                                                          "Activities of extraterritorial organisations and bodies",
                                                          "Activities of households as employers; undifferentiated goods-and services-producing activities of households for own use",
                                                          "Administrative and support service activities",
                                                          "Agriculture, forestry and fishing",
                                                          "Arts, entertainment and recreation",
                                                          "Construction",
                                                          "Education",
                                                          "Electricity, gas, steam and air conditioning supply",
                                                          "Financial and insurance activities",
                                                          "Human health and social work activities",
                                                          "Information and communication",
                                                          "Manufacturing",
                                                          "Mining and quarrying",
                                                          "Other service activities",
                                                          "Professional, scientific and technical activities",
                                                          "Public administration and defence; compulsory social security",
                                                          "Real estate activities",
                                                          "Transportation and storage",
                                                          "Water supply; sewerage, waste management and remediation activities",
                                                          "Wholesale and retail trade; repair of motor vehicles and motorcycles"
))
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


shape_set<-c(16,16,16,16,16,16,
             17,17,17,17,17,17,
             15,15,15,15,15,15,
             18,18,18,18,18,18)

create_shape_set<-function(dataset, colname){
  shapes<-head(shape_set, length(levels(as.factor(comparison_data[[colname]])))-1)
  names(shapes)<-levels(as.factor(comparison_data[[colname]]))[levels(as.factor(comparison_data[[colname]]))!="All"]
  shapes[["All"]]<-16
  shapeScale<-scale_shape_manual(name = colname ,values = shapes, labels = function(x) str_wrap(x, width = 27), breaks=levels(dataset[[colname]]))
  return(shapeScale)
}


create_size_set<-function(dataset, colname){
  sizes<-head(c(rep(3, times = 18), rep(4, times=6)), length(levels(as.factor(comparison_data[[colname]])))-1)
  names(sizes)<-levels(as.factor(comparison_data[[colname]]))[levels(as.factor(comparison_data[[colname]]))!="All"]
  sizes[["All"]]<-3
  sizeScale<-scale_size_manual(name = colname ,values = sizes, labels = function(x) str_wrap(x, width = 27), breaks=levels(dataset[[colname]]))
  return(sizeScale)
}


colour_set<-c('#cd7a00', '#339966', '#9966cc', '#8d4d57', '#A33600', '#054ce6',
              '#cd7a00', '#339966', '#9966cc', '#8d4d57', '#A33600', '#054ce6',
              '#cd7a00', '#339966', '#9966cc', '#8d4d57', '#A33600', '#054ce6',
              '#cd7a00', '#339966', '#9966cc', '#8d4d57', '#A33600', '#054ce6')

gap_colour_set<-c('#00006Abf', '#cd7a00bf', '#339966bf', '#9966ccbf', '#8d4d57bf', '#A33600bf', '#054ce6bf',
                  '#cd7a00bf', '#339966bf', '#9966ccbf', '#8d4d57bf', '#A33600bf', '#054ce6bf',
                  '#cd7a00bf', '#339966bf', '#9966ccbf', '#8d4d57bf', '#A33600bf', '#054ce6bf',
                  '#cd7a00bf', '#339966bf', '#9966ccbf', '#8d4d57bf', '#A33600bf', '#054ce6bf')

create_colour_set<-function(dataset, colname){
  colours<-head(colour_set, length(levels(as.factor(comparison_data[[colname]])))-1)
  names(colours)<-levels(as.factor(comparison_data[[colname]]))[levels(as.factor(comparison_data[[colname]]))!="All"]
  colours[["All"]]<-"#00006A"
  colScale<-scale_colour_manual(name = colname ,values = colours, labels = function(x) str_wrap(x, width = 27), breaks=levels(dataset[[colname]]))
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

sexSizes <- create_size_set(comparison_data, "Sex")
ageSizes <- create_size_set(comparison_data, "AgeGroup")
wpSizes <- create_size_set(comparison_data, "WorkingPattern")
occSizes <- create_size_set(comparison_data, "Occupation")
wrSizes <- create_size_set(comparison_data, "WorkRegion")
indSizes <- create_size_set(comparison_data, "Industry")

graph_titles<-c("Gender pay gap (%)"="Gender pay gap (comparison group = men)", "Ethnicity pay gap (%)"="Ethnicity pay gap (comparison group = white employees)", "Disability pay gap (%)"="Disability pay gap (comparison group = employees without a disability)")

submenu_mapping<-c("sex"="Sex", "wp"="WorkingPattern", "age"="AgeGroup", "wr"="WorkRegion", "occ"="Occupation", "ind"="Industry")

ui <- dashboardPage(
  dashboardHeader(title = "Pay Inequalities"),
  dashboardSidebar(
    sidebarMenu(id="sidebar-no-react",
                menuItem("About the dashboard", tabName = "about", icon = icon("dashboard")),
                menuItem("Pay gap data", tabName = "paygap", icon = icon("th"))),
    sidebarMenu(id="sidebar",
                menuItem("Pay comparisons", tabName = "paycomparisons", icon = icon("th"), id="paycomparisons_input",
                         menuSubItem('Sex',
                                     tabName = 'sex',
                                     icon = icon('line-chart')),
                         menuSubItem('Working pattern',
                                     tabName = 'wp',
                                     icon = icon('line-chart')),
                         menuSubItem('Age group',
                                     tabName = 'age',
                                     icon = icon('line-chart')),
                         menuSubItem('Work region',
                                     tabName = 'wr',
                                     icon = icon('line-chart')),
                         menuSubItem('Occupation',
                                     tabName = 'occ',
                                     icon = icon('line-chart')),
                         menuSubItem('Industry',
                                     tabName = 'ind',
                                     icon = icon('line-chart')))
    )
  ),
  dashboardBody(setBackgroundColor("white", shinydashboard = TRUE),
                tags$head(
                  tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
                ),
                tabItems(
                  tabItem(tabName = "about",
                          h2("About the Pay Inequalities dashboard"),
                          p("This pay inequalities dashboard provides data which allows users to explore some of the differences in pay between different groups."),
                          h3("Pay gap data"),
                          p("The 'Pay gap data' tab allows user to see the median percentage difference in earnings compared to a base group.",br(),
                            "Pay gap data is available for:"),
                          h4("Gender"),
                          p(strong("Source:")),
                          a("ONS Gender pay gap 2018 revised", href='https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/earningsandworkinghours/datasets/annualsurveyofhoursandearningsashegenderpaygaptables')%>%tagAppendAttributes(class = 'link-style'),
                          h4("Disability"),
                          p(strong("Source:")),
                          a("ONS Disability pay gap 2018 revised", href='https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/disability/datasets/rawpaygapsbydisability')%>%tagAppendAttributes(class = 'link-style'),
                          h4("Ethnicity"),
                          p(strong("Source:")),
                          a("ONS Ethnicity pay gap 2018 revised", href='https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/earningsandworkinghours/datasets/ethnicitypaygapreferencetable')%>%tagAppendAttributes(class = 'link-style'),
                          h4("Pay comparison data"),
                          p("The 'Pay comparisons' tab allows users to compare the median yearly earnings for different sub categories.",br(),
                            "Pay comparison data is available for:"),
                          h4("Sex"),
                          p(strong("Source:")),
                          h4("Working pattern"),
                          p(strong("Source:")),
                          h4("Age group"),
                          p(strong("Source:")),
                          h4("Work region"),
                          p(strong("Source:")),
                          h4("Occupation"),
                          p(strong("Source:")),
                          h4("Industry"),
                          p(strong("Source:"))
                  ),
                  tabItem(tabName = "paygap",
                          h2("Pay gap"),
                          fluidRow(
                            column(2,
                                   radioGroupButtons("gap_radio", "Inequality type",
                                                     c("Sex"="Gender pay gap (%)",
                                                       "Disability"="Disability pay gap (%)",
                                                       "Ethnicity"="Ethnicity pay gap (%)"),
                                                     direction = "vertical"),
                                   radioButtons("breakdown_radio", "Breakdown",
                                                available_breakdowns[["Gender pay gap (%)"]],
                                                selected="WorkingPattern"
                                   ) %>%
                                     tagAppendAttributes(role = 'radiogroup')
                                   #uiOutput('breakdown_radio')
                                   
                            ),
                            column(9, offset=0.5,
                                   plotOutput("plot1"),
                            )
                          ),
                          fluidRow(column(2,downloadButton("downloadData", textOutput("gap_download_text")%>%tagAppendAttributes(class = 'btn-download-text'), class = "btn-download download-gap")))
                  ),
                  tabItem(tabName = "sex",
                          h2(textOutput("sex_tab_title")),
                          fluidRow(
                            column(12,
                                   box(width=12, height=700,
                                       pickerInput("sex_dropdown", "Sex",
                                                   levels(comparison_data$Sex)[levels(comparison_data$Sex)!="All"],
                                                   options = list(
                                                     `actions-box` = TRUE,
                                                     size = 15,
                                                     `selected-text-format` = "count > 0",
                                                     `none-selected-text`="No items selected"),
                                                   multiple = TRUE,
                                                   choicesOpt = list(
                                                     icon = c(rep("glyphicon-none", times=length(levels(comparison_data$Sex)[levels(comparison_data$Sex)!="All"])))
                                                   )),
                                       plotOutput("plot2"),
                                       downloadButton("downloadDataSex", textOutput("sex_download_text")%>%tagAppendAttributes(class = 'btn-download-text'), class = "btn-download"))))),
                  tabItem(tabName = "wp",
                          h2(textOutput("wp_tab_title")),
                          fluidRow(
                            column(12,
                                   box(width=12, height=700,
                                       pickerInput("wp_dropdown", "Working pattern",
                                                   levels(comparison_data$WorkingPattern)[levels(comparison_data$WorkingPattern)!="All"],
                                                   options = list(
                                                     `actions-box` = TRUE,
                                                     size = 15,
                                                     `selected-text-format` = "count>0",
                                                     `none-selected-text`="No items selected"),
                                                   multiple = TRUE,
                                                   choicesOpt = list(
                                                     icon = c(rep("glyphicon-none", times=length(levels(comparison_data$WorkingPattern)[levels(comparison_data$WorkingPattern)!="All"])))
                                                   )),
                                       
                                       plotOutput("plot3"),
                                       downloadButton("downloadDataWP", textOutput("wp_download_text")%>%tagAppendAttributes(class = 'btn-download-text'), class = "btn-download"))))),
                  tabItem(tabName = "age",
                          h2(textOutput("age_tab_title")),
                          fluidRow(
                            column(12,
                                   box(width=12, height=700,
                                       pickerInput("age_dropdown", "Age group",
                                                   levels(comparison_data$AgeGroup)[levels(comparison_data$AgeGroup)!="All"],
                                                   options = list(
                                                     `actions-box` = TRUE,
                                                     size = 15,
                                                     `selected-text-format` = "count>0",
                                                     `none-selected-text`="No items selected"),
                                                   multiple = TRUE,
                                                   choicesOpt = list(
                                                     icon = c(rep("glyphicon-none", times=length(levels(comparison_data$AgeGroup)[levels(comparison_data$AgeGroup)!="All"])))
                                                   )
                                       ),
                                       plotOutput("plot4"),
                                       downloadButton("downloadDataAge", textOutput("age_download_text")%>%tagAppendAttributes(class = 'btn-download-text'), class = "btn-download"))))),
                  tabItem(tabName = "wr",
                          h2(textOutput("wr_tab_title")),
                          fluidRow(
                            column(12,
                                   box(width=12, height=700,
                                       pickerInput("wr_dropdown", "Work region",
                                                   levels(comparison_data$WorkRegion)[levels(comparison_data$WorkRegion)!="All"],
                                                   options = list(
                                                     `actions-box` = TRUE,
                                                     size = 15,
                                                     `selected-text-format` = "count>0",
                                                     `none-selected-text`="No items selected"),
                                                   multiple = TRUE,
                                                   choicesOpt = list(
                                                     icon = c(rep("glyphicon-none", times=length(levels(comparison_data$WorkRegion)[levels(comparison_data$WorkRegion)!="All"])))
                                                   )
                                       ),
                                       plotOutput("plot5"),
                                       downloadButton("downloadDataWR", textOutput("wr_download_text")%>%tagAppendAttributes(class = 'btn-download-text'), class = "btn-download"))))),
                  tabItem(tabName = "occ",
                          h2(textOutput("occ_tab_title")),
                          fluidRow(
                            column(12,
                                   box(width=12, height=700,
                                       pickerInput("occ_dropdown", "Occupation",
                                                   levels(comparison_data$Occupation)[levels(comparison_data$Occupation)!="All"],
                                                   options = list(
                                                     `actions-box` = TRUE,
                                                     size = 15,
                                                     `selected-text-format` = "count>0",
                                                     `none-selected-text`="No items selected"),
                                                   multiple = TRUE,
                                                   choicesOpt = list(
                                                     icon = c(rep("glyphicon-none", times=length(levels(comparison_data$Occupation)[levels(comparison_data$Occupation)!="All"])))
                                                   )
                                       ),
                                       plotOutput("plot6"),
                                       downloadButton("downloadDataOcc", textOutput("occ_download_text")%>%tagAppendAttributes(class = 'btn-download-text'), class = "btn-download"))))),
                  tabItem(tabName = "ind",
                          h2(textOutput("ind_tab_title")),
                          fluidRow(
                            column(12,
                                   box(width=12, height=700,
                                       pickerInput("ind_dropdown", "Industry",
                                                   levels(comparison_data$Industry)[levels(comparison_data$Industry)!="All"],
                                                   options = list(
                                                     `actions-box` = TRUE,
                                                     size = 15,
                                                     `selected-text-format` = "count>0",
                                                     `none-selected-text`="No items selected"),
                                                   multiple = TRUE,
                                                   choicesOpt = list(
                                                     icon = c(rep("glyphicon-none", times=length(levels(comparison_data$Industry)[levels(comparison_data$Industry)!="All"])))
                                                   )
                                       ),
                                       plotOutput("plot7"),
                                       downloadButton("downloadDataInd", textOutput("ind_download_text")%>%tagAppendAttributes(class = 'btn-download-text'), class = "btn-download"))))
                          
                  )
                )
  )
)




server <- function(input, output, session) {
  
  observe({
    x <- input$gap_radio
    
    # Can also set the label and select items
    updateRadioButtons(session, "breakdown_radio",
                       choices=available_breakdowns[[x]],
                       selected = available_breakdowns[[x]][1]
    )
  })
  
  
  dat <- reactive({
    keep<-c("Year", "Units", "Value", "comment")
    if (input$gap_radio=="Ethnicity pay gap (%)"){
      data<-paygap_data %>%
        filter(Units==input$gap_radio) %>%
        select(!!unlist(keep), input$breakdown_radio)
    }
    else {
      data<-paygap_data %>%
        filter(Units==input$gap_radio) %>%
        filter_at(vars(names(paygap_data)[!names(paygap_data) %in% append(keep, input$breakdown_radio)]), all_vars(.=="All"))%>%
        select(!!unlist(keep), input$breakdown_radio)
    }
  })
  
  fill<-reactive({
    fill<-input$breakdown_radio
  })
  
  gap_data_download<-reactive({
    data<-paygap_data %>%
      filter(Units==input$gap_radio) %>%
      select(Year,where(~length(unique(.))>1),comment)
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$gap_radio, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(gap_data_download(), file, row.names = FALSE)
    }
  )
  
  comparison_filename<-reactive({
    paste("PayComparison_", input$sidebar, ".csv", sep="")
  })
  
  comparison_content<-reactive({
    paste("comparison_data_download()", sep="")
  })
  
  comparison_min<-reactive({
    abs(round(min(comparison_data_download()$Value), -3)-5000)
  })
  
  comparison_max<-reactive({
    round(max(comparison_data_download()$Value), -3)+2000
  })
  
  
  output$downloadDataSex <- downloadHandler(
    filename = function() {comparison_filename()},
    content = function(file) {write.csv(eval(parse(text = comparison_content())), file, row.names = FALSE)}
  )
  
  output$downloadDataWP <- downloadHandler(
    filename = function() {comparison_filename()},
    content = function(file) {write.csv(eval(parse(text = comparison_content())), file, row.names = FALSE)}
  )
  
  output$downloadDataAge <- downloadHandler(
    filename = function() {comparison_filename()},
    content = function(file) {write.csv(eval(parse(text = comparison_content())), file, row.names = FALSE)}
  )  
  
  output$downloadDataWR <- downloadHandler(
    filename = function() {comparison_filename()},
    content = function(file) {write.csv(eval(parse(text = comparison_content())), file, row.names = FALSE)}
  )
  
  output$downloadDataOcc <- downloadHandler(
    filename = function() {comparison_filename()},
    content = function(file) {write.csv(eval(parse(text = comparison_content())), file, row.names = FALSE)}
  )
  
  output$downloadDataInd <- downloadHandler(
    filename = function() {comparison_filename()},
    content = function(file) {write.csv(eval(parse(text = comparison_content())), file, row.names = FALSE)}
  )
  output$gap_download_text <- renderText({ 
    paste("Download data for\n", input$gap_radio, "\nas CSV")
  })
  
  
  comparison_download_text<-reactive({
    paste("Download data for \n", "Pay comparison:", str_to_lower(gsub("([a-z])([A-Z])", "\\1 \\2", submenu_mapping[input$sidebar])), "\nas CSV")
  })
  
  comparison_tab_title<-reactive({
    paste("Pay comparison:", str_to_lower(gsub("([a-z])([A-Z])", "\\1 \\2", submenu_mapping[input$sidebar])))
  })
  
  output$sex_download_text <- renderText({ comparison_download_text() })
  output$wp_download_text <- renderText({ comparison_download_text() })
  output$age_download_text <- renderText({ comparison_download_text() })
  output$wr_download_text <- renderText({ comparison_download_text() })
  output$occ_download_text <- renderText({ comparison_download_text() })
  output$ind_download_text <- renderText({ comparison_download_text() })
  
  
  output$sex_tab_title <- renderText({ comparison_tab_title() })
  output$wp_tab_title <- renderText({ comparison_tab_title() })
  output$age_tab_title <- renderText({ comparison_tab_title() })
  output$wr_tab_title <- renderText({ comparison_tab_title() })
  output$occ_tab_title <- renderText({ comparison_tab_title() })
  output$ind_tab_title <- renderText({ comparison_tab_title() })
  
  
  
  
  observeEvent(input$gap_radio,
               output$plot1<-renderPlot({
                 ggplot(dat(),aes_string(x=input$breakdown_radio, y="Value"))+
                   geom_bar(position=position_dodge(width=0.8, preserve = "single"), stat = 'identity', width=0.5, fill='#00006Abf')+
                   coord_flip()+
                   theme_classic()+
                   geom_hline(yintercept=0)+
                   labs(title=graph_titles[input$gap_radio], y="Percentage (%)", x=gsub("([a-z])([A-Z])", "\\1 \\2", input$breakdown_radio))+
                   scale_x_discrete(labels = function(x) str_wrap(x, width = 25), limits=rev)+
                   scale_y_continuous(breaks = seq(-10, 50, by = 5))+
                   theme(text = element_text(size = 20), axis.line = element_blank(), axis.ticks=element_blank(),
                         legend.position="none", panel.grid.major.x=element_line(color = "#BEBEBE"),
                         axis.title.y = element_blank(), axis.title.x = element_text(hjust=1))},
                 height = 700)
               
  )
  
  submenu_item<-reactive({
    submenu_mapping[input$sidebar]
  })
  
  # submenu_item_var<-reactive({
  #   eval(parse(text = submenu_item()))
  # })
  
  
  dat_comparison <- reactive ({
    req(input$sidebar, cancelOutput = TRUE)
    keep<-c("Year", "Units", "Value", toString(submenu_item()), "comment")
    keep<-keep[keep != ""]
    
    data<-comparison_data %>%
      filter_at(vars(names(comparison_data)[!names(comparison_data) %in% keep]), all_vars(.=="All"))%>%
      select(!!unlist(keep)) %>%
      filter(eval(parse(text = submenu_item())) %in% c("All", eval(parse(text = paste("input$",input$sidebar,"_dropdown", sep="")))))
    
  })
  
  
  
  comparison_data_download<-reactive({
    data<-comparison_data %>%
      filter(eval(parse(text = submenu_mapping[input$sidebar])) %in% unique(comparison_data[[submenu_mapping[input$sidebar]]])) %>%
      filter_at(vars(names(comparison_data)[!names(comparison_data) %in% c("Year", "Units", "Value", submenu_mapping[input$sidebar], "comment")]), all_vars(.=="All")) %>%
      select(Year,where(~length(unique(.))>1),comment)
  })
  
  
  graph_base<-reactive({
    req(input$sidebar, cancelOutput = TRUE)
    plot<-ggplot(total_data)+
      geom_line(aes(x=Year, y=Value, group=eval(parse(text = submenu_mapping[input$sidebar])), colour=eval(parse(text = submenu_mapping[input$sidebar]))))+
      geom_point(aes(x=Year, y=Value, group=eval(parse(text = submenu_mapping[input$sidebar])), colour=eval(parse(text = submenu_mapping[input$sidebar])), shape=eval(parse(text = submenu_mapping[input$sidebar])), size=eval(parse(text = submenu_mapping[input$sidebar]))))+
      theme_classic()+
      eval(parse(text = paste(input$sidebar,"Colours", sep="")))+
      eval(parse(text = paste(input$sidebar,"Shapes", sep="")))+
      eval(parse(text = paste(input$sidebar,"Sizes", sep="")))+
      #scale_y_continuous(limits = c(22500,27000), expand = c(0,0), 
      #                  breaks = c(22500, 23000, 24000, 25000, 26000, 27000), labels = c(0, 23000, 24000, 25000, 26000, 27000))+
      labs(title=paste("Pay comparison by", str_to_lower(gsub("([a-z])([A-Z])", "\\1 \\2", submenu_mapping[input$sidebar]))), y="Median")+
      theme(legend.title = element_blank(), text = element_text(size = 20), axis.line.y = element_blank(), axis.line.x = element_line(color="#BEBEBE"),
            legend.text = element_text(size = 11), legend.key.size = unit(2,"line"), axis.title.y = element_text(angle = 0),
            panel.grid.major.y=element_line(color = "#BEBEBE"))+
      annotate(geom = "segment", x = 2015.9, xend = 2016, y =  comparison_min()+600, yend = comparison_min()+450, color = "#BEBEBE")+
      annotate(geom = "segment", x = 2015.9, xend = 2016, y =  comparison_min()+300, yend = comparison_min()+450, color = "#BEBEBE")+
      annotate(geom = "segment", x = 2015.9, xend = 2015.9, y =  -Inf, yend = comparison_min()+300, color = "#BEBEBE")+
      annotate(geom = "segment", x = 2015.9, xend = 2015.9, y =  comparison_min()+600, yend = Inf, color = "#BEBEBE")+
      scale_x_continuous(expand = c(0, 0), limits = c(2015.9,2020.2))+
      scale_y_continuous(expand = c(0, 0), limits = c(comparison_min(),comparison_max()))
    legend<-get_legend(plot)
    plot<-plot+theme(legend.position = "none")
    ggdraw(plot_grid(plot_grid(plot, ncol=1, align='v'),
                     plot_grid(legend, ncol=2),
                     rel_widths=c(1, 0.5)))
  })
  
  
  
  changed_graph_base<-reactive({
    req(input$sidebar, cancelOutput = TRUE)
    plot<-ggplot(dat_comparison())+
      geom_line(aes(x=Year, y=Value, group=eval(parse(text = submenu_mapping[input$sidebar])), colour=eval(parse(text = submenu_mapping[input$sidebar]))))+
      geom_point(aes(x=Year, y=Value, group=eval(parse(text = submenu_mapping[input$sidebar])), colour=eval(parse(text = submenu_mapping[input$sidebar])), shape=eval(parse(text = submenu_mapping[input$sidebar])), size=eval(parse(text = submenu_mapping[input$sidebar]))))+
      theme_classic()+
      eval(parse(text = paste(input$sidebar,"Colours", sep="")))+
      eval(parse(text = paste(input$sidebar,"Shapes", sep="")))+
      eval(parse(text = paste(input$sidebar,"Sizes", sep="")))+
      labs(title=paste("Pay comparison by", str_to_lower(gsub("([a-z])([A-Z])", "\\1 \\2", submenu_mapping[input$sidebar]))), y="Median")+
      theme(legend.title = element_blank(), text = element_text(size = 20), axis.line.y = element_blank(), axis.line.x = element_line(color="#BEBEBE"),
            legend.text = element_text(size = 11), legend.key.size = unit(2,"line"), axis.title.y = element_text(angle = 0),
            panel.grid.major.y=element_line(color = "#BEBEBE"))+
      annotate(geom = "segment", x = 2015.9, xend = 2016, y =  comparison_min()+600, yend = comparison_min()+450, color = "#BEBEBE")+
      annotate(geom = "segment", x = 2015.9, xend = 2016, y =  comparison_min()+300, yend = comparison_min()+450, color = "#BEBEBE")+
      annotate(geom = "segment", x = 2015.9, xend = 2015.9, y =  -Inf, yend = comparison_min()+300, color = "#BEBEBE")+
      annotate(geom = "segment", x = 2015.9, xend = 2015.9, y =  comparison_min()+600, yend = Inf, color = "#BEBEBE")+
      scale_x_continuous(expand = c(0, 0), limits = c(2015.9,2020.2))+
      scale_y_continuous(expand = c(0, 0), limits = c(comparison_min(),comparison_max()))
    legend<-get_legend(plot)
    plot<-plot+theme(legend.position = "none")
    ggdraw(plot_grid(plot_grid(plot, ncol=1, align='v'),
                     plot_grid(legend, ncol=2),
                     rel_widths=c(1, 0.5)))
  })
  
  
  
  output$plot2<-renderPlot({
    graph_base()
  }, height=500)
  
  observeEvent(input$sex_dropdown,
               output$plot2<-renderPlot({
                 changed_graph_base()}, height=500))
  
  output$plot3<-renderPlot({
    graph_base()
  }, height=500)
  
  observeEvent(input$wp_dropdown,
               output$plot3<-renderPlot({
                 changed_graph_base()}, height=500))
  
  output$plot4<-renderPlot({
    graph_base()
  }, height=500)
  
  observeEvent(input$age_dropdown,
               output$plot4<-renderPlot({
                 changed_graph_base()}, height=500))
  
  output$plot5<-renderPlot({
    graph_base()
  }, height=500)
  
  observeEvent(input$wr_dropdown,
               output$plot5<-renderPlot({
                 changed_graph_base()}, height=500))
  
  output$plot6<-renderPlot({
    graph_base()
  }, height=500)
  
  observeEvent(input$occ_dropdown,
               output$plot6<-renderPlot({
                 changed_graph_base()}, height=500))
  
  
  
  output$plot7<-renderPlot({
    plot<-ggplot(total_data)+
      geom_line(aes(x=Year, y=Value, group=eval(parse(text = submenu_mapping[input$sidebar])), colour=eval(parse(text = submenu_mapping[input$sidebar]))))+
      geom_point(aes(x=Year, y=Value, group=eval(parse(text = submenu_mapping[input$sidebar])), colour=eval(parse(text = submenu_mapping[input$sidebar])), shape=eval(parse(text = submenu_mapping[input$sidebar])), size=eval(parse(text = submenu_mapping[input$sidebar]))))+
      theme_classic()+
      eval(parse(text = paste(input$sidebar,"Colours", sep="")))+
      eval(parse(text = paste(input$sidebar,"Shapes", sep="")))+
      eval(parse(text = paste(input$sidebar,"Sizes", sep="")))+
      labs(title=paste("Pay comparison by", str_to_lower(gsub("([a-z])([A-Z])", "\\1 \\2", submenu_mapping[input$sidebar]))), y="Median")+
      theme(legend.title = element_blank(), text = element_text(size = 20), axis.line.y = element_blank(), axis.line.x = element_line(color="#BEBEBE"),
            legend.text = element_text(size = 11), legend.key.size = unit(2,"line"), axis.title.y = element_text(angle = 0),
            panel.grid.major.y=element_line(color = "#BEBEBE"))+
      annotate(geom = "segment", x = 2015.9, xend = 2016, y =  comparison_min()+600, yend = comparison_min()+450, color = "#BEBEBE")+
      annotate(geom = "segment", x = 2015.9, xend = 2016, y =  comparison_min()+300, yend = comparison_min()+450, color = "#BEBEBE")+
      annotate(geom = "segment", x = 2015.9, xend = 2015.9, y =  -Inf, yend = comparison_min()+300, color = "#BEBEBE")+
      annotate(geom = "segment", x = 2015.9, xend = 2015.9, y =  comparison_min()+600, yend = Inf, color = "#BEBEBE")+
      scale_x_continuous(expand = c(0, 0), limits = c(2015.9,2020.2))+
      scale_y_continuous(expand = c(0, 0), limits = c(comparison_min(),comparison_max()))+
      guides(col=guide_legend(ncol=2,byrow=TRUE))
    legend<-get_legend(plot)
    plot<-plot+theme(legend.position = "none")
    ggdraw(plot_grid(plot_grid(plot, ncol=1, align='v'),
                     plot_grid(legend, ncol=1),
                     rel_widths=c(1, 0.5)))
  }, height=500)
  
  observeEvent(input$ind_dropdown,
               output$plot7<-renderPlot({
                 plot<-ggplot(dat_comparison())+
                   geom_line(aes(x=Year, y=Value, group=eval(parse(text = submenu_mapping[input$sidebar])), colour=eval(parse(text = submenu_mapping[input$sidebar]))))+
                   geom_point(aes(x=Year, y=Value, group=eval(parse(text = submenu_mapping[input$sidebar])), colour=eval(parse(text = submenu_mapping[input$sidebar])), shape=eval(parse(text = submenu_mapping[input$sidebar])), size=eval(parse(text = submenu_mapping[input$sidebar]))))+
                   theme_classic()+
                   eval(parse(text = paste(input$sidebar,"Colours", sep="")))+
                   eval(parse(text = paste(input$sidebar,"Shapes", sep="")))+
                   eval(parse(text = paste(input$sidebar,"Sizes", sep="")))+
                   labs(title=paste("Pay comparison by", str_to_lower(gsub("([a-z])([A-Z])", "\\1 \\2", submenu_mapping[input$sidebar]))), y="Median")+
                   theme(legend.title = element_blank(), text = element_text(size = 20), axis.line.y = element_blank(), axis.line.x = element_line(color="#BEBEBE"),
                         legend.text = element_text(size = 11), legend.key.size = unit(2,"line"), axis.title.y = element_text(angle = 0),
                         panel.grid.major.y=element_line(color = "#BEBEBE"))+
                   annotate(geom = "segment", x = 2015.9, xend = 2016, y =  comparison_min()+600, yend = comparison_min()+450, color = "#BEBEBE")+
                   annotate(geom = "segment", x = 2015.9, xend = 2016, y =  comparison_min()+300, yend = comparison_min()+450, color = "#BEBEBE")+
                   annotate(geom = "segment", x = 2015.9, xend = 2015.9, y =  -Inf, yend = comparison_min()+300, color = "#BEBEBE")+
                   annotate(geom = "segment", x = 2015.9, xend = 2015.9, y =  comparison_min()+600, yend = Inf, color = "#BEBEBE")+
                   scale_x_continuous(expand = c(0, 0), limits = c(2015.9,2020.2))+
                   scale_y_continuous(expand = c(0, 0), limits = c(comparison_min(),comparison_max()))+
                   guides(col=guide_legend(ncol=2,byrow=TRUE))
                 legend<-get_legend(plot)
                 plot<-plot+theme(legend.position = "none")
                 ggdraw(plot_grid(plot_grid(plot, ncol=1, align='v'),
                                  plot_grid(legend, ncol=1),
                                  rel_widths=c(1, 0.5)))
               }, height=500)
               
  )
  
  
}


# Run the application 
shinyApp(ui = ui, server = server)
