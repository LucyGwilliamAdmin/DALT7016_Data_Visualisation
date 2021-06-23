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

all_data<-read.csv("AppData/final_data.csv", fileEncoding = "UTF-8-BOM")

paygap_data<-all_data %>%
  filter(Units %in% c("Disability pay gap (%)", "Ethnicity pay gap (%)", "Gender pay gap (%)"))

comparison_data<-all_data %>%
  filter(Units=="Median")


available_breakdowns_gender<-list("Working pattern"="WorkingPattern", "Age group"="AgeGroup",
                                  "Work region"="WorkRegion", "Occupation"="Occupation")

available_breakdowns_ethnicity<-list("Ethnicity"="Ethnicity")

available_breakdowns_disability<-list("Working pattern"="WorkingPattern", "Sex"="Sex","Age group"="AgeGroup",
                                      "Work region"="WorkRegion", "Occupation"="Occupation", "Ethnicity"="Ethnicity", "Impairment")



available_breakdowns<-list("Gender pay gap (%)"=available_breakdowns_gender,
                           "Ethnicity pay gap (%)"=available_breakdowns_ethnicity,
                           "Disability pay gap (%)"=available_breakdowns_disability)

total_data<-comparison_data %>%
  filter_at(vars(names(comparison_data)[!names(comparison_data) %in% c("Year", "Value", "Units", "comment")]), all_vars(.=="Total"))


shape_set<-c(16,16,16,16,16,16,
             17,17,17,17,17,17,
             15,15,15,15,15,15,
             18,18,18,18,18,18)

create_shape_set<-function(dataset, colname){
  shapes<-head(shape_set, length(levels(as.factor(comparison_data[[colname]])))-1)
  names(shapes)<-levels(as.factor(comparison_data[[colname]]))[levels(as.factor(comparison_data[[colname]]))!="Total"]
  shapes[["Total"]]<-16
  shapeScale<-scale_shape_manual(name = colname ,values = shapes, labels = function(x) str_wrap(x, width = 27))
  return(shapeScale)
}


create_size_set<-function(dataset, colname){
  sizes<-head(c(rep(3, times = 18), rep(4, times=6)), length(levels(as.factor(comparison_data[[colname]])))-1)
  names(sizes)<-levels(as.factor(comparison_data[[colname]]))[levels(as.factor(comparison_data[[colname]]))!="Total"]
  sizes[["Total"]]<-3
  sizeScale<-scale_size_manual(name = colname ,values = sizes, labels = function(x) str_wrap(x, width = 27))
  return(sizeScale)
}


colour_set<-c('#cd7a00', '#339966', '#9966cc', '#8d4d57', '#A33600', '#054ce6',
              '#cd7a00', '#339966', '#9966cc', '#8d4d57', '#A33600', '#054ce6',
              '#cd7a00', '#339966', '#9966cc', '#8d4d57', '#A33600', '#054ce6',
              '#cd7a00', '#339966', '#9966cc', '#8d4d57', '#A33600', '#054ce6')

create_colour_set<-function(dataset, colname){
  colours<-head(colour_set, length(levels(as.factor(comparison_data[[colname]])))-1)
  names(colours)<-levels(as.factor(comparison_data[[colname]]))[levels(as.factor(comparison_data[[colname]]))!="Total"]
  colours[["Total"]]<-"#00006A"
  colScale<-scale_colour_manual(name = colname ,values = colours, labels = function(x) str_wrap(x, width = 27))
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


ui <- dashboardPage(
  dashboardHeader(title = "Pay Inequalities"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("About the dashboard", tabName = "about", icon = icon("dashboard")),
      menuItem("Pay gap data", tabName = "paygap", icon = icon("th")),
      menuItem("Pay comparisons", tabName = "paycomparisons", icon = icon("th"),
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
  tags$style(type='text/css', "p, a {font-size: 18px;} h4 {font-size: 20px;}"),
                tabItems(
                  tabItem(tabName = "about",
                          h2("About the Pay Inequalities dashboard"),
                          p("This pay inequalities dashboard provides data which allows users to explore some of the differences in pay between different groups."),
                          h3("Pay gap data"),
                          p("The 'Pay gap data' tab allows user to see the median percentage difference in earnings compared to a base group.",br(),
                            "Pay gap data is available for:"),
                          h4("Gender"),
                          p(strong("Source:")),
                          a("ONS Gender pay gap 2018 revised", href='https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/earningsandworkinghours/datasets/annualsurveyofhoursandearningsashegenderpaygaptables'),
                          h4("Disability"),
                          p(strong("Source:")),
                          a("ONS Disability pay gap 2018 revised", href='https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/disability/datasets/rawpaygapsbydisability'),
                          h4("Ethnicity"),
                          p(strong("Source:")),
                          a("ONS Ethnicity pay gap 2018 revised", href='https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/earningsandworkinghours/datasets/ethnicitypaygapreferencetable'),
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
                                                     status="primary"),
                                   radioButtons("breakdown_radio", "Breakdown",
                                                available_breakdowns[["Gender pay gap (%)"]],
                                                selected="WorkingPattern"
                                   )
                                   #uiOutput('breakdown_radio')
                                   
                            ),
                            column(9, offset=0.5,
                                   plotOutput("plot1")
                            )
                          )
                  ),
                  tabItem(tabName = "sex",
                          h2("Pay comparisons: Sex"),
                          fluidRow(
                            column(12,
                                   box(width=12, height=700,
                                       pickerInput("sex_dropdown", "Sex",
                                                   unique(comparison_data$Sex)[unique(comparison_data$Sex)!="Total"],
                                                   options = list(
                                                     `actions-box` = TRUE,
                                                     size = 15,
                                                     `selected-text-format` = "count>0",
                                                     `none-selected-text`="No items selected"),
                                                   multiple = TRUE
                                                   ),
                                       plotOutput("plot2"))))),
                  tabItem(tabName = "wp",
                          h2("Pay comparisons: Working pattern"),
                          fluidRow(
                            column(12,
                                   box(width=12, height=700,
                                       pickerInput("wp_dropdown", "Working pattern",
                                                            unique(comparison_data$WorkingPattern)[unique(comparison_data$WorkingPattern)!="Total"],
                                                            options = list(
                                                              `actions-box` = TRUE,
                                                              size = 15,
                                                              `selected-text-format` = "count>0",
                                                              `none-selected-text`="No items selected"),
                                                            multiple = TRUE
                                   ),
                                   plotOutput("plot3"))))),
                  tabItem(tabName = "age",
                          h2("Pay comparisons: Age group"),
                          fluidRow(
                            column(12,
                                   box(width=12, height=700,
                                       pickerInput("age_dropdown", "Age group",
                                                            unique(comparison_data$AgeGroup)[unique(comparison_data$AgeGroup)!="Total"],
                                                            options = list(
                                                              `actions-box` = TRUE,
                                                              size = 15,
                                                              `selected-text-format` = "count>0",
                                                              `none-selected-text`="No items selected"),
                                                            multiple = TRUE
                                   ),
                                   plotOutput("plot4"))))),
                  tabItem(tabName = "wr",
                          h2("Pay comparisons: Work region"),
                          fluidRow(
                            column(12,
                                   box(width=12, height=700,
                                       pickerInput("wr_dropdown", "Work region",
                                                            unique(comparison_data$WorkRegion)[unique(comparison_data$WorkRegion)!="Total"],
                                                            options = list(
                                                              `actions-box` = TRUE,
                                                              size = 15,
                                                              `selected-text-format` = "count>0",
                                                              `none-selected-text`="No items selected"),
                                                            multiple = TRUE
                                   ),
                                   plotOutput("plot5"))))),
                  tabItem(tabName = "occ",
                          h2("Pay comparisons: Occupation"),
                          fluidRow(
                            column(12,
                                   box(width=12, height=700,
                                       pickerInput("occ_dropdown", "Occupation",
                                                            unique(comparison_data$Occupation)[unique(comparison_data$Occupation)!="Total"],
                                                            options = list(
                                                              `actions-box` = TRUE,
                                                              size = 15,
                                                              `selected-text-format` = "count>0",
                                                              `none-selected-text`="No items selected"),
                                                            multiple = TRUE
                                   ),
                                   plotOutput("plot6"))))),
                  tabItem(tabName = "ind",
                          h2("Pay comparisons: Industry"),
                          fluidRow(
                            column(12,
                                   box(width=12, height=700,
                                       pickerInput("ind_dropdown", "Industry",
                                                            unique(comparison_data$Industry)[unique(comparison_data$Industry)!="Total"],
                                                            options = list(
                                                              `actions-box` = TRUE,
                                                              size = 15,
                                                              `selected-text-format` = "count>0",
                                                              `none-selected-text`="No items selected"),
                                                            multiple = TRUE
                                   ),
                                   plotOutput("plot7"))))
                          
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
  
  #observe({
  # output$breakdown_radio <- renderUI({
  #  options <- available_breakdowns[[input$gap_radio]]
  # The options are dynamically generated on the server
  # radioButtons("breakdown_radio", 'Breakdown', options)
  #})
  # })
  
  
  dat <- reactive({
    if (input$gap_radio=="Gender pay gap (%)") {
      keep<-c("Year", "Units", "Value", "WorkingPattern")
    }
    else {
      keep<-c("Year", "Units", "Value")
    }
    if (input$gap_radio=="Ethnicity pay gap (%)"){
      data<-paygap_data %>%
        filter(Units==input$gap_radio) %>%
        select(!!unlist(keep), input$breakdown_radio)
    }
    else {
      data<-paygap_data %>%
        filter(Units==input$gap_radio) %>%
        filter_at(vars(names(paygap_data)[!names(paygap_data) %in% append(keep, input$breakdown_radio)]), all_vars(.=="Total"))%>%
        select(!!unlist(keep), input$breakdown_radio)
    }
  })
  
  fill<-reactive({
    if (input$gap_radio=="Gender pay gap (%)") {
      fill<-"WorkingPattern"
    }
    else {
      fill<-input$breakdown_radio
    }
  })
  
  
  observeEvent(input$gap_radio,
               output$plot1<-renderPlot({
                 ggplot(dat(),aes_string(x=input$breakdown_radio, y="Value", pattern=fill(), fill=fill()))+
                   #geom_bar(position=position_dodge(width=0.8, preserve = "single"), stat = 'identity', width=0.5)+
                   geom_bar_pattern(position = position_dodge(preserve = "single", width=0.8),
                                   stat="identity",
                                   width=0.5,
                                   #color = "black", 
                                   pattern_fill = "white",
                                   pattern_angle = 45,
                                   pattern_density = 0.3,
                                   pattern_spacing = 0.025,
                                   pattern_key_scale_factor = 0.6)+
                   scale_pattern_manual(values = c("none", "none", "none", "none", "none", "none", "stripe", "stripe", "stripe", "stripe", "stripe", "stripe", "crosshatch", "crosshatch")) +
                   coord_flip()+
                   theme_bw()+
                   scale_fill_manual(values=colour_set)+
                   labs(title=input$gap_radio, y="Percentage", x=input$breakdown_radio)+
                   scale_x_discrete(labels = function(x) str_wrap(x, width = 25))+
                   guides(pattern = guide_legend(reverse = TRUE),
                          fill = guide_legend(reverse = TRUE))+
                   theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 20),
                         legend.key.size = unit(1, "cm"),
                         axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))},
                 height = 700)
               
  )
  
  dat_sex <- reactive({
    keep<-c("Year", "Units", "Value", "Sex", "comment")
    keep<-keep[keep != ""]
    
    data<-comparison_data %>%
      filter_at(vars(names(comparison_data)[!names(comparison_data) %in% keep]), all_vars(.=="Total"))%>%
      select(!!unlist(keep)) %>%
      filter(Sex %in% c("Total", input$sex_dropdown))
    
  })
  
  dat_wp <- reactive({
    keep<-c("Year", "Units", "Value", "WorkingPattern", "comment")
    keep<-keep[keep != ""]
    
    data<-comparison_data %>%
      filter_at(vars(names(comparison_data)[!names(comparison_data) %in% keep]), all_vars(.=="Total"))%>%
      select(!!unlist(keep)) %>%
      filter(WorkingPattern %in% c("Total", input$wp_dropdown))
    
  })
  
  dat_age <- reactive({
    keep<-c("Year", "Units", "Value", "AgeGroup", "comment")
    keep<-keep[keep != ""]
    
    data<-comparison_data %>%
      filter_at(vars(names(comparison_data)[!names(comparison_data) %in% keep]), all_vars(.=="Total"))%>%
      select(!!unlist(keep)) %>%
      filter(AgeGroup %in% c("Total", input$age_dropdown))
    
  })
  
  dat_wr <- reactive({
    keep<-c("Year", "Units", "Value", "WorkRegion", "comment")
    keep<-keep[keep != ""]
    
    data<-comparison_data %>%
      filter_at(vars(names(comparison_data)[!names(comparison_data) %in% keep]), all_vars(.=="Total"))%>%
      select(!!unlist(keep)) %>%
      filter(WorkRegion %in% c("Total", input$wr_dropdown))
    
  })
  
  dat_occ <- reactive({
    keep<-c("Year", "Units", "Value", "Occupation", "comment")
    keep<-keep[keep != ""]
    
    data<-comparison_data %>%
      filter_at(vars(names(comparison_data)[!names(comparison_data) %in% keep]), all_vars(.=="Total"))%>%
      select(!!unlist(keep)) %>%
      filter(Occupation %in% c("Total", input$occ_dropdown))
    
  })
  
  dat_ind <- reactive({
    keep<-c("Year", "Units", "Value", "Industry", "comment")
    keep<-keep[keep != ""]
    
    data<-comparison_data %>%
      filter_at(vars(names(comparison_data)[!names(comparison_data) %in% keep]), all_vars(.=="Total"))%>%
      select(!!unlist(keep)) %>%
      filter(Industry %in% c("Total", input$ind_dropdown))
    
  })
  
  output$plot2<-renderPlot({
    plot<-ggplot(total_data)+
      geom_line(aes(x=Year, y=Value, group=Sex, colour=Sex))+
      geom_point(aes(x=Year, y=Value, group=Sex, colour=Sex, shape=Sex, size=Sex))+
      theme_bw()+
      sexColours+
      sexShapes+
      sexSizes+
      labs(title="Pay Comparison by Sex", y="Median")+
      theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(),
            legend.text = element_text(size = 11), legend.key.size = unit(2,"line"))
    legend<-get_legend(plot)
    plot<-plot+theme(legend.position = "none")
    ggdraw(plot_grid(plot_grid(plot, ncol=1, align='v'),
                     plot_grid(legend, ncol=2),
                     rel_widths=c(1, 0.5)))
    }, height=500)
  
  observeEvent(input$sex_dropdown,
               output$plot2<-renderPlot({
                 plot<-ggplot(dat_sex())+
                   geom_line(aes(x=Year, y=Value, group=Sex, colour=Sex))+
                   geom_point(aes(x=Year, y=Value, group=Sex, colour=Sex, shape=Sex, size=Sex))+
                   theme_bw()+
                   sexColours+
                   sexShapes+
                   sexSizes+
                   labs(title="Pay Comparison by Sex", y="Median")+
                   theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(),
                         legend.text = element_text(size = 11), legend.key.size = unit(2,"line"))
                 legend<-get_legend(plot)
                 plot<-plot+theme(legend.position = "none")
                 ggdraw(plot_grid(plot_grid(plot, ncol=1, align='v'),
                                  plot_grid(legend, ncol=2),
                                  rel_widths=c(1, 0.5)))
                 }, height=500)
               
               
               
  )
  
  output$plot3<-renderPlot({
    plot<-ggplot(total_data)+
      geom_line(aes(x=Year, y=Value, group=WorkingPattern, colour=WorkingPattern))+
      geom_point(aes(x=Year, y=Value, group=WorkingPattern, colour=WorkingPattern, shape=WorkingPattern, size=WorkingPattern))+
      theme_bw()+
      wpColours+
      wpShapes+
      wpSizes+
      labs(title="Pay Comparison by Working Pattern", y="Median")+
      theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(),
            legend.text = element_text(size = 11), legend.key.size = unit(2,"line"))
    legend<-get_legend(plot)
    plot<-plot+theme(legend.position = "none")
    ggdraw(plot_grid(plot_grid(plot, ncol=1, align='v'),
                     plot_grid(legend, ncol=2),
                     rel_widths=c(1, 0.5)))
    }, height=500)
  
  observeEvent(input$wp_dropdown,
               output$plot3<-renderPlot({
                 plot<-ggplot(dat_wp())+
                   geom_line(aes(x=Year, y=Value, group=WorkingPattern, colour=WorkingPattern))+
                   geom_point(aes(x=Year, y=Value, group=WorkingPattern, colour=WorkingPattern, shape=WorkingPattern, size=WorkingPattern))+
                   theme_bw()+
                   wpColours+
                   wpShapes+
                   wpSizes+
                   labs(title="Pay Comparison by Working Pattern", y="Median")+
                   theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(),
                         legend.text = element_text(size = 11), legend.key.size = unit(2,"line"))
                 legend<-get_legend(plot)
                 plot<-plot+theme(legend.position = "none")
                 ggdraw(plot_grid(plot_grid(plot, ncol=1, align='v'),
                                  plot_grid(legend, ncol=2),
                                  rel_widths=c(1, 0.5)))
                 }, height=500)
               
  )
  
  output$plot4<-renderPlot({
    plot<-ggplot(total_data)+
      geom_line(aes(x=Year, y=Value, group=AgeGroup, colour=AgeGroup))+
      geom_point(aes(x=Year, y=Value, group=AgeGroup, colour=AgeGroup, shape=AgeGroup, size=AgeGroup))+
      theme_bw()+
      ageColours+
      ageShapes+
      ageSizes+
      labs(title="Pay Comparison by Age Group", y="Median")+
      theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(),
            legend.text = element_text(size = 11), legend.key.size = unit(2,"line"))
    legend<-get_legend(plot)
    plot<-plot+theme(legend.position = "none")
    ggdraw(plot_grid(plot_grid(plot, ncol=1, align='v'),
                     plot_grid(legend, ncol=2),
                     rel_widths=c(1, 0.5)))
    }, height=500)
  
  observeEvent(input$age_dropdown,
               output$plot4<-renderPlot({
                 plot<-ggplot(dat_age())+
                   geom_line(aes(x=Year, y=Value, group=AgeGroup, colour=AgeGroup))+
                   geom_point(aes(x=Year, y=Value, group=AgeGroup, colour=AgeGroup, shape=AgeGroup, size=AgeGroup))+
                   theme_bw()+
                   ageColours+
                   ageShapes+
                   ageSizes+
                   labs(title="Pay Comparison by Age Group", y="Median")+
                   theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(),
                         legend.text = element_text(size = 11), legend.key.size = unit(2,"line"))
                 legend<-get_legend(plot)
                 plot<-plot+theme(legend.position = "none")
                 ggdraw(plot_grid(plot_grid(plot, ncol=1, align='v'),
                                  plot_grid(legend, ncol=2),
                                  rel_widths=c(1, 0.5)))
                 }, height=500)
               
  )
  
  output$plot5<-renderPlot({
    plot<-ggplot(total_data)+
      geom_line(aes(x=Year, y=Value, group=WorkRegion, colour=WorkRegion))+
      geom_point(aes(x=Year, y=Value, group=WorkRegion, colour=WorkRegion, shape=WorkRegion, size=WorkRegion))+
      theme_bw()+
      wrColours+
      wrShapes+
      wrSizes+
      labs(title="Pay Comparison by Work Region", y="Median")+
      theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(),
            legend.text = element_text(size = 11), legend.key.size = unit(2,"line"))
    legend<-get_legend(plot)
    plot<-plot+theme(legend.position = "none")
    ggdraw(plot_grid(plot_grid(plot, ncol=1, align='v'),
                     plot_grid(legend, ncol=2),
                     rel_widths=c(1, 0.5)))
    }, height=500)
  
  observeEvent(input$wr_dropdown,
               output$plot5<-renderPlot({
                 plot<-ggplot(dat_wr())+
                   geom_line(aes(x=Year, y=Value, group=WorkRegion, colour=WorkRegion))+
                   geom_point(aes(x=Year, y=Value, group=WorkRegion, colour=WorkRegion, shape=WorkRegion, size=WorkRegion))+
                   theme_bw()+
                   wrColours+
                   wrShapes+
                   wrSizes+
                   labs(title="Pay Comparison by Work Region", y="Median")+
                   theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(),
                         legend.text = element_text(size = 11), legend.key.size = unit(2,"line"))
                 legend<-get_legend(plot)
                 plot<-plot+theme(legend.position = "none")
                 ggdraw(plot_grid(plot_grid(plot, ncol=1, align='v'),
                                  plot_grid(legend, ncol=2),
                                  rel_widths=c(1, 0.5)))
                 }, height=500)
               
  )
  
  output$plot6<-renderPlot({
    plot<-ggplot(total_data)+
      geom_line(aes(x=Year, y=Value, group=Occupation, colour=Occupation))+
      geom_point(aes(x=Year, y=Value, group=Occupation, colour=Occupation, shape=Occupation, size=Occupation))+
      theme_bw()+
      occColours+
      occShapes+
      occSizes+
      labs(title="Pay Comparison by Occupation", y="Median")+
      theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(),
            legend.text = element_text(size = 11), legend.key.size = unit(2,"line"))
    legend<-get_legend(plot)
    plot<-plot+theme(legend.position = "none")
    ggdraw(plot_grid(plot_grid(plot, ncol=1, align='v'),
                     plot_grid(legend, ncol=2),
                     rel_widths=c(1, 0.5)))
    }, height=500)
  
  observeEvent(input$occ_dropdown,
               output$plot6<-renderPlot({
                 plot<-ggplot(dat_occ())+
                   geom_line(aes(x=Year, y=Value, group=Occupation, colour=Occupation))+
                   geom_point(aes(x=Year, y=Value, group=Occupation, colour=Occupation, shape=Occupation, size=Occupation))+
                   theme_bw()+
                   occColours+
                   occShapes+
                   occSizes+
                   labs(title="Pay Comparison by Occupation", y="Median")+
                   theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(),
                         legend.text = element_text(size = 11), legend.key.size = unit(2,"line"))
                 legend<-get_legend(plot)
                 plot<-plot+theme(legend.position = "none")
                 ggdraw(plot_grid(plot_grid(plot, ncol=1, align='v'),
                                  plot_grid(legend, ncol=2),
                                  rel_widths=c(1, 0.5)))
                 }, height=500)
               
  )
  
  output$plot7<-renderPlot({
    plot<-ggplot(total_data)+
      geom_line(aes(x=Year, y=Value, group=Industry, colour=Industry))+
      geom_point(aes(x=Year, y=Value, group=Industry, colour=Industry, shape=Industry, size=Industry))+
      theme_bw()+
      indColours+
      indShapes+
      indSizes+
      labs(title="Pay Comparison by Industry", y="Median")+
      theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(),
            legend.text = element_text(size = 11), legend.key.size = unit(1,"cm"),
            legend.spacing.x = unit(1, "cm"))+
      guides(col=guide_legend(ncol=2,byrow=TRUE))
    legend<-get_legend(plot)
    plot<-plot+theme(legend.position = "none")
    ggdraw(plot_grid(plot_grid(plot, ncol=1, align='v'),
                     plot_grid(legend, ncol=1),
                     rel_widths=c(1, 0.5)))
    }, height=500)
  
  observeEvent(input$ind_dropdown,
               output$plot7<-renderPlot({
                 plot<-ggplot(dat_ind())+
                   geom_line(aes(x=Year, y=Value, group=Industry, colour=Industry))+
                   geom_point(aes(x=Year, y=Value, group=Industry, colour=Industry, shape=Industry, size=Industry))+
                   theme_bw()+
                   indColours+
                   indShapes+
                   indSizes+
                   labs(title="Pay Comparison by Industry", y="Median")+
                   theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(),
                         legend.text = element_text(size = 11), legend.key.size = unit(1,"cm"),
                         legend.spacing.x = unit(1, "cm"))+
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
