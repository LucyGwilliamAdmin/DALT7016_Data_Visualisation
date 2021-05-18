library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(ggplot2)
library(dplyr)
library(stringr)
library(RColorBrewer)
library(ggpubr)

ui <- dashboardPage(
  dashboardHeader(title = "Pay Inequalities"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("About the dashboard", tabName = "about", icon = icon("dashboard")),
      menuItem("Pay gap data", tabName = "paygap", icon = icon("th")),
      menuItem("Pay comparisons", tabName = "paycomparisons", icon = icon("th"))
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
                  tabItem(tabName = "paycomparisons",
                          h2("Pay comparisons"),
                          fluidRow(
                            column(12,
                                   box(width=12,pickerInput("sex_dropdown", "Sex",
                                                            c("Female", "Male"),
                                                            options = list(
                                                              `actions-box` = TRUE,
                                                              size = 15,
                                                              `selected-text-format` = "count>0",
                                                              `none-selected-text`="No items selected"),
                                                            multiple = TRUE
                                   ),
                                   plotOutput("plot2"),plotOutput("plot2_leg")))),
                          fluidRow(
                            column(6,
                                   box(width=12,pickerInput("wp_dropdown", "Working pattern",
                                                            c("Full-time", "Part-time"),
                                                            options = list(
                                                              `actions-box` = TRUE,
                                                              size = 15,
                                                              `selected-text-format` = "count>0",
                                                              `none-selected-text`="No items selected"),
                                                            multiple = TRUE
                                   ),
                                   plotOutput("plot3")))),
                          fluidRow(
                            column(6,
                                   box(width=12,pickerInput("age_dropdown", "Age group",
                                                            c("16-17", "18-21", "22-29",
                                                              "30-39", "40-49", "50-59",
                                                              "60+"),
                                                            options = list(
                                                              `actions-box` = TRUE,
                                                              size = 15,
                                                              `selected-text-format` = "count>0",
                                                              `none-selected-text`="No items selected"),
                                                            multiple = TRUE
                                   ),
                                   plotOutput("plot4"))),
                            column(6,
                                   box(width=12,pickerInput("wr_dropdown", "Work region",
                                                            c("East", "East Midlands", "London",
                                                              "North East", "North West", "Northern Ireland",
                                                              "Scotland", "South East", "South West", "Wales",
                                                              "West Midlands", "Yorkshire and The Humber"),
                                                            options = list(
                                                              `actions-box` = TRUE,
                                                              size = 15,
                                                              `selected-text-format` = "count>0",
                                                              `none-selected-text`="No items selected"),
                                                            multiple = TRUE
                                   ),
                                   plotOutput("plot5")))),
                          fluidRow(
                            column(6,
                                   box(width=12,pickerInput("occ_dropdown", "Occupation",
                                                            c("Administrative and secretarial occupations",
                                                              "Associate professional and technical occupations",
                                                              "Caring, leisure and other service occupations",
                                                              "Elementary occupations",
                                                              "Managers, directors and senior officials",
                                                              "Process, plant and machine operatives",
                                                              "Professional occupations",
                                                              "Sales and customer service occupations",
                                                              "Skilled trades occupations"),
                                                            options = list(
                                                              `actions-box` = TRUE,
                                                              size = 15,
                                                              `selected-text-format` = "count>0",
                                                              `none-selected-text`="No items selected"),
                                                            multiple = TRUE
                                   ),
                                   plotOutput("plot6"))),
                            column(6,
                                   box(width=12,pickerInput("ind_dropdown", "Industry",
                                                            c("Accommodation and food service activities",
                                                              "Activities of extraterritorial organisations and bodies",
                                                              "Activities of households as employers; undifferianted goods-and-services-producing activities of households for own use",
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
                                                            ),
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
                 ggplot(dat(),aes_string(x=input$breakdown_radio, y="Value", fill=fill()))+
                   theme_bw()+
                   geom_bar(position=position_dodge(width=0.8, preserve = "single"), stat = 'identity', width=0.5)+
                   coord_flip()+
                   labs(title=input$gap_radio, y="Percentage", x=input$breakdown_radio)+
                   scale_x_discrete(labels = function(x) str_wrap(x, width = 25))+
                   guides(fill = guide_legend(reverse = TRUE))+
                   theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 20),
                         legend.key.size = unit(1, "cm"),
                         axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))},
                 height = 700,width = 1150)
               
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
    keep<-c("Year", "Value", "WorkingPattern", "comment")
    keep<-keep[keep != ""]
    
    data<-comparison_data %>%
      filter_at(vars(names(comparison_data)[!names(comparison_data) %in% keep]), all_vars(.=="Total"))%>%
      select(!!unlist(keep)) %>%
      filter(WorkingPattern %in% c("Total", input$wp_dropdown))
    
  })
  
  dat_age <- reactive({
    keep<-c("Year", "Value", "AgeGroup", "comment")
    keep<-keep[keep != ""]
    
    data<-comparison_data %>%
      filter_at(vars(names(comparison_data)[!names(comparison_data) %in% keep]), all_vars(.=="Total"))%>%
      select(!!unlist(keep)) %>%
      filter(AgeGroup %in% c("Total", input$age_dropdown))
    
  })
  
  dat_wr <- reactive({
    keep<-c("Year", "Value", "WorkRegion", "comment")
    keep<-keep[keep != ""]
    
    data<-comparison_data %>%
      filter_at(vars(names(comparison_data)[!names(comparison_data) %in% keep]), all_vars(.=="Total"))%>%
      select(!!unlist(keep)) %>%
      filter(WorkRegion %in% c("Total", input$wr_dropdown))
    
  })
  
  dat_occ <- reactive({
    keep<-c("Year", "Value", "Occupation", "comment")
    keep<-keep[keep != ""]
    
    data<-comparison_data %>%
      filter_at(vars(names(comparison_data)[!names(comparison_data) %in% keep]), all_vars(.=="Total"))%>%
      select(!!unlist(keep)) %>%
      filter(Occupation %in% c("Total", input$occ_dropdown))
    
  })
  
  dat_ind <- reactive({
    keep<-c("Year", "Value", "Industry", "comment")
    keep<-keep[keep != ""]
    
    data<-comparison_data %>%
      filter_at(vars(names(comparison_data)[!names(comparison_data) %in% keep]), all_vars(.=="Total"))%>%
      select(!!unlist(keep)) %>%
      filter(Industry %in% c("Total", input$ind_dropdown))
    
  })
  
  output$plot2<-renderPlot({
    ggplot(total_data)+
      geom_line(aes(x=Year, y=Value, group=Sex, colour=Sex))+
      geom_point(aes(x=Year, y=Value, group=Sex, colour=Sex, shape=Sex, size=Sex))+
      theme_bw()+
      sexColours+
      sexSizes+
      sexShapes+
      labs(title="Pay Comparison by Sex", y="Median")+
      theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank())+
      guides(size=FALSE)})
  
  observeEvent(input$sex_dropdown,
               output$plot2<-renderPlot({
                 sex_plot<-ggplot(dat_sex())+
                   geom_line(aes(x=Year, y=Value, group=Sex, colour=Sex))+
                   geom_point(aes(x=Year, y=Value, group=Sex, colour=Sex, shape=Sex, size=Sex))+
                   theme_bw()+
                   sexColours+
                   sexShapes+
                   sexSizes+
                   labs(title="Pay Comparison by Sex", y="Median")+
                   theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(),
                         legend.text = element_text(size = 10))+
                   guides(size=FALSE)
                 #plot(sex_plot)
                 sex_legend<-get_legend(sex_plot)
                 sex_plot<-sex_plot+theme(legend.position = "none")
                 output$plot2_leg<-renderPlot({
                   #as_ggplot(sex_legend)
                   ggdraw(plot_grid(plot_grid(sex_plot, ncol=1, align='v'),
                                    plot_grid(NULL, sex_legend, ncol=2),
                                    rel_widths=c(1, 0.2)))
                   })
                 })
               
               
               
  )
  
  output$plot3<-renderPlot({
    ggplot(total_data)+
      geom_line(aes(x=Year, y=Value, group=WorkingPattern, colour=WorkingPattern))+
      geom_point(aes(x=Year, y=Value, group=WorkingPattern, colour=WorkingPattern, shape=WorkingPattern, size=1))+
      theme_bw()+
      wpColours+
      wpShapes+
      labs(title="Pay Comparison by Working Pattern", y="Median")+
      theme(plot.title = element_text(hjust = 0.5), legend.position="bottom")+
      guides(size=FALSE)})
  
  observeEvent(input$wp_dropdown,
               output$plot3<-renderPlot({
                 ggplot(dat_wp())+
                   geom_line(aes(x=Year, y=Value, group=WorkingPattern, colour=WorkingPattern))+
                   geom_point(aes(x=Year, y=Value, group=WorkingPattern, colour=WorkingPattern, shape=WorkingPattern, size=1))+
                   theme_bw()+
                   wpColours+
                   wpShapes+
                   labs(title="Pay Comparison by Working Pattern", y="Median")+
                   theme(plot.title = element_text(hjust = 0.5), legend.position="bottom",
                         legend.title = element_blank())+
                   guides(col=guide_legend(ncol=3,byrow=TRUE), size=FALSE)})
               
  )
  
  output$plot4<-renderPlot({
    ggplot(total_data)+
      geom_line(aes(x=Year, y=Value, group=AgeGroup, colour=AgeGroup))+
      geom_point(aes(x=Year, y=Value, group=AgeGroup, colour=AgeGroup, shape=AgeGroup, size=1))+
      theme_bw()+
      ageColours+
      ageShapes+
      labs(title="Pay Comparison by Age Group", y="Median")+
      theme(plot.title = element_text(hjust = 0.5), legend.position="bottom",
            legend.title = element_blank())+
      guides(size=FALSE)})
  
  observeEvent(input$age_dropdown,
               output$plot4<-renderPlot({
                 ggplot(dat_age())+
                   geom_line(aes(x=Year, y=Value, group=AgeGroup, colour=AgeGroup))+
                   geom_point(aes(x=Year, y=Value, group=AgeGroup, colour=AgeGroup, shape=AgeGroup, size=1))+
                   theme_bw()+
                   ageColours+
                   ageShapes+
                   labs(title="Pay Comparison by Age Group", y="Median")+
                   theme(plot.title = element_text(hjust = 0.5), legend.position="bottom",
                         legend.title = element_blank())+
                   guides(col=guide_legend(ncol=7,byrow=TRUE), size=FALSE)})
               
  )
  
  output$plot5<-renderPlot({
    ggplot(total_data)+
      geom_line(aes(x=Year, y=Value, group=WorkRegion, colour=WorkRegion))+
      geom_point(aes(x=Year, y=Value, group=WorkRegion, colour=WorkRegion, shape=WorkRegion, size=1))+
      theme_bw()+
      wrColours+
      wrShapes+
      labs(title="Pay Comparison by Work Region", y="Median")+
      theme(plot.title = element_text(hjust = 0.5), legend.position="bottom",
            legend.title = element_blank())+
      guides(size = FALSE)})
  
  observeEvent(input$wr_dropdown,
               output$plot5<-renderPlot({
                 ggplot(dat_wr())+
                   geom_line(aes(x=Year, y=Value, group=WorkRegion, colour=WorkRegion))+
                   geom_point(aes(x=Year, y=Value, group=WorkRegion, colour=WorkRegion, shape=WorkRegion, size=1))+
                   theme_bw()+
                   wrColours+
                   wrShapes+
                   labs(title="Pay Comparison by Work Region", y="Median")+
                   theme(plot.title = element_text(hjust = 0.5), legend.position="bottom",
                         legend.title = element_blank())+
                   guides(col=guide_legend(ncol=5,byrow=TRUE), size=FALSE)})
               
  )
  
  output$plot6<-renderPlot({
    ggplot(total_data)+
      geom_line(aes(x=Year, y=Value, group=Occupation, colour=Occupation))+
      geom_point(aes(x=Year, y=Value, group=Occupation, colour=Occupation, shape=Occupation, size=1))+
      theme_bw()+
      occColours+
      occShapes+
      labs(title="Pay Comparison by Occupation", y="Median")+
      theme(plot.title = element_text(hjust = 0.5), legend.position="bottom",
            legend.title = element_blank())+
      guides(size = FALSE)})
  
  observeEvent(input$occ_dropdown,
               output$plot6<-renderPlot({
                 ggplot(dat_occ())+
                   geom_line(aes(x=Year, y=Value, group=Occupation, colour=Occupation))+
                   geom_point(aes(x=Year, y=Value, group=Occupation, colour=Occupation, shape=Occupation, size=1))+
                   theme_bw()+
                   occColours+
                   occShapes+
                   labs(title="Pay Comparison by Occupation", y="Median")+
                   theme(plot.title = element_text(hjust = 0.5), legend.position="bottom",
                         legend.title = element_blank())+
                   guides(col=guide_legend(ncol=3,byrow=TRUE), size = FALSE)})
               
  )
  
  output$plot7<-renderPlot({
    ggplot(total_data)+
      geom_line(aes(x=Year, y=Value, group=Industry, colour=Industry))+
      geom_point(aes(x=Year, y=Value, group=Industry, colour=Industry, shape=Industry, size=1))+
      theme_bw()+
      indColours+
      indShapes+
      labs(title="Pay Comparison by Industry", y="Median")+
      theme(plot.title = element_text(hjust = 0.5), legend.position="bottom",
            legend.title = element_blank())+
      guides(size=FALSE)})
  
  observeEvent(input$ind_dropdown,
               output$plot7<-renderPlot({
                 ggplot(dat_ind())+
                   geom_line(aes(x=Year, y=Value, group=Industry, colour=Industry))+
                   geom_point(aes(x=Year, y=Value, group=Industry, colour=Industry, shape=Industry, size=1))+
                   theme_bw()+
                   indColours+
                   indShapes+
                   labs(title="Pay Comparison by Industry", y="Median")+
                   theme(plot.title = element_text(hjust = 0.5), legend.position="bottom",
                         legend.title = element_blank())+
                   guides(col=guide_legend(ncol=2,byrow=TRUE), size=FALSE)})
               
  )
  
  
}


# Run the application 
shinyApp(ui = ui, server = server)
