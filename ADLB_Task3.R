library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)
library(plotly)
library(tidyverse)

file_path <- 'adlb.xlsx'

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("Laboratory parameters - summary"),
  
  # Main panel
  fluidRow(
    column(12,
    tabsetPanel(
      id = "tabs",
      
      # Tab panel to display box plot
      tabPanel(
        "BoxPlot",
        
        # Description
        div(
          style = "margin-bottom: 20px; padding: 10px; background-color: #f9f9f9; border-radius: 5px;",
          h4("Explore the change and distribution of laboratory parameters during the course of trial."),
          p("Use the controls below to filter and customize the time-series box plot.")
        ),
        
        # Customization filters and controls
        fluidRow(
          column(2,
                  selectInput("boxparam", "Parameter",
                      choices = c("BASO", "EOS", "HCT", "HGB", "LYM", "MCH", "MCHC", "MCV", "MONO", "PLAT", "RBC", "WBC"),
                      selected = "HGB")
        ),
        
        column(3,
                  selectInput("boxtrtarm", "Treatment Arm",
                      choices = c("Placebo", "Low dose", "High dose"),
                      selected = "Placebo")
        )),
        
        # Time-series box pot
        div(
          style = "margin-top: 30px;",
          plotlyOutput("boxPlot", height = "600px", width = "100%")
        )
      ),
      
      # Tab panel to display dumbbell plot
      tabPanel(
        "Dumbbell plot",
        
        # Description
        div(
          style = "margin-bottom: 20px; padding: 10px; background-color: #f9f9f9; border-radius: 5px;",
          h4("Explore the change of laboratory parameters during the course of trial between specific time periods across different treatment arms."),
          p("Use the controls below to filter and customize the dumbbell plot."),
          tags$a(href="https://plotly.com/~nimal/1/", "Overview plot")
        ),
        
        # Customization filters and controls
        fluidRow(
          column(2,
            selectInput("dpparam", "Parameter",
                        choices = c("BASO", "EOS", "HCT", "HGB", "LYM", "MCH", "MCHC", "MCV", "MONO", "PLAT", "RBC", "WBC"),
                        selected = "HGB")
            ),
        
          column(3,
            selectInput("start", "Start",
                        choices = c("Baseline","Week 2","Week 4","Week 6","Week 8","Week 12","Week 16","Week 20","Week 24","End of Treatment","Week 26"),
                        selected = "Baseline")
           ),
        
          column(3,
            conditionalPanel(
              condition = 'input.start == "Baseline"',
              selectInput("end", "End",
                          choices = c("Week 2","Week 4","Week 6","Week 8","Week 12","Week 16","Week 20","Week 24","End of Treatment","Week 26"),
                          selected = "Week 26")),
           
            conditionalPanel(
              condition = 'input.start == "Week 2"',
              selectInput("end", "End",
                          choices = c("Week 4","Week 6","Week 8","Week 12","Week 16","Week 20","Week 24","End of Treatment","Week 26"),
                          selected = "Week 26")),
           
            conditionalPanel(
              condition = 'input.start == "Week 4"',
              selectInput("end", "End",
                          choices = c("Week 6","Week 8","Week 12","Week 16","Week 20","Week 24","End of Treatment","Week 26"),
                          selected = "Week 26")),
           
           conditionalPanel(
              condition = 'input.start == "Week 6"',
              selectInput("end", "End",
                          choices = c("Week 8","Week 12","Week 16","Week 20","Week 24","End of Treatment","Week 26"),
                          selected = "Week 26")),
          
            conditionalPanel(
              condition = 'input.start == "Week 8"',
              selectInput("end", "End",
                          choices = c("Week 12","Week 16","Week 20","Week 24","End of Treatment","Week 26"),
                          selected = "Week 26")),
          
            conditionalPanel(
            condition = 'input.start == "Week 12"',
              selectInput("end", "End",
                        choices = c("Week 16","Week 20","Week 24","End of Treatment","Week 26"),
                        selected = "Week 26")),
           
          conditionalPanel(
            condition = 'input.start == "Week 16"',
            selectInput("end", "End",
                        choices = c("Week 20","Week 24","End of Treatment","Week 26"),
                        selected = "Week 26")),
       
          conditionalPanel(
            condition = 'input.start == "Week 20"',
            selectInput("end", "End",
                        choices = c("Week 24","End of Treatment","Week 26"),
                        selected = "Week 26")),
          
            conditionalPanel(
            condition = 'input.start == "Week 24"',
              selectInput("end", "End",
                        choices = c("End of Treatment","Week 26"),
                        selected = "Week 26"))
            )
        ),
        
        div(
          style = "margin-top: 30px;",
          plotlyOutput("dbPlot", height = "100%", width = "100%")
        )
      ),
      
      tabPanel(
        "Table",
        
        # Description
        div(
          style = "margin-bottom: 20px; padding: 10px; background-color: #f9f9f9; border-radius: 5px;",
          h4("Table of minimum and maximum values for each LBNRIND flag"),
          p("Ambiguous values have been highlighted in red.")
        ),
        
        # Table
        div(
          style = "margin-top: 30px;",
          imageOutput("table", height = "100%", width = "100%")
          )
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Read the Excel file
  adlb_sheet <- read_excel(file_path, sheet = "adlb")
  
  # Reactive expression to read and process data
  reactive_data <- reactive({
    
    # Columns to drop
    drops_ad <- c("STUDYID", "PARCAT1", "USUBJID", "TRTP", "TRTPN", 
                  "AEHLGTCD", "LBSEQ")
    adlb <- adlb_sheet[, !(names(adlb_sheet) %in% drops_ad)]
    
    # Common data pre-processing
    adlb <- adlb |> mutate(TRTA = ifelse(TRTA == "Trt A", "Low dose", TRTA))
    adlb <- adlb |> mutate(TRTA = ifelse(TRTA == "Trt B", "High dose", TRTA))
    
    # Common data filtering
    norm_param <- adlb |> filter(PARAMN %in% seq(1, 12), !AVISIT %in% c("."))
    abnorm_baseline <- adlb |> filter(LBNRIND == "ABNORMAL", ABLFL == "Y")
    abnorm_baseline_subs <- unique(abnorm_baseline$SUBJID)
    
    # Box plot data
    boxPlot_data <- norm_param |> filter(PARAMCD == input$boxparam, TRTA == input$boxtrtarm)
    boxPlot_data$AVISIT <- factor(boxPlot_data$AVISIT, levels=c("Baseline","Week 2","Week 4","Week 6","Week 8","Week 12","Week 16","Week 20","Week 24","End of Treatment","Week 26"))
    
    # Dumbbell plot data
      dbPlot_data <- norm_param |> group_by(PARAMCD, AVISIT, TRTA) |> summarise(values = format(round(mean(AVAL), 3))) |> ungroup() |> mutate_at(vars(values), as.numeric)
      dbPlot_data <- dbPlot_data |> filter(AVISIT %in% c(input$start, input$end), PARAMCD == input$dpparam)
      dbPlot_data <- dbPlot_data |> mutate(AVISIT = ifelse(AVISIT == "End of Treatment", "EOT", AVISIT))
      
      if(input$end == "End of Treatment"){
        param_chg <- dbPlot_data  %>%
          group_by(TRTA) %>%
          pivot_wider(names_from = AVISIT, values_from = values) %>%
          mutate(chg = EOT - !!sym(input$start)) %>%
          mutate(pchg = (chg/!!sym(input$start)) * 100)
      }
      
      else{
        param_chg <- dbPlot_data  %>%
          group_by(TRTA) %>%
          pivot_wider(names_from = AVISIT, values_from = values) %>%
          mutate(chg = !!sym(input$end) - !!sym(input$start)) %>%
          mutate(pchg = (chg/!!sym(input$start)) * 100)
      }
      
      param_chg$chg <- round(param_chg$chg, 3)
      param_chg$pchg <- round(param_chg$pchg, 2)
      dbPlot_data$chg <- c(param_chg$chg, param_chg$chg)
      dbPlot_data$pchg <- c(param_chg$pchg, param_chg$pchg)
      dbPlot_data <- dbPlot_data |> mutate(AVISIT = ifelse(AVISIT == "EOT", "End of Treatment", AVISIT))
      
    
    # Calculate mean values for geom_line
    bp_mean <- norm_param |> filter(PARAMCD == input$boxparam, TRTA == input$boxtrtarm) |>
      group_by(AVISIT) |> 
      summarise(mean_value = mean(AVAL, na.rm = TRUE)) |> 
      ungroup()
    
    return(list(boxPlot_data = boxPlot_data, bp_mean = bp_mean, dbPlot_data = dbPlot_data))
  })
  
  output$boxPlot <- renderPlotly({
    data <- reactive_data()
    boxPlot_data <- data$boxPlot_data
    bp_mean <- data$bp_mean
    
    options(repr.plot.width =50, repr.plot.height =10)
    
    p <- ggplot(boxPlot_data, aes(x = AVISIT, y = AVAL)) +
      geom_boxplot(fill = 'white') +
      labs(x = "", y = "",
           title = paste("Change of", input$boxparam, "over treatment duration in", input$boxtrta, "treatment group")) +
      theme_minimal() +
      geom_line(data = bp_mean, aes(x = AVISIT, y = mean_value, group = 1), color = "blue")
    
    if(input$boxparam == "BASO"){
      p <- p + geom_hline(yintercept = 0, color = "green", linetype = "dashed", size = 1)
      p <- p + geom_hline(yintercept = .2, color = "red", linetype = "dashed", size = 1)
    }
    if(input$boxparam == "MCV"){
      p <- p + geom_hline(yintercept = 80, color = "green", linetype = "dashed", size = 1)
      p <- p + geom_hline(yintercept = 100, color = "red", linetype = "dashed", size = 1)
    }
    
    if(input$boxparam == "MCH"){
      p <- p + geom_hline(yintercept = 1.5, color = "green", linetype = "dashed", size = 1)
      p <- p + geom_hline(yintercept = 2.15, color = "red", linetype = "dashed", size = 1)
    }
    
    if(input$boxparam == "MCHC"){
      p <- p + geom_hline(yintercept = 19, color = "green", linetype = "dashed", size = 1)
    }
    
    if(input$boxparam == "WBC"){
      p <- p + geom_hline(yintercept = 3.8, color = "green", linetype = "dashed", size = 1)
      p <- p + geom_hline(yintercept = 10.7, color = "red", linetype = "dashed", size = 1)
    }
    
    if(input$boxparam == "MONO"){
      p <- p + geom_hline(yintercept = .12, color = "green", linetype = "dashed", size = 1)
      p <- p + geom_hline(yintercept = .92, color = "red", linetype = "dashed", size = 1)
    }
    
    if(input$boxparam == "EOS"){
      p <- p + geom_hline(yintercept = 0, color = "green", linetype = "dashed", size = 1)
      p <- p + geom_hline(yintercept = .57, color = "red", linetype = "dashed", size = 1)
    }
    
    if(input$boxparam == "PLAT"){
      p <- p + geom_hline(yintercept = 130, color = "green", linetype = "dashed", size = 1)
      p <- p + geom_hline(yintercept = 395, color = "red", linetype = "dashed", size = 1)
    }
    
    if(input$boxparam == "LYM"){
      p <- p + geom_hline(yintercept = .8, color = "green", linetype = "dashed", size = 1)
    }
     
    ggplotly(p)
  })
  
  output$dbPlot <- renderPlotly({
    data <- reactive_data()
    dp_data <- data$dbPlot_data
    
    options(repr.plot.width =50, repr.plot.height =10)
    
      p <-  ggplot(dp_data, aes(x= values, y= TRTA,
                                text = case_when(
                                  AVISIT == input$start ~ paste(input$start, "value:", values),
                                  AVISIT == input$end ~ paste(input$end, "value:", values, "<br>Change:", chg, "(", pchg, "%)"))
       )) +
        geom_line(aes(group = TRTA))+
        geom_point(aes(color=AVISIT), size=4) +
        labs(y="", x="", title = paste(input$dpparam ,"change - Summary"), colour = "Legend")
      
      if(input$dpparam == "BASO"){
        p <- p + geom_vline(xintercept = 0, color = "green", linetype = "dashed", size = 1)
        p <- p + geom_vline(xintercept = .2, color = "red", linetype = "dashed", size = 1)
      }
      
      if(input$dpparam == "MCV"){
        p <- p + geom_vline(xintercept = 80, color = "green", linetype = "dashed", size = 1)
        p <- p + geom_vline(xintercept = 100, color = "red", linetype = "dashed", size = 1)
      }
    
      if(input$dpparam == "MCH"){
        p <- p + geom_vline(xintercept = 1.5, color = "green", linetype = "dashed", size = 1)
        p <- p + geom_vline(xintercept = 2.15, color = "red", linetype = "dashed", size = 1)
      }
      
      if(input$dpparam == "MCHC"){
        p <- p + geom_vline(xintercept = 19, color = "green", linetype = "dashed", size = 1)
      }
      
      if(input$dpparam == "WBC"){
        p <- p + geom_vline(xintercept = 3.8, color = "green", linetype = "dashed", size = 1)
        p <- p + geom_vline(xintercept = 10.7, color = "red", linetype = "dashed", size = 1)
      }
      
      if(input$dpparam == "MONO"){
        p <- p + geom_vline(xintercept = .12, color = "green", linetype = "dashed", size = 1)
        p <- p + geom_vline(xintercept = .92, color = "red", linetype = "dashed", size = 1)
      }
      
      if(input$dpparam == "EOS"){
        p <- p + geom_vline(xintercept = 0, color = "green", linetype = "dashed", size = 1)
        p <- p + geom_vline(xintercept = .57, color = "red", linetype = "dashed", size = 1)
      }
      
      if(input$dpparam == "PLAT"){
        p <- p + geom_vline(xintercept = 130, color = "green", linetype = "dashed", size = 1)
        p <- p + geom_vline(xintercept = 395, color = "red", linetype = "dashed", size = 1)
      }
      
      if(input$dpparam == "LYM"){
        p <- p + geom_vline(xintercept = .8, color = "green", linetype = "dashed", size = 1)
      }
      
    ggplotly(p, tooltip = "text")
  })
  
  
    # A plot of fixed size
    output$table <- renderImage({
      
      img_file <- './table.png'

      # Return a list
      list(src = img_file,
           alt = "Table containing min. and max. values for low, normal and high flags")}
    , deleteFile = FALSE)
}

# Run the application 
shinyApp(ui = ui, server = server)
