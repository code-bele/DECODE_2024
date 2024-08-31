library(shiny)
library(plotly)
library(ggplot2)
library(readxl)
library(dplyr)
library(tidyr)

# Define the fixed file path
file_path <- "adae.xlsx"  # Replace with your actual file path

# Define UI for application
ui <- fluidPage(
  titlePanel("Adverse Events Analysis"),
  
  # Main panel with tabbed layout
  mainPanel(
    tabsetPanel(
      id = "tabs",
      
      tabPanel("Treemap Plot", 
               textOutput("description_treemap"),
               sliderInput("num_events", "Number of Adverse Events", 
                           min = 1, max = 60, value = 20),
               plotlyOutput("treemapPlot")),
      
      tabPanel("Dot Plot", 
               textOutput("description_dotplot"),
               selectInput("trta_filter", "Select Treatment", 
                           choices = c("All treatment doses", "Placebo", "Xanomeline Low Dose", "Xanomeline High Dose"),
                           selected = "All treatment doses"),
               plotlyOutput("dotPlot")),
      
      tabPanel("Box Plot",
               textOutput("description_boxplot"),
               selectInput("log_scale", "Log Scale",
                           choices = c("No" = FALSE, "Yes" = TRUE),
                           selected = FALSE),
               plotlyOutput("boxPlot"))
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive expression to read and process data
  reactive_data <- reactive({
    # Read the Excel file
    adverse <- read_xlsx(file_path, sheet = "cdisc-adae")
    
    # Columns to drop
    drops_ad <- c("SAFFL", "ASTDTF", "ADURU", "AELLTCD", "AEPTCD", 
                  "AEHLGTCD", "AESOCCD", "AEACN", "AOCCFL",
                  "AOCCSFL", "AOCC02FL", "AOCC03FL", "AOCC04FL", "AOCC01FL",
                  "AEHLTCD")
    adae <- adverse[, !(names(adverse) %in% drops_ad)]
    
    # Filter and process data for treemap
    num <- input$num_events
    adae_filtered <- adae %>% filter( TRTEMFL== "Y")
    frequent_ae <- adae_filtered %>%
      group_by(AETERM) %>%
      filter(n() > num)
    
    df <- as.data.frame(table(frequent_ae$AETERM, frequent_ae$AEBODSYS))
    df <- df %>% filter(Freq != 0)
    colnames(df) <- c("labels", "parents", "values")
    
    # Calculate sums for each parent and add an overall parent
    parent_sums <- df %>%
      group_by(parents) %>%
      summarise(values = sum(values))
    overall_sum <- sum(parent_sums$values)
    
    df <- rbind(
      df,
      parent_sums %>% mutate(labels = parents, parents = "Adverse Events"),
      data.frame(labels = "Adverse Events", parents = "", values = overall_sum)
    )
    
    # Prepare data for dot plot
    ad <- adae
    ad <- ad %>% select(ADURN, AESCAN, AESDTH, AESHOSP, AESLIFE, AESER, TRTA)
    ad <- ad %>% filter(AESCAN == 'Y' | AESDTH == 'Y' | AESLIFE == 'Y' | AESHOSP == 'Y' | AESER == "Y")
    names(ad) <- c("ADURN", "Cancer", "Death", "Hospitalization", "Life_Threat", "Severe", "TRTA")
    
    columns_to_replace <- c("Cancer", "Death", "Hospitalization", "Life_Threat", "Severe")
    for (col in columns_to_replace) {
      ad[[col]] <- ifelse(ad[[col]] == "Y", col, ad[[col]])
      ad[[col]] <- ifelse(ad[[col]] == "N", "", ad[[col]])
    }
    ad$Combination <- paste(ad$Cancer, ad$Death, ad$Hospitalization, ad$Life_Threat, ad$Severe, sep = "-")
    
    count_data <- ad %>%
      group_by(ADURN, Combination, TRTA) %>%
      summarise(Count = n(), .groups = 'drop')
    
    expanded_data <- count_data %>%
      uncount(Count) %>%
      group_by(ADURN, Combination) %>%
      mutate(Y_Position = row_number())
    
    # Process data for box plot
    log_scale <- input$log_scale
    frequent_ae <- frequent_ae %>% filter(!is.na(ADURN))
    if (log_scale) {
      frequent_ae <- frequent_ae %>% mutate(ADURN = log2(ADURN))
    }
    
    list(
      treemap_data = df, 
      dot_plot_data = expanded_data, 
      box_plot_data = frequent_ae
    )
  })
  
  # Render treemap plot
  output$treemapPlot <- renderPlotly({
    data <- reactive_data()$treemap_data
    plot_ly(
      data,
      labels = ~labels,
      parents = ~parents,
      values = ~values,
      type = 'treemap',
      textinfo = 'label+value+percent',
      marker = list(line = list(color = "black", width = 2)),
      textfont = list(size = 14, color = "white")  # Uniform font color and increased size
    ) %>%
      layout(
        title = "Adverse Events Treemap",
        autosize = TRUE,
        width = 900,  # Increase plot width
        height = 700  # Increase plot height
      )
  })
  
  # Render dot plot
  output$dotPlot <- renderPlotly({
    # Filter data based on TRTA selection
    trta_filter <- input$trta_filter
    data <- reactive_data()$dot_plot_data
    
    if (trta_filter != "All treatment doses") {
      data <- data %>% filter(TRTA == trta_filter)
    }
    
    # Calculate size based on Count and merge with original data
    sizes <- data %>%
      group_by(ADURN, Combination) %>%
      summarise(size = n(), .groups = 'drop')
    
    expanded_data_merged <- data %>%
      left_join(sizes, by = c("ADURN", "Combination"))
    
    plot_ly(expanded_data_merged, 
            x = ~factor(ADURN), 
            y = ~Combination,
            size = ~size, 
            color = ~Combination,  # Different colors for different combinations
            text = ~paste("Duration:", ADURN, "<br>Combination:", Combination, "<br>Number of Cases:", size),
            hoverinfo = "text",
            type = 'scatter', 
            mode = 'markers',
            marker = list(opacity = 1, 
                          sizemode = 'diameter', 
                          sizeref =  max(expanded_data_merged$size) / (length(~Combination) * 1.5) ,  # Adjusted size reference
                          sizemin = 2,  # Reduced minimum size
                          sizemax = 5 )) %>%  # Reduced maximum size
      layout(title = "Categorical Dot Plot of Duration of AE and Effects",
             xaxis = list(title = "Duration"),
             yaxis = list(title = "Adverse Event Combinations"),
             showlegend = FALSE,  # Completely remove legend
             autosize = TRUE,  # Automatically size plot
             width = 800,  # Adjust plot width
             height = 600)  # Adjust plot height
  })
  
  # Render box plot
  
  output$boxPlot <- renderPlotly({
    data <- reactive_data()$box_plot_data
    p <- ggplot(data, aes(x = AETERM, y = ADURN)) +
      geom_boxplot(fill = 'white') +
      geom_jitter(aes(color = AETERM), size = 3, alpha = 0.5) +
      labs(x = "Adverse Event Term", y = "Duration",
           title = "Boxplot of Duration by Major Adverse Event Term") +
      scale_color_discrete(name = "Adverse Events") +
      theme(axis.title = element_text(size = 12),
            axis.text = element_text(size = 10, angle = 45, hjust = 1),  # Rotate x-axis labels
            legend.position = "none") +
      theme(plot.margin = margin(20, 20, 20, 20))  # Increase plot margins
    ggplotly(p) %>%
      layout(width = 1000,  # Increase plot width
             height = 700,  # Increase plot height
             autosize = TRUE)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
