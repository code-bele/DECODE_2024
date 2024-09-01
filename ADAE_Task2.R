library(shiny)
library(shinythemes)
library(plotly)
library(dplyr)
library(readxl)
library(tidyr)

file_path <- 'adae.xlsx' 
adverse <- read_xlsx(file_path, sheet = "cdisc-adae")

# Columns to drop
drops_ad <- c("SAFFL", "ASTDTF", "ADURU", "AELLTCD", "AEPTCD", 
              "AEHLGTCD", "AESOCCD", "AEACN", "AOCCFL",
              "AOCCSFL", "AOCC02FL", "AOCC03FL", "AOCC04FL", "AOCC01FL",
              "AEHLTCD")
adae <- adverse[, !(names(adverse) %in% drops_ad)]

# Replace NA in AEREL with "Unknown"
adae <- adae %>%
  mutate(AEREL = ifelse(is.na(AEREL), "UNKNOWN", AEREL)) %>%
  filter(TRTEMFL == "Y")

# Define UI for application
ui <- fluidPage(
  theme = shinytheme("cosmo"),  # Apply an aesthetic theme
  titlePanel("Major Adverse Events Analysis"),
  
  # Main content with full width
  fluidRow(
    column(width = 12,
           tabsetPanel(
             id = "tabs",
             
             # Tree Map Tab
             tabPanel("Tree Map", 
                      # Description at the top
                      div(
                        style = "margin-bottom: 20px; padding: 10px; background-color: #f9f9f9; border-radius: 5px;",
                        h4("Explore the occurrence of adverse events and their related body systems across different categories and treatments."),
                        p("Use the controls below to filter and customize the tree map visualization.")
                      ),
                      
                      # Controls
                      fluidRow(
                        column(3,
                               sliderInput("num_events", "Minimum Number of Adverse Events:", 
                                           min = 1, max = 60, value = 1, width = "100%")
                        ),
                        column(2,
                               selectInput("count_type", "Count Type",
                                           choices = c("Number of Events" = "events",
                                                       "Number of Subjects" = "subjects"),
                                           selected = "events")
                        ),
                        column(2,
                               selectInput("trta_filter", "Select Treatment", 
                                           choices = c("All", "Placebo",
                                                       "Xanomeline Low Dose", 
                                                       "Xanomeline High Dose"),
                                           selected = "All")
                        ),
                        column(2,
                               selectInput("aerel_filter", "Select AE Relationship", 
                                           choices = c("All", unique(adae$AEREL)),
                                           selected = "All")
                        ),
                        
                        column(2, 
                               selectInput("aesev_filter", "Select AE Severity", 
                                           choices = c("All", unique(adae$AESEV)),
                                           selected = "All")
                        )
                      ),
                      
                      # Tree map Plot
                      div(
                        style = "margin-top: 30px;",
                        plotlyOutput("treemapPlot", height = "800px")
                      )
             ),
             
             # Dot Plot Tab
             tabPanel("Dot Plot", 
                      # Description at the top
                      div(
                        style = "margin-bottom: 20px; padding: 10px; background-color: #f9f9f9; border-radius: 5px;",
                        h4("Visualize the duration of adverse events with serious outcomes across different treatments and combinations."),
                        p("Use the controls below to filter and customize the dot plot visualization.")
                      ),
                      
                      # Controls
                      fluidRow(
                        column(4,
                               selectInput("trta_filter_2", "Select Treatment", 
                                           choices = c("All treatment doses", "Placebo", 
                                                       "Xanomeline Low Dose", "Xanomeline High Dose"),
                                           selected = "All treatment doses")
                        )
                      ),
                      
                      # Dot Plot
                      div(
                        style = "margin-top: 30px;",
                        plotlyOutput("dotPlot", height = "700px", width = "100%")
                      )
             ),
             
             # Box Plot Tab
             tabPanel("Box Plot",
                      # Description at the top
                      div(
                        style = "margin-bottom: 20px; padding: 10px; background-color: #f9f9f9; border-radius: 5px;",
                        h4("Analyze the distribution of adverse event durations by event term."),
                        p("Use the controls below to filter and customize the box plot visualization.")
                      ),
                      
                      # Controls
                      fluidRow(
                        column(3,
                               selectInput("log_scale", "Log Scale:",
                                           choices = c("No" = FALSE, "Yes" = TRUE),
                                           selected = FALSE)
                        ),
                        column(3,
                               sliderInput("num_events_2", "Minimum Number of Adverse Events:", 
                                           min = 1, max = 60, value = 20, width = "100%")
                        )
                      ),
                      
                      # Box Plot
                      div(
                        style = "margin-top: 30px;",
                        plotlyOutput("boxPlot", height = "700px", width = "100%")
                      )
             )
           )
    )
  )
)

server <- function(input, output, session) {
  
  # Load data and preprocess it before rendering UI
  
  
  # Update UI inputs dynamically based on filtered data
  observe({
    updateSelectInput(session, "aerel_filter", 
                      choices = c("All", unique(adae$AEREL)),
                      selected = "All")
    updateSelectInput(session, "aesev_filter", 
                      choices = c("All", unique(adae$AESEV)),
                      selected = "All")
  })
  
  # Reactive expression to process data based on inputs
  reactive_data <- reactive({
    adae_filtered <- adae
    
    # Apply filters based on user selection
    if (input$trta_filter != "All") {
      adae_filtered <- adae_filtered %>% filter(TRTA == input$trta_filter)
    }
    if (input$aerel_filter != "All") {
      adae_filtered <- adae_filtered %>% filter(AEREL == input$aerel_filter)
    }
    if (input$aesev_filter != "All") {
      adae_filtered <- adae_filtered %>% filter(AESEV == input$aesev_filter)
    }
    
    num <- input$num_events
    
    # Further processing based on the input number of events
    adae_filtered <- adae_filtered %>%
      group_by(AETERM) %>%
      filter(n() > num) %>%
      ungroup()
    
    if (nrow(adae_filtered) == 0) {
      return(list(treemap_data = data.frame(labels = character(), 
                                            parents = character(),
                                            values = numeric())))
    }
    
    # Count based on the selected count type
    if (input$count_type == "events") {
      df <- adae_filtered %>%
        count(AETERM, AEBODSYS) %>%
        rename(labels = AETERM, parents = AEBODSYS, values = n)
    } else {
      df <- adae_filtered %>%
        group_by(AETERM, AEBODSYS) %>%
        summarise(values = n_distinct(USUBJID), .groups = 'drop') %>%
        rename(labels = AETERM, parents = AEBODSYS)
    }
    
    # Calculate sums for each AEBODSYS parent
    parent_sums <- df %>%
      group_by(parents) %>%
      summarise(values = sum(values), .groups = 'drop') %>%
      mutate(labels = parents, parents = "Adverse Events")
    
    # Create the overall parent node
    total_sum <- sum(parent_sums$values)
    total_node <- data.frame(labels = "Adverse Events", 
                             parents = "", 
                             values = total_sum)
    
    # Combine all data into one dataframe
    df <- bind_rows(df, parent_sums, total_node)
    
    # Prepare data for dot plot
    ad <- adae_filtered %>% 
      select(ADURN, AESCAN, AESDTH, AESHOSP, AESLIFE, AESER, TRTA, AETERM, USUBJID)
    
    ad <- ad %>% filter(AESCAN == 'Y' | AESDTH == 'Y' | AESLIFE == 'Y' | 
                          AESHOSP == 'Y' | AESER == "Y")
    
    # Rename columns for clarity
    names(ad) <- c("ADURN", "Cancer", "Death", "Hospitalization",
                   "Life_Threat", "Severe", "TRTA", "AETERM", "USUBJID")
    
    # Replace 'Y' with column names and 'N' with empty strings
    columns_to_replace <- c("Cancer", "Death", "Hospitalization", 
                            "Life_Threat", "Severe")
    for (col in columns_to_replace) {
      ad[[col]] <- ifelse(ad[[col]] == "Y", col, "")
    }
    ad$Combination <- apply(ad[, columns_to_replace], 1, function(x) {
      paste(na.omit(x), collapse = "-")
    })
    
    # Remove trailing '-' if any
    ad$Combination <- gsub("-$", "", ad$Combination)
    
    # Handle cases where Combination might be empty
    ad$Combination <- ifelse(ad$Combination == "", "None", ad$Combination)
    
    count_data <- ad %>%
      group_by(ADURN, Combination, TRTA) %>%
      summarise(Count = n(), .groups = 'drop')
    
    expanded_data <- count_data %>%
      uncount(Count) %>%
      group_by(ADURN, Combination) %>%
      mutate(Y_Position = row_number()) %>%
      ungroup()
    
    # Process data for box plot
    num2 <- input$num_events_2
    log_scale <- input$log_scale
    frequent_ae <- adae_filtered %>%
      group_by(AETERM) %>%
      filter(n() > num2) %>%
      ungroup() %>%
      filter(!is.na(ADURN))
    
    if (log_scale) {
      frequent_ae <- frequent_ae %>% mutate(ADURN = log2(ADURN))
    }
    
    list(
      treemap_data = df,
      dot_plot_data = expanded_data,
      box_plot_data = frequent_ae
    )
  })
  
  # Render Tree Map Plot
  output$treemapPlot <- renderPlotly({
    data <- reactive_data()$treemap_data
    if (nrow(data) == 0) return(NULL)
    
    plot_ly(
      data,
      labels = ~labels,
      parents = ~parents,
      values = ~values,
      type = 'treemap',
      textinfo = 'label+value+percent',
      marker = list(line = list(color = "black", width = 2)),
      hovertemplate = paste(
        "<b>%{label}</b><br>",
        "Value: %{value}<br>",
        "Parent: %{parent}<br>"
      )
    ) %>%
      layout(
        title = list(
          text = "Distribution of Adverse Events",
          font = list(size = 24)
        ),
        margin = list(l = 50, r = 50, t = 50, b = 50)
      )
  })
  
  # Render Dot Plot
  output$dotPlot <- renderPlotly({
    data <- reactive_data()$dot_plot_data
    
    # Filter data based on TRTA selection
    trta_filter_2 <- input$trta_filter_2
    if (trta_filter_2 != "All treatment doses") {
      data <- data %>% filter(TRTA == trta_filter_2)
    }
    
    # Calculate size based on Count and merge with original data
    sizes <- data %>%
      group_by(ADURN, Combination) %>%
      summarise(size = n(), .groups = 'drop')
    
    expanded_data_merged <- data %>%
      left_join(sizes, by = c("ADURN", "Combination"))
    
    plot_ly(expanded_data_merged, 
            x = ~ADURN, 
            y = ~Combination,
            type = 'scatter', 
            mode = 'markers',
            size = ~size, 
            color = ~Combination,  # Different colors for different combinations
            colors = "Set1",
            marker = list(
              opacity = 0.55,
              sizemode = 'diameter',
              sizeref = max(expanded_data_merged$size) / (length(unique(expanded_data_merged$Combination)) * 0.35),
              sizemin = 3
            ),
            hoverinfo = 'text',
            text = ~paste(
              "<b>Combination:</b>", Combination, "<br>",
              "<b>Duration:</b>", ADURN, " days<br>",
              "<b>Number of Cases:</b>", size
            )
    ) %>%
      layout(
        showlegend = FALSE, 
        title = list(
          text = "Adverse Event Duration by Combination and Treatment",
          font = list(size = 24)
        ),
        xaxis = list(
          title = "Duration of Adverse Events (days)", 
          zeroline = FALSE,
          showline = TRUE,
          showgrid = TRUE,
          mirror = "ticks",
          titlefont = list(size = 18),
          tickfont = list(size = 14)
        ),
        yaxis = list(
          title = "",
          zeroline = FALSE,
          showline = FALSE,
          showgrid = TRUE,
          mirror = "ticks",
          tickfont = list(size = 14)
        ),
        margin = list(l = 100, r = 50, t = 100, b = 100)
      )
  })
  
  # Render Box Plot
  output$boxPlot <- renderPlotly({
    data <- reactive_data()$box_plot_data
    if (nrow(data) == 0) return(NULL)
    
    p <- ggplot(data, aes(x = AETERM, y = ADURN, fill = AETERM)) +
      geom_boxplot(outlier.shape = NA, alpha = 0.7) +
      geom_jitter(aes(color = AETERM), size = 2, alpha = 0.6, width = 0.2) +
      labs(
        x = "Adverse Event Term",
        y = ifelse(input$log_scale, "Log2(Duration)", "Duration (days)"),
        title = "Boxplot of Duration by Major Adverse Event Term"
      ) +
      scale_fill_brewer(palette = "Set3") +
      scale_color_brewer(palette = "Set1") +
      theme_minimal(base_size = 15) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        legend.position = "none",
        plot.margin = margin(20, 20, 20, 20)
      )
    
    ggplotly(p, tooltip = c("x", "y")) %>%
      layout(
        title = list(
          text = "Boxplot of Duration by Major Adverse Event Term",
          font = list(size = 24)
        ),
        xaxis = list(title = "Adverse Event Term", tickfont = list(size = 14)),
        yaxis = list(title = ifelse(input$log_scale, "Log2(Duration)", "Duration (days)"), tickfont = list(size = 14)),
        margin = list(l = 80, r = 50, t = 80, b = 150)
      )
  })
  
}

shinyApp(ui = ui, server = server)
