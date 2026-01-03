# ============================================================================
# ACORN Explorer - Interactive Geodemographic Classification Tool
# Apple Glass-Inspired Design
# ============================================================================

# Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
 shiny,
 shinythemes,
 shinyWidgets,
 DT,
 tidyverse,
 plotly,
 scales,
 openxlsx,
 here,
 sf
)

# ============================================================================
# LOAD DATA
# ============================================================================

# Check for existing clustering results
data_path <- here::here("output", "clustering_results_2023.RData")

if (file.exists(data_path)) {
 load(data_path)
 DATA_LOADED <- TRUE
} else {
 DATA_LOADED <- FALSE
 # Create sample data structure for demo
 df_final <- tibble(
   GEOID = paste0("0", 1001:1100, "000100"),
   NAME = paste("Census Tract", 1:100, ", Sample County, Sample State"),
   X10 = factor(sample(LETTERS[1:10], 100, replace = TRUE)),
   X55 = factor(sample(1:55, 100, replace = TRUE)),
   cluster = factor(sample(1:250, 100, replace = TRUE)),
   pct_white = runif(100, 20, 95),
   pct_black = runif(100, 2, 40),
   pct_hispanic = runif(100, 5, 60),
   pct_bachelor = runif(100, 10, 60),
   median_home_value = runif(100, 100000, 800000),
   pct_renter = runif(100, 15, 70),
   pop_density = runif(100, 100, 20000)
 )
}

# Define cluster labels
cluster_labels <- c(
 "A" = "Hispanic and Kids",
 "B" = "Wealthy Nuclear Families",
 "C" = "Middle Income, Single Family",
 "D" = "Native American",
 "E" = "Wealthy Urbanites",
 "F" = "Low Income and Diverse",
 "G" = "Old, Wealthy White",
 "H" = "Low Income Minority Mix",
 "I" = "Poor, African-American",
 "J" = "Residential Institutions, Young"
)

cluster_colors <- c(
 "A" = "#69D2E7", "B" = "#E0E4CC", "C" = "#B5B5B5", "D" = "#79BD9A",
 "E" = "#F38630", "F" = "#EDC951", "G" = "#8C8590", "H" = "#CDB380",
 "I" = "#547980", "J" = "#4ECDC4"
)

# Domain definitions
domains <- list(
 "Demography" = list(
   color = "#007AFF",
   icon = "👥",
   vars = c("pct_male_under_5", "pct_male_25_29", "pct_female_under_5", "pct_female_25_29")
 ),
 "Race & Ethnicity" = list(
   color = "#AF52DE",
   icon = "🌍",
   vars = c("pct_white", "pct_black", "pct_asian", "pct_hispanic", "pct_american_indian")
 ),
 "Education" = list(
   color = "#5856D6",
   icon = "🎓",
   vars = c("pct_less_than_hs", "pct_hs_graduate", "pct_bachelor", "pct_graduate_prof")
 ),
 "Income" = list(
   color = "#34C759",
   icon = "💰",
   vars = c("pct_income_under_10k", "pct_income_200k_plus", "pct_public_assistance")
 ),
 "Housing" = list(
   color = "#FF9500",
   icon = "🏠",
   vars = c("pct_renter", "pct_vacant", "pct_1unit_detached", "median_home_value")
 ),
 "Transportation" = list(
   color = "#FF2D55",
   icon = "🚗",
   vars = c("pct_no_car", "pct_public_transit", "pct_commute_30_34")
 ),
 "Digital Access" = list(
   color = "#5AC8FA",
   icon = "📱",
   vars = c("pct_broadband", "pct_no_internet", "pct_has_computer")
 ),
 "Employment" = list(
   color = "#FF6B6B",
   icon = "💼",
   vars = c("pct_manufacturing", "pct_professional_scientific", "pct_education_health")
 )
)

# Get all analysis variables from data
if (DATA_LOADED) {
 analysis_vars <- names(df_final)[!names(df_final) %in%
   c("GEOID", "NAME", "X2", "X10", "X10_code", "X31", "X55", "cluster")]
} else {
 analysis_vars <- c("pct_white", "pct_black", "pct_hispanic", "pct_bachelor",
                    "median_home_value", "pct_renter", "pop_density")
}

# ============================================================================
# UI
# ============================================================================

ui <- fluidPage(

 # Include custom CSS
 tags$head(
   tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
   tags$style(HTML("
     .shiny-output-error { visibility: hidden; }
     .shiny-output-error:before { visibility: hidden; }
   "))
 ),

 # Header
 div(class = "app-header",
   div(class = "app-logo",
     div(class = "app-logo-icon", "🗺️"),
     div(
       h1(class = "app-title", "ACORN Explorer"),
       p(class = "app-subtitle", "Interactive Geodemographic Classification")
     )
   ),
   div(class = "header-stats",
     if (DATA_LOADED) {
       tagList(
         span(style = "color: #34C759; font-weight: 600;", "● Data Loaded"),
         span(style = "margin-left: 16px; color: #86868B;",
              paste(format(nrow(df_final), big.mark = ","), "Census Tracts"))
       )
     } else {
       span(style = "color: #FF9500; font-weight: 600;", "● Demo Mode - Run build_acorn_2023.R first")
     }
   )
 ),

 # Main Navigation Tabs
 tabsetPanel(
   id = "main_tabs",
   type = "pills",

   # ========== TAB 1: OVERVIEW ==========
   tabPanel(
     title = "Overview",
     icon = icon("chart-pie"),
     value = "overview",

     fluidRow(style = "margin-top: 20px;",
       # Stats Row
       column(12,
         fluidRow(
           column(3,
             div(class = "stat-card",
               div(class = "stat-value", textOutput("stat_tracts", inline = TRUE)),
               div(class = "stat-label", "Census Tracts")
             )
           ),
           column(3,
             div(class = "stat-card",
               div(class = "stat-value", "10"),
               div(class = "stat-label", "Cluster Groups")
             )
           ),
           column(3,
             div(class = "stat-card",
               div(class = "stat-value", textOutput("stat_vars", inline = TRUE)),
               div(class = "stat-label", "Variables")
             )
           ),
           column(3,
             div(class = "stat-card",
               div(class = "stat-value", "2023"),
               div(class = "stat-label", "ACS Data Year")
             )
           )
         )
       )
     ),

     fluidRow(style = "margin-top: 20px;",
       # Distribution Chart
       column(8,
         div(class = "glass-card",
           div(class = "glass-card-header",
             div(class = "glass-card-icon icon-blue", "📊"),
             div(
               h3(class = "glass-card-title", "Cluster Distribution"),
               p(class = "glass-card-subtitle", "Percentage of census tracts in each group")
             )
           ),
           plotlyOutput("distribution_chart", height = "350px")
         )
       ),

       # Cluster Legend
       column(4,
         div(class = "glass-card",
           div(class = "glass-card-header",
             div(class = "glass-card-icon icon-purple", "🏷️"),
             div(
               h3(class = "glass-card-title", "Cluster Groups"),
               p(class = "glass-card-subtitle", "10 geodemographic segments")
             )
           ),
           uiOutput("cluster_legend")
         )
       )
     ),

     fluidRow(
       column(12,
         div(class = "glass-card",
           div(class = "glass-card-header",
             div(class = "glass-card-icon icon-teal", "📈"),
             div(
               h3(class = "glass-card-title", "Cluster Profiles"),
               p(class = "glass-card-subtitle", "Compare key characteristics across groups")
             )
           ),
           plotlyOutput("profile_heatmap", height = "400px")
         )
       )
     )
   ),

   # ========== TAB 2: WEIGHTING ==========
   tabPanel(
     title = "Custom Weights",
     icon = icon("sliders-h"),
     value = "weighting",

     fluidRow(style = "margin-top: 20px;",
       column(4,
         div(class = "glass-card",
           div(class = "glass-card-header",
             div(class = "glass-card-icon icon-orange", "⚖️"),
             div(
               h3(class = "glass-card-title", "Domain Weights"),
               p(class = "glass-card-subtitle", "Adjust importance of each category")
             )
           ),

           # Weight sliders for each domain
           lapply(names(domains), function(domain) {
             div(class = "domain-weight-card",
               div(class = "domain-header",
                 div(class = "domain-name",
                   span(domains[[domain]]$icon),
                   span(domain)
                 ),
                 div(class = "domain-percent", textOutput(paste0("weight_val_", gsub(" |&", "_", domain)), inline = TRUE))
               ),
               sliderInput(
                 inputId = paste0("weight_", gsub(" |&", "_", domain)),
                 label = NULL,
                 min = 0,
                 max = 100,
                 value = 100,
                 step = 5,
                 ticks = FALSE
               )
             )
           }),

           hr(style = "border-color: rgba(0,0,0,0.06);"),

           div(style = "display: flex; gap: 10px;",
             actionButton("reset_weights", "Reset All", class = "btn-glass", style = "flex: 1;"),
             actionButton("apply_weights", "Apply Weights", class = "btn-primary-glass", style = "flex: 1;")
           )
         )
       ),

       column(8,
         div(class = "glass-card",
           div(class = "glass-card-header",
             div(class = "glass-card-icon icon-green", "🎯"),
             div(
               h3(class = "glass-card-title", "Weighted Distribution Preview"),
               p(class = "glass-card-subtitle", "See how weights affect cluster assignments")
             )
           ),
           plotlyOutput("weighted_preview", height = "300px")
         ),

         div(class = "glass-card",
           div(class = "glass-card-header",
             div(class = "glass-card-icon icon-pink", "📉"),
             div(
               h3(class = "glass-card-title", "Weight Impact Analysis"),
               p(class = "glass-card-subtitle", "Variable importance after weighting")
             )
           ),
           plotlyOutput("weight_impact", height = "300px")
         )
       )
     )
   ),

   # ========== TAB 3: EXPLORE DATA ==========
   tabPanel(
     title = "Explore Data",
     icon = icon("table"),
     value = "explore",

     fluidRow(style = "margin-top: 20px;",
       column(3,
         div(class = "glass-card",
           div(class = "glass-card-header",
             div(class = "glass-card-icon icon-blue", "🔍"),
             div(
               h3(class = "glass-card-title", "Filters"),
               p(class = "glass-card-subtitle", "Refine your selection")
             )
           ),

           selectInput(
             "filter_state",
             "State",
             choices = c("All States" = "all"),
             selected = "all"
           ),

           selectInput(
             "filter_cluster",
             "Cluster Group",
             choices = c("All Groups" = "all", setNames(LETTERS[1:10],
               paste(LETTERS[1:10], "-", cluster_labels))),
             selected = "all"
           ),

           sliderInput(
             "filter_pop_density",
             "Population Density",
             min = 0,
             max = 50000,
             value = c(0, 50000),
             step = 1000
           ),

           hr(style = "border-color: rgba(0,0,0,0.06);"),

           h4(style = "font-size: 14px; font-weight: 600; margin-bottom: 12px;", "Display Columns"),

           checkboxGroupInput(
             "display_cols",
             NULL,
             choices = c(
               "Basic Info" = "basic",
               "Demographics" = "demographics",
               "Income" = "income",
               "Housing" = "housing",
               "All Variables" = "all_vars"
             ),
             selected = "basic"
           )
         )
       ),

       column(9,
         div(class = "glass-card",
           div(class = "glass-card-header",
             div(class = "glass-card-icon icon-purple", "📋"),
             div(
               h3(class = "glass-card-title", "Census Tract Data"),
               p(class = "glass-card-subtitle", textOutput("table_count", inline = TRUE))
             )
           ),
           DTOutput("data_table")
         )
       )
     )
   ),

   # ========== TAB 4: EXPORT ==========
   tabPanel(
     title = "Export",
     icon = icon("download"),
     value = "export",

     fluidRow(style = "margin-top: 20px;",
       column(6,
         div(class = "glass-card",
           div(class = "glass-card-header",
             div(class = "glass-card-icon icon-green", "📤"),
             div(
               h3(class = "glass-card-title", "Export to Excel"),
               p(class = "glass-card-subtitle", "Download data sorted by state and county")
             )
           ),

           h4(style = "font-size: 14px; font-weight: 600; margin: 16px 0 12px;", "Export Options"),

           checkboxGroupInput(
             "export_options",
             NULL,
             choices = c(
               "Include all variables" = "all_vars",
               "Include index scores" = "index_scores",
               "Include cluster descriptions" = "descriptions",
               "Separate sheets by state" = "by_state"
             ),
             selected = c("descriptions")
           ),

           selectInput(
             "export_cluster_level",
             "Cluster Level",
             choices = c(
               "X10 (10 Groups)" = "X10",
               "X31 (31 Subgroups)" = "X31",
               "X55 (55 Types)" = "X55",
               "All Levels" = "all"
             ),
             selected = "X10"
           ),

           hr(style = "border-color: rgba(0,0,0,0.06);"),

           div(class = "export-btn-container",
             downloadButton("download_xlsx", "Download Excel (.xlsx)", class = "btn-success-glass export-btn"),
             downloadButton("download_csv", "Download CSV", class = "btn-glass export-btn")
           )
         )
       ),

       column(6,
         div(class = "glass-card",
           div(class = "glass-card-header",
             div(class = "glass-card-icon icon-teal", "📊"),
             div(
               h3(class = "glass-card-title", "Export Preview"),
               p(class = "glass-card-subtitle", "First 10 rows of your export")
             )
           ),
           DTOutput("export_preview")
         ),

         div(class = "glass-card",
           div(class = "glass-card-header",
             div(class = "glass-card-icon icon-orange", "📁"),
             div(
               h3(class = "glass-card-title", "Export Summary"),
               p(class = "glass-card-subtitle", "What's included in your download")
             )
           ),
           uiOutput("export_summary")
         )
       )
     )
   )
 )
)

# ============================================================================
# SERVER
# ============================================================================

server <- function(input, output, session) {

 # ========== REACTIVE VALUES ==========

 weights <- reactiveValues(
   Demography = 100,
   Race_Ethnicity = 100,
   Education = 100,
   Income = 100,
   Housing = 100,
   Transportation = 100,
   Digital_Access = 100,
   Employment = 100
 )

 # ========== OVERVIEW TAB ==========

 output$stat_tracts <- renderText({
   format(nrow(df_final), big.mark = ",")
 })

 output$stat_vars <- renderText({
   length(analysis_vars)
 })

 output$distribution_chart <- renderPlotly({
   dist_data <- df_final %>%
     count(X10) %>%
     mutate(
       pct = n / sum(n) * 100,
       label = cluster_labels[as.character(X10)]
     )

   plot_ly(dist_data, x = ~X10, y = ~pct, type = "bar",
           marker = list(
             color = cluster_colors[as.character(dist_data$X10)],
             line = list(color = "white", width = 2)
           ),
           text = ~paste0(round(pct, 1), "%"),
           textposition = "outside",
           hoverinfo = "text",
           hovertext = ~paste0(
             "<b>Group ", X10, "</b><br>",
             label, "<br>",
             format(n, big.mark = ","), " tracts<br>",
             round(pct, 1), "%"
           )) %>%
     layout(
       xaxis = list(title = "", tickfont = list(size = 14, family = "Inter")),
       yaxis = list(title = "Percentage of Tracts", ticksuffix = "%",
                    tickfont = list(family = "Inter")),
       paper_bgcolor = "rgba(0,0,0,0)",
       plot_bgcolor = "rgba(0,0,0,0)",
       font = list(family = "Inter"),
       margin = list(t = 40)
     ) %>%
     config(displayModeBar = FALSE)
 })

 output$cluster_legend <- renderUI({
   tagList(
     lapply(LETTERS[1:10], function(ltr) {
       div(style = "display: flex; align-items: center; padding: 8px 0; border-bottom: 1px solid rgba(0,0,0,0.04);",
         span(class = paste0("cluster-badge cluster-", ltr), ltr),
         span(style = "margin-left: 12px; font-size: 13px;", cluster_labels[ltr])
       )
     })
   )
 })

 output$profile_heatmap <- renderPlotly({
   # Select key variables for profile
   key_vars <- c("pct_white", "pct_black", "pct_hispanic", "pct_bachelor",
                 "median_home_value", "pct_renter", "pop_density")
   key_vars <- key_vars[key_vars %in% names(df_final)]

   if (length(key_vars) == 0) return(NULL)

   # Calculate index scores by group
   profile_data <- df_final %>%
     select(X10, all_of(key_vars)) %>%
     group_by(X10) %>%
     summarise(across(everything(), ~mean(., na.rm = TRUE))) %>%
     pivot_longer(-X10, names_to = "variable", values_to = "value") %>%
     group_by(variable) %>%
     mutate(index = value / mean(value) * 100) %>%
     ungroup() %>%
     mutate(variable = str_replace_all(variable, "_", " ") %>% str_to_title())

   # Create heatmap
   plot_ly(profile_data,
           x = ~variable,
           y = ~X10,
           z = ~index,
           type = "heatmap",
           colorscale = list(
             c(0, "#D7191C"),
             c(0.25, "#FDAE61"),
             c(0.5, "#F5F5F5"),
             c(0.75, "#A6D96A"),
             c(1, "#1A9641")
           ),
           zmin = 50, zmax = 150,
           text = ~round(index),
           texttemplate = "%{text}",
           hoverinfo = "text",
           hovertext = ~paste0(
             "<b>Group ", X10, "</b><br>",
             variable, "<br>",
             "Index: ", round(index, 1)
           )) %>%
     layout(
       xaxis = list(title = "", tickangle = -45, tickfont = list(size = 11, family = "Inter")),
       yaxis = list(title = "", tickfont = list(size = 12, family = "Inter"), autorange = "reversed"),
       paper_bgcolor = "rgba(0,0,0,0)",
       plot_bgcolor = "rgba(0,0,0,0)",
       font = list(family = "Inter"),
       margin = list(b = 100)
     ) %>%
     colorbar(title = "Index", ticksuffix = "") %>%
     config(displayModeBar = FALSE)
 })

 # ========== WEIGHTING TAB ==========

 # Generate weight value outputs
 lapply(names(domains), function(domain) {
   output_id <- paste0("weight_val_", gsub(" |&", "_", domain))
   input_id <- paste0("weight_", gsub(" |&", "_", domain))

   output[[output_id]] <- renderText({
     paste0(input[[input_id]], "%")
   })
 })

 # Reset weights
 observeEvent(input$reset_weights, {
   lapply(names(domains), function(domain) {
     input_id <- paste0("weight_", gsub(" |&", "_", domain))
     updateSliderInput(session, input_id, value = 100)
   })
 })

 output$weighted_preview <- renderPlotly({
   # Placeholder chart showing current distribution
   dist_data <- df_final %>%
     count(X10) %>%
     mutate(pct = n / sum(n) * 100)

   plot_ly(dist_data, labels = ~X10, values = ~pct, type = "pie",
           marker = list(colors = cluster_colors[as.character(dist_data$X10)]),
           textinfo = "label+percent",
           textfont = list(family = "Inter", size = 14),
           hoverinfo = "text",
           hovertext = ~paste0("Group ", X10, ": ", round(pct, 1), "%")) %>%
     layout(
       paper_bgcolor = "rgba(0,0,0,0)",
       showlegend = FALSE,
       font = list(family = "Inter")
     ) %>%
     config(displayModeBar = FALSE)
 })

 output$weight_impact <- renderPlotly({
   # Show domain weights as bar chart
   weight_data <- tibble(
     domain = names(domains),
     weight = sapply(names(domains), function(d) {
       input_id <- paste0("weight_", gsub(" |&", "_", d))
       input[[input_id]] %||% 100
     }),
     color = sapply(domains, function(d) d$color)
   )

   plot_ly(weight_data, y = ~reorder(domain, weight), x = ~weight,
           type = "bar", orientation = "h",
           marker = list(color = ~color),
           text = ~paste0(weight, "%"),
           textposition = "outside") %>%
     layout(
       xaxis = list(title = "Weight (%)", range = c(0, 110), tickfont = list(family = "Inter")),
       yaxis = list(title = "", tickfont = list(family = "Inter")),
       paper_bgcolor = "rgba(0,0,0,0)",
       plot_bgcolor = "rgba(0,0,0,0)",
       font = list(family = "Inter"),
       margin = list(l = 120)
     ) %>%
     config(displayModeBar = FALSE)
 })

 # ========== EXPLORE TAB ==========

 # Update state choices based on data
 observe({
   if (DATA_LOADED && "NAME" %in% names(df_final)) {
     states <- df_final %>%
       mutate(state = str_extract(NAME, "[A-Za-z ]+$") %>% str_trim()) %>%
       pull(state) %>%
       unique() %>%
       sort()
     updateSelectInput(session, "filter_state",
                       choices = c("All States" = "all", setNames(states, states)))
   }
 })

 filtered_data <- reactive({
   data <- df_final

   # Filter by state
   if (input$filter_state != "all" && "NAME" %in% names(data)) {
     data <- data %>%
       filter(str_detect(NAME, paste0(input$filter_state, "$")))
   }

   # Filter by cluster
   if (input$filter_cluster != "all") {
     data <- data %>%
       filter(X10 == input$filter_cluster)
   }

   # Filter by population density
   if ("pop_density" %in% names(data)) {
     data <- data %>%
       filter(pop_density >= input$filter_pop_density[1],
              pop_density <= input$filter_pop_density[2])
   }

   data
 })

 output$table_count <- renderText({
   paste(format(nrow(filtered_data()), big.mark = ","), "tracts shown")
 })

 output$data_table <- renderDT({
   data <- filtered_data()

   # Select columns based on checkbox
   if ("all_vars" %in% input$display_cols) {
     cols <- names(data)
   } else {
     cols <- c("GEOID", "NAME", "X10")
     if ("demographics" %in% input$display_cols) {
       demo_cols <- c("pct_white", "pct_black", "pct_hispanic", "pct_asian")
       cols <- c(cols, demo_cols[demo_cols %in% names(data)])
     }
     if ("income" %in% input$display_cols) {
       inc_cols <- c("pct_income_200k_plus", "pct_income_under_10k", "median_home_value")
       cols <- c(cols, inc_cols[inc_cols %in% names(data)])
     }
     if ("housing" %in% input$display_cols) {
       house_cols <- c("pct_renter", "pct_vacant", "median_home_value")
       cols <- c(cols, house_cols[house_cols %in% names(data)])
     }
   }

   cols <- unique(cols[cols %in% names(data)])

   datatable(
     data[, cols, drop = FALSE],
     options = list(
       pageLength = 15,
       scrollX = TRUE,
       dom = 'frtip',
       language = list(search = "Search:")
     ),
     rownames = FALSE,
     class = "display"
   ) %>%
     formatRound(
       columns = cols[sapply(data[, cols], is.numeric)],
       digits = 1
     )
 })

 # ========== EXPORT TAB ==========

 export_data <- reactive({
   data <- df_final

   # Add state and county columns
   data <- data %>%
     mutate(
       state = str_extract(NAME, "[A-Za-z ]+$") %>% str_trim(),
       county = str_extract(NAME, "(?<=, )[^,]+(?=,)") %>% str_trim()
     ) %>%
     arrange(state, county, GEOID)

   # Add descriptions if selected
   if ("descriptions" %in% input$export_options) {
     data <- data %>%
       mutate(cluster_description = cluster_labels[as.character(X10)])
   }

   # Select cluster level
   if (input$export_cluster_level != "all") {
     cluster_cols <- c("GEOID", "NAME", "state", "county", input$export_cluster_level)
     if ("descriptions" %in% input$export_options) {
       cluster_cols <- c(cluster_cols, "cluster_description")
     }
     if ("all_vars" %in% input$export_options) {
       cluster_cols <- c(cluster_cols, analysis_vars[analysis_vars %in% names(data)])
     }
     data <- data %>% select(any_of(cluster_cols))
   }

   data
 })

 output$export_preview <- renderDT({
   datatable(
     head(export_data(), 10),
     options = list(
       pageLength = 10,
       scrollX = TRUE,
       dom = 't'
     ),
     rownames = FALSE,
     class = "display compact"
   )
 })

 output$export_summary <- renderUI({
   data <- export_data()
   div(
     p(style = "margin: 8px 0;",
       strong("Rows: "), format(nrow(data), big.mark = ",")),
     p(style = "margin: 8px 0;",
       strong("Columns: "), ncol(data)),
     p(style = "margin: 8px 0;",
       strong("States: "), length(unique(data$state))),
     p(style = "margin: 8px 0;",
       strong("File size: "), "~",
       round(object.size(data) / 1024 / 1024, 1), "MB (estimated)")
   )
 })

 # Download handlers
 output$download_xlsx <- downloadHandler(
   filename = function() {
     paste0("acorn_clusters_", Sys.Date(), ".xlsx")
   },
   content = function(file) {
     data <- export_data()

     if ("by_state" %in% input$export_options) {
       # Create workbook with sheets by state
       wb <- createWorkbook()

       states <- unique(data$state)
       for (st in states) {
         state_data <- data %>% filter(state == st)
         sheet_name <- substr(st, 1, 31)  # Excel sheet name limit
         addWorksheet(wb, sheet_name)
         writeData(wb, sheet_name, state_data)
       }

       saveWorkbook(wb, file)
     } else {
       write.xlsx(data, file)
     }
   }
 )

 output$download_csv <- downloadHandler(
   filename = function() {
     paste0("acorn_clusters_", Sys.Date(), ".csv")
   },
   content = function(file) {
     write_csv(export_data(), file)
   }
 )
}

# ============================================================================
# RUN APP
# ============================================================================

shinyApp(ui = ui, server = server)
