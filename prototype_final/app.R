source('global.R')

ui <- navbarPage(
  title = tagList(icon("bus"), "Transport Equity"),
  theme = shinytheme("flatly"),
  
  # Link the external CSS file
  tags$head(
    tags$style(HTML("
      .control-box {
        border: 1px solid #ccc;
        padding: 15px;
        background-color: #f9f9f9;
        box-shadow: 0 2px 5px rgba(0, 0, 0, 0.1);
        margin-bottom: 15px;
        border-radius: 12px;  /* Optional: rounded corners */
      }
      .control-box h3 {
        margin-top: 0;
        margin-bottom: 15px;
      }
      .tab-title-bold {
        font-weight: bold;
      }
    "))
  ),
  
  # Custom Accessibility Score Builder Tab
  tabPanel(
    "Custom Accessibility Score Builder",
    sidebarLayout(
      sidebarPanel(
        # Metric Builder Control Box
        div(
          class = "control-box",
          tags$h4(icon("sliders-h"), "Metric Builder", class = "text-primary fw-bold"),
          p("Increase the weight for a given factor to increase its relative importance in the accessibility score."),
          sliderInput("c1", "Weight for Total Travel Time", min = 0, max = 1, value = 0.33, step = 0.01),
          sliderInput("c2", "Weight for Walking Distance", min = 0, max = 1, value = 0.33, step = 0.01),
          sliderInput("c3", "Weight for Number of Transfers", min = 0, max = 1, value = 0.33, step = 0.01)
        ),
        
        # Day Type and Time Control Box
        div(
          class = "control-box",
          tags$h4(icon("calendar"), "Set Day Type and Time", class = "text-primary fw-bold"),
          selectInput("selected_day_type", "Day Type", 
                      choices = c("Select a day type..." = "", unique(api_data$`Day Type`)), 
                      selected = NULL),
          selectInput("selected_day_time", "Day Time", 
                      choices = c("Select a day time" = "", unique(api_data$`PeriodName`)), 
                      selected = NULL)
        ),
        
        # Planning Area Control Box
        div(
          class = "control-box",
          tags$h4(icon("map-marker-alt"), "Set Planning Area", class = "text-primary fw-bold"),
          selectInput("selected_planning_area", 
                      label = "Planning Area",
                      choices = c("Choose a planning area..." = "", unique(subzone_boundaries$Planning_Area)),
                      selected = NULL),
          actionButton("generateHeatmap", "Generate Heatmap", icon = icon("map")),
          hr(),
          helpText("Adjust weights to customise the Accessibility Score based on demographic profile."),
          helpText("Generate heatmaps to explore accessibility by area.")
        ),
      ),
      
      mainPanel(
        tabsetPanel(
          id = "heatTabs",
          tabPanel(
            title = tags$span(class = "tab-title-bold", "Singapore"),
            value = "tab_default",
            plotOutput("defaultHeatmap"),
            DTOutput("defaultTable")
          )
        )
      )
    )
  ),
  
  # Simulation Tab
  tabPanel(
    "Simulation",
    fluidPage(
      titlePanel("MRT Stop Simulation"),
      sidebarLayout(
        sidebarPanel(
          width = 4,
          
          # Map and Pin Controls
          div(
            class = "control-box",
            tags$h4(icon("map-marked-alt"), "Place New Station", class = "text-primary fw-bold"),
            leafletOutput("simulationMap", height = 400),
            tags$p("Click on the map to place a new MRT station"),
            actionButton("resetPin", "Reset Pin", icon = icon("undo")),
            hr(),
            div(class = 'control-box', id = "pinInfo", uiOutput("pinCoordinates"))
          ),
          
          # Station Type Input
          div(
            class = "control-box",
            tags$h4(icon("calendar"), "Set Station Type", class = "text-primary fw-bold"),
            selectInput("bus_or_mrt", "Select Bus or MRT", unique(simulation$first_type))
          ),
          
          # Accessibility Parameter Sliders
          div(
            class = "control-box",
            tags$h4(icon("sliders-h"), "Metric Builder", class = "text-primary fw-bold"),
            p("Increase the weight for a given factor to increase its relative importance in the accessibility score."),
            sliderInput("sim_c1", "Weight for Total Travel Time", min = 0, max = 1, value = 0.33, step = 0.01),
            sliderInput("sim_c2", "Weight for Walking Distance", min = 0, max = 1, value = 0.33, step = 0.01),
            sliderInput("sim_c3", "Weight for Number of Transfers", min = 0, max = 1, value = 0.33, step = 0.01)
          )
        ),
        
        # Main Panel with Simulation Results
        mainPanel(
          width = 8,
          tabsetPanel(
            id = "simulationResults",
            tabPanel(
              "Heatmap",
              br(),
              
              # Added Score Change Heatmap to Results Summary
              fluidRow(
                column(12, div(class = "control-box", 
                               h4("Accessibility Score Changes", style = "text-align: center;"),
                               plotOutput("scoreChangePlot", height = "500px"),
                               hr()
                ))
              )
            ),
            tabPanel(
              "Table Summary",
              br(),
              
              # Moved Detailed Changes Table to Impact Analysis
              fluidRow(
                column(12,
                       div(class = "control-box",
                           h4("Detailed Changes by Subzone"),
                           DTOutput("allChangesTable")
                       )
                )
              )
            )
          )
        )
      )
    )
  ),
  
  # About Tab
  tabPanel(
    "About",
    fluidPage(
      h3("About This Application"),
      p("The 'Visualising Transport Equity in Singapore' application was developed by 'Subway Surfers' to explore how well Singapore's public transport system ensures fair and equal access to mobility, specifically in terms of accessibility to public healthcare facilities."),
      
      
      h4("How To Use The Application:"),
      p("The application allows users to define their own accessibility criteria based on demographic needs. By adjusting the weight of various factors (such as travel time, walking distance, and number of transfers), users can create a metric that reflects the challenges faced by specific groups, like the elderly or individuals with mobility issues. The accessibility scores are then computed and visualized in heatmaps and tables across different regions of Singapore."),
      
      h4("Key Features:"),
      tags$ul(
        tags$li(strong("Custom Accessibility Score Builder:"), 
                " Create a personalized accessibility score based on the importance of considering different factors when evaluating accessibility: Total Travel Time, Total Walking Distance, and Number of Transfers, tailored to specific demographic needs."),
        tags$li(strong("Interactive Day Type and Time Settings:"), 
                " Focus your analysis to specific times and days, by selecting the desired settings in the sidebar."),
        tags$li(strong("Planning Area Selection:"), 
                " Focus your analysis by zooming into a specific planning area in Singapore to assess transportation accessibility to healthcare facilities in specific regions."),
        tags$li(strong("Simulation of MRT Station Placement:"), 
                " Simulate the impact of placing new MRT stations on accessibility scores, evaluating how transportation improvements affect access to healthcare."),
        tags$li(strong("Impact Analysis:"), 
                " Assess the overall impact of accessibility changes, helping identify the effectiveness of placing a new station at a particular location.")
      ),
      
      
      h4("Data Sources and Methodology:"),
      p('This tool relies on the OneMap Api for routing data.'),
      h4("Intended Users:"),
      p("This application is primarily designed for government agencies like the Land Transport Authority (LTA), urban planners, and researchers who are interested in improving public transport accessibility, especially to healthcare services. The tool allows them to make data-driven decisions that prioritize equitable access to healthcare services across all demographic groups."),
      
      h4("Why This Matters:"),
      p("Ensuring equitable access to healthcare is essential for fostering an inclusive society. Public transport plays a critical role in connecting people to healthcare facilities, especially for underserved and vulnerable populations. This tool provides valuable insights into the existing gaps in the public transport network and supports efforts to improve access, reduce disparities, and promote social equity.")
    )
  )
  
  
  
)




server <- function(input, output, session) {
  created_tabs <- reactiveVal(c("tab_default"))
  created_planning_areas <- reactiveVal(c())
  
  get_accessibility_data <- reactive({
    req(input$c1, input$c2, input$c3)
    
    if (input$selected_day_type != "") {
      api_data= api_data %>% filter(`Day Type` == input$selected_day_type)
    } else if (input$selected_day_time != '') {
      api_data= api_data %>% filter(`PeriodName` == input$selected_day_time)
    } 
    sum_weights=input$c1+input$c2+input$c3
    lambda=0.15
    api_data %>%
      mutate(acc_score = ( input$c1*(1-TT)+input$c2*(1-TWD)+input$c3*(1-TRF)  ) * (exp(-lambda*HD) ) /(sum_weights)  ) %>% # acc score to be modified
      group_by(Subzone, Hospital_Polyclinic) %>%
      slice_max(acc_score, n = 1) %>%
      unique() %>%
      ungroup() %>%
      group_by(Subzone) %>%
      mutate(accessibility_score = round(mean(acc_score),3)) %>%
      select(Subzone, accessibility_score) %>%
      distinct()
  })
  
  heatmap_data <- reactive({
    df <- get_accessibility_data()
    left_join(subzone_boundaries, df, by = "Subzone") 
    
  })
  
  output$defaultHeatmap <- renderPlot({
    df <- heatmap_data()
    
    # Get the subzone with the minimum accessibility score
    min_subzone <- df %>% filter(accessibility_score == min(accessibility_score, na.rm = TRUE)) %>% slice(1)
    min_centroid <- st_centroid(min_subzone$geometry)
    min_coords <- st_coordinates(min_centroid)
    
    bbox <- st_bbox(df$geometry)
    x_fixed <- bbox["xmax"] - 0.001
    y_fixed <- bbox["ymax"] - 0.001
    
    # Define annotation box position (bottom-right of the map)
    ann_x <- bbox["xmax"] - 0.02  # x-position of the label
    ann_y <- bbox["ymin"] + 0.02  # y-position of the label
    
    ggplot(df) +
      geom_sf(aes(fill = accessibility_score), color = 'white') +
      scale_fill_viridis_c(na.value = "grey") +
      labs( fill = "Accessibility Score") +
      theme_minimal() +
      coord_sf(expand = TRUE) +  # Prevent cutoffs at edges
      annotate("label",
               x = ann_x,  # Fix annotation at bottom-right
               y = ann_y,  # Ensure it's above the bottom edge
               label = paste("Lowest Score:\n", min_subzone$Subzone, "\n", round(min_subzone$accessibility_score, 2)),
               color = "black",
               fill = "white",
               fontface = "bold",
               family = "serif",
               size = 4,
               hjust = 1, vjust = 0) +  # Align the label to the bottom-right
      geom_segment(
        aes(
          x = min_coords[1],
          y = min_coords[2],
          xend = ann_x-0.1750 ,  # Arrow points to top-left of annotation box
          yend = ann_y + 0.045   # Arrow points to top-left of annotation box
        ),
        arrow = arrow(length = unit(0.2, "cm")),
        color = "black",
        curvature = 0.3
      ) +
      theme(
        axis.title.x = element_blank(),  # Remove axis title for x
        axis.title.y = element_blank(),  # Remove axis title for y
        axis.text.x = element_blank(),   # Remove x-axis labels
        axis.text.y = element_blank(),   # Remove y-axis labels
        axis.ticks = element_blank(),    # Remove axis ticks
        plot.title = element_text(
          size = 20,          # Title size
          hjust = 0.5,        # Center the title
          family = "serif",   # Font family (can be changed to others like "sans", etc.)
          face = "bold",      # Make the title bold
          color = "black",    # Title color
          margin = margin(b = 20)  # Add space below the title
        ),
        panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank()   # Remove minor gridlines
      )
    
  })
  
  
  output$defaultTable <- renderDT({
    df <- heatmap_data() %>%
      arrange(desc(accessibility_score)) %>%
      mutate(Subzone=str_to_title(Subzone)) %>%
      select(Subzone, accessibility_score) %>%
      st_drop_geometry()
    
    datatable(df, options = list(pageLength = 5))
  })
  
  observe({
    available_planning_areas <- setdiff(unique(subzone_boundaries$Planning_Area), created_planning_areas())
    updateSelectInput(session, "selected_planning_area",
                      choices = c("Choose a planning area..." = "", available_planning_areas))
  })
  
  observeEvent(input$generateHeatmap, {
    req(input$selected_planning_area)
    tab_id <- paste0(input$selected_planning_area)
    
    
    
    created_tabs(c(created_tabs(), tab_id))
    created_planning_areas(c(created_planning_areas(), input$selected_planning_area))
    
    appendTab("heatTabs",
              tabPanel(
                title = str_to_title(tab_id),
                value = tab_id,
                plotOutput(paste0(tab_id, "_map")),
                DTOutput(paste0(tab_id, "_table")),
                plotOutput(paste0(tab_id, "_age_plot")),
                actionButton(paste0(tab_id, "_delete"), "Delete Tab", icon = icon("trash"))
              ),
              select = TRUE
    )
    
    output[[paste0(tab_id, "_map")]] <- renderPlot({
      df <- heatmap_data() %>% filter(Planning_Area == tab_id)
      
      min_subzone <- df %>% filter(accessibility_score == min(accessibility_score, na.rm = TRUE)) %>% slice(1)
      min_centroid <- st_centroid(min_subzone$geometry)
      min_coords <- st_coordinates(min_centroid)
      
      bbox <- st_bbox(df$geometry)
      x_fixed <- bbox["xmax"] - 0.01
      y_fixed <- bbox["ymax"] - 0.01
      
      
      
      # Check if all accessibility_score values are NA
      if (all(is.na(df$accessibility_score))) {
        # If all are NA, fill the plot with grey
        ggplot(df) +
          geom_sf(fill = "grey", color = 'white') +  # Set all polygons to grey
          labs(title = "Accessibility Score by Subzone", fill = "Accessibility Score") +
          theme_minimal()
      } else {
        ggplot(df) +
          geom_sf(aes(fill = accessibility_score), color = 'white') +
          scale_fill_viridis_c(na.value = "grey") +
          labs(fill = "Accessibility Score") +
          theme_minimal() +
          coord_sf(expand = TRUE) +  # Prevent cutoffs at edges
          annotate("label",
                   x = min(x_fixed, bbox["xmax"] - 0.005),  # Ensure label stays within bounds
                   y = min(y_fixed, bbox["ymax"] - 0.005),
                   label = paste("Lowest Score:\n", min_subzone$Subzone, "\n", round(min_subzone$accessibility_score, 2)),
                   color = "black",
                   fill = "white",
                   fontface = "bold",
                   family = "serif",
                   size = 4,
                   hjust = 1, vjust = 1) +
          geom_segment(
            aes(
              x = min_coords[1],
              y = min_coords[2],
              xend = min(x_fixed, bbox["xmax"] - 0.005),
              yend = min(y_fixed, bbox["ymax"] - 0.005)
            ),
            arrow = arrow(length = unit(0.2, "cm")),
            color = "black",
            curvature = 0.3
          ) +
          theme(
            axis.title.x = element_blank(),  # Remove axis title for x
            axis.title.y = element_blank(),  # Remove axis title for y
            axis.text.x = element_blank(),   # Remove x-axis labels
            axis.text.y = element_blank(),   # Remove y-axis labels
            axis.ticks = element_blank(),    # Remove axis ticks
            plot.title = element_text(
              size = 20,          # Title size
              hjust = 0.5,        # Center the title
              family = "serif",   # Font family (can be changed to others like "sans", etc.)
              face = "bold",      # Make the title bold
              color = "black",    # Title color
              margin = margin(b = 20)  # Add space below the title
            ),
            panel.grid.major = element_blank(),  # Remove major gridlines
            panel.grid.minor = element_blank()   # Remove minor gridlines
          )
        
      }
      
      
      
      
      
    })
    
    output[[paste0(tab_id, "_table")]] <- renderDT({
      df <- heatmap_data() %>%
        filter(Planning_Area == tab_id) %>%
        arrange(desc(accessibility_score)) %>%
        mutate(Subzone=str_to_title(Subzone)) %>%
        select(Subzone, accessibility_score) %>%
        st_drop_geometry()
      
      datatable(df, options = list(pageLength = 5))
    })
    
    output[[paste0(tab_id, "_age_plot")]] <- renderPlot({
      
      area_age_data <- age_data %>% 
        filter(Planning_Area == str_to_title(tab_id)) 
      
      
      # Create bar chart
      ggplot(area_age_data, aes(x = Age_Group, y = Population, fill = Age_Group)) +
        geom_bar(stat = "identity", width = 0.7) +
        
        labs(
          title = paste("Population by Age Group -", str_to_title(tab_id)),
          x = "Age Group",
          y = "Population Count",
          fill = "Age Group"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
          legend.position = "none",
          panel.grid.major.x = element_blank(),
          axis.text = element_text(size = 11))
    })
    
    
    observeEvent(input[[paste0(tab_id, "_delete")]], {
      removeTab("heatTabs", tab_id)
      created_tabs(setdiff(created_tabs(), tab_id))
      created_planning_areas(setdiff(created_planning_areas(), input$selected_planning_area))
    })
  })
  
  observe({
    existing_tabs <- input$heatTabs
    closed_tabs <- setdiff(created_tabs(), c(existing_tabs, "tab_default"))
    if (length(closed_tabs) > 0) {
      created_tabs(setdiff(created_tabs(), closed_tabs))
    }
  })
  
  
  
  
  #SIMULATION TAB
  
  station_coords <- reactiveVal(data.frame(lng = NA, lat = NA))
  
  output$simulationMap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 103.8198, lat = 1.3521, zoom = 11)
  })
  
  observeEvent(input$simulationMap_click, {
    click <- input$simulationMap_click
    coords <- data.frame(lng = click$lng, lat = click$lat)
    station_coords(coords)
    
    leafletProxy("simulationMap") %>%
      clearMarkers() %>%
      addMarkers(lng = coords$lng, lat = coords$lat)
    
    output$pinCoordinates <- renderUI({
      HTML(paste0(
        "<b>Coordinates of New ", str_to_title(input$bus_or_mrt), " Station</b><br>",
        "Longitude: ", round(coords$lng, 3), "<br>",
        "Latitude: ", round(coords$lat, 3)
      ))
    })
    
  })
  
  observeEvent(input$resetPin, {
    station_coords(data.frame(lng = NA, lat = NA))
    output$pinCoordinates <- renderUI({ HTML("No station placed yet.") })
    leafletProxy("simulationMap") %>% clearMarkers()
  })
  
  
  observeEvent(input$generateHeatmap1, {
    req(input$selected_planning_area1)
    
    
    
  })
  
  get_accessibility_data_bef_sim <- reactive({
    
    df_bef_sim <- simulation
    
    sum_weights <- input$sim_c1 + input$sim_c2 + input$sim_c3
    lambda <- 0.15
    
    
    df_bef_sim %>%
      mutate(
        TWD = (TWD - min(TWD)) / (max(TWD) - min(TWD))
      ) %>%

      mutate(acc_score = ( input$sim_c1*(1-TT) + input$sim_c2*(1-TWD) + input$sim_c3*(1-TRF)  ) * (exp(-lambda*HD) ) /(sum_weights)  )%>%

      group_by(Subzone, Hospital_Polyclinic) %>%
      slice_max(acc_score, n = 1) %>%
      ungroup() %>%
      group_by(Subzone) %>%
      summarise(accessibility_score = round(mean(acc_score), 3),Planning_Area=Planning_Area) %>%
      distinct()
  })
  
  get_accessibility_data_aft_sim <- reactive({
    coords <- station_coords()
    req(!is.na(coords$lat), !is.na(coords$lng))
    df_aft_sim <- simulation
    
    sum_weights <- input$sim_c1 + input$sim_c2 + input$sim_c3
    lambda <- 0.15
    HD=3
    
    df_aft_sim %>%
      mutate(
        origin_to_newstop = haversine(Subzone_Lat, Subzone_Long, coords$lat, coords$lng),
        dest_to_newstop = haversine(Hospital_Polyclinic_Lat, Hospital_Polyclinic_Long, coords$lat, coords$lng)) %>%
      mutate(new_origin_to_firststop = ifelse(input$bus_or_mrt==first_type, pmin(origin_to_newstop, origin_to_firststop), origin_to_firststop)) %>%
      mutate(new_dest_to_laststop = ifelse(input$bus_or_mrt==last_type, pmin(dest_to_newstop, dest_to_laststop), dest_to_laststop)) %>%
      
      mutate(new_TWD = TWD*(new_origin_to_firststop+new_dest_to_laststop ) / (origin_to_firststop+dest_to_laststop))  %>%
      
      
      
      mutate(
        new_TWD = (new_TWD - min(new_TWD)) / (max(new_TWD) - min(new_TWD))) %>%
      
      
      mutate(acc_score = ( input$sim_c1*(1-TT) + input$sim_c2*(1-new_TWD) + input$sim_c3*(1-TRF)  ) * (exp(-lambda*HD) ) /(sum_weights)  )%>%

      group_by(Subzone, Hospital_Polyclinic) %>%
      slice_max(acc_score, n = 1) %>%
      ungroup() %>%
      group_by(Subzone) %>%
      summarise(accessibility_score = round(mean(acc_score), 3),Planning_Area=Planning_Area) %>%
      distinct()
  })
  
  output$before_sim <- renderPlot({
    heat1 <- left_join(subzone_boundaries, get_accessibility_data_bef_sim(), by = "Subzone")
    ggplot(heat1) +
      geom_sf(aes(fill = accessibility_score)) +
      scale_fill_viridis_c(na.value = "grey") +
      theme_minimal() +
      labs(title = "Before New MRT Station", fill = "Score")
  })
  
  output$after_sim <- renderPlot({
    heat2 <- left_join(subzone_boundaries, get_accessibility_data_aft_sim(), by = "Subzone")
    ggplot(heat2) +
      geom_sf(aes(fill = accessibility_score)) +
      scale_fill_viridis_c(na.value = "grey")+
      theme_minimal() +
      labs(title = "After New MRT Station", fill = "Score")
  })
  
  
  
  # 
  # output$comparisonTable <- renderDT({
  #   left_join(get_accessibility_data_bef_sim(), get_accessibility_data_aft_sim(), by = "Subzone", suffix = c("_before", "_after")) %>%
  #     filter(accessibility_score_before<accessibility_score_after)
  # })
  
  # Reactive to calculate score changes
  score_changes <- reactive({
    req(get_accessibility_data_bef_sim(), get_accessibility_data_aft_sim())
    
    before <- get_accessibility_data_bef_sim() %>% 
      select(Planning_Area,Subzone, accessibility_score) %>% 
      rename(before_score = accessibility_score)
    
    after <- get_accessibility_data_aft_sim() %>% 
      select(Planning_Area,Subzone, accessibility_score) %>% 
      rename(after_score = accessibility_score)
    
    full_join(before, after, by = "Subzone") %>%
      mutate(
        score_change = after_score - before_score,
        percent_change = 100*(after_score - before_score) / before_score ,
        change_direction = ifelse(score_change > 0, "Improved", "Declined")
      ) %>%
      arrange(desc(score_change))
  })
  
  # output$scoreChangePlot <- renderPlot({
  #   changes <- score_changes() %>%
  #     left_join(subzone_boundaries, by = "Subzone") %>%
  #     st_as_sf()
  #   
  #   ggplot(changes) +
  #     geom_sf(aes(fill = score_change), color = NA) +
  #     scale_fill_gradient2(low = "red", mid = "white", high = "blue", 
  #                          midpoint = 0, na.value = "grey") +
  #     labs(
  #       title = "Accessibility Score Changes After New Station",
  #       fill = "Score Change"
  #     ) +
  #     theme_minimal() +
  #     theme(legend.position = "bottom")
  # })
  
  output$scoreChangePlot <- renderPlot({
    # Get all subzones with change data
    relevant_planning_areas=subzone_boundaries %>%
      mutate(geometry = st_make_valid(geometry))%>%
      left_join(score_changes(), by = "Subzone")%>%
      group_by(Planning_Area)%>%
      summarise(score_changes_by_planning_area = mean(score_change, na.rm = TRUE)) %>%
      filter(score_changes_by_planning_area>0)
    
    
    
    plot_data <- subzone_boundaries %>%
      left_join(score_changes(), by = "Subzone")  %>%
      filter(Planning_Area %in% relevant_planning_areas$Planning_Area)
    
    
    
    ggplot() +
      # Base layer: All subzones (grey)
      geom_sf(data = plot_data, fill = "#f0f0f0", color = "white", size = 0.1) +
      
      # Overlay: Only subzones with changes
      geom_sf(
        data = filter(plot_data, !is.na(score_change)),
        aes(fill = score_change), 
        color = "white", 
        size = 0.2
      ) +
      
      # Gradient for changed areas
      scale_fill_gradient2(
        low = "#e74c3c",  # Red for decline
        mid = "#f8f9fa",  # White for midpoint
        high = "#2ecc71", # Green for improvement
        midpoint = 0,
        name = "Score Change"
      ) +
      
      labs(
        title = paste0("Impact of New ", input$bus_or_mrt,' Station')
      ) +
      theme_void() +
      theme(
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.text = element_text(angle = 45, hjust = 1, vjust = 1),
        legend.key.height = unit(0.5, "cm")
      )
  })
  
  
  
  # output$scoreScatterPlot <- renderPlot({
  #   changes <- score_changes()
  #   
  #   ggplot(changes, aes(x = before_score, y = after_score, color = score_change)) +
  #     geom_point(alpha = 0.7, size = 3) +
  #     geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
  #     scale_color_gradient2(low = "#e74c3c", mid = "#f8f9fa", high = "#2ecc71", midpoint = 0) +
  #     labs(
  #       x = "Original Accessibility Score",
  #       y = "New Accessibility Score",
  #       title = "Score Changes: Before vs. After Simulation",
  #       color = "Change"
  #     ) +
  #     theme_minimal() +
  #     theme(legend.position = "bottom")
  # })
  
  # Histogram of changes (Keep this)
  # output$changeHistogram <- renderPlot({
  #   changes <- score_changes()
  #   
  #   ggplot(changes, aes(x = score_change, fill = ifelse(score_change > 0, "Improved", "Declined"))) +
  #     geom_histogram(bins = 30, color = "white") +
  #     scale_fill_manual(values = c("Improved" = "#2ecc71", "Declined" = "#e74c3c")) +
  #     labs(
  #       x = "Score Change", 
  #       y = "Number of Subzones", 
  #       title = "Distribution of Accessibility Score Changes"
  #     ) +
  #     theme_minimal() +
  #     theme(
  #       legend.position = "none",
  #       plot.title = element_text(hjust = 0.5, face = "bold"),
  #       panel.grid.minor = element_blank()
  #     )
  # })
  
  
  # Detailed changes table (Keep this,)
  output$allChangesTable <- renderDT({
    score_changes() %>%
      select(Subzone, before_score, after_score, score_change, percent_change) %>%
      mutate(Subzone = str_to_title(Subzone)) %>%
      arrange(desc(abs(score_change))) %>%  # Sort by magnitude of change
      filter(score_change>0) %>%
      datatable(
        options = list(
          pageLength = 10,
          dom = 'Blfrtip',
          buttons = c('copy', 'csv', 'excel'),
          lengthMenu = list(c(10, 25, 50, -1), c('10', '25', '50', 'All'))
        ),
        rownames = FALSE,
        extensions = 'Buttons',
        colnames = c('Subzone', 'Before', 'After', 'Change', '% Change'),
        caption = htmltools::tags$caption(
          style = 'caption-side: top; text-align: center;',
          'Table: Accessibility score changes after adding new station'
        )
      ) %>%
      formatRound(columns = c('before_score', 'after_score', 'score_change'), digits = 3) %>%
      formatRound(columns = 'percent_change', digits = 1) %>%
      formatStyle(
        'score_change',
        color = styleInterval(c(0), c('#e74c3c', '#2ecc71')),
        fontWeight = 'bold'
      ) %>%
      formatStyle(
        'percent_change',
        color = styleInterval(c(0), c('#e74c3c', '#2ecc71'))
      )
  })
}

shinyApp(ui, server)
