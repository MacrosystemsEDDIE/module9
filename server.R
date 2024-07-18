shinyServer(function(input, output, session) {
  
  #### Presentation ----

  #** Recap Presentation slides ----
  output$slides <- renderSlickR({
    slickR(recap_slides) + settings(dots = TRUE)
  })
  
  #### Introduction ----
  # Hide download button until report is generated
  output$handoutbuilt <- reactive({
    return(file.exists("report.docx"))
  })
  outputOptions(output, 'handoutbuilt', suspendWhenHidden= FALSE)
  
  handout_file <- "Student_handout.docx"
  
  output$stud_dl <-  downloadHandler(
    filename = function() {
      handout_file
    },
    content = function(file) {
      file.copy("report.docx", file)
    }
  )
  
  #### Activity A
  
  #### Objective 1 ----
  
  # LTREB Sites datatable ----
  output$table01 <- DT::renderDT(
    sites_df[, c(1:2)], selection = "single", options = list(stateSave = TRUE, dom = 't'), server = FALSE
  )
  
  observe({
    if(input$row_num != "") {
      dt_proxy <- dataTableProxy("table01")
      selectRows(dt_proxy, input$row_num)
    }
  })
  
  # to keep track of previously selected row
  prev_row <- reactiveVal()
  siteID <- reactiveValues(lab = NULL)
  
  # new icon style
  my_icon = makeAwesomeIcon(icon = 'flag', markerColor = 'red', iconColor = 'white')
  
  
  # Select NEON DT rows ----
  lake_data <- reactiveValues(df = NULL,
                              wtemp = NULL,
                              do = NULL,
                              turb = NULL)
  site_photo_file <- reactiveValues(img = NULL)

  observeEvent(input$table01_rows_selected, {
    
    siteID$lab <- input$table01_rows_selected
    
    row_selected = sites_df[input$table01_rows_selected, ]
    proxy <- leafletProxy('ltrebmap')
    proxy %>%
      addAwesomeMarkers(layerId = as.character(row_selected$SiteID),
                        lng=row_selected$Longitude,
                        lat=row_selected$Latitude,
                        icon = my_icon)
    
    # Reset previously selected marker
    if(!is.null(prev_row()))
    {
      proxy %>%
        removeMarker(layerId = as.character(prev_row()$SiteID))
    }
    # set new value to reactiveVal
    prev_row(row_selected)
    
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = "Loading LTREB data",
                 detail = "This may take a while. This window will disappear
                     when it is loaded.", value = 0.33)
    
    #load LTREB data
    lake_data$df <- reservoir_data %>%
      filter(site_id == pull(sites_df[input$table01_rows_selected, "SiteID"]),
             variable %in% c("Temp_C_mean","DO_mgL_mean","Turbidity_FNU_mean"),
             !(variable == "DO_mgL_mean" & depth_m >= 2 & depth_m <= 8)) %>% # remove metalimnetic DO
      mutate(observation = round(observation, 1),
             depth_ft = round(depth_m*3.28,1))
    
    #retrieve site photooutput$display.image <- renderImage({
    site_photo_file$img <- paste("www/",row_selected$SiteID,".jpg",sep="")

    #show site info
    output$site_info <- renderText({
      module_text[row_selected$SiteID, ]
    })
    
    #pull recent data
    lake_data$wtemp <- lake_data$df %>%
      select(datetime, variable, depth_m, depth_ft, observation) %>%
      filter(variable == "Temp_C_mean")
    
    lake_data$do <- lake_data$df %>%
      select(datetime, variable, depth_m, depth_ft, observation) %>%
      filter(variable == "DO_mgL_mean" & (depth_m <= 2 | depth_m >= 8)) # remove metalimnion
    
    lake_data$turb <- lake_data$df %>%
      select(datetime, variable, depth_ft, observation) %>%
      filter(variable == "Turbidity_FNU_mean")
    
    progress$set(value = 1)
    
  })
  
  # LTREB map ----
  output$ltrebmap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addMarkers(data = sites_df,
                 layerId = ~SiteID, clusterOptions = markerClusterOptions(),
                 label = ~ReservoirName, icon = ~ltrebIcons[1])
    
  })
  
  # Show reservoir image ----
  output$site_photo <- renderImage({
    
    validate(
      need(input$table01_rows_selected != "",
           message = "Please select a site in the table.")
    )
    list(src = site_photo_file$img,
         alt = "Image failed to render.",
         height = 320,
         width = 400)
  }, deleteFile = FALSE)
  
  #### Objective 2 ----
  
  #** water temperature Presentation slides ----
  output$wtemp_slides <- renderSlickR({
    slickR(wtemp_slides) + settings(dots = TRUE)
  })
  
  # Plot water temperature
  plot.wtemp <- reactiveValues(main=NULL)
  
  observe({
    
    output$wtemp_plot <- renderPlotly({ 
      
      validate(
        need(input$table01_rows_selected != "",
             message = "Please select a site in Objective 1.")
      )
      validate(
        need(!is.null(lake_data$df),
             message = "Please select a site in Objective 1.")
      )
      validate(
        need(input$plot_wtemp > 0,
             message = "Click 'Plot water temperature'")
      )
      
      df <- lake_data$wtemp %>%
        mutate(depth_ft = as.factor(depth_ft))
      
      palette_yb <- colorRampPalette(colors = c("#A9A448", "#023858"))(13)
      
      
      p <- ggplot(data = df, aes(x = datetime, y = observation, group = depth_ft, color = depth_ft))+
        geom_line()+
        xlab("")+
        ylab("Water temperature (degrees Celsius)")+
        scale_color_manual(name = "Depth (ft)",values = palette_yb)+
        ylim(c(0,35))+
        theme_bw()
      
      plot.wtemp$main <- p
      
      return(ggplotly(p, dynamicTicks = TRUE, tooltip=c("x", "y", "color")))
      
    })
    
  })
  
  # Download plot of water temperature
  output$save_wtemp_plot <- downloadHandler(
    filename = function() {
      paste("Q8-10-plot-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = 8, height = 4,
                       res = 200, units = "in")
      }
      ggsave(file, plot = plot.wtemp$main, device = device)
    }
  )
  
  #** dissolved oxygen Presentation slides ----
  output$do_slides <- renderSlickR({
    slickR(do_slides) + settings(dots = TRUE)
  })
  
  # Plot dissolved oxygen
  plot.do <- reactiveValues(main=NULL)
  
  observe({
    
    output$do_plot <- renderPlotly({ 
      
      validate(
        need(input$table01_rows_selected != "",
             message = "Please select a site in Objective 1.")
      )
      validate(
        need(!is.null(lake_data$df),
             message = "Please select a site in Objective 1.")
      )
      validate(
        need(input$plot_do > 0,
             message = "Click 'Plot dissolved oxygen'")
      )
      
      df <- lake_data$do %>%
        mutate(depth_ft = as.factor(depth_ft),
               layer = ifelse(depth_m <= 2, "surface waters","bottom waters")) %>%
        mutate(layer = factor(layer, levels = c("surface waters","bottom waters")))
      
      p <- ggplot(data = df, aes(x = datetime, y = observation, group = layer, color = layer))+
        geom_line()+
        xlab("")+
        ylab("Dissolved oxygen (ppm)")+
        scale_color_manual(name = "Depth", values = c("#A9A448", "#023858"))+
        theme_bw()
      
      plot.do$main <- p
      
      return(ggplotly(p, dynamicTicks = TRUE, tooltip=c("x", "y", "color")))
      
    })
    
  })
  
  # Download plot of DO
  output$save_do_plot <- downloadHandler(
    filename = function() {
      paste("Q12-14-plot-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = 8, height = 4,
                       res = 200, units = "in")
      }
      ggsave(file, plot = plot.do$main, device = device)
    }
  )
  
  #** turbidity Presentation slides ----
  output$turb_slides <- renderSlickR({
    slickR(turb_slides) + settings(dots = TRUE)
  })
  
  # Plot turbidity
  plot.turb <- reactiveValues(main=NULL)
  
  observe({
    
    output$turb_plot <- renderPlotly({ 
      
      validate(
        need(input$table01_rows_selected != "",
             message = "Please select a site in Objective 1.")
      )
      validate(
        need(!is.null(lake_data$df),
             message = "Please select a site in Objective 1.")
      )
      validate(
        need(input$plot_turb > 0,
             message = "Click 'Plot turbidity'")
      )
      
      df <- lake_data$turb 
      
      p <- ggplot(data = df, aes(x = datetime, y = observation))+
        geom_point(aes(color = "surface water turbidity"))+
        xlab("")+
        ylab("Turbidity (FNU)")+
        scale_color_manual(values = c("surface water turbidity" = "#A9A448"), name = "")+
        theme_bw()
      
      plot.turb$main <- p
      
      return(ggplotly(p, dynamicTicks = TRUE, tooltip=c("x", "y", "color")))
      
    })
    
  })
  
  # Download plot of turbidity
  output$save_turb_plot <- downloadHandler(
    filename = function() {
      paste("Q17-plot-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = 8, height = 4,
                       res = 200, units = "in")
      }
      ggsave(file, plot = plot.turb$main, device = device)
    }
  )
  
  #### Activity B ----
  
  # Output potential extraction depths
  observe({
    
    output$extraction_depths <- renderUI({
      
      validate(
        need(!is.null(lake_data$df),
             message = "Please select a site in Activity A.")
      )
      
      site = pull(sites_df[input$table01_rows_selected, "SiteID"])
      
      if(site == "fcre"){
        extraction_depths <- paste("<b>","Possible extraction depths: 5.2 ft and 29.5 ft.","</b>", sep = "")
      }
      if(site == "bvre"){
        extraction_depths <- paste("<b>","Possible extraction depths: 4.9 ft and 42.7 ft.","</b>", sep = "")
      }
      

      HTML(paste(extraction_depths))
    })
    
  })
  
  # Plot summertime data
  plot.summer.data <- reactiveValues(main=NULL)
  
  observe({
    
    output$summer_data_plot <- renderPlotly({ 
      
      validate(
        need(!is.null(lake_data$df),
             message = "Please select a site in Activity A")
      )
      validate(
        need(input$plot_summer_data > 0,
             message = "Click 'Plot summer data'")
      )
      
      df <- lake_data$df %>%
        filter(month(datetime) == 7) %>%
        mutate(depth_ft = as.factor(depth_ft),
               observation = ifelse(variable == "Temp_C_mean",round(observation*(9/5) + 32,1),observation))
      
      # end_july <- df %>%
      #   mutate(day = format(as.Date(datetime), "%m-%d")) %>%
      #   filter(day == "07-31" & depth_m == 0.1) 
      
      # New facet label names for variables
      var.labs <- c("Water temperature (degrees Fahrenheit)","Dissolved oxygen (ppm)","Turbidity (NTU)")
      names(var.labs) <- unique(df$variable)
      
      palette_yb <- colorRampPalette(colors = c("#A9A448", "#023858"))(13)
      
      p <- ggplot(data = df, aes(x = datetime, y = observation, group = depth_ft, color = depth_ft))+
        geom_line()+
        xlab("")+
        ylab("")+
        facet_wrap(vars(variable), nrow = 3, scales = "free_y", 
                   labeller = labeller(variable = var.labs), strip.position = "top")+
        #geom_vline(data = end_july, aes(xintercept = as.numeric(end_july)))+
        scale_color_manual(name = "Depth (ft)", values = palette_yb)+
        ggtitle("Summer water quality data")+
        theme_bw()
      
      plot.summer.data$main <- p
      
      return(ggplotly(p, dynamicTicks = TRUE, tooltip=c("x", "y", "color")) %>% 
               layout(height = 700, width = 800))
      
    })
    
  })
  
  # Download plot of summer data
  output$save_summer_data_plot <- downloadHandler(
    filename = function() {
      paste("Q19-plot-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = 8, height = 4,
                       res = 200, units = "in")
      }
      ggsave(file, plot = plot.summer.data$main, device = device)
    }
  )
  
  # Plot fall data
  plot.fall.data <- reactiveValues(main=NULL)
  
  observe({
    
    output$fall_data_plot <- renderPlotly({ 
      
      validate(
        need(!is.null(lake_data$df),
             message = "Please select a site in Activity A")
      )
      validate(
        need(input$plot_fall_data > 0,
             message = "Click 'Plot fall data'")
      )
      
      df <- lake_data$df 
      
      turnover_date <- df %>%
        filter(variable == "Temp_C_mean") %>%
        group_by(datetime) %>%
        mutate(temp_diff = ifelse(range(observation, na.rm = TRUE)[2] - range(observation, na.rm = TRUE)[1] < 1,1,NA)) %>%
        ungroup() %>%
        filter(month(datetime) %in% c(7:12) & !is.na(temp_diff)) %>%
        slice(first(temp_diff)) %>%
        pull(datetime)
      
      fall_dates <- seq.Date(from = as.Date(turnover_date) - 15, to = as.Date(turnover_date) + 15, by = 'days')

      
      plot_data <- df %>%
        filter(datetime %in% fall_dates) %>%
        mutate(depth_ft = as.factor(depth_ft),
               observation = ifelse(variable == "Temp_C_mean",round(observation*(9/5) + 32,1),observation))
      
      # New facet label names for variables
      var.labs <- c("Water temperature (degrees Fahrenheit)","Dissolved oxygen (ppm)","Turbidity (NTU)")
      names(var.labs) <- unique(df$variable)
      
      palette_yb <- colorRampPalette(colors = c("#A9A448", "#023858"))(13)
      
      p <- ggplot(data = plot_data, aes(x = datetime, y = observation, group = depth_ft, color = depth_ft))+
        geom_line()+
        xlab("")+
        ylab("")+
        facet_wrap(vars(variable), nrow = 3, scales = "free_y", 
                   labeller = labeller(variable = var.labs), strip.position = "top")+
        scale_color_manual(name = "Depth (ft)", values = palette_yb)+
        ggtitle("Fall water quality data")+
        theme_bw()
      
      plot.fall.data$main <- p
      
      return(ggplotly(p, dynamicTicks = FALSE, tooltip=c("x", "y", "color")) %>% layout(height = 700, width = 800))
      
    })
    
  })
  
  # Download plot of fall data
  output$save_fall_data_plot <- downloadHandler(
    filename = function() {
      paste("Q24-plot-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = 8, height = 4,
                       res = 200, units = "in")
      }
      ggsave(file, plot = plot.fall.data$main, device = device)
    }
  )
  
  # Plot winter data
  plot.winter.data <- reactiveValues(main=NULL)
  
  observe({
    
    output$winter_data_plot <- renderPlotly({ 
      
      validate(
        need(!is.null(lake_data$df),
             message = "Please select a site in Activity A")
      )
      validate(
        need(input$plot_winter_data > 0,
             message = "Click 'Plot winter data'")
      )
      
      df <- lake_data$df %>%
        filter(month(datetime) == 1) %>%
        mutate(depth_ft = as.factor(depth_ft),
               observation = ifelse(variable == "Temp_C_mean",round(observation*(9/5) + 32,1),observation))
      
      # New facet label names for variables
      var.labs <- c("Water temperature (degrees Fahrenheit)","Dissolved oxygen (ppm)","Turbidity (NTU)")
      names(var.labs) <- unique(df$variable)
      
      palette_yb <- colorRampPalette(colors = c("#A9A448", "#023858"))(13)
      
      p <- ggplot(data = df, aes(x = datetime, y = observation, group = depth_ft, color = depth_ft))+
        geom_line()+
        xlab("")+
        ylab("")+
        facet_wrap(vars(variable), nrow = 3, scales = "free_y", 
                   labeller = labeller(variable = var.labs), strip.position = "top")+
        scale_color_manual(name = "Depth (ft)", values = palette_yb)+
        ggtitle("Winter water quality data")+
        theme_bw()
      
      plot.winter.data$main <- p
      
      return(ggplotly(p, dynamicTicks = FALSE, tooltip=c("x", "y", "color")) %>% layout(height = 700, width = 800))
      
    })
    
  })
  
  # Download plot of winter data
  output$save_winter_data_plot <- downloadHandler(
    filename = function() {
      paste("Q30-plot-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = 8, height = 4,
                       res = 200, units = "in")
      }
      ggsave(file, plot = plot.winter.data$main, device = device)
    }
  )
  
  # forecasting slides
  output$forecast_slides <- renderSlickR({
    slickR(forecast_slides) + settings(dots = TRUE)
  })
  
  # Plot forecast for practicing interpretation
  plot.fc <- reactiveValues(main=NULL)
  
  observe({
    
    output$fc_plot <- renderPlotly({ 
      
      validate(
        need(input$plot_fc > 0,
             message = "Click 'View turnover forecast'")
      )
      
      df <- forecast_data %>%
        filter(fc_id == "second_forecast") %>%
        mutate(perc_turnover = prob_turnover*100)
      
      # Text 
      ann_text <- data.frame(datetime = as.Date(c("2023-10-12")),
                             perc_turnover = c(80),
                             lab = "today")
      
      p <- ggplot(data = df, aes(x = datetime, y = perc_turnover))+
        geom_line(aes(color = "percent chance of turnover"), linewidth = 1)+
        geom_vline(xintercept = as.numeric(as.Date("2023-10-13")), linetype = 2)+
        ggtitle("Turnover forecast")+
        xlab("")+
        ylab("Percent chance of turnover (%)")+
        ylim(c(0,100))+
        scale_color_manual(values = c("percent chance of turnover" = "#8EB1AF"), name = "")+
        geom_text(data = ann_text, aes(x = datetime, y = perc_turnover, label = lab))+
        theme_bw()
      
      plot.fc$main <- p
      
      return(ggplotly(p, dynamicTicks = TRUE, tooltip=c("x", "y", "color")))
      
    })
    
  })
  
  # Download plot of second forecast
  output$save_fc_plot <- downloadHandler(
    filename = function() {
      paste("Q17-plot-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = 8, height = 4,
                       res = 200, units = "in")
      }
      ggsave(file, plot = plot.fc$main, device = device)
    }
  )
  
  
  #### Activity C ----
  
  # Plot real-time water temperature data for the first time period
  plot.realtime.wtemp.1 <- reactiveValues(main=NULL)
  
  observe({
    
    output$realtime_wtemp_plot_1 <- renderPlotly({ 
    
      validate(
        need(input$plot_realtime_data_1 > 0,
             message = "Click 'Plot real-time data'")
      )
      
      realtime.dates <- seq.Date(from = as.Date("2023-10-06") - 30, to = as.Date("2023-10-06"), by = 'days')
      
      df <- realtime_fcr_data %>%
        filter(variable == "Temp_C_mean") %>%
        mutate(depth_ft = as.factor(depth_ft)) %>%
        rename(wq.variable = variable) %>%
        filter(datetime %in% realtime.dates) %>%
        mutate(observation = round(observation*(9/5) + 32,1))
      
      palette_yb <- colorRampPalette(colors = c("#A9A448", "#023858"))(13)
      
      # Text 
      ann_text <- data.frame(datetime = as.Date(c("2023-10-05")),
                             observation = c(80),
                             lab = "today",
                             wq.variable = factor(unique(df$wq.variable)))
      
      p <- ggplot()+
        geom_line(data = df, aes(x = datetime, y = observation, group = depth_ft, color = depth_ft))+
        xlab("")+
        ylab("Water temperature (degrees Fahrenheit)")+
        scale_color_manual(name = "Depth (ft)",values = palette_yb)+
        geom_vline(xintercept = as.numeric(as.Date("2023-10-06")), linetype = 2, linewidth = 1)+
        geom_text(data = ann_text, aes(x = datetime, y = observation, label = lab))+
        theme_bw()
      
      plot.realtime.wtemp.1$main <- p
      
      return(ggplotly(p, dynamicTicks = TRUE, tooltip=c("x", "y", "color")))
      
    })
    
  })
  
  # Download plot of water temperature
  output$save_realtime_wtemp_plot_1 <- downloadHandler(
    filename = function() {
      paste("Q8-10-plot-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = 8, height = 4,
                       res = 200, units = "in")
      }
      ggsave(file, plot = plot.realtime.wtemp.1$main, device = device)
    }
  )
  
  # Message re: turbidity threshold
  output$turb_message1 <- renderText({"The horizontal line indicates the raw water turbidity threshold of 20 NTU."})
  
  # Plot realtime wq data for first time period
  plot.realtime.data.1 <- reactiveValues(main=NULL)
  
  observe({
    
    output$realtime_data_1_plot <- renderPlotly({ 
      
      validate(
        need(input$plot_realtime_data_1 > 0,
             message = "Click 'Plot real-time data'")
      )
      
      realtime.dates <- seq.Date(from = as.Date("2023-10-06") - 30, to = as.Date("2023-10-06"), by = 'days')
      
      df <- realtime_fcr_data %>%
        filter(variable %in% c("DO_mgL_mean","Turbidity_FNU_mean")) %>%
        mutate(depth_ft = as.factor(depth_ft),
               layer = ifelse(depth_m <= 2, "surface waters","bottom waters")) %>%
        mutate(layer = factor(layer, levels = c("surface waters","bottom waters"))) %>%
        filter(datetime %in% realtime.dates) %>%
        rename(wq.variable = variable)
      
      # New facet label names for variables
      var.labs <- c("Dissolved oxygen (ppm)","Turbidity (NTU)")
      names(var.labs) <- unique(df$wq.variable)
      
      # Text for facets
      ann_text <- data.frame(datetime = as.Date(c("2023-10-04","2023-10-04")),
                             observation = c(10,7),
                             lab = rep("today", times = 2),
                             wq.variable = factor(unique(df$wq.variable)))
      
      p <- ggplot()+
        geom_line(data = df, aes(x = datetime, y = observation, group = layer, color = layer))+
        xlab("")+
        ylab("")+
        geom_hline(data = df %>% filter(wq.variable == "Turbidity_FNU_mean"),
                   aes(yintercept = 20), col = "#8A5F50")+
        facet_wrap(vars(wq.variable), nrow = 1, scales = "free_y", 
                   labeller = labeller(wq.variable = var.labs), strip.position = "top")+
        scale_color_manual(name = "Depth", values = c("#A9A448", "#023858"))+
        ggtitle("Real-time water quality data")+
        geom_vline(xintercept = as.numeric(as.Date("2023-10-06")), linetype = 2)+
        geom_text(data = ann_text, aes(x = datetime, y = observation, label = lab))+
        theme_bw()
      
      plot.realtime.data.1$main <- p
      
      return(ggplotly(p, dynamicTicks = FALSE, tooltip=c("x", "y", "color")) %>% layout(height = 400, width = 1200))
      
    })
    
  })
  
  # Download plot of real-time data
  output$save_realtime_data_plot_1 <- downloadHandler(
    filename = function() {
      paste("Q38-39-plot-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = 8, height = 3,
                       res = 300, units = "in")
      }
      ggsave(file, plot = plot.realtime.data.1$main, device = device)
    }
  )
  
  # Plot forecast for first time period
  plot.fc1 <- reactiveValues(main=NULL)
  
  observe({
    
    output$fc1_plot <- renderPlotly({ 
      
      validate(
        need(input$plot_fc1 > 0,
             message = "Click 'View turnover forecast'")
      )
      
      df <- forecast_data %>%
        filter(fc_id == "first_forecast") %>%
        mutate(perc_turnover = prob_turnover*100)
      
      # Text 
      ann_text <- data.frame(datetime = as.Date(c("2023-10-05")),
                             perc_turnover = c(80),
                             lab = "today")
      
      p <- ggplot(data = df, aes(x = datetime, y = perc_turnover))+
        geom_line(aes(color = "percent chance of turnover"), linewidth = 1)+
        geom_vline(xintercept = as.numeric(as.Date("2023-10-06")), linetype = 2)+
        xlab("")+
        ylab("Percent chance of turnover (%)")+
        ggtitle("Turnover forecast")+
        ylim(c(0,100))+
        scale_color_manual(values = c("percent chance of turnover" = "#8EB1AF"), name = "")+
        geom_text(data = ann_text, aes(x = datetime, y = perc_turnover, label = lab))+
        theme_bw()
      
      plot.fc1$main <- p
      
      return(ggplotly(p, dynamicTicks = TRUE, tooltip=c("x", "y", "color")))
      
    })
    
  })
  
  # Download plot of first forecast
  output$save_fc1_plot <- downloadHandler(
    filename = function() {
      paste("Q17-plot-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = 8, height = 4,
                       res = 200, units = "in")
      }
      ggsave(file, plot = plot.fc1$main, device = device)
    }
  )
  
  # Plot real-time water temperature data for the second time period
  plot.realtime.wtemp.2 <- reactiveValues(main=NULL)
  
  observe({
    
    output$realtime_wtemp_plot_2 <- renderPlotly({ 
      
      validate(
        need(input$plot_realtime_data_2 > 0,
             message = "Click 'Plot real-time data'")
      )
      
      realtime.dates <- seq.Date(from = as.Date("2023-10-13") - 30, to = as.Date("2023-10-13"), by = 'days')
      
      df <- realtime_fcr_data %>%
        filter(variable == "Temp_C_mean") %>%
        mutate(depth_ft = as.factor(depth_ft)) %>%
        rename(wq.variable = variable) %>%
        filter(datetime %in% realtime.dates) %>%
        mutate(observation = round(observation*(9/5) + 32,1))
      
      palette_yb <- colorRampPalette(colors = c("#A9A448", "#023858"))(13)
      
      # Text 
      ann_text <- data.frame(datetime = as.Date(c("2023-10-12")),
                             observation = c(80),
                             lab = "today",
                             wq.variable = factor(unique(df$wq.variable)))
      
      p <- ggplot()+
        geom_line(data = df, aes(x = datetime, y = observation, group = depth_ft, color = depth_ft))+
        xlab("")+
        ylab("Water temperature (degrees Fahrenheit)")+
        scale_color_manual(name = "Depth (ft)",values = palette_yb)+
        geom_vline(xintercept = as.numeric(as.Date("2023-10-13")), linetype = 2, linewidth = 1)+
        geom_text(data = ann_text, aes(x = datetime, y = observation, label = lab))+
        theme_bw()
      
      plot.realtime.wtemp.2$main <- p
      
      return(ggplotly(p, dynamicTicks = TRUE, tooltip=c("x", "y", "color")))
      
    })
    
  })
  
  # Download plot of water temperature
  output$save_realtime_wtemp_plot_2 <- downloadHandler(
    filename = function() {
      paste("Q8-10-plot-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = 8, height = 4,
                       res = 200, units = "in")
      }
      ggsave(file, plot = plot.realtime.wtemp.2$main, device = device)
    }
  )
  
  # Message re: turbidity threshold
  output$turb_message2 <- renderText({"The horizontal line indicates the raw water turbidity threshold of 20 NTU."})
  
  # Plot realtime data for second time period
  plot.realtime.data.2 <- reactiveValues(main=NULL)
  
  observe({
    
    output$realtime_data_2_plot <- renderPlotly({ 
      
      validate(
        need(input$plot_realtime_data_2 > 0,
             message = "Click 'Plot real-time data'")
      )
      
      realtime.dates <- seq.Date(from = as.Date("2023-10-13") - 30, to = as.Date("2023-10-13"), by = 'days')
      
      df <- realtime_fcr_data %>%
        filter(variable %in% c("DO_mgL_mean","Turbidity_FNU_mean")) %>%
        mutate(depth_ft = as.factor(depth_ft),
               layer = ifelse(depth_m <= 2, "surface waters","bottom waters")) %>%
        mutate(layer = factor(layer, levels = c("surface waters","bottom waters"))) %>%
        filter(datetime %in% realtime.dates) %>%
        rename(wq.variable = variable)
      
      # New facet label names for variables
      var.labs <- c("Dissolved oxygen (ppm)","Turbidity (NTU)")
      names(var.labs) <- unique(df$wq.variable)
      
      # Text for facets
      ann_text <- data.frame(datetime = as.Date(c("2023-10-11","2023-10-11")),
                             observation = c(9.5,6),
                             lab = rep("today", times = 2),
                             wq.variable = factor(unique(df$wq.variable)))
      
      p <- ggplot()+
        geom_line(data = df, aes(x = datetime, y = observation, group = layer, color = layer))+
        xlab("")+
        ylab("")+
        geom_hline(data = df %>% filter(wq.variable == "Turbidity_FNU_mean"),
                   aes(yintercept = 20), col = "#8A5F50")+
        facet_wrap(vars(wq.variable), nrow = 1, scales = "free_y", 
                   labeller = labeller(wq.variable = var.labs), strip.position = "top")+
        scale_color_manual(name = "Depth", values = c("#A9A448", "#023858"))+
        ggtitle("Real-time water quality data")+
        geom_vline(xintercept = as.numeric(as.Date("2023-10-13")), linetype = 2)+
        geom_text(data = ann_text, aes(x = datetime, y = observation, label = lab))+
        theme_bw()
      
      plot.realtime.data.2$main <- p
      
      #return(p)
      return(ggplotly(p, dynamicTicks = FALSE, tooltip=c("x", "y", "color")) %>% layout(height = 400, width = 1200))
      
    })
    
  })
  
  # Download plot of real-time data
  output$save_realtime_data_plot_2 <- downloadHandler(
    filename = function() {
      paste("Q42-43-plot-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = 8, height = 3,
                       res = 300, units = "in")
      }
      ggsave(file, plot = plot.realtime.data.2$main, device = device)
    }
  )
  
  # Plot forecast for second time period
  plot.fc2 <- reactiveValues(main=NULL)
  
  observe({
    
    output$fc2_plot <- renderPlotly({ 
      
      validate(
        need(input$plot_fc2 > 0,
             message = "Click 'View turnover forecast'")
      )
      
      df <- forecast_data %>%
        filter(fc_id == "second_forecast") %>%
        mutate(perc_turnover = prob_turnover*100)
      
      # Text 
      ann_text <- data.frame(datetime = as.Date(c("2023-10-12")),
                             perc_turnover = c(80),
                             lab = "today")
      
      p <- ggplot(data = df, aes(x = datetime, y = perc_turnover))+
        geom_line(aes(color = "percent chance of turnover"), linewidth = 1)+
        geom_vline(xintercept = as.numeric(as.Date("2023-10-13")), linetype = 2)+
        ggtitle("Turnover forecast")+
        xlab("")+
        ylab("Percent chance of turnover (%)")+
        ylim(c(0,100))+
        scale_color_manual(values = c("percent chance of turnover" = "#8EB1AF"), name = "")+
        geom_text(data = ann_text, aes(x = datetime, y = perc_turnover, label = lab))+
        theme_bw()
      
      plot.fc2$main <- p
      
      return(ggplotly(p, dynamicTicks = TRUE, tooltip=c("x", "y", "color")))
      
    })
    
  })
  
  # Download plot of second forecast
  output$save_fc2_plot <- downloadHandler(
    filename = function() {
      paste("Q17-plot-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = 8, height = 4,
                       res = 200, units = "in")
      }
      ggsave(file, plot = plot.fc2$main, device = device)
    }
  )
  
  # Message re: turbidity threshold
  output$turb_message3 <- renderText({"The horizontal line indicates the raw water turbidity threshold of 20 NTU."})
  
  # Plot outcome
  plot.outcome <- reactiveValues(main=NULL)
  
  observe({
    
    output$outcome_plot <- renderPlotly({ 
      
      validate(
        need(input$plot_outcome > 0,
             message = "Click 'Plot resevoir data'")
      )
      
      realtime.dates <- seq.Date(from = as.Date("2023-11-10") - 30, to = as.Date("2023-11-10"), by = 'days')
      
      df <- realtime_fcr_data %>%
        filter(variable %in% c("DO_mgL_mean","Turbidity_FNU_mean")) %>%
        mutate(depth_ft = as.factor(depth_ft),
               layer = ifelse(depth_m <= 2, "surface waters","bottom waters")) %>%
        mutate(layer = factor(layer, levels = c("surface waters","bottom waters"))) %>%
        filter(datetime %in% realtime.dates) %>%
        rename(wq.variable = variable)
      
      # New facet label names for variables
      var.labs <- c("Dissolved oxygen (ppm)","Turbidity (NTU)")
      names(var.labs) <- unique(df$wq.variable)
      
      # Text for facets
      ann_text <- data.frame(datetime = as.Date(c("2023-10-19","2023-10-19")),
                             observation = c(12,90),
                             lab = rep("turnover", times = 2),
                             wq.variable = factor(unique(df$wq.variable)))
      
      p <- ggplot()+
        geom_line(data = df, aes(x = datetime, y = observation, group = layer, color = layer))+
        xlab("")+
        ylab("")+
        geom_hline(data = df %>% filter(wq.variable == "Turbidity_FNU_mean"),
                   aes(yintercept = 20), col = "#8A5F50")+
        facet_wrap(vars(wq.variable), nrow = 1, scales = "free_y", 
                   labeller = labeller(wq.variable = var.labs), strip.position = "top")+
        scale_color_manual(name = "Depth", values = c("#A9A448", "#023858"))+
        ggtitle("Real-time water quality data")+
        geom_vline(xintercept = as.numeric(as.Date("2023-10-22")), linetype = 2)+
        geom_text(data = ann_text, aes(x = datetime, y = observation, label = lab))+
        theme_bw()
      
      plot.outcome$main <- p
      
      #return(p)
      return(ggplotly(p, dynamicTicks = FALSE, tooltip=c("x", "y", "color")) %>% layout(height = 400, width = 1200))
      
    })
    
  })
  
  # Download plot of outcome data
  output$save_outcome <- downloadHandler(
    filename = function() {
      paste("Q42-43-plot-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = 8, height = 3,
                       res = 300, units = "in")
      }
      ggsave(file, plot = plot.outcome$main, device = device)
    }
  )
  
  
  
  
  #### Navigating Tabs ----
    
    # Navigating Tabs ----
    #* Main Tab ====
    rv1 <- reactiveValues(prev = 0, nxt = 2)
    observeEvent(input$maintab, {
      curr_tab1 <- input$maintab
      rv1$prev <- readr::parse_number(curr_tab1) - 1
      rv1$nxt <- readr::parse_number(curr_tab1) + 1
    })
    
    observe({
      
      toggleState(id = "prevBtn1", condition = rv1$prev > 0)
      if(rv1$nxt > 4) {
        shinyjs::disable("nextBtn1")
      } else {
        shinyjs::enable("nextBtn1")
      }
      hide(selector = ".page")
    })
    
    
    # Next button
    observe({
      curr_tab1 <- input$maintab
      idx <- which(tab_names$tab_id == curr_tab1)
      new_nam <- tab_names$name[idx + 1]
      if(curr_tab1 == "mtab4" & rv1$nxt > 4) {
        updateActionButton(session, inputId = "nextBtn1", label = paste("End of module"))
      } else {
        updateActionButton(session, inputId = "nextBtn1", label = paste(new_nam, ">"))
      }   
      })
    
    # Previous button
    observe({
      curr_tab1 <- input$maintab
      idx <- which(tab_names$tab_id == curr_tab1)
      new_nam <- tab_names$name[idx - 1]
      if(curr_tab1 == "mtab1") {
        updateActionButton(session, inputId = "prevBtn1", label = paste("Module begins"))
      } else {
        # shinyjs::show(id = "prevBtn1")
        updateActionButton(session, inputId = "prevBtn1", label = paste("<", new_nam))
      }
    })
    
    
    # Advancing Tabs
    observeEvent(input$nextBtn1, {
      
      curr_tab1 <- input$maintab
      idx <- which(tab_names$tab_id == curr_tab1)
        updateTabsetPanel(session, "maintab",
                          selected = paste0("mtab", rv1$nxt))
      
      shinyjs::runjs("window.scrollTo(0, 0)") # scroll to top of page
    })
    
    # Moving back through tabs
    observeEvent(input$prevBtn1, {
      curr_tab1 <- input$maintab
      idx <- which(tab_names$tab_id == curr_tab1)
        updateTabsetPanel(session, "maintab",
                          selected = paste0("mtab", rv1$prev))
      shinyjs::runjs("window.scrollTo(0, 0)")
      
    })
    

  
  # Help buttons ----
  observeEvent(input$help, {
    introjs(session, events = list(onbeforechange = readCallback("switchTabs")))
  })
  observeEvent(input$help2, {
    shinyalert(title = "Resume Progress", text = "Use this field to upload your '.eddie' file to resume your progress.", type = "info")
  })
  
  # Bookmarking
  # use this to check inputs if need to update bookmarking
  # observe({
  #   list_of_inputs <<- reactiveValuesToList(input)
  # })
  # inp <- unlist(names(list_of_inputs))
  bookmarkingWhitelist <- c("plot_chla","plot_lag1","plot_lag2","calc_ac","plot_ac","plot_pacf","fit_model" ,"calc_bias",            
                            "calc_rmse","calc_proc_distrib","plot_high_freq","calc_ic_uc","fc1" ,"fc1_viz" ,             
                            "view_new_obs","update_ic","second_forecast_da","view_ic_no_da","second_forecast_no_da", "plot_low_ic" ,         
                            "plot_fc_low_obs_uc","plot_high_ic","plot_fc_high_obs_uc" , 
                            "fc_series_no_da","calc_bias2","calc_rmse2","fc_series_weekly","calc_bias3","calc_rmse3",           
                            "fc_series_daily","calc_bias4" ,"calc_rmse4"  ,"fc_scenario_weekly" ,"fc_scenario_daily","fc_compare",           
                            "calc_bias5","calc_rmse5", "calc_bias6" ,"calc_rmse6" ,"show_ic","show_obs",             
                            "show_obs2","show_obs3"  )

  observeEvent(input$bookmarkBtn, {
    session$doBookmark()
  })

  ExcludedIDs <- reactiveVal(value = NULL)

  observe({
    toExclude <- setdiff(names(input), bookmarkingWhitelist)
    setBookmarkExclude(toExclude)
    ExcludedIDs(toExclude)
  })

  # Save extra values in state$values when we bookmark
  onBookmark(function(state) {
    state$values$sel_row <- input$table01_rows_selected
  })

  # Read values from state$values when we restore
  onRestore(function(state) {
    updateTabsetPanel(session, "maintab",
                      selected = "mtab4")
    updateTabsetPanel(session, "tabseries1",
                      selected = "obj1")
  })

  onRestored(function(state) {
    updateSelectizeInput(session, "row_num", selected = state$values$sel_row)
  })
  
  
})

# end
