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
                              chla = NULL)
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
      filter(site_id == pull(sites_df[input$table01_rows_selected, "SiteID"]))
    
    #retrieve site photooutput$display.image <- renderImage({
    site_photo_file$img <- paste("www/",row_selected$SiteID,".jpg",sep="")

    #show site info
    output$site_info <- renderText({
      module_text[row_selected$SiteID, ]
    })
    
    #pull recent data
    lake_data$wtemp <- lake_data$df %>%
      select(datetime, variable, depth_m, observation) %>%
      filter(variable == "Temp_C_mean")
    
    lake_data$do <- lake_data$df %>%
      select(datetime, variable, depth_m, observation) %>%
      filter(variable == "DO_mgL_mean")
    
    lake_data$turb <- lake_data$df %>%
      select(datetime, variable, observation) %>%
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
      
      df <- lake_data$wtemp
      
      p <- ggplot(data = df, aes(x = datetime, y = observation, group = depth_m, color = depth_m))+
        geom_line()+
        xlab("")+
        ylab("Water temperature (degrees Celsius)")+
        scale_color_continuous(trans = 'reverse', name = "Depth (m)")+
        ylim(c(0,35))+
        theme_bw()
      
      plot.wtemp$main <- p
      
      return(ggplotly(p, dynamicTicks = TRUE))
      
    })
    
  })
  
  # Download plot of water temperature
  output$save_wtemp_plot <- downloadHandler(
    filename = function() {
      paste("Q5a-plot-", Sys.Date(), ".png", sep="")
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
      
      df <- lake_data$do
      
      p <- ggplot(data = df, aes(x = datetime, y = observation, group = depth_m, color = depth_m))+
        geom_line()+
        xlab("")+
        ylab("Dissolved oxygen (mg/L)")+
        scale_color_continuous(trans = 'reverse', name = "Depth (m)")+
        theme_bw()
      
      plot.do$main <- p
      
      return(ggplotly(p, dynamicTicks = TRUE))
      
    })
    
  })
  
  # Download plot of DO
  output$save_do_plot <- downloadHandler(
    filename = function() {
      paste("Q7a-plot-", Sys.Date(), ".png", sep="")
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
        geom_point(aes(color = "Turbidity"))+
        xlab("")+
        ylab("Turbidity (FNU)")+
        scale_color_manual(values = c("Turbidity" = "brown"), name = "")+
        theme_bw()
      
      plot.turb$main <- p
      
      return(ggplotly(p, dynamicTicks = TRUE))
      
    })
    
  })
  
  # Download plot of turbidity
  output$save_turb_plot <- downloadHandler(
    filename = function() {
      paste("Q9a-plot-", Sys.Date(), ".png", sep="")
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
  
  res_data <- reactiveValues(df = NULL,
                             wtemp = NULL,
                             do = NULL,
                             chla = NULL,
                             tds = NULL,
                             turb = NULL)
  
  observeEvent(input$load_res_data, {
    
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = "Loading LTREB data",
                 detail = "This may take a while. This window will disappear
                     when it is loaded.", value = 0.33)
    
    # load in reservoir data
    res_data$df <- read_csv("./data/reservoir_data.csv")
    res_data$wtemp <- res_data$df %>%
      filter(variable == "Temp_C_mean")
    res_data$do <- res_data$df %>%
      filter(variable == "DO_mgL_mean")
    res_data$chla <- res_data$df %>%
      filter(variable == "Chla_ugL_mean")
    res_data$tds <- res_data$df %>%
      filter(variable == "TDS_mgL_mean")
    res_data$turb <- res_data$df %>%
      filter(variable == "Turbidity_FNU_mean")
    
    progress$set(value = 1)
    
  })
  
  # Plot 1 year of FCR water temperature
  plot.fcr.wtemp <- reactiveValues(main=NULL)
  
  observe({
    
    output$fcr_wtemp_plot <- renderPlotly({ 
      
      validate(
        need(!is.null(res_data$df),
             message = "Click 'Load reservoir data'")
      )
      validate(
        need(input$plot_res_wtemp > 0,
             message = "Click 'Plot water temperature'")
      )
      
      df <- res_data$wtemp %>%
        filter(site_id == "fcre")
      
      p <- ggplot(data = df, aes(x = datetime, y = observation, group = depth_m, color = depth_m))+
        geom_line()+
        xlab("")+
        ylab("Water temperature (degrees Celsius)")+
        scale_color_continuous(trans = 'reverse', name = "Depth (m)")+
        ggtitle("Falling Creek Reservoir")+
        ylim(c(0,35))+
        theme_bw()
      
      plot.fcr.wtemp$main <- p
      
      return(ggplotly(p, dynamicTicks = FALSE))
      
    })
    
  })
  
  # Download plot of FCR water temperature
  output$save_fcr_wtemp_plot <- downloadHandler(
    filename = function() {
      paste("Q10a-plot1-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = 8, height = 4,
                       res = 200, units = "in")
      }
      ggsave(file, plot = plot.fcr.wtemp$main, device = device)
    }
  )
  
  # Plot 1 year of BVR water temperature
  plot.bvr.wtemp <- reactiveValues(main=NULL)
  
  observe({
    
    output$bvr_wtemp_plot <- renderPlotly({ 
      
      validate(
        need(!is.null(res_data$df),
             message = "Click 'Load reservoir data'")
      )
      validate(
        need(input$plot_res_wtemp > 0,
             message = "Click 'Plot water temperature'")
      )
      
      df <- res_data$wtemp %>%
        filter(site_id == "bvre")
      
      p <- ggplot(data = df, aes(x = datetime, y = observation, group = depth_m, color = depth_m))+
        geom_line()+
        xlab("")+
        ylab("Water temperature (degrees Celsius)")+
        scale_color_continuous(trans = 'reverse', name = "Depth (m)")+
        ylim(c(0,35))+
        ggtitle("Beaverdam Reservoir")+
        theme_bw()
      
      plot.bvr.wtemp$main <- p
      
      return(ggplotly(p, dynamicTicks = FALSE))
      
    })
    
  })
  
  # Download plot of BVR water temperature
  output$save_bvr_wtemp_plot <- downloadHandler(
    filename = function() {
      paste("Q10a-plot2-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = 8, height = 4,
                       res = 200, units = "in")
      }
      ggsave(file, plot = plot.bvr.wtemp$main, device = device)
    }
  )
  
  # Plot 1 year of FCR dissolved oxygen
  plot.fcr.do <- reactiveValues(main=NULL)
  
  observe({
    
    output$fcr_do_plot <- renderPlotly({ 
      
      validate(
        need(!is.null(res_data$df),
             message = "Click 'Load reservoir data' above")
      )
      validate(
        need(input$plot_res_do > 0,
             message = "Click 'Plot dissolved oxygen'")
      )
      
      df <- res_data$do %>%
        filter(site_id == "fcre")
      
      p <- ggplot(data = df, aes(x = datetime, y = observation, group = depth_m, color = depth_m))+
        geom_line()+
        xlab("")+
        ylab("Dissolved oxygen (mg/L)")+
        scale_color_continuous(trans = 'reverse', name = "Depth (m)")+
        ylim(c(0,20))+
        ggtitle("Falling Creek Reservoir")+
        theme_bw()
      
      plot.fcr.do$main <- p
      
      return(ggplotly(p, dynamicTicks = TRUE))
      
    })
    
  })
  
  # Download plot of FCR dissolved oxygen
  output$save_fcr_do_plot <- downloadHandler(
    filename = function() {
      paste("Q12a-plot1-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = 8, height = 4,
                       res = 200, units = "in")
      }
      ggsave(file, plot = plot.fcr.do$main, device = device)
    }
  )
  
  # Plot 1 year of BVR dissolved oxygen
  plot.bvr.do <- reactiveValues(main=NULL)
  
  observe({
    
    output$bvr_do_plot <- renderPlotly({ 
      
      validate(
        need(!is.null(res_data$df),
             message = "Click 'Load reservoir data' above")
      )
      validate(
        need(input$plot_res_do > 0,
             message = "Click 'Plot dissolved oxygen'")
      )
      
      df <- res_data$do %>%
        filter(site_id == "bvre")
      
      p <- ggplot(data = df, aes(x = datetime, y = observation, group = depth_m, color = depth_m))+
        geom_line()+
        xlab("")+
        ylab("Dissolved oxygen (mg/L)")+
        scale_color_continuous(trans = 'reverse', name = "Depth (m)")+
        ylim(c(0,20))+
        ggtitle("Beaverdam Reservoir")+
        theme_bw()
      
      plot.bvr.do$main <- p
      
      return(ggplotly(p, dynamicTicks = TRUE))
      
    })
    
  })
  
  # Download plot of BVR dissolved oxygen
  output$save_bvr_do_plot <- downloadHandler(
    filename = function() {
      paste("Q12a-plot2-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = 8, height = 4,
                       res = 200, units = "in")
      }
      ggsave(file, plot = plot.bvr.do$main, device = device)
    }
  )
  
  # Plot 1 year of FCR chla
  plot.fcr.chla <- reactiveValues(main=NULL)
  
  observe({
    
    output$fcr_chla_plot <- renderPlotly({ 
      
      validate(
        need(!is.null(res_data$df),
             message = "Click 'Load reservoir data' above")
      )
      validate(
        need(input$plot_res_chla > 0,
             message = "Click 'Plot chlorophyll-a'")
      )
      
      df <- res_data$chla %>%
        filter(site_id == "fcre")
      
      p <- ggplot(data = df, aes(x = datetime, y = observation))+
        geom_point(aes(color = "Chl-a"))+
        xlab("")+
        ylab("Chlorophyll-a (ug/L)")+
        scale_color_manual(values = c("Chl-a" = "chartreuse4"), name = "")+
        ggtitle("Falling Creek Reservoir")+
        geom_hline(yintercept = 20)+
        theme_bw()
      
      plot.fcr.chla$main <- p
      
      return(ggplotly(p, dynamicTicks = TRUE))
      
    })
    
  })
  
  # Download plot of FCR chla
  output$save_fcr_chla_plot <- downloadHandler(
    filename = function() {
      paste("Q16a-plot1-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = 8, height = 4,
                       res = 200, units = "in")
      }
      ggsave(file, plot = plot.fcr.chla$main, device = device)
    }
  )
  
  # Plot 1 year of BVR chla
  plot.bvr.chla <- reactiveValues(main=NULL)
  
  observe({
    
    output$bvr_chla_plot <- renderPlotly({ 
      
      validate(
        need(!is.null(res_data$df),
             message = "Click 'Load reservoir data' above")
      )
      validate(
        need(input$plot_res_chla > 0,
             message = "Click 'Plot chlorophyll-a'")
      )
      
      df <- res_data$chla %>%
        filter(site_id == "bvre")
      
      p <- ggplot(data = df, aes(x = datetime, y = observation))+
        geom_point(aes(color = "Chl-a"))+
        xlab("")+
        ylab("Chlorophyll-a (ug/L)")+
        scale_color_manual(values = c("Chl-a" = "chartreuse4"), name = "")+
        ggtitle("Beaverdam Reservoir")+
        geom_hline(yintercept = 20)+
        theme_bw()
      
      plot.bvr.chla$main <- p
      
      return(ggplotly(p, dynamicTicks = TRUE))
      
    })
    
  })
  
  # Download plot of BVR chla
  output$save_bvr_chla_plot <- downloadHandler(
    filename = function() {
      paste("Q16a-plot2-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = 8, height = 4,
                       res = 200, units = "in")
      }
      ggsave(file, plot = plot.bvr.chla$main, device = device)
    }
  )
  
  # Plot 1 year of FCR tds
  plot.fcr.tds <- reactiveValues(main=NULL)
  
  observe({
    
    output$fcr_tds_plot <- renderPlotly({ 
      
      validate(
        need(!is.null(res_data$df),
             message = "Click 'Load reservoir data' above")
      )
      validate(
        need(input$plot_res_tds > 0,
             message = "Click 'Plot TDS/turbidity'")
      )
      
      df <- res_data$tds %>%
        filter(site_id == "fcre")
      
      p <- ggplot(data = df, aes(x = datetime, y = observation))+
        geom_point(aes(color = "TDS"))+
        xlab("")+
        ylab("Total dissolved solids (mg/L)")+
        scale_color_manual(values = c("TDS" = "darkorange"), name = "")+
        ggtitle("Falling Creek Reservoir")+
        theme_bw()
      
      plot.fcr.tds$main <- p
      
      return(ggplotly(p, dynamicTicks = TRUE))
      
    })
    
  })
  
  # Download plot of FCR tds
  output$save_fcr_tds_plot <- downloadHandler(
    filename = function() {
      paste("Q20a-plot1-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = 8, height = 4,
                       res = 200, units = "in")
      }
      ggsave(file, plot = plot.fcr.tds$main, device = device)
    }
  )
  
  # Plot 1 year of BVR tds
  plot.bvr.tds <- reactiveValues(main=NULL)
  
  observe({
    
    output$bvr_tds_plot <- renderPlotly({ 
      
      validate(
        need(!is.null(res_data$df),
             message = "Click 'Load reservoir data' above")
      )
      validate(
        need(input$plot_res_tds > 0,
             message = "Click 'Plot TDS/turbidity'")
      )
      
      df <- res_data$tds %>%
        filter(site_id == "bvre")
      
      p <- ggplot(data = df, aes(x = datetime, y = observation))+
        geom_point(aes(color = "TDS"))+
        xlab("")+
        ylab("Total dissolved solids (mg/L)")+
        scale_color_manual(values = c("TDS" = "darkorange"), name = "")+
        ggtitle("Beaverdam Reservoir")+
        theme_bw()
      
      plot.bvr.tds$main <- p
      
      return(ggplotly(p, dynamicTicks = TRUE))
      
    })
    
  })
  
  # Download plot of BVR tds
  output$save_bvr_tds_plot <- downloadHandler(
    filename = function() {
      paste("Q20a-plot2-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = 8, height = 4,
                       res = 200, units = "in")
      }
      ggsave(file, plot = plot.bvr.tds$main, device = device)
    }
  )
  
  # Plot 1 year of FCR turbidity
  plot.fcr.turb <- reactiveValues(main=NULL)
  
  observe({
    
    output$fcr_turb_plot <- renderPlotly({ 
      
      validate(
        need(!is.null(res_data$df),
             message = "Click 'Load reservoir data' above")
      )
      validate(
        need(input$plot_res_tds > 0,
             message = "Click 'Plot TDS/turbidity'")
      )
      
      df <- res_data$turb %>%
        filter(site_id == "fcre")
      
      p <- ggplot(data = df, aes(x = datetime, y = observation))+
        geom_point(aes(color = "Turbidity"))+
        xlab("")+
        ylab("Turbidity (FNU)")+
        scale_color_manual(values = c("Turbidity" = "brown4"), name = "")+
        ggtitle("Falling Creek Reservoir")+
        theme_bw()
      
      plot.fcr.turb$main <- p
      
      return(ggplotly(p, dynamicTicks = TRUE))
      
    })
    
  })
  
  # Download plot of FCR turbidity
  output$save_fcr_turb_plot <- downloadHandler(
    filename = function() {
      paste("Q21a-plot1-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = 8, height = 4,
                       res = 200, units = "in")
      }
      ggsave(file, plot = plot.fcr.turb$main, device = device)
    }
  )
  
  # Plot 1 year of BVR turbidity
  plot.bvr.turb <- reactiveValues(main=NULL)
  
  observe({
    
    output$bvr_turb_plot <- renderPlotly({ 
      
      validate(
        need(!is.null(res_data$df),
             message = "Click 'Load reservoir data' above")
      )
      validate(
        need(input$plot_res_tds > 0,
             message = "Click 'Plot TDS/turbidity'")
      )
      
      df <- res_data$turb %>%
        filter(site_id == "bvre")
      
      p <- ggplot(data = df, aes(x = datetime, y = observation))+
        geom_point(aes(color = "Turbidity"))+
        xlab("")+
        ylab("Turbidity (FNU)")+
        scale_color_manual(values = c("Turbidity" = "brown4"), name = "")+
        ggtitle("Beaverdam Reservoir")+
        theme_bw()
      
      plot.bvr.turb$main <- p
      
      return(ggplotly(p, dynamicTicks = TRUE))
      
    })
    
  })
  
  # Download plot of BVR turbidity
  output$save_bvr_turb_plot <- downloadHandler(
    filename = function() {
      paste("Q21a-plot2-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = 8, height = 4,
                       res = 200, units = "in")
      }
      ggsave(file, plot = plot.bvr.turb$main, device = device)
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
