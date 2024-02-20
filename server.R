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
    url <- "https://renc.osn.xsede.org/bio230121-bucket01/vera4cast/targets/project_id=vera4cast/duration=P1D/daily-insitu-targets.csv.gz"
    
    lake_data$df <- read_csv(url, show_col_types = FALSE) %>%
      filter(site_id == pull(sites_df[input$table01_rows_selected, "SiteID"]))
    
    #retrieve site photooutput$display.image <- renderImage({
    site_photo_file$img <- paste("www/",row_selected$SiteID,".jpg",sep="")

    #show site info
    output$site_info <- renderText({
      module_text[row_selected$SiteID, ]
    })
    
    #pull recent data
    recent_dates <- seq.Date(from = Sys.Date() - 30, to = Sys.Date(), by = 'days')
    
    lake_data$wtemp <- lake_data$df %>%
      select(datetime, variable, depth_m, observation) %>%
      filter(datetime %in% recent_dates & variable == "Temp_C_mean")
    
    lake_data$do <- lake_data$df %>%
      select(datetime, variable, depth_m, observation) %>%
      filter(datetime %in% recent_dates & variable == "DO_mgL_mean")
    
    lake_data$chla <- lake_data$df %>%
      select(datetime, variable, observation) %>%
      filter(datetime %in% recent_dates & variable == "Chla_ugL_mean")
    
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
      
      if(pull(sites_df[input$table01_rows_selected, "SiteID"]) == "fcre"){
      p <- ggplot(data = df, aes(x = datetime, y = observation, group = depth_m, color = depth_m))+
        geom_line()+
        xlab("")+
        ylab("Dissolved oxygen (mg/L)")+
        scale_color_continuous(trans = 'reverse', name = "Depth (m)")+
        theme_bw()
      }
      if(pull(sites_df[input$table01_rows_selected, "SiteID"]) == "bvre"){
        p <- ggplot(data = df, aes(x = datetime, y = observation, color = as.factor(depth_m)))+
          geom_line()+
          xlab("")+
          ylab("Dissolved oxygen (mg/L")+
          scale_color_manual(values = c("lightsteelblue"), name = "Depth (m)")+
          theme_bw()
      }
      
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
  
  #** chlorophyll-a Presentation slides ----
  output$chla_slides <- renderSlickR({
    slickR(chla_slides) + settings(dots = TRUE)
  })
  
  # Plot chlorophyll-a
  plot.chla <- reactiveValues(main=NULL)
  
  observe({
    
    output$chla_plot <- renderPlotly({ 
      
      validate(
        need(input$table01_rows_selected != "",
             message = "Please select a site in Objective 1.")
      )
      validate(
        need(!is.null(lake_data$df),
             message = "Please select a site in Objective 1.")
      )
      validate(
        need(input$plot_chla > 0,
             message = "Click 'Plot chlorophyll-a'")
      )
      
      df <- lake_data$chla
      
      p <- ggplot(data = df, aes(x = datetime, y = observation))+
        geom_point(aes(color = "Chl-a"))+
        xlab("")+
        ylab("Chlorophyll-a (ug/L)")+
        scale_color_manual(values = c("Chl-a" = "chartreuse4"), name = "")+
        theme_bw()
      
      plot.chla$main <- p
      
      return(ggplotly(p, dynamicTicks = TRUE))
      
    })
    
  })
  
  # Download plot of chl-a
  output$save_chla_plot <- downloadHandler(
    filename = function() {
      paste("Q9a-plot-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = 8, height = 4,
                       res = 200, units = "in")
      }
      ggsave(file, plot = plot.chla$main, device = device)
    }
  )
  
  #### Activity B ----
  
  
  
  
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
