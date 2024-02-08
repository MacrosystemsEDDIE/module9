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
