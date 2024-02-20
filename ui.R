ui <- function(req) {
  
  tagList( # Added functionality for not losing your settings
    # shinythemes::themeSelector(), # user-defined theme
    # Java to prompt the students to click a button
    # Java script https://community.rstudio.com/t/keeping-track-of-idle-time-during-app-usage/1735
    tags$script("
              (function() {
  var timeoutWarningMsecs = 12 * 60 * 1000;
  var idleTimer;

  function onTimeout() {
    alert('Warning: Session is about to time out! Please click a button to prevent losing progress.');
  }

  function startIdleTimer() {
    if (idleTimer) clearTimeout(idleTimer);
    idleTimer = setTimeout(onTimeout, timeoutWarningMsecs);
  }

  $(document).on('shiny:message shiny:inputchanged', startIdleTimer);

})();"),
    tags$style(type = "text/css", "text-align: justify"),
    tags$head(tags$link(rel = "shortcut icon", href = "macroeddi_ico_green.ico")), # Add icon for web bookmarks
    tags$head(includeHTML(("google-analytics.html"))),
    fluidPage(
      column(10,
             br(),
             p(tags$b("Teaching materials associated with this module can be found at ",
                      tags$a(href="http://module9.macrosystemseddie.org", 
                             "http://module9.macrosystemseddie.org.", target="_blank")))
      )
    ),
    navbarPage(title = "Module 9: Using High-Frequency Data to Manage Water Quality",
               position = "static-top", id = "maintab",
               tags$header(
                 fluidRow(
                   column(11,
                          bookmarkButton(id = "bookmarkBtn", label = "Bookmark my progress"),
                          br(), 
                          p(tags$em("At any time, use this button to obtain a link that saves your progress."))
                   ),
                   column(1, align = "right",
                          introBox(
                            actionButton("help", label = "Help", icon = icon("question-circle")), data.step = 7, data.intro = help_text["help", 1]
                          )
                   )
                 )
               ),
               # 1. Introduction ----
               tabPanel(introBox(tab_names["mtab1", 2],
                                 data.step = 2,
                                 data.intro = help_text["tab_nav1", 1]
                                 ),
               value = "mtab1",
               introjsUI(), # must include in UI
               introBox(
                 img(src = "eddie_banner_2020_test.png", height = 100,
                     width = 1544, top = 5),
                 data.step = 1,
                 data.intro = help_text["welcome", 1]
               ),
               withMathJax(), # NEEDS to be here for rendering eqn's in data.table
               
               tags$style(".btn-file {
             background-color:#98CAB2;
             border-color: #2E4F84;
             }

             .progress-bar {
             background-color: #2E4F84;
             }"),
               # Change progress bar color
               tags$style(paste0("
               .irs-grid-text { font-size: 10pt; }
                                   .irs-bar,
.irs-bar-edge,
.irs-single,
.irs-grid-pol {
  background: ", slider_col, ";
  border-color: ", slider_col, ";
}")),
               includeCSS("www/slider_cols.css"),
               tags$style(HTML("
               .irs-bar {
                        border-color: transparent;
                        background-color: transparent;
                        }
                        #first {
                        border: 4px double red;
                        }
                        #13a_graz {
                        margin-bottom: 10px;
                        }
                        #bla_border {
                        border: 2px solid black;
                        }
                        #bla_border2 {
                        border: 1px solid black;
                        box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19);
                        }
                        #txt_j {
                        text-align: justify;
                        }
                        #txt_c {
                        text-align: center;
                        }
                        #txt_l {
                        text-align: left;
                        }
                        #ackn {
                        color: gray;
                        font-size: 12px
                        }
                        #pheno img {
                        transition:transform 0.25s ease;
                        max-width: 100%; width: 100%; height: auto
                        }
                        #nextBtn1:hover {
                        background-color: yellow;
                        }
                        #dl_btn {
                        width:290px
                        }
                        #pheno:hover img{
    -webkit-transform:scale(1.5);
    transform:scale(1.5);
}
                        #wh_link a {
                        color: #FFFFFF
                        }
                        #q6_tab {
                        'border':'1px solid #ddd'
                        }
                        .box.box-solid.box-primary>.box-header {

                }

                .box.box-solid.box-primary{

                background:#C1E4E2
                }
                .box.box-solid.box-success{

                background: #F5F094;
                }
                .box.box-solid.box-info{

                background: #DDE4E1;
                }
                .box.box-solid.box-warning>.box-header {

                }

                .box.box-solid.box-warning{

                background:#2E4F84
                }
                        ")),
               introBox(
                 fluidRow(
                   column(6,
                          #* Module text ====
                          h2("Using High-Frequency Data to Manage Water Quality"),
                          h3("Focal question"),
                          h4(tags$b(tags$i("How can we use high-frequency data to improve water quality?"))),
                          h3("Summary"),
                          p("In recent decades, there have been substantial improvements in our ability to monitor water quality in real time using sensors that measure variables at a high frequency (every few minutes)."),
                          p("In this module, you will explore data collected using high-frequency sensors and learn how to interpret these data to inform water quality management."),
                          h3("Learning Outcomes"),
                          tags$line(),
                          tags$ul(
                            tags$li(id = "txt_j", module_text["LO1", ]),
                            tags$li(id = "txt_j", module_text["LO2", ]),
                            tags$li(id = "txt_j", module_text["LO3", ]),
                            tags$li(id = "txt_j", module_text["LO4", ])
                          )
                   ),
                   column(6, 
                          br(), br(), br(),
                          img(src = "mod9_conceptual_figure.png", height = "100%",
                              width = "100%")
                   )
                 ), data.step = 8, data.intro = help_text["start", 1]
               ),
               hr(),
               fluidRow(
                 column(4,
                        h3("Introductory presentation"),
                        p("The presentation accompanying this module introduces key variables for assessing water quality, how high-frequency sensors can be used to measure water quality, and how high-frequency sensor data can aid in management decision-making."),
                        p(tags$b("What is water quality?")),
                        tags$ul(
                          tags$li(module_text["water_quality", ])
                        ),
                        p(tags$b("What is meant by high-frequency water quality data?")),
                        tags$ul(
                          tags$li(module_text["high_freq_data", ])
                        ),
                        p(tags$b("How are high-frequency water quality data collected?")),
                        tags$ul(
                          tags$li(module_text["collection", ])
                        ),
                        p(tags$i("Click through the slides to recap some of the main points from the lecture."))
                 ),
                 column(8, offset = 0, align = "center",
                        h3("Key Slides",
                           align = "center"),
                        h5("Click the arrows to navigate through the slides", align = "center"),
                        wellPanel(
                          slickROutput("slides", width = "700px", height = "525px")
                        )
                 )
               ),
               hr(),
               fluidRow(
                 column(10, 
                        box(id = "box1", width = 10, status = "success",
                            solidHeader = TRUE,
                            fluidRow(
                              column(8, offset = 1,
                                     introBox(
                                       h3("Workflow for this module"),
                                       tags$ol(
                                         tags$li(id = "txt_j", module_text["workflow1", ]),
                                         tags$li(id = "txt_j", module_text["workflow2", ]),
                                         tags$li(id = "txt_j", module_text["workflow3", ]),
                                         tags$li(id = "txt_j", module_text["workflow4", ])
                                       ),
                                       data.step = 5, data.intro = help_text["questions", 1]
                                     )
                              )
                            )
                        )
                 )
               ), hr(),
               fluidRow(
                 column(6,
                        h3("Saving your progress"),
                        p(style="text-align: justify;", "As you go, fill out answers to questions in the Canvas quiz. Some of the plots you generate in the web app will be needed for the Canvas quiz. When prompted, be sure to download these plots so you can copy-paste them into the Canvas quiz."),
                        p(style="text-align: justify;", "If you run out of time to finish all the activities you can save your progress and return to it at a later date. Click the 'Bookmark my progress' button at the top of the page and you will obtain a link, which you should save by copy-pasting it into the first question in your Canvas quiz. Be sure to save your quiz responses! When you are ready to resume work, paste the link into your web browser, and it will load a Shiny app session that contains your progress."),
                        br()
                 )
               ),
               hr(),
               fluidRow(
                 column(10, align = "left",
                        box(id = "box1", width = 10, status = "primary",
                            solidHeader = TRUE,
                            fluidRow(
                              column(8, offset = 1,
                                     h3("Before you start..."),
                                     p(id = "txt_j", "Open your Canvas quiz. Then, answer the following questions in the Canvas quiz."),
                                     introBox(
                                       h3(tags$b("Think about it!")),
                                       p(tags$b(quest["q1", 1])),
                                       p(tags$b(quest["q2", 1])),
                                       data.step = 5, data.intro = help_text["questions", 1]
                                     )
                              )
                            )
                        )
                 )
               ),
               hr(),
               fluidRow(
                 column(6,
                        h3("Data sources"),
                        p(HTML(paste0('This module will introduce how to use high-frequency water quality data to inform drinking water management using data from  ', a(href = "https://www.ltreb-reservoirs.org/", "Virginia Reservoirs LTREB sites", target = "_blank"), ", which are drinking water supply reservoirs located in southwest Virginia and owned and operated by the Western Virginia Water Authority.")))
                 ),
                 column(6, align = "center",
                        a(
                          href = "https://www.ltreb-reservoirs.org/",
                          img(src = "ltreb.png", title = "Virginia Reservoirs LTREB logo", height = "80%",
                              width = "80%"), target = "_blank"
                        ),
                        a(
                          href = "https://www.westernvawater.org/",
                          img(src = "wvwa.png", title = "Western Virginia Water Authority logo", height = "80%",
                              width = "80%"), target = "_blank"
                        )
                 )
               )
               ),
               
               # 2. Activity A ----
               tabPanel(title = tab_names["mtab2", 2], value = "mtab2",
                        img(src = "eddie_banner_2020_test.png", height = 100,
                            width = 1544, top = 5),
                        fluidRow(
                          column(12,
                                 wellPanel(style = paste0("background: ", obj_bg),
                                           h2("Activity A - Access and explore high-frequency water quality data"),
                                           p(module_text["act_A", ])
                                 )
                          ),
                          column(12,
                                 box(id = "box1", width = 10, status = "success",
                                     solidHeader = TRUE,
                                     fluidRow(
                                       column(10, offset = 1,
                                              introBox(
                                                h3("Objective 1: Select and learn about a focal drinking water reservoir"))
                                       )
                                     )
                                 )
                          )
                        ),
                        hr(),
                        fluidRow(
                          #** LTREB Intro ----
                          column(4,
                                 h2("Site Description"),
                                 p("Select a site in the table to highlight on the map"),
                                 conditionalPanel("input.row_num > 25",
                                                  selectizeInput("row_num", "Select row",
                                                                 choices = 1:nrow(sites_df),
                                                                 options = list(
                                                                   placeholder = 'Please select a row',
                                                                   onInitialize = I('function() { this.setValue(""); }'))
                                                  )
                                 ),
                                 DTOutput("table01")
                          ),
                          #** Site map ----
                          column(4,
                                 h2("Map of Virginia Reservoir LTREB sites"),
                                 wellPanel(
                                   leafletOutput("ltrebmap")
                                 )
                          )
                          ,
                          #** Site photo ----
                          column(4,
                                 h2("Site photo"),
                                 wellPanel(
                                   imageOutput("site_photo"),
                                   p(id = "txt_j", module_text["site_photo", ])
                                 )
                          )
                        ), 
                        br(),
                        fluidRow(
                          wellPanel(
                            h4(tags$b("About Site")),
                            textOutput("site_info")
                          )
                        ),
                        fluidRow(
                          column(10, align = "left",
                                 box(id = "box3", width = 10, status = "primary",
                                     solidHeader = TRUE,
                                     fluidRow(
                                       column(7, offset = 1,
                                              h3("Questions"),
                                              p(tags$b(quest["q3", 1]))
                                       )
                                     ),
                                     fluidRow(
                                       column(4, offset = 1, align = "left", style = paste0("background: ", ques_bg),
                                              p(tags$em(quest["q3a", 1] , width = "90%")),
                                              p(tags$em(quest["q3b", 1], width = "90%")),
                                              p(tags$em(quest["q3c", 1], width = "90%"))
                                       ),
                                       column(4, offset = 1, align = "left", style = paste0("background: ", ques_bg),
                                              p(tags$em(quest["q3d", 1] , width = "90%")),
                                              p(tags$em(quest["q3e", 1], width = "90%")),
                                              p(tags$em(quest["q3f", 1], width = "90%"))
                                       )
                                     )
                                 )
                          )
                        ),
                        hr(),
                        fluidRow(
                          column(12,
                                 box(id = "box1", width = 10, status = "success",
                                     solidHeader = TRUE,
                                     fluidRow(
                                       column(10, offset = 1,
                                              introBox(
                                                h3("Objective 2: Explore real-time high-frequency water quality data from your chosen reservoir"))
                                       )
                                     )
                                 )
                          )
                        ),
                        hr(),
                        fluidRow(
                          column(12,
                                 h4("Now we will visualize high-frequency data from your chosen reservoir and explore how these data can be related to water quality."))
                        ),
                        fluidRow(
                          column(4,
                                 h3("Water temperature"),
                                 p(tags$i("Click through the slides to understand how water temperature data can be related to water quality. The information presented on the slides is also summarized in text below the slides to help you answer the questions.")),
                                 br(),
                                 box(id = "box12", width = 12, status = "primary",
                                     solidHeader = TRUE,
                                     fluidRow(
                                       column(10, offset = 1, align = "left",
                                              h4("Questions"),
                                              p(tags$b(quest["q4", 1]))
                                       )
                                     )
                                 )
                          ),
                          column(8, offset = 0, align = "center",
                                 h3("Using water temperature to assess water quality",
                                    align = "center"),
                                 h5("Click the arrows to navigate through the slides", align = "center"),
                                 wellPanel(
                                   slickROutput("wtemp_slides", width = "700px", height = "525px")
                                 )
                          )
                        ),
                        fluidRow(
                          column(6,
                                 p(tags$b("Why is water temperature important?")),
                                 tags$ul(
                                   tags$li(module_text["wtemp", ])
                                 ),
                                 p(tags$b("What is thermal stratification?")),
                                 tags$ul(
                                   tags$li(module_text["stratification", ])
                                 ),
                                 img(src = "water_density.png", height = "60%", id = "bla_border",
                                     width = "60%", tags$style("border: solid 2px black;")),
                                 p("Water density vs. water temperature"),
                                 p(tags$em("Source: Mike Arthur and Demian Saffer, accessed at: https://www.e-education.psu.edu/earth111/node/842"))
                                 ),
                          column(6,
                                 p(tags$b("How does thermal stratification change over the course of a year?")),
                                 tags$ul(
                                   tags$li(module_text["seasonal_strat", ])
                                 ),
                                 p(tags$b("How does thermal stratification affect water quality?")),
                                 tags$ul(
                                   tags$li(module_text["strat_wq", ])
                                 ),
                                 p(tags$b("What is turnover?")),
                                 tags$ul(
                                   tags$li(module_text["turnover", ])
                                 ),
                                 p(tags$b("How does turnover affect water quality?")),
                                 tags$ul(
                                   tags$li(module_text["turnover_wq", ])
                                 )
                                 )
                        ),
                        hr(),
                        fluidRow(
                          column(4,
                                 h3("Plot water temperature data"),
                                 p("Click the button below to plot water temperature data at your chosen reservoir site."),
                                 actionButton("plot_wtemp", "Plot high-frequency water temperature data"),
                                 br(),br(),
                                 box(id = "box12", width = 12, status = "primary", 
                                     solidHeader = TRUE,
                                     fluidRow(
                                       column(10, offset = 1,
                                              h4("Questions"),
                                              p(tags$b(quest["q5", 1]))
                                       )
                                     )
                                 )
                          ),
                          column(8,
                                 wellPanel(
                                   plotlyOutput("wtemp_plot")
                                 ),
                                 downloadButton("save_wtemp_plot", "Download plot", icon = icon("download"))
                                 )
                        ),
                        hr(),
                        fluidRow(
                          column(4,
                                 h3("Dissolved oxygen"),
                                 p(tags$b("What is dissolved oxygen?")),
                                 tags$ul(
                                   tags$li(module_text["do", ])
                                 ),
                                 p(tags$b("How is dissolved oxygen related to water temperature?")),
                                 tags$ul(
                                   tags$li(module_text["do_wtemp", ])
                                 ),
                                 img(src = "oxygen_solubility.png", height = "90%", id = "bla_border",
                                     width = "90%", tags$style("border: solid 2px black;")),
                                 p("Oxygen solubility vs. water temperature"),
                                 p(tags$em("Source: Kenneth C. Waterman, accessed at: https://www.researchgate.net/figure/Effect-of-temperature-on-oxygen-solubility-in-water-generated-by-extrapolation-of-data_fig5_7957124")),
                                 p(tags$b("How can dissolved oxygen affect water quality?")),
                                 tags$ul(
                                   tags$li(module_text["do_wq", ])
                                 ),
                                 p(tags$i("Click through the slides to understand how dissolved oxygen data can be related to water quality.")),
                                 br(),
                                 box(id = "box12", width = 12, status = "primary",
                                     solidHeader = TRUE,
                                     fluidRow(
                                       column(10, offset = 1,
                                              h4("Questions"),
                                              p(tags$b(quest["q6", 1]))
                                       )
                                     )
                                 )
                          ),
                          column(8, offset = 0, align = "center",
                                 h3("Using dissolved oxygen to assess water quality",
                                    align = "center"),
                                 h5("Click the arrows to navigate through the slides", align = "center"),
                                 wellPanel(
                                   slickROutput("do_slides", width = "700px", height = "525px")
                                 )
                          )
                        ),
                        hr(),
                        fluidRow(
                          column(4,
                                 h3("Plot dissolved oxygen data"),
                                 p("Click the button below to plot dissolved oxygen data at your chosen reservoir site."),
                                 actionButton("plot_do", "Plot high-frequency dissolved oxygen data"),
                                 br(),br(),
                                 box(id = "box12", width = 12, status = "primary",
                                     solidHeader = TRUE,
                                     fluidRow(
                                       column(10, offset = 1,
                                              h4("Questions"),
                                              p(tags$b(quest["q7", 1]))
                                       )
                                     )
                                 )
                          ),
                          column(8,
                                 wellPanel(
                                   plotlyOutput("do_plot")
                                 ),
                                 downloadButton("save_do_plot", "Download plot", icon = icon("download"))
                          )
                        ),
                        hr(),
                        fluidRow(
                          column(4,
                                 h3("Chlorophyll-a"),
                                 p(tags$b("What is chlorophyll-a?")),
                                 tags$ul(
                                   tags$li(module_text["chla", ])
                                 ),
                                 p(tags$b("How can phytoplankton affect water quality?")),
                                 tags$ul(
                                   tags$li(module_text["phyto_wq", ])
                                 ),
                                 p(tags$b("How can we relate chlorophyll-a data to water quality?")),
                                 tags$ul(
                                   tags$li(module_text["chla_wq", ])
                                 ),
                                 p(tags$i("Click through the slides to understand how chlorophyll-a data can be related to water quality.")),
                                 br(),
                                 box(id = "box12", width = 12, status = "primary",
                                     solidHeader = TRUE,
                                     fluidRow(
                                       column(10, offset = 1,
                                              h4("Questions"),
                                              p(tags$b(quest["q8", 1]))
                                       )
                                     )
                                 )
                          ),
                          column(8, offset = 0, align = "center",
                                 h3("Using chlorophyll-a to assess water quality",
                                    align = "center"),
                                 h5("Click the arrows to navigate through the slides", align = "center"),
                                 wellPanel(
                                   slickROutput("chla_slides", width = "700px", height = "525px")
                                 )
                          )
                        ),
                        hr(),
                        fluidRow(
                          column(4,
                                 h3("Plot chlorophyll-a data"),
                                 p("Click the button below to plot chlorophyll-a data at your chosen reservoir site."),
                                 actionButton("plot_chla", "Plot high-frequency chlorophyll-a data"),
                                 br(),br(),
                                 box(id = "box12", width = 12, status = "primary",
                                     solidHeader = TRUE,
                                     fluidRow(
                                       column(10, offset = 1,
                                              h4("Questions"),
                                              p(tags$b(quest["q9", 1]))
                                       )
                                     )
                                 )
                          ),
                          column(8,
                                 wellPanel(
                                   plotlyOutput("chla_plot")
                                 ),
                                 downloadButton("save_chla_plot", "Download plot", icon = icon("download"))
                          )
                        ),
                        hr(),
                        fluidRow(
                          column(12,
                                 box(id = "box1", width = 10, status = "success",
                                     solidHeader = TRUE,
                                     fluidRow(
                                       column(10, offset = 1,
                                              introBox(
                                                h3("Next step:"),
                                                h4("Use high-frequency water quality data to explore how water quality changes over the course of a year in two drinking water reservoirs."))
                                       )
                                     )
                                 )
                          )
                        )
               ),
               # 6. Activity B ----
               tabPanel(title = tab_names["mtab3", 2], value = "mtab3",
                        img(src = "eddie_banner_2020_test.png", height = 100,
                            width = 1544, top = 5),
                        fluidRow(
                          column(12,
                                 wellPanel(style = paste0("background: ", obj_bg),
                                           h2("Activity B - Assess how water quality changes over time"),
                                           p(module_text["act_B", ])
                                 )
                          ),
                          column(12,
                                 box(id = "box1", width = 10, status = "success",
                                     solidHeader = TRUE,
                                     fluidRow(
                                       column(10, offset = 1,
                                              introBox(
                                                h3("Objective 3: Compare water temperature over a year at two reservoirs"))
                                       )
                                     )
                                 )
                          )
                        ),
                        hr(),
                        fluidRow(
                          column(12,
                                 h4("Water temperature"),
                                 p("Click the 'Plot water temperature' button below to view high-frequency water temperature data from the most recent complete calendar year at Falling Creek and Beaverdam Reservoirs. Then, answer the questions below."),
                                 p(tags$em("Note these plots are interactive! You can scroll over them to see data values, zoom in and out, and change your view window. Plot options will appear in the top right corner of the plot when you scroll over it."))
                                 )
                        ),
                        fluidRow(
                          column(6,
                                 wellPanel(
                                   plotlyOutput("fcr_wtemp_plot")
                                 ),
                                 downloadButton("save_fcr_wtemp_plot", "Download plot", icon = icon("download"))
                                 ),
                          column(6,
                                 wellPanel(
                                   plotlyOutput("bvr_wtemp_plot")
                                 ),
                                 downloadButton("save_bvr_wtemp_plot", "Download plot", icon = icon("download"))
                                 )
                        )
               ),
               # 7. Activity C ----
               tabPanel(title = tab_names["mtab4", 2], value = "mtab4",
                        img(src = "eddie_banner_2020_test.png", height = 100,
                            width = 1544, top = 5),
                        fluidRow(
                          column(12,
                                 wellPanel(style = paste0("background: ", obj_bg),
                                           h2("Activity C - Provide management recommendations using high-frequency data"),
                                           p(module_text["act_C", ])
                                 )
                          )
                        )
               )
    ),
    # Tab navigation buttons ----
    br(), hr(),
    useShinyjs(),
    introBox(
      # h4("Use the buttons below to navigate through the tabs", align = "center"),
      box(width = 12, status = "info",
          solidHeader = TRUE,
          fluidRow(
            
            column(5, align = "center",
                   br(),
                   hover_action_button(
                     inputId = "prevBtn1",
                     label = "< Previous",
                     button_animation = "glow",
                     style = paste0("color: ", nav_txt, "; background-color: ", nav_butt, "; border-color: #00664B; padding:15px; font-size:22px;")
                   ),
                   bsTooltip("prevBtn1", title = "Navigate to previous tab", placement = "left", trigger = "hover"),
                   br(), br()
                   
            ),
            column(2, align = "center",
                   br(),
                   br(), br()
            ),
            column(5, align = "center",
                   br(),
                   use_hover(popback = TRUE),
                   hover_action_button(
                     inputId = "nextBtn1",
                     label = "Next >",
                     button_animation = "glow",
                     style = paste0("color: ", nav_txt, "; background-color: ", nav_butt, "; border-color: #00664B; padding:15px; font-size:22px;")
                   ),
                   bsTooltip("nextBtn1", title = "Navigate to next tab", placement = "right", trigger = "hover"),
                   br(), br()
                   # )
            )
          )
      ), data.step = 3, data.intro = help_text["tab_nav2", 1], data.position = "right"
    ),
    hr(),
    fluidRow(
      column(8, offset = 1,
             br(),
             p(module_text["acknowledgement", ], id = "ackn"),
             p(app_update_txt, id = "ackn")
      )
    )
  )
}

shinyUI(ui)

# end
