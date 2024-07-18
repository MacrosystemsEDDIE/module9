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
    tags$header(
      introBox(
        img(src = "eddie_banner_2020_test.png", height = 100,
            width = 1544, top = 5),
        data.step = 1,
        data.intro = help_text["welcome", 1]
      )
    ),
    fluidPage(
      column(11,
             br(),
             p(tags$b("Teaching materials associated with this module can be found at ",
                      tags$a(href="http://module9.macrosystemseddie.org", 
                             "http://module9.macrosystemseddie.org.", target="_blank"))),
             h2(tags$b("Module 9: Using High-Frequency Data to Manage Water Quality")),
             bookmarkButton(id = "bookmarkBtn", label = "Bookmark my progress"),
             p(tags$em("At any time, use this button to obtain a link that saves your progress."))
      ),
      column(1, align = "right",
             br(),
             introBox(
               actionButton("help", label = "Help", icon = icon("question-circle")), data.step = 7, data.intro = help_text["help", 1]
             )
      )
    ),
    navbarPage(position = "static-top", id = "maintab",
               tags$header(
                 fluidRow(
                 )
               ),
               # 1. Introduction ----
               tabPanel(introBox(tab_names["mtab1", 2],
                                 data.step = 2,
                                 data.intro = help_text["tab_nav1", 1]
                                 ),
               value = "mtab1",
               introjsUI(), # must include in UI
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

                background:#D6AB9C
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
                                     ),
                                     fluidRow(
                                       column(7, offset = 1,
                                              p(tags$b("Virginia's Water Quality Assessment Guidance Manual gives the following guidance on water quality evaluation using a trophic state index (TSI), which may be calculated from Secchi depth (SD), chlorophyll-a (CA), or total phosphorus (TP):")),
                                              p(tags$em("A trophic state index value of 60 or greater for any one of the 3 indices will indicate that nutrient enrichment from anthropogenic sources are adversely interfering, directly or indirectly, with the designated uses. A TSI value of 60 corresponds to a CA concentration of 20 ug/l, a SD of 1 meter, and a TP concentration of 48 ug/l.")),
                                              p(tags$b(quest["q4", 1]))
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
                                                h3("Objective 2: Explore high-frequency water quality data from your chosen reservoir"))
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
                                              p(tags$b(quest["q5", 1])),
                                              p(tags$b(quest["q6", 1]))
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
                                              p(tags$b(quest["q7", 1])),
                                              p(tags$b(quest["q8", 1])),
                                              p(tags$b(quest["q9", 1]))
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
                                 p(tags$i("Click through the slides to understand how dissolved oxygen data can be related to water quality. The information presented on the slides is also summarized in text below the slides to help you answer the questions.")),
                                 br(),
                                 box(id = "box12", width = 12, status = "primary",
                                     solidHeader = TRUE,
                                     fluidRow(
                                       column(10, offset = 1,
                                              h4("Questions"),
                                              p(tags$b(quest["q10", 1])),
                                              p(tags$b(quest["q11", 1]))
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
                          column(6,
                                 p(tags$b("What is dissolved oxygen?")),
                                 tags$ul(
                                   tags$li(module_text["do", ])
                                 ),
                                 p(tags$b("How is dissolved oxygen related to water temperature?")),
                                 tags$ul(
                                   tags$li(module_text["do_wtemp", ])
                                 ),
                                 img(src = "oxygen_solubility.png", height = "60%", id = "bla_border",
                                     width = "60%", tags$style("border: solid 2px black;")),
                                 p("Oxygen solubility vs. water temperature"),
                                 p(tags$em("Source: Kenneth C. Waterman, accessed at: https://www.researchgate.net/figure/Effect-of-temperature-on-oxygen-solubility-in-water-generated-by-extrapolation-of-data_fig5_7957124"))
                                 ),
                          column(6,
                                 p(tags$b("How can dissolved oxygen affect water quality?")),
                                 tags$ul(
                                   tags$li(module_text["do_wq", ])
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
                                              p(tags$b(quest["q12", 1])),
                                              p(tags$b(quest["q13", 1])),
                                              p(tags$b(quest["q14", 1]))
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
                                 h3("Turbidity"),
                                 p(tags$i("Click through the slides to understand how turbidity data can be related to water quality.")),
                                 br(),
                                 box(id = "box12", width = 12, status = "primary",
                                     solidHeader = TRUE,
                                     fluidRow(
                                       column(10, offset = 1,
                                              h4("Questions"),
                                              p(tags$b(quest["q15", 1])),
                                              p(tags$b(quest["q16", 1]))
                                       )
                                     )
                                 )
                          ),
                          column(8, offset = 0, align = "center",
                                 h3("Using turbidity to assess water quality",
                                    align = "center"),
                                 h5("Click the arrows to navigate through the slides", align = "center"),
                                 wellPanel(
                                   slickROutput("turb_slides", width = "700px", height = "525px")
                                 )
                          )
                        ),
                        hr(),
                        fluidRow(
                          column(6,
                                 p(tags$b("What is turbidity?")),
                                 tags$ul(
                                   tags$li(module_text["turb", ])
                                 ),
                                 p(tags$b("How do we measure turbidity?")),
                                 tags$ul(
                                   tags$li(module_text["turb_measure", ])
                                 )
                                 ),
                          column(6,
                                 p(tags$b("How is turbidity related to water quality?")),
                                 tags$ul(
                                   tags$li(module_text["turb_wq", ]),
                                   tags$li(tags$b(module_text["turb_reg", ])),
                                   tags$li(module_text["turb_treat", ]),
                                   tags$li(module_text["turb_operator", ])
                                 )
                                 )
                        ),
                        hr(),
                        fluidRow(
                          column(4,
                                 h3("Plot turbidity data"),
                                 p("Click the button below to plot turbidity data at your chosen reservoir site."),
                                 actionButton("plot_turb", "Plot high-frequency turbidity data"),
                                 br(),br(),
                                 box(id = "box12", width = 12, status = "primary",
                                     solidHeader = TRUE,
                                     fluidRow(
                                       column(10, offset = 1,
                                              h4("Questions"),
                                              p(tags$b(quest["q17", 1])),
                                              p(tags$b(quest["q18", 1]))
                                       )
                                     )
                                 )
                          ),
                          column(8,
                                 wellPanel(
                                   plotlyOutput("turb_plot")
                                 ),
                                 downloadButton("save_turb_plot", "Download plot", icon = icon("download"))
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
                                                h4("Use high-frequency water quality data to make decisions about withdrawal depth in your focal reservoir."))
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
                                           h2("Activity B - Use high-frequency water quality data to make water treatment plant operation decisions"),
                                           p(module_text["act_B", ])
                                 )
                          ),
                          column(12,
                                 box(id = "box1", width = 10, status = "success",
                                     solidHeader = TRUE,
                                     fluidRow(
                                       column(10, offset = 1,
                                              introBox(
                                                h3("Objective 3: Use high-frequency water quality data to make water withdrawal depth decisions at different times of year"))
                                       )
                                     )
                                 )
                          )
                        ),
                        hr(),
                        fluidRow(
                          column(6,
                                 box(id = "box12", width = 12, status = "warning",
                                     solidHeader = TRUE,
                                     fluidRow(
                                       column(10, offset = 1,
                                              h3("Operation scenario"),
                                              h4("Decide: from which depth should water be extracted for treatment?"),
                                              p("You are operating a reservoir with intake valves at multiple depths. During the course of a year, you must decide which intake valve to use for water extraction for treatment."),
                                              p("Remember, water quality can differ greatly at different depths in the reservoir! Your objective is to withdraw water at the depth that has the highest water quality, to minimize treatment difficulty and cost."),
                                              p("Click the plotting buttons below to view reservoir data from different times of year. Then, use what you have learned in previous objectives to choose an intake valve depth."),
                                              h4("The depths of water intake valves in your reservoir are given below."),
                                              wellPanel(
                                                htmlOutput("extraction_depths"),
                                                p("You will use water quality data to decide which of these depths is best for water extraction.")
                                              )
                                       )
                                     )
                                 )
                                 ),
                          column(6,
                                 img(src = "Carvins2023.png", height = "90%", id = "bla_border",
                                     width = "90%", tags$style("border: solid 2px black;")),
                                 p(tags$em("Carvin's Cove Reservoir, Roanoke, VA")),
                                 p(tags$em("Photo credit: Ryan Keverline"))
                                 )
                        ),
                        hr(),
                        fluidRow(
                          column(6,
                                 h3("Summer"),
                                 p("Click the 'Plot summer data' button below to view high-frequency data from the most recent complete calendar year at Falling Creek and Beaverdam Reservoirs. Then, answer the questions below."),
                                 p(tags$em("These plots are interactive! You can scroll over them to see data values, zoom in and out, and change your view window. Plot options will appear in the top right corner of the plot when you scroll over it.")),
                                 fluidRow(
                                   column(4,
                                          actionButton("plot_summer_data", "Plot summer data")
                                   )
                                 )
                                 ),
                          column(6,
                                 )
                        ),
                        fluidRow(br()),
                        fluidRow(
                          column(4,
                                 box(id = "box12", width = 12, status = "primary",
                                     solidHeader = TRUE,
                                     fluidRow(
                                       column(10, offset = 1,
                                              h4("Questions"),
                                              p(tags$b(quest["q19", 1])),
                                              p(tags$b(quest["q20", 1])),
                                              p(tags$b(quest["q21", 1]))
                                       )
                                     )
                                 )
                          ),
                          column(8,
                                 wellPanel(
                                   plotlyOutput("summer_data_plot", width = "800px", height = "700px")
                                 ),
                                 downloadButton("save_summer_data_plot", "Download plot", icon = icon("download")),
                                 br(),br()
                                 )
                        ),
                        fluidRow(
                          column(12,
                                 box(id = "box12", width = 12, status = "warning",
                                     solidHeader = TRUE,
                                     fluidRow(
                                       column(10, offset = 1,
                                              h3("Make a decision for water extraction on July 31."),
                                              h4(tags$em("Hint: you can scroll over the plot to see the exact values of the water quality variables at different depths on July 31.")),
                                              p(tags$b(quest["q22", 1])),
                                              p(tags$b(quest["q23", 1]))
                                       )
                                     )
                                 )
                                 )
                        ),
                        hr(),
                        fluidRow(
                          column(6,
                                 h3("Fall"),
                                 p("Click the 'Plot fall data' button below to view high-frequency data from the most recent complete calendar year at Falling Creek and Beaverdam Reservoirs. Then, answer the questions below."),
                                 p(tags$em("These plots are interactive! You can scroll over them to see data values, zoom in and out, and change your view window. Plot options will appear in the top right corner of the plot when you scroll over it.")),
                                 fluidRow(
                                   column(4,
                                          actionButton("plot_fall_data", "Plot fall data")
                                   )
                                 )
                          ),
                          column(6,
                          )
                        ),
                        fluidRow(br()),
                        fluidRow(
                          column(4,
                                 box(id = "box12", width = 12, status = "primary",
                                     solidHeader = TRUE,
                                     fluidRow(
                                       column(10, offset = 1,
                                              h4("Questions"),
                                              p(tags$b(quest["q24", 1])),
                                              p(tags$em("Hint! Notice that the data are plotted in degrees Fahrenheit!")),
                                              p(tags$b(quest["q25", 1])),
                                              p(tags$b(quest["q26", 1]))
                                       )
                                     )
                                 )
                          ),
                          column(8,
                                 wellPanel(
                                   plotlyOutput("fall_data_plot", width = "800px", height = "700px")
                                 ),
                                 downloadButton("save_fall_data_plot", "Download plot", icon = icon("download")),
                                 br(),br()
                          )
                        ),
                        fluidRow(
                          column(12,
                                 box(id = "box12", width = 12, status = "warning",
                                     solidHeader = TRUE,
                                     fluidRow(
                                       column(10, offset = 1,
                                              h3("Make a decision for water extraction before and after fall turnover."),
                                              p(tags$b(quest["q27", 1])),
                                              p(tags$b(quest["q28", 1])),
                                              p(tags$b(quest["q29", 1]))
                                       )
                                     )
                                 )
                          )
                        ),
                        hr(),
                        fluidRow(
                          column(6,
                                 h3("Winter"),
                                 p("Click the 'Plot winter data' button below to view high-frequency data from the most recent complete calendar year at Falling Creek and Beaverdam Reservoirs. Then, answer the questions below."),
                                 p(tags$em("These plots are interactive! You can scroll over them to see data values, zoom in and out, and change your view window. Plot options will appear in the top right corner of the plot when you scroll over it.")),
                                 fluidRow(
                                   column(4,
                                          actionButton("plot_winter_data", "Plot winter data")
                                   )
                                 )
                          ),
                          column(6,
                          )
                        ),
                        fluidRow(br()),
                        fluidRow(
                          column(4,
                                 box(id = "box12", width = 12, status = "primary",
                                     solidHeader = TRUE,
                                     fluidRow(
                                       column(10, offset = 1,
                                              h4("Questions"),
                                              p(tags$b(quest["q30", 1])),
                                              p(tags$b(quest["q31", 1])),
                                              p(tags$b(quest["q32", 1]))
                                       )
                                     )
                                 )
                          ),
                          column(8,
                                 wellPanel(
                                   plotlyOutput("winter_data_plot", width = "800px", height = "700px")
                                 ),
                                 downloadButton("save_winter_data_plot", "Download plot", icon = icon("download")),
                                 br(),br()
                          )
                        ),
                        fluidRow(
                          column(12,
                                 box(id = "box12", width = 12, status = "warning",
                                     solidHeader = TRUE,
                                     fluidRow(
                                       column(10, offset = 1,
                                              h3("Make a decision for water extraction on Jan. 31."),
                                              p(tags$b(quest["q33", 1])),
                                              p(tags$b(quest["q34", 1]))
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
                                                h3("Objective 4: Define water quality forecasting and interpret a fall turnover forecast"))
                                       )
                                     )
                                 )
                          )
                        ),
                        hr(),
                        fluidRow(
                          column(6,
                                 h3("Forecasting fall turnover"),
                                 p("You have seen that fall turnover can have substantial impacts on water quality."),
                                 p("Because of this, researchers have been working to develop water temperature forecasts to predict fall turnover in advance."),
                                 p("With advance notice of turnover, water managers may be able to take preemptive management action to avoid water quality degradation or increased treatment costs during turnover."),
                                 p("Below, you will learn about water temperature forecasting and how to interpret a turnover forecast."),
                                 box(id = "box12", width = 12, status = "warning",
                                     solidHeader = TRUE,
                                     fluidRow(
                                       column(10, offset = 1,
                                              h3("Fall turnover effects on water quality"),
                                              p("The picture to the right illustrates the potential effects of fall turnover on water quality."),
                                              p("When temperatures homogenize across the water column, dissolved substances and particulate matter can be brought to the surface, increasing turbidity."),
                                              p("The bottle on the left was a sample taken from Falling Creek Reservoir just before turnover, while the bottle on the right was collected right after turnover - just a few days later!"),
                                              p(tags$em("Photo credit: Bethany Bookout"))
                                       )
                                     )
                                 )
                                 ),
                          column(6,
                                 img(src = "turnover_water_samples.png", height = "90%", id = "bla_border",
                                     width = "90%", tags$style("border: solid 2px black;")),
                                 )
                        ),
                        hr(),
                        fluidRow(
                          column(4,
                                 h3("Water quality forecasting"),
                                 p(tags$b("What is a forecast?")),
                                 tags$ul(
                                   tags$li(module_text["forecast", ])
                                 ),
                                 p(tags$b("What is forecast uncertainty?")),
                                 tags$ul(
                                   tags$li(module_text["uncertainty", ])
                                 )
                          ),
                          column(8, offset = 0, align = "center",
                                 h3("What is water quality forecasting?",
                                    align = "center"),
                                 h5("Click the arrows to navigate through the slides", align = "center"),
                                 wellPanel(
                                   slickROutput("forecast_slides", width = "700px", height = "525px")
                                 )
                          )
                        ),
                        hr(),
                        fluidRow(
                          column(8,
                                 wellPanel(
                                   h4("View turnover forecast"),
                                   p("Click the button below to view a 1 to 16-day-ahead turnover forecast."),
                                   actionButton("plot_fc", "View turnover forecast"),
                                   br(),br(),
                                   plotlyOutput("fc_plot"),
                                   br(),br(),
                                   downloadButton("save_fc_plot", "Download plot", icon = icon("download"))
                                 ),
                          ),
                          column(4,
                                 box(id = "box12", width = 12, status = "primary",
                                     solidHeader = TRUE,
                                     fluidRow(
                                       column(10, offset = 1,
                                              h3("Interpret the forecast plot."),
                                              p(tags$b(quest["q35", 1])),
                                              p(tags$b(quest["q36", 1])),
                                              p(tags$b(quest["q37", 1]))
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
                                                h3("Next step:"),
                                                h4("Make water treatment decisions using high-frequency water quality data and water quality forecasts."))
                                       )
                                     )
                                 )
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
                                           h2("Activity C - Make water treatment decisions using high-frequency water quality data and forecasts"),
                                           p(module_text["act_C", ])
                                 )
                          ),
                        column(12,
                               box(id = "box1", width = 10, status = "success",
                                   solidHeader = TRUE,
                                   fluidRow(
                                     column(10, offset = 1,
                                            introBox(
                                              h3("Objective 5: Make water treatment decisions using high-frequency water quality data and forecasts"))
                                     )
                                   )
                               )
                        )
               ),
               hr(),
               fluidRow(
                 column(6,
                        box(id = "box12", width = 12, status = "warning",
                            solidHeader = TRUE,
                            fluidRow(
                              column(10, offset = 1,
                                     h3("Operation scenario"),
                                     h4("Decide: should I begin additional treatment measures to prevent water quality concerns during fall turnover, and if so, when?"),
                                     p("You are operating a small reservoir water treatment plant with a very short detention time - only 30 minutes from the raw water intake to the bottom filter. In addition, the reservoir feeding your plant is small, with a short residence time. This means that water can travel from the inflow to the dam, where it is withdrawn for treatment, in a matter of hours to a few days, especially during high rainfall events. There is only one extraction depth in the surface waters of the reservoir. As a result, your plant is very sensitive to high turbidity in the raw water, and you must be able to act quickly to avoid exceeding the regulatory limit of 0.3 NTU turbidity on the bottom filter."),
                                     p(tags$b("Specifically, previous operators have had difficulty meeting regulatory limits for bottom filter turbidity when raw water turbidity exceeds 20 NTU.")),
                                     p("Your supervisor has alerted you that this reservoir has experienced increased turbidity around the time of fall turnover in the past. Usually, the highest turbidity is experienced from about a week before until about a week after turnover. In addition, it has been a rainy fall so far this year. Cumulative rainfall last month was three inches above what your region typically experiences."),
                                     h4("Your objective is to determine if and when to enact additional treatment measures to ensure you meet the regulatory limit for turbidity immediately before and after fall turnover."),
                                     p(tags$b("You will make a series of four treatment decisions using reservoir data and turnover forecasts.")),
                                     p("Click the plotting buttons below to view reservoir data and turnover forecasts. Then, use what you have learned in previous objectives to choose whether to enact additional treatment measures to counteract potentially high turbidity during fall turnover.")
                              )
                            )
                        )
                 ),
                 column(6,
                        img(src = "SHRtreatmentPlant.png", height = "90%", id = "bla_border",
                            width = "90%", tags$style("border: solid 2px black;")),
                        p(tags$em("Spring Hollow Water Treatment Facility, Roanoke County, VA")),
                        p(tags$em("Photo credit: Western Virginia Water Authority"))
                 )
               ),
               hr(),
                        fluidRow(
                          column(12,
                                 h3("1. Make a decision using real-time reservoir data"),
                                 h4("Today is October 6."),
                                 p("Below are plots of real-time reservoir data over the past month showing water temperature, dissolved oxygen, and turbidity in your reservoir."),
                                 p(tags$b("You know from talking with other operators who work at this plant that turnover in the reservoir typically happens in mid to late October.")),
                                 p("Use the information in the plots to make a treatment decision.")
                                 )
                        ),
               br(),
               fluidRow(
                 column(12,
                        wellPanel(
                          h4("Plot real-time reservoir data"),
                          p("Click the button below to plot real-time data at the reservoir you are operating."),
                          actionButton("plot_realtime_data_1", "Plot real-time data"),
                          br(),br(),
                          plotlyOutput("realtime_wtemp_plot_1", width = "1200px", height = "400px"),
                          br(),br(),
                          downloadButton("save_realtime_wtemp_plot_1", "Download plot", icon = icon("download")),
                          br(),br(),
                          span(textOutput("turb_message1"), style="color:#8A5F50; font-size:16px"),
                          br(),
                          plotlyOutput("realtime_data_1_plot", width = "1200px", height = "400px"),
                          br(),br(),
                          downloadButton("save_realtime_data_plot_1", "Download plot", icon = icon("download")),
                          br(),br()
                        )
                 )
               ),
               br(),
               fluidRow(
                 column(12,
                        box(id = "box12", width = 12, status = "warning",
                            solidHeader = TRUE,
                            fluidRow(
                              column(10, offset = 1,
                                     h3("Make a decision for water treatment on October 6 using real-time data."),
                                     p(tags$b(quest["q38", 1])),
                                     p(tags$b(quest["q39", 1]))
                              )
                            )
                        )
                 )
               ),
              hr(),
               fluidRow(
                 column(12,
                        h3("2. Make a decision using a turnover forecast"),
                        h4("Today is October 6."),
                        p("Below is a plot showing a turnover forecast in your reservoir, starting today and forecasting up to 16 days into the future."),
                        p("Use the information in the forecast to make a treatment decision.")
                 )
               ),
               fluidRow(
                 column(8,
                        wellPanel(
                          h4("View turnover forecast"),
                          p("Click the button below to view a 1 to 16-day-ahead turnover forecast at the reservoir you are operating."),
                          actionButton("plot_fc1", "View turnover forecast"),
                          br(),br(),
                          plotlyOutput("fc1_plot"),
                          br(),br(),
                          downloadButton("save_fc1_plot", "Download plot", icon = icon("download"))
                        ),
                 )
               ),
               br(),
               fluidRow(
                 column(12,
                        box(id = "box12", width = 12, status = "warning",
                            solidHeader = TRUE,
                            fluidRow(
                              column(10, offset = 1,
                                     h3("Make a decision for water treatment on October 6 using a turnover forecast."),
                                     p(tags$b(quest["q40", 1])),
                                     p(tags$b(quest["q41", 1])),
                                     p(tags$b(quest["q42", 1]))
                              )
                            )
                        )
                 )
               ),
                        hr(),
              fluidRow(
                column(12,
                       h3("3. Make a decision using real-time reservoir data"),
                       h4("Today is October 13."),
                       p("A week has passed since you made your last decision."),
                       p("Below are plots of real-time reservoir data over the past month showing water temperature, dissolved oxygen, and turbidity in your reservoir."),
                       p(tags$b("Remember, turnover in the reservoir typically happens in mid to late October.")),
                       p("Use the information in the plots to make a treatment decision.")
                )
              ),
              br(),
              fluidRow(
                column(12,
                       wellPanel(
                         h4("Plot real-time reservoir data"),
                         p("Click the button below to plot real-time data at the reservoir you are operating."),
                         actionButton("plot_realtime_data_2", "Plot real-time data"),
                         br(),br(),
                         plotlyOutput("realtime_wtemp_plot_2", width = "1200px", height = "400px"),
                         br(),br(),
                         downloadButton("save_realtime_wtemp_plot_2", "Download plot", icon = icon("download")),
                         br(),br(),
                       span(textOutput("turb_message2"), style="color:#8A5F50; font-size:16px"),
                       br(),
                       plotlyOutput("realtime_data_2_plot", width = "1200px", height = "400px"),
                       br(),br(),
                       downloadButton("save_realtime_data_plot_2", "Download plot", icon = icon("download")),
                       br(),br()
                       )
                )
              ),
              br(),
              fluidRow(
                column(12,
                       box(id = "box12", width = 12, status = "warning",
                           solidHeader = TRUE,
                           fluidRow(
                             column(10, offset = 1,
                                    h3("Make a decision for water treatment on October 13 using real-time data."),
                                    p(tags$b(quest["q43", 1])),
                                    p(tags$b(quest["q44", 1]))
                             )
                           )
                       )
                )
              ),
              hr(),
              fluidRow(
                column(12,
                       h3("4. Make a decision using a turnover forecast"),
                       h4("Today is October 13."),
                       p("Below is a plot showing a turnover forecast in your reservoir, which has been updated since last week using the most recent reservoir data."),
                       p("The forecast starts today and forecasts up to 16 days into the future."),
                       p("Use the information in the forecast to make a treatment decision.")
                )
              ),
              fluidRow(
                column(8,
                       wellPanel(
                         h4("View turnover forecast"),
                         p("Click the button below to view a 1 to 16-day-ahead turnover forecast at the reservoir you are operating."),
                         actionButton("plot_fc2", "View turnover forecast"),
                         br(),br(),
                         plotlyOutput("fc2_plot"),
                         br(),br(),
                         downloadButton("save_fc2_plot", "Download plot", icon = icon("download"))
                       ),
                )
              ),
              br(),
              fluidRow(
                column(12,
                       box(id = "box12", width = 12, status = "warning",
                           solidHeader = TRUE,
                           fluidRow(
                             column(10, offset = 1,
                                    h3("Make a decision for water treatment on October 13 using a turnover forecast."),
                                    p(tags$b(quest["q45", 1])),
                                    p(tags$b(quest["q46", 1]))
                             )
                           )
                       )
                )
              ),
                        hr(),
                        fluidRow(
                          column(12,
                                 h3("Evaluate your decision."),
                                 h4("Today is November 10."),
                                 p("Nearly a month has passed since you made your last decision."),
                                 p("Below are two plots showing data from your reservoir over the past month, ",tags$b("including just before and after fall turnover, which occurred on October 22.")),
                                 p("Use the information in the plots below to evaluate your decisions using real-time data and turnover forecasts.")
                          )
                        ),
                        fluidRow(
                          column(12,
                                 wellPanel(
                                   h4("View reservoir data before and after turnover"),
                                   p("Click the button below to plot data at the reservoir you are operating from before and after turnover."),
                                   actionButton("plot_outcome", "Plot reservoir data"),
                                   br(),br(),
                                   span(textOutput("turb_message2"), style="color:#8A5F50; font-size:16px"),
                                   br(),
                                   plotlyOutput("outcome_plot", width = "1200px", height = "400px"),
                                   br(),br(),
                                   downloadButton("save_outcome", "Download plot", icon = icon("download")),
                                   br(),br()
                                 )
                          )
                        ),
                        hr(),
                        fluidRow(
                          column(12,
                                 box(id = "box1", width = 10, status = "primary",
                                     solidHeader = TRUE,
                                     fluidRow(
                                       column(10, offset = 1,
                                              introBox(
                                                h3("Questions"),
                                                p(tags$b(quest["q47", 1])),
                                                p(tags$b(quest["q48", 1])),
                                                p(tags$b(quest["q49", 1])),
                                                p(tags$b(quest["q50", 1]))
                                                )
                                       )
                                     )
                                 )
                          )
                        ),
              hr(),
              fluidRow(
                column(12,
                       h2("Completed Module!"),
                       h3("You have completed the module! Congratulations!"),
                       h4("Please check through the answers in your Canvas quiz and be sure you have copy-pasted in all the required plots before you submit the quiz to your instructor."),
                       h4("You’ve now made operations decisions informed by high-frequency water quality data and forecasts - well done!")
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
