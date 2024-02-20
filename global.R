# Load required libraries
suppressPackageStartupMessages(library(shinyBS, quietly = TRUE))
suppressPackageStartupMessages(library(shinydashboard, quietly = TRUE))
suppressPackageStartupMessages(library(rintrojs, quietly = TRUE))
suppressPackageStartupMessages(library(slickR, quietly = TRUE))
suppressPackageStartupMessages(library(sortable, quietly = TRUE))
suppressPackageStartupMessages(library(ggplot2, quietly = TRUE))
suppressPackageStartupMessages(library(stringr, quietly = TRUE))
suppressPackageStartupMessages(library(hover, quietly = TRUE))
suppressPackageStartupMessages(library(shiny, quietly = TRUE))
suppressPackageStartupMessages(library(shinyjs, quietly = TRUE))
suppressPackageStartupMessages(library(DT, quietly = TRUE))
suppressPackageStartupMessages(library(sf, quietly = TRUE))
suppressPackageStartupMessages(library(leaflet, quietly = TRUE))
suppressPackageStartupMessages(library(plotly, quietly = TRUE))
suppressPackageStartupMessages(library(kableExtra, quietly = TRUE))
suppressPackageStartupMessages(library(magrittr, quietly = TRUE))
suppressPackageStartupMessages(library(mvtnorm, quietly = TRUE))
suppressPackageStartupMessages(library(ggpubr, quietly = TRUE))
suppressPackageStartupMessages(library(ncdf4, quietly = TRUE))
suppressPackageStartupMessages(library(tidyverse, quietly = TRUE))
suppressPackageStartupMessages(library(lubridate, quietly = TRUE))
library(shinyalert, quietly = TRUE, warn.conflicts = FALSE)

# Enable bookmarking
enableBookmarking(store = "url")

# Colors for plots
scale_colour_discrete <- ggthemes::scale_colour_colorblind
scale_fill_discrete <- ggthemes::scale_fill_colorblind
cols <- RColorBrewer::brewer.pal(8, "Dark2")
cols2 <- ggthemes::ggthemes_data$colorblind$value
l.cols <- RColorBrewer::brewer.pal(8, "Set2")
p.cols <- RColorBrewer::brewer.pal(12, "Paired")

# colors for theme
obj_bg <- "#DDE4E1"
nav_butt <- "#2E4F84"
nav_txt <- "#fff" # white = #fff; black = #000000
slider_col <- "#DDE4E1"
ques_bg <- "#C1E4E2"

# Functions required
invisible(sapply(list.files("R", full.names = TRUE), source))

# Load text input
module_text <- read.csv("data/module_text.csv", row.names = 1, header = FALSE)

# Load and format questions
quest <- read.csv("data/student_questions.csv", row.names = 1)
qid <- row.names(quest)

# Help documentation
help_text <- read.csv("data/help_text.csv", row.names = 1)

# Slides
recap_slides <- list.files("www/key_slides", full.names = TRUE)
chla_slides <- list.files("www/chlorophyll", full.names = TRUE)
wtemp_slides <- list.files("www/waterTemperature", full.names = TRUE)
do_slides <- list.files("www/dissolvedOxygen", full.names = TRUE)



# Add last update time
app_time <- format(file.info("ui.R")$mtime, "%Y-%m-%d")
app_update_txt <- paste0("This app was last updated on: ", app_time)

# Tab names for updating buttons
tab_names <- read.csv("data/tab_names.csv", fileEncoding = "UTF-8-BOM")
rownames(tab_names) <- tab_names[, 1]

# Create site dataframe
sites_df <- tibble(SiteID = c("fcre","bvre"),
                   ReservoirName = c("Falling Creek Reservoir","Beaverdam Reservoir"),
                   Latitude = c(37.30325, 37.31288),
                   Longitude = c(-79.8373, -79.8159))

# Icons
ltrebIcons <- iconList(
  Aquatic = makeIcon("icons/water-icon.png", iconWidth = 28, iconHeight = 28)
)

# end
