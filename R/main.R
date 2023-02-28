

# 1. Setting environment ----

RunModel <- readline("Do you want to update the model? [y/n]: ")

library(data.table)
library(lubridate)
library(ggplot2)
library(scales)
library(forcats)
library(ggtext)

here <- here::here
read_excel <- readxl::read_excel
glue <- glue::glue
`%>%` <- magrittr::`%>%`

source("R/fixed-values.R")


# 2. Importing and reshaping data

source("R/import-reshape-data.R")


# 3. Modeling to explore the prices

if(RunModel == "y") source("R/model.R")


# 4. Explaining Results

rmarkdown::render("R/Report.Rmd",
                  output_file = "Report-Summary.html",
                  output_dir = file.path(getwd(),"Reports"),
                  envir = .GlobalEnv)
