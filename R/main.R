

# 1. Setting environment ----

RunModel <- "n"
# readline("Do you want to update the model? [y/n]: ")

if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")
renv::restore()

library(data.table)
library(lubridate)

library(ggplot2)
library(scales)
library(forcats)
library(plotly)

theme_set(theme_classic())

here <- here::here
read_excel <- readxl::read_excel
glue <- glue::glue
`%>%` <- magrittr::`%>%` #Microsoft doesn't the base yet

source("R/fixed-values.R")


# 2. Importing and reshaping data

source("R/import-reshape-data.R")


# 3. Modeling to explore the prices

if(RunModel == "y") source("R/model.R")


# 4. Explaining Results

rmarkdown::render("R/Report.Rmd",
                  output_file = "Report.html",
                  output_dir = file.path(getwd(),"Reports"),
                  envir = .GlobalEnv)
