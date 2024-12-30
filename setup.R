# setup.R


# Set computer
cpu <- "david-pc"  # "pc", "mac"
cpu <- "jmb"

# Load required packages
pacman::p_load("data.table", "tidyr", "dplyr", "lubridate", "stringr", "janitor", "ncdf4", "reshape2", # data manipulation
               "foreach", "parallel", "doParallel",  # parallel computing
               "ggplot2", "egg", "pals", "viridis", "gridExtra", "grid", "scales", # plots
               "zoo",  # time series
               "rnaturalearthdata", "rnaturalearth", "ggmap", # base data
               "sf", "raster", "geosphere", "move", "sfheaders")  # spatial

# Source custom functions
source("fun/processing_tools.R")  # sources from external repo
source("fun/fun_ais.R")


# Set main data paths
if(cpu == "david-pc") main_dir <- "C:/Users/dm620/SML Dropbox/gitdata/raor_sat_monitoring/"
if(cpu == "david-mac") main_dir <- "~/Dropbox/GitData/covid-uwnoise"


# Create data paths
input_dir <- paste(main_dir, "input", sep="/")
if (!dir.exists(input_dir)) dir.create(input_dir, recursive = TRUE)

output_dir <- paste(main_dir, "output", sep="/")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

