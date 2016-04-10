#################### Project Directory Setup ########################
library(RCurl)
library(XML)
library(geosphere)
library(caret)
library(RColorBrewer)
library(dplyr)
library(rpart)
library(doMC)
library(lubridate)
library(ggplot2)
library(ggmap)
# set up project folder, create necessary subdirectories
dir.create("code")
dir.create("rawdata")
dir.create("data")
dir.create("resources")
dir.create("report")
dir.create("images")

file.create("preprocess.R")
file.create("model.R")
file.create("submission.R")
file.create("plot.R")
file.create("util.R")


# move files to corresponding folders
file.rename(from = "preprocess.R", to = "code/preprocess.R")
file.rename(from = "model.R", to = "code/model.R")
file.rename(from = "submission.R", to = "code/submission.R")
file.rename(from = "util.R", to = "code/util.R")
file.rename(from = "plot.R", to = "code/plot.R")
