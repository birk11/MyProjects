setwd("P:/Toulouse Business School/TBS Classes/Projects/19-October to 31-October/Project Information-20181023")

install.packages("purrr")
install.packages("quanteda")
install.packages("DMwR2")
install.packages("DMwR")
install.packages("RJSON")
install.packages("tm")  # for text mining
install.packages("SnowballC") # for text stemming
install.packages("wordcloud") # word-cloud generator 
install.packages("RColorBrewer") # color palettes
install.packages("pracma")
install.packages("tm")  # for text mining
install.packages("SnowballC") # for text stemming
install.packages("wordcloud") # word-cloud generator 
install.packages("RColorBrewer") # color palettes
install.packages("stringr")
# Load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("pracma")
# Load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library(RJSONIO)
library(quanteda)
library(purrr)
library(dplyr)
library(tidyr)
library(DMwR)
library(DMwR2)
library(dplyr)
library(tidyr)
library(rjson)
library(RJSONIO)
library("plyr")
library(jsonlite)
library(dplyr)
library(purrr)
library(tidyr)
library(jsonlite)
library(stringr)

movies=read.csv("tmdb_5000_movies.csv" ,header = TRUE,sep= ",")
credits=read.csv("tmdb_5000_credits.csv" ,header = TRUE,sep= ",")
recommendation_DT <- merge(movies,credits,by="title")

# Storing movie titles in list to show it on UI (for Shiny R App)

movie_names=recommendation_DT$title
movie_names=as.list(movie_names)
