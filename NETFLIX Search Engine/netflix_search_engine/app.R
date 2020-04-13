
###################### This entire Shiny R code is written by Kishore Kumar Biradavolu #############################################  
#Here it takes input and runs the script2 everytime to show different outputs###########################
########################################################################################################

# Load packages ----
library(shiny)
library(maps)
library(mapproj)
library(shinyWidgets)
# Load data ----

#movies<- read.csv("data/tmdb_5000_movies.csv")
#credits<- read.csv("data/tmdb_5000_credits.csv")
#combined<- read.csv("data/recommendation_DT.csv")
#keywords<- read.csv("data/KeyWords_Engine.csv")
#genre <- read.csv("data/Frequent_genre.csv")
#key_modifed <- read.csv("data/After_Synonyms_Combied Frequencies.csv")


# Source helper functions -----
#source("Packages_Libraries.R")
#source("Search_Engine_Script1.R")
#source("Search_Engine_Script2.R")
#source("Search_Engine_Script3.R")
source("Script2.R")

# User interface ----
ui <- fluidPage(
  
  setBackgroundColor("black"),
  setBackgroundImage(src = "NETFLIX11.png"),
  
  titlePanel(span("Welcome back,Breaking Data!", style = "color:white")),
  
  sidebarLayout(
    sidebarPanel(
      
      # fluidRow(
      #   
      #   column(10, 
      #          textInput("user_movie_id", h3("Search Engine"), 
      #                    value = "Enter a movie id..."))   
      # ),
      # fluidRow(
      #   column(12, 
      #          helpText("Note:You can see the recommended movies to the right of the search engine.The size of the movies shows the closest match for the movie id (you have entered)."))
      # ),br(),
      # 
      # fluidRow(
      #   column(12, 
      #          helpText(strong("From the below , select the number of movies you are interested to see as recommendation.")))
      # ),
      
      fluidRow(
        column(12,selectInput("var", 
                              label = "Enter or Chose a movie name for recommendations",
                              choices = movie_names,
                              selected = "Avatar") )
        
      ),
      
      fluidRow(
        column(12, 
               helpText(span(strong("Note:", style = "color:blue")),"You can see the recommended movies to the below right of the select area."))
      ),
      code('we recommend to select from the list and not to enter manually')
    ),
    
    mainPanel(
      img(src = "NETFLIX2.png", height = 300, width = 800),
      br(),
      br(),
      fluidRow(
        column(12, 
               h2("Below are the Recommendations :",style="color:yellow"))
      ),br(),
      
      strong(h5(tableOutput('table'),style="color:red"))
      
      
    )
  )
)

# Server logic ----
server <- function(input, output) {
  
  moviereact<-reactive({moviefunc(moviename=input$var)})
  
  output$table<-renderTable({data.frame(moviereact())},striped = TRUE, bordered = FALSE,  
                            hover = TRUE, spacing = 'l',width = '300',  
                            align = 'c',  
                            colnames = FALSE)
}

# Run app ----
shinyApp(ui, server)