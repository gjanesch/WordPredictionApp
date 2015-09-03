### Greg Janesch, updated 4/22/2015
### Description: server file for the word prediction app


library(shiny)
library(data.table)
source("makePrediction.R")


shinyServer(function(input, output) {
    
    ## Load the necessary files
    dictionary <- read.table("dictionary.txt",stringsAsFactors = FALSE, header = TRUE)
    bigrams <- readRDS("bigram_table.rds")
    trigrams <- readRDS("trigram_table.rds")
    tetragrams <- readRDS("tetragram_table.rds")

    ## Predict the next word
    predictedWord <- reactive(makePrediction(input$sentence, dictionary, bigrams, trigrams, tetragrams))
    output$recite <- renderText(input$sentence)
    output$prediction <- renderText(predictedWord())
    
})