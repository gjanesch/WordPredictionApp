### Greg Janesch, updated 4/22/2015
### Description: UI portion of word prediction app


library(shiny)


shinyUI(fluidPage(
    
    titlePanel("Word Prediction App"),
    

    ## Description of the app, as well as the input field
    sidebarPanel("This app is designed to predict the next word in a sentence based on an input sequence of words.",
                 br(),
                 br(),
                 "To predict a word, input part of a sentence into the text bar below, then press \"Submit\" to get the prediction.",
                 br(),
                 br(),
                 br(),
                 textInput("sentence", "Predict next word for:", value = "This is a test"),
                 submitButton("Submit")
                 ),
    
    ## The output text for the app, plus a brief summary of how it works
    mainPanel(
        #textOutput("Prediction for the next word:"),
        #htmlOutput("recite"),
        h3("Input sentence: ", span(textOutput("recite"), style = "color:blue")),
        h3("Predicted next word: ", span(textOutput("prediction"), style = "color:blue")),
        br(), br(), br(),
        h3("How It Works"),
        "This app takes the input phrase and compares it against a collection of known phrases derived from a sample data set.",
        "The sample phrases are two, three, and four words in length; as a result, the app compares the (last) three words of the input to the sample data.",
        "If no match is found, it backs off to the three-word phrases, then the two-word ones.  If a match is not found in any of those, the single most common word, \"the\", is returned."
        )
    
))