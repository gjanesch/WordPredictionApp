### Greg Janesch, updated 4/22/2015
### Provides two functions for the actual prediction for the app


### Description: takes the input phrase, cleans it, and passes it to the nextWord() function
### with the correct parameters
makePrediction <- function(input, dictionary, bigrams, trigrams, tetragrams){
    
    ## Start by cleaning the input of non-alphabetic characters (except apostrophes) and leading
    ## or trailing whitespace
    input <- gsub("[^a-zA-Z']", " ", input)
    input <- tolower(input)
    input <- gsub(x = input, pattern = "^\\s*|\\s*$", replace = "")
    
    ## Split into words and try to match with the dictionary
    words <- unlist(strsplit(input, split = "\\s{1,}"))
    indices <- dictionary[match(words, dictionary$word), "index"]
    
    ## Start with predicting using the last three words of the input if possible, reducing to two
    ## or one word as needed; if no matches are found, return the word "the"
    if(length(indices) >= 3){
        print("Length 3 - tetragram")
        predictedWord <- nextWord(indices, tetragrams, dictionary)
        if(is.na(predictedWord)){
            print("reduce to length 2")
            predictedWord <- nextWord(indices, trigrams, dictionary)
            if(is.na(predictedWord)){
                print("reduce to length 1")
                predictedWord <- nextWord(indices, bigrams, dictionary)
                if(is.na(predictedWord)){
                    print("just send back the")
                    predictedWord <- "the"
                }
            }
        }
    }else if(length(indices) == 2){
        print("Length 2 - trigram")
        predictedWord <- nextWord(indices, trigrams, dictionary)
        if(is.na(predictedWord)){
            predictedWord <- nextWord(indices, bigrams, dictionary)
            if(is.na(predictedWord)){
                predictedWord <- "the"
            }
        }
    }else if(length(indices) == 1){
        print("Length 1 - bigram")
        predictedWord <- nextWord(indices, bigrams, dictionary)
        if(is.na(predictedWord)){
            predictedWord <- "the"
        }
    }else{
        print("Length 0 - \"the\"")
        predictedWord <- "the"
    }
    
    return(predictedWord)
    
}


## Description: compares the input string to one of the ngram tables and tries to find a match
nextWord <- function(indices, ngrams, dictionary){
    
    n <- ncol(ngrams)
    
    if(length(indices) > n-1){
        indices <- tail(indices, n-1)
    }
    
    logicalVector <- !logical(nrow(ngrams))
    for(i in 1:(n-1)){
        logicalVector <- logicalVector & (ngrams[[i]] == indices[i])
        print(sum(logicalVector))
    }
    location <- match(TRUE, logicalVector)
    print(location)
    if(!is.na(location)){
        location <- ngrams[[location,n]]
        print(location)
        nextWord <- dictionary[location, "word"]
    }else{
        nextWord <- NA
    }
    
    print(nextWord)
    
    return(nextWord)
}