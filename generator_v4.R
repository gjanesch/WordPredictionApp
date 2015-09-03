### Greg Janesch, updated 04/08/2015
### Description: a collection of text processing functions

library(plyr)
library(dplyr)

### Function description: Reads in the sentence files one at a time
unigramGenerator_v4 <- function(){

    print(paste(Sys.time(), "  Starting", sep = " "))
    
    emoticonregex <- "o[._]o|[:;8=]d|[:;][-']d|:[opc]|:'[()]|d[:;]"
    profanity <- readLines("profanity.txt")

    types <- c("blogs","news","twitter")
    files <- paste("sentences_", types, ".txt", sep = "")

    unigrams <- character(0)

    for(file in files){

        print(paste(Sys.time(), "  Reading in", file, sep = " "))
        lines <- readLines(file)

        print(paste(Sys.time(), "  Initial filtering", sep = " "))
        words <- filter_v4(lines, profanity, emoticonregex)

        unigrams <- c(unigrams, words)
        words <- NULL

        print(paste(Sys.time(), "  Done with", file, sep = " "))

    }

    print(paste(Sys.time(), "  Tabulating", sep = " "))
    unigram_table <- table(unigrams)

    print(paste(Sys.time(), "  Writing", sep = " "))
    writeLines(unigrams, "unigrams.txt")
    write.table(unigram_table[-1,], "unigram_table.txt")

    print(paste(Sys.time(), "  Done.", sep = " "))

}

filter_v4 <- function(lines, profanity, emoticonregex){
    
    lines <- tolower(lines)

    lines <- gsub(lines, pattern = "[<>]", rep = "")
    
    lines <- paste(lines, "<b>", sep = " ", collapse = " ")
    
    print(paste(Sys.time(), "  Splitting", sep = " "))
    words <- unlist(strsplit(lines, split = " "))
    
    lines <- NULL
    
    print(paste(Sys.time(), "  Additional filtering", sep = " "))
    
    words[words == "&"] <- "and"
    
    ## Remove words with more than two consecutive occurrences of a single letter; any instances are presumed to be not real english
    words[grep(words, pattern = "\\.{2,}")] <- ""
    
    ## Strip all characters apart from letters, apostrophes, and <>
    words <- gsub(words, pattern = "[^[:alpha:][:space:]'<>]", replacement = "")
    words <- gsub(words, pattern = "^[^a-zA-Z<]+|[^a-zA-Z>]+$", replacement = "")
    
    words[words %in% profanity] <- ""
    
    multiplechars <- grep(words, pattern = "([[:alpha:]])(\\1{2,})")
    words[multiplechars] <- ""
    
    words[words == ""] <- "<b>"
    
    return(words)
    
    
}

## used uniLim = 100
ngramGenerator_v4 <- function(uniLim = 0){

    library(plyr)
    library(dplyr)

    ## Read in the unigrams
    print(paste(Sys.time(), "  Reading in", sep = " "))
    unigrams <- readLines("unigrams.txt")
    n <- 1:length(unigrams)
    

    ## Remove infrequent unigrams, substitute <b> so later filters get it
    if(uniLim > 0){
        print(paste(Sys.time(), "  Removing infrequent terms", sep = " "))
        unigram_table <- read.table("unigram_table.txt")
        infrequentwords <- unigram_table[unigram_table$Freq <= uniLim, "unigrams"]
        unigrams[unigrams %in% infrequentwords] <- "<b>"
        unigram_table <- NULL
        print(paste("Unique unigrams: ", length(unique(unigrams))))
    }

    ## Generate and write the dictionary for this case
    print(paste(Sys.time(), "  Creating and writing dictionary", sep = " "))
    unique_words <- sort(unique(unigrams))
    dictionary <- data.frame(index = 0:(length(unique_words)-1), word = unique_words)
    write.table(dictionary[-1,], "dictionary.txt")

    ## Substitute everything in the unigrams vector for the corresponding
    unigrams <- as.integer(mapvalues(unigrams, dictionary$word, dictionary$index))

    
    ## Find occurrences of <b> to figure out where to filter
    print(paste(Sys.time(), "  Finding blanks", sep = " "))
    blanks <- which(unigrams == 0L)


    ## Find the indices where valid bigrams begin
    print(paste(Sys.time(), "  Bigrams - getting start indices", sep = " "))
    not_bigram <- union(blanks, blanks - 1)
    not_bigram <- not_bigram[order(not_bigram)]
    bigram_start <- n[-not_bigram]
    print(paste("Bigram count: ", length(bigram_start), sep = ""))


    ## Generate & write the bigrams to a file
    n_bi <- length(bigram_start)
    bi_loops <- c(seq(from = 1, to = n_bi, by = 500000), n_bi)
    iter <- length(bi_loops) - 1
    for(point in 1:iter){
        print(paste(Sys.time(), "  Generating & writing bigrams - loop", point, "of", iter, sep = " "))
        point_start <- bigram_start[bi_loops[point]:bi_loops[point+1]]
        bigrams <- paste(unigrams[point_start], unigrams[point_start + 1], sep = " ")
        write(bigrams, "bigrams.txt", append = TRUE)
    }
    bigrams <- NULL
    bigram_start <- NULL

    ## Find the indices where valid trigrams begin
    print(paste(Sys.time(), "  Trigrams - getting start indices", sep = " "))
    not_trigram <- union(not_bigram, blanks - 2)
    not_trigram <- not_trigram[order(not_trigram)]
    trigram_start <- n[-not_trigram]
    not_bigram <- NULL
    print(paste("Trigram count: ", length(trigram_start), sep = ""))


    ## Generate and write the trigrams; needs to be done via loop
    n_tri <- length(trigram_start)
    tri_loops <- c(seq(from = 1, to = n_tri, by = 500000), n_tri)
    iter <- length(tri_loops) - 1
    for(point in 1:iter){
        print(paste(Sys.time(), "  Generating & writing trigrams - loop", point, "of", iter, sep = " "))
        point_start <- trigram_start[tri_loops[point]:tri_loops[point+1]]
        trigrams <- paste(unigrams[point_start], unigrams[point_start + 1], unigrams[point_start + 2], sep = ",")
        write(trigrams, "trigrams.txt", append = TRUE)
    }
    trigrams <- NULL
    trigram_start <- NULL


    ## Find the indices where valid tetragrams begin
    print(paste(Sys.time(), "  Tetragrams - getting start indices", sep = " "))
    not_tetragram <- union(not_trigram, blanks - 3)
    not_tetragram <- not_tetragram[order(not_tetragram)]
    tetragram_start <- n[-not_tetragram]
    not_trigram <- NULL
    not_tetragram <- NULL
    print(paste("Tetragram count: ", length(tetragram_start), sep = ""))
    

    ## Generate and write the tetragrams; needs to be done via loop
    n_tetra <- length(tetragram_start)
    tetra_loops <- c(seq(from = 1, to = n_tetra, by = 500000), n_tetra)
    iter <- length(tetra_loops) - 1
    for(point in 1:iter){
        print(paste(Sys.time(), "  Generating & writing tetragrams - loop", point, "of", iter, sep = " "))
        point_start <- tetragram_start[tetra_loops[point]:tetra_loops[point+1]]
        tetragrams <- paste(unigrams[point_start], unigrams[point_start + 1], unigrams[point_start + 2], unigrams[point_start + 3], sep = ",")
        write(tetragrams, "tetragrams.txt", append = TRUE)
    }
    tetragrams <- NULL
    tetragram_start <- NULL

    print(paste(Sys.time(), "  Done", sep = " "))

}

