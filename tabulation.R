
library(plyr)
library(dplyr)

bigramTabler_v4 <- function(dictionary){

    for(begin in 0:14){

        print(paste(Sys.time(), "   Working on bigrams_", begin, sep = ""))

        filename <- paste("bigrams_", begin, ".txt", sep = "")
        target <- paste("bigrams_", sprintf("%02d",begin), "_tabulated.csv", sep = "")

        bigrams <- read.table(filename, sep = ",", header = FALSE)
        names(bigrams) <- c("w1","w2")
        bigram_table <- bigrams %>% count(w1,w2) %>% as.data.frame()
        bigram_table <- bigram_table %>% group_by(w1)
        bigram_table <- bigram_table %>% filter(n == max(n))
        bigram_table <- bigram_table %>% filter(w2 == max(dictionary[w2,"index"])) %>% as.data.frame()
        bigram_table <- bigram_table[,-3]

        write_header = FALSE
        if(begin == 0){write_header = TRUE}
        write.table(bigram_table, target, row.names = FALSE, col.names = write_header, sep = ",")

        rm(bigrams, bigram_table)
    }


}

trigramTabler_v4 <- function(dictionary){

    for(begin in 0:14){

        print(paste(Sys.time(), "   Working on trigrams_", begin, sep = ""))

        filename <- paste("trigrams_", begin, ".txt", sep = "")
        target <- paste("trigrams_", sprintf("%02d",begin), "_tabulated.csv", sep = "")

        trigrams <- read.csv(filename, header = FALSE)
        names(trigrams) <- c("w1","w2","w3")
        trigram_table <- trigrams %>% count(w1,w2,w3) %>% as.data.frame()
        trigram_table <- trigram_table[trigram_table$n > 1,]
        trigram_table <- trigram_table %>% group_by(w1,w2) %>% filter(n == max(n))
        trigram_table <- trigram_table %>% filter(w3 == max(dictionary[w3,"index"])) %>% as.data.frame()
        trigram_table <- trigram_table[,-4]

        write_header = FALSE
        if(begin == 0){write_header = TRUE}
        write.table(trigram_table, target, row.names = FALSE, col.names = write_header, sep = ",")

        rm(trigrams, trigram_table)
    }


}


tetragramTabler_v4 <- function(dictionary){

    for(begin in 0:14){

        print(paste(Sys.time(), "   Working on tetragrams_", begin, sep = ""))

        filename <- paste("tetragrams_", begin, ".txt", sep = "")
        target <- paste("tetragrams_", sprintf("%02d",begin), "_tabulated.csv", sep = "")

        tetragrams <- read.csv(filename, header = FALSE)
        names(tetragrams) <- c("w1","w2","w3","w4")
        tetragram_table <- tetragrams %>% count(w1,w2,w3,w4) %>% as.data.frame()
        tetragram_table <- tetragram_table[tetragram_table$n > 1,]
        tetragram_table <- tetragram_table %>% group_by(w1,w2,w3) %>% filter(n == max(n))
        tetragram_table <- tetragram_table %>% filter(w4 == max(dictionary[w4,"index"])) %>% as.data.frame()
        tetragram_table <- tetragram_table[,-5]

        write_header = FALSE
        if(begin == 0){write_header = TRUE}
        write.table(tetragram_table, target, row.names = FALSE, col.names = write_header, sep = ",")

        rm(tetragrams, tetragram_table)
    }


}
