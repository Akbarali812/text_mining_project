pacman::p_load(pacman, tm, SnowballC, dplyr, wordcloud, RColorBrewer)
# Import data
bookSS <- readLines('F:/MS Course/System Simulation/Presentation/PrideandPrejudice.txt')

corpusSS <- Corpus(VectorSource(bookSS)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords("english")) %>%
  tm_map(stripWhitespace) %>%
  tm_map(stemDocument)

# Create term-document matrices and remove sparse terms
tdmSS <- DocumentTermMatrix(corpusSS) %>%
  removeSparseTerms(1 -(5/length(corpusSS)))

# Calculate and sort by word frequencies
word.freqSS <- sort(colSums(as.matrix(tdmSS)), decreasing = T)


# Create frequency table
tableSS <- data.frame(word = names(word.freqSS),
                      absolute.frequency = word.freqSS,
                      relative.frequency = word.freqSS/length(word.freqSS))
subset(tableSS, absolute.frequency>80)    %>%
  
  ggplot(aes(word, absolute.frequency)) +
  geom_bar(stat="identity", fill="darkblue", colour="darkgreen") +
  theme(axis.text.x=element_text(angle=45, hjust=1))
# Remove the words from the row names
row.names(tableSS) <- NULL

#Show the 15 most common words
head(tableSS, 15)

# Export the 1000 most common words in csv file
write.csv(tableSS[1:1000, ], "SS_1000.csv")


################################################################################
set.seed(42)
# Limit words by specifying min frequency
wordcloud(names(word.freqSS),word.freqSS, min.freq=100)

# Add color
wordcloud(names(word.freqSS),word.freqSS,min.freq=40,colors=brewer.pal(5,"Dark2"))

rm(list = ls())
