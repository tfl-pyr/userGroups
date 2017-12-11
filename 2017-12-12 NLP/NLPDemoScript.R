#########################################
### TM, WORDCLOUD and STRINGDIST demo ###
#########################################

## wordcloud example based on https://datascienceplus.com/building-wordclouds-in-r/
## stringdist borrowed from Rob Duff...

# load packages (you'll need to install.packages if you don't already have the ones listed below!)
library(dplyr)
library(RColorBrewer)
library(stringdist)
library(tm)
library(wordcloud)

# set wd based on script location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load up our data
jepQ <- read.csv("Jeopardy.csv", header = TRUE, stringsAsFactors = FALSE)
tflApi <- read.csv("TFL_API_TubeStations.csv", header = TRUE, stringsAsFactors = FALSE)
cupid <- read.csv("CUPID_TubeStations.csv", header = TRUE, stringsAsFactors = FALSE)

##########################################
## Wordcloud -----
##########################################

# First we'll look at some text cleaning using tm, and then create a wordcloud using...wordcloud

# make a corpus
jeoCorp <- Corpus(DataframeSource(data.frame(as.character(jepQ$Question))))

# set to lower
jeoCorp <- tm_map(jeoCorp, content_transformer(tolower))

# clean up by removing punctuation and english stopwords eg 'all', 'and', 'off', 'the'
jeoCorp <- tm_map(jeoCorp, removePunctuation)
#jeoCorp <- tm_map(jeoCorp, removeNumbers)
jeoCorp <- tm_map(jeoCorp, PlainTextDocument)
jeoCorp <- tm_map(jeoCorp, removeWords, stopwords("english"))

# now we stem (learning -> learn, jumping -> jump)
jeoCorp <- tm_map(jeoCorp, stemDocument)

# and now finally we wordcloud, dropping it out as a png
png("myWordCloud.png", width = 1280, height = 800)
wordcloud(jeoCorp, scale = c(5,0.5), max.words = 100, random.order = FALSE, colors = brewer.pal(8, "Dark2"))
dev.off()

##########################################
## stringdist -----
##########################################

# Now let's look at using stringdist using TFL_API and CUPID data
# first we're going to match by a traditional join
tflApi$Station <- toupper(tflApi$Station)
cupidJoined <- left_join(cupid, tflApi, by = "Station")

# did we miss any? (HINT: Yes we did)
notJoined <- cupidJoined[is.na(cupidJoined$id),"Station"]
notJoined

# We can look at different techniques to see how approximate (FUZZY!!) one string is matched to another
# Check out https://www.joyofdata.de/blog/comparison-of-string-distance-algorithms/
# You can also use ?stringdist-metrics for the help manual
# Rob has outlined two examples but you may also want to look at QGRAMS
# Each technique will have its own advantages so you can chose the best one for your situation

# simplist approach - HAMMING, str lengths need to be identical. position penalty only.
stringdist("WSTMNSTR", "WESTMINSTER", method = "hamming")
stringdist("WASTMANSTAR", "WESTMINSTER", method = "hamming")
stringdist("RWESTMINSTE", "WESTMINSTER", method = "hamming") # poor score yet good match!

# not so simple RESTRICTED DAMERAU-LEVENSHTEIN or OSA
stringdist("RWESTMINSTE", "WESTMINSTER", method = "osa") # our default

# Back to the example from our data - using default method (looks at number of insertions, deletions, replacements
# needed to turn a into b - allows neighbouring symbols to be swapped)

# first example
notJoined[1]
stringdist(notJoined[1], tflApi$Station) # get the scores on the doors
min(stringdist(notJoined[1], tflApi$Station)) # identify the smallest number/nearest match
which(stringdist(notJoined[1], tflApi$Station) == min(stringdist(notJoined[1], tflApi$Station))) # find it
tflApi$Station[which(stringdist(notJoined[1], tflApi$Station) == min(stringdist(notJoined[1], tflApi$Station)))] # check it

# second example
notJoined[5]
tflApi$Station[which(stringdist(notJoined[5], tflApi$Station) == min(stringdist(notJoined[5], tflApi$Station)))] # check it

# seems to work fairly well, let's complete the table
for (i in 1:nrow(cupidJoined)){
  cupidJoined$FuzzyStation[i] <- tflApi$Station[which(stringdist(cupidJoined$Station[i], tflApi$Station) == min(stringdist(cupidJoined$Station[i], tflApi$Station)))]
}

# and finally let's show some quick QGRAM examples (look more closely in own time!)
# note that q is the size of the qgram - change the numbers below and see what happens!
stringdist::qgrams("the quick brown fox jumps over the lazy dog", q = 1)
stringdist::qgrams("Crazy Fredrick bought many very exquisite opal jewels", q = 5)
