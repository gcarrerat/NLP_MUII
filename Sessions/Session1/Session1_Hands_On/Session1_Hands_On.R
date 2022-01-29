# Session 1 Hands On

## Read test from Internet and selection of text

urlQuijoteGutenber <- "https://www.gutenberg.org/files/2000/2000-0.txt" 
lines <- readLines(urlQuijoteGutenber,encoding = "UTF-8") #It takes a few seconds
grep(pattern = "***", lines, fixed = TRUE) 
#Warning! Without fixed the regex is "\\*\\*\\*"
# Result: [1]    24 37704 37706 lines ids
linesQ <- lines[25:37703] 
length(linesQ) 
# Result: [1] 37679

grep(pattern = "En un lugar de", linesQ, fixed = TRUE) 
#Lines 1045 and 13513. The good one is the first one

linesQ <- linesQ[-c(1:1044)] # Remove the prologue
length(linesQ) # 36,635

linesQ[1:5]

paste(linesQ[1:5], collapse = " ")

# Basic checks
library(utf8)
# Check encoding
linesQ[!utf8_valid(linesQ)]

# Check character normalization. Specifically, the normalized composed from (NFC)
linesQ_NFC <- utf8_normalize(linesQ)
sum(linesQ_NFC != linesQ) #0 means all right. The text is in NFC

## 3. Basic Structuration
stringQ <- paste(linesQ, collapse = "\n") #One big string
paragraphs <- unlist(strsplit(stringQ, "\\n\\n\\n"))  #Warn! (1)strsplit returns a list,
                                                      #      (2)escape \n and
                                                      #      (3)by default, fixed=FALSE
                                                      #Using fixed=TRUE this should be
                                                      #     "\n\n\n", fixed = TRUE
parEmpty <- which(paragraphs == "") #No empty paragraphs #paragraphs <- paragraphs[-parEmpty]
length(paragraphs) # 128

substring(paragraphs[1], 1, 200)

## 4. Some cleaning

#Testing the regex
gsub("[\n]{1,}", " ", c(par1="with one \nbut also\n", 
                        par2="with a seq of \n\nlike this"
                      )
)

paragraphswoNL <- gsub("[\n]{1,}", " ", paragraphs) #wo = without 
substring(paragraphswoNL[1], 1, 200)

paragraphs <- gsub("[ ]{2,}", " ", paragraphswoNL) #We reassign the varible paragraphs 
substring(paragraphs[1], 1, 200)

library(spacyr)
#Use spacy_install() if you have never used spacyr before. This will install a miniconda environment
#spacy_download_langmodel('es') #This downloads the model es_core_news_sm to disk 
spacy_initialize(model = "es_core_news_sm") #Loads the Spanish model fron disk
#Gets sentences from paragraphs
phrases <- spacy_tokenize(paragraphs,
                          #If you use quanteda you can use
                          #  corpus_reshape(corpus, to = "sentences"))
                          #Taks a while.
                          #Returns a list with 138 elements, each one
                          #        is a string vector.
                          what="sentence" #By default remove_separators = TRUE # (removes trailing spaces)
)
v_phrases <- unlist(phrases)
numphrases <- length(v_phrases) #8,975 sentences sum(v_phrases=="") #1
sum(v_phrases=="") #1

v_phrases <- v_phrases[-which(v_phrases=="")] #8,974 sentences

#A simple histogram will do fine
hist(nchar(v_phrases),
     main = "Histogram of sentence size",
     xlab = "Sentece size (number of characters)", ylab = "Ocurrences"
)

tokens <- spacy_tokenize(paragraphs
                         #Parameters asigned by default:
)#Returns a list v_tokens <- unlist(tokens)
v_tokens[1:10]

length(v_tokens) #442,164 tokens (many repeated)

length(unique(v_tokens)) #24,130 different (unique) tokens.

#As a list
head(sort(table(v_tokens), decreasing = TRUE), n = 25)





