# Session 2 Hands On

## 1.1. Getting the data

caps <- readRDS(file="Qcaps.rds")
text_part1 <- paste(unlist(caps[53:89]), collapse="\n")
text_part2 <- paste(unlist(caps[90:126]), collapse="\n")

## 1.2. Creating a BPE model

library(tokenizers.bpe)
#I can't use a single line with all the text (text_part1), but I can use a vector of chapters
model <- bpe(unlist(caps[53:89]))
#We apply the model to the second part of the text (here we can use a single string)
subtoks2 <- bpe_encode(model, x = text_part2, type = "subwords")
head(unlist(subtoks2), n=20)

# Fix underscore
niceSubwords <- function(strings){
  gsub("\U2581", "_", strings)
}
niceSubwords(head(unlist(subtoks2), n=20))

library(sentencepiece)
#We will use the spanish language model to get a good identification of sentences.
library(spacyr)
#spacy_install(prompt=FALSE)
#spacy_download_langmodel('es')
spacy_initialize(model = "es_core_news_sm") #Downloaded by spacy_download_langmodel('es')
sentences_part1 <- spacy_tokenize(text_part1, what="sentence") #Returns a list
v_sentences_part1 <- unlist(sentences_part1) #We get 2401 sentences
#Write the sentences in a file
train_file <- "Qsentencepiece.BPE.part1.txt"
writeLines(text = v_sentences_part1, con = train_file)
#Create a BPE model
model <- sentencepiece(train_file,
                       type = "bpe",
                       coverage = 0.9999,
                       vocab_size = 5000,
                       #threads = 1,
                       model_dir = getwd(),
                       verbose = FALSE)

subtoks2_sentencepiece <- sentencepiece_encode(model, x = text_part2, type = "subwords")
niceSubwords(head(unlist(subtoks2_sentencepiece), n=20)) #tokenization made by sentencepiece

model <- bpe(v_sentences_part1)
subtoks2_alt <- bpe_encode(model, x = text_part2, type = "subwords")
niceSubwords(head(unlist(subtoks2_alt), n=20))

v <- unlist(subtoks2_alt)
strwrap(niceSubwords(substring(paste(v, collapse = " "), 1, 200)), exdent = 2)

#The pipe quivalent is this
library(magrittr) #although it
paste(v, collapse = " ") %>%
  substring(1, 200) %>%
  niceSubwords() %>%
  strwrap(exdent = 2)

# 2. Distance between texts

#Creates a quanteda corpus
library(quanteda)
texts_caps <- unlist(caps)
names(texts_caps) <- paste("Chap.", 1:length(texts_caps)) #assigns a name to each string
corpus_capsQ <- corpus(texts_caps)
docvars(corpus_capsQ, field="Chapter") <- 1:length(texts_caps) #docvar with chapter number
corpus_capsQ

#Creates the dfm (document-feature matrix)
dfm_capsQ <- dfm(tokens(corpus_capsQ),
                 #Default values:
                 # tolower = TRUE
                 #Convers to lowercase
                 # remove_padding = FALSE #Does padding (fills with blanks)
)
#Does a dendrogram
distMatrix <-dist(as.matrix(dfm_capsQ),
                  method="euclidean")
groups <-hclust(distMatrix , method="ward.D")

## 2.1. Dendrograms

plot(groups,
     cex =0.25, #Size of labels
     hang= -1, #Same hight labels
     xlab = "", #Text of axis x
     ylab = "", #Text of axis y
     main = "" #Text of drawing
)
rect.hclust(groups, k=4)

plot(groups,
     cex =0.25, #Size of labels
     hang= -1, #Same hight labels
     xlab = "", #Text of axis x
     ylab = "", #Text of axis y
     main = "" #Text of drawing
)
rect.hclust(groups, k=10)

## 2.2. Most frequent (and infrequent) features

topfeatures(dfm_capsQ)

#Without puntuation marks
dfm_capsQ_1 <- dfm(tokens(corpus_capsQ,
                          remove_punct = TRUE
                          #Default values:
                          # remove_punct = FALSE,
                          # remove_symbols = FALSE,
                          # remove_numbers = FALSE,
                          # remove_url = FALSE,
                          # remove_separators = TRUE,
                          # split_hyphens = FALSE
                          ),
#Default values:
# tolower = TRUE
#Convert to lowercase
# remove_padding = FALSE #Does padding (fill up blanks)
              )
#Without stop words

dfm_capsQ_2 <- dfm_remove(dfm_capsQ_1, stopwords("es"))
topfeatures(dfm_capsQ_2)

topfeatures(dfm_capsQ_2,
            decreasing = FALSE #By default it is TRUE
)

## 2.3. Using docvars

corpus_part1 <- corpus_subset(corpus_capsQ,
                              Chapter < 53 #I keep chaps from 1 to 52
)
corpus_part2 <- corpus_subset(corpus_capsQ,
                              Chapter > 52 #I keep chaps from 53 to end
)
dfm_part1_noPunct <- dfm(tokens(corpus_part1, remove_punct = TRUE))
dfm_part2_noPunct <- dfm(tokens(corpus_part2, remove_punct = TRUE))
dfm_part1_noPunct_noSW <- dfm_remove(dfm_part1_noPunct, stopwords("es"))
dfm_part2_noPunct_noSW <- dfm_remove(dfm_part2_noPunct, stopwords("es"))
#Most frequent feat
topfeatures(dfm_part1_noPunct_noSW)

topfeatures(dfm_part2_noPunct_noSW)

#Less frequent feat
topfeatures(dfm_part1_noPunct_noSW, decreasing = FALSE)

topfeatures(dfm_part2_noPunct_noSW, decreasing = FALSE)

# 3. Document classification

## 3.1. Naive Bayes

library(quanteda) #Required to read a corpus object
data_corpus_LMRD <- readRDS("data_corpus_LMRD.rds")
dfmat <- dfm(tokens(data_corpus_LMRD)) #50.000 docs x 149.653 feats
#Only uses 106MB RAM
dfmat_train <- dfm_subset(dfmat, set == "train")
dfmat_test <- dfm_subset(dfmat, set == "test")
library(quanteda.textmodels) #For textmodel_nb()
library(caret) #For confusionMatrix()
nbPredictions <- function(dist){ #dist = "multinomial" or "Bernoulli"
  #Compute a nb (Naive Bayes) model
  multi <- textmodel_nb(dfmat_train,
                        dfmat_train$polarity,
                        distribution = dist)
  #Predictions with the model
pred <- predict(multi, #the computed model
                  newdata = dfmat_test)
  
#Compute the confusion matriz for our prediction
confM <- confusionMatrix(pred, docvars(dfmat_test)$polarity)
#Accuracy is the number of labels (strings) that match...
my_acc_coincidences <- sum(as.character(pred) == as.character(docvars(dfmat_test)$polarity))
#...divided by the total number of labels
my_acc_total <- length(as.character(pred))
my_acc <- my_acc_coincidences/my_acc_total
my_acc #Sale 0.82876. Con "multinomial" era 0.81304
#Precision
precision <- confM$byClass['Pos Pred Value']
precision #Sale 0.7951591 (antes 0.878327)
#Recall
recall <- confM$byClass['Sensitivity']
recall #Sale 0.88568 (antes 0.8953488)
list(acc = my_acc, p = precision, r = recall)
}

nbPredictions("multinomial")

nbPredictions("Bernoulli")

#Like before but using tf-idf
dfmat <- dfm_tfidf(dfmat, #Over the previous dfmat, compute tf-idf
                   scheme_tf = "prop" #By default it is "count". The good one for TF-IDF is "prop"
)
dfmat_train <- dfm_subset(dfmat, set == "train")

dfmat_test <- dfm_subset(dfmat, set == "test")
nbPredictions("multinomial")

nbPredictions("Bernoulli")

## 3.2. SVM Model

#Compute a SVM model
multi <- textmodel_svm(dfmat_train,
                       dfmat_train$polarity,
                       weight = "uniform") #Default value. Other options: "docfreq", "termfreq".

set.seed(123) #Reproducible results
svmPredictions <- function(x,
                           #Sample size
                           weight){ #weight can be "uniform", "docfreq" or "termfreq".
  
  #Instead of using all the documents marked as train, I take only x documents
  dfmat_train <- dfm_sample(dfm_subset(dfmat, set == "train"),
                            x #Sample size
  )
  dfmat_test <- dfm_subset(dfmat, set == "test")
  multi <- textmodel_svm(dfmat_train,
                         dfmat_train$polarity,
                         weight = weight)
  pred <- predict(multi,
                  newdata = dfmat_test)
  confM <- confusionMatrix(pred, docvars(dfmat_test)$polarity)
  my_acc_coincidences <- sum(as.character(pred) == as.character(docvars(dfmat_test)$polarity))
  my_acc_total <- length(as.character(pred))
  my_acc <- my_acc_coincidences/my_acc_total
  precision <- confM$byClass['Pos Pred Value']
  recall <- confM$byClass['Sensitivity']
  list(acc = my_acc, p = precision, r = recall)
}

svmPredictions(10000, "uniform")

svmPredictions(10000, "docfreq")
#svmPredictions(10000, "termfreq") #Produces an error

