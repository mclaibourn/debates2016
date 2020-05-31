rm(list=ls())
library(tidyverse)
library(quanteda)
library(wordcloud)
library(wordcloud2)
library(RColorBrewer)

setwd("~/Box Sync/mpc/dataForDemocracy/debate2016analysis/")
load("debateSpeech2016.RData") # the corpus as created in acquire_debates.R

# subset for just HRC, DJT
candcorpus <- corpus_subset(debate16corpus, speaker=="HRC" | speaker=="DJT")

# create document feature matrix
debatedfm <- dfm(candcorpus, 
               remove = c(stopwords("english"), 
                          "applause", "laughter", "crosstalk", "laughther"),
               remove_punct = TRUE,
               remove_numbers = TRUE,
               verbose = TRUE)

# most frequent terms
topfeatures(debatedfm, n = 20, groups = "speaker")

# set up for figure
debatedf <- as.data.frame(debatedfm) # turn into dataframe
hrcdf <- rowSums(t(debatedf[1:13,])) # sum word use by candidate
djtdf <- rowSums(t(debatedf[14:27,]))
d <- as.data.frame(cbind(hrcdf, djtdf)) # combine into dataframe
d$word <- rownames(d)

# install.packages("webshot")
library(webshot)
webshot::install_phantomjs()
library("htmlwidgets")

# word frequency clouds (wordcloud2): this won't work without the image files
palB <- colorRampPalette(brewer.pal(9,"Blues"))(32)[13:32]
palO <- colorRampPalette(brewer.pal(9,"Oranges"))(32)[13:32]
# with figures!
d <- d[,c(3,1,2)] # re-order for use with wordcloud2
d.hrc <- d[,c(1,2)]
hrcfig = wordcloud2(head(d.hrc,300), color=palB, size = 0.75, figPath = "dImage.png")
hrcfig

# save it in html
saveWidget(hrcfig,"tmp.html", selfcontained = F)
# and in pdf
webshot("tmp.html","hrc_donkey.png", delay = 10, vwidth = 480, vheight=480)

d.djt <- d[,c(1,3)]
djtfig <- wordcloud2(head(d.djt,300), color=palO, size = 0.75, figPath = "rImage.jpeg")
djtfig

# save it in html
saveWidget(djtfig,"tmp.html", selfcontained = F)
# then in pdf
webshot("tmp.html","djt_elephant.pdf", delay = 10, vwidth = 768, vheight=446)

# word comparison cloud (wordcloud)
d <- as.data.frame(cbind(hrcdf, djtdf))
colnames(d) <- c("HRC", "DJT")
comparison.cloud(d, max.words=200, 
                 scale=c(4,.33), random.order=FALSE, 
                 colors=c("blue3", "orange3"), title.size=1)
