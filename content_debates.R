#####################
# 2016 Debate Text
# Michele Claibourn
# Debate Themes
#####################

rm(list=ls())
library(tidyverse)
library(quanteda)
library(scales)
library(wordcloud)
library(wordcloud2)
library(RColorBrewer)
library(stm)

setwd("~/Box Sync/mpc/datafordemocracy/debate2016analysis/")
load("debateSpeech2016Sentiment.RData")

# subset for just HRC, DJT
candcorpus <- corpus_subset(debate16corpus, speaker=="HRC" | speaker=="DJT")


#########################
# Examining the Substance
# (quanteda)
#########################
### key words in context
kwic(corpus_subset(candcorpus, speaker=="HRC"), "immig", 4, "regex") 
kwic(corpus_subset(candcorpus, speaker=="DJT"), "immig", 4, "regex")

kwic(corpus_subset(candcorpus, speaker=="HRC"), "family", 4)
kwic(corpus_subset(candcorpus, speaker=="DJT"), "family", 4)

kwic(corpus_subset(candcorpus, speaker=="HRC"), "hands", 3)
kwic(corpus_subset(candcorpus, speaker=="DJT"), "hands", 3)

### frequent words
# create document feature matrix
otherStop("applause", "laughter", "crosstalk", "laughther")
debatedfm <- dfm(candcorpus, 
               remove = c(stopwords("english"), otherStop),
               remove_punct = TRUE,
               remove_numbers = TRUE,
               verbose = TRUE)
debatedfm

# most frequent terms
topfeatures(debatedfm, n = 25, groups = "speaker")

# setting up for figures
debatedfm <- rbind(hrcdfm1, djtdfm1) # bind dfms
debatedfm
debatedf <- as.data.frame(debatedfm) # turn into dataframe

hrcdf <- rowSums(t(debatedf[1:13,])) # sum word use by candidate
djtdf <- rowSums(t(debatedf[14:27,]))

d <- as.data.frame(cbind(hrcdf, djtdf)) # combine into dataframe
d$word <- rownames(d)
d <- d[,c(3,1,2)]

# word frequency dot plot
d <- arrange(d, desc(hrcdf))
pc <- ggplot(d[1:25,], aes(x = reorder(word, hrcdf), y = hrcdf))
pc + geom_point() + coord_flip() + ggtitle("Clinton's Top Words") + labs(y = "Word Counts", x = "Top Words")

d <- arrange(d, desc(djtdf))
pt <- ggplot(d[1:25,], aes(x=reorder(word, djtdf), y=djtdf))
pt + geom_point() + coord_flip() + ggtitle("Trump's Top Words") + labs(y = "Word Counts", x = "Top Words")

# word frequency clouds (wordcloud2): this won't work without the image files
palB <- colorRampPalette(brewer.pal(9,"Blues"))(32)[13:32]
palO <- colorRampPalette(brewer.pal(9,"Oranges"))(32)[13:32]
# with figures!
d.hrc <- d[,c(1,2)]
wordcloud2(head(d.hrc,200), color=palB, figPath = "dImage.png")
d.djt <- d[,c(1,3)]
wordcloud2(head(d.djt,200), color=palO, figPath = "rImage.jpeg")

# word comparison cloud (wordcloud)
d <- as.data.frame(cbind(hrcdf, djtdf))
colnames(d) <- c("HRC", "DJT")
comparison.cloud(d, max.words=200, 
                 scale=c(4,.33), random.order=FALSE, 
                 colors=c("blue3", "orange3"), title.size=1)


######################
# Ideological Scaling:
# wordfish (quanteda)
######################
# Create DFM
debateDfm2 <- dfm(debate16corpus, 
               remove = c(stopwords("english"), otherStop),
               remove_punct= TRUE,
               remove_numbers = TRUE,
               stem = TRUE)
debateDfm2

debateWF <- textmodel_wordfish(debateDfm2, dir=c(14,29)) # chose HRC, DJT 10/19 debate
debateWF
debateWFdf <- as.data.frame(cbind(debateWF@docs,debateWF@theta, debateWF@se.theta))
names(debateWFdf) <- c("id", "position", "se")
debateWFdf$id <- as.character(debateWFdf$id)
debateWFdf$position <- as.numeric(as.character(debateWFdf$position))
debateWFdf$se <- as.numeric(as.character(debateWFdf$se))

# Add to original data frame
debates16 <- cbind(debates16, debateWFdf)
# And plot!
p <- ggplot(debates16, aes(x = date, y = position))
p + geom_point(aes(color=party, shape=speaker), size=3) +
  scale_x_date(labels = date_format("%m/%d"), breaks = date_breaks("5 weeks")) + # (scales)
  ggtitle("Ideological Position of Candidate Debate Speech") + 
  labs(y = "Left (neg) to Right (pos) Position", x = "Date of Debate") +
  scale_color_manual(values=c("blue3", "orange3"), name="Speaker", guide=guide_legend(reverse = TRUE)) +
  scale_shape_manual(values=c(19,19,1,1)) + guides(shape=FALSE) +
  theme(plot.title = element_text(face="bold", size=20, hjust=0)) + 
  theme(axis.title = element_text(face="bold", size=16)) + 
  theme(panel.grid.minor = element_blank(), legend.position = c(0.77,0.9), legend.text=element_text(face="bold", size=12))
ggsave(file="figures/location.png")
# needs some work, exploration, further development!

# Pull words, weights, and fixed effects
wordWFdf <- as.data.frame(cbind(debateWF@features, debateWF@beta, debateWF@psi))
names(wordWFdf) <- c("word", "weight", "fixed")
wordWFdf$word <- as.character(wordWFdf$word)
wordWFdf$weight <- as.numeric(as.character(wordWFdf$weight))
wordWFdf$fixed <- as.numeric(as.character(wordWFdf$fixed))

p <- ggplot(wordWFdf, aes(x=weight, y=fixed, label=word))
p + geom_text()


#################################
# Structural Topic Model (stm;
# alternatives: topicmodels, lda)
#################################
debateDocVars <- docvars(debate16corpus)
debateDocVars$speech <- debates16$speech
moderators <- c("chris", "anderson", "martha", "lester", "elaine")
debatedfm <- dfm(debate16corpus, 
                 remove = c(stopwords("english"), moderators, otherStop),
                 remove_punct = TRUE,
                 stem = TRUE)
debatedfm
debatedfm <- dfm_trim(debatedfm, min_count = 2)

debOut <- convert(debatedfm, to = "stm", 
                  docvars = debateDocVars)

## Explore
# searchK estimates topic exclusivity, coherence; model likelihood, residuals for each k
debEval <- searchK(debOut$documents, debOut$vocab, K = seq(10,75,5), 
                   prevalence = ~ as.factor(party) + as.factor(season), 
                   data = debOut$meta, init.type = "Spectral")
plot(debEval)

## Estimate with k=25
debFit25 <- stm(debOut$documents, debOut$vocab, K = 25,
              prevalence = ~ as.factor(party) + as.factor(season), max.em.its = 75,
              data = debOut$meta, init.type = "Spectral")

## Examine
# Topic quality
topicQuality(debFit25, debOut$documents)
# Topic prevalence
plot(debFit25, type = "summary", labeltype="frex")
# Topic top words
labelTopics(debFit25)
# Topic prevalence by covariates
debEffect25 <- estimateEffect(1:25 ~ party + season, debFit25, meta = debOut$meta)
plot(debEffect25, covariate = "party", topics = 1, xlim=c(-0.2, 0.5),
     labeltype="custom", custom.labels=c("Dem", "Rep"))


##################################
# Dynamic visualization of results
# (LDAvis and stmBrowser)
##################################
# install.packages("LDAvis")
# install.packages("servr")
library(LDAvis)
library(servr)

# toLDAvis(mod=debFit25, docs=debOut$documents) # in window
toLDAvis(debFit25, debOut$documents, R=20, out.dir = "debatesLDAvis", open.browser = FALSE) # webpage
# See output: http://people.virginia.edu/~mpc8t/rhetoric2016/debatesLDAvis/

## stmBrowser: https://github.com/mroberts/stmBrowser
library(stmBrowser)

stmBrowser(debFit25, data=debOut$meta, 
           covariates=c("party", "season"), text="speech", 
           n=length(debOut$documents), directory=getwd())
# See output: http://people.virginia.edu/~mpc8t/rhetoric2016/debatesSTMvis/


## More Plots
debateTopics <- as.data.frame(debFit25$theta)
debateTopics <- cbind(debateTopics, debates16)

# Topic prevalance by party
partyTopics <- debateTopics %>% 
  select(c(1:25,29)) %>% 
  group_by(party) %>% 
  summarize_all(funs(sum))

# Topic prevalence by debate
topicsLong <- debateTopics %>% 
  select(V1:V25, date, party) %>% 
  gather(topic, value, -date, -party)

topicsLongGeneral <- topicsLong %>% filter(date>as.Date("2016-09-01"))
p <- ggplot(topicsLongGeneral, aes(x=topic, y=value, fill=party))
p + geom_bar(stat="identity") + facet_wrap(~date) + 
  scale_fill_manual(values=c("blue3", "orange3"))


#################### 
# Issue attention
# Using dictionaries
# (quanteda)
# Based on "First 100 days" agenda: https://assets.donaldjtrump.com/_landings/contract/O-TRU-102316-Contractv02.pdf
# First cut, super speculative, needs work

# corruption/special interests
kwic(debate16corpus, "lobby", 3, valuetype="regex") 
corruption <- c("corrupt*", "lobby*", "swamp", "interest", "interests", "revolving")

# federal regulations
kwic(debate16corpus, "bureaucra*", 3, valuetype="regex")
regulation <- c("regulat*", "bureaucra*")

# trade
trade <- c("nafta", "trade", "export*", "import*", "tpp", "pacific")

# energy
energy <- c("energy", "coal", "gas", "shale", "keystone")

# Obama exec orders, constitution
constitution <- c("executive", "supreme", "justice")

# immigration
immigration <- c("immig*", "mexic*", "border", "muslim*", "islam*")

# health care
healthcare <- c("health", "obamacare", "aca", "affordable", "medicare", "medicaid")
taxes <- c("tax*")

issues <- dictionary(list(corruption=corruption, regulation=regulation, trade=trade,
                          energy=energy, constitution=constitution, immigration=immigration,
                          healthcare=healthcare, taxes=taxes))

debateIssuesDFM <- dfm(debate16corpus, dictionary = issues) # apply dictionary
head(debateIssuesDFM,30)

# Turn this into a dataframe
debateIssue <- as.data.frame(debateIssuesDFM, row.names = debateIssuesDFM@Dimnames$docs)
debateIssue$id <- row.names(debateIssue) 

# Add to existing data frame
debates16 <- cbind(debates16, debateIssue)

# And plot!
p <- ggplot(debates16, aes(x=date, y=immigration))
p + geom_point(aes(color=party, shape=speaker), size=3) + 
  geom_hline(yintercept=median(debates16$immigration), color="gray50") +
  scale_x_date(labels = date_format("%m/%d"), breaks = date_breaks("5 weeks")) + 
  ggtitle("Issues in Candidate Debate Speech: Immigration") + 
  labs(y = "Frequency of Issue Mentions", x = "Date of Debate") +
  scale_color_manual(values=c("blue3", "orange3"), name="Speaker", guide=guide_legend(reverse = TRUE)) +
  scale_shape_manual(values=c(19,19,1,1)) + guides(shape=FALSE) +
  theme(plot.title = element_text(face="bold", size=20, hjust=0)) + 
  theme(axis.title = element_text(face="bold", size=16)) + 
  theme(panel.grid.minor = element_blank(), legend.position = c(0.77,0.9), legend.text=element_text(face="bold", size=12))

# Or plot all eight issues
names(debates16)[ncol(debates16)] <- "id2"
debates16long2 <- debates16 %>% 
  filter(speaker == "DJT") %>% 
  select(date, season, corruption, regulation, trade, energy, constitution, immigration, healthcare, taxes) %>% 
  gather(issue, value, -date, -season)

p <- ggplot(debates16long2, aes(x=date, y=value)) 
p + geom_point(size=2, color="orange3") +
  scale_x_date(labels = date_format("%m/%d"), breaks = date_breaks("5 weeks")) + 
  ggtitle("Issue Attentiocn in Trump's Debate Speech") + 
  labs(y = "Issue Attention", x = "Date of Debate") + facet_wrap(~ issue, ncol=2) + 
  theme(plot.title = element_text(face="bold", size=20, hjust=0)) + 
  theme(axis.title = element_text(face="bold", size=16)) + 
  theme(strip.text = element_text(size=16)) +
  theme(panel.grid.minor = element_blank())

save.image("debateTopics.Rdata")
