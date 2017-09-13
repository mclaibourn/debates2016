#####################
# 2016 Debate Text
# Michele Claibourn
# Readability/Tone
#####################

rm(list=ls())
library(tidyverse)
library(quanteda)
library(scales)
library(stringr)
library(tidytext) 

setwd("~/Box Sync/mpc/datafordemocracy/debate2016analysis/")
load("debateSpeech2016.RData")


#################################
# Readability/Complexity Analysis
# (quanteda; alternative: korpus)
#################################
fk <- textstat_readability(debate16corpus, measure = "Flesch.Kincaid")
debates16$readability <- fk

# And plot!
p <- ggplot(debates16, aes(x = date, y = readability))
# Add color for party, better labels/formatting for axes
p + geom_point(aes(color=party, shape=speaker), size=3) +
  scale_x_date(labels = date_format("%m/%d"), breaks = date_breaks("5 weeks")) + # (scales)
  ggtitle("'Readability' of Candidate Debate Speech") + 
  labs(y = "Readability (grade level)", x = "Date of Debate") +
  scale_color_manual(values=c("blue3", "orange3"), name="Speaker", guide=guide_legend(reverse = TRUE)) +
  scale_shape_manual(values=c(19,19,1,1)) + guides(shape=FALSE) +
  theme(plot.title = element_text(face="bold", size=20, hjust=0)) + 
  theme(axis.title = element_text(face="bold", size=16)) + 
  theme(panel.grid.minor = element_blank(), legend.position = c(0.77,0.9), legend.text=element_text(face="bold", size=12))
ggsave(file="figures/readability.png")


###############################
# Sentiment Analysis (quanteda;
# alternative: tidytext)
###############################
# Pull dictionaries from tidytext 
# (all are freely available online, and could be loaded as word vectors)
bing <- sentiments %>% filter(lexicon=="bing") 
table(bing$sentiment)
sample(bing$word[bing$sentiment=="negative"], 10) # examples
sample(bing$word[bing$sentiment=="positive"], 10) # examples


##############
# Overall Tone
sentDict <- dictionary(list(positive = bing$word[bing$sentiment=="positive"], negative = bing$word[bing$sentiment=="negative"]))
debateToneDFM <- dfm(debate16corpus, dictionary = sentDict) # apply dictionary
head(debateToneDFM,10)

# Turn this into a dataframe, create tone=positive-negative
debateTone <- as.data.frame(debateToneDFM, row.names = debateToneDFM@Dimnames$docs)
debateTone$id <- row.names(debateTone) # put row names into column

debateTone <- debateTone %>% 
  mutate(tone = positive - negative)
summary(debateTone$tone)
hist(debateTone$tone)

# Add to existing data frame
debates16$tone <- debateTone$tone

# Plot!
p <- ggplot(debates16, aes(x=date, y=tone))
p + geom_point(aes(color=party, shape=speaker), size=3) + 
  geom_hline(yintercept=median(debates16$tone), color="gray50") +
  scale_x_date(labels = date_format("%m/%d"), breaks = date_breaks("5 weeks")) + 
  ggtitle("'Tone' of Candidate Debate Speech") + 
  labs(y = "Overall Tone (Negative to Positive)", x = "Date of Debate") +
  scale_color_manual(values=c("blue3", "orange3"), name="Speaker", guide=guide_legend(reverse = TRUE)) +
  scale_shape_manual(values=c(19,19,1,1)) + guides(shape=FALSE) +
  theme(plot.title = element_text(face="bold", size=20, hjust=0)) + 
  theme(axis.title = element_text(face="bold", size=16)) + 
  theme(panel.grid.minor = element_blank(), legend.position = c(0.77,0.9), legend.text=element_text(face="bold", size=12))
ggsave(file="figures/tone.png")


####################
# Tone by sentence
# Change the documentary units into sentences
debate16corpusSent <- corpus_reshape(debate16corpus, to="sentences")
debate16corpusSent
# Apply dictionary
debateToneSentDFM <- dfm(debate16corpusSent, dictionary = sentDict)
head(debateToneSentDFM)

# Turn this into a dataframe, create tone=positive-negative
debateToneSent <- as.data.frame(debateToneSentDFM)
debateToneSent$id <- row.names(debateToneSent) # keep row names as id
debateToneSent$seq <- as.integer(str_sub(debateToneSent$id, 16)) # extract sentence order
debateToneSent$event <- str_sub(debateToneSent$id, 1,14) # (stringr)
debateToneSent$speaker <- str_sub(debateToneSent$id, 1,3)
debateToneSent$date <- str_sub(debateToneSent$id, 5,14)
debateToneSent$party <- ifelse(debateToneSent$speaker %in% c("HRC", "TMK"), "Dem", "Rep")
debateToneSent$date <- as.Date(debateToneSent$date, format="%Y-%m-%d")

debateToneSent <- debateToneSent %>% 
  mutate(tone = positive - negative)
summary(debateToneSent$tone)
hist(debateToneSent$tone)

# Graph distribution of tone during general election
p <- ggplot(debateToneSent[debateToneSent$date>as.Date("2016-09-01"),], aes(x = reorder(event, date), y=tone)) 
p + geom_violin(aes(color=party)) + scale_color_manual(values=c("blue3", "orange3"))

# Graph 10/09/2016 debate tone by sentence
debateToneSent2 <- debateToneSent %>% # get desired debate
  filter(event=="HRC 2016-10-09" | event=="DJT 2016-10-09")
p <- ggplot(debateToneSent2, aes(x=seq, y=tone))
p + geom_point(size=0.5) + geom_smooth(method="loess", span=.33) + 
  facet_wrap(~event, scales="free_x")

# Peak positive, negative
summary(debateToneSent2$tone)
maxPos <- subset(debateToneSent2, tone==max(debateToneSent2$tone), select=id) # most positive sentence
maxNeg <- subset(debateToneSent2, tone==min(debateToneSent2$tone), select=id) # most negative sentence
debate16corpusSent$documents$text[docnames(debate16corpusSent)==maxPos$id]
debate16corpusSent$documents$text[docnames(debate16corpusSent)==maxNeg$id]


###########################
# Other Emotional Affect
nrc <- sentiments %>% filter(lexicon=="nrc") # (loaded with tidytext)
table(nrc$sentiment)
sample(nrc$word[nrc$sentiment=="fear"], 10) # examples

affectDict <- dictionary(list(angerW=nrc$word[nrc$sentiment=="anger"], 
                              fearW=nrc$word[nrc$sentiment=="fear"],
                              anticipationW=nrc$word[nrc$sentiment=="anticipation"],
                              trustW=nrc$word[nrc$sentiment=="trust"]))
debateAffectDFM <- dfm(debate16corpus, dictionary = affectDict) # apply dictionary
head(debateAffectDFM,10)

# Turn this into a dataframe, add to existing dataframe
debateAffect <- as.data.frame(debateAffectDFM, row.names = debateAffectDFM@Dimnames$docs)
debateAffect$id <- row.names(debateAffect) # put row names into column
debateAffect <- debateAffect %>% 
  mutate(tot=angerW+fearW+anticipationW+trustW,
         anger=(angerW/tot)*100,
         fear=(fearW/tot)*100,
         anticipation=(anticipationW/tot)*100,
         trust=(trustW/tot)*100)

debates16[,8:11] <- debateAffect[,7:10]

# Plot!
p <- ggplot(debates16, aes(x=date, y=anger))
p + geom_point(aes(color=party, shape=speaker), size=3) + 
  geom_hline(yintercept=mean(debates16$anger), color="gray50") +
  scale_x_date(labels = date_format("%m/%d"), breaks = date_breaks("5 weeks")) + 
  ggtitle("Anger Affect within Clinton, Trump Debate Speech") + 
  labs(y = "Anger Affect", x = "Date of Debate") +
  scale_color_manual(values=c("blue3", "orange3"), name="Speaker", guide=guide_legend(reverse = TRUE)) +
  scale_shape_manual(values=c(19,19,1,1)) + guides(shape=FALSE) +
  theme(plot.title = element_text(face="bold", size=20, hjust=0)) + 
  theme(axis.title = element_text(face="bold", size=16)) + 
  theme(panel.grid.minor = element_blank(), legend.position = c(0.77,0.9), legend.text=element_text(face="bold", size=12))

# Or plot all four affect variables
debates16long <- debates16 %>% 
  select(date, speaker, party, anger, fear, anticipation, trust) %>% 
  gather(sentiment, value, -date, -speaker, -party)

p <- ggplot(debates16long, aes(x=date, y=value)) 
p + geom_point(aes(color=party, shape=speaker), size=2) + 
  scale_x_date(labels = date_format("%m/%d"), breaks = date_breaks("5 weeks")) + 
  ggtitle("'Affect' of Candidate Debate Speech") + 
  labs(y = "Level of Affect", x = "Date of Debate") +
  facet_wrap(~ sentiment, ncol=2, scales="free_y") + 
  scale_color_manual(values=c("blue3", "orange3")) +
  scale_shape_manual(values=c(19,19,1,1)) + guides(shape="none", color="none") +
  theme(plot.title = element_text(face="bold", size=20, hjust=0)) + 
  theme(axis.title = element_text(face="bold", size=16)) + 
  theme(strip.text = element_text(size=16)) +
  theme(panel.grid.minor = element_blank(), legend.position = c(0.85,0.9), legend.text=element_text(face="bold", size=12))
ggsave(file="figures/affect.png")

save.image("debateSpeech2016Sentiment.RData")
