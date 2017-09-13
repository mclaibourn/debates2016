#####################
# 2016 Debate Text
# Michele Claibourn
# Acquire data
#####################

rm(list=ls())
library(dplyr)
library(rvest)
library(tm)
library(stringr)
library(quanteda)

setwd("~/Box Sync/mpc/datafordemocracy/debate2016analysis/")

if (!file.exists("debates16")) {
  dir.create("debates16")
}
setwd("debates16") # then set it as working directory


############################
# Scrape 2016 primary debate
# transcripts (rvest)
############################

# Load the source page
source.page <- read_html("http://www.presidency.ucsb.edu/debates.php", encoding = "windows-1251")
# windows-1251 found from page source code; UTF-8 is default
# Get URLs associated with desired text
url1 <- source.page %>% 
  html_nodes("a") %>%  
  html_attr("href") 
head(url1)
url1 <- url1[c(46:71)] # select just obs that reflect 2016 primary debates

# Get the text of the link (the date of the debate)
link1 <- source.page %>% 
  html_nodes("td.docdate") %>%  
  html_text() 
head(link1)
link1 <- link1[c(1:4,6:15,17:28)] # select just obs that reflect 2016 primary debates

# Combine `links` and `urls` into a data frame
debates <- data.frame(links=link1, urls=url1, stringsAsFactors=FALSE)

# Add party as a variable (for labeling the files)
debates$party <- c(rep("general",4), rep("democrat", 10), rep("republican", 12))
# Remove republican primary debate from which Trump opted out
debates <- subset(debates, links!="January 28th, 2016")

# Loop through each link in our debates data.frame (nrow(debate)) and 
# a. grab the html (read_html()), isolating node with text (".displaytext"),
# b. extract the text (html_text),
# c. append appropriate party label-year to downloaded file (paste0)
# d. and send output to file (sink/cat)
for(i in seq(nrow(debates))) {
  text <- read_html(debates$urls[i]) %>% # load the page
    html_nodes(".displaytext") %>% # isloate the text
    html_text() # get the text
  
  filename <- paste0(debates$party[i], "-", debates$links[i], ".txt")
  sink(file = filename) %>% # open file to write 
    cat(text)  # put the contents of "text" in the file
  sink() # close the file
}


####################################
# Create data frame with speech acts
# of each candidate in each debate
####################################
dem.files <- DirSource(pattern="(democrat|general)") # list of dem files (tm)

# initialize data frame
hrc.debate <- data.frame(speech=character(length(dem.files$filelist)),
                         date=character(length(dem.files$filelist)), 
                         speaker="HRC", stringsAsFactors=FALSE) 

for(i in 1:length(dem.files$filelist)){
  hrc.text <- readLines(dem.files$filelist[i])  
  hrc.split <- unlist(strsplit(hrc.text, "[A-Z]+:"))[-1]
  hrc.speak <- unlist(str_extract_all(hrc.text, "[A-Z]+:")) # (stringr)
  hrc <- hrc.split[hrc.speak %in% "CLINTON:"]
  hrc.debate$date[i] <- str_extract(dem.files$filelist[i], "\\-(.*?)\\.") # put date in data frame
  hrc.debate$speech[i] <- paste(hrc, collapse = "") # put text in data frame
}

# And for all republican debates
rep.files <- DirSource(pattern="(republican|general)") 

# initialize data frame
djt.debate <- data.frame(speech=character(length(rep.files$filelist)),
                         date=character(length(rep.files$filelist)), 
                         speaker="DJT", stringsAsFactors=FALSE) 

for(i in 1:length(rep.files$filelist)){
  djt.text <- readLines(rep.files$filelist[i])  
  djt.split <- unlist(strsplit(djt.text, "[A-Z]+:"))[-1]
  djt.speak <- unlist(str_extract_all(djt.text, "[A-Z]+:"))
  djt <- djt.split[djt.speak %in% "TRUMP:"]
  djt.debate$date[i] <- str_extract(rep.files$filelist[i], "\\-(.*?)\\.") 
  djt.debate$speech[i] <- paste(djt, collapse = "") 
}

## Or, per Clay's suggestion, a function
getLines <- function(x, person){
  text <- readLines(x)
  id <- unlist(str_extract_all(text, "[A-Z]+:"))
  Lines <- unlist(strsplit(text, "[A-Z]+:"))[-1]
  Lines[id %in% person]
}

# Use function to get VP debate speech acts
PenceLines <- lapply(rep.files$filelist, getLines, person = "PENCE:")
PenceLines <- PenceLines[[2]]
PenceLines <- paste(PenceLines, collapse = " ")
KaineLines <- lapply(rep.files$filelist, getLines, person = "KAINE:")
KaineLines <- KaineLines[[2]]
KaineLines <- paste(KaineLines, collapse = " ")

# Add these to debate dataframes
hrc.debate$speech[12] <- KaineLines
hrc.debate$speaker[12] <- "TMK"
djt.debate$speech[2] <- PenceLines
djt.debate$speaker[2] <- "MRP"

# Combine, add document variables
debates16 <- rbind(hrc.debate, djt.debate)
debates16$party <- ifelse(debates16$speaker %in% c("HRC", "TMK"), "Dem", "Rep")
debates16$date <- gsub("(-|th|rd|\\.)", "", debates16$date)
debates16$date <- as.Date(debates16$date, "%B %d, %Y")
debates16$season <- ifelse(debates16$date>as.Date("2016-09-01"), "General", "Primary")
debates16$season <- ifelse(debates16$speaker %in% c("TMK", "MRP"), "VP Debate", debates16$season)
debates16 <- debates16 %>% arrange(party, date)


#############################
# Create a corpus (quanteda;
# alternative: tm)
#############################
# in later analysis, quanteda is not recognizing speech as UTF8
library(stringi)
stri_enc_mark(debates16$speech)
debates16$speech <- iconv(debates16$speech, from = "windows-1251", to = "UTF-8", sub = "")

debate16corpus <- corpus(debates16$speech, 
                         docnames = paste(debates16$speaker, debates16$date))
docvars(debate16corpus, "speaker") <- debates16$speaker
docvars(debate16corpus, "date") <- debates16$date
docvars(debate16corpus, "party") <- debates16$party
docvars(debate16corpus, "season") <- debates16$season
summary(debate16corpus)

# stri_enc_mark(debate16corpus$documents$text) # some still ASCII?

rm(djt, djt.speak, djt.split, djt.text, filename, hrc, hrc.speak, hrc.split, hrc.text, i, KaineLines, PenceLines, text)
setwd("../")
save.image("debateSpeech2016.RData")
