library(rvest)
library(stringr)
library(dplyr)
library(SnowballC)
library(tm)
library(DT)
library(htmlwidgets)


#####################################
### Scraping Wiki for transcripts ###
#####################################


tnodes <- c("5", "7", "9", "11")
main <- read_html("https://theinfosphere.org/Episode_Transcript_Listing")

epmaster <- data.frame(matrix(nrow=1, ncol=5))
colnames(epmaster) <- c("Title", "Original.airdateFOX", "Production.code", "TV.Broadcast", "X.")

for (i in tnodes){
  eplist <- main %>% html_nodes(paste0("table.overview:nth-child(", i, ")")) %>% html_table()
  eplist <- as.data.frame(eplist)
  epmaster <- rbind(epmaster, eplist)
}

#Constructing URLs for each transcript
epmaster <-subset(epmaster, Title!="")
epmaster$Title <- str_replace_all(epmaster$Title, " ", "_")
epmaster$Title <- str_replace_all(epmaster$Title, "\\?", "%3F")
epmaster$Title <- str_replace_all(epmaster$Title, "30%", "30%25")


#Download the transcripts and combine into one object
bigtranscript <- list()
for (i in 1:nrow(epmaster)){
  temptranscript <- read_html(paste0("https://theinfosphere.org/", epmaster[i,1]), encoding='UTF-8') %>% html_nodes("#mw-content-text") %>% html_text()
  bigtranscript <- rbind(bigtranscript, temptranscript)
}


#####################################
######### Tidying things up #########
#####################################


#Remove stage directions
bigtranscript <- str_replace_all(bigtranscript, "\\[.+\\]", "")


#These episodes have timestamps in each line.  Remove those and replace with newline to ensure consistency between all episodes
bigtranscript[1] <- str_replace_all(bigtranscript[1], "\\([:digit:]{1,2}\\:[:digit:]{1,2}\\).{4}", "/n")
bigtranscript[2] <- str_replace_all(bigtranscript[2], "\\([:digit:]{1,2}\\:[:digit:]{1,2}\\).{4}", "/n")
bigtranscript[3] <- str_replace_all(bigtranscript[3], "\\([:digit:]{1,2}\\:[:digit:]{1,2}\\).{4}", "/n")
bigtranscript[4] <- str_replace_all(bigtranscript[4], "\\([:digit:]{1,2}\\:[:digit:]{1,2}\\).{4}", "/n")
bigtranscript[5] <- str_replace_all(bigtranscript[5], "\\([:digit:]{1,2}\\:[:digit:]{1,2}\\).{4}", "/n")
bigtranscript[6] <- str_replace_all(bigtranscript[6], "\\([:digit:]{1,2}\\:[:digit:]{1,2}\\).{4}", "/n")
bigtranscript[7] <- str_replace_all(bigtranscript[7], "\\([:digit:]{1,2}\\:[:digit:]{1,2}\\).{4}", "/n")
bigtranscript[54] <- str_replace_all(bigtranscript[54], "\\([:digit:]{1,2}\\:[:digit:]{1,2}\\).{4}", "/n")


#Characters in Universe A on Farnsworth Parabox (bigtranscript[71]) should have names standardized.
bigtranscript[71] <- str_replace_all(bigtranscript[71], " A(?=\\:)", "")  


#########################
###### Text mining ######
#########################


#create list of characters to analyze
mychars <- c("Fry", "Bender", "Leela", "Farnsworth", "Amy", "Hermes", "Zapp", "Kif", "Calculon", "Cubert", "Lrrr", "Nixon", "Mom", "Robot Devil", "Elzar", "Coilette", "Morbo", "Linda", "Clamps", "Donbot", "Joey", "Flexo", "Scruffy", "Zoidberg")


#Create dataframe for each character in mychars
for (i in 1:length(mychars)){
  nam <- paste(mychars[i], "_lines", sep="")
  assign(nam, str_extract_all(bigtranscript, paste0("(?<=", mychars[i], "\\: ).*")))
}


#Word count for each character
for (i in 1:length(mychars)){
  b <- paste0(mychars[i], '_done')
  assign(b, get(paste0(mychars[i], "_lines")) 
         %>% VectorSource() 
         %>% Corpus() 
         %>% tm_map(FUN=tolower)
         %>% tm_map(FUN = removeWords, stopwords('english'))
         %>% tm_map(FUN=stripWhitespace)
         %>% tm_map(FUN=removePunctuation)
         #%>% tm_map(FUN=stemDocument) may or may not include this
         %>% DocumentTermMatrix()
  )
  Frequency <- colSums(as.matrix(get(paste0(mychars[i], '_done'))))
  assign(b, data.frame(Character=mychars[i], Rank=rank(Frequency), Word=names(Frequency), Frequency=Frequency))
}

wordsByCharacter <- data.frame(matrix(nrow=1, ncol=4))
colnames(wordsByCharacter) <- c('Character', 'Rank', 'Word', 'Frequency')

for (i in 1:length(mychars)){
  CharTop10 <- head(get(paste0(mychars[i], '_done'))[order(get(paste0(mychars[i], '_done'))$Rank, decreasing = F), ], n = 10)
  wordsByCharacter <- rbind(wordsByCharacter, CharTop10)
}  


mydt <- datatable(wordsByCharacter, filter='top', options=list(pageLength=11, autoWidth=T), rownames=F)

saveWidget(mydt, 'Futurama_words_by_character.html')