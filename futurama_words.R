library('rvest')
library('stringr')
library('dplyr')
library('SnowballC')
library('tm')
library('DT')
library('htmlwidgets')


#####################################
### Scraping Wiki for transcripts ###
#####################################


tnodes <- c("5", "7", "9", "11")
transcriptList <- read_html("https://theinfosphere.org/Episode_Transcript_Listing")

epMaster <- data.frame(matrix(nrow=1, ncol=5))
colnames(epMaster) <- c("Title", "Original.airdateFOX", "Production.code", "TV.Broadcast", "X.")

for (i in tnodes){
  episode <- transcriptList %>% html_nodes(paste0("table.overview:nth-child(", i, ")")) %>% html_table()%>% as.data.frame()
  epMaster <- rbind(epMaster, episode)
}

#Constructing URLs for each transcript
epMaster <-subset(epMaster, Title!="")
epMaster$Title <- str_replace_all(epMaster$Title, " ", "_")
epMaster$Title <- str_replace_all(epMaster$Title, "\\?", "%3F")
epMaster$Title <- str_replace_all(epMaster$Title, "30%", "30%25")


#Download the transcripts and combine into one object
fullTranscripts <- list()
for (i in 1:nrow(epMaster)){
  tempTranscript <- read_html(paste0("https://theinfosphere.org/", epMaster[i,1]), encoding='UTF-8') %>% html_nodes("#mw-content-text") %>% html_text()
  fullTranscripts <- rbind(fullTranscripts, tempTranscript)
}


#####################################
######### Tidying things up #########
#####################################


#Remove stage directions
fullTranscripts <- str_replace_all(fullTranscripts, "\\[.+\\]", "")
fullTranscripts <- str_replace_all(fullTranscripts, "\\([:digit:]{1,2}\\:[:digit:]{1,2}\\).{4}", "/n")


#These episodes have timestamps in each line.  Remove those and replace with newline to ensure consistency between all episodes
fullTranscripts[1] <- str_replace_all(fullTranscripts[1], "\\([:digit:]{1,2}\\:[:digit:]{1,2}\\).{4}", "/n")
fullTranscripts[2] <- str_replace_all(fullTranscripts[2], "\\([:digit:]{1,2}\\:[:digit:]{1,2}\\).{4}", "/n")
fullTranscripts[3] <- str_replace_all(fullTranscripts[3], "\\([:digit:]{1,2}\\:[:digit:]{1,2}\\).{4}", "/n")
fullTranscripts[4] <- str_replace_all(fullTranscripts[4], "\\([:digit:]{1,2}\\:[:digit:]{1,2}\\).{4}", "/n")
fullTranscripts[5] <- str_replace_all(fullTranscripts[5], "\\([:digit:]{1,2}\\:[:digit:]{1,2}\\).{4}", "/n")
fullTranscripts[6] <- str_replace_all(fullTranscripts[6], "\\([:digit:]{1,2}\\:[:digit:]{1,2}\\).{4}", "/n")
fullTranscripts[7] <- str_replace_all(fullTranscripts[7], "\\([:digit:]{1,2}\\:[:digit:]{1,2}\\).{4}", "/n")
fullTranscripts[54] <- str_replace_all(fullTranscripts[54], "\\([:digit:]{1,2}\\:[:digit:]{1,2}\\).{4}", "/n")


#Characters in Universe A on Farnsworth Parabox (fullTranscripts[71]) should have names standardized.
fullTranscripts[71] <- str_replace_all(fullTranscripts[71], " A(?=\\:)", "")


#########################
###### Text mining ######
#########################


#create list of characters to analyze
mychars <- c("Fry", "Bender", "Leela", "Farnsworth", "Amy", "Hermes", "Zapp", "Kif", "Calculon", "Cubert", "Lrrr", "Nixon", "Mom", "Robot Devil", "Elzar", "Coilette", "Morbo", "Linda", "Clamps", "Donbot", "Joey", "Flexo", "Scruffy", "Zoidberg")


#Create dataframe for each character in mychars
for (i in 1:length(mychars)){
  nam <- paste(mychars[i], "_lines", sep="")
  assign(nam, str_extract_all(fullTranscripts, paste0("(?<=", mychars[i], "\\: ).*")))
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
  tmp <- get(paste0(mychars[i], '_done'))
  tmp <- tmp[tmp$Word!='character0', ]
  CharBottom10 <- head(tmp[order(tmp$Rank, decreasing = F), ], n = 10)
  wordsByCharacter <- rbind(wordsByCharacter, CharBottom10)
  CharTop10 <- head(tmp[order(tmp$Rank, decreasing = T), ], n = 10)
  wordsByCharacter <- rbind(wordsByCharacter, CharTop10)
}

write.csv(wordsByCharacter, 'words.csv')
