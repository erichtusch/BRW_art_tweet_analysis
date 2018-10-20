##### artist_tweet_api.R  #####
##  read in file of artist names and demographics

library(dplyr)
library(rjson)
library(tidytext)
#library(qdap)

##### functions #####
tweet.json2df <- function(tweet.json,tweets.only = T){
  ##  TODO  This is a likely place to improve:
  ##    ensuring a tweet is actually about an artist
  cat('\nprocessing json list into tweet df',names(tweet.json)[1])
  artist.df <- data.frame()
  if(tweets.only){
    cat('\nprocessing html and tweets only, output only tweets')
    # tweets and text only
    artist.df <- bind_rows(lapply(tweet.json,function(x) data.frame(html = as.character(x$html),
                                                                    text = as.character(x$text)))) %>%
      # engish only
      filter(grepl('lang="en"',html)) %>% 
      # text only
      select(text)
  }else{
    cat('\nbinding all data; mutating non-tweet data')
    # numbers as numbers
    artist.df <- bind_rows(lapply(tweet.json,function(x) as.data.frame(x))) %>%
      # engish only
      filter(grepl('lang="en"',html)) %>%
      # convert numbers to numbers
      mutate_at(vars(one_of("likes","replies",'retweets')),funs(as.integer(.))) %>%
      # convert timestamp
      mutate(ts = as.POSIXct(timestamp,format = "%Y-%m-%dT%H:%M:%S"))
  }
  return(artist.df)
}

##  get words from tweets ##
sentiments.from.tweets <- function(artist.df,
                                   sentiment.dfs = list(bing = get_sentiments(lexicon = "bing"),
                                                        nrc = get_sentiments(lexicon = "nrc"),
                                                        afinn = get_sentiments(lexicon = "afinn"))){
  ##  split out words from tweets, making all lowercase, and removing some punctiation
  ##  join wordlist to three sentiment lists
  cat('\ngetting wordlist from artist tweets',names(artist.df))
  words.from.tweets <- list()
  words.from.tweets$base <- data.frame(words = unlist(strsplit(paste(artist.df.list[[i]][["text"]],collapse = ' '),
                                                               split = " ")),
                                  stringsAsFactors = F) %>%
    mutate(words = qdap::strip(words,apostrophe.remove = F,lower.case = T))
  cat('\njoining sentiment df to artist tweets')
  for(j in 1:length(sentiment.dfs)){
    cat('\n\t',names(sentiment.dfs)[j])
    words.from.tweets[[j+1]] <- inner_join(words.from.tweets$base,sentiment.dfs[[j]],by=c("words"="word"))
    names(words.from.tweets)[j+1] <- names(sentiment.dfs)[j]
  }
  return(words.from.tweets)
}
##### script starts here  #####
##### load artist data file #####
artist.info.fp <-"/Users/turkeyboy/Documents/BRW_art_tweet_analysis/Artists.xlsx" 
if(!file.exists(artist.info.fp)){
  cat('\n\nfind artist info xlsx')
  artist.info.fp <- file.choose()
}
cat('\nloading artist info xlsx')
artist.info <- xlsx::read.xlsx(file = artist.info.fp,sheetIndex = 1,as.data.frame = T,stringsAsFactors = F) %>% 
  filter(!is.na(last.name))
##### load json #####
cat('\nfind json\n\n')
json.fp <- file.choose()
ans <- readline("read rest of jsons in this directory?\t>>  ")
if (trimws(toupper(ans))=="Y"){
  json.fp <- file.path(dirname(json.fp),dir(dirname(json.fp))[grep("json$",dir(dirname(json.fp)))])
}
##  combined step of reading and processing
artist.df.list = list()
tweet.words.split = list()
for (i in json.fp){
  artist.name <- tools::file_path_sans_ext(basename(i))
  cat('\nreading current artist tweets',artist.name)
  current.json <- fromJSON(file = i)
  cat('\nnaming current json list')
  names(current.json) = artist.name
  cat("\nprocessing current artist",artist.name)
  cat('\nn tweets:',length(current.json))
  #name list item
  artist.df.list[[tools::file_path_sans_ext(basename(i))]] <- tweet.json2df(current.json)
  # kill current json
  current.json <- NULL
}

##### connect to sentiments #####
##  create wordlist for tweets about each artists
##  join each artist's wordlist to each lexicon
artist.sentiment.list <- list()
for (i in 1:length(artist.df.list)){
  cat('\n splitting tweets into words into one long-ass data frame
and joining to sentiment lists')
  artist.sentiment.list[[i]] <- sentiments.from.tweets(artist.df.list[[i]])
  #name list item
  if(length(artist.df.list)==1){
    names(artist.sentiment.list) = names(artist.df.list)[i]
  } else {
    names(artist.sentiment.list)[i] <- names(artist.df.list)[i]
  }
}

##### add artist data #####
for(i in 1:length(artist.sentiment.list)){
  artist.sentiment.list[[i]]$info <- artist.info %>% filter(paste0(first.name,last.name) == 
                                                              names(artist.sentiment.list)[i])
}

##### get positive/negative average proportion per artist #####
sentiment.lists <- c("afinn","bing","nrc")
sentiment.list.measures <- c("score","sentiment","sentiment")
cat('creating counts for scores / sentiments')
for(i in 1:length(artist.sentiment.list)){
  cat('\nartist:',names(artist.sentiment.list)[i])
  for (j in sentiment.lists){
    # get colname to group (sentiment or score)
    counting.colname <- names(artist.sentiment.list[[i]][[j]])[2]
    # count scores and sentiments, then get percentages
    cat('\nadding',paste(j,"count",sep='_'),counting.colname)
    artist.sentiment.list[[i]][[paste(j,"count",sep='_')]] <- 
      artist.sentiment.list[[i]][[j]] %>% group_by_at(vars(one_of(counting.colname))) %>%
      count() %>%
      mutate(perc = n/sum(.$n))
  }
  # get mean afinn score per artist
  cat('\nmean afinn score')
  artist.sentiment.list[[i]][["mean_afinn_score"]] <- artist.sentiment.list[[i]][["afinn_count"]] %>%
    mutate(weighted.score = as.numeric(score)*as.numeric(perc)) %>% ungroup() %>% select(weighted.score) %>% colMeans()
}


##### build output  #####
##  build output list
output.list <- list()
# build artist sentiment summary measures
output.list[["artist.sentiment.summ.meas"]] <- data.frame()
# loop through each artist
for(i in 1:length(artist.sentiment.list)){
  cat('\nbuilding summary measure row for artist',names(artist.sentiment.list)[i])
  # first few cols are info
  current.artist.row <-artist.sentiment.list[[i]]$info
  # cols for counts and percents of each sentiment list
  for(j in sentiment.lists){
    current.itemname <- paste(j,"count",sep="_")
    current.counts <- artist.sentiment.list[[i]][[current.itemname]] %>% ungroup() %>% select(n) %>% t()
    current.percs <- artist.sentiment.list[[i]][[current.itemname]] %>% ungroup() %>% select(perc) %>% t()
    colnames(current.counts) <- paste(current.itemname,artist.sentiment.list[[i]][[current.itemname]][[names(artist.sentiment.list[[i]][[j]])[2]]],sep="_")
    colnames(current.percs) <-  paste(gsub("count","perc",current.itemname),
                                      artist.sentiment.list[[i]][[current.itemname]][[names(artist.sentiment.list[[i]][[j]])[2]]],sep="_")
    current.artist.row <- bind_cols(current.artist.row,as.data.frame(current.counts)) %>% 
      bind_cols(as.data.frame(current.percs)) %>% mutate(mean_afinn_score = artist.sentiment.list[[i]]$mean_afinn_score)
  }
  output.list[["artist.sentiment.summ.meas"]] <- output.list[["artist.sentiment.summ.meas"]] %>%
    bind_rows(current.artist.row)
}
##  sentiment-specific sheets for each artist
for(i in 1:length(artist.sentiment.list)){
  for(j in sentiment.lists){
    cat('\ncreating sentiment-specific',j,'sheet for artist',names(artist.sentiment.list)[i])
    output.list[[paste(names(artist.sentiment.list)[i],j,sep="_")]] <- artist.sentiment.list[[i]][[j]] %>%
      group_by_all() %>% count() %>% arrange(desc(n))
  }
}

##### write output  #####
cat('\nwriting output')
ofp <- file.path(dirname(artist.info.fp),paste0("artist sentiment summary measures ",Sys.Date(),'.xlsx'))
writexl::write_xlsx(x = output.list,path = ofp)
