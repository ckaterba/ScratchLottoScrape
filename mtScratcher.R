library(rvest)
library(stringr)
library(XML)
library(tidyverse)

fileURL <- "https://www.montanalottery.com/en/view/scratch"

###
#Plan 1: use regex to extract game names and odds for each scratch lotto game from raw html code
###

scratch <- read_html(fileURL) %>%
  html_nodes("script") %>%
  html_text(trim=TRUE)

#script from html code with all game info that appears in tables
longString <- scratch[5]
#extracts all info associated to the scratch games
#there are currently some repeated games for some reason
games <- str_extract_all(longString, '\\{\\"gameName\\":\\"(.*?)\\"gameType\\":\\"SCRATCH\\"(.*?)\"jsClass\":\"Desktop.ScratchGame\"\\}')[[1]]

#helper functions 

#enter odds as a string eg. "1:4" -> 1/5 = .2
#while loop to fix a typo in scraped data
oddsToProb <- function(odds){
  temp <-str_split(odds, ":")[[1]]
  num <- as.numeric(temp[1])
  denom <- temp[2]
  while( str_count(denom, "\\.")>1 ){
    denom <- str_remove(denom, "\\.")
  }
  denom <- as.numeric(denom)
  return( num/(num + denom))
}
#the following functionsmust input an elt or vect of games
getName <- function(game){
  return( str_extract(game, '(?<=\\"gameName\\":\\")(.*?)(?=\\")' ) )
}
getPrice <- function(game){
  return( as.numeric(str_extract(game, '(?<=\\"gamePrice\\":)(.*?)(?=,)')) )
}
#winnings as string to numeric value, main problems are games #62 and 35
#only use in getGameTable
setWinnings <- function(winnings, game){
  if(winnings=="Ticket"){return(getPrice(game))}
  if(str_detect(winnings, "\\$")){return(as.numeric(str_remove(winnings, "\\$")))}
  if(winnings=="FORD F-150 XLT TRUCK"){return(35350)}
  return(as.numeric(winnings))
}
#workhorse that uses regex to pull winnings, odds from each element of games
getGameTable <- function(game){
  temp <- str_extract(game, '(?<=\\"gameTable\\":\\[)(.*?)(?=\\])') %>% 
    str_extract_all('\\{\\"win(.*?)odds\\"(.*?)"\\}')
  temp <- temp[[1]] %>%
    str_remove_all("\\,") %>%
    str_remove('\\{\\"win\\"(.*?)prize\\"\\:\\"') %>%
    str_remove('\\"\\}')  %>%
    str_split('"\\"odds\\":\\"')
  df <- data.frame(Payout = character(), 
                   Odds = character(),
                   PayoutNumerical = numeric(),
                   Winnings = numeric(),
                   Probability = numeric())
  for(i in 1:length(temp)){
    df <- add_row(df,
                  Payout = temp[[i]][1],
                  Odds = temp[[i]][2], 
                  PayoutNumerical = setWinnings(temp[[i]][1],game),
                  Winnings = setWinnings(temp[[i]][1],game) - getPrice(game),
                  Probability = oddsToProb(temp[[i]][2])
    )
  }
  win <- sum(df$Probability)
  lose <-  1 - win
  df <- add_row(df, Payout = "$0", 
                Odds = str_c( as.character(round(10*lose, 2)), as.character(round(10*win,2)), sep=":"),
                PayoutNumerical = 0,
                Winnings = -getPrice(game),
                Probability = lose)
  return(df)
}

#function builds tibble for distribution of r.v. 
probDist <- function(game){
  df <- as_tibble(getGameTable(game)) %>%
    group_by(Winnings) %>%
    summarise(Probability = sum(Probability)) %>%
    return(df)
}

#list1 values of R.V., list2 probabilities of observing those values
expVal <- function(list1, list2){
  if(length(list1)!=length(list2)){return("Lists different lenghts!")}
  return( sum(list1*list2) )
}
#list1 values of R.V., list2 probabilities of observing those values
variance <- function(list1, list2){
  if(length(list1)!=length(list2)){return("Lists different lenghts!")}
  return( sum( (list1 - expVal(list1,list2))^2*list2))
}

allGames <- function(){
  df <- tibble(Name = character(),
               Price = numeric(),
               MaxPayout = numeric(),
               ProbWin = numeric(), 
               ExpectedValue = numeric(),
               StdDev = numeric())
  for(game in games){
    tab <- probDist(game)
    df <- add_row(df, 
                  Name = getName(game),
                  Price = getPrice(game),
                  MaxPayout = max(tab$Winnings),
                  ProbWin = 1 - tab$Probability[1],
                  ExpectedValue = expVal(tab$Winnings, tab$Probability),
                  StdDev = sqrt(variance(tab$Winnings, tab$Probability)) )
  }
  df <-add_column(df,"TicketsSixty" = 60 / df$Price) 
  df <-add_column(df,"ExpValofSixty" = df$TicketsSixty*df$ExpectedValue) 
  df <-add_column(df,"StdDevSixty" = sqrt(df$TicketsSixty*(df$StdDev)^2))
  return(df)
}



###
#Plan 2: be smarter and try to iterate over something in html or interact with js?
###