#Load packages.
library(data.table)
library(rvest)

#Specify the years.
years <- list(seq(from = 2000, to = 2020))

#Specify the race.
r <- "dauphine"

#Create an empty dataset to append to. 
df <- data.frame(Rnk = character(), Rider = character(), Time = character(), stringsAsFactors = FALSE)

#Loop through each year's race results page.
for (y in years[[1]]) {

  #Set the url and parse the html file. 
  url <- paste("https://www.procyclingstats.com/race/",r,"/",y,"/gc", sep = "")
  html <- read_html(url)
  
  #Specify the location of the results table.
  tloc <- "body > div.wrapper > div.content > div.res-left > div.resultCont > table"
  df_sub <- html %>% html_nodes(tloc) %>% .[[2]] %>% html_table(fill = TRUE)
  
  #Clean the data before appending.
  df_sub$Rnk   <- with(df_sub, ifelse(is.na(df_sub$Rnk), shift(df_sub$Rnk, type = 'lag') + 1, df_sub$Rnk))
  df_sub$Rider <- substr(df_sub$Rider, 1, nchar(df_sub$Rider) - nchar(df_sub$Team))
  df_sub$Time  <- substr(df_sub$Time, 1, ceiling(nchar(df_sub$Time)/2))
  df_sub$Year <- y
  
  #Format the time field.
  df_sub$Time <- with(df_sub, ifelse(grepl(",,", df_sub$Time), shift(df_sub$Time, type = 'lag'), df_sub$Time))
  df_sub$Time <- with(df_sub, ifelse(nchar(df_sub$Time) == 4, paste("00:0", df_sub$Time, sep = ""), df_sub$Time))
  df_sub$Time <- with(df_sub, ifelse(nchar(df_sub$Time) == 5, paste("00:", df_sub$Time, sep = ""), df_sub$Time))
  df_sub$Time <- with(df_sub, ifelse(nchar(df_sub$Time) == 7, paste("0", df_sub$Time, sep = ""), df_sub$Time))
  df_sub$Time <- as.numeric(substr(df_sub$Time, 1, 2)) + as.numeric(substr(df_sub$Time, 4, 5)) / 60 + 
                 as.numeric(substr(df_sub$Time, 7, 8)) / 3600
  df_sub$Time[df_sub$Rnk != 1] <- df_sub$Time[df_sub$Rnk != 1] + df_sub$Time[1]
  
  #Select the relevant data fields. 
  df_sub <- df_sub[c("Year", "Rnk", "Rider", "Age", "Team", "Time")]
  
  #Append to the empty dataset.
  df <- rbind(df, df_sub)
  
}
