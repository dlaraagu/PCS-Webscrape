#Load packages.
library(data.table)
library(rvest)

#Specify the years.
years <- list(seq(from = 2000, to = 2020))

#Specify the race.
r <- "giro-d-italia"

#Create an empty dataset to append to. 
df <- data.frame(Rnk = character(), Rider = character(), Time = character(), stringsAsFactors = FALSE)

#Loop through each year's race results page.
for (y in years[[1]]) {

  #Set the url and parse the html file. 
  url <- paste("https://www.procyclingstats.com/race/",r,"/",y,"/gc", sep = "")
  html <- read_html(url)
  
  #Specify the location of the results table.
  tloc <- "body > div.wrapper > div.content > div.res-left > div.resultCont > table"
  df_sub <- html %>% html_nodes(tloc) %>% .[[ind]] %>% html_table(fill = TRUE)
  
  #Clean the data before appending.
  df_sub$Rider <- ifelse(is.na(df_sub$Team), df_sub$Rider, mapply(gsub, pattern = df_sub$Team, replacement = "", x = df_sub$Rider))
  df_sub$Time  <- substr(df_sub$Time, 1, ceiling(nchar(df_sub$Time) / 2))
  df_sub$Rank  <- as.numeric(rownames(df_sub))
  df_sub$Year  <- y
  
  #Remove riders who DNFed.
  df_sub <- subset(df_sub, is.na(df_sub$Rnk) | (df_sub$Rnk != "DNF" & df_sub$Rnk != "OTL" & df_sub$Rnk != "DNS" & df_sub$Rnk != "DSQ"))
  
  #Format the time field.
  while (any(grepl(",,", df_sub$Time))) { 
    df_sub$Time <- with(df_sub, ifelse(grepl(",,", df_sub$Time), shift(df_sub$Time, type = 'lag'), df_sub$Time))
  }
  
  df_sub$Time <- with(df_sub, ifelse(nchar(df_sub$Time) == 4, paste("00:0", df_sub$Time, sep = ""), df_sub$Time))
  df_sub$Time <- with(df_sub, ifelse(nchar(df_sub$Time) == 5, paste("00:", df_sub$Time, sep = ""), df_sub$Time))
  df_sub$Time <- with(df_sub, ifelse(nchar(df_sub$Time) == 7, paste("0", df_sub$Time, sep = ""), df_sub$Time))
  
  df_sub$hrs  <- as.numeric(sapply(strsplit(df_sub$Time, ":"), "[", 1))
  df_sub$min  <- as.numeric(sapply(strsplit(df_sub$Time, ":"), "[", 2))
  df_sub$sec  <- as.numeric(sapply(strsplit(df_sub$Time, ":"), "[", 3))
  df_sub$Time <- df_sub$hrs + (df_sub$min / 60) + (df_sub$sec / 3600)
  df_sub$Time <- ifelse(df_sub$Time < df_sub$Time[1], df_sub$Time + df_sub$Time[1], df_sub$Time)
  
  #Select the relevant data fields. 
  df_sub <- df_sub[c("Year", "Rank", "Rider", "Age", "Team", "Time")]
  
  #Append to the empty dataset.
  df <- rbind(df, df_sub)
}
