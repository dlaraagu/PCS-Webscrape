library(rvest)

years <- list(seq(from = 2000, to = 2019))

df <- data.frame(Rnk = character(), Rider = character(), Time = character(), stringsAsFactors = FALSE)

for (y in years[[1]]){

url <- paste("https://www.procyclingstats.com/race/dauphine/",y,"/gc", sep = "")
html <- read_html(url)
tloc <- "body > div.wrapper > div.content > div.res-left > div:nth-child(4) > table"

list <- html %>% html_nodes(tloc) %>% html_table(fill = TRUE)
data <- list[[1]]

data$Year <- y
data$Rider <- substr(data$Rider, 1, nchar(data$Rider) - nchar(data$Team))
data$Time <- substr(data$Time, 1, ceiling(nchar(data$Time)/2))

data <- data[c("Rnk", "Rider", "Team", "Time", "Year")]
df <- rbind(df, data)
  
}