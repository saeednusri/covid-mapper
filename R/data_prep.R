library(dplyr)
library(chron)
require(xlsx)

europa <- "https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide.xlsx"

download.file(europa, "world.xlsx", method = "libcurl")
world <- read.xlsx("world.xlsx", sheetName = "COVID-19-geographic-disbtributi")
canada <- read.csv("https://health-infobase.canada.ca/src/data/covidLive/covid19.csv")

#world <- read.csv("data/covid_data/world.csv")
#canada <- read.csv("data/covid_data/canada_covid19.csv")

alberta <- read.csv("data/covid_data/alberta_covid.csv")

w <- world[, colnames(world) %in% c("dateRep", "cases", "deaths", "countriesAndTerritories")]

colnames(w) <- c("Date", "Cases", "Deaths", "NAME")
w$Date <- format(as.POSIXct(w$Date, format = "%d/%m/%Y"), format = "%Y-%m-%d")
w$Date <- as.Date(w$Date)
w$NAME <- gsub("_", " ", w$NAME)
Dates <- as.data.frame(seq(as.Date(min(w$Date)), as.Date(max(w$Date)), by = 1))
names(Dates) <- "Date"
w1 <- inner_join(Dates, w, by = "Date")

code <- unique(w1$NAME)
df_final <- data.frame()
for (i in 1:length(code)){
  df_sel <- w1[w1$NAME == code[i], ]
  df_sel$TotalCases <- cumsum(df_sel$Cases)
  df_sel$TotalDeaths <- cumsum(df_sel$Deaths)
  df_final <- rbind(df_final, df_sel)
}

w1 <- df_final

c <- canada[, colnames(canada) %in% c("date", "numconf", "numdeaths", "prname")]
colnames(c) <- c("NAME", "Date", "Cases", "Deaths")
c$Date <- format(as.POSIXct(c$Date, format = "%d-%m-%Y"), format = "%Y-%m-%d")
c$Date <- as.Date(c$Date)
c1 <- inner_join(Dates, c, by = "Date")


alberta$Date <- format(as.POSIXct(alberta$Date, format = "%Y-%m-%d"), format = "%Y-%m-%d")
alberta$Date <- as.Date(alberta$Date)

albert <- alberta[alberta$Case.type == "Confirmed", ]
alberta_dead <- albert[albert$Case.status == "Died", ]

pseudo <- data.frame()

for(i in 1:length(unique(Dates$Date))){
  rm(l)
  date <- as.Date(unlist(Dates$Date[i]))
  k <- as.data.frame(unlist(unique(alberta$Alberta.Health.Services.Zone)))
  names(k) <- "NAME"
  l <- cbind(date, k)
  colnames(l) <- c("Date", "NAME")
  pseudo <- rbind(pseudo, l)
}


albert_group <- albert %>% group_by(Date.reported, Alberta.Health.Services.Zone) %>%  count(Case.type)
colnames(albert_group) <- c("Date", "NAME", "Case.type", "Cases")
albert_group$Date <- as.Date(albert_group$Date)

ac<- left_join(pseudo, albert_group, by = c("Date", "NAME"))

albert_group_d <- alberta_dead %>% group_by(Date.reported, Alberta.Health.Services.Zone) %>%  count(Case.type)
colnames(albert_group_d) <- c("Date", "NAME", "Case.type", "Deaths")
albert_group_d$Date <- as.Date(albert_group_d$Date)

ad <- left_join(pseudo, albert_group_d, by = c("Date", "NAME"))
 
comb <- left_join(ac, ad, by = c("Date","NAME"))
a <- comb[,colnames(comb) %in% c("NAME", "Date", "Cases", "Deaths")]
a[is.na(a)] <- 0

code <- unique(a$NAME)
df_final <- data.frame()
for (i in 1:length(code)){
  df_sel <- a[a$NAME == code[i], ]
  df_sel$TotalCases <- cumsum(df_sel$Cases)
  df_sel$TotalDeaths <- cumsum(df_sel$Deaths)
  df_final <- rbind(df_final, df_sel)
}

a <- data_final

final <- rbind(w1, c1)
final <- rbind(final, a)

saveRDS(final, "data-files/final.RDS")
