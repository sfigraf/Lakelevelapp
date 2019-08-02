library(tidyverse)
library(stringr)
library(data.table)
library(lubridate)
library(rvest)
library(plotly)
library(leaflet)
library(readxl)
library(geojsonio)
library(rgdal)
library(ggrepel)
library(ggthemes)
library(googlesheets)


#gs_auth(new_user = TRUE) #was for authenitcating google account


###1999- stage
#a function that reads the google sheet 
read_google_sheet <- function(spreadsheetname) {
  x <- gs_title(as.character(spreadsheetname))
  x <- gs_read(x)
  return(x)
}

echo <- read_google_sheet("Lake Stage_Echo_1999-2016")
eagle <- read_google_sheet("Lake Stage_Eagle_2000-2016")
bubble <- read_google_sheet("Lake Stage_Bubble_1999-2016")
jordan <- read_google_sheet("Lake Stage_Jordan_1999-2018")
sealcove <- read_google_sheet("Lake Stage_Seal Cove_1999-2018")
tarn <- read_google_sheet("Lake Stage_The Tarn_2000-2016")
uphadlock <- read_google_sheet("Lake Stage_Upper Hadlock_1999-2016")
witchhole <- read_google_sheet("Lake Stage_Witch Hole_1999-2016")

###2017 stage
#function for reading the Stage 2017, 2018, and 19 spreadsheets
read_later_sheet <- function(spreadsheetname,worksheetname) {
  x <- gs_title(as.character(spreadsheetname))
  x <- gs_read(x, ws = as.character(worksheetname))
  colnames(x) <- as.character(as.vector(x[1,]))
  x <- x[-c(1),]
  
  return(x)
}


lowhadlock2017 <- read_later_sheet("Stage 2017", "Lower Hadlock")
jordan2017 <- read_later_sheet("Stage 2017", "Jordan Pond")
bubble20171 <- read_later_sheet("Stage 2017", "Bubble Pond")
bubble20172 <- read_later_sheet("Stage 2017", "Bubble Pond 2")
uphadlock2017 <- read_later_sheet("Stage 2017", "Upper Hadlock")
roundpond2017 <- read_later_sheet("Stage 2017", "Round Pond")
hodgdon20171 <- read_later_sheet("Stage 2017", "Hodgdon Pond")
hodgdon20172 <- read_later_sheet("Stage 2017", "Hodgdon Pond 2")
sealcove2017 <- read_later_sheet("Stage 2017", "Seal Cove Pond")
longpond2017 <- read_later_sheet("Stage 2017", "Long Pond")
seawall2017 <- read_later_sheet("Stage 2017", "Seawall Pond")
echo2017 <- read_later_sheet("Stage 2017", "Echo Lake")
lakewoodsd232017 <- read_later_sheet("Stage 2017", "Lake Wood SD2 & SD3")
tarn2017 <- read_later_sheet("Stage 2017", "The Tarn")
beaverdam2017 <- read_later_sheet("Stage 2017", "Beaver Dam")
eagle20171 <- read_later_sheet("Stage 2017", "Eagle Lake")
eagle20172 <- read_later_sheet("Stage 2017", "Eagle Lake 2")
auntbetty2017 <- read_later_sheet("Stage 2017", "Aunt Betty's")
witchhole2017 <- read_later_sheet("Stage 2017", "Witch Hole")
lowbreakneck2017 <- read_later_sheet("Stage 2017", "Lower Breakneck")
upbreakneck2017 <- read_later_sheet("Stage 2017", "Upper Breakneck")

##2018 stage
lowhadlock2018 <- read_later_sheet("Stage 2018", "Lower Hadlock")
jordan2018 <- read_later_sheet("Stage 2018", "Jordan Pond")
bubble20181 <- read_later_sheet("Stage 2018", "Bubble Pond")
bubble20182 <- read_later_sheet("Stage 2018", "Bubble Pond 2")
uphadlock2018 <- read_later_sheet("Stage 2018", "Upper Hadlock")
roundpond2018 <- read_later_sheet("Stage 2018", "Round Pond")
hodgdon20181 <- read_later_sheet("Stage 2018", "Hodgdon Pond")
hodgdon20182 <- read_later_sheet("Stage 2018", "Hodgdon Pond 2")
sealcove2018 <- read_later_sheet("Stage 2018", "Seal Cove Pond")
longpond2018 <- read_later_sheet("Stage 2018", "Long Pond")
seawall2018 <- read_later_sheet("Stage 2018", "Seawall Pond")
echo2018 <- read_later_sheet("Stage 2018", "Echo Lake")
lakewoodsd232018 <- read_later_sheet("Stage 2018", "Lake Wood SD2 & SD3")
tarn2018 <- read_later_sheet("Stage 2018", "The Tarn")
beaverdam2018 <- read_later_sheet("Stage 2018", "Beaver Dam")
eagle20181 <- read_later_sheet("Stage 2018", "Eagle Lake")
eagle20182 <- read_later_sheet("Stage 2018", "Eagle Lake 2")
auntbetty2018 <- read_later_sheet("Stage 2018", "Aunt Betty's")
witchhole2018 <- read_later_sheet("Stage 2018", "Witch Hole")
lowbreakneck2018 <- read_later_sheet("Stage 2018", "Lower Breakneck")
upbreakneck2018 <- read_later_sheet("Stage 2018", "Upper Breakneck")

###2019 Stage
lowhadlock2019 <- read_later_sheet("Stage 2019", "Lower Hadlock")
jordan2019 <- read_later_sheet("Stage 2019", "Jordan Pond")
bubble20191 <- read_later_sheet("Stage 2019", "Bubble Pond")
bubble20192 <- read_later_sheet("Stage 2019", "Bubble Pond 2")
uphadlock2019 <- read_later_sheet("Stage 2019", "Upper Hadlock")
roundpond2019 <- read_later_sheet("Stage 2019", "Round Pond")
hodgdon20191 <- read_later_sheet("Stage 2019", "Hodgdon Pond")
hodgdon20192 <- read_later_sheet("Stage 2019", "Hodgdon Pond 2")
sealcove2019 <- read_later_sheet("Stage 2019", "Seal Cove Pond")
longpond2019 <- read_later_sheet("Stage 2019", "Long Pond")
seawall2019 <- read_later_sheet("Stage 2019", "Seawall Pond")
echo2019 <- read_later_sheet("Stage 2019", "Echo Lake")
lakewoodsd232019 <- read_later_sheet("Stage 2019", "Lake Wood SD2 & SD3")
tarn2019 <- read_later_sheet("Stage 2019", "The Tarn SD1")
beaverdam2019 <- read_later_sheet("Stage 2019", "Beaver Dam")
eagle20191 <- read_later_sheet("Stage 2019", "Eagle Lake")
eagle20192 <- read_later_sheet("Stage 2019", "Eagle Lake 2")
auntbetty2019 <- read_later_sheet("Stage 2019", "Aunt Betty's")
witchhole2019 <- read_later_sheet("Stage 2019", "Witch Hole")
lowbreakneck2019 <- read_later_sheet("Stage 2019", "Lower Breakneck")
upbreakneck2019 <- read_later_sheet("Stage 2019", "Upper Breakneck SD1")


X <- gs_title("Stage 2017")
x[["ws"]]
length(x$ws$ws_title)
# for (i in length(x$ws$ws_title)) {
#   ws_title <- read_later_sheet("Stage 2017", i)
# }

#columns2 <- c("Data Entered by", "Date","Rain in last 48 hrs","Distance From Datum (feet)","Tape Up (TU)/Tape Down(TD)","Water temp. (C)","Time","Comments (Weather conditions, etc)","Observer","Wave Height (ft)","Distance from datum to ground (ft)","Week #","Water Level (in relation to datum)","datum")
# colnames(jordan2017)
# colnames <- as.character(as.vector(jordan2017[1,]))
# colnames(jordan2017) <- as.character(as.vector(jordan2017[1,]))
# jordan2017 <- jordan2017[-c(1),]


colnames(uphadlock)[5]
str_detect(names(df[12]), "Distance from Datum")
colnames(df[5])
df <- uphadlock

colfunction <- function(df) {
  df <- df[,c(1:14)]
  
  for(i in 1:ncol(df)){
    
    ifelse(str_detect(names(df[i]), "Distance from Datum|Distance From Datum|Feet below datum|Feet below datum, site 1|Feet from datum") == TRUE,
           names(df)[i] <- "Distance From Datum",
           ifelse(str_detect(names(df[i]), "Temp|temp") == TRUE,
                  names(df)[i] <- "Wtemp",
                  df <- df))
  }
  
  
  df <- df %>%
    mutate(Date = as_date(mdy(Date)),
           "Distance From Datum" = as.numeric(`Distance From Datum`)) %>% #changing to same date format as other sheets
    drop_na("Distance From Datum") %>%
    drop_na("Date")
  df <- df[,c("Date","Distance From Datum","Wtemp")]
  # df <- df %>%
  #   mutate("Distance From Datum" = as.numeric(`Distance From Datum`))
  return(df)
}
#x <- colfunction(uphadlock)
#names(ldf)[site]

#function joins all the data and 
join_function <- function(df,df2017,df2018,df2019) {
  df <- colfunction(df)
  df2017 <- colfunction(df2017)
  df2018 <- colfunction(df2018)
  df2019 <- colfunction(df2019)
  x <- rbind(df, df2017,df2018,df2019) #puts the data together vertically
  #print(nrow(x))
  x <- x[!duplicated(x$Date), ] #deletes rows of the same date
  x <- x %>%
    arrange(Date)
  return(x)
}

hadlockmaster <- join_function(uphadlock,uphadlock2017,uphadlock2018,uphadlock2019)
hadlockmaster <- hadlockmaster %>%
  mutate(name = "Upper Hadlock")
write_csv(hadlockmaster,"hadlock.csv")

echomaster <- join_function(echo,echo2017,echo2018,echo2019)
echomaster <- echomaster %>%
  mutate(name = "Echo Lake")
write_csv(echomaster,"echo.csv")

jordanmaster <- join_function(jordan,jordan2017,jordan2018,jordan2019)
jordanmaster <- jordanmaster %>%
  mutate(name = "Jordan Pond")
write_csv(jordanmaster,"jordan.csv")

sealcovemaster <- join_function(sealcove,sealcove2017,sealcove2018,sealcove2019)
sealcovemaster <- sealcovemaster %>%
  mutate(name = "Seal Cove")
write_csv(sealcovemaster,"sealcove.csv")

tarnmaster <- join_function(tarn,tarn2017,tarn2018,tarn2019)
tarnmaster <- tarnmaster %>%
  mutate(name = "The Tarn")
write_csv(tarnmaster,"tarn.csv")

witchholemaster <- join_function(witchhole,witchhole2017,witchhole2018,witchhole2019)
witchholemaster <- witchholemaster %>%
  mutate(name = "Witch Hole")
write_csv(witchholemaster,"witchhole.csv")

#for the locations that only have 3 datasheets
join_function2 <- function(df2017,df2018,df2019) {
  
  df2017 <- colfunction(df2017)
  df2018 <- colfunction(df2018)
  df2019 <- colfunction(df2019)
  x <- rbind(df2017,df2018,df2019) #puts the data together vertically
  #print(nrow(x))
  x <- x[!duplicated(x$Date), ] #deletes rows of the same date
  x <- x %>%
    arrange(Date)
  return(x)
}

upbreakneckmaster <- join_function2(upbreakneck2017,upbreakneck2018,upbreakneck2019)
upbreakneckmaster <- upbreakneckmaster %>%
  mutate(name = "Upper Breakneck Pond")
write_csv(upbreakneckmaster,"upbreakneck.csv")

seawallmaster <- join_function2(seawall2017,seawall2018,seawall2019)
seawallmaster <- seawallmaster %>%
  mutate(name = "Seawall")
write_csv(seawallmaster,"seawall.csv")

roundpondmaster <- join_function2(roundpond2017,roundpond2018,roundpond2019)
roundpondmaster <- roundpondmaster %>%
  mutate(name = "Round Pond")
write_csv(roundpondmaster,"roundpond.csv")

lowhadlockmaster <- join_function2(lowhadlock2017,lowhadlock2018,lowhadlock2019)
lowhadlockmaster <- lowhadlockmaster %>%
  mutate(name = "Lower Hadlock Pond")
write_csv(lowhadlockmaster,"lowhadlock.csv")

lowbreakneckmaster <- join_function2(lowbreakneck2017,lowbreakneck2018,lowbreakneck2019)
lowbreakneckmaster <- lowbreakneckmaster %>%
  mutate(name = "Lower Breakneck Pond")
write_csv(lowbreakneckmaster,"lowbreakneck.csv")

longpondmaster <- join_function2(longpond2017,longpond2018,longpond2019)
longpondmaster <- longpondmaster %>%
  mutate(name = "Long Pond")
write_csv(longpondmaster,"longpond.csv")

auntbettymaster <- join_function2(auntbetty2017,auntbetty2018,auntbetty2019)
auntbettymaster <- auntbettymaster %>%
  mutate(name = "Aunt Betty Pond")
write_csv(auntbettymaster,"auntbetty.csv")

beaverdammaster <- join_function2(beaverdam2017,beaverdam2018,beaverdam2019)
beaverdammaster <- beaverdammaster %>%
  mutate(name = "Beaver Dam")
write_csv(beaverdammaster,"beaverdam.csv")

#gonna have to mess around with eagle, bubble, and hodgdon as well as lakewood
# lakewoodsmaster <- join_function2(lakewoodsd232017,lakewoodsd232018,lakewoodsd232019)
# write_csv(lakewoodsmaster,"lakewoodsd3.csv") #this is sd3 beacause I elimiated the sd2 columnn because there wasn't much data on it
#colnames(lakewoodsd232017)

#for data with 6 dataframes
join_function3 <- function(df20171,df20172,df20181,df20182,df20191,df20192) {
  
  df20171 <- colfunction(df20171)
  df20172 <- colfunction(df20172)
  
  df20181 <- colfunction(df20181)
  df20182 <- colfunction(df20182)
  
  df20191 <- colfunction(df20191)
  df20192 <- colfunction(df20192)
  
  x <- rbind(df20171,df20172,df20181,df20182,df20191,df20192) #puts the data together vertically
  #print(nrow(x))
  x <- x[!duplicated(x$Date), ] #deletes rows of the same date
  x <- x %>%
    arrange(Date)
  return(x)
}


hodgdonmaster <- join_function3(hodgdon20171,hodgdon20172,hodgdon20181,hodgdon20182,hodgdon20191,hodgdon20192)
hodgdonmaster <- hodgdonmaster %>%
  mutate(name = "Hodgdon Pond")
write_csv(hodgdonmaster,"hodgdon.csv")

eaglemaster <- join_function3(eagle20171,eagle20172,eagle20181,eagle20182,eagle20191,eagle20192)
eaglemaster <- eaglemaster %>%
  mutate(name = "Eagle Lake")
write_csv(eaglemaster,"eagle.csv")

bubblemaster <- join_function3(bubble20171,bubble20172,bubble20181,bubble20182,bubble20191,bubble20192)
bubblemaster <- bubblemaster %>%
  mutate(name = "Bubble Pond")
write_csv(bubblemaster,"bubble.csv")

##for lakewood
colfunction_lakewood <- function(df) {
  df <- df[,c(1:14)]
  
  for(i in 1:ncol(df)){
    
    ifelse(str_detect(names(df[i]), "Feet below datum, site 1") == TRUE,
           names(df)[i] <- "Distance From Datum",
           ifelse(str_detect(names(df[i]), "Temp|temp") == TRUE,
                  names(df)[i] <- "Wtemp",
                  df <- df))
  }
  
  
  df <- df %>%
    mutate(Date = as_date(mdy(Date)),
           "Distance From Datum" = as.numeric(`Distance From Datum`)) %>% #changing to same date format as other sheets
    drop_na("Distance From Datum") %>%
    drop_na("Date")
  df <- df[,c("Date","Distance From Datum","Wtemp")]
  # df <- df %>%
  #   mutate("Distance From Datum" = as.numeric(`Distance From Datum`))
  return(df)
}

join_function_lakewood <- function(df2017,df2018,df2019) {
  
  df2017 <- colfunction_lakewood(df2017)
  df2018 <- colfunction_lakewood(df2018)
  df2019 <- colfunction_lakewood(df2019)
  x <- rbind(df2017,df2018,df2019) #puts the data together vertically
  #print(nrow(x))
  x <- x[!duplicated(x$Date), ] #deletes rows of the same date
  x <- x %>%
    arrange(Date)
  return(x)
}

lakewoodsd3master <- join_function_lakewood(lakewoodsd232017,lakewoodsd232018,lakewoodsd232019)
lakewoodsd3master <- lakewoodsd3master %>%
  mutate(name = "Lake Wood")
write_csv(lakewoodsd3master,"lakewoodsd3.csv")

x <- read_csv("lakewoodsd3.csv")
###reading all csvs and getting them to list
hadlock <- read_csv("hadlock.csv")
jordan <- read_csv("jordan.csv")
sealcove <- read_csv("sealcove.csv")
tarn <- read_csv("tarn.csv")
echo <- read_csv("echo.csv")
witchhole <- read_csv("witchhole.csv")
upbreakneck <- read_csv("upbreakneck.csv")
seawall <- read_csv("seawall.csv")
roundpond <- read_csv("roundpond.csv")
lowhadlock <- read_csv("lowhadlock.csv")
lowbreakneck <- read_csv("lowbreakneck.csv")
longpond <- read_csv("longpond.csv")
auntbetty <- read_csv("auntbetty.csv")
beaverdam <- read_csv("beaverdam.csv")
hodgdon <- read_csv("hodgdon.csv")
eagle <- read_csv("eagle.csv")
bubble <- read_csv("bubble.csv")
lakewoodsd3 <- read_csv("lakewoodsd3.csv")

ldf <- list(hadlock,jordan,sealcove,tarn,echo,witchhole,upbreakneck,seawall,roundpond,lowhadlock,lowbreakneck,longpond,auntbetty,beaverdam,hodgdon,eagle,bubble,lakewoodsd3)
names1 <- c("Upper.Hadlock","Jordan.Pond", "Seal.Cove","The.Tarn","Echo.Lake","Witch.Hole","Upper.Breakneck","Seawall.Pond","Round.Pond","Lower.Hadlock","Lower.Breakneck","Long.Pond","Aunt.Betty.Pond","Beaver.Dam","Hodgdon.Pond","Eagle.Lake","Bubble.Pond","Lakewood.sd3")
names(ldf) <- names1


#graphs


alllakes <- lapply(ldf, function(x) {
  filter(x, Date == 2018)
})

ldf[["Upper.Hadlock"]] %>%
  ggplot(aes(x = Date,
             y = `Distance From Datum`)) +
  ggseasonplot()


#tryign seasonal plot
library(ggplot2)
library(forecast)
?ggseasonplot
theme_set(theme_classic())
x <- as.ts(hadlock)
x <- hadlock

# Subset data
#nottem_small <- window(nottem, start=c(1920, 1), end=c(1925, 12))  # subset a smaller timewindow

# Plot
View(AirPassengers)
ggseasonplot(AirPassengers) + labs(title="Seasonal plot: International Airline Passengers")
seasonplot(as.ts(hadlock), s = ) + labs(title="Seasonal plot: Hadlock")
?ggseasonplot
#ggseasonplot(nottem_small) + labs(title="Seasonal plot: Air temperatures at Nottingham Castle")

x %>% 
  mutate(
    year = factor(year(Date)),     # use year to define separate curves
    date = update(Date, year = 1)) %>%  # use a constant year for the x-axis
  ggplot(aes(date, `Distance From Datum`, color = year)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  geom_line()
names1[1]
#p + geom_line()
