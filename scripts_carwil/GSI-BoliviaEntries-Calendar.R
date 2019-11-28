# load packages
install.packages("googlesheets")
install.packages("tidyverse")
# install.packages("tidyr")

# activate libraries
library(tidyr)
library(dplyr)
library(magrittr)
library(reshape)
library(scales)
library(ggplot2)
library(googlesheets)

# which google sheets do you have access to?
# may ask you to authenticate in a browser!
gs_ls()

# get the Bolivia database google sheet
bd <- gs_title("Bolivia Database")

# list worksheets
gs_ws_ls(bd)

# get State perpetrator direct deaths
calendar_spdd <- gs_read(ss=bd, ws = "State perpetrator direct deaths", range = "A3:M38", col_names=c("Year",1,2,3,4,5,6,7,8,9,10,11,12))
calendar_spdd.long <- gather(calendar_spdd, "month", "deaths", 2:13)
calendar_spdd.long$month <-as.integer(calendar_spdd.long$month)
arrange(calendar_spdd.long, Year, month)

# make a heatmap for the calendar
calendar <- calendar_spdd.long
textcol <- "grey60"

p <- ggplot(calendar, aes(x = month, y = Year, fill = deaths)) +
  geom_tile(aes(fill = deaths)) + 
  geom_tile(colour="white", size=0.25, show.legend=FALSE) + 
  scale_y_continuous(trans = "reverse", 
                     breaks = unique(calendar$Year)) + 
  scale_x_discrete(expand=c(0,0),
                   breaks = unique(calendar$month)) +
  scale_fill_gradientn(colours=c("white","red")) + 
  geom_text(aes(label=deaths)) + 
  theme(axis.text.y=element_text(colour=textcol, face="bold"), 
        axis.text.x=element_text(colour=textcol), 
        axis.ticks=element_line(size=0.4), 
        plot.background=element_blank(), 
        panel.border=element_blank())

plot(p)

# old version: scale_y_discrete(expand=c(0,0),breaks=c("1985", "1990", "1995", "2000", "2005", "2010", "2015"))
# scale_y_continuous(trans = "reverse", breaks = unique(calendar$Year))
