# load packages
install.packages("googlesheets")
install.packages("tidyverse")
install.packages('openssl')

# activate libraries
library(tidyr)
library(readr)
library(dplyr)
library(ggplot2)
library(forcats)
library(googlesheets)
library(stringr)
library(scales) # Utilities for scales and formatting
library(lubridate)

# Read HathiTrust-generated database of Named Entities, with corresponding titles and years
# should be…
# Parsed with column specification:
#  cols(
#    vol_id = col_character(),
#    page_seq = col_integer(),
#    entity = col_character(),
#    type = col_character(),
#    source = col_character(),
#    year1 = col_integer(),
#    year2 = col_integer(),
#    year3 = col_integer()
#  )
entities <- read_csv("data/Entities-InformeR-Years.csv")
entities.tally <- (entities %>% group_by(entity) %>% tally(sort = TRUE))


# which google sheets do you have access to?
# may ask you to authenticate in a browser!
gs_ls()

# get the Bolivia database google sheet
bd <- gs_title("Bolivia Database")

# Import the Entries
de <- gs_read(ss=bd, ws = "Entries", comment="#", skip = 2)
# Create a simplified dataset with fewer variables
deaths <- select(de, event_title, year, month, day, dec_firstname, dec_surnames, dec_age, dec_gender, intentionality, community, municipality, province, department, pres_admin, protest_campaign, protest_domain, state_perpetrator, state_responsibility)
deaths <- mutate(deaths,
       date = (paste(deaths$year, deaths$month, deaths$day, sep="-") %>% 
                 ymd() %>% as.Date()))

# create an event count by title that also counts the number of state perpetrator 
#   and state victim deaths
#
event.responsibility <- deaths %>% group_by(event_title) %>%
  summarize(
    n = n(),
    n_state_perp = sum(state_perpetrator=="Yes", na.rm = TRUE),
    n_state_victim = sum(state_responsibility=="State victim", na.rm = TRUE),
    n_state_separate = sum(str_detect(state_responsibility, "Separate from state"), na.rm = TRUE)
  ) %>%
  arrange(desc(n))
# merge the event table with the corresponding year(s)
event.responsibility <- event.responsibility %>% 
  left_join(unique(select(deaths, event_title, year))) %>%
  select(event_title, year, everything())
event.responsibility3 <- filter(event.responsibility, n>3)
# barplot(data.matrix(select(event.responsibility3, n, n_state_perp, n_state_victim, n_state_separate)), col=colors()[c(23,89,12)] , border="white", font.axis=2, beside=T, legend=event.responsibility3$event_title, xlab="group", font.lab=2)

# event.tally
event.tally <- (deaths %>% group_by(event_title) %>% tally(sort = TRUE))



# creating a simpler table with just years and names to run the search of NamedEntities
deaths.names <- select(de, year, dec_firstname, dec_surnames)
# need this function to deal with NA in names
paste3 <- function(...,sep=", ") {
  L <- list(...)
  L <- lapply(L,function(x) {x[is.na(x)] <- ""; x})
  ret <-gsub(paste0("(^",sep,"|",sep,"$)"),"",
             gsub(paste0(sep,sep),sep,
                  do.call(paste,c(L,list(sep=sep)))))
  is.na(ret) <- ret==""
  ret
}
deaths.names<-mutate(deaths.names,
       FullName = paste3( dec_firstname,  dec_surnames, sep = ' '))
deaths.named <- filter(deaths.names, !is.na(FullName)) # exclude NA
deaths.named <- (deaths.named %>% separate(dec_firstname, into=c("dec_firstname1", "dec_firstname2", "dec_firstname3"), sep = ' ', extra = "merge", remove = FALSE))
deaths.named <- (deaths.named %>% separate(dec_surnames, into=c("dec_surname1", "dec_surname2", "dec_surname3"), sep = ' ', extra = "warn", remove = FALSE))
# generate frequency
deaths.named<-mutate(deaths.named,
                     FullName11 = paste3( dec_firstname1,  dec_surname1, sep = ' '))

# generatce lookup table for matched full names
fullname.lt <- match(deaths.named$FullName, entities$entity)
fullname11.lt <- match(deaths.named$FullName11, entities$entity)

# generate a blank tibble to incorporate the results
results  <- filter(entities, entity == "futile search")
# this for loop seeks out matches (per the lookup table), then prints a header row
# followed by a tibble of the corresponding matches
# all of the match results are concatenated into "results"
for (i in c(1:length(fullname.lt))) {
# replicate for loop for FullName11 (1st firstname and 1st surname)    
  if (!is.na(fullname11.lt[i])) {
      if (!is.na(deaths.named$dec_firstname2[i]) | !is.na(deaths.named$dec_surname2[i])) {
        print(paste3(deaths.named$year[i], deaths.named$FullName[i], sep = ' '))
        results.i <- filter(entities, entity == deaths.named$FullName11[i])
        results.i <- filter(results.i, (year1 == deaths.named$year[i]) | 
                 (year2 == deaths.named$year[i]) | (year3 == deaths.named$year[i]))
        if (nrow(results.i)==0) {
          print("Matched names don't match in their years.")
        } else {
          print(results.i)
          results <- bind_rows(results, results.i)
        }
      }
  }
# here's the if loop for the whole name
  if (!is.na(fullname.lt[i])) {
    print(paste3(deaths.named$year[i], deaths.named$FullName[i], sep = ' '))
    results.i <- filter(entities, entity == deaths.named$FullName[i]) 
    results.i <- filter(results.i, (year1 == deaths.named$year[i]) | 
                          (year2 == deaths.named$year[i]) | (year3 == deaths.named$year[i]))
    if (nrow(results.i)==0) {
      print("Matched names don't match in their years.")
    } else {
      print(results.i)
      results <- bind_rows(results, results.i)
    }
  }
  }




# ============================================================
# END SECTION DEALING WITH MATCHING
# 
# Everything below here is an experiment in data visualization
# ============================================================

de$`Brief identifier`


#generate tally of deaths by event
(eventfreq <- deaths %>% group_by(`Brief identifier`) %>% tally(sort = TRUE))
(eventfreq.2 <- deaths %>% group_by(`Protest Domain' broad`, `Brief identifier`) %>% tally(sort = TRUE))



# This still needs work with the months
ggplot(de, aes(as.integer(Month), as.factor(Year)))+ geom_bin2d() 

# histogram by year (currently binned into five year chunks)
ggplot(data = de) +
  geom_bar(mapping = aes (x = Year, color = `Presidential Administration`), binwidth = 5)

# reorder factors of the Presidencies by year
# This reordering automatically puts them chronologically
#   see... https://stat545.com/block029_factors.html
fct_reorder(de$`Presidential Administration`, de$Year) %>% 
  levels() %>% head()

# histogram by presidency
ggplot(data = de) + 
    geom_bar(mapping = aes (x = fct_reorder(de$`Presidential Administration`, de$Year))) +
  coord_flip()


# histogram by presidency
ggplot(data = de) +
  geom_bar(mapping = aes (x = fct_reorder(de$`Presidential Administration`, de$Year), 
                          color = `State perpetrator?`)) +
  coord_flip()

# histogram by presidency and department
ggplot(data = de) +
  geom_bar(mapping = aes (x = `Presidential Administration`, color = Department))+
  coord_flip()

ggplot(data = de) +
  geom_bar(mapping = aes (x = Year, color = Department), binwidth = 2)

# not working yet… (11February) trying for horizontal bar chart.
# see 
ggplot(de, aes(x=`Presidential Administration`, y=count(`Presidential Administration`))) +
  geom_bar(stat='identity') +
  coord_flip()

# summary table by presidency, sorted to descend
prestable <- de %>% filter(!is.na(de$`Presidential Administration`)) %>%  count(`Presidential Administration`, sort = TRUE) 

           


# ==== replicate the sunburst example below for my data here ====
sum_total_deaths = sum(eventfreq.2$n)
innerRing = eventfreq %>% summarize(total_deaths=sum(n))
sunburst_d1= ggplot(innerRing) + 
  geom_bar(data=innerRing, aes(x=1, y=total_deaths), fill='darkgrey', stat='identity') +
  geom_text(aes(x=1, y=sum_total_deaths/2, label=paste('Total deaths: ', comma(total_deaths))), color='white')
sunburst_d1 + coord_polar("y")
sunburst_d2 <- sunburst_d1 +
  geom_bar(data=eventfreq,
           aes(x=2, y=n, fill=n),
           color='white', position='stack', stat='identity', size=0.6) + 
  geom_text(data=eventfreq, aes(label=paste(`Brief identifier`, n), x=2, y=n), position='stack')
sunburst_d2 + coord_polar("y")


# === Experiment with starburst chart per https://medium.com/optima-blog/create-basic-sunburst-graphs-with-ggplot2-7d7484d92c61
# === Goal is to eventually create a sunburst chart for location (or protest domain and event)
pop.eg = read.csv('data/egypt_pop_2006.csv') # 2 cols: gov,population
sum_total_pop = sum(pop.eg$population)
firstLevel = pop.eg %>% summarize(total_pop=sum(population))
sunburst_0 = ggplot(firstLevel) # Just a foundation
sunburst_1 = 
  sunburst_0 + 
  geom_bar(data=firstLevel, aes(x=1, y=total_pop), fill='darkgrey', stat='identity') +
  geom_text(aes(x=1, y=sum_total_pop/2, label=paste('Egypt in 2006 had', comma(total_pop))), color='white')
sunburst_1
# turn the y axis into theta in polar chart
sunburst_1 + coord_polar("y")
# a new layer for governorates
gov_pop = pop.eg %>% group_by(gov) %>%
  summarize(total_pop=sum(population)) %>%
  arrange(desc(total_pop))

# stacked bar chart.
# Notice here the `position=’stack’`, this is the reason why ggplot positioned text properly
sunburst_2 <- sunburst_1 +
  geom_bar(data=gov_pop,
           aes(x=2, y=total_pop, fill=total_pop),
           color='white', position='stack', stat='identity', size=0.6) + 
  geom_text(data=gov_pop, aes(label=paste(gov, total_pop), x=2, y=total_pop), position='stack')
# now curve this into a sunburst chart
sunburst_2 + coord_polar('y')

#but labels are ugly so…
secondLevel <- gov_pop %>%
  mutate(running=cumsum(total_pop), pos=running - total_pop/2)

sunburst_2 = sunburst_1 +
  geom_bar(data=secondLevel,
           aes(x=2, y=total_pop, fill=total_pop, stroke=3),
           color='white', position='stack', stat='identity')

sunburst_2_text = sunburst_2 +
  geom_text(data=secondLevel,
            aes(label=paste(gov, comma(total_pop)), x=2, y=pos))
sunburst_2_text + coord_polar("y")

# text angle cleverness (from https://gist.github.com/yahiaelgamal/353c728d4ab4f0e078b4e9a5e9d57f07)
compute_angle = function(perc){
  angle = -1
  #if(perc < 0.25) # 1st q [90,0]
  #angle = 90 - (perc/0.25) * 90
  #else if(perc < 0.5) # 2nd quarter [0, -90]
  #angle = (perc-0.25) / 0.25 * -90
  #else if(perc < 0.75) # 3rd q [90, 0]
  #angle = 90 - ((perc-0.5) / 0.25 * 90)
  #else if(perc < 1.00) # last q [0, -90]
  #angle = ((perc -0.75)/0.25) * -90
  
  if(perc < 0.5) # 1st half [90, -90]
    angle = (180 - (perc/0.5) * 180) - 90
  else # 2nd half [90, -90]
    angle = (90 - ((perc - 0.5)/0.5) * 180)
  
  return(angle)
}

secondLevel = gov_pop %>%
  mutate(running=cumsum(total_pop), pos=running - total_pop/2) %>% group_by(1:n()) %>% 
  mutate(angle=compute_angle((running - total_pop/2) / sum_total_pop))
sunburst_2 = sunburst_1 + geom_bar(data=secondLevel,
                                   aes(x=2, y=total_pop, fill=total_pop, stroke=3),
                                   color='white', position='stack', stat='identity')
sunburst_3 = sunburst_2 + geom_text(data=secondLevel, aes(label=paste(gov, comma(total_pop)), x=2, y=pos, angle=angle))
sunburst_3 + scale_y_continuous(labels=comma) + scale_fill_continuous(low='white', high='darkred') + coord_polar('y') + theme_minimal()

# === End starburst chart



