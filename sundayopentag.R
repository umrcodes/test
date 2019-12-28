new
hhdfgdfg
if (!require('pacman')) install.packages('pacman'); library('pacman')

pacman::p_load('curl','tidyverse', 'lubridate', 'writexl','taskscheduleR', 'httr', 'readxl') 
auorig <- read_csv("au.csv",
                   col_types = cols(Time = col_datetime(format = "%d/%m/%Y %H:%M")))
ghj
hkj
\hkjkjjk


au <- auorig
au <- au %>%
  arrange(au$Time)%>%
  mutate(
    year = year(au$Time),
    month = month(au$Time, abbr = TRUE, label = TRUE),
    week_startdate = floor_date(au$Time, unit = "weeks",
                                week_start = getOption("lubridate.week.start", 7)),
    week_day = wday(au$Time, abbr = TRUE, label = TRUE),
    date =  date(au$Time),
    day = day(au$Time),
    hour = hour(au$Time),
  )  %>%
  group_by(week_startdate)  %>%
  mutate(
    wopen_price = first(open),
    wopen_tagged = 
      ifelse(
      (wopen_price >= low &  wopen_price <= high & 
         week_day != 'Mon' & week_day !='Sun'), 
      "tagged_wopen", "didnt_tag"
  )) %>%
  distinct(week_startdate, wopen_tagged , .keep_all = TRUE) %>%
  filter(if(any(wopen_tagged == 'tagged_wopen')) wopen_tagged == 'tagged_wopen' else TRUE)


  write_csv(au, paste0('au',format(Sys.time(),'_%y.%m.%d_%H.%M.%S'),'.csv'))