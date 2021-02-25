# ECON 5253
# Ethan Maddy
# 2/25/2021
# PS4a

# Load file
system('wget -O dates.json "https://www.vizgr.org/historical-events/search.php?format=json&begin_date=00000101&end_date=20210219&lang=en"')
system('cat dates.json')

# Convert
library(jsonlite)
library(tidyverse)
mylist <- fromJSON('dates.json')
mydf <- bind_rows(mylist$result[-1])

# Type/class of object
class(mydf)
class(mydf$date)

# Head n rows
head(mydf)
head(mydf,10) 