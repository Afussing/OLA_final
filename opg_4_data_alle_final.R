library(tidyverse)
library(ggsoccer)
library(stringr)
library(mongolite)
library(jsonlite)

#### Opgave 4.1 ####
# Generelt for alle skal der establish en connection først:

conmatches <- mongo(collection = "matches", 
                    db = "NewWyscout", 
                    url = "mongodb://localhost"
)

congames <- mongo(collection = "games", 
                  db = "NewWyscout", 
                  url = "mongodb://localhost")

conspiller <- mongo(collection = "players", 
                    db = "NewWyscout", 
                    url = "mongodb://localhost")

#############################
###### Data opgave 4.1 ######
#############################
# Lav en dynamisk webapp hvor man ser et bar-plot af spillerens samlede målscore og xG-sum 
# og hvor man kan regulere antallet af spillere. I skal vedlægge koden. Det gælder for alle tre opgaver.

# Polen
dfmål <- as.data.frame(flatten(congames$find('{"shot.isGoal": true}'))) 

df_polen_label <- as.data.frame(flatten(conmatches$find(query = '{"competitionId": 692}',
                                                        fields = '{}')))
colnames(df_polen_label)[1] = "matchId"
dfmål_polen <- inner_join(dfmål, df_polen_label, by = "matchId")

### Opretter nu kolonne med sæson
# Først sorteres kolonne data, så tidspunkt ikke fremgår og sæson-kolonne oprettes

dfmål_polen$date <- as.Date(gsub(" \\d{2}:\\d{2}:\\d{2}$", "", dfmål_polen$date))

dfmål_polen$sæson <- ifelse(dfmål_polen$date < as.Date("2022-05-22"), "21/22", "22/23")

dfmål_polen <- dfmål_polen %>% mutate(
  player.name.final = paste0(player.name, " - ", team.name)
)

# Holland
df_holland_label <- as.data.frame(flatten(conmatches$find(query = '{"competitionId": 635}',
                                                          fields = '{}')))
colnames(df_holland_label)[1] = "matchId"
dfmål_holland <- inner_join(dfmål, df_holland_label, by = "matchId")

### Opretter nu kolonne med sæson
# Først sorteres kolonne data, så tidspunkt ikke fremgår og sæson-kolonne oprettes

dfmål_holland$date <- as.Date(gsub(" \\d{2}:\\d{2}:\\d{2}$", "", dfmål_holland$date))

dfmål_holland$sæson <- ifelse(dfmål_holland$date < as.Date("2022-05-16"), "21/22", "22/23")

dfmål_holland <- dfmål_holland %>% mutate(
  label_final = paste0(date,": ", label)
)

dfmål_holland <- dfmål_holland %>% mutate(
  player.name.final = paste0(player.name, " - ", team.name)
)


#############################
###### Data opgave 4.2 ######
#############################

# Polen

### Dataframes Matches og Games sammensættes ###

df_polen_vl <- paste0(df_polen_label$matchId, collapse = ",")

query_polen <- paste0('{"type.primary": "pass", "matchId": {"$in": [',df_polen_vl,'] }}')

dfpolen <- congames$find(query = query_polen,
                         fields = '{}')

df_polen <- as.data.frame(flatten(dfpolen))

df_polen <- inner_join(df_polen,df_polen_label, by = "matchId")

### Opretter nu kolonne med sæson
# Først sorteres kolonne data, så tidspunkt ikke fremgår og sæson-kolonne oprettes

df_polen$date <- as.Date(gsub(" \\d{2}:\\d{2}:\\d{2}$", "", df_polen$date))

df_polen$sæson <- ifelse(df_polen$date < as.Date("2022-05-22"), "21/22", "22/23")

# Label sammensættes med dato

df_polen <- df_polen %>% mutate(
  label_final = paste0(date,": ",label)
)

df_polen <- df_polen %>% mutate(
  Hjemmebane = str_extract(label, "^[^\\-]+") %>% str_trim(), #  Beholder alle ord før det første "-"
  Udebane = str_extract(label, "(?<=-\\s)[^\\d]+") %>% str_trim()
)

# Holland


### Dataframes Matches og Games sammensættes ###


holland_vl <- paste0(df_holland_label$matchId, collapse = ",")
query_holland <- paste0('{"type.primary": "pass", "matchId": {"$in": [',holland_vl,'] }}')

dfholland <- congames$find(query = query_holland,
                         fields = '{}')

df_holland <- as.data.frame(flatten(dfholland))

df_holland <- inner_join(df_holland,df_holland_label, by = "matchId")

### Opretter nu kolonne med sæson
# Først sorteres kolonne data, så tidspunkt ikke fremgår og sæson-kolonne oprettes

df_holland$date <- as.Date(gsub(" \\d{2}:\\d{2}:\\d{2}$", "", df_holland$date))

df_holland$sæson <- ifelse(df_holland$date < as.Date("2022-05-22"), "21/22", "22/23")

# Label sammensættes med dato

df_holland <- df_holland %>% mutate(
  label_final = paste0(date,": ",label)
)

df_holland <- df_holland %>% mutate(
  Hjemmebane = str_extract(label, "^[^\\-]+") %>% str_trim(), #  Beholder alle ord før det første "-"
  Udebane = str_extract(label, "(?<=-\\s)[^\\d]+") %>% str_trim()
)


#############################
###### Data opgave 4.3 ######
#############################

# Lav en dynamisk webapp vha ggsoccer, hvor man kan vælge en kamp og derpå se et plot af skud og hvor fordelingen mellem de to hold vises vha farve.
# Desuden skal man vha farven kunne se hvilke skud der gik i mål. 

### Forberedelse af data.frames

# Polen
dfskud_polen <- as.data.frame(flatten(congames$find('{"type.primary": { "$in": ["shot", "penalty"]}}')))
dfskud_polen <- inner_join(dfskud_polen,df_polen_label, by = "matchId")

dfskud_polen <- dfskud_polen %>% mutate(
  Hjemmebane = str_extract(label, "^[^\\-]+") %>% str_trim(), #  Beholder alle ord før det første "-"
  Udebane = str_extract(label, "(?<=-\\s)[^\\d]+") %>% str_trim() %>% gsub(",", "", .)
)
unique(dfskud_polen$type.primary)
dfskud_polen$type.primary <- gsub("penalty", "shot", dfskud_polen$type.primary)

# Holland
dfskud_holland <- as.data.frame(flatten(congames$find('{"type.primary": { "$in": ["shot", "penalty"]}}')))
dfskud_holland <- inner_join(dfskud_holland,df_holland_label, by = "matchId")

dfskud_holland <- dfskud_holland %>% mutate(
  Hjemmebane = str_extract(label, "^[^\\-]+") %>% str_trim(),
  Udebane = str_extract(label, "(?<=-\\s)[^\\d]+") %>% str_trim() %>% gsub(",", "", .)
)

dfskud_holland$type.primary <- gsub("penalty", "shot", dfskud_holland$type.primary)
