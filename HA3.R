library(rio)
library(tidyverse)
library(stringi)

berliner_staddteile <- c("Gesundbrunnen", "Hansaviertel", "Mitte", "Moabit","Tiergarten", 
                        "Wedding","Friedrichshain", "Kreuzberg","Blankenburg", "Blankenfelde", 
                        "Buch", "Französisch Buchholz", "Heinersdorf", "Karow", "Pankow", 
                        "Niederschönhausen", "Prenzlauer Berg", "Rosenthal", 
                        "Stadtrandsiedlung Malchow", "Weißensee", "Wilhelmsruh","Charlottenburg", 
                        "Charlottenburg-Nord", "Grunewald", "Halensee", "Schmargendorf", "Westend", 
                        "Wilmersdorf", "Falkenhagener Feld", "Gatow", "Hakenfelde", "Haselhorst", 
                        "Kladow", "Siemensstadt", "Spandau", "Staaken", "Wilhelmstadt",
                        "Dahlem", 
                        "Lankwitz", "Lichterfelde", "Nikolassee", "Steglitz", "Wannsee", "Zehlendorf",
                        "Friedenau", "Lichtenrade", "Mariendorf", "Marienfelde","Schöneberg")

abbreviatedBerlinerStaddteile <- abbreviate(berliner_staddteile)

df <- import("C:/Users/degha/Desktop/Praktische Datenanalyze-Hausaufgaben/geburtstag.sav")


desc_len <- nchar(df$beschrei)
max_len <- max(desc_len, na.rm = TRUE)
min_len <- min(desc_len[desc_len > 0], na.rm = TRUE)

longest_desc = df[desc_len == max_len, c("name", "beschrei")]
shortest_desc = df[desc_len == min_len, c("name", "beschrei")] 
no_description = sum(desc_len == 0 | is.na(desc_len))

max_length_names = df$name[desc_len == max_len]

sorted_by_year = df[desc_len == max_len, ][order(-df$jahr[desc_len == max_len]),]

name_counts = list(
  starts_E = sum(grepl("^E", df$name)),
  starts_K_or_N = sum(grepl("^[KN]", df$name)),
  ends_us = sum(grepl("us$", df$name)),
  starts_T_ends_a = sum(grepl("^T.*a$", df$name)),
  contains_U = sum(grepl("U", df$name)),
  E_to_F = sum(grepl("^[E-F]", df$name)),
  Albert = sum(df$name == "Albert")
)

first_names = {
  name_parts <- strsplit(df$name, " ")
  first_name <- sapply(name_parts, `[`, 1)
  table(first_name)["Albert"]
}



