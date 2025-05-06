### Tourismus #######################################################################################
####################### Load Data from OGD ##########################################################
#####################################################################################################

# install.packages("pacman")
pacman::p_load(RODBC, odbc, DBI, tidyverse, data.table, httr, digest)
conflicted::conflict_prefer("year", "lubridate")
conflicted::conflict_prefer("filter", "dplyr")

# load data for events:
tourismus_events <- httr::GET("https://data.bs.ch/api/explore/v2.1/catalog/datasets/100074/exports/csv?&lang=de&timezone=Europe%2FBerlin&use_labels=true&delimiter=%3B") %>%
  content(., "text") %>%
  fread(sep=";") %>% 
  select(Datum = Veranstaltungstag, Event = Name, Hinweise) %>% 
  mutate(Datum = as.Date(Datum),
         Event = case_when(Event %in% c("Konzerte St.Jakob-Park", "Konzerte St.Jakobs-Halle & allg. Events") ~ Hinweise,
                           T ~ Event)) %>% 
  filter(Event %in% c("Art Basel", "Basel Tattoo", "Fantasy Basel", "Swiss Indoors", "Weihnachtsmarkt", "Fasnacht") | str_detect(Event, "Dispenza")) %>% 
  select(-Hinweise) %>% 
  {. ->> tourismus_events_fasnacht_fehler} %>%
  # Fasnacht enthält einen Tag zu wenig, der erste Tag der jeweiligen Fasnacht wird hinzugefügt
  bind_rows(tourismus_events_fasnacht_fehler %>%
              filter(Event == "Fasnacht") %>% 
  group_by(year = year(Datum), Event) %>%  # Group by year
  summarise(Datum = c(min(Datum) - 1, Datum), 
            .groups = "drop") %>% 
    select(-year)) %>% 
  # Jahr an Event anhängen:
  mutate(Event = paste0(Event, " ", substr(Datum, 1, 4))) %>% 
  distinct()

# load data for tourismus and left join with events: ####
tourismus_taeglich_1 <- read.csv("data/100413_tourismus-daily.csv", stringsAsFactors = FALSE, check.names = FALSE) %>% 
  rename(Datum_Jahr = 'Datum Jahr',
         Datum_Monat = 'Datum Monat',
         Datum_Tag = 'Datum Tag',
         Datum_Monat_Tag = 'Datum Monat Tag') %>% 
  mutate(Datum = as.Date(Datum, format = "%Y-%m-%d")) %>% 
  arrange(desc(Datum), Hotelkategorie) %>%
  left_join(tourismus_events)

tourismus_taeglich_2 <- read.csv("data/100414_tourismus-daily.csv", stringsAsFactors = FALSE, check.names = FALSE) %>% 
  rename(Datum_Jahr = 'Datum Jahr',
         Datum_Monat = 'Datum Monat',
         Datum_Tag = 'Datum Tag',
         Datum_Monat_Tag = 'Datum Monat Tag') %>% 
  mutate(Datum = as.Date(Datum, format = "%Y-%m-%d")) %>% 
  arrange(desc(Datum), Hotelkategorie, Herkunftsland) %>% 
  left_join(tourismus_events) 


write_if_changed <- function(data, path) {
  new_hash <- digest(data)
  if (file.exists(path)) {
    old_data <- read.csv(path)
    old_hash <- digest(old_data)
    if (identical(old_hash, new_hash)) {
      message(sprintf("No changes in %s – skipping write.", path))
      return(invisible(FALSE))
    }
  }
  write.csv(data, path, row.names = FALSE)
  message(sprintf("Wrote updated data to %s", path))
  invisible(TRUE)
}

# CSV herausschreiben:
saveRDS(tourismus_taeglich_1, file = "data/tourismus_taeglich_1.rds")
saveRDS(tourismus_taeglich_2, file = "data/tourismus_taeglich_2.rds")

