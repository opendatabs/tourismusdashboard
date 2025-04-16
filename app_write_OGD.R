### Tourismus #######################################################################################
####################### Write Data to OGD ###########################################################
#####################################################################################################

# install.packages("pacman")
pacman::p_load(RODBC, odbc, DBI, tidyverse)

# Establish connection using the file DSN path
conn <- dbConnect(odbc::odbc(), .connection_string = Sys.getenv("DB_CONNECTION"))
# List all tables in the schema to confirm connection
tables <- dbListTables(conn,  schema = "elm")
print("Available tables in the schema:")
print(tables)

# Define the schema and table name
schema <- "elm"
table_name1 <- "vHotelDayData"
table_name2 <- "vHotelDayNationData"

# Query for the whole df:
query_finale1 <- sprintf("SELECT * FROM %s.%s", schema, table_name1)
vHotelDayData <- dbGetQuery(conn, query_finale1)
glimpse(vHotelDayData)

# Query for the whole df:
query_finale2 <- sprintf("SELECT * FROM %s.%s", schema, table_name2)
vHotelDayNationData <- dbGetQuery(conn, query_finale2)
glimpse(vHotelDayNationData)

# Close the database connection
dbDisconnect(conn)
print("Connection closed.")


# Daten aus dwh anpassen, damit sie die Struktur der OGD-Daten erhalten
# tourismus_taeglich_1:
tourismus_taeglich_1 <- vHotelDayData %>%
  mutate(hotelKategorie = recode(hotelKategorie,
                                 "1-Sternhotel" = "1- und 2-Stern",
                                 "2-Sternhotel" = "1- und 2-Stern",
                                 "3-Sternhotel" = "3-Stern",
                                 "4-Sternhotel" = "4- und 5-Stern",
                                 "5-Sternhotel" = "4- und 5-Stern")) %>%
  filter(istGeschlossen == 0) %>%
  select(Datum = tagDatum,
         Datum_Jahr = jahrNummer,
         Monat = monatInJahrNummer,
         Datum_Tag = tagInMonatNummer,
         Hotelkategorie = hotelKategorie,
         `Anzahl Ankünfte` = anzAnkuenfte,
         `Anzahl Logiernächte` = anzUebernachtungen,
         `Anzahl verfügbare Zimmer` = anzZimmerverfuegbar,
         `Anzahl belegte Zimmer` = anzZimmerBelegt) %>%
  mutate(Datum = as.Date(Datum),
         Datum_Monat = recode(Monat,
                              "1" = "Januar",
                              "2" = "Februar",
                              "3" = "März",
                              "4" = "April",
                              "5" = "Mai",
                              "6" = "Juni",
                              "7" = "Juli",
                              "8" = "August",
                              "9" = "September",
                              "10" = "Oktober",
                              "11" = "November",
                              "12" = "Dezember"),
         Datum_Monat_Tag = as.character(substr(Datum, 6, 10)),
         Zimmerauslastung = (`Anzahl belegte Zimmer` / `Anzahl verfügbare Zimmer`) * 100) %>%
  select(Datum:Datum_Jahr,
         Datum_Monat,
         Monat:Datum_Tag,
         Datum_Monat_Tag,
         Hotelkategorie,
         everything()) %>%
  group_by(Datum, Datum_Jahr, Datum_Monat, Monat, Datum_Tag, Datum_Monat_Tag, Hotelkategorie) %>%
  summarise(`Anzahl Ankünfte` = sum(`Anzahl Ankünfte`),
            `Anzahl Logiernächte` = sum(`Anzahl Logiernächte`),
            `Anzahl verfügbare Zimmer` = sum(`Anzahl verfügbare Zimmer`),
            `Anzahl belegte Zimmer` = sum(`Anzahl belegte Zimmer`),
            Zimmerauslastung = sum(`Anzahl belegte Zimmer`/`Anzahl verfügbare Zimmer`*100),
            .groups = 'drop') %>%
  {. ->> tourismus_taeglich_dwh_hot_ungrouped} %>%
  bind_rows(tourismus_taeglich_dwh_hot_ungrouped %>%
              group_by(Datum, Datum_Jahr, Datum_Monat, Monat, Datum_Tag, Datum_Monat_Tag) %>%
              summarise(`Anzahl Ankünfte` = sum(`Anzahl Ankünfte`),
                        `Anzahl Logiernächte` = sum(`Anzahl Logiernächte`),
                        `Anzahl verfügbare Zimmer` = sum(`Anzahl verfügbare Zimmer`),
                        `Anzahl belegte Zimmer` = sum(`Anzahl belegte Zimmer`),
                        Zimmerauslastung = sum(`Anzahl belegte Zimmer`/`Anzahl verfügbare Zimmer`*100)) %>%
              mutate(Hotelkategorie = "Total")) %>%
  arrange(Datum) %>% 
  rename('Datum Jahr' = Datum_Jahr,
         'Datum Monat' = Datum_Monat,
         'Datum Tag' = Datum_Tag,
         'Datum Monat Tag' = Datum_Monat_Tag)


# tourismus_taeglich_2:
tourismus_taeglich_2 <- vHotelDayNationData %>%
  mutate(hotelKategorie = recode(hotelKategorie,
                                 "1-Sternhotel" = "1- und 2-Stern",
                                 "2-Sternhotel" = "1- und 2-Stern",
                                 "3-Sternhotel" = "3-Stern",
                                 "4-Sternhotel" = "4- und 5-Stern",
                                 "5-Sternhotel" = "4- und 5-Stern")) %>%
  select(Datum = tagDatum,
         Datum_Jahr = jahrNummer,
         Monat = monatInJahrNummer,
         Datum_Tag = tagInMonatNummer,
         Hotelkategorie = hotelKategorie,
         `Anzahl Ankünfte` = anzAnkuenfte,
         `Anzahl Logiernächte` = anzUebernachtungen,
         Nationalitaet = nationBez) %>%
  mutate(Datum = as.Date(Datum),
         Datum_Monat = recode(Monat,
                              "1" = "Januar",
                              "2" = "Februar",
                              "3" = "März",
                              "4" = "April",
                              "5" = "Mai",
                              "6" = "Juni",
                              "7" = "Juli",
                              "8" = "August",
                              "9" = "September",
                              "10" = "Oktober",
                              "11" = "November",
                              "12" = "Dezember"),
         Datum_Monat_Tag = as.character(substr(Datum, 6, 10))) %>%
  select(Datum:Datum_Jahr,
         Datum_Monat,
         Monat:Datum_Tag,
         Datum_Monat_Tag,
         Hotelkategorie,
         `Anzahl Ankünfte`:`Anzahl Logiernächte`,
         Nationalitaet) %>%
  group_by(Datum, Datum_Jahr, Datum_Monat, Monat, Datum_Tag, Datum_Monat_Tag, Hotelkategorie, Nationalitaet) %>%
  summarise(`Anzahl Ankünfte` = sum(`Anzahl Ankünfte`),
            `Anzahl Logiernächte` = sum(`Anzahl Logiernächte`),
            .groups = 'drop') %>%
  {. ->> tourismus_taeglich_2_dwh_hot_ungrouped} %>%
  bind_rows(tourismus_taeglich_2_dwh_hot_ungrouped %>%
              group_by(Datum, Datum_Jahr, Datum_Monat, Monat, Datum_Tag, Datum_Monat_Tag, Nationalitaet) %>%
              summarise(`Anzahl Ankünfte` = sum(`Anzahl Ankünfte`),
                        `Anzahl Logiernächte` = sum(`Anzahl Logiernächte`)) %>%
              mutate(Hotelkategorie = "Total")) %>%
  {. ->> tourismus_taeglich_2_dwh_hot_ungrouped2} %>%
  bind_rows(tourismus_taeglich_2_dwh_hot_ungrouped2 %>%
              group_by(Datum, Datum_Jahr, Datum_Monat, Monat, Datum_Tag, Datum_Monat_Tag, Hotelkategorie) %>%
              summarise(`Anzahl Ankünfte` = sum(`Anzahl Ankünfte`),
                        `Anzahl Logiernächte` = sum(`Anzahl Logiernächte`)) %>%
              mutate(Nationalitaet = "Total")) %>%
  bind_rows(tourismus_taeglich_2_dwh_hot_ungrouped2 %>%
              filter(Nationalitaet != "Schweiz") %>%
              group_by(Datum, Datum_Jahr, Datum_Monat, Monat, Datum_Tag, Datum_Monat_Tag, Hotelkategorie) %>%
              summarise(`Anzahl Ankünfte` = sum(`Anzahl Ankünfte`),
                        `Anzahl Logiernächte` = sum(`Anzahl Logiernächte`)) %>%
              mutate(Nationalitaet = "Ausland")) %>%
  arrange(Datum) %>% 
  rename('Datum Jahr' = Datum_Jahr,
         'Datum Monat' = Datum_Monat,
         'Datum Tag' = Datum_Tag,
         'Datum Monat Tag' = Datum_Monat_Tag,
         Herkunftsland = Nationalitaet)


# write CSV for OGD:
# export tables:
write.csv(tourismus_taeglich_1, file = "data/100413_tourismus-daily.csv", row.names = FALSE)
write.csv(tourismus_taeglich_2, file = "data/100414_tourismus-daily.csv", row.names = FALSE)
