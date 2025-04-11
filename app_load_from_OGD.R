### Tourismus #######################################################################################
####################### Load Data from OGD ##########################################################
#####################################################################################################

# install.packages("pacman")
pacman::p_load(RODBC, odbc, DBI, tidyverse, data.table, httr)

# load data: ####
tourismus_taeglich_1 <- httr::GET("https://data.bs.ch/api/explore/v2.1/catalog/datasets/100413/exports/csv?lang=de&timezone=Europe%2FBerlin&use_labels=true&delimiter=%3B") %>%
  content(., "text") %>%
  fread(sep=";")

tourismus_taeglich_2 <- httr::GET("https://data.bs.ch/api/explore/v2.1/catalog/datasets/100414/exports/csv?lang=de&timezone=Europe%2FBerlin&use_labels=true&delimiter=%3B") %>%
  content(., "text") %>%
  fread(sep=";")


# CSV herausschreiben:
write.csv(tourismus_taeglich_1, file = "data/tourismus_taeglich_1.csv", row.names = FALSE)
write.csv(tourismus_taeglich_2, file = "data/tourismus_taeglich_2.csv", row.names = FALSE)
