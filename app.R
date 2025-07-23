# ### Tourismus #######################################################################################
# ####################### Dashboard ###################################################################
# #####################################################################################################
  
# load packages:
library(dplyr)
library(DT)
library(lubridate)
library(tidyr)
library(janitor)
library(shiny)
library(highcharter)
library(bslib)
library(bsicons)
library(shinyWidgets)
library(shinycssloaders)

conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("month", "lubridate")

source("app_functions.R")

tourismus_taeglich_1 <- readRDS("data/tourismus_taeglich_1.rds") %>% 
  mutate(Datum = as.Date(Datum, format = "%Y-%m-%d"))

tourismus_taeglich_2 <- readRDS("data/tourismus_taeglich_2.rds") %>% 
  mutate(Datum = as.Date(Datum, format = "%Y-%m-%d"))

# Define Variables
colors_plots <- rev(c("#245333", "#2a9749", "#b8d6be"))
colors_valueboxes <- c("#2a9749", "#777777")
quelle <- "Tourismusstatistik des Kantons Basel-Stadt"

JJJJ <- max(tourismus_taeglich_1$Datum_Jahr)
JJJJ_alt <- JJJJ - 1
MM <- max(tourismus_taeglich_1$Monat[tourismus_taeglich_1$Datum_Jahr == JJJJ])
DD <- max(tourismus_taeglich_1$Datum_Tag[tourismus_taeglich_1$Datum_Jahr == JJJJ & tourismus_taeglich_1$Monat == MM])
monate_deutsch <- c("Januar", "Februar", "März", "April", "Mai", "Juni", "Juli", "August", "September", "Oktober", "November", "Dezember")

# UI:
ui <- page_navbar(
  
  theme = bs_theme(
    version = 5,
    primary = "#1e6d8b",
    secondary = "white"
  ),
  
  bg = "#ddecde",
  underline = FALSE,
  header = tags$head(
                     tags$link(rel = "stylesheet", type = "text/css", href = "app.css"),
                     tags$link(href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;700&display=swap", rel = "stylesheet")
  ),
  # 
  # footer = tags$footer(
  #   style = "font-size: 11px; text-align: center; width; 100%; position: relative; bottom: 0;",
  #   paste0(
  #     "Quelle: ",
  #     quelle,
  #     " | Dashboard-Version: V1.0 | Daten: ",
  #     min(tourismus_taeglich_1$Datum),
  #     " bis ",
  #     max(tourismus_taeglich_1$Datum)
  #   )
  # ),
  
  nav_panel(
    "Übersicht",
    h1("Tourismuszahlen Basel-Stadt"),
    h3(
      paste0(
        if (MM == 1) {
          paste0(monate_deutsch[1], " ", JJJJ)
        } else {
          paste0(monate_deutsch[1], " bis ", monate_deutsch[MM], " ", JJJJ)
        },
        " gegenüber dem Vorjahr"
      )
    ),
    page_fluid(
    layout_columns(
      uiOutput("Logiernaechte"),
      uiOutput("Ankuenfte"),
      uiOutput("Aufenthaltsdauer")
    ),
    layout_columns(
      uiOutput("verfuegbareZimmer"),
      uiOutput("belegteZimmer"),
      uiOutput("Zimmerauslastung")
    )
    ),
    layout_columns(
      page_fluid(
        h4("Logiernächte nach Herkunftsland - Top 10"),
        highchartOutput("barPlot_herkunft"))
    )
  ),
  
  nav_panel(
    "Monat",
    h1("Tourismuszahlen Basel-Stadt"),
    uiOutput("dynamic_subtitle1"),
    page_fluid(
      div(
        class = "full-width-div",
        div(
          class = "full-width-inner",
      layout_columns(
        selectInput(
          "jahr_monat",
          HTML(paste0(
            bs_icon("calendar4"), " Auswertungsjahr"
          )),
          choices = unique(tourismus_taeglich_1$Datum_Jahr)[-length(unique(tourismus_taeglich_1$Datum_Jahr))],
          selected = max(unique(tourismus_taeglich_1$Datum_Jahr))
        ),
        uiOutput("monat_ui"),
        selectInput(
          "hotelkategorie_monat",
          HTML(paste0(bs_icon("house"), " Hotelkategorie")),
          choices = c("Total", setdiff(unique(tourismus_taeglich_1$Hotelkategorie), "Total")),
          selected = "Total"
        ),
        uiOutput("herkunft_ui_monat"))
      )
      ),
      navset_underline(id = "monattabs",
                       nav_panel(
                         "Aufenthalt", 
                                     layout_columns(
                                       uiOutput("Logiernaechte_Monat"),
                                       uiOutput("Ankuenfte_Monat"),
                                       uiOutput("Aufenthaltsdauer_Monat")
                                     ),
                                   
                                       page_fluid(
                                         h4("Tourismuszahlen nach Monat"),
                                         radioButtons(
                                           inline = TRUE,
                                           inputId = "kat_aufenthalt_monat",
                                           label = NULL,
                                           choices = c("Logiernächte", "Ankünfte", "Aufenthaltsdauer (in Tagen)"),
                                           selected = "Logiernächte"
                                         ),
                                           uiOutput("dynamic_month_plot1")
                                       ),
                         layout_columns(
                                         DTOutput("dataTable1_Monat",  height = 500) %>% withSpinner(color="#2a9749")
                         )
                                     
                       ),
                       nav_panel(
                         "Zimmer",
                                     layout_columns(
                                       uiOutput("verfuegbareZimmer_Monat"),
                                       uiOutput("belegteZimmer_Monat"),
                                       uiOutput("Zimmerauslastung_Monat")
                                     ),
                           
                                       page_fluid(
                                         h4("Tourismuszahlen nach Monat"),
                                         radioButtons(
                                           inline = TRUE,
                                           inputId = "kat_zimmer_monat",
                                           label = NULL,
                                           choices = c("Verfügbare Zimmer", "Belegte Zimmer", "Zimmerauslastung"),
                                           selected = "Verfügbare Zimmer"
                                         ),
                                           uiOutput("dynamic_month_plot2")
                                       ),
                         layout_columns(
                                         DTOutput("dataTable2_Monat", height = 500) %>% withSpinner(color="#2a9749")
                         )
                                     
                       )
      )
      
    )
  ),
  
  nav_panel(
    "Tag",
    h1("Tourismuszahlen Basel-Stadt"),
    uiOutput("dynamic_subtitle2"),
    
    page_fluid(
      div(
        class = "full-width-div",
        div(
          class = "full-width-inner",
          layout_columns(
            dateInput(
              "startDate_tag",
              HTML(paste0(bs_icon("calendar3-week"), " Startdatum")),
              value = as.Date(paste0(JJJJ, "-", MM, "-01")),
              min = "2024-01-01",
              max = as.Date(paste0(JJJJ, "-", MM, "-", DD)),
              datesdisabled = "2024-02-29",
              autoclose = TRUE,
              language = "de"
            ),
            dateInput(
              "endDate_tag",
              HTML(paste0(bs_icon("calendar3-week"), " Enddatum")),
              value = as.Date(paste0(JJJJ, "-", MM, "-", DD)),
              min = "2024-01-01",
              max = as.Date(paste0(JJJJ, "-", MM, "-", DD)),
              datesdisabled = "2024-02-29",
              autoclose = TRUE,
              language = "de"
            ),
            selectInput(
              "hotelkategorie_tag",
              HTML(paste0(bs_icon("house"), " Hotelkategorie")),
              choices = c("Total", setdiff(unique(tourismus_taeglich_1$Hotelkategorie), "Total")),
              selected = "Total"
            ),
            uiOutput("herkunft_ui_tag")
          )
        )
      ),
      
      navset_underline(
        id = "tagtabs",
        
        nav_panel(
          "Aufenthalt",
          layout_columns(
            uiOutput("Logiernaechte_Tag"),
            uiOutput("Ankuenfte_Tag"),
            uiOutput("Aufenthaltsdauer_Tag")
          ),
          
          page_fluid(
            h4("Tourismuszahlen nach Tag"),
            radioButtons(
              inline = TRUE,
              inputId = "kat_aufenthalt_tag",
              label = NULL,
              choices = c("Logiernächte", "Ankünfte"),
              selected = "Logiernächte"
            ),
            uiOutput("dynamic_day_plot1")
          ),
          layout_columns(
            div(
              p("Die abgebildeten Events wurden frei gewählt und stehen nicht zwangsläufig mit den Tourismus-Zahlen in Verbindung."),
              style = "margin-top: -20px; font-size: 0.9em;"
            )
          ),

          layout_columns(
            DTOutput("dataTable1_Tag", height = 500) %>% withSpinner(color = "#2a9749")
          )
        ),
        
        nav_panel(
          "Zimmer",
          layout_columns(
            uiOutput("verfuegbareZimmer_Tag"),
            uiOutput("belegteZimmer_Tag"),
            uiOutput("Zimmerauslastung_Tag")
          ),
          
          page_fluid(
            h4("Tourismuszahlen nach Tag"),
            radioButtons(
              inline = TRUE,
              inputId = "kat_zimmer_tag",
              label = NULL,
              choices = c("Verfügbare Zimmer", "Belegte Zimmer", "Zimmerauslastung"),
              selected = "Verfügbare Zimmer"
            ),
            uiOutput("dynamic_day_plot2")
          ),
          layout_columns(
            div(
              p("Die abgebildeten Events wurden frei gewählt und stehen nicht zwangsläufig mit den Tourismus-Zahlen in Verbindung."),
              style = "margin-top: -20px; font-size: 0.9em;"
            )
          ),
          
          layout_columns(
            DTOutput("dataTable2_Tag", height = 500) %>% withSpinner(color = "#2a9749")
          )
        )
      )
    )
  ),
  
  nav_panel(
    "Info",
    h1("Informationen"),
    h3("Hier finden Sie die wichtigsten Definitionen zur Tourismusstatistik und eine Kontaktangabe für Fragen."),
    div(
      class = "full-width-div",
      div(
        class = "full-width-inner",
        h2("Definitionen"),
        h5("Hier werden die wichtigsten Begriffe erläutert, welche im Dashboard verwendet werden."),
        uiOutput("info_text")
      )
    ),
    page_fluid(
      h3("Kontakt"),
      img(src = "fh.jpg"),
      p("Fabienne Hofer", style = "font-weight: bold;"),
      a("+41 61 267 87 47", class = "kontakt"),
      a("fabienne.hofer@bs.ch", href = "mailto:fabienne.hofer@bs.ch", class = "kontakt")
      
    )
  )
)

# server:
server <- function(input, output, session) {

   output$monat_ui <- renderUI({
    req(input$jahr_monat)
    selected_year <- input$jahr_monat
    max_month <- max(tourismus_taeglich_1$Monat[tourismus_taeglich_1$Datum_Jahr == input$jahr_monat])
    min_date <- paste0(selected_year, "-1")
    max_date <- paste0(selected_year, "-", max_month)

    airMonthpickerInput(
      inputId = "monat",
      minDate = min_date,
      maxDate = max_date,
      value = max_date,
      view = "months",
      language = "de",
      addon = "none",
      dateFormat = "MMMM",
      range = TRUE,
      label = tooltip(
        trigger = list(
          HTML(paste0(bs_icon("calendar3"), " Monat(e)")),
          bs_icon("info-circle")
        ),
        'Wählen Sie einen oder mehrere Monate (Zeitspanne) aus.'
      )
    )
  })
   

  # Value Box Monat Aufenthalt:
  reactive_value_text1 <- reactive({
    req(input$monat, input$jahr_monat)
    selected_year <- as.numeric(input$jahr_monat)
    selected_month_range <- isolate(input$monat)
    
    if (is.null(selected_month_range)) {
      return(paste0("Jahr ", selected_year))
    }
    
    if (length(selected_month_range) == 1) {

      month <- format(as.Date(selected_month_range[1]), "%B")
      return(paste0(month, " ", selected_year))
    } else {
 
      month_start <- format(as.Date(selected_month_range[1]), "%B")
      month_end <- format(as.Date(selected_month_range[2]), "%B")
      return(paste0(month_start, " bis ", month_end, " ", selected_year))
    }
  })
  
  output$dynamic_subtitle1 <- renderUI({
    h3(paste0(reactive_value_text1(), " gegenüber dem Vorjahr"))
  })
  
  reactive_value_text2 <- reactive({
    req(input$startDate_tag, input$endDate_tag, input$window_width)
    
    date_format <-  "%e. %B %Y"
    
    start_date <- format(input$startDate_tag, date_format)
    end_date <- format(input$endDate_tag, date_format)
    
    # Überprüfen, ob Start- und Enddatum identisch sind
    if (input$startDate_tag == input$endDate_tag) {
      return(start_date)
    } else {
      return(paste0(start_date, " bis ", end_date))
    }
  })
  
  output$dynamic_subtitle2 <- renderUI({
    h3(paste0(reactive_value_text2(), " gegenüber dem Vorjahr"))
  })
  
  # Neuster Monat alle Hotels - Tab Übersicht Value Boxen und Plot
  newest_month_all_hotels <- reactive({
    tourismus_taeglich_1$Datum <- as.Date(tourismus_taeglich_1$Datum)
    newest_month <- max(tourismus_taeglich_1$Datum)
    
    tourismus_taeglich_1 %>%
      filter(
        format(Datum, "%Y-%m") >= format(as.Date(paste0(JJJJ, "-01-01")), "%Y-%m"),
        format(Datum, "%Y-%m") <= format(newest_month, "%Y-%m"),
        Hotelkategorie == "Total"
      )
  })
  
  # Entsprechendes Vorjahr alle Hotels - Tab Übersicht Value Boxen und Plot
  newest_month_all_hotels_past <- reactive({
    tourismus_taeglich_1$Datum <- as.Date(tourismus_taeglich_1$Datum)
    newest_month <- max(tourismus_taeglich_1$Datum)
    previous_year_month <- newest_month - lubridate::years(1)
    
    tourismus_taeglich_1 %>%
      filter(
        format(Datum, "%Y-%m") >= format(as.Date(paste0(JJJJ_alt, "-01-01")), "%Y-%m"),
        format(Datum, "%Y-%m") <= format(previous_year_month, "%Y-%m"),
        Hotelkategorie == "Total"
      )
  })
  
  # Value Box Monat Aufenthalt:
  val_box_monat_aufenthalt <- reactive({
    req(input$monat, input$hotelkategorie_monat, input$herkunft_monat)
    
    start_month <- as.Date(paste(input$monat[1], "-01", sep = ""))
    end_month <- as.Date(paste(input$monat[length(input$monat)], "-01", sep = "")) + months(1) - days(1)
    
    tourismus_taeglich_2 %>%
      filter(Datum >= start_month & Datum <= end_month,
             Hotelkategorie == input$hotelkategorie_monat,
             Herkunftsland == input$herkunft_monat)
  })
  
  # Value Box Monat Aufenthalt Past:
  val_box_monat_aufenthalt_past <- reactive({
    req(input$monat, input$hotelkategorie_monat, input$herkunft_monat)
    
    start_month <- as.Date(paste(input$monat[1], "-01", sep = ""))
    end_month <- as.Date(paste(input$monat[length(input$monat)], "-01", sep = "")) + months(1) - days(1)
    
    start_month_prev_year <- start_month - years(1)
    end_month_prev_year <- end_month - years(1)
    
    if (lubridate::month(end_month) == 2 & lubridate::day(end_month) == 29) {
      end_month_prev_year <- as.Date(paste0(lubridate::year(end_month) - 1, "-02-28"))
    }
    
    tourismus_taeglich_2 %>%
      filter(format(Datum, "%Y-%m") >= format(start_month_prev_year, "%Y-%m"),
             format(Datum, "%Y-%m") <= format(end_month_prev_year, "%Y-%m"),
             Hotelkategorie == input$hotelkategorie_monat,
             Herkunftsland == input$herkunft_monat)
  })
  
  # Value Box Monat Zimmer:
  val_box_monat_zimmer <- reactive({
    req(input$monat, input$hotelkategorie_monat)
    
    start_month <- as.Date(paste(input$monat[1], "-01", sep = ""))
    end_month <- as.Date(paste(input$monat[length(input$monat)], "-01", sep = "")) + months(1) - days(1)
    
    tourismus_taeglich_1 %>%
      filter(Datum >= start_month & Datum <= end_month,
             Hotelkategorie == input$hotelkategorie_monat)
  })
  
  # Value Box Monat Zimmer Past:
  val_box_monat_zimmer_past <- reactive({
    req(input$monat, input$hotelkategorie_monat)
    
    start_month <- as.Date(paste(input$monat[1], "-01", sep = ""))
    end_month <- as.Date(paste(input$monat[length(input$monat)], "-01", sep = "")) + months(1) - days(1)
    
    start_month_prev_year <- start_month - years(1)
    end_month_prev_year <- end_month - years(1)
    
    if (lubridate::month(end_month) == 2 & lubridate::day(end_month) == 29) {
      end_month_prev_year <- as.Date(paste0(lubridate::year(end_month) - 1, "-02-28"))
    }
    
    tourismus_taeglich_1 %>%
      filter(format(Datum, "%Y-%m") >= format(start_month_prev_year, "%Y-%m"),
             format(Datum, "%Y-%m") <= format(end_month_prev_year, "%Y-%m"),
             Hotelkategorie == input$hotelkategorie_monat)
  })
  
  # Value Box Tag Zimmer:
  val_box_tag_zimmer <- reactive({
    req(input$startDate_tag, input$endDate_tag, input$hotelkategorie_tag)
    
    start_date <- as.Date(input$startDate_tag)
    end_date <- as.Date(input$endDate_tag)
    
    tourismus_taeglich_1 %>%
      filter(Datum >= start_date & Datum <= end_date,
             Hotelkategorie == input$hotelkategorie_tag)
  })
  
  # Value Box Tag Zimmer Past:
  val_box_tag_zimmer_past <- reactive({
    req(input$startDate_tag, input$endDate_tag, input$hotelkategorie_tag)
    
    # Convert the input dates to the first of the month
    start_month <- as.Date(input$startDate_tag)
    end_month <- as.Date(input$endDate_tag)
    
    # Compute corresponding previous year period
    start_month_prev_year <- start_month - years(1)
    end_month_prev_year <- end_month - years(1)
    
    # Handle leap year case for February 29 → February 28
    if (lubridate::month(end_month) == 2 & lubridate::day(end_month) == 29) {
      end_month_prev_year <- as.Date(paste0(lubridate::year(end_month) - 1, "-02-28"))
    }
    
    # Apply filter to match the previous year's period
    tourismus_taeglich_1 %>%
      filter(
        Datum >= start_month_prev_year,
        Datum <= end_month_prev_year,
        Hotelkategorie == input$hotelkategorie_tag
      )
  })
  
  # Value Box Tag Aufenthalt:
  val_box_tag_aufenthalt <- reactive({
    req(input$startDate_tag, input$endDate_tag, input$herkunft_tag)
    
    start_date <- as.Date(input$startDate_tag)
    end_date <- as.Date(input$endDate_tag)
    
    tourismus_taeglich_2 %>%
      filter(Datum >= start_date & Datum <= end_date,
             Hotelkategorie == input$hotelkategorie_tag,
             Herkunftsland == input$herkunft_tag)
  })
  
  # Value Box Tag Aufenthalt Past:
  val_box_tag_aufenthalt_past <- reactive({
    req(input$startDate_tag, input$endDate_tag, input$hotelkategorie_tag, input$herkunft_tag)
    
    start_month <- as.Date(input$startDate_tag)
    end_month <- as.Date(input$endDate_tag)
    
    start_month_prev_year <- start_month - years(1)
    end_month_prev_year <- end_month - years(1)
    
    if (lubridate::month(end_month) == 2 & lubridate::day(end_month) == 29) {
      end_month_prev_year <- as.Date(paste0(lubridate::year(end_month) - 1, "-02-28"))
    }
    
    tourismus_taeglich_2 %>%
      filter(Datum >= start_month_prev_year,
             Datum <= end_month_prev_year,
             Hotelkategorie == input$hotelkategorie_tag,
             Herkunftsland == input$herkunft_tag)
  })
  
  
  # data_monat_zimmer
  data_monat_zimmer <- reactive({
    req(input$monat)
    
    # Überprüfen, ob ein oder mehrere Monate ausgewählt wurden
    if (length(input$monat) == 1) {
      # Nur ein Monat ausgewählt
      start_month <- as.Date(paste(input$monat[1], "-01", sep = ""))
      end_month <- start_month + months(1) - days(1)
    } else {
      # Zeitraum mit Start- und Endmonat
      start_month <- as.Date(paste(input$monat[1], "-01", sep = ""))
      end_month <- as.Date(paste(input$monat[2], "-01", sep = "")) + months(1) - days(1)
    }
    
    # Verschiebe den Start- und Endmonat um ein Jahr zurück
    start_month_prev_year <- start_month - years(1)
    if (lubridate::month(end_month) == 2 & lubridate::day(end_month) == 29) {
      end_month_prev_year <- as.Date(paste0(lubridate::year(end_month) - 1, "-02-28"))
    } else {
      end_month_prev_year <- end_month - years(1)
    }
    
    # Datensätze für den ausgewählten Zeitraum und den Vergleichszeitraum im Vorjahr erstellen
    bind_1 <- bind_rows(
      tourismus_taeglich_1 %>%
        filter(
          Datum >= start_month & Datum <= end_month,
          Hotelkategorie == input$hotelkategorie_monat
        ),
      tourismus_taeglich_1 %>%
        filter(
          Datum >= start_month_prev_year & Datum <= end_month_prev_year,
          Hotelkategorie == input$hotelkategorie_monat
        )
    )
    
    # Gruppieren, Zusammenfassen und weitere Berechnungen
    bind_1 %>%
      group_by(Datum_Jahr, Datum_Monat, Monat, Hotelkategorie) %>%
      summarise(
        `Anzahl Ankünfte` = sum(`Anzahl Ankünfte`, na.rm = TRUE),
        `Anzahl Logiernächte` = sum(`Anzahl Logiernächte`, na.rm = TRUE),
        `Anzahl verfügbare Zimmer` = mean(`Anzahl verfügbare Zimmer`, na.rm = TRUE),
        `Anzahl belegte Zimmer` = mean(`Anzahl belegte Zimmer`, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(Zimmerauslastung = (`Anzahl belegte Zimmer` / `Anzahl verfügbare Zimmer`) * 100) %>%
      arrange(desc(Datum_Jahr), Monat) %>%
      mutate(Datum_Monat = factor(Datum_Monat, levels = unique(Datum_Monat)))
  })
  
  # data_monat_aufenthalt
  data_monat_aufenthalt <- reactive({
    req(input$monat)
    
    # Überprüfen, ob ein oder mehrere Monate ausgewählt wurden
    if (length(input$monat) == 1) {
      # Nur ein Monat ausgewählt
      start_month <- as.Date(paste(input$monat[1], "-01", sep = ""))
      end_month <- start_month + months(1) - days(1)
    } else {
      # Zeitraum mit Start- und Endmonat
      start_month <- as.Date(paste(input$monat[1], "-01", sep = ""))
      end_month <- as.Date(paste(input$monat[2], "-01", sep = "")) + months(1) - days(1)
    }
    
    # Verschiebe den Start- und Endmonat um ein Jahr zurück
    start_month_prev_year <- start_month - years(1)
    if (lubridate::month(end_month) == 2 & lubridate::day(end_month) == 29) {
      end_month_prev_year <- as.Date(paste0(lubridate::year(end_month) - 1, "-02-28"))
    } else {
      end_month_prev_year <- end_month - years(1)
    }
    
    # Filter anwenden
    bind_1 <- bind_rows(
      tourismus_taeglich_2 %>%
        filter(
          Datum >= start_month & Datum <= end_month,
          Hotelkategorie == input$hotelkategorie_monat,
          Herkunftsland == input$herkunft_monat
        ),
      tourismus_taeglich_2 %>%
        filter(
          Datum >= start_month_prev_year & Datum <= end_month_prev_year,
          Hotelkategorie == input$hotelkategorie_monat,
          Herkunftsland == input$herkunft_monat
        )
    )
    
    # Gruppieren, Zusammenfassen und weitere Berechnungen
    bind_1 %>%
      group_by(Datum_Jahr, Datum_Monat, Monat, Hotelkategorie, Herkunftsland) %>%
      summarise(
        `Anzahl Ankünfte` = sum(`Anzahl Ankünfte`, na.rm = TRUE),
        `Anzahl Logiernächte` = sum(`Anzahl Logiernächte`, na.rm = TRUE),
        Aufenthaltsdauer = sum(`Anzahl Logiernächte` / `Anzahl Ankünfte`, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(Datum_Jahr), Monat) %>%
      mutate(Datum_Monat = factor(Datum_Monat, levels = unique(Datum_Monat)))
  })
  
  # data_tag_aufenthalt
  data_tag_aufenthalt <- reactive({
    req(input$startDate_tag, input$endDate_tag, input$herkunft_tag, input$hotelkategorie_tag)
    
    start_date <- as.Date(input$startDate_tag)
    end_date <- as.Date(input$endDate_tag)
    
    start_date_past <- as.Date(input$startDate_tag) - lubridate::years(1)
    end_date_prev <- as.Date(input$endDate_tag)
    
    if (lubridate::month(end_date_prev) == 2 & lubridate::day(end_date_prev) == 29) {
      end_date_past <- as.Date(paste0(lubridate::year(end_date_prev) - 1, "-02-28"))
    } else {
      end_date_past <- end_date_prev - lubridate::years(1)
    }
    
    # Filter anwenden
    bind_1 <- bind_rows(
      tourismus_taeglich_2 %>%
        filter(
          Datum >= start_date & Datum <= end_date,
          Hotelkategorie == input$hotelkategorie_tag,
          Herkunftsland == input$herkunft_tag
        ),
      tourismus_taeglich_2 %>%
        filter(
          Datum >= start_date_past & Datum <= end_date_past,
          Hotelkategorie == input$hotelkategorie_tag,
          Herkunftsland == input$herkunft_tag
        )
    )
    
    # Gruppieren, Zusammenfassen und weitere Berechnungen
    bind_1 %>%
      group_by(Datum, Datum_Jahr, Datum_Monat, Datum_Monat_Tag, Hotelkategorie, Herkunftsland, Event) %>%
      summarise(
        `Anzahl Ankünfte` = sum(`Anzahl Ankünfte`, na.rm = TRUE),
        `Anzahl Logiernächte` = sum(`Anzahl Logiernächte`, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(Datum_Jahr), Datum_Monat_Tag) %>%
      mutate(Datum_Monat_Tag = factor(Datum_Monat_Tag, levels = unique(Datum_Monat_Tag))) %>%
      complete(Datum_Monat_Tag, Datum_Jahr, fill = list(`Anzahl Logiernächte` = NA))
  })
  
  # data_tag_zimmer
  data_tag_zimmer <- reactive({
    req(input$startDate_tag, input$endDate_tag, input$hotelkategorie_tag)
    
    start_date <- as.Date(input$startDate_tag)
    end_date <- as.Date(input$endDate_tag)
    
    start_date_past <- as.Date(input$startDate_tag) - lubridate::years(1)
    end_date_prev <- as.Date(input$endDate_tag)
    
    if (lubridate::month(end_date_prev) == 2 & lubridate::day(end_date_prev) == 29) {
      end_date_past <- as.Date(paste0(lubridate::year(end_date_prev) - 1, "-02-28"))
    } else {
      end_date_past <- end_date_prev - lubridate::years(1)
    }
    
    # Filter anwenden (kein Filter auf Herkunftsland, wenn "Total" ausgewählt)
    bind_1 <- bind_rows(
      tourismus_taeglich_1 %>%
        filter(
          Datum >= start_date & Datum <= end_date,
          Hotelkategorie == input$hotelkategorie_tag
        ),
      tourismus_taeglich_1 %>%
        filter(
          Datum >= start_date_past & Datum <= end_date_past,
          Hotelkategorie == input$hotelkategorie_tag
        )
    )
    
    # Gruppieren, Zusammenfassen und weitere Berechnungen
    bind_1 %>%
      group_by(Datum, Datum_Jahr, Datum_Monat, Datum_Monat_Tag, Hotelkategorie, Event) %>%
      summarise(
        `Anzahl Ankünfte` = sum(`Anzahl Ankünfte`, na.rm = TRUE),
        `Anzahl Logiernächte` = sum(`Anzahl Logiernächte`, na.rm = TRUE),
        `Anzahl verfügbare Zimmer` = sum(`Anzahl verfügbare Zimmer`, na.rm = TRUE),
        `Anzahl belegte Zimmer` = sum(`Anzahl belegte Zimmer`, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(Zimmerauslastung = (`Anzahl belegte Zimmer` / `Anzahl verfügbare Zimmer`) * 100) %>%
      arrange(desc(Datum_Jahr), Datum_Monat_Tag) %>%
      mutate(Datum_Monat_Tag = factor(Datum_Monat_Tag, levels = unique(Datum_Monat_Tag))) %>%
      complete(Datum_Monat_Tag, Datum_Jahr, fill = list(`Zimmerauslastung` = NA))
  })
  
  
  # Data Monat Aufenthalt Table:
  monat_aufenthalt_table <- reactive({
    data_monat_aufenthalt() %>%
      arrange(desc(Datum_Jahr)) %>%
      select(
        Datum_Jahr,
        Datum_Monat,
        Hotelkategorie,
        Herkunft = Herkunftsland,
        `Anzahl Ankünfte`,
        `Anzahl Logiernächte`,
        "Aufenthaltsdauer (in Tagen)" = Aufenthaltsdauer
      ) %>%  # Nur die gewünschten Spalten auswählen
      rename(Jahr = Datum_Jahr,
             Monat = Datum_Monat)
  })
  
  # Data Monat Zimmer Table:
  monat_zimmer_table <- reactive({
    data_monat_zimmer() %>%
      arrange(desc(Datum_Jahr)) %>%
      mutate(Zimmerauslastung = round(Zimmerauslastung, 1)) %>%
      select(
        Datum_Jahr,
        Datum_Monat,
        Hotelkategorie,
        `Anzahl verfügbare Zimmer`,
        `Anzahl belegte Zimmer`,
        Zimmerauslastung
      ) %>%  # Nur die gewünschten Spalten auswählen
      rename(
        Jahr = Datum_Jahr,
        Monat = Datum_Monat,
        'Zimmerauslastung (in %)' = Zimmerauslastung,
        'Verfügbare Zimmer' = 'Anzahl verfügbare Zimmer',
        'Belegte Zimmer' = 'Anzahl belegte Zimmer'
      )
  })
  
  # Data Tag Aufenthalt Table:
  tag_aufenthalt_table <- reactive({
    data_tag_aufenthalt() %>%
      arrange(desc(Datum_Jahr)) %>%
      select(
        Datum,
        Hotelkategorie,
        Herkunft = Herkunftsland,
        `Anzahl Ankünfte`,
        `Anzahl Logiernächte`,
        Event) 
  })
  
  # Data Tag Zimmer Table:
  tag_zimmer_table <- reactive({
    data_tag_zimmer() %>%
      arrange(desc(Datum_Jahr)) %>%
      mutate(
        Zimmerauslastung = round(Zimmerauslastung, 1)) %>%
      select(
        Datum,
        Hotelkategorie,
        `Anzahl verfügbare Zimmer`,
        `Anzahl belegte Zimmer`,
        Zimmerauslastung,
        Event
      ) %>%  # Nur die gewünschten Spalten auswählen
      rename(
        'Zimmerauslastung (in %)' = Zimmerauslastung,
        'Verfügbare Zimmer' = 'Anzahl verfügbare Zimmer',
        'Belegte Zimmer' = 'Anzahl belegte Zimmer'
      )
  })
  
  
  # Monatsdaten aggregieren und filtern
  filtered_data_nat_newest <- reactive({
    tourismus_taeglich_2$Datum <- as.Date(tourismus_taeglich_2$Datum)
    newest_month <- max(tourismus_taeglich_2$Datum)
    previous_year_month <- newest_month - lubridate::years(1)
    
    data_aggregated <- tourismus_taeglich_2 %>%
      filter(
        (format(Datum, "%Y-%m") >= format(as.Date(paste0(JJJJ, "-01-01")), "%Y-%m") &
        format(Datum, "%Y-%m") <= format(newest_month, "%Y-%m")) |
        (format(Datum, "%Y-%m") >= format(as.Date(paste0(JJJJ_alt, "-01-01")), "%Y-%m") &
        format(Datum, "%Y-%m") <= format(previous_year_month, "%Y-%m")),
        Hotelkategorie == "Total",
        Herkunftsland != "Total",
        Herkunftsland != "Ausland"
      ) %>%
      group_by(Datum_Jahr, Herkunftsland) %>%
      summarise(
        `Anzahl Ankünfte` = sum(`Anzahl Ankünfte`, na.rm = TRUE),
        `Anzahl Logiernächte` = sum(`Anzahl Logiernächte`, na.rm = TRUE),
        Aufenthaltsdauer = sum(`Anzahl Logiernächte`, na.rm = TRUE) / sum(`Anzahl Ankünfte`, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Top 10 Nationalitäten für das aktuellste Jahr ermitteln
    aktuelles_jahr <- max(data_aggregated$Datum_Jahr, na.rm = TRUE)
    top_10_nat <- data_aggregated %>%
      filter(Datum_Jahr == aktuelles_jahr) %>%
      group_by(Herkunftsland) %>%
      summarise(Total_Ankuenfte = sum(`Anzahl Ankünfte`, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(Total_Ankuenfte)) %>%
      slice_head(n = 10) %>%
      pull(Herkunftsland)
    
    # Daten für die Top 10 Nationalitäten und die letzten beiden Jahre filtern
    data_filtered <- data_aggregated %>%
      filter(Datum_Jahr %in% c(aktuelles_jahr - 1, aktuelles_jahr) & Herkunftsland %in% top_10_nat) %>%
      # mutate(Datum_Monat = as.factor(Datum_Monat)) %>% 
      complete(Herkunftsland, Datum_Jahr, fill = list(`Anzahl Logiernächte` = NA)) %>%
      arrange(desc(`Anzahl Logiernächte`), desc(Datum_Jahr))
    
    data_filtered
  })
  
  
  #Value-Box-Funktion
  create_value_box <- function(current_data, current_data_digits, previous_data, previous_data_digits, label, unit_value = NULL, unit_change = NULL) {
    current_value <- current_data
    previous_value <- previous_data
    value_change <- current_value - previous_value
    value_change_proz <- (value_change / previous_value) * 100
    
    custom_color <- ifelse(value_change > 0, colors_valueboxes[1], colors_valueboxes[2])
    
    arrow_icon <- if (value_change > 0) {
      tags$span(style = "color: #777777; font-size: 0.5em;",
        icon("arrow-trend-up")
      )
    } else {
      tags$span(style = "color: #777777; font-size: 0.5em;",
                icon("arrow-trend-down")
      )
    }
    

    
    value_box(
      value = tagList(
        tags$span(
          style = "display: block; line-height: 0.7em; margin-bottom: 0; padding: 0px 0px 0px 0px;",
          add_thousand_separator(current_value, current_data_digits), unit_value
        ),
        tags$span(
          style = "display: block; line-height: 0.7em; padding: 0px 0px 0px 0px;",
        tags$span(
          style = "font-size: 0.5em; color: #777777;",
          paste0(
            " (",
            ifelse(value_change > 0, "+", ""),
            add_thousand_separator(value_change, previous_data_digits), unit_change,
            " | ",
            ifelse(value_change > 0, "+", ""),
            add_thousand_separator(round(value_change_proz, 1), 1),
            "%)"
          )),
        arrow_icon
        )
      ),
      label,
      showcase = NULL,
      height = NULL
    )
    
  }
  
  # Value-Boxen:
  # Ankünfte (aktuellste Jahresperiode):
  output$Ankuenfte <- renderUI({
    create_value_box(
      current_data = sum(newest_month_all_hotels()$`Anzahl Ankünfte`),
      current_data_digits = 0,
      previous_data = sum(newest_month_all_hotels_past()$`Anzahl Ankünfte`),
      previous_data_digits = 0,
      label = tagList(p("Ankünfte", class = "value-box-headline"),
                                       p("aufsummiert", class = "value-box-subline")),
    )
  })
  
  # Logiernächte (aktuellste Jahresperiode):
  output$Logiernaechte <- renderUI({
    create_value_box(
      current_data = sum(newest_month_all_hotels()$`Anzahl Logiernächte`),
      current_data_digits = 0,
      previous_data = sum(newest_month_all_hotels_past()$`Anzahl Logiernächte`),
      previous_data_digits = 0,
      label = tagList(p("Logiernächte", class = "value-box-headline"),
                      p("aufsummiert", class = "value-box-subline")),
    )
  })
  
  # Aufenthaltsdauer (aktuellste Jahresperiode):
  output$Aufenthaltsdauer <- renderUI({
    create_value_box(
      current_data = mean(sum(newest_month_all_hotels()$`Anzahl Logiernächte`) / sum(newest_month_all_hotels()$`Anzahl Ankünfte`)),
      current_data_digits = 2,
      previous_data = mean(sum(newest_month_all_hotels_past()$`Anzahl Logiernächte`) / sum(newest_month_all_hotels_past()$`Anzahl Ankünfte`)),
      previous_data_digits = 2,
      label = tagList(p("Aufenthaltsdauer", class = "value-box-headline"),
                      p("durchschnittlich", class = "value-box-subline")),
      unit_value = " Tage",
    )
  })
  
  # Verfügbare Zimmer (aktuellste Jahresperiode):
  output$verfuegbareZimmer <- renderUI({
    create_value_box(
      current_data = mean(newest_month_all_hotels()$`Anzahl verfügbare Zimmer`),
      current_data_digits = 0,
      previous_data = mean(newest_month_all_hotels_past()$`Anzahl verfügbare Zimmer`),
      previous_data_digits = 0,
      label = tagList(p("Verfügbare Zimmer", class = "value-box-headline"),
                      p("durchschnittlich", class = "value-box-subline")),
    )
  })
  
  # Belegte Zimmer (aktuellste Jahresperiode):
  output$belegteZimmer <- renderUI({
    create_value_box(
      current_data = mean(newest_month_all_hotels()$`Anzahl belegte Zimmer`),
      current_data_digits = 0,
      previous_data = mean(newest_month_all_hotels_past()$`Anzahl belegte Zimmer`),
      previous_data_digits = 0,
      label = tagList(p("Belegte Zimmer", class = "value-box-headline"),
                      p("durchschnittlich", class = "value-box-subline")),
    )
  })
  
  # Zimmerauslastung (aktuellste Jahresperiode):
  output$Zimmerauslastung <- renderUI({
    create_value_box(
      current_data = mean(sum(newest_month_all_hotels()$`Anzahl belegte Zimmer`) / sum(newest_month_all_hotels()$`Anzahl verfügbare Zimmer`) * 100),
      current_data_digits = 1,
      previous_data = mean(sum(newest_month_all_hotels_past()$`Anzahl belegte Zimmer`) / sum(newest_month_all_hotels_past()$`Anzahl verfügbare Zimmer`) * 100),
      previous_data_digits = 1,
      label = tagList(p("Zimmerauslastung", class = "value-box-headline"),
                      p("durchschnittlich", class = "value-box-subline")),
      unit_value = "%",
      unit_change = "PP"
    )
  })
  
  # Value-Boxen Monat:
  # Ankünfte (Monat):
  output$Ankuenfte_Monat <- renderUI({
    create_value_box(
      current_data = sum(val_box_monat_aufenthalt()$`Anzahl Ankünfte`),
      current_data_digits = 0,
      previous_data = sum(val_box_monat_aufenthalt_past()$`Anzahl Ankünfte`),
      previous_data_digits = 0,
      label = tagList(p("Ankünfte", class = "value-box-headline"),                                        
                      p("aufsummiert", class = "value-box-subline"))
    )
  })
  
  # Logiernächte (Monat):
  output$Logiernaechte_Monat <- renderUI({
    create_value_box(
      current_data = sum(val_box_monat_aufenthalt()$`Anzahl Logiernächte`),
      current_data_digits = 0,
      previous_data = sum(val_box_monat_aufenthalt_past()$`Anzahl Logiernächte`),
      previous_data_digits = 0,
      label = tagList(p("Logiernächte", class = "value-box-headline"),                                        
                      p("aufsummiert", class = "value-box-subline"))
    )
  })
  
  output$Logiernaechte_Monat_Text <- renderUI({
    current_data = sum(val_box_monat_aufenthalt()$`Anzahl Logiernächte`)
    previous_data = sum(val_box_monat_aufenthalt_past()$`Anzahl Logiernächte`)
    diff <- current_data - previous_data
    mehr_weniger <- ifelse(diff > 0, "mehr", "weniger")
    # JJJJ <- max(tourismus_taeglich_1$Datum_Jahr)
    # JJJJ_alt <- JJJJ - 1
    
    HTML(
      paste0(
        "Im Jahr ", JJJJ, " wurden in den baselstädtischen Hotelbetrieben ", format(current_data, big.mark = "'"), " Logiernächte registriert. ",
        "Das sind ", format(abs(diff), big.mark = "'"), " ", mehr_weniger, " als im Jahr ", JJJJ_alt, "."
      )
    )
  })
  
  # Aufenthaltsdauer (Monat):
  output$Aufenthaltsdauer_Monat <- renderUI({
    create_value_box(
      current_data = mean(sum(val_box_monat_aufenthalt()$`Anzahl Logiernächte`) / sum(val_box_monat_aufenthalt()$`Anzahl Ankünfte`)),
      current_data_digits = 2,
      previous_data = mean(sum(val_box_monat_aufenthalt_past()$`Anzahl Logiernächte`) / sum(val_box_monat_aufenthalt_past()$`Anzahl Ankünfte`)),
      previous_data_digits = 2,
      label = tagList(p("Aufenthaltsdauer", class = "value-box-headline"),                       
                      p("durchschnittlich", class = "value-box-subline")),
      unit_value = " Tage"
    )
  })
  
  # Verfügbare Zimmer (Monat):
  output$verfuegbareZimmer_Monat <- renderUI({
    create_value_box(
      current_data = mean(val_box_monat_zimmer()$`Anzahl verfügbare Zimmer`),
      current_data_digits = 0,
      previous_data = mean(val_box_monat_zimmer_past()$`Anzahl verfügbare Zimmer`),
      previous_data_digits = 0,
      label = tagList(p("Verfügbare Zimmer", class = "value-box-headline"),                       
                      p("durchschnittlich", class = "value-box-subline")),
    )
  })
  
  # Belegte Zimmer (Monat):
  output$belegteZimmer_Monat <- renderUI({
    create_value_box(
      current_data = mean(val_box_monat_zimmer()$`Anzahl belegte Zimmer`),
      current_data_digits = 0,
      previous_data = mean(val_box_monat_zimmer_past()$`Anzahl belegte Zimmer`),
      previous_data_digits = 0,
      label = tagList(p("Belegte Zimmer", class = "value-box-headline"),                       
                      p("durchschnittlich", class = "value-box-subline")),
    )
  })
  
  # Zimmerauslastung (Monat):
  output$Zimmerauslastung_Monat <- renderUI({
    create_value_box(
      current_data = mean(sum(val_box_monat_zimmer()$`Anzahl belegte Zimmer`) / sum(val_box_monat_zimmer()$`Anzahl verfügbare Zimmer`) * 100),
      current_data_digits = 1,
      previous_data = mean(sum(val_box_monat_zimmer_past()$`Anzahl belegte Zimmer`) / sum(val_box_monat_zimmer_past()$`Anzahl verfügbare Zimmer`) * 100),
      previous_data_digits = 1,
      label = tagList(p("Zimmerauslastung", class = "value-box-headline"),                       
                      p("durchschnittlich", class = "value-box-subline")),
      unit_value = "%",
      unit_change = "PP"
    )
  })
  
  # Value-Boxen Tag:
  # Ankünfte (Tag):
  output$Ankuenfte_Tag <- renderUI({
    create_value_box(
      current_data = sum(val_box_tag_aufenthalt()$`Anzahl Ankünfte`),
      current_data_digits = 0,
      previous_data = sum(val_box_tag_aufenthalt_past()$`Anzahl Ankünfte`),
      previous_data_digits = 0,
      label = tagList(p("Ankünfte", class = "value-box-headline"),                                        
                      p("aufsummiert", class = "value-box-subline"))
    )
  })
  
  # Logiernächte (Tag):
  output$Logiernaechte_Tag <- renderUI({
    create_value_box(
      current_data = sum(val_box_tag_aufenthalt()$`Anzahl Logiernächte`),
      current_data_digits = 0,
      previous_data = sum(val_box_tag_aufenthalt_past()$`Anzahl Logiernächte`),
      previous_data_digits = 0,
      label = tagList(p("Logiernächte", class = "value-box-headline"),                                        
                      p("aufsummiert", class = "value-box-subline"))
    )
  })
  
  # Aufenthaltsdauer (Tag):
  output$Aufenthaltsdauer_Tag <- renderUI({
    create_value_box(
      current_data = mean(sum(val_box_tag_aufenthalt()$`Anzahl Logiernächte`) / sum(val_box_tag_aufenthalt()$`Anzahl Ankünfte`)),
      current_data_digits = 2,
      previous_data = mean(sum(val_box_tag_aufenthalt_past()$`Anzahl Logiernächte`) / sum(val_box_tag_aufenthalt_past()$`Anzahl Ankünfte`)),
      previous_data_digits = 2,
      label = tagList(p("Aufenthaltsdauer", class = "value-box-headline"),                       
                      p("durchschnittlich", class = "value-box-subline")),
      unit_value = " Tage"
    )
  })
  
  # Verfügbare Zimmer (Tag):
  output$verfuegbareZimmer_Tag <- renderUI({
    create_value_box(
      current_data = mean(val_box_tag_zimmer()$`Anzahl verfügbare Zimmer`),
      current_data_digits = 0,
      previous_data = mean(val_box_tag_zimmer_past()$`Anzahl verfügbare Zimmer`),
      previous_data_digits = 0,
      label = tagList(p("Verfügbare Zimmer", class = "value-box-headline"),                       
                      p("durchschnittlich", class = "value-box-subline")),
    )
    
  })
  
  # Belegte Zimmer (Tag):
  output$belegteZimmer_Tag <- renderUI({
    create_value_box(
      current_data = mean(val_box_tag_zimmer()$`Anzahl belegte Zimmer`),
      current_data_digits = 0,
      previous_data = mean(val_box_tag_zimmer_past()$`Anzahl belegte Zimmer`),
      previous_data_digits = 0,
      label = tagList(p("Belegte Zimmer", class = "value-box-headline"),                       
                      p("durchschnittlich", class = "value-box-subline")),
    )
  })
  
  # Zimmerauslastung (Tag):
  output$Zimmerauslastung_Tag <- renderUI({
    create_value_box(
      current_data = mean(sum(val_box_tag_zimmer()$`Anzahl belegte Zimmer`) / sum(val_box_tag_zimmer()$`Anzahl verfügbare Zimmer`) * 100),
      current_data_digits = 1,
      previous_data = mean(sum(val_box_tag_zimmer_past()$`Anzahl belegte Zimmer`) / sum(val_box_tag_zimmer_past()$`Anzahl verfügbare Zimmer`) * 100),
      previous_data_digits = 1,
      label = tagList(p("Zimmerauslastung", class = "value-box-headline"),                       
                      p("durchschnittlich", class = "value-box-subline")),
      unit_value = "%",
      unit_change = "PP"
    )
  })
  
  
  
  # Bar plot 1:
  output$barPlot_herkunft <- renderHighchart({
    hc <- highchart() %>%
      hc_chart(type = "column") %>% 
      hc_xAxis(
        categories = gsub("Vereinigte Staaten von Amerika", "USA", unique(filtered_data_nat_newest()$Herkunftsland)),
        tickInterval = 1,
        labels = list(
          style = list(fontSize = "10px")
        )
      ) %>%
      hc_legend(
        layout = 'horizontal',
        align = 'left',
        verticalAlign = 'top',
        
        enabled = TRUE
      ) %>%
      hc_tooltip(shared = T) %>%
      hc_plotOptions(column = list(
        dataLabels = list(enabled = FALSE)
      )) %>%
      hc_add_series(
        filtered_data_nat_newest(),
        type = "column",
        hcaes(x = Herkunftsland, y = `Anzahl Logiernächte`, group = Datum_Jahr)
      ) %>%
      hc_colors(colors_plots) %>%
      hc_credits(enabled = TRUE, text = paste0("Quelle: ", quelle)) %>%
      # hc_exporting(enabled = TRUE) %>%
      hc_add_theme(stata_theme)
    # waiter_hide()
    hc
  })
  
  
  # Line Plot 1 Monat:
  output$linePlot1_Monat <- renderHighchart({
    # Monatsnamen definieren
    month_mapping_long <- c(
      "1" = "Januar", "2" = "Februar", "3" = "März", "4" = "April", "5" = "Mai", "6" = "Juni", 
      "7" = "Juli", "8" = "August", "9" = "September", "10" = "Oktober", "11" = "November", "12" = "Dezember"
    )
    month_mapping_short <- c(
      "1" = "Jan", "2" = "Feb", "3" = "Mär", "4" = "Apr", "5" = "Mai", "6" = "Jun", 
      "7" = "Jul", "8" = "Aug", "9" = "Sep", "10" = "Okt", "11" = "Nov", "12" = "Dez"
    )
    
    data <- data_monat_aufenthalt() %>%
      mutate(
        Monat_Jahr = paste(month_mapping_long[as.character(Monat)], Datum_Jahr)
      ) %>% arrange(Datum_Jahr, Monat)
    
    # Einmalige Monate und "Monat Jahr"-Kombis extrahieren
    unique_months <- unique(data$Monat)
    unique_month_year <- unique(data$Monat_Jahr)
    is_single_month <- length(unique_months) == 1

    
    if(input$kat_aufenthalt_monat %in% c("Ankünfte", "Logiernächte")) {
      if (is_single_month) {
        hc <- highchart() %>%
          hc_chart(type = "column") %>%
          # hc_size(560) %>%
          hc_xAxis(categories = unique_month_year) %>%
          hc_yAxis(visible = FALSE) %>%
          hc_legend(
            layout = 'horizontal',
            align = 'left',
            verticalAlign = 'top',
            enabled = FALSE
          ) %>%
          hc_tooltip(shared = TRUE) %>%
          hc_plotOptions(
            column = list(
              dataLabels = list(
                enabled = TRUE,
                style = list(
                  color = "#333333",
                  fontSize = "12px",
                  fontWeight = "bold"
                )
              )
            ),
            column = list(pointPadding = 0)
          ) %>%
          hc_add_series(
            data = data,
            type = "column",
            hcaes(x = as.character(Monat_Jahr), y = .data[[paste("Anzahl", input$kat_aufenthalt_monat)]]),
            color = '#2a9749',
            name = input$kat_aufenthalt_monat
          ) %>%
          hc_colors(colors_plots) %>%
          hc_credits(enabled = TRUE,     
                     style = list(
                       fontSize = "11px"),
                     text = paste0("Quelle: ", quelle)) %>%
          # hc_exporting(enabled = TRUE) %>%
          hc_add_theme(stata_theme)
        
      } else {
        hc <- highchart() %>%
          hc_chart(type = "line") %>%
          # hc_size(836) %>%
          hc_xAxis(categories = month_mapping_short[unique_months]) %>%
          hc_legend(
            layout = 'horizontal',
            align = 'left',
            verticalAlign = 'top',
            enabled = TRUE
          ) %>%
          hc_tooltip(shared = TRUE) %>%
          hc_plotOptions(
            line = list(
              dataLabels = list(enabled = FALSE),
              marker = list(enabled = FALSE, symbol = "circle")
            ),
            column = list(pointPadding = 0)
          )
        
        # Serien nach Jahr hinzufügen
        for (jahr in sort(unique(data$Datum_Jahr))) {
          jahr_data <- data %>% filter(Datum_Jahr == jahr)
          hc <- hc %>%
            hc_add_series(
              data = jahr_data,
              type = "line",
              hcaes(x = Monat, y = .data[[paste("Anzahl", input$kat_aufenthalt_monat)]]),
              name = as.character(jahr)
            )
        }
      } 
      hc <- hc %>%
        hc_colors(colors_plots) %>%
        hc_credits(enabled = TRUE,     
                   style = list(
                     fontSize = "11px"),
                   text = paste0("Quelle: ", quelle)) %>%
        # hc_exporting(enabled = TRUE) %>%
        hc_add_theme(stata_theme)
    }
    
    else if(input$kat_aufenthalt_monat == "Aufenthaltsdauer (in Tagen)") {
      if (is_single_month) {
        hc <- highchart() %>%
          hc_chart(type = "column") %>%
          # hc_size(560) %>%
          hc_xAxis(categories = unique_month_year) %>%
          hc_yAxis(visible = FALSE) %>%
          hc_legend(
            layout = 'horizontal',
            align = 'left',
            verticalAlign = 'top',
            enabled = FALSE
          ) %>%
          hc_tooltip(
            shared = TRUE,
            formatter = JS(
              "function () {
      let tooltip = '<span style=\"font-size: 10px; color: #333333\">' + this.x + '</span><br>';
      this.points.forEach(function (point) {
        tooltip += '<span style=\"color:' + point.color + '\">\u25CF</span> '
                  + point.series.name + ': <b>'
                  + point.y.toFixed(2).toString().replace('.', ',') + '</b><br>';
      });
      return tooltip;
    }"
            )
          ) %>%
          hc_tooltip(shared = TRUE) %>%
          hc_plotOptions(
            column = list(
              dataLabels = list(
                enabled = TRUE,
                formatter = JS("function() {
        return Highcharts.numberFormat(this.y, 2, ',', '');
      }"),
                style = list(
                  color = "#333333",
                  fontSize = "12px",
                  fontWeight = "bold"
                )
              )
            ),
            column = list(pointPadding = 0)
          ) %>%
          hc_add_series(
            data = data,
            type = "column",
            hcaes(x = as.character(Monat_Jahr), y = Aufenthaltsdauer),
            color = '#2a9749',
            name = "Aufenthaltsdauer (in Tagen)"
          ) %>%
          hc_colors(colors_plots) %>%
          hc_credits(enabled = TRUE,     
                     style = list(
                       fontSize = "11px"),
                     text = paste0("Quelle: ", quelle)) %>%
          # hc_exporting(enabled = TRUE) %>%
          hc_add_theme(stata_theme)
        
      } else {
        hc <- highchart() %>%
          hc_chart(type = "line") %>%
          # hc_size(836) %>%
          hc_xAxis(categories = month_mapping_short[unique_months]) %>%
          hc_legend(
            layout = 'horizontal',
            align = 'left',
            verticalAlign = 'top',
            enabled = TRUE
          ) %>%
          hc_tooltip(
            shared = TRUE,
            formatter = JS(
              "function () {
      let tooltip = '<span style=\"font-size: 10px; color: #333333\">' + this.x + '</span><br>';
      this.points.forEach(function (point) {
        tooltip += '<span style=\"color:' + point.color + '\">\u25CF</span> '
                  + point.series.name + ': <b>'
                  + point.y.toFixed(2).toString().replace('.', ',') + '</b><br>';
      });
      return tooltip;
    }"
            )
          ) %>%
          hc_plotOptions(
            line = list(
              dataLabels = list(enabled = FALSE),
              marker = list(enabled = FALSE, symbol = "circle")
            ),
            column = list(pointPadding = 0)
          )
        
        # Serien nach Jahr hinzufügen
        for (jahr in sort(unique(data$Datum_Jahr))) {
          jahr_data <- data %>% filter(Datum_Jahr == jahr)
          hc <- hc %>%
            hc_add_series(
              data = jahr_data,
              type = "line",
              hcaes(x = Monat, y = Aufenthaltsdauer),
              name = as.character(jahr)
            )
        }
      } 
      hc <- hc %>%
        hc_colors(colors_plots) %>%
        hc_credits(enabled = TRUE,     
                   style = list(
                     fontSize = "11px"),
                   text = paste0("Quelle: ", quelle)) %>%
        # hc_exporting(enabled = TRUE) %>%
        hc_add_theme(stata_theme)
    }
    
    hc
  })
  
  output$dynamic_month_plot1 <- renderUI({
    if (length(input$monat) == 1) {
      layout_columns(
        col_widths=c(6,6),
      highchartOutput("linePlot1_Monat") %>% withSpinner(color="#2a9749")
      )
    } else {
      layout_columns(
      highchartOutput("linePlot1_Monat") %>% withSpinner(color="#2a9749")
      )
    }
  })
  
  # Line Plot 2 Monat:
  output$linePlot2_Monat <- renderHighchart({
    # Monatsnamen definieren
    month_mapping_long <- c(
      "1" = "Januar", "2" = "Februar", "3" = "März", "4" = "April", "5" = "Mai", "6" = "Juni", 
      "7" = "Juli", "8" = "August", "9" = "September", "10" = "Oktober", "11" = "November", "12" = "Dezember"
    )
    month_mapping_short <- c(
      "1" = "Jan", "2" = "Feb", "3" = "Mär", "4" = "Apr", "5" = "Mai", "6" = "Jun", 
      "7" = "Jul", "8" = "Aug", "9" = "Sep", "10" = "Okt", "11" = "Nov", "12" = "Dez"
    )
    
    data <- data_monat_zimmer() %>%
      mutate(
        Monat_Jahr = paste(month_mapping_long[as.character(Monat)], Datum_Jahr),
        `Anzahl verfügbare Zimmer` = round(`Anzahl verfügbare Zimmer`, 0),
        `Anzahl belegte Zimmer` = round(`Anzahl belegte Zimmer`, 0)
      ) %>% arrange(Datum_Jahr, Monat)
    
    # Einmalige Monate und "Monat Jahr"-Kombis extrahieren
    unique_months <- unique(data$Monat)
    unique_month_year <- unique(data$Monat_Jahr)
    is_single_month <- length(unique_months) == 1
    
    
    if(input$kat_zimmer_monat %in% c("Verfügbare Zimmer", "Belegte Zimmer")) {
      if (is_single_month) {
        hc <- highchart() %>%
          hc_chart(type = "column") %>%
          # hc_size(560) %>%
          hc_xAxis(categories = unique_month_year) %>%
          hc_yAxis(visible = FALSE) %>%
          hc_legend(
            layout = 'horizontal',
            align = 'left',
            verticalAlign = 'top',
            enabled = FALSE
          ) %>%
          hc_tooltip(
            shared = TRUE,
            formatter = JS(
              "function () {
    let tooltip = '<span style=\"font-size: 10px; color: #333333\">' + this.x + '</span><br>';
    this.points.forEach(function (point) {
      tooltip += '<span style=\"color:' + point.color + '\">\u25CF</span> '
                + point.series.name + ': <b>'
                + Highcharts.numberFormat(point.y, 0, ',', ' ') + '</b><br>';
    });
    return tooltip;
  }"
            )
          ) %>%
          hc_plotOptions(
            column = list(
              dataLabels = list(
                enabled = TRUE,
                format = "{point.y:,.0f}",
                style = list(
                  color = "#333333",
                  fontSize = "12px",
                  fontWeight = "bold"
                )
              )
            ),
            column = list(pointPadding = 0)
          ) %>%
          hc_add_series(
            data = data,
            type = "column",
            hcaes(
              x = as.character(Monat_Jahr), 
              y = .data[[paste0("Anzahl ", tolower(substr(input$kat_zimmer_monat, 1, 1)), substr(input$kat_zimmer_monat, 2, nchar(input$kat_zimmer_monat)))]]
            ),
            color = '#2a9749',
            name = input$kat_zimmer_monat
          ) %>%
          hc_colors(colors_plots) %>%
          hc_credits(enabled = TRUE,     
                     style = list(
                       fontSize = "11px"),
                     text = paste0("Quelle: ", quelle)) %>%
          # hc_exporting(enabled = TRUE) %>%
          hc_add_theme(stata_theme)
        
      } else {
        hc <- highchart() %>%
          hc_chart(type = "line") %>%
          # hc_size(836) %>%
          hc_xAxis(categories = month_mapping_short[unique_months]) %>%
          hc_legend(
            layout = 'horizontal',
            align = 'left',
            verticalAlign = 'top',
            enabled = TRUE
          ) %>%
          hc_tooltip(shared = TRUE) %>%
          hc_plotOptions(
            line = list(
              dataLabels = list(enabled = FALSE),
              marker = list(enabled = FALSE, symbol = "circle")
            ),
            column = list(pointPadding = 0)
          )
        
        # Serien nach Jahr hinzufügen
        for (jahr in sort(unique(data$Datum_Jahr))) {
          jahr_data <- data %>% filter(Datum_Jahr == jahr)
          hc <- hc %>%
            hc_add_series(
              data = jahr_data,
              type = "line",
              hcaes(
                x = Monat, 
                y = .data[[paste0("Anzahl ", tolower(substr(input$kat_zimmer_monat, 1, 1)), substr(input$kat_zimmer_monat, 2, nchar(input$kat_zimmer_monat)))]]
              ),
              name = as.character(jahr)
            )
        }
      } 
      hc <- hc %>%
        hc_colors(colors_plots) %>%
        hc_credits(enabled = TRUE,     
                   style = list(
                     fontSize = "11px"),
                   text = paste0("Quelle: ", quelle)) %>%
        # hc_exporting(enabled = TRUE) %>%
        hc_add_theme(stata_theme)
    }
    
    else if(input$kat_zimmer_monat == "Zimmerauslastung") {
      if (is_single_month) {
        hc <- highchart() %>%
          hc_chart(type = "column") %>%
          # hc_size(560) %>%
          hc_xAxis(categories = unique_month_year) %>%
          hc_yAxis(visible = FALSE) %>%
          hc_legend(
            layout = 'horizontal',
            align = 'left',
            verticalAlign = 'top',
            enabled = FALSE
          ) %>%
          hc_tooltip(
            shared = TRUE,
            formatter = JS(
              "function () {
      let tooltip = '<span style=\"font-size: 10px; color: #333333\">' + this.x + '</span><br>';
      this.points.forEach(function (point) {
        tooltip += '<span style=\"color:' + point.color + '\">\u25CF</span> '
                  + point.series.name + ': <b>'
                  + point.y.toFixed(1).toString().replace('.', ',') + '%' + '</b><br>';
      });
      return tooltip;
    }"
            )
          ) %>%
          hc_plotOptions(
            column = list(
              dataLabels = list(
                enabled = TRUE,
                formatter = JS(
                  "function () {
          return this.y.toFixed(1).toString().replace('.', ',') + '%';
        }"
                ),
                style = list(
                  color = "#333333",
                  fontSize = "12px",
                  fontWeight = "bold"
                )
              )
            ),
            column = list(pointPadding = 0)
          ) %>%
          hc_add_series(
            data = data,
            type = "column",
            hcaes(x = as.character(Monat_Jahr), y = Zimmerauslastung),
            color = '#2a9749',
            name = "Zimmerauslastung"
          ) %>%
          hc_colors(colors_plots) %>%
          hc_credits(enabled = TRUE,     
                     style = list(
                       fontSize = "11px"),
                     text = paste0("Quelle: ", quelle)) %>%
          # hc_exporting(enabled = TRUE) %>%
          hc_add_theme(stata_theme)
        
      } else {
        hc <- highchart() %>%
          hc_chart(type = "line") %>%
          # hc_size(836) %>%
          hc_xAxis(categories = month_mapping_short[unique_months]) %>%
          hc_yAxis(
            labels = list(format = "{value}%"),
            min = 0,
            max = 100
          ) %>%
          hc_legend(
            layout = 'horizontal',
            align = 'left',
            verticalAlign = 'top',
            enabled = TRUE
          ) %>%
          hc_tooltip(
            shared = TRUE,
            formatter = JS(
              "function () {
      let tooltip = '<span style=\"font-size: 10px; color: #333333\">' + this.x + '</span><br>';
      this.points.forEach(function (point) {
        tooltip += '<span style=\"color:' + point.color + '\">\u25CF</span> '
                  + point.series.name + ': <b>'
                  + point.y.toFixed(1).toString().replace('.', ',') + '%' + '</b><br>';
      });
      return tooltip;
    }"
            )
          ) %>%
          hc_plotOptions(
            line = list(
              dataLabels = list(enabled = FALSE),
              marker = list(enabled = FALSE, symbol = "circle")
            ),
            column = list(pointPadding = 0)
          )
        
        # Serien nach Jahr hinzufügen
        for (jahr in sort(unique(data$Datum_Jahr))) {
          jahr_data <- data %>% filter(Datum_Jahr == jahr)
          hc <- hc %>%
            hc_add_series(
              data = jahr_data,
              type = "line",
              hcaes(x = Monat, y = Zimmerauslastung),
              name = as.character(jahr)
            )
        }
        
      } 
      hc <- hc %>%
        hc_colors(colors_plots) %>%
        hc_credits(enabled = TRUE,     
                   style = list(
                     fontSize = "11px"),
                   text = paste0("Quelle: ", quelle)) %>%
        # hc_exporting(enabled = TRUE) %>%
        hc_add_theme(stata_theme)
    }
    
    hc
  })
  
  output$dynamic_month_plot2 <- renderUI({
    if (length(input$monat) == 1) {
      layout_columns(
        col_widths=c(6,6),
      highchartOutput("linePlot2_Monat") %>% withSpinner(color="#2a9749"))
    } else {
      layout_columns(
      highchartOutput("linePlot2_Monat") %>% withSpinner(color="#2a9749"))
    }
  })
  
  # Line Plot 1 Tag:
  output$linePlot1_Tag <- renderHighchart({
    data <- data_tag_aufenthalt() %>%
      mutate(
        Datum_Monat_Tag = format(as.Date(Datum_Monat_Tag, format="%m-%d"), "%e. %b"),
        Tag_Jahr = paste(Datum_Monat_Tag, Datum_Jahr)
      )
    
    unique_days <- unique(data$Datum_Monat_Tag)
    is_single_day <- length(unique_days) == 1
    categories <- unique(data$Datum_Monat_Tag)
    category_indices <- setNames(0:(length(categories) - 1), categories)
    
    event_ranges <- data %>%
      filter(!is.na(Event) & Event != "") %>%
      group_by(Event) %>%
      summarise(
        start_day = dplyr::first(Datum_Monat_Tag),
        end_day = dplyr::last(Datum_Monat_Tag),
        .groups = "drop"
      ) %>%
      distinct()
    
    plot_bands <- list()
    if (nrow(event_ranges) > 0) {
      plot_bands <- lapply(1:nrow(event_ranges), function(i) {
        start_idx <- category_indices[[event_ranges$start_day[i]]]
        end_idx <- category_indices[[event_ranges$end_day[i]]]
        if (is.na(start_idx) || is.na(end_idx)) return(NULL)
        list(
          from = start_idx - 0.4,
          to = end_idx + 0.4,
          color = "rgba(200,200,200,0.3)",
          label = list(
            text = event_ranges$Event[i],
            rotation = -90,
            align = "left",
            verticalAlign = "bottom",
            x = 12,
            y = -5,
            style = list(color = "#606060", fontSize = "10px")
          )
        )
      })
      plot_bands <- Filter(Negate(is.null), plot_bands)
    }
    
    if (input$kat_aufenthalt_tag %in% c("Ankünfte", "Logiernächte")) {
      if (is_single_day) {
        hc <- highchart() %>%
          hc_chart(type = "column") %>%
          # hc_size(560) %>%
          hc_xAxis(categories = data$Tag_Jahr) %>%
          hc_yAxis(visible = FALSE) %>%
          hc_legend(enabled = FALSE) %>%
          hc_tooltip(shared = TRUE) %>%
          hc_plotOptions(
            column = list(
              dataLabels = list(
                enabled = TRUE,
                style = list(
                  color = "#333333",
                  fontSize = "12px",
                  fontWeight = "bold"
                )
              )
            ),
            column = list(pointPadding = 0)
          ) %>%
          hc_add_series(
            data = data,
            type = "column",
            hcaes(x = as.character(Tag_Jahr), y = .data[[paste("Anzahl", input$kat_aufenthalt_tag)]]),
            color = '#2a9749',
            name = input$kat_aufenthalt_tag
          )
      } else {
        hc <- highchart() %>%
          hc_chart(type = "line") %>%
          # hc_size(836) %>%
          hc_xAxis(
            categories = categories,
            plotBands = if (length(plot_bands) > 0) plot_bands else NULL
          ) %>%
          hc_legend(
            layout = 'horizontal',
            align = 'left',
            verticalAlign = 'top',
            enabled = T
          ) %>%
          hc_tooltip(shared = TRUE) %>%
          hc_plotOptions(
            line = list(
              dataLabels = list(enabled = FALSE),
              marker = list(enabled = FALSE, symbol = "circle")
            ),
            column = list(pointPadding = 0)
          )
        
        for (jahr in sort(unique(data$Datum_Jahr))) {
          jahr_data <- data %>% filter(Datum_Jahr == jahr)
          hc <- hc %>%
            hc_add_series(
              data = jahr_data,
              type = "line",
              hcaes(x = Datum_Monat_Tag, y = .data[[paste("Anzahl", input$kat_aufenthalt_tag)]]),
              name = as.character(jahr)
            )
        }
      }
      
    }
    
    # Common styling and final touches
    hc <- hc %>%
      hc_colors(colors_plots) %>%
      hc_credits(enabled = TRUE, style = list(fontSize = "11px"), text = paste0("Quelle: ", quelle)) %>%
      # hc_exporting(enabled = TRUE) %>%
      hc_add_theme(stata_theme)
    
    hc
  })
  
  output$dynamic_day_plot1 <- renderUI({
    if (input$startDate_tag == input$endDate_tag) {
      layout_columns(
        col_widths=c(6,6),
      highchartOutput("linePlot1_Tag") %>% withSpinner(color="#2a9749"))
    } else {
      layout_columns(
      highchartOutput("linePlot1_Tag") %>% withSpinner(color="#2a9749"))
    }
  })
  
  # Line Plot 2 Tag:
  output$linePlot2_Tag <- renderHighchart({
    data <- data_tag_zimmer() %>%
      mutate(
        Datum_Monat_Tag = format(as.Date(Datum_Monat_Tag, format = "%m-%d"), "%e. %b"),
        Tag_Jahr = paste(Datum_Monat_Tag, Datum_Jahr),
        `Anzahl verfügbare Zimmer` = round(`Anzahl verfügbare Zimmer`, 0),
        `Anzahl belegte Zimmer` = round(`Anzahl belegte Zimmer`, 0)
      )
    
    unique_days <- unique(data$Datum_Monat_Tag)
    is_single_day <- length(unique_days) == 1
    categories <- unique(data$Datum_Monat_Tag)
    category_indices <- setNames(0:(length(categories) - 1), categories)
    
    # PlotBands for Events
    event_ranges <- data %>%
      filter(!is.na(Event) & Event != "") %>%
      group_by(Event) %>%
      summarise(
        start_day = dplyr::first(Datum_Monat_Tag),
        end_day = dplyr::last(Datum_Monat_Tag),
        .groups = "drop"
      ) %>%
      distinct()
    
    plot_bands <- list()
    if (nrow(event_ranges) > 0) {
      plot_bands <- lapply(1:nrow(event_ranges), function(i) {
        start_idx <- category_indices[[event_ranges$start_day[i]]]
        end_idx <- category_indices[[event_ranges$end_day[i]]]
        if (is.na(start_idx) || is.na(end_idx)) return(NULL)
        list(
          from = start_idx - 0.4,
          to = end_idx + 0.4,
          color = "rgba(200,200,200,0.3)",
          label = list(
            text = event_ranges$Event[i],
            rotation = -90,
            align = "left",
            verticalAlign = "bottom",
            x = 12,
            y = -5,
            style = list(color = "#606060", fontSize = "10px")
          )
        )
      })
      plot_bands <- Filter(Negate(is.null), plot_bands)
    }
    
    # Dynamic y-variable
    y_var <- switch(
      input$kat_zimmer_tag,
      "Verfügbare Zimmer" = "Anzahl verfügbare Zimmer",
      "Belegte Zimmer" = "Anzahl belegte Zimmer",
      "Zimmerauslastung" = "Zimmerauslastung"
    )
    
    tooltip_formatter <- if (input$kat_zimmer_tag == "Zimmerauslastung") {
      JS("function () {
        let tooltip = '<span style=\"font-size: 10px; color: #333333\">' + this.x + '</span><br>';
        this.points.forEach(function (point) {
          tooltip += '<span style=\"color:' + point.color + '\">\u25CF</span> '
                    + point.series.name + ': <b>'
                    + point.y.toFixed(1).toString().replace('.', ',') + '%' + '</b><br>';
        });
        return tooltip;
      }")
    } 
    
    if (is_single_day) {
      hc <- highchart() %>%
        hc_chart(type = "column") %>%
        # hc_size(560) %>%
        hc_xAxis(categories = data$Tag_Jahr) %>%
        hc_yAxis(visible = FALSE) %>%
        hc_legend(enabled = FALSE) %>%
        hc_tooltip(shared = TRUE, formatter = tooltip_formatter) %>%
        hc_plotOptions(
          column = list(
            dataLabels = list(
              enabled = TRUE,
              formatter = JS(
                "function () {
          if (this.series.name === 'Zimmerauslastung') {
            return this.y.toFixed(1).replace('.', ',') + '%';
          } else {
            return Highcharts.numberFormat(this.y, 0, '.', ' ');
          }
        }"
              ),
              style = list(
                color = "#333333",
                fontSize = "12px",
                fontWeight = "bold"
              )
            )
          ),
          column = list(pointPadding = 0)
        ) %>%
        hc_add_series(
          data = data,
          type = "column",
          hcaes(x = as.character(Datum_Monat_Tag), y = .data[[y_var]]),
          color = '#2a9749',
          name = input$kat_zimmer_tag
        )
    } else {
      hc <- highchart() %>%
        hc_chart(type = "line") %>%
        # hc_size(836) %>%
        hc_xAxis(categories = categories, plotBands = plot_bands) %>%
        hc_yAxis(
          labels = if (input$kat_zimmer_tag == "Zimmerauslastung") list(format = "{value}%") else NULL,
          min = if (input$kat_zimmer_tag == "Zimmerauslastung") 0 else NULL,
          max = if (input$kat_zimmer_tag == "Zimmerauslastung") 100 else NULL
        ) %>%
        hc_legend(
          layout = 'horizontal',
          align = 'left',
          verticalAlign = 'top',
          enabled = T
        ) %>%
        hc_tooltip(shared = TRUE, formatter = tooltip_formatter) %>%
        hc_plotOptions(
          line = list(
            dataLabels = list(enabled = FALSE),
            marker = list(enabled = FALSE, symbol = "circle")
          ),
          column = list(pointPadding = 0)
        )
      
      for (jahr in sort(unique(data$Datum_Jahr))) {
        jahr_data <- data %>% filter(Datum_Jahr == jahr)
        hc <- hc %>%
          hc_add_series(
            jahr_data,
            type = "line",
            hcaes(x = Datum_Monat_Tag, y = .data[[y_var]]),
            name = as.character(jahr)
          )
      }
    }
    
    hc %>%
      hc_colors(colors_plots) %>%
      hc_credits(enabled = TRUE, text = paste0("Quelle: ", quelle), style = list(fontSize = "11px")) %>%
      # hc_exporting(enabled = TRUE) %>%
      hc_add_theme(stata_theme)
  })
  
  output$dynamic_day_plot2 <- renderUI({
    if (input$startDate_tag == input$endDate_tag) {
      layout_columns(
        col_widths=c(6,6),
      highchartOutput("linePlot2_Tag") %>% withSpinner(color="#2a9749"))
    } else {
      layout_columns(
      highchartOutput("linePlot2_Tag") %>% withSpinner(color="#2a9749"))
    }
  })

  # Data table 1:
  output$dataTable1_Monat <- renderDT( {
    # Extract and format selected month range
    selected_months <- input$monat
    
    if (!is.null(selected_months) && length(selected_months) == 2) {
      startDate <- format(as.Date(selected_months[1]), "%B_%Y")  # "Januar_2024"
      endDate <- format(as.Date(selected_months[2]), "%B_%Y")    # "Dezember_2024"
      file_suffix <- paste0(startDate, "_bis_", endDate)
    } else {
      startDate <- format(as.Date(selected_months[1]), "%B_%Y")
      endDate <- startDate    
      file_suffix <- paste0(startDate)
    }
    
    datatable(
      monat_aufenthalt_table(),
      extensions = 'Buttons',
      rownames = FALSE,
      options = list(
        paging = FALSE,
        scrollY = "500px",
        scrollCollapse = TRUE,
        lengthChange = FALSE,
        searching = FALSE,
        info = FALSE,
        dom = 'frtBip',
        buttons = list(
          list(
            extend = 'csv',
            # text = '<i class="fa-solid fa-download"></i> csv',
            text = HTML(paste0(img(src = "download.svg", class = "hover-icon"), " csv")),
            filename = paste0("Aufenthalt_nach_Monat_", file_suffix),
            exportOptions = list(
              modifier = list(page = 'all'),  # Export all data, not just the current page
              format = list(
                body = JS("function(data, row, column, node) {
                return (typeof data === 'string') ? data.replace(/\\u2009/g, '').replace(',', '.') : data;
              }")
              )
            )
          ),
          list(
            extend = 'excel',
            # text = '<i class="fa-solid fa-download"></i> xlsx',
            text = HTML(paste0(img(src = "download.svg", class = "hover-icon"), " xlsx")),
            filename = paste0("Aufenthalt_nach_Monat_", file_suffix),
            exportOptions = list(
              modifier = list(page = 'all'),  # Export all data, not just the current page
              format = list(
                body = JS("function(data, row, column, node) {
                return (typeof data === 'string') ? data.replace(/\\u2009/g, '').replace(',', '.') : data;
              }")
              )
            )
          )
        ),
        columnDefs = list(
          list(visible = FALSE, targets = c(2,3)),
          list(className = 'dt-left', targets = 0)  # Erste Spalte linksbündig
        )
      )
      # , filter = 'top'  # Enables column filters
    ) %>%
      # Apply thousands separator only for table display
      formatStyle(
        columns = c('Anzahl Ankünfte', 'Anzahl Logiernächte', 'Aufenthaltsdauer (in Tagen)'),
        `text-align` = 'right'
      ) %>%
      formatStyle(
        columns = c('Jahr'),
        `text-align` = 'left'
      ) %>%
      formatCurrency(
        columns = c('Anzahl Ankünfte', 'Anzahl Logiernächte'),
        currency = "",
        interval = 3,
        mark = "\u2009",
        dec.mark = ",",
        digits = 0
      ) %>%
      formatRound(
        columns = c('Aufenthaltsdauer (in Tagen)'),
        digits = 2,
        mark = "\u2009",
        dec.mark = ","
      )
  }, server = F)
  
  
  # Data table 2:
  output$dataTable2_Monat <- renderDT({
    # Extract and format selected month range
    selected_months <- input$monat
    
    if (!is.null(selected_months) && length(selected_months) == 2) {
      startDate <- format(as.Date(selected_months[1]), "%B_%Y")  # "Januar_2024"
      endDate <- format(as.Date(selected_months[2]), "%B_%Y")    # "Dezember_2024"
      file_suffix <- paste0(startDate, "_bis_", endDate)
    } else {
      startDate <- format(as.Date(selected_months[1]), "%B_%Y")
      endDate <- startDate    
      file_suffix <- paste0(startDate)
    }
    
    datatable(
      monat_zimmer_table(),
      extensions = 'Buttons',
      rownames = FALSE,
      options = list(
        paging = FALSE,
        scrollY = "500px",
        scrollCollapse = TRUE,
        lengthChange = FALSE,
        searching = FALSE,
        info = FALSE,
        dom = 'frtBip',
        buttons = list(
          list(
            extend = 'csv',
            # text = '<i class="fa-solid fa-download"></i> csv',
            text = HTML(paste0(img(src = "download.svg", class = "hover-icon"), " csv")),
            filename = paste0("Zimmer_nach_Monat_", file_suffix),
            exportOptions = list(
              modifier = list(page = 'all'),  # Export all data, not just the current page
              format = list(
                body = JS("function(data, row, column, node) {
                return (typeof data === 'string') ? data.replace(/\\u2009/g, '').replace(',', '.') : data;
              }")
              )
            )
          ),
          list(
            extend = 'excel',
            # text = '<i class="fa-solid fa-download"></i> xlsx',
            text = HTML(paste0(img(src = "download.svg", class = "hover-icon"), " xlsx")),
            filename = paste0("Zimmer_nach_Monat_", file_suffix),
            exportOptions = list(
              modifier = list(page = 'all'),  # Export all data, not just the current page
              format = list(
                body = JS("function(data, row, column, node) {
                return (typeof data === 'string') ? data.replace(/\\u2009/g, '').replace(',', '.') : data;
              }")
              )
            )
          )
        ),
        columnDefs = list(
          list(visible = FALSE, targets = c(2)),
          list(className = 'dt-left', targets = 0) 
        )
      )
      # ,
      # filter = 'top'  # Enables column filters
    ) %>%
      # Apply thousands separator only for table display
      formatStyle(
        columns = c('Verfügbare Zimmer', 'Belegte Zimmer', 'Zimmerauslastung (in %)'),
        `text-align` = 'right'
      ) %>%
      formatCurrency(
        columns = c('Verfügbare Zimmer', 'Belegte Zimmer'),
        currency = "",
        interval = 3,
        mark = "\u2009",
        dec.mark = ",",
        digits = 0
      ) %>%
      formatRound(
        columns = c('Zimmerauslastung (in %)'),
        digits = 1,
        mark = "\u2009",
        dec.mark = ","
      )
  }, server = F)
  
  
  # Data table 3:
  output$dataTable1_Tag <- renderDT({
    datatable(
      tag_aufenthalt_table(),
      extensions = 'Buttons',
      rownames = FALSE,
      options = list(
        paging = FALSE,
        scrollY = "500px",
        scrollCollapse = TRUE,
        lengthChange = FALSE,
        searching = F,
        info = FALSE,
        # paging = T,
        dom = 'frtBip',
        buttons = list(
          list(            
            extend = 'csv',
            text = HTML(paste0(img(src = "download.svg", class = "hover-icon"), " csv")),
            filename = paste0(
              "Aufenthalt nach Tag_",
              input$startDate_tag,
              "_bis_",
              input$endDate_tag
            ),
            exportOptions = list(
              modifier = list(page = 'all'),  # Export all data, not just the current page
              format = list(
                body = JS("function(data, row, column, node) {
                return (typeof data === 'string') ? data.replace(/\\u2009/g, '').replace(',', '.') : data;
              }")
              )
            )
          ),
          list(
            extend = 'excel',
            text = HTML(paste0(img(src = "download.svg", class = "hover-icon"), " xlsx")),
            filename = paste0(
              "Aufenthalt nach Tag_",
              input$startDate_tag,
              "_bis_",
              input$endDate_tag
            ),
            exportOptions = list(
              modifier = list(page = 'all'),  # Export all data, not just the current page
              format = list(
                body = JS("function(data, row, column, node) {
                return (typeof data === 'string') ? data.replace(/\\u2009/g, '').replace(',', '.') : data;
              }")
              )
            )
          )
        ),
        columnDefs = list(
          list(visible = FALSE, targets = c(1,2)),
          list(width = "100px", targets = 0),
          list(className = 'dt-left', targets = 0) 
        )
      )
    ) %>%
      formatCurrency(
        columns = c('Anzahl Ankünfte', 'Anzahl Logiernächte'),
        currency = "",
        interval = 3,
        mark = "\u2009",
        dec.mark = ",",
        digits = 0
      )
  }, server = F)
  
  
  # Data table 4:
  output$dataTable2_Tag <- renderDT({
    datatable(
      tag_zimmer_table(),
      extensions = 'Buttons',
      rownames = FALSE,
      options = list(
        paging = FALSE,
        scrollY = "500px",
        scrollCollapse = TRUE,
        lengthChange = FALSE,
        searching = F,
        info = FALSE,
        dom = 'frtBip',
        buttons = list(
          list(
            extend = 'csv',
            # text = '<i class="fa-solid fa-download"></i> csv',
            text = HTML(paste0(img(src = "download.svg", class = "hover-icon"), " csv")),
            filename = paste0(
              "Zimmer nach Tag_",
              input$startDate_tag,
              "_bis_",
              input$endDate_tag
            ),
            exportOptions = list(
              modifier = list(page = 'all'),  # Export all data, not just the current page
              format = list(
                body = JS("function(data, row, column, node) {
                return (typeof data === 'string') ? data.replace(/\\u2009/g, '').replace(',', '.') : data;
              }")
              )
            )
          ),
          list(
            extend = 'excel',
            # text = '<i class="fa-solid fa-download"></i> xlsx',
            text = HTML(paste0(img(src = "download.svg", class = "hover-icon"), " xlsx")),
            filename = paste0(
              "Zimmer nach Tag_",
              input$startDate_tag,
              "_bis_",
              input$endDate_tag
            ),
            exportOptions = list(
              modifier = list(page = 'all'),  # Export all data, not just the current page
              format = list(
                body = JS("function(data, row, column, node) {
                return (typeof data === 'string') ? data.replace(/\\u2009/g, '').replace(',', '.') : data;
              }")
              )
            )
          )
        ),
        columnDefs = list(
          list(visible = FALSE, targets = c(1)),
          list(width = "100px", targets = 0),
          list(className = 'dt-left', targets = 0) 
        )
      )
      # ,
      # filter = 'top'  # This line enables column filters
    ) %>%
      # Apply thousands separator to all specified columns
      formatCurrency(
        columns = c('Verfügbare Zimmer', 'Belegte Zimmer'),
        # Columns with integers
        currency = "",
        interval = 3,
        mark = "\u2009",
        # Thousands separator (narrow space)
        dec.mark = ",",
        digits = 0
      ) %>%
      # Apply decimal formatting to specific columns
      formatRound(
        columns = c('Zimmerauslastung (in %)'),
        # Columns with decimals
        digits = 1,
        # Number of decimal places
        mark = "\u2009",
        # Thousands separator (narrow space)
        dec.mark = ","    # Decimal separator (comma)
      )
  }, server = F)
  
  
  # Infotab:
  output$info_text <- renderUI({
      accordion(
        id = "info_accordion",
        open = FALSE,
        
        accordion_panel(
          "Ankünfte",
          "Mit Ankünften wird die Anzahl Gäste (Kinder eingeschlossen) erfasst, die eine oder mehrere Nächte in einem Hotelbetrieb verbringen."
        ),
        
        accordion_panel(
          "Logiernächte",
          "Anzahl der durch die Gäste (Kinder eingeschlossen) in einem Hotelbetrieb verbrachten Nächte."
        ),
        
        accordion_panel(
          "Aufenthaltsdauer",
          "Anzahl Logiernächte dividiert durch die Anzahl Ankünfte in einem Hotelbetrieb."
        ),
        
        accordion_panel(
          "Verfügbare Zimmer",
          "Die Gesamtanzahl der Zimmer, die in einem bestimmten Zeitraum in einer Unterkunft zur Verfügung stehen."
        ),
        
        accordion_panel(
          "Belegte Zimmer",
          "Die Anzahl der Zimmer, die in einem bestimmten Zeitraum tatsächlich von Gästen belegt wurden."
        ),
        
        accordion_panel(
          "Zimmerauslastung",
          "Gesamtzahl der belegten Zimmer im Verhältnis zur Gesamtzahl der verfügbaren Zimmer."
        ),
        
        accordion_panel(
          "Hotelkategorie",
          "Die Hotels werden je nach Ausstattungsgrad und Grad der gebotenen Dienstleistungen in 1- bis 5-Sterne-Betriebe eingeteilt. Gewisse Betriebe gelten als nicht klassiert. Seit September 2015 verfügt Basel nur noch über ein 5-Sterne-Hotel. Aus Datenschutzgründen werden die Hotelkategorien seither zusammengefasst."
        ),
        
        accordion_panel("Herkunftsland",
                        "Land des ständigen Wohnsitzes der Gäste.")
      )
    
  })
  
  
  # Date input Modifkation:
  observe({
    # Sicherstellen, dass Start- und Enddatum nicht NULL sind
    if (!is.null(input$startDate_tag) && !is.null(input$endDate_tag)) {
      # Startdatum als Date-Objekt
      start_date <- as.Date(input$startDate_tag)
      
      # Enddatum kann nur ab dem Startdatum sein
      updateDateInput(session, "endDate_tag", 
                      min = start_date,  # Sperrt alle Tage vor dem Startdatum
                      value = ifelse(as.Date(input$endDate_tag) < start_date, start_date, as.Date(input$endDate_tag))  # Wenn Enddatum kleiner als Startdatum, auf Startdatum setzen
      )
    }
  })
  

  # Dropdown Menu für Herkunft nur im Sub-Tab Aufenthalt (Monat):
  active_tab1 <- reactive({
    input$monattabs  # hier 'tabs' auf den Tab-Name anpassen
  })
  output$herkunft_ui_monat <- renderUI({
    if (active_tab1() == "Aufenthalt") {
      # Nur anzeigen, wenn Tab1 aktiv ist
      selectInput(
        "herkunft_monat",
        HTML(paste0(bs_icon("globe-americas"), " Herkunft")),
        choices = c("Total", "Schweiz", "Ausland", setdiff(sort(unique(tourismus_taeglich_2$Herkunftsland)), c("Total", "Schweiz", "Ausland"))),
        selected = "Total"
      )
    } else {
      # Wenn Tab2 aktiv ist, wird nichts angezeigt
      return(NULL)
    }
  })
  
  
  # Dropdown Menu für Herkunft nur im Sub-Tab Aufenthalt (Monat):
  active_tab2 <- reactive({
    input$tagtabs  # hier 'tabs' auf den Tab-Name anpassen
  })
  output$herkunft_ui_tag <- renderUI({
    if (active_tab2() == "Aufenthalt") {
      # Nur anzeigen, wenn Tab1 aktiv ist
      selectInput(
        "herkunft_tag",
        HTML(paste0(bs_icon("globe-americas"), " Herkunft")),
        choices = c("Total", "Schweiz", "Ausland", setdiff(sort(unique(tourismus_taeglich_2$Herkunftsland)), c("Total", "Schweiz", "Ausland"))),
        selected = "Total"
      )
    } else {
      # Wenn Tab2 aktiv ist, wird nichts angezeigt
      return(NULL)
    }
  })
}


# Run the application
shinyApp(ui = ui, server = server)
