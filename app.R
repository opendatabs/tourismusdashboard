# ### Tourismus #######################################################################################
# ####################### Dashboard ###################################################################
# #####################################################################################################

###Preliminaries: ####
# Remove all objects from the global environment
rm(list = ls())
  
# load packages:
library(tidyverse)
library(DT)
library(lubridate)
library(tidyr)
library(janitor)
library(shiny)
library(shinydashboard)
library(stringr)
library(highcharter)
library(bslib)
library(jsonlite)
library(httr)
library(data.table)
library(bsicons)
library(shinyWidgets)
library(shinycssloaders)
library(waiter)

conflicted::conflict_prefer("filter", "dplyr")

source("app_functions.R")

# Daten als csv laden:
tourismus_taeglich_1 <- read.csv("data/tourismus_taeglich_1.csv", stringsAsFactors = FALSE, check.names = FALSE) %>% 
  rename(Datum_Jahr = 'Datum Jahr',
         Datum_Monat = 'Datum Monat',
         Datum_Tag = 'Datum Tag',
         Datum_Monat_Tag = 'Datum Monat Tag') %>% 
  mutate(Datum = as.Date(Datum, format = "%Y-%m-%d"))
  
tourismus_taeglich_2 <- read.csv("data/tourismus_taeglich_2.csv", stringsAsFactors = FALSE, check.names = FALSE) %>% 
  rename(Datum_Jahr = 'Datum Jahr',
         Datum_Monat = 'Datum Monat',
         Datum_Tag = 'Datum Tag',
         Datum_Monat_Tag = 'Datum Monat Tag') %>% 
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
ui <- navbarPage(
  title = span(HTML("Tourismus-Dashboard"), style = "margin-right: 100px;"),
  
  theme = bs_theme(
    version = 5,
    primary = "white",
    secondary = "grey"
  ),
  
  header = tags$head(use_waiter(),
    tags$style(
      HTML(
        "a:hover { color: #2a9749 !important; }
      body { font-family: 'Inter', sans-serif; }
      .navbar.navbar-default { background-color: #b8d6be !important; color: white !important; }
      .navbar-nav {
  display: flex;
  align-items: left;
  justify-content: left;
  gap: 40px;
  margin-top: 10px;
  margin-bottom: 10px;
}

      .navbar .navbar-brand {
        font-size: 24px;
        font-family: 'Inter';
      }
      .navbar-nav > li > a {font-size: 16px; /* Ändere die Schriftgröße hier */}
      .dataTables_wrapper * { font-family: 'Inter', sans-serif !important; }
      table.dataTable { font-family: 'Inter', sans-serif !important; }
      table.dataTable thead th { font-weight: 500 !important; font-style: italic !important; }
      .dataTables_wrapper .dataTables_paginate, .dataTables_wrapper .dataTables_filter input, .dataTables_wrapper .dataTables_length select { font-family: 'Inter', sans-serif !important; }
      .dataTables_wrapper .dataTables_filter input {
        font-size: 12px !important;
      }
      .dataTables_wrapper .dataTable {
        font-size: 12px !important;
      }
      .dataTables_wrapper .dt-buttons {
        margin-top: 5px !important;
        margin-bottom: 5px !important;
        font-size: 12px !important; /* Reduziert die Schriftgröße der Buttons */
      }
      .dataTables_wrapper .dataTables_paginate .paginate_button {
        font-size: 12px !important; /* Reduziert die Schriftgröße des Paging */
        padding: 5px 10px !important; /* Macht das Paging kleiner */
      }
      .dataTables_wrapper .dataTables_length select {
        font-size: 12px !important; /* Reduziert die Schriftgröße des Dropdowns für die Länge */
        padding: 3px 5px !important; /* Macht das Dropdown kleiner */
      }
      .dataTables_wrapper .dataTables_filter input {
        padding: 3px 5px !important; /* Macht das Filterfeld kleiner */
      }
      /* Change color of the selected option in the dropdown */
      .selectize-dropdown .selectize-dropdown-content .selected {
        background-color: #2a9749 !important; /* Custom selected color */
        color: white !important; /* Ensure contrast */
      }
      ")
    ),
    tags$link(href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;700&display=swap", rel = "stylesheet"),
    tags$script(HTML("
      function sendWidth() {
        Shiny.setInputValue('window_width', window.innerWidth);
      }
      window.addEventListener('resize', sendWidth);
      window.addEventListener('load', sendWidth);
    "))
  ),
  
  footer = tags$footer(
    style = "font-size: 11px; text-align: center; width; 100%; position: relative; bottom: 0;",
    paste0(
      "Quelle: ",
      quelle,
      " | Dashboard-Version: V1.0 | Daten: ",
      min(tourismus_taeglich_1$Datum),
      " bis ",
      max(tourismus_taeglich_1$Datum)
    )
  ),
  
  tabPanel(
    "Übersicht",
    value_box(
      title = "Tourismuszahlen", 
      value = if (MM == 1) {
        paste0(monate_deutsch[1], " ", JJJJ)
      } else {
        paste0(monate_deutsch[1], " - ", monate_deutsch[MM], " ", JJJJ)
      },
      p(
        if (MM == 1) {
          paste0("gegenüber ", monate_deutsch[1], " ", JJJJ_alt)
        } else {
          paste0("gegenüber ", monate_deutsch[1], " - ", monate_deutsch[MM], " ", JJJJ_alt)
        }
      ),
      showcase = bs_icon("calendar4-range"),
      theme_color = "primary",
      showcase_layout = "top right" 
    ),
    layout_columns(
      uiOutput("Ankuenfte"),
      uiOutput("Logiernaechte"),
      uiOutput("Aufenthaltsdauer")
    ),
    layout_columns(
      uiOutput("verfuegbareZimmer"),
      uiOutput("belegteZimmer"),
      uiOutput("Zimmerauslastung")
    ),
    layout_columns(
      card(
        card_header("Logiernächte nach Herkunftsland - Top 10"),
        highchartOutput("barPlot_herkunft")
      )
    )
  ),
  
  tabPanel(
    "Monat",
    fluidPage(
    layout_sidebar(
      sidebar = sidebar(
        open = T,
        fillable = FALSE,
        selectInput(
          "jahr_monat",
          HTML(paste0(
            bs_icon("calendar4"), " Auswertungsjahr"
          )),
          choices = unique(tourismus_taeglich_1$Datum_Jahr)[-1],
          selected = max(unique(tourismus_taeglich_1$Datum_Jahr)[-1])
        ),
        uiOutput("monat_ui"),
        selectInput(
          "hotelkategorie_monat",
          HTML(paste0(bs_icon("house"), " Hotelkategorie")),
          choices = c("Total", setdiff(unique(tourismus_taeglich_1$Hotelkategorie), "Total")),
          selected = "Total"
        ),
        uiOutput("herkunft_ui_monat")
      ),
      navset_underline(id = "monattabs",
                       nav_panel(
                         "Aufenthalt", 
                         br(),
                         layout_columns(
                                       uiOutput("dynamic_value_box1")
                                     ),
                                     layout_columns(
                                       uiOutput("Ankuenfte_Monat"),
                                       uiOutput("Logiernaechte_Monat"),
                                       uiOutput("Aufenthaltsdauer_Monat")
                                     ),
                                     layout_columns(
                                       card(
                                         card_header("Logiernächte nach Monat"),
                                         highchartOutput("linePlot1_Monat", height = 500) %>% withSpinner(color="#2a9749")
                                       ),
                                       card(
                                         card_header("Monatliche Daten"),
                                         DTOutput("dataTable1_Monat",  height = 500) %>% withSpinner(color="#2a9749")
                                       )
                                     )
                       ),
                       nav_panel(
                         "Zimmer", 
                         br(),
                         layout_columns(
                                       uiOutput("dynamic_value_box2")
                                     ),
                                     layout_columns(
                                       uiOutput("verfuegbareZimmer_Monat"),
                                       uiOutput("belegteZimmer_Monat"),
                                       uiOutput("Zimmerauslastung_Monat")
                                     ),
                                     layout_columns(
                                       card(
                                         card_header("Zimmerauslastung nach Monat"),
                                         highchartOutput("linePlot2_Monat", height = 500) %>% withSpinner(color="#2a9749")
                                       ),
                                       card(
                                         card_header("Monatliche Daten"),
                                         DTOutput("dataTable2_Monat", height = 500) %>% withSpinner(color="#2a9749")
                                       )
                                     )
                       )
                       
      )
      
    ))
  ),
  
  tabPanel(
    "Tag",
    fluidPage(
      tags$style(HTML("
 .datepicker table tr td.active, 
      .datepicker table tr td.active:hover {
        background-color: #2a9749 !important;
        color: white !important;
      }
      
      .datepicker {
        font-family: Inter, sans-serif;
        border-radius: 8px;
        border: 1px solid #ccc;
        box-shadow: 2px 2px 10px rgba(0,0,0,0.2);
      }
      
      .datepicker table tr td, 
      .datepicker table tr th {
        padding: 8px;
      }
      
      .datepicker-dropdown {
        border-radius: 10px;
        overflow: hidden;
      }
          .air-datepicker-cell.-selected- {
      background-color: #2a9749 !important; /* Hintergrundfarbe */
    }
        .air-datepicker-body--day-name {
      color: #1e4557 !important; /* Ändere die Farbe hier */
      
        }
    ")),
      layout_sidebar(
        sidebar = sidebar(
          open = T,
          fillable = FALSE,
          dateInput(
            "startDate_tag",
            HTML(paste0(bs_icon("calendar3-week"), " Startdatum")),
            value = as.Date(paste0(JJJJ, "-", MM, "-01")),
            min = "2024-01-01",
            max = as.Date(paste0(JJJJ, "-", MM, "-", DD)),
            datesdisabled = "2024-02-29",
            autoclose = TRUE,
            language = "de",
          ),
          dateInput(
            "endDate_tag",
            HTML(paste0(bs_icon("calendar3-week"), " Enddatum")),
            value = as.Date(paste0(JJJJ, "-", MM, "-", DD)),
            min = "2024-01-01",
            max = as.Date(paste0(JJJJ, "-", MM, "-", DD)),
            datesdisabled = "2024-02-29",
            autoclose = TRUE,
            language = "de",
          ),
          selectInput(
                           "hotelkategorie_tag",
                           HTML(paste0(bs_icon("house"), " Hotelkategorie")),
                           choices = c("Total", setdiff(unique(tourismus_taeglich_1$Hotelkategorie), "Total")),
                           selected = "Total"
                         ),
          uiOutput("herkunft_ui_tag")
        ),
        navset_underline(id = "tagtabs",
                         nav_panel(
                           "Aufenthalt", 
                           br(),
                           layout_columns(
                             uiOutput("dynamic_value_box3")
                           ),
                           layout_columns(
                             uiOutput("Ankuenfte_Tag"),
                             uiOutput("Logiernaechte_Tag"),
                             uiOutput("Aufenthaltsdauer_Tag")
                           ),
                           layout_columns(
                             card(
                               card_header("Logiernächte nach Tag"),
                               highchartOutput("linePlot1_Tag", height = 500) %>% withSpinner(color="#2a9749")
                             ),
                             card(
                               card_header("Tägliche Daten"),
                               DTOutput("dataTable1_Tag",  height = 500) %>% withSpinner(color="#2a9749")
                             )
                           )
                         ),
                         nav_panel(
                           "Zimmer", 
                           br(),
                           layout_columns(
                             uiOutput("dynamic_value_box4")
                           ),
                           layout_columns(
                             uiOutput("verfuegbareZimmer_Tag"),
                             uiOutput("belegteZimmer_Tag"),
                             uiOutput("Zimmerauslastung_Tag")
                           ),
                           layout_columns(
                             card(
                               card_header("Zimmerauslastung nach Tag"),
                               highchartOutput("linePlot2_Tag", height = 500) %>% withSpinner(color="#2a9749")
                             ),
                             card(
                               card_header("Tägliche Daten"),
                               DTOutput("dataTable2_Tag", height = 500) %>% withSpinner(color="#2a9749")
                             )
                           )
                         )
                         
        )
        
      ))
  ),
  
  tabPanel(
    "Info",
    fluidPage(
      h3("Definitionen"),
      p("Hier werden die wichtigsten Begriffe erläutert, welche im Dashboard verwendet werden."),
      uiOutput("info_text"),
      br(),
      h3("Kontakt"),
      p("Statistisches Amt Basel-Stadt", br(), "Fabienne Hofer", br(), a("fabienne.hofer@bs.ch", href = "mailto:fabienne.hofer@bs.ch", style = "color: black;"))
    )
  )
)

# server:
server <- function(input, output, session) {
  
  waiter_show(
    html = tagList(
      bs5_spinner(
        color = c("dark")
      ),
      h3("Tourismus-Dashboard wird geladen...", style = "color: black;")
    ),
    color = "#b8d6be"
  )
  
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
      # Single month selected
      month <- format(as.Date(selected_month_range[1]), "%B")
      return(paste0(month, " ", selected_year))
    } else {
      # Range of months selected
      month_start <- format(as.Date(selected_month_range[1]), "%B")
      month_end <- format(as.Date(selected_month_range[2]), "%B")
      return(paste0(month_start, " - ", month_end, " ", selected_year))
    }
  })
  
  # Value Box Monat Aufenthalt Past:
  reactive_value_text_past1 <- reactive({
    req(input$monat, input$jahr_monat)
    selected_year <- as.numeric(input$jahr_monat)
    selected_year_past <- selected_year - 1
    selected_month_range <- input$monat
    
    if (is.null(selected_month_range)) {
      return(paste0("Jahr ", selected_year_past))
    }
    
    if (length(selected_month_range) == 1) {
      # Single month selected
      month <- format(as.Date(selected_month_range[1]), "%B")
      return(paste0(month, " ", selected_year_past))
    } else {
      # Range of months selected
      month_start <- format(as.Date(selected_month_range[1]), "%B")
      month_end <- format(as.Date(selected_month_range[2]), "%B")
      return(paste0(month_start, " - ", month_end, " ", selected_year_past))
    }
  })
  
  # Dynamic value box
  output$dynamic_value_box1 <- renderUI({
    req(input$monat, input$jahr_monat)
    value_text1 <- reactive_value_text1()
    value_text_past1 <- reactive_value_text_past1()
    
    value_box(
      title = "Tourismuszahlen",
      value = value_text1,
      p(paste0("gegenüber ", value_text_past1)),
      showcase = bs_icon("calendar4-range"),
      theme_color = "primary",
      showcase_layout = "top right"
    )
  })
  
  
  # Value Box Monat Zimmer:
  reactive_value_text2 <- reactive({
    req(input$monat, input$jahr_monat)
    selected_year <- as.numeric(input$jahr_monat)
    selected_month_range <- input$monat
    
    if (is.null(selected_month_range)) {
      return(paste0("Jahr ", selected_year))
    }
    
    if (length(selected_month_range) == 1) {
      # Single month selected
      month <- format(as.Date(selected_month_range[1]), "%B")
      return(paste0(month, " ", selected_year))
    } else {
      # Range of months selected
      month_start <- format(as.Date(selected_month_range[1]), "%B")
      month_end <- format(as.Date(selected_month_range[2]), "%B")
      return(paste0(month_start, " - ", month_end, " ", selected_year))
    }
  })
  
  # Value Box Monat Zimmer Past:
  reactive_value_text_past2 <- reactive({
    req(input$monat, input$jahr_monat)
    selected_year <- as.numeric(input$jahr_monat)
    selected_year_past <- selected_year - 1
    selected_month_range <- input$monat
    
    if (is.null(selected_month_range)) {
      return(paste0("Jahr ", selected_year_past))
    }
    
    if (length(selected_month_range) == 1) {
      # Single month selected
      month <- format(as.Date(selected_month_range[1]), "%B")
      return(paste0(month, " ", selected_year_past))
    } else {
      # Range of months selected
      month_start <- format(as.Date(selected_month_range[1]), "%B")
      month_end <- format(as.Date(selected_month_range[2]), "%B")
      return(paste0(month_start, " - ", month_end, " ", selected_year_past))
    }
  })
  
  # Dynamic value box
  output$dynamic_value_box2 <- renderUI({
    req(input$monat)
    value_text2 <- reactive_value_text2()
    value_text_past2 <- reactive_value_text_past2()
    
    value_box(
      title = "Tourismuszahlen",
      value = value_text2,
      p(paste0("gegenüber ", value_text_past2)),
      showcase = bs_icon("calendar4-range"),
      theme_color = "primary",
      showcase_layout = "top right"
    )
  })
  

  reactive_value_text3 <- reactive({
    req(input$startDate_tag, input$endDate_tag, input$window_width)
    
    date_format <- ifelse(input$window_width < 768, "%e. %b %y", "%e. %B %Y")
    
    start_date <- format(input$startDate_tag, date_format)
    end_date <- format(input$endDate_tag, date_format)
    
    # Überprüfen, ob Start- und Enddatum identisch sind
    if (input$startDate_tag == input$endDate_tag) {
      return(start_date)  # Nur das Startdatum anzeigen
    } else {
      return(paste0(start_date, " - ", end_date))
    }
  })
  
  reactive_value_text_past3 <- reactive({
    req(input$startDate_tag, input$endDate_tag, input$window_width)
    
    date_format <- ifelse(input$window_width < 768, "%e. %b %y", "%e. %B %Y")
    
    start_date <- format(input$startDate_tag - lubridate::years(1), date_format)
    
    if (lubridate::month(input$endDate_tag) == 2 & lubridate::day(input$endDate_tag) == 29) {
      end_date <- format(as.Date(paste0(lubridate::year(input$endDate_tag) - 1, "-02-28")), date_format)
    } else {
      end_date <- format(input$endDate_tag - lubridate::years(1), date_format)
    }
    
    # Überprüfen, ob Start- und Enddatum identisch sind
    if (input$startDate_tag == input$endDate_tag) {
      return(start_date)  # Nur das Startdatum anzeigen
    } else {
      return(paste0(start_date, " - ", end_date))
    }
  })
  
  # Dynamic value box
  output$dynamic_value_box3 <- renderUI({
    value_text3 <- reactive_value_text3()
    value_text_past3 <- reactive_value_text_past3()
    
    value_box(
      title = "Tourismuszahlen",
      value = value_text3,
      p(paste0(ifelse(input$window_width < 768, "ggü. ", "gegenüber "), value_text_past3)),
      showcase = bs_icon("calendar4-range"),
      theme_color = "primary",
      showcase_layout = "top right"
    )
  })
  
  reactive_value_text4 <- reactive({
    req(input$startDate_tag, input$endDate_tag, input$window_width)
    
    date_format <- ifelse(input$window_width < 768, "%e. %b %y", "%e. %B %Y")
    
    start_date <- format(input$startDate_tag, date_format)
    end_date <- format(input$endDate_tag, date_format)
    
    # Überprüfen, ob Start- und Enddatum identisch sind
    if (input$startDate_tag == input$endDate_tag) {
      return(start_date)  # Nur das Startdatum anzeigen
    } else {
      return(paste0(start_date, " - ", end_date))
    }
  })
  
  reactive_value_text_past4 <- reactive({
    req(input$startDate_tag, input$endDate_tag, input$window_width)
    
    date_format <- ifelse(input$window_width < 768, "%e. %b %y", "%e. %B %Y")
    
    start_date <- format(input$startDate_tag - lubridate::years(1), date_format)
    
    if (lubridate::month(input$endDate_tag) == 2 & lubridate::day(input$endDate_tag) == 29) {
      end_date <- format(as.Date(paste0(lubridate::year(input$endDate_tag) - 1, "-02-28")), date_format)
    } else {
      end_date <- format(input$endDate_tag - lubridate::years(1), date_format)
    }
    
    # Überprüfen, ob Start- und Enddatum identisch sind
    if (input$startDate_tag == input$endDate_tag) {
      return(start_date)  # Nur das Startdatum anzeigen
    } else {
      return(paste0(start_date, " - ", end_date))
    }
  })
  
  # Dynamic value box
  output$dynamic_value_box4 <- renderUI({
    value_text4 <- reactive_value_text4()
    value_text_past4 <- reactive_value_text_past4()
    
    value_box(
      title = "Tourismuszahlen",
      value = value_text4,
      p(paste0(ifelse(input$window_width < 768, "ggü. ", "gegenüber "), value_text_past4)),
      showcase = bs_icon("calendar4-range"),
      theme_color = "primary",
      showcase_layout = "top right"
    )
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
    req(input$monat, input$herkunft_monat)
    
    if (length(input$monat) == 1) {
      start_month <-
        as.Date(paste(input$monat[1], "-01", sep = ""))
      end_month <- start_month + months(1) - days(1)
    } else {
      start_month <-
        as.Date(paste(input$monat[1], "-01", sep = ""))
      end_month <-
        as.Date(paste(input$monat[2], "-01", sep = "")) + months(1) - days(1)
    }
    
      tourismus_taeglich_2 %>%
        filter(
          Datum >= start_month &
            Datum <= end_month,
          Hotelkategorie == input$hotelkategorie_monat,
          Herkunftsland == input$herkunft_monat
        )
  })
  
  val_box_monat_aufenthalt_past <- reactive({
    req(input$monat)
    
    if (length(input$monat) == 1) {
      start_month <-
        as.Date(paste(input$monat[1], "-01", sep = ""))
      end_month <- start_month + months(1) - days(1)
    } else {
      start_month <-
        as.Date(paste(input$monat[1], "-01", sep = ""))
      end_month <-
        as.Date(paste(input$monat[2], "-01", sep = "")) + months(1) - days(1)
    }
    
    start_month_prev_year <- start_month - years(1)
    
    if (lubridate::month(end_month) == 2 & lubridate::day(end_month) == 29) {
      end_month_prev_year <- as.Date(paste0(lubridate::year(end_month) - 1, "-02-28"))
    } else {
      end_month_prev_year <- end_month - lubridate::years(1)
    }
    
    # Filter anwenden
      tourismus_taeglich_2 %>%
        filter(
          Datum >= start_month_prev_year &
            Datum <= end_month_prev_year,
          Hotelkategorie == input$hotelkategorie_monat,
          Herkunftsland == input$herkunft_monat
        )
  })
  
  val_box_monat_zimmer <- reactive({
    req(input$monat)

    # Überprüfen, ob ein oder mehrere Monate ausgewählt wurden
    if (length(input$monat) == 1) {
      # Nur ein Monat ausgewählt
      start_month <-
        as.Date(paste(input$monat[1], "-01", sep = ""))
      end_month <- start_month + months(1) - days(1)
    } else {
      # Zeitraum mit Start- und Endmonat
      start_month <-
        as.Date(paste(input$monat[1], "-01", sep = ""))
      end_month <-
        as.Date(paste(input$monat[2], "-01", sep = "")) + months(1) - days(1)
    }

    # Filtern des Datensatzes basierend auf dem Zeitraum
    tourismus_taeglich_1 %>%
      filter(Datum >= start_month &
               Datum <= end_month,
             Hotelkategorie == input$hotelkategorie_monat)
  })

  
  val_box_monat_zimmer_past <- reactive({
    req(input$monat)

    # Überprüfen, ob ein oder mehrere Monate ausgewählt wurden
    if (length(input$monat) == 1) {
      # Nur ein Monat ausgewählt
      start_month <-
        as.Date(paste(input$monat[1], "-01", sep = ""))
      end_month <- start_month + months(1) - days(1)
    } else {
      # Zeitraum mit Start- und Endmonat
      start_month <-
        as.Date(paste(input$monat[1], "-01", sep = ""))
      end_month <-
        as.Date(paste(input$monat[2], "-01", sep = "")) + months(1) - days(1)
    }

    # Verschiebe den Start- und Endmonat um ein Jahr zurück
    start_month_prev_year <- start_month - years(1)
    if (lubridate::month(end_month) == 2 & lubridate::day(end_month) == 29) {
      end_month_prev_year <- as.Date(paste0(lubridate::year(end_month) - 1, "-02-28"))
    } else {
      end_month_prev_year <- end_month - lubridate::years(1)
    }

    # Filtern des Datensatzes für den gleichen Zeitraum im Vorjahr
    tourismus_taeglich_1 %>%
      filter(
        Datum >= start_month_prev_year &
          Datum <= end_month_prev_year,
        Hotelkategorie == input$hotelkategorie_monat
      )
  })


  val_box_tag_zimmer <- reactive({
    req(input$startDate_tag, input$endDate_tag)
    
    # Start- und Enddatum aus den Eingaben
    start_date <- as.Date(input$startDate_tag)
    end_date <- as.Date(input$endDate_tag)
    
    tourismus_taeglich_1 %>%
      filter(Datum >= start_date &
               Datum <= end_date,
             Hotelkategorie == input$hotelkategorie_tag)
  })
  
  val_box_tag_zimmer_past <- reactive({
    req(input$startDate_tag, input$endDate_tag)
    
    start_date <- as.Date(input$startDate_tag) - lubridate::years(1)
    end_date_prev <- as.Date(input$endDate_tag)
    
    if (lubridate::month(end_date_prev) == 2 & lubridate::day(end_date_prev) == 29) {
      end_date <- as.Date(paste0(lubridate::year(end_date_prev) - 1, "-02-28"))
    } else {
      end_date <- end_date_prev - lubridate::years(1)
    }
    
    tourismus_taeglich_1 %>%
      filter(Datum >= start_date & Datum <= end_date,
             Hotelkategorie == input$hotelkategorie_tag)
  })
  
  
  # Value Box Tag Aufenthalt
  val_box_tag_aufenthalt <- reactive({
    req(input$startDate_tag, input$endDate_tag, input$herkunft_tag)
    
    # Start- und Enddatum aus den Eingaben
    start_date <- as.Date(input$startDate_tag)
    end_date <- as.Date(input$endDate_tag)
    
    # Filter anwenden
      tourismus_taeglich_2 %>%
        filter(
          Datum >= start_date &
            Datum <= end_date,
          Hotelkategorie == input$hotelkategorie_tag,
          Herkunftsland == input$herkunft_tag
        )
  })
  
  #Value Box Tag Aufenthalt past:
  val_box_tag_aufenthalt_past <- reactive({
    req(input$startDate_tag, input$endDate_tag, input$herkunft_tag)
    
    start_date <- as.Date(input$startDate_tag) - lubridate::years(1)
    end_date_prev <- as.Date(input$endDate_tag)
    
    if (lubridate::month(end_date_prev) == 2 & lubridate::day(end_date_prev) == 29) {
      end_date <- as.Date(paste0(lubridate::year(end_date_prev) - 1, "-02-28"))
    } else {
      end_date <- end_date_prev - lubridate::years(1)
    }
    
    # Filter anwenden
      tourismus_taeglich_2 %>%
        filter(
          Datum >= start_date &
            Datum <= end_date,
          Hotelkategorie == input$hotelkategorie_tag,
          Herkunftsland == input$herkunft_tag
        )
  })
  
  # data_monat_zimmer
  data_monat_zimmer <- reactive({
    req(input$monat)
    
    # Überprüfen, ob ein oder mehrere Monate ausgewählt wurden
    if (length(input$monat) == 1) {
      # Nur ein Monat ausgewählt
      start_month <-
        as.Date(paste(input$monat[1], "-01", sep = ""))
      end_month <- start_month + months(1) - days(1)
    } else {
      # Zeitraum mit Start- und Endmonat
      start_month <-
        as.Date(paste(input$monat[1], "-01", sep = ""))
      end_month <-
        as.Date(paste(input$monat[2], "-01", sep = "")) + months(1) - days(1)
    }
    
    # Verschiebe den Start- und Endmonat um ein Jahr zurück
    start_month_prev_year <- start_month - years(1)
    if (lubridate::month(end_month) == 2 & lubridate::day(end_month) == 29) {
      end_month_prev_year <- as.Date(paste0(lubridate::year(end_month) - 1, "-02-28"))
    } else {
      end_month_prev_year <- end_month - lubridate::years(1)
    }
    
    # Datensätze für den ausgewählten Zeitraum und den Vergleichszeitraum im Vorjahr erstellen
    bind_1 <- bind_rows(
      tourismus_taeglich_1 %>%
        filter(
          Datum >= start_month &
            Datum <= end_month,
          Hotelkategorie == input$hotelkategorie_monat
        ),
      tourismus_taeglich_1 %>%
        filter(
          Datum >= start_month_prev_year &
            Datum <= end_month_prev_year,
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
  
  
  # Data_monat_aufenthalt
  data_monat_aufenthalt <- reactive({
    req(input$monat)
    
    # Überprüfen, ob ein oder mehrere Monate ausgewählt wurden
    if (length(input$monat) == 1) {
      # Nur ein Monat ausgewählt
      start_month <-
        as.Date(paste(input$monat[1], "-01", sep = ""))
      end_month <- start_month + months(1) - days(1)
    } else {
      # Zeitraum mit Start- und Endmonat
      start_month <-
        as.Date(paste(input$monat[1], "-01", sep = ""))
      end_month <-
        as.Date(paste(input$monat[2], "-01", sep = "")) + months(1) - days(1)
    }
    
    # Verschiebe den Start- und Endmonat um ein Jahr zurück
    start_month_prev_year <- start_month - years(1)
    if (lubridate::month(end_month) == 2 & lubridate::day(end_month) == 29) {
      end_month_prev_year <- as.Date(paste0(lubridate::year(end_month) - 1, "-02-28"))
    } else {
      end_month_prev_year <- end_month - lubridate::years(1)
    }
    
    # Filter anwenden
      bind_1 <- bind_rows(
        tourismus_taeglich_2 %>%
          filter(
            Datum >= start_month &
              Datum <= end_month,
            Hotelkategorie == input$hotelkategorie_monat,
            Herkunftsland == input$herkunft_monat
          ),
        tourismus_taeglich_2 %>%
          filter(
            Datum >= start_month_prev_year &
              Datum <= end_month_prev_year,
            Hotelkategorie == input$hotelkategorie_monat,
            Herkunftsland == input$herkunft_monat
          )
      )    # Gruppieren, Zusammenfassen und weitere Berechnungen
      bind_1 %>%
        group_by(Datum_Jahr,
                 Datum_Monat,
                 Monat,
                 Hotelkategorie,
                 Herkunftsland) %>%
        summarise(
          `Anzahl Ankünfte` = sum(`Anzahl Ankünfte`, na.rm = TRUE),
          `Anzahl Logiernächte` = sum(`Anzahl Logiernächte`, na.rm = TRUE),
          Aufenthaltsdauer = sum(`Anzahl Logiernächte` / `Anzahl Ankünfte`, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        arrange(desc(Datum_Jahr), Monat) %>%
        mutate(Datum_Monat = factor(Datum_Monat, levels = unique(Datum_Monat)))

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
            Datum >= start_date &
              Datum <= end_date,
            Hotelkategorie == input$hotelkategorie_tag,
            Herkunftsland == input$herkunft_tag
          ),
        tourismus_taeglich_2 %>%
          filter(
            Datum >= start_date_past &
              Datum <= end_date_past,
            Hotelkategorie == input$hotelkategorie_tag,
            Herkunftsland == input$herkunft_tag
          )
      )    # Gruppieren, Zusammenfassen und weitere Berechnungen
      bind_1 %>%
        group_by(Datum,
                 Datum_Jahr,
                 Datum_Monat,
                 Datum_Monat_Tag,
                 Hotelkategorie,
                 Herkunftsland) %>%
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
          Datum >= start_date &
            Datum <= end_date,
          Hotelkategorie == input$hotelkategorie_tag
        ),
      tourismus_taeglich_1 %>%
        filter(
          Datum >= start_date_past &
            Datum <= end_date_past,
          Hotelkategorie == input$hotelkategorie_tag
        )
    )
    # Gruppieren, Zusammenfassen und weitere Berechnungen
    bind_1 %>%
      group_by(Datum, Datum_Jahr, Datum_Monat, Datum_Monat_Tag, Hotelkategorie) %>%
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
  
  # Data Tag Aufenthalt Table:
  tag_aufenthalt_table <- reactive({
    data_tag_aufenthalt() %>%
      arrange(desc(Datum_Jahr)) %>%
      select(
        Datum,
        Hotelkategorie,
        Herkunft = Herkunftsland,
        `Anzahl Ankünfte`,
        `Anzahl Logiernächte`) 
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
        Zimmerauslastung
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
      tags$span(
        bsicons::bs_icon("arrow-up-right-circle"),
        style = paste0("color: ", colors_valueboxes[1], "; font-size: 2rem;")
      )
    } else {
      tags$span(
        bsicons::bs_icon("arrow-down-right-circle"),
        style = paste0("color: ", colors_valueboxes[2], "; font-size: 2rem;")
      )
    }
    
    value_box(
      value = HTML(
        paste0(
          "<span style='color: #333333; font-size: 0.9em;'>",
          add_thousand_separator(current_value, current_data_digits), unit_value,
          "</span> ",
          "<span style='font-size: 0.5em; color:",
          custom_color,
          ";'>(",
          ifelse(value_change > 0, "+", ""),
          add_thousand_separator(value_change, previous_data_digits), unit_change,
          " | ",
          ifelse(value_change > 0, "+", ""),
          add_thousand_separator(round(value_change_proz, 1), 1),
          "%)</span>"
        )
      ),
      label,
      showcase = tagList(arrow_icon),
      showcase_layout = "top right",
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
      label = "Ankünfte \u2211"
    )
  })
  
  # Logiernächte (aktuellste Jahresperiode):
  output$Logiernaechte <- renderUI({
    create_value_box(
      current_data = sum(newest_month_all_hotels()$`Anzahl Logiernächte`),
      current_data_digits = 0,
      previous_data = sum(newest_month_all_hotels_past()$`Anzahl Logiernächte`),
      previous_data_digits = 0,
      label = "Logiernächte \u2211"
    )
  })
  
  # Aufenthaltsdauer (aktuellste Jahresperiode):
  output$Aufenthaltsdauer <- renderUI({
    create_value_box(
      current_data = mean(sum(newest_month_all_hotels()$`Anzahl Logiernächte`) / sum(newest_month_all_hotels()$`Anzahl Ankünfte`)),
      current_data_digits = 2,
      previous_data = mean(sum(newest_month_all_hotels_past()$`Anzahl Logiernächte`) / sum(newest_month_all_hotels_past()$`Anzahl Ankünfte`)),
      previous_data_digits = 2,
      label = "Aufenthaltsdauer \u00D8",
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
      label = "Verfügbare Zimmer \u00D8"
    )
  })
  
  # Belegte Zimmer (aktuellste Jahresperiode):
  output$belegteZimmer <- renderUI({
    create_value_box(
      current_data = mean(newest_month_all_hotels()$`Anzahl belegte Zimmer`),
      current_data_digits = 0,
      previous_data = mean(newest_month_all_hotels_past()$`Anzahl belegte Zimmer`),
      previous_data_digits = 0,
      label = "Belegte Zimmer \u00D8"
    )
  })
  
  # Zimmerauslastung (aktuellste Jahresperiode):
  output$Zimmerauslastung <- renderUI({
    create_value_box(
      current_data = mean(sum(newest_month_all_hotels()$`Anzahl belegte Zimmer`) / sum(newest_month_all_hotels()$`Anzahl verfügbare Zimmer`) * 100),
      current_data_digits = 1,
      previous_data = mean(sum(newest_month_all_hotels_past()$`Anzahl belegte Zimmer`) / sum(newest_month_all_hotels_past()$`Anzahl verfügbare Zimmer`) * 100),
      previous_data_digits = 1,
      label = "Zimmerauslastung \u00D8",
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
      label = "Ankünfte \u2211"
    )
  })
  
  # Logiernächte (Monat):
  output$Logiernaechte_Monat <- renderUI({
    create_value_box(
      current_data = sum(val_box_monat_aufenthalt()$`Anzahl Logiernächte`),
      current_data_digits = 0,
      previous_data = sum(val_box_monat_aufenthalt_past()$`Anzahl Logiernächte`),
      previous_data_digits = 0,
      label = "Logiernächte \u2211"
    )
  })
  
  # Aufenthaltsdauer (Monat):
  output$Aufenthaltsdauer_Monat <- renderUI({
    create_value_box(
      current_data = mean(sum(val_box_monat_aufenthalt()$`Anzahl Logiernächte`) / sum(val_box_monat_aufenthalt()$`Anzahl Ankünfte`)),
      current_data_digits = 2,
      previous_data = mean(sum(val_box_monat_aufenthalt_past()$`Anzahl Logiernächte`) / sum(val_box_monat_aufenthalt_past()$`Anzahl Ankünfte`)),
      previous_data_digits = 2,
      label = "Aufenthaltsdauer \u00D8",
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
      label = "Verfügbare Zimmer \u00D8"
    )
  })
  
  # Belegte Zimmer (Monat):
  output$belegteZimmer_Monat <- renderUI({
    create_value_box(
      current_data = mean(val_box_monat_zimmer()$`Anzahl belegte Zimmer`),
      current_data_digits = 0,
      previous_data = mean(val_box_monat_zimmer_past()$`Anzahl belegte Zimmer`),
      previous_data_digits = 0,
      label = "Belegte Zimmer \u00D8"
    )
  })
  
  # Zimmerauslastung (Monat):
  output$Zimmerauslastung_Monat <- renderUI({
    create_value_box(
      current_data = mean(sum(val_box_monat_zimmer()$`Anzahl belegte Zimmer`) / sum(val_box_monat_zimmer()$`Anzahl verfügbare Zimmer`) * 100),
      current_data_digits = 1,
      previous_data = mean(sum(val_box_monat_zimmer_past()$`Anzahl belegte Zimmer`) / sum(val_box_monat_zimmer_past()$`Anzahl verfügbare Zimmer`) * 100),
      previous_data_digits = 1,
      label = "Zimmerauslastung \u00D8",
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
      label = "Ankünfte \u2211"
    )
  })
  
  # Logiernächte (Tag):
  output$Logiernaechte_Tag <- renderUI({
    create_value_box(
      current_data = sum(val_box_tag_aufenthalt()$`Anzahl Logiernächte`),
      current_data_digits = 0,
      previous_data = sum(val_box_tag_aufenthalt_past()$`Anzahl Logiernächte`),
      previous_data_digits = 0,
      label = "Logiernächte \u2211"
    )
  })
  
  # Aufenthaltsdauer (Tag):
  output$Aufenthaltsdauer_Tag <- renderUI({
    create_value_box(
      current_data = mean(sum(val_box_tag_aufenthalt()$`Anzahl Logiernächte`) / sum(val_box_tag_aufenthalt()$`Anzahl Ankünfte`)),
      current_data_digits = 2,
      previous_data = mean(sum(val_box_tag_aufenthalt_past()$`Anzahl Logiernächte`) / sum(val_box_tag_aufenthalt_past()$`Anzahl Ankünfte`)),
      previous_data_digits = 2,
      label = "Aufenthaltsdauer \u00D8",
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
      label = "Verfügbare Zimmer \u00D8"
    )
    
  })
  
  # Belegte Zimmer (Tag):
  output$belegteZimmer_Tag <- renderUI({
    create_value_box(
      current_data = mean(val_box_tag_zimmer()$`Anzahl belegte Zimmer`),
      current_data_digits = 0,
      previous_data = mean(val_box_tag_zimmer_past()$`Anzahl belegte Zimmer`),
      previous_data_digits = 0,
      label = "Belegte Zimmer \u00D8"
    )
  })
  
  # Zimmerauslastung (Tag):
  output$Zimmerauslastung_Tag <- renderUI({
    create_value_box(
      current_data = mean(sum(val_box_tag_zimmer()$`Anzahl belegte Zimmer`) / sum(val_box_tag_zimmer()$`Anzahl verfügbare Zimmer`) * 100),
      current_data_digits = 1,
      previous_data = mean(sum(val_box_tag_zimmer_past()$`Anzahl belegte Zimmer`) / sum(val_box_tag_zimmer_past()$`Anzahl verfügbare Zimmer`) * 100),
      previous_data_digits = 1,
      label = "Zimmerauslastung \u00D8",
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
      hc_exporting(enabled = TRUE) %>%
      hc_add_theme(stata_theme)
    waiter_hide()
    hc
  })
  
  
  # Line Plot 1 Monat:
  output$linePlot1_Monat <- renderHighchart({
    # Daten vorab filtern und Gruppengröße berechnen
    data <- data_monat_aufenthalt()
    group_counts <- data %>%
      group_by(Datum_Jahr) %>%
      summarise(count = n()) %>%
      ungroup()
    
    # Prüfen, ob nur ein Monat ausgewählt ist
    unique_months <- unique(data$Monat)
    is_single_month <- length(unique_months) == 1
    
    # Diagrammtyp dynamisch setzen
    chart_type <- if (is_single_month) "column" else "line"
    
    month_mapping <- c(
      "1" = "Jan", "2" = "Feb", "3" = "Mär", "4" = "Apr", "5" = "Mai", "6" = "Jun", 
      "7" = "Jul", "8" = "Aug", "9" = "Sep", "10" = "Okt", "11" = "Nov", "12" = "Dez"
    )
    
    # Um die Monatsnamen für die x-Achse zu holen, indem die numerischen Werte genutzt werden
    xAxis_months <- month_mapping[as.character(unique_months)]
    
    hc <- highchart() %>%
      hc_chart(type = chart_type) %>%
      hc_xAxis(categories = xAxis_months) %>%
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
          marker = list(enabled = FALSE, symbol = "circle") # Marker für Linien entfernen
        ),
        column = list(pointPadding = 0.2) # pointPadding gesetzt, dataLabels entfernt
      )
    
    # Hinzufügen der Serien
    for (jahr in rev(unique(data$Datum_Jahr))) {
      jahr_data <- data %>% filter(Datum_Jahr == jahr)
      
      hc <- hc %>%
        hc_add_series(
          jahr_data,
          type = chart_type,
          hcaes(x = Monat, y = `Anzahl Logiernächte`),
          name = as.character(jahr)
        )
    }
    
    hc <- hc %>%
      hc_colors(colors_plots) %>%
      hc_credits(enabled = TRUE, text = paste0("Quelle: ", quelle)) %>%
      hc_exporting(enabled = TRUE) %>%
      hc_add_theme(stata_theme)
  })
  
  # Line Plot 2 Monat:
  output$linePlot2_Monat <- renderHighchart({
    # Daten vorab filtern und Gruppengröße berechnen
    data <- data_monat_zimmer()
    group_counts <- data %>%
      group_by(Datum_Jahr) %>%
      summarise(count = n()) %>%
      ungroup()
    
    # Prüfen, ob nur ein Monat ausgewählt ist
    unique_months <- unique(data$Monat)
    is_single_month <- length(unique_months) == 1
    
    # Diagrammtyp dynamisch setzen
    chart_type <- if (is_single_month) "column" else "line"
    
    month_mapping <- c(
      "1" = "Jan", "2" = "Feb", "3" = "Mär", "4" = "Apr", "5" = "Mai", "6" = "Jun", 
      "7" = "Jul", "8" = "Aug", "9" = "Sep", "10" = "Okt", "11" = "Nov", "12" = "Dez"
    )
    
    # Um die Monatsnamen für die x-Achse zu holen
    xAxis_months <- month_mapping[as.character(unique_months)]
    
    hc <- highchart() %>%
      hc_chart(type = chart_type) %>%
      hc_xAxis(categories = xAxis_months) %>%
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
          marker = list(enabled = FALSE, symbol = "circle") # Marker für Linien entfernen
        ),
        column = list(pointPadding = 0.2) # pointPadding gesetzt, dataLabels entfernt
      )
    
    # Hinzufügen der Serien
    for (jahr in rev(unique(data$Datum_Jahr))) {
      jahr_data <- data %>% filter(Datum_Jahr == jahr)
      
      hc <- hc %>%
        hc_add_series(
          jahr_data,
          type = chart_type,
          hcaes(x = Monat, y = Zimmerauslastung),
          name = as.character(jahr),
          marker = list(enabled = (group_counts %>% filter(Datum_Jahr == jahr) %>% pull(count) == 1), symbol = "circle")
        )
    }
    
    hc <- hc %>%
      hc_colors(colors_plots) %>%
      hc_credits(enabled = TRUE, text = paste0("Quelle: ", quelle)) %>%
      hc_exporting(enabled = TRUE) %>%
      hc_add_theme(stata_theme)
  })
  
  # Line Plot 1 Tag:
  output$linePlot1_Tag <- renderHighchart({
    
    data <- data_tag_aufenthalt() %>%
      mutate(
        Datum_Monat_Tag = format(as.Date(Datum_Monat_Tag, format="%m-%d"), "%e. %b")
      )
    
    group_counts <- data %>%
      group_by(Datum_Jahr) %>%
      summarise(count = n()) %>%
      ungroup()
    
    # Prüfen, ob nur ein Tag ausgewählt ist
    unique_days <- unique(data$Datum_Monat_Tag)
    is_single_day <- length(unique_days) == 1
    
    # Diagrammtyp dynamisch setzen
    chart_type <- if (is_single_day) "column" else "line"
    
    hc <- highchart() %>%
      hc_chart(type = chart_type) %>%
      hc_xAxis(
        if (is_single_day) {
          list(categories = list(unique(data$Datum_Monat_Tag)))
        } else {
          list(categories = unique(data$Datum_Monat_Tag))
        }
      ) %>%  
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
          marker = list(enabled = FALSE, symbol = "circle") # Marker nur bei "line" deaktivieren
        ),
        column = list(pointPadding = 0.2) # Falls "column" gewählt wird
      )
    
    # Hinzufügen der Serien mit dynamischem Typ
    for (jahr in unique(data$Datum_Jahr)) {
      jahr_data <- data %>% filter(Datum_Jahr == jahr)
      
      hc <- hc %>%
        hc_add_series(
          jahr_data,
          type = chart_type,
          hcaes(x = Datum_Monat_Tag, y = `Anzahl Logiernächte`), 
          name = as.character(jahr)
        )
    }
    
    hc <- hc %>%
      hc_colors(colors_plots) %>%
      hc_credits(enabled = TRUE, text = paste0("Quelle: ", quelle)) %>%
      hc_exporting(enabled = TRUE) %>%
      hc_add_theme(stata_theme)
  })
  
  # Line Plot 2 Tag:
  output$linePlot2_Tag <- renderHighchart({
    # Daten vorab filtern und Gruppengröße berechnen
    data <- data_tag_zimmer() %>%
      mutate(
        Datum_Monat_Tag = format(as.Date(Datum_Monat_Tag, format="%m-%d"), "%e. %b")
      )
    
    group_counts <- data %>%
      group_by(Datum_Jahr) %>%
      summarise(count = n()) %>%
      ungroup()
    
    # Überprüfen, ob nur ein Tag ausgewählt ist
    is_single_day <- length(unique(data$Datum_Monat_Tag)) == 1
    
    # Dynamischen Diagrammtyp festlegen
    chart_type <- ifelse(is_single_day, "column", "line")
    
    hc <- highchart() %>%
      hc_chart(type = chart_type) %>%
      hc_xAxis(
        if (is_single_day) {
          list(categories = list(unique(data$Datum_Monat_Tag)))
        } else {
          list(categories = unique(data$Datum_Monat_Tag))
        }
      ) %>% 
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
        series = list(
          marker = list(enabled = FALSE, symbol = "circle")  # Marker deaktivieren
        ),
        column = list(pointPadding = 0.2)
      )
    
    # Hinzufügen der Serien
    for (jahr in unique(data$Datum_Jahr)) {
      jahr_data <- data %>% filter(Datum_Jahr == jahr)
      hc <- hc %>%
        hc_add_series(
          jahr_data,
          type = chart_type,
          hcaes(x = Datum_Monat, y = Zimmerauslastung),
          name = as.character(jahr)
        )
    }
    
    hc <- hc %>%
      hc_colors(colors_plots) %>%
      hc_credits(enabled = TRUE, text = paste0("Quelle: ", quelle)) %>%
      hc_exporting(enabled = TRUE) %>%
      hc_add_theme(stata_theme)
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
        pageLength = 12,
        lengthChange = FALSE,
        searching = FALSE,
        info = FALSE,
        paging = TRUE,
        dom = 'frtBip',
        buttons = list(
          list(
            extend = 'csv',
            # text = '<i class="fa-solid fa-download"></i> csv',
            text = HTML(paste0(bs_icon("download", lib = "font-awesome"), " csv")),
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
            text = HTML(paste0(bs_icon("download", lib = "font-awesome"), " xlsx")),
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
          list(visible = FALSE, targets = c(2,3))
        )
      )
      # , filter = 'top'  # Enables column filters
    ) %>%
      # Apply thousands separator only for table display
      formatStyle(
        columns = c('Anzahl Ankünfte', 'Anzahl Logiernächte'),
        `text-align` = 'right'
      ) %>%
      formatStyle(
        columns = c('Aufenthaltsdauer (in Tagen)'),
        `text-align` = 'right'
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
        pageLength = 12,
        lengthChange = FALSE,
        searching = FALSE,
        info = FALSE,
        paging = TRUE,
        dom = 'frtBip',
        buttons = list(
          list(
            extend = 'csv',
            # text = '<i class="fa-solid fa-download"></i> csv',
            text = HTML(paste0(bs_icon("download", lib = "font-awesome"), " csv")),
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
            text = HTML(paste0(bs_icon("download", lib = "font-awesome"), " xlsx")),
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
          list(visible = FALSE, targets = c(2))
        )
      )
      # ,
      # filter = 'top'  # Enables column filters
    ) %>%
      # Apply thousands separator only for table display
      formatStyle(
        columns = c('Verfügbare Zimmer', 'Belegte Zimmer'),
        `text-align` = 'right'
      ) %>%
      formatStyle(
        columns = c('Zimmerauslastung (in %)'),
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
        pageLength = 12,
        lengthChange = FALSE,
        searching = F,
        info = FALSE,
        paging = T,
        dom = 'frtBip',
        buttons = list(
          list(            
            extend = 'csv',
            text = HTML(paste0(bs_icon("download", lib = "font-awesome"), " csv")),
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
            text = HTML(paste0(bs_icon("download", lib = "font-awesome"), " xlsx")),
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
          list(width = "100px", targets = 0)
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
        pageLength = 12,
        lengthChange = FALSE,
        searching = F,
        info = FALSE,
        paging = T,
        dom = 'frtBip',
        buttons = list(
          list(
            extend = 'csv',
            # text = '<i class="fa-solid fa-download"></i> csv',
            text = HTML(paste0(bs_icon("download", lib = "font-awesome"), " csv")),
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
            text = HTML(paste0(bs_icon("download", lib = "font-awesome"), " xlsx")),
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
          list(width = "100px", targets = 0)
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
    tagList(
      tags$style(HTML("
      .accordion-button {
        background-color: #f8f9fa !important;
        color: #333 !important;
        font-weight: bold;
        border: none;
        box-shadow: none;
        transition: all 0.3s ease-in-out;
      }
      
      .accordion-button:hover {
        background-color: #e2e6ea !important;
        color: #000 !important;
      }
      
      .accordion-button:not(.collapsed) {
        background-color: #d1d8e0 !important;
        color: #333 !important;
      }

      .accordion-body {
        background-color: #ffffff;
        color: #555;
        font-size: 14px;
        padding: 15px;
        border-top: 1px solid #ddd;
      }

      .accordion {
        border-radius: 10px;
        overflow: hidden;
      }
    ")),
      
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
