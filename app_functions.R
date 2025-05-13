### Tourismus #######################################################################################
####################### Dashboard Functions #########################################################
#####################################################################################################

# function for thousands separator and decimal comma:
add_thousand_separator <- function(x, digits) {
  formatted_number <- formatC(x, format = "f", big.mark = "\u2009", decimal.mark = ",", digits = digits)
  return(formatted_number)
}

# format search engine:
options(DT.options = list(
  language = list(
    search = 'Suche:',
    lengthMenu = "Zeige _MENU_ Zeilen",
    info = "Zeigt _START_ bis _END_ von _TOTAL_ Zeilen",
    infoEmpty = "Zeigt 0 bis 0 von 0 Zeilen",
    thousands = "\u2009",  # Thousands separator (narrow space)
    decimal = ",",  # Decimal separator (comma)
    paginate = list("first" = "Erste", "last" = "Letzte", "next" = "Nächste", "previous" = "Vorherige")
  )
))

#StatA-theme mit wichtigsten formalen Vorgaben für StatA-Grafiken    
stata_theme <- hc_theme(
  chart = list(
    spacingBottom = 30,
    backgroundColor = "white",
    style = list(
      fontFamily = "Inter"
    ),
    events = list(
      load = JS(
        "function () {
          this.credits.element.onclick = function () { };

          // For top-left legends with no x defined: move legend to x position of first yAxis
          if (this['legend']['options']['align'] == 'left' && this['legend']['options']['verticalAlign'] == 'top' && this['legend']['options']['x'] == 0) {
            this.update(
              {
                legend: {
                  x: this.yAxis[0].left - this.spacingBox.x - this.legend.padding
                }
              }
            );
          }
        }"
      )
    )
  ),
  lang = list(
    decimalPoint = ",",       # Set comma as the decimal separator
    thousandsSep = "\u2009"   # Set \u2009 as the thousands separator
  ),
  title = list(
    align = "left",
    style = list(
      color = "#000000",
      fontWeight = "normal",
      fontFamily = "Inter"
    )
  ),
  subtitle = list(
    text = "",
    align = "left",
    style = list(
      fontWeight = "normal",
      color = "#000000",
      fontFamily = "Inter"
    )
  ),
  plotOptions = list(
    series = list(
      borderWidth = 0.0000001,
      dataLabels = list(
        style = list(
          fontFamily = "Inter",
          fontSize = "10px"
        )
      )
    )
  ),
  xAxis = list(
    lineColor = '#B9CFD7',
    lineWidth = 0.5,
    tickLength = 0,
    uniqueNames = TRUE,
    title = list(
      style = list(
        color = "#000000",
        fontFamily = "Inter"
      )
    ),
    labels = list(
      style = list(
        fontSize = "11px",
        color = "#000000",
        fontFamily = "Inter"
      )
    )
  ),
  yAxis = list(
    min = 0,
    reversedStacks = FALSE,
    gridLineWidth = 0.5,
    gridLineColor = '#B9CFD7',
    lineColor = '#B9CFD7',
    title = list(
      style = list(
        color = "#000000",
        fontFamily = "Inter"
      ),
      text = NULL
    ),
    labels = list(
      format = "{value:,.0f}",
      style = list(
        color = "#000000",
        fontFamily = "Inter"
      )
    )
  ),
  legend = list(
    symbolRadius = 0,
    itemStyle = list(
      fontWeight = "normal",
      fontFamily = "Inter"
    )
  ),
  credits = list(
    enabled = TRUE,
    style = list(
      color = "#000000",
      fontSize = "12px",
      cursor = "default",
      fontFamily = "Inter"
    ),
    position = list(
      align = "left",
      verticalAlign = "bottom",
      x = 10
    )
  ),
  exporting = list(
    buttons = list(
      contextButton = list(
        menuItems = list(
          list(text = '<span style="font-weight: bold; font-family: Inter;">Grafik herunterladen</span>'),
          list(
            text = "Bild - PNG",
            onclick = JS("function () { this.exportChart({ type: 'image/png' }); }")
          ),
          list(
            text = "Bild - JPEG",
            onclick = JS("function () { this.exportChart({ type: 'image/jpeg' }); }")
          ),
          list(
            text = "Bild - PDF",
            onclick = JS("function () { this.exportChart({ type: 'application/pdf' }); }")
          ),
          list(
            text = "Bild - SVG",
            onclick = JS("function () { this.exportChart({ type: 'image/svg+xml' }); }")
          )
        )
      )
    )
  )
)
