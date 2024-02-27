library(shiny)
library(jsonlite)
library(ggplot2)
library(shinythemes)
library(httr)

# Simple sidebar / main panel layout with theme
ui <- fluidPage(theme=shinytheme("slate"),
                titlePanel("Utforsk WHO sitt indikator-API"),
                
                sidebarLayout(
                  sidebarPanel(
                    p("Denne webappen lar deg enkelt sjekke ulike indikatorer fra WHO sitt offentlige API."),
                    p("Så langt er det støtte for helsepersonell pr 100.000 innbyggere, andel MRSA og ESBL-produserende E.Coli i blodinfeksjoner, andel hjem med tilgang til enkel håndvask, samt andel av befolkningen under den internasjonale fattigdomsgrensen."),
                    p("Formålet med appen er først og fremst at jeg skulle bli bedre kjent med Shiny-rammeverket."),
                    p("Ved manglende data fra APIet, feks er det ikke registrert andel hjem med håndvask i Norge, vil det dukke opp en feilmelding."),
                    p("Under kan du velge hvilket land du vil sjekke indikatorene for. All koden finner du på GitHub i lenken nederst. "),
                    br(),
                    uiOutput("countrySlider"),
                    tags$a(href="https://github.com/olavaga/who_indikatorer", "GitHub repository")
                  ),
                  mainPanel(
                    fluidRow(
                      column(6, plotOutput(outputId = "healthPlot"),
                             br(),
                             plotOutput(outputId = "hygienePlot")),
                      column(6, plotOutput(outputId = "amrPlot"),
                             br(),
                             plotOutput(outputId = "financialRiskPlot"))
                    )
                  )
                )
)

# Add filter condition for country to an endpoint
filterCountry <- \(endpoint, country) paste0(endpoint, 
                                             "?$filter=SpatialDim%20eq%20%27", 
                                             country,
                                             "%27")

server <- function(input, output) {
  # Get list of countries available in WHO API
  response.countries <- reactive({
    fromJSON("https://ghoapi.azureedge.net/api/DIMENSION/COUNTRY/DimensionValues/")
  })
  
  df.countries <- reactive({
    response <- response.countries()
    as.data.frame(response$value)
  })
  
  # Create a named vector of countries, i.e Norway maps to NOR
  dropdown <- reactive({
    vec = df.countries()$Code
    names(vec) <- df.countries()$Title
    vec
  })
  
  # Create slider and default to Nigeria, because it has all datasets in demo
  output$countrySlider <- renderUI({
    selectInput("countrySelector","Velg land:", 
                choices=dropdown(), 
                selected = "NGA")
  })
  
  # Get handwashing data
  response.hygiene <- reactive({
    endpoint <- "https://ghoapi.azureedge.net/api/WSH_HYGIENE_BASIC"
    fromJSON(filterCountry(endpoint, input$countrySelector))
  })
  
  # Cast the response to dataframe when available
  df.hygiene <- reactive({
    response <- response.hygiene()
    as.data.frame(response$value)
  })
  
  # Map different categories in dim1 to understandable words
  hygiene.color <- function() {
    sapply(df.hygiene()$Dim1, 
           \(x) switch(x,
                       "RESIDENCEAREATYPE_URB" = "Urban",
                       "RESIDENCEAREATYPE_RUR" = "Rural",
                       "RESIDENCEAREATYPE_TOTL" = "Total"))
  }
  
  # Get data for doctors and nurses per 100.000
  response.doctors <- reactive({
    endpoint <- "https://ghoapi.azureedge.net/api/HWF_0001"
    fromJSON(filterCountry(endpoint, input$countrySelector))
  })
  
  df.doctors <- reactive({
    response <- response.doctors()
    as.data.frame(response$value)
  })
  
  response.nurses <- reactive({
    endpoint <- "https://ghoapi.azureedge.net/api/HWF_0006"
    fromJSON(filterCountry(endpoint, input$countrySelector))
  })
  
  df.nurses <- reactive({
    response <- response.nurses()
    as.data.frame(response$value)
  })
  
  # Combine doctors and nurses to single dataframe
  df.healthcare <- reactive({
    rbind(df.nurses(), df.doctors())
  })
  
  # Translates indicators into Norwegian for legend labels
  df.healthcare.profession <- function() {
    sapply(df.healthcare()$IndicatorCode,
           \(x) switch(x,
                       "HWF_0001" = "Leger",
                       "HWF_0006" = "Sykepleiere og jordmødre"))
  }
  
  # Get poverty data
  response.poverty <- reactive({
    endpoint <- "https://ghoapi.azureedge.net/api/SI_POV_DAY1"
    fromJSON(filterCountry(endpoint, input$countrySelector))
  })
  
  df.poverty <- reactive({
    response <- response.poverty()
    as.data.frame(response$value)
  })
  
  # Get E.coli and MRSA blood infection data
  response.ecoli <- reactive({
    endpoint <- "https://ghoapi.azureedge.net/api/AMR_INFECT_ECOLI"
    fromJSON(filterCountry(endpoint, input$countrySelector))
  })
  
  response.mrsa <- reactive({
    endpoint <- "https://ghoapi.azureedge.net/api/AMR_INFECT_MRSA"
    fromJSON(filterCountry(endpoint, input$countrySelector))
  })
  
  # Combine both indicators as one data frame
  df.amr <- reactive({
    response.ecoli <- response.ecoli()
    response.mrsa <- response.mrsa()
    df.ecoli <- as.data.frame(response.ecoli$value)
    df.mrsa <- as.data.frame(response.mrsa$value)
    rbind(df.ecoli, df.mrsa)
  })
  
  # Map indicator names to understandable words for legend
  amr.color <- function() {
    sapply(df.amr()$IndicatorCode,
           \(x) switch(x,
                       "AMR_INFECT_ECOLI" = "ESBL E.coli",
                       "AMR_INFECT_MRSA" = "MRSA"))
  }
  
  # Handwashing plot
  output$hygienePlot <- renderPlot({
    ggplot(df.hygiene()) + 
      geom_line(aes(x = TimeDim, y = NumericValue, color = hygiene.color())) + 
      labs(title = "Hjem med tilgang til enkel håndvask",
           x = "År",
           y = "%",
           color = "Boligområde") +
      theme_classic()
  })
  
  # Healthcare personell per 100.000 plot
  output$healthPlot <- renderPlot({
    ggplot(df.healthcare()) + 
      geom_point(aes(x = TimeDim, y = NumericValue, color=df.healthcare.profession())) + 
      labs(title = "Helsepersonell per 100.000 innbyggere",
           x = "År",
           y = "pr 100.000",
           color = "Yrkesgruppe") +
      theme_classic()
    
  })
  
  # Joint sepsis E.Coli and MRSA plot
  output$amrPlot <- renderPlot({
    ggplot(df.amr()) + 
      geom_point(aes(x = TimeDim, y = NumericValue, color=amr.color())) + 
      labs(title = "Resistens i blodstrømsinfeksjoner",
           x = "År",
           y = "%",
           color = "Bakterie") +
      theme_classic()
    
  })
  
  # Plots % of population with daily income below 1.9 dollars
  output$financialRiskPlot <- renderPlot({
    ggplot(df.poverty()) + 
      geom_point(aes(x = TimeDim, y = NumericValue)) + 
      ggtitle("Inntekt under 1.9 dollar per dag") +
      xlab("År") + 
      ylab("Andel") +
      theme_classic()
    
  })
}

shinyApp(ui = ui, server = server)
