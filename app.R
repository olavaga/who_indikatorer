library(shiny)
library(jsonlite)
library(ggplot2)
library(shinythemes)
library(httr)

ui <- fluidPage(theme=shinytheme("slate"),
    titlePanel("Utforsk WHO sitt indikator-API"),

    sidebarLayout(
        sidebarPanel(
          p("Denne webappen lar deg enkelt sjekke ulike indikatorer fra WHO sitt offentlige API."),
          br(),
          uiOutput("countrySlider")
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

server <- function(input, output) {
    response.countries <- reactive({
      fromJSON("https://ghoapi.azureedge.net/api/DIMENSION/COUNTRY/DimensionValues/")
    })
    
    df.countries <- reactive({
      response <- response.countries()
      as.data.frame(response$value)
    })
    
    dropdown <- reactive({
      vec = df.countries()$Code
      names(vec) <- df.countries()$Title
      vec
    })
    
    output$countrySlider <- renderUI({
      print("rendering countrySlider")
      selectInput("countrySelector","Velg land:", 
                  choices=dropdown(), 
                  selected = "NGA")
    })
    
    response.hygiene <- reactive({
      endpoint <- "https://ghoapi.azureedge.net/api/WSH_HYGIENE_BASIC"
      fromJSON(paste0(endpoint, "?$filter=SpatialDim%20eq%20%27", input$countrySelector,"%27"))
    })
    
    df.hygiene <- reactive({
      response <- response.hygiene()
      as.data.frame(response$value)
    })
    
    hygiene.color <- function() {
      sapply(df.hygiene()$Dim1, 
             \(x) switch(x,
                         "RESIDENCEAREATYPE_URB" = "Urban",
                         "RESIDENCEAREATYPE_RUR" = "Rural",
                         "RESIDENCEAREATYPE_TOTL" = "Total"))
    }
    
    response.doctors <- reactive({
      endpoint <- "https://ghoapi.azureedge.net/api/HWF_0001"
      fromJSON(paste0(endpoint, "?$filter=SpatialDim%20eq%20%27", input$countrySelector,"%27"))
    })
    
    df.doctors <- reactive({
      response <- response.doctors()
      as.data.frame(response$value)
    })
    
    response.nurses <- reactive({
      endpoint <- "https://ghoapi.azureedge.net/api/HWF_0006"
      fromJSON(paste0(endpoint, "?$filter=SpatialDim%20eq%20%27", input$countrySelector,"%27"))
    })
    
    df.nurses <- reactive({
      response <- response.nurses()
      as.data.frame(response$value)
    })
    
    df.healthcare <- reactive({
      rbind(df.nurses(), df.doctors())
    })
    
    df.healthcare.profession <- function() {
      sapply(df.healthcare()$IndicatorCode,
             \(x) switch(x,
                         "HWF_0001" = "Leger",
                         "HWF_0006" = "Sykepleiere og jordmødre"))
    }
    
    response.poverty <- reactive({
      endpoint <- "https://ghoapi.azureedge.net/api/SI_POV_DAY1"
      fromJSON(paste0(endpoint, "?$filter=SpatialDim%20eq%20%27", input$countrySelector,"%27"))
    })
    
    df.poverty <- reactive({
      response <- response.poverty()
      as.data.frame(response$value)
    })
    
    response.ecoli <- reactive({
      endpoint <- "https://ghoapi.azureedge.net/api/AMR_INFECT_ECOLI"
      fromJSON(paste0(endpoint, "?$filter=SpatialDim%20eq%20%27", input$countrySelector,"%27"))
    })
    
    response.mrsa <- reactive({
      endpoint <- "https://ghoapi.azureedge.net/api/AMR_INFECT_MRSA"
      fromJSON(paste0(endpoint, "?$filter=SpatialDim%20eq%20%27", input$countrySelector,"%27"))
    })
    
    df.amr <- reactive({
      response.ecoli <- response.ecoli()
      response.mrsa <- response.mrsa()
      df.ecoli <- as.data.frame(response.ecoli$value)
      df.mrsa <- as.data.frame(response.mrsa$value)
      rbind(df.ecoli, df.mrsa)
    })
    
    amr.color <- function() {
      sapply(df.amr()$IndicatorCode,
             \(x) switch(x,
                         "AMR_INFECT_ECOLI" = "ESBL E.coli",
                         "AMR_INFECT_MRSA" = "MRSA"))
    }
    
    output$hygienePlot <- renderPlot({
      ggplot(df.hygiene()) + 
        geom_line(aes(x = TimeDim, y = NumericValue, color = hygiene.color())) + 
        labs(title = "Hjem med tilgang til enkel håndvask",
             x = "År",
             y = "%",
             color = "Boligområde") +
        theme_classic()
    })
    
    output$healthPlot <- renderPlot({
      ggplot(df.healthcare()) + 
        geom_point(aes(x = TimeDim, y = NumericValue, color=df.healthcare.profession())) + 
        labs(title = "Helsepersonell per 100.000 innbyggere",
             x = "År",
             y = "pr 100.000",
             color = "Yrkesgruppe") +
        theme_classic()
      
    })
    
    output$amrPlot <- renderPlot({
      ggplot(df.amr()) + 
        geom_point(aes(x = TimeDim, y = NumericValue, color=amr.color())) + 
        labs(title = "Resistens i blodstrømsinfeksjoner",
             x = "År",
             y = "%",
             color = "Bakterie") +
        theme_classic()
      
    })
    
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
