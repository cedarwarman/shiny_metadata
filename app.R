library(shiny)
library(readxl)
library(bslib)
library(DT)

# Loading the accession metadata
accessions <- read_excel(file.path(getwd(), "data", "accessions.xlsx"))

ui <- bootstrapPage(
  div(class = "container-fluid",
    div(class = "row justify-content-center",
      align = "center",
      div(class = "col-xl-4",
          DTOutput("accessions_table", width = "100%")
      )
    )
  )  
)

# ui <- fluidPage(
#   dataTableOutput("accessions_table")
# )

server <- function(input, output, session) {
  theme  = bs_theme(version = 5)
  output$accessions_table <- renderDT(accessions, options = list(scrollX = TRUE,
                                                                 scrollY = "500px",
                                                                 pageLength = 500,
                                                                 # autoWidth = TRUE,
                                                                 fixedHeader = TRUE)) 
}

shinyApp(ui, server)