library(shiny)
library(readxl)
library(bslib)

# Loading the accession metadata
accessions <- read_excel(file.path(getwd(), "data", "accessions.xlsx"))

# ui <- bootstrapPage(
#   div(class = "container-fluid",
#     div(class = "row",
#       align = "center",
#       dataTableOutput("accessions_table")
#     )
#   )  
# )

ui <- fluidPage(
  dataTableOutput("accessions_table")
)

server <- function(input, output, session) {
  theme  = bs_theme(version = 5)
  output$accessions_table <- renderDataTable(accessions) 
}

shinyApp(ui, server)