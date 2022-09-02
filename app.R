library(shiny)
library(readxl)
library(bslib)
library(DT)
library(shinythemes)

# Loading the accession metadata
accessions <- read_excel(file.path(getwd(), "data", "accessions.xlsx"))

ui <- bootstrapPage(
  theme = shinytheme("slate"),
  # themeSelector(),
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
  output$accessions_table <- renderDT(accessions, 
                                      style = "bootstrap",
                                      options = list(scrollX = TRUE,
                                                     scrollY = "600px",
                                                     pageLength = 500,
                                                     # autoWidth = TRUE,
                                                     fixedHeader = TRUE, 
                                                     dom = "ft"))
                                                     # info = FALSE))
}

shinyApp(ui, server)