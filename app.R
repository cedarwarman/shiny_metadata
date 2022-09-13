library(shiny)
library(readxl)
library(bslib)
library(DT)
library(shinythemes)
library(ggplot2)
library(plotly)

# Loading the accession metadata
accessions <- read_excel(file.path(getwd(), "data", "accessions.xlsx"))

# Loading the flower measurements for the flower plot
flower_measurements <- read.table(file.path(getwd(), "data", "flower_measurements.tsv"),
                                  header = TRUE)


# Functions ---------------------------------------------------------------
make_flower_plot <- function(plot_type, flower_df, row_selection) {
  # Putting the theme here to make it easier to edit
  plot_theme <- theme(
    axis.title = element_text(size = 12, face = 'bold', color = 'white'),
    axis.text = element_text(size = 10, face = 'bold', color = 'white'),
    axis.text.x = element_blank(),
    plot.title = element_text(size = 18, 
                              face = 'bold', 
                              margin = margin(0, 0, 5, 0), 
                              color = 'white'),
    axis.title.x = element_blank(),
    panel.border = element_blank(),
    panel.background = element_rect(fill = '#060606'), 
    plot.background = element_rect(fill = '#060606'), 
    axis.line = element_line(size = 1, color = 'white'),
    axis.ticks = element_line(size = 1, color = 'white'),
    axis.ticks.x = element_blank(),
    axis.ticks.length = unit(3, 'pt'),
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, 'cm'),
    panel.grid = element_blank(),
    legend.position = 'none'
  )  
  
  # Same with the fill colors
  fill_values = c('white', '#f542e3')
    
  if (plot_type == "ratio") {   
    if (length(row_selection)) {
      # Make a new column for if it gets highlighted or not
      selected_accessions <- accessions$name_CW[row_selection]
      flower_df$row_selected <- NA
      flower_df$row_selected[flower_df$accession_id %in% selected_accessions] <- "selected"
      flower_df$row_selected[is.na(flower_df$row_selected)] <- "not_selected"
      
      # Make the plot
      output_plot <- ggplot(data = flower_df,
                            aes(x = reorder(accession_id, anther_over_pistil, median),
                            y = anther_over_pistil)) +
        geom_boxplot(aes(color = row_selected, fill = row_selected),
                     size = 0.2,
                     # plotly ignores the outlier aesthetics. A partial fix can be 
                     # found here: https://github.com/plotly/plotly.R/issues/1114
                     outlier.size = 2,
                     outlier.shape = 2) +
        scale_fill_manual(values = fill_values) +
        scale_color_manual(values = fill_values) +
        labs(title = "Anther and pistil lengths by accession",
             y = "Ratio of AL/PL") +
        scale_y_continuous(breaks = seq(0.7, 1.4, 0.1),
                           labels = seq(0.7, 1.4, 0.1),
                           limits = c(0.7, 1.4)) +
        theme_bw() +
        plot_theme
      
    } else {
      output_plot <- ggplot(data = flower_df,
                            aes(x = reorder(accession_id, anther_over_pistil, median),
                            y = anther_over_pistil)) +
        geom_boxplot(color = 'white',
                     size = 0.2,
                     # plotly ignores the outlier aesthetics. A partial fix can be 
                     # found here: https://github.com/plotly/plotly.R/issues/1114
                     outlier.size = 2,
                     outlier.shape = 2) +
        labs(title = "Anther and pistil lengths by accession",
             y = "Ratio of AL/PL") +
        scale_y_continuous(breaks = seq(0.7, 1.4, 0.1),
                           labels = seq(0.7, 1.4, 0.1),
                           limits = c(0.7, 1.4)) +
        theme_bw() +
        plot_theme
    }
  } else if (plot_type == "anther") {
    if (length(row_selection)) {
      # Make a new column for if it gets highlighted or not
      selected_accessions <- accessions$name_CW[row_selection]
      flower_df$row_selected <- NA
      flower_df$row_selected[flower_df$accession_id %in% selected_accessions] <- "selected"
      flower_df$row_selected[is.na(flower_df$row_selected)] <- "not_selected"
      
      # Make the plot
      output_plot <- ggplot(data = flower_df,
                            aes(x = reorder(accession_id, anther_length, median),
                                y = anther_length)) +
        geom_boxplot(aes(color = row_selected, fill = row_selected),
                     size = 0.2,
                     # plotly ignores the outlier aesthetics. A partial fix can be 
                     # found here: https://github.com/plotly/plotly.R/issues/1114
                     outlier.size = 2,
                     outlier.shape = 2) +
        scale_fill_manual(values = fill_values) +
        scale_color_manual(values = fill_values) +
        labs(title = "Anther lengths by accession",
             y = "Anther length (mm)") +
        scale_y_continuous(breaks = seq(5, 13, 1),
                           labels = seq(5, 13, 1),
                           limits = c(5, 13)) +
        theme_bw() +
        plot_theme
      
    } else {
      output_plot <- ggplot(data = flower_df,
                            aes(x = reorder(accession_id, anther_length, median),
                                y = anther_length)) +
        geom_boxplot(color = 'white',
                     size = 0.2,
                     # plotly ignores the outlier aesthetics. A partial fix can be 
                     # found here: https://github.com/plotly/plotly.R/issues/1114
                     outlier.size = 2,
                     outlier.shape = 2) +
        labs(title = "Anther lengths by accession",
             y = "Anther length (mm)") +
        scale_y_continuous(breaks = seq(5, 13, 1),
                           labels = seq(5, 13, 1),
                           limits = c(5, 13)) +
        theme_bw() +
        plot_theme
    }
  } else {
    if (length(row_selection)) {
      # Make a new column for if it gets highlighted or not
      selected_accessions <- accessions$name_CW[row_selection]
      flower_df$row_selected <- NA
      flower_df$row_selected[flower_df$accession_id %in% selected_accessions] <- "selected"
      flower_df$row_selected[is.na(flower_df$row_selected)] <- "not_selected"
      
      # Make the plot
      output_plot <- ggplot(data = flower_df,
                            aes(x = reorder(accession_id, pistil_length, median),
                                y = pistil_length)) +
        geom_boxplot(aes(color = row_selected, fill = row_selected),
                     size = 0.2,
                     # plotly ignores the outlier aesthetics. A partial fix can be 
                     # found here: https://github.com/plotly/plotly.R/issues/1114
                     outlier.size = 2,
                     outlier.shape = 2) +
        scale_fill_manual(values = fill_values) +
        scale_color_manual(values = fill_values) +
        labs(title = "Pistil lengths by accession",
             y = "Pistil length (mm)") +
        scale_y_continuous(breaks = seq(5, 13, 1),
                           labels = seq(5, 13, 1),
                           limits = c(5, 13)) +
        theme_bw() +
        plot_theme
      
    } else {
      output_plot <- ggplot(data = flower_df,
                            aes(x = reorder(accession_id, pistil_length, median),
                                y = pistil_length)) +
        geom_boxplot(color = 'white',
                     size = 0.2,
                     # plotly ignores the outlier aesthetics. A partial fix can be 
                     # found here: https://github.com/plotly/plotly.R/issues/1114
                     outlier.size = 2,
                     outlier.shape = 2) +
        labs(title = "Pistil lengths by accession",
             y = "Pistil length (mm)") +
        scale_y_continuous(breaks = seq(5, 13, 1),
                           labels = seq(5, 13, 1),
                           limits = c(5, 13)) +
        theme_bw() +
        plot_theme
    }
  }
  
  output_plot <- ggplotly(output_plot) %>% config(displayModeBar = FALSE)
  
  return(output_plot)
}


ui <- bootstrapPage(
  theme  = bs_theme(version = 5,
                    bootswatch = 'cyborg',
                    primary = '#f542e3'),
  tags$style(type='text/css',
             ".table.dataTable tbody tr.active td { background-color: #f542e3 ; }
             table.dataTable { border-collapse: collapse !important; }
             .recalculating {opacity: 1.0; }"),
  tags$head(tags$link(rel = "shortcut icon", href = "favicon.ico")),
  
  div(class = "container-fluid",
    # div(class = "row justify-content-center",
    #   align = "center",
    #   div(class = "col-xl-1",
    #     checkboxInput("show_columns", "Show all columns", value = FALSE)
    #   )
    # ),
    div(class = "row justify-content-center",
      align = "center",
      div(class = "col-xl",
        h1(strong("Pollen metadata"))
      )
    ),
    div(class = "row justify-content-center",
      align = "center",
      div(class = "col-xl-6",
        # DTOutput("accessions_table", width = "100%")
        DTOutput("accessions_table")
      ),
      div(class = "col-xl-6",
        plotlyOutput("ratio_plot", height = "30vh"),
        plotlyOutput("anther_plot", height = "30vh"),
        plotlyOutput("pistil_plot", height = "30vh")
      )
    )
  )  
)

# ui <- fluidPage(
#   dataTableOutput("accessions_table")
# )

server <- function(input, output, session) {
  output$accessions_table <- renderDT(
    accessions, 
    style = "bootstrap",
    options = list(
      scrollX = TRUE,
      scrollY = "85vh",
      pageLength = 500,
      fixedHeader = TRUE, 
      dom = "ft",
      columnDefs = list(
        list(
          visible = FALSE, targets = c(3:4, 6:13, 15:18, 20:26, 32:46)
        )
      )
      # autoWidth = TRUE,
      # info = FALSE
   ))
   
  output$ratio_plot <- renderPlotly({
    make_flower_plot("ratio", flower_measurements, input$accessions_table_rows_selected)
  })
 
  output$anther_plot <- renderPlotly({
    make_flower_plot("anther", flower_measurements, input$accessions_table_rows_selected)
  })
  
  output$pistil_plot <- renderPlotly({
    make_flower_plot("pistil", flower_measurements, input$accessions_table_rows_selected)
  })
  
}

shinyApp(ui, server)