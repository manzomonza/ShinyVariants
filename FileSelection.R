library(shiny)
library(ggplot2)

# Sample data frame with sample information and associated files
samples <- data.frame(
  Sample = c('Y123', 'Y124', 'X432', 'Y234'),
  File = c('Y123_foi.tsv', 'Y124_foi.tsv', 'X432_foi.tsv', 'Y234_foi.tsv')
)

# UI
ui <- fluidPage(
  titlePanel("Sample Analysis"),
  tabsetPanel(
    tabPanel("List Samples",
             textInput("sample_input", "Enter Sample ID (regex)"),
             verbatimTextOutput("sample_list")),
    tabPanel("Sample Details",
             selectInput("selected_sample", "Select Sample", choices = NULL),
             verbatimTextOutput("selected_sample_info"),
             actionButton("run_code_button", "Run Code on Associated File"),
             plotOutput("sample_graph"))
  )
)

# Server
server <- function(input, output, session) {

  # Populate the dropdown with matching samples
  observe({
    matching_samples <- samples$Sample[grep(input$sample_input, samples$Sample)]
    updateSelectInput(session, "selected_sample", choices = matching_samples)
  })

  # List matching samples
  output$sample_list <- renderPrint({
    req(input$sample_input)
    matching_samples <- samples$Sample[grep(input$sample_input, samples$Sample)]
    if (length(matching_samples) > 0) {
      paste("Matching Samples:", paste(matching_samples, collapse = ", "))
    } else {
      "No matching samples found."
    }
  })

  # Display selected sample information
  output$selected_sample_info <- renderPrint({
    req(input$selected_sample)
    selected_sample <- samples[samples$Sample == input$selected_sample, ]
    if (nrow(selected_sample) > 0) {
      paste("Selected Sample Information:", "\n",
            "Sample ID:", selected_sample$Sample, "\n",
            "Associated File:", selected_sample$File)
    } else {
      "Sample not found."
    }
  })

  # Display a sample-specific graph
  output$sample_graph <- renderPlot({
    req(input$selected_sample)
    selected_sample <- samples[samples$Sample == input$selected_sample, ]
    if (nrow(selected_sample) > 0) {
      # For this example, we'll create a simple scatter plot
      ggplot(data.frame(x = rnorm(50), y = rnorm(50)), aes(x, y)) +
        geom_point() +
        labs(title = paste("Graph for Sample:", input$selected_sample))
    }
  })
}

# Run the app
shinyApp(ui, server)

