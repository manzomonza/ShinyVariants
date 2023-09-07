# Load packages
library(shiny)
library(shinydashboard)
library(DT)

snvs = list.files(path = "sample_database", pattern = "parsed_snv.tsv", recursive = TRUE, full.names = TRUE)

df = data.frame(ID = c("Y1234", "Y321"),
                paths = snvs)
classifications = c("report", "do not report")
classval = c("report",'do_not_report')

add_report_radio = function(input_table){
  m = matrix(
    character(0), nrow = nrow(input_table), ncol = 2,
    dimnames = list(input_table$gene, classifications)
  )

  for (i in seq_len(nrow(input_table))) {
    for (j in seq_along(classifications)) {
      class_name = classifications[j]
      default_val = ifelse(class_name == "do not report", "checked", "")
      m[i, j] = sprintf(
        '<input type="radio" name="%s" value="%s" %s/>',
        input_table$gene[i], classval[j], default_val
      )
    }
  }
  snv = cbind(input_table,m)
  return(snv)
}



# Define UI
ui <- dashboardPage(
  # Header
  dashboardHeader(title = "Sample Dashboard"),
  # Sidebar
  dashboardSidebar(
    sidebarMenu(
      # Menu item for selecting sample
      menuItem("Select Sample", tabName = "sample", icon = icon("list")),
      # Menu item for displaying content
      menuItem("Display Content", icon = icon("eye"),
               # Submenu items for different tabs
               menuSubItem("Plot", tabName = "plot"),
               menuSubItem("Table", tabName = "table"),
               menuSubItem("Text", tabName = "text"),
               menuSubItem("Foo", tabName = "foo")
      )
    )
  ),
  # Body
  dashboardBody(
    tabItems(
      # Tab item for selecting sample
      tabItem(tabName = "sample",
              # Select input for choosing sample
              selectInput("sample", "Choose a sample",
                          choices = NULL) # The choices will be updated in the server part
      ),
      # Tab item for displaying plot
      tabItem(tabName = "plot",
              # Plot output for showing sample plot
              plotOutput("sample_plot")
      ),
      # Tab item for displaying table
      tabItem(tabName = "table",
              # Table output for showing sample table
              tableOutput("sample_table")
      ),
      # Tab item for displaying text
      tabItem(tabName = "text",
              # Text output for showing sample text
              textOutput("sample_text")

      ),
      tabItem(tabName = "foo",
              # Text output for showing sample text
              DTOutput("foo"),
              # A button to download the table as a tsv file
              downloadButton("download", "Download the table")
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {

  # Read the table from a CSV file into a dataframe
  # You can provide the path to your CSV file here
  #df <- read.csv("samples.csv")

  # Update the select input with the choices from the dataframe
  updateSelectInput(session,
                    inputId = "sample",
                    choices = setNames(df$ID, df$Name))

  # Render plot output based on selected sample
  output$sample_plot <- renderPlot({
    selected_sample = input$sample
    source("./shinyR/renderPlot_func.R", local = TRUE)
  })
  # Render table output based on selected sample
  output$sample_table <- renderTable({
    # Get selected sample ID
    selected_sample <- input$sample

    # Generate sample table using selected sample ID as column values
    table <- data.frame(Name = c("Alice", "Bob", "Charlie"),
                        Value = selected_sample)
    table
  })

  # Reactive expression for the data
  output$foo = DT::renderDataTable(
    snv, escape = FALSE, selection = 'none', server = FALSE,
    options = list(dom = 't', paging = FALSE, ordering = FALSE),
    callback = JS("table.rows().every(function(i, tab, row) {
          var $this = $(this.node());
          $this.attr('id', this.data()[0]);
          $this.addClass('shiny-input-radiogroup');
        });
        Shiny.unbindAll(table.table().node());
        Shiny.bindAll(table.table().node());")
  )

  selected_data = reactive({
    varvals = rep(NA, length(snv$gene))
    varvals = sapply(snv$gene, function(i) input[[i]])
    tibble::tibble(Variants = snv$gene,
                   values = varvals)
  })

  output$sel = renderTable({
    selected_data()
  })

  output$download = downloadHandler(
    filename = function() {
      paste0('selected_rows.csv')
    },
    content = function(file) {
      selected_data = selected_data()
      selected_data$ID = "TEST"
      write.csv(selected_data, file, row.names = FALSE)
    }
  )




  # Render text output based on selected sample
  output$sample_text <- renderText({
    # Get selected sample ID
    selected_sample <- input$sample

    # Generate sample text using selected sample ID as part of the sentence
    text <- paste("This is a sample text for", selected_sample, ".")
    text
  })
}

# Run the application
shinyApp(ui = ui, server = server)
