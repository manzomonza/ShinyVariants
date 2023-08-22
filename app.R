library(shiny)
library(shinyjs)

ui <- fluidPage(
		  useShinyjs(),
		    navbarPage("Variant Interpretation",
		    tabPanel("IGV report",
                  			sidebarLayout(
				        sidebarPanel(
					textInput("datapath", "Let the magic begin (paste corrected path as described):"),
					actionButton("igv_execute", "Run script to generate IGV reports")
					),
					mainPanel(
					h2("Specify the directory"),
					p(""),
                                        img(src = "igv.png", height = 300, width = 650)
                                        )
					)
					),
		    )
		  )

server <- function(input, output, session) {
	        igv_script <- function(file_path) {
	            # Execute shell script on specified file path
	            system(paste("/home/ionadmin/igvreps/pipeline_igvreport.sh", file_path))
		    }

  observeEvent(
	       input$igv_execute, {
	       # Get the directory path from the text input
               dir_path <- input$datapath
               # Find all files named "prep_snv.txt" in the directory and its subdirectories
               file_paths <- list.files(path = dir_path, pattern = "prep_snv.txt", recursive = TRUE, full.names = TRUE)
               # Execute the shell script on each file found
               lapply(file_paths, igv_script)
              }
  )
}

shinyApp(ui, server)

