library(shiny)
library(DT)


cnv = "/Users/manzo/USB/USB_Diagnostics/VariantAggregation/outputs/Y518_B2023.6176_OcaV3_output/parsed_output/parsed_cnv.tsv"
cnv = readr::read_tsv(cnv)

snv = "/Users/manzo/USB/USB_Diagnostics/VariantAnnotationModules/testing/parsed_output/Y12_B2022.54547_OCAPlus_w512_parsed_output/snv.txt"
snv = readr::read_tsv(snv)

snv = VariantAnnotationModules::amino_acid_code_3_to_1(snv)
snv$oncokb = paste0("https://www.oncokb.org/gene/", snv$gene,"/", gsub("p\\.",'', snv$protein))
snv$link <- paste0("<a href='", snv$oncokb, "' target='_blank'>", snv$protein, "</a>")
classifications = c("benign", "likely benign", "unknown", "likely pathogenic", "pathogenic", "predictive")
classval = c("benign", "likely benign", "unknown", "likely pathogenic", "pathogenic", "predictive")
# Create a generic dataframe
generic_df <- data.frame(
  dplyr::select(snv,rowid,gene, coding,protein, link),
  stringsAsFactors = FALSE
)


uids = paste(snv$gene, snv$coding, sep = '_')

shinyApp(
  ui = fluidPage(
    title = paste('Variant Report for case',x),
    DT::dataTableOutput('foo'),
    tableOutput('sel'),
    downloadButton('download', 'Download row classifications')
  ),
  server = function(input, output, session) {
    m = matrix(
      character(0), nrow = nrow(snv), ncol = 6,
      dimnames = list(uids, classifications)
    )

    for (i in seq_len(nrow(m))) {
      for (j in seq_along(classifications)) {
        class_name = classifications[j]
        default_val = ifelse(class_name == "unknown", "checked", "")
        m[i, j] = sprintf(
          '<input type="radio" name="%s" value="%s" %s/>',
          uids[i], classval[j], default_val
        )
      }
    }

    output$foo = DT::renderDataTable(
      cbind(generic_df,m), escape = FALSE, selection = 'single', server = FALSE,
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
      varvals = rep(NA, length(uids))
      varvals = sapply(uids, function(i) input[[i]])
      tib = dplyr::select(generic_df, -link)
      tib$values = varvals
      tib
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
  }
)

