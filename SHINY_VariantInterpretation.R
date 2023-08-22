library(shiny)
library(DT)

snv = "./testfiles/Y537_B2023.6661_OcaV3_output/parsed_output/parsed_snv.tsv"
clinvar_hits = "./testfiles/Y537_B2023.6661_OcaV3_output/annotation_output/annotation_ClinVar.tsv"
clinvar_hits = readr::read_tsv(clinvar_hits)
snv = readr::read_tsv(snv)

snv = VariantAnnotationModules::amino_acid_code_3_to_1(snv)

addLinks_oncokb = function(snv){
  snv$oncokb = paste0("https://www.oncokb.org/gene/", snv$gene,"/", gsub("p\\.",'', snv$protein))
  snv$link_oncokb <- paste0("<a href='", snv$oncokb, "' target='_blank'>", snv$protein, "</a>")
  return(snv)
}


addLinks_clinvar = function(snv, clinvar_hits){
  snv_clinvar = dplyr::left_join(snv, dplyr::select(clinvar_hits, -protein), by = c("gene",'rowid', 'coding'))
  snv_clinvar$link_clinvar = NA
  for (i in 1:nrow(snv_clinvar)){
    if(!is.na(snv_clinvar$ClinVar_VariationID[i])){
      snv_clinvar$link_clinvar[i] = paste0("https://www.ncbi.nlm.nih.gov/clinvar/variation/",
                                           snv_clinvar$ClinVar_VariationID[i])
      snv_clinvar$link_clinvar[i] <- paste0("<a href='", snv_clinvar$link_clinvar[i], "' target='_blank'>", snv_clinvar$ClinVar_Significance[i], "</a>")
    }
  }
  return(snv_clinvar)
}

snvl = addLinks_oncokb(snv)
snv_clinvar = dplyr::left_join(snvl, dplyr::select(clinvar_hits, -protein), by = c("gene",'rowid', 'coding'))
snv_clinvar$ClinVar_VariationID
snvl = addLinks_clinvar(snvl, clinvar_hits = clinvar_hits)
dplyr::select(snvl, contains("link"))

classifications = c("benign", "likely benign", "unknown", "likely pathogenic", "pathogenic", "predictive")
classval = c("benign", "likely benign", "unknown", "likely pathogenic", "pathogenic", "predictive")
# Create a generic dataframe
generic_df <- data.frame(
  dplyr::select(snvl,rowid,gene, coding,protein, link_oncokb,link_clinvar),
  stringsAsFactors = FALSE
)


uids = paste(snvl$gene, snvl$rowid, sep = '_')
x = unique(snv$sampleNames)
shinyApp(
  ui = fluidPage(
    title = paste('Variant Report for case:',x),
    DT::dataTableOutput('foo'),
    tableOutput('sel'),
    downloadButton('download', 'Download row classifications')
  ),
  server = function(input, output, session) {
    m = matrix(
      character(0), nrow = nrow(snvl), ncol = 6,
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
      tib = dplyr::select(generic_df, -contains("link"))
      tib$Clinical_Interpretation = varvals
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
        selected_data$ID = x
        write.csv(selected_data, file, row.names = FALSE)
      }
    )
  }
)

