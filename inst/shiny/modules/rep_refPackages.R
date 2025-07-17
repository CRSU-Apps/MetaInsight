rep_refPackages_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    # UI
    strong("Download List of References"), br(), br(),
    strong("Select download file type"),
    selectInput(ns('refFileType'), label = "",
                choices = c("PDF" = ".pdf", "HTML" = ".html", "Word" = ".docx")),
    downloadButton(ns("dlrefPackages"), "Download References")
  )
}

rep_refPackages_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

    output$dlrefPackages <- downloadHandler(
      filename = function() {
        paste0("ref-packages-", Sys.Date(), input$refFileType)
        },
      content = function(file) {
        # Create BIB file
        bib_file <- "Rmd/references.bib"
        temp_bib_file <- tempfile(pattern = "ref_", fileext = ".bib")
        # Package always cited
        knitcitations::citep(citation("metainsight"))
        knitcitations::citep(citation("knitcitations"))
        knitcitations::citep(citation("knitr"))
        knitcitations::citep(citation("rmarkdown"))
        knitcitations::citep(citation("gemtc"))
        knitcitations::citep(citation("BUGSnet"))
        knitcitations::citep(citation("bayesplot"))
        knitcitations::citep(citation("bnma"))
        knitcitations::citep(citation("combinat"))
        knitcitations::citep(citation("cookies"))
        knitcitations::citep(citation("cowplot"))
        knitcitations::citep(citation("DT"))
        knitcitations::citep(citation("data.table"))
        knitcitations::citep(citation("data.tree"))
        knitcitations::citep(citation("dplyr"))
        knitcitations::citep(citation("ggiraphExtra"))
        knitcitations::citep(citation("ggplot2"))
        knitcitations::citep(citation("ggrepel"))
        knitcitations::citep(citation("glue"))
        knitcitations::citep(citation("grid"))
        knitcitations::citep(citation("igraph"))
        knitcitations::citep(citation("jsonlite"))
        knitcitations::citep(citation("lubridate"))
        knitcitations::citep(citation("MCMCvis"))
        knitcitations::citep(citation("magick"))
        knitcitations::citep(citation("markdown"))
        knitcitations::citep(citation("matrixcalc"))
        knitcitations::citep(citation("metafor"))
        knitcitations::citep(citation("mirai"))
        knitcitations::citep(citation("mvtnorm"))
        knitcitations::citep(citation("netmeta"))
        knitcitations::citep(citation("patchwork"))
        knitcitations::citep(citation("plotly"))
        knitcitations::citep(citation("plotrix"))
        knitcitations::citep(citation("plyr"))
        knitcitations::citep(citation("quarto"))
        knitcitations::citep(citation("R6"))
        knitcitations::citep(citation("R.utils"))
        knitcitations::citep(citation("rio"))
        knitcitations::citep(citation("rintrojs"))
        knitcitations::citep(citation("shinyBS"))
        knitcitations::citep(citation("shinyalert"))
        knitcitations::citep(citation("shinybusy"))
        knitcitations::citep(citation("shinycssloaders"))
        knitcitations::citep(citation("shinydashboard"))
        knitcitations::citep(citation("shinyjs"))
        knitcitations::citep(citation("stringr"))
        knitcitations::citep(citation("shinyWidgets"))
        knitcitations::citep(citation("svglite"))
        knitcitations::citep(citation("tidyr"))
        knitcitations::citep(citation("tools"))
        knitcitations::citep(citation("zip"))

        # Write BIBTEX file
        knitcitations::write.bibtex(file = temp_bib_file)
        # Replace NOTE fields with VERSION when R package
        bib_ref <- readLines(temp_bib_file)
        bib_ref <- gsub(pattern = "note = \\{R package version", replace = "version = \\{R package", x = bib_ref)
        writeLines(bib_ref, con = temp_bib_file)
        file.rename(temp_bib_file, bib_file)
        # Render reference file
        md_ref_file <- tempfile(pattern = "ref_", fileext = ".md")
        rmarkdown::render("Rmd/references.Rmd",
                          output_format =
                            switch(
                              input$refFileType,
                              ".pdf" = rmarkdown::pdf_document(),
                              ".html" = rmarkdown::html_document(),
                              ".docx" = rmarkdown::word_document()
                            ),
                          output_file = file,
                          clean = TRUE,
                          encoding = "UTF-8")
      })


  }

)}
