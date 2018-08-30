#!/usr/bin/env Rscript
library(shiny)
library(shinydashboard)
library(data.table)
library(DT)
library(first90)
library(rhandsontable)

server <- function(input, output) {

    data(survey_hts)

    values = reactiveValues()

    dat <- survey_hts[country == "Malawi" &
        hivstatus == "all" &
        agegr == "15-49" &
        sex == "both" &
        outcome == "evertest"]

    table_data = as.data.frame(dat)

    values$table_data = table_data

    text_renderer = "function (instance, td, row, col, prop, value, cellProperties) {
        Handsontable.renderers.TextRenderer.apply(this, arguments);
        td.style.textAlign = 'right';
    }"

    output$hot <- renderRHandsontable({
            rhandsontable(values$table_data, stretchH = "all") %>%
                hot_col("outcome", allowInvalid = TRUE) %>%
                hot_col("agegr", allowInvalid = TRUE)  %>%
                hot_col("est", renderer=text_renderer) %>%
                hot_col("se", renderer=text_renderer) %>%
                hot_col("ci_l", renderer=text_renderer) %>%
                hot_col("ci_u", renderer=text_renderer) %>%
                hot_col("year", format="0")
    })


    output$MainBody <- renderUI({
        fluidPage(
            includeCSS("style.css"),
            box(width = 12,
                h3(strong("Handsontable example"), align = "center"),
                hr(),
                column(12, rHandsontableOutput("hot"),
                        fluidRow(
                            column(6,
                                downloadButton("downloadTemplate", "Download template"),
                                fileInput("file1", "Upload CSV",
                                        accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
                            )
                        )
                )
            )
        )
    })

    output$downloadTemplate <- downloadHandler(
            filename = "template.csv",
            content = function(file) {
                matrix_template <- matrix(c(rep.int(NA,length(values$table_data))),nrow=1,ncol=length(dat))

                # make it a data.frame and give cols the same names as data
                template <- data.frame(matrix_template)
                colnames(template) <- colnames(values$table_data)
                write.csv(template, file, row.names = FALSE, na = "")
            }
    )

    observeEvent(input$file1, {
        inFile <- input$file1

        if (is.null(inFile))
            return(NULL)

        values$table_data <<- read.csv(inFile$datapath)
    })

    observe({
        if(!is.null(input$hot))
            values$table_data <- hot_to_r(input$hot)
    })
}
