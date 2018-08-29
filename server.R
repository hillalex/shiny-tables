#!/usr/bin/env Rscript
library(shiny)
library(shinydashboard)
library(data.table)
library(DT)
library(first90)
library(rhandsontable)

server <- function(input, output) {
    vals <- reactiveValues()

    data(survey_hts)

    dat <- survey_hts[country == "Malawi" &
        hivstatus == "all" &
        agegr == "15-49" &
        sex == "both" &
        outcome == "evertest"]

    vals$table_data = as.data.frame(dat)

    getData = reactive({
        if (!is.null(input$hot)) {
            DF = hot_to_r(input$hot)
        } else {
            DF = dat
        }
        DF
    })

    output$hot <- renderRHandsontable({
        DF = getData()
        if (!is.null(DF)) {
            rhandsontable(DF, stretchH = "all")
        }
    })

    output$MainBody <- renderUI({
        fluidPage(
            box(width = 12,
                h3(strong("Actions on datatable with buttons"), align = "center"),
                hr(),
                column(6, offset = 6,
                    HTML('<div class="btn-group" role="group" aria-label="Basic example">'),
                            actionButton(inputId = "add", label = "Add a new row"),
                            actionButton(inputId = "del", label = "Delete selected rows"),
                    HTML('</div>')
                ),
                column(12, dataTableOutput("Main_table"))
            ),
            box(width = 12,
                column(12, rHandsontableOutput("hot")))
        )
    })

    output$Main_table <- renderDT(vals$table_data, editable = TRUE)

    proxy = dataTableProxy('Main_table')

    observeEvent(input$Main_table_cell_edit, {
        info = input$Main_table_cell_edit
        str(info)
        i = info$row
        j = info$col
        v = info$value

        vals$table_data[i, j] <<- DT::coerceValue(v, vals$table_data[i, j])
        replaceData(proxy, vals$table_data, resetPaging = FALSE)  # important
    })

    observeEvent(input$del, {
        str(input$Main_table_rows_selected)
        str(vals$table_data)
        vals$table_data <- vals$table_data[-c(input$Main_table_rows_selected), ]
        replaceData(proxy, vals$table_data, resetPaging = FALSE)  # important
    })

    observeEvent(input$add, {
        temprow <- matrix(c(rep.int(NA,length(dat))),nrow=1,ncol=length(dat))

        # make it a data.frame and give cols the same names as data
        newrow <- data.frame(temprow)
        colnames(newrow) <- colnames(dat)

        vals$table_data <- rbind(vals$table_data,newrow)
        replaceData(proxy, vals$table_data, resetPaging = FALSE)  # important
    })

}
