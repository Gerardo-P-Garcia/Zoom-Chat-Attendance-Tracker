library(shiny)
library(DT)

#library(readtext)
#test <- readtext("C:/Users/Shadow/Documents/Data/ciampi_sec2.txt")

cleaner <- function(text){
    require(dplyr)

    # Cut names at 'to'
    #for(i in 1:length(text)){
    text$names <- tolower(text$names)

    text$names <- gsub(" to .*", "", text$names)
    text$names <- gsub(" *. from ", "", text$names)
    text$names <- gsub(" : .*", "", text$names)
    text$names <- gsub("[0-9]", "", text$names)
    text$names <- gsub(".*:", "", text$names)

    # Remove leading and trailing white-space
    text$names <- stringr::str_trim(text$names, side='left')
    text$names <- stringr::str_trim(text$names, side='right')

    # Capitalize first letter and first letter after a space
    text$names <- gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2",
        text$names, perl=TRUE)

    # Alphabetize
    text <- text%>%
        arrange(names)

    # Remove duplicates
    text <- unique(text)

    return(text)

}

shinyApp(
    ui = fluidPage(
        fluidRow(
            fileInput("upload", "Choose Text File from Zoom Chat",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            downloadButton("download")
        ),
        fluidRow(
            DT::dataTableOutput('x1'),
            verbatimTextOutput("print")
        )
    ),
    server = function(input, output, session) {

        # In this edited example x is now a reactive expression, dependent on input$upload

        # Key to the solution is the use of reactiveValues, stored as vals
        vals <- reactiveValues(x = NULL)

        observe({


            # input$upload will be NULL initially. After the user selects
            # and uploads a file, head of that data file by default,
            # or all rows if selected, will be shown.

            req(input$upload)

            # when reading semicolon separated files,
            # having a comma separator causes `read.csv` to error
            tryCatch(
                {
                    x <- read.csv(input$upload$datapath,
                                  header = FALSE,
                                  sep = "\n",
                                  stringsAsFactors = TRUE,
                                  row.names = NULL)

                },
                error = function(e) {
                    # return a safeError if a parsing error occurs
                    stop(safeError(e))
                }
            )
            # Reactive values updated from x
            colnames(x) <- c('names')
            x <- cleaner(x)
            vals$x <- x
        })

        output$x1 = DT::renderDataTable(vals$x, selection = 'none', rownames = FALSE, edit = TRUE)

        proxy = dataTableProxy('x1')

        observeEvent(input$x1_cell_edit, {
            info = input$x1_cell_edit
            str(info)
            i = info$row
            j = info$col + 1
            v = info$value
            # Below is the crucial spot where the reactive value is used where a reactive expression cannot be used
            vals$x[i, j] <<- DT:::coerceValue(v, vals$x[i, j])
            replaceData(proxy, vals$x, resetPaging = FALSE, rownames = FALSE)
        })

        output$download <- downloadHandler(paste("attendance_", Sys.Date(), ".csv", sep=""),
                                           content = function(file){
                                               write.table(vals$x, file, sep="\t", row.names = F, col.names = F, quote=FALSE)
                                           },
                                           contentType = "text/csv")

    }
)
