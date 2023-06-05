#' A vector filter for factors with only a few choices
#'
#' @param input requisite shiny module field specifying incoming ui input
#'   reactiveValues
#' @param output requisite shiny module field capturing output for the shiny
#'   data filter ui
#' @param session requisite shiny module field containing the active shiny
#'   session
#' @param x a reactive expression resolving to the vector to filter
#' @param filter_na a logical value indicating whether to filter \code{NA}
#'   values from the \code{x} vector
#' @param verbose a \code{logical} value indicating whether or not to print log
#'   statements out to the console
#'
#' @return a \code{\link[shiny]{reactiveValues}} list containing a logical
#'   vector called "mask" which can be used to filter the provided vector and an
#'   element "code" which is the expression used to generate the mask.
#'
#' @importFrom shiny reactive reactiveValues renderUI selectInput isolate
shiny_vector_filter_factor_many <- function(input, output, session,
                                            x = shiny::reactive(factor()), filter_na = shiny::reactive(FALSE),
                                            verbose = FALSE) {
  ns <- session$ns

  x_wo_NAs <- shiny::reactive(Filter(Negate(is.na), x()))
  module_return <- shiny::reactiveValues(code = TRUE, mask = TRUE)

  # 增加server 功能
  xr <- if (is.reactive(x)) x else reactive(x)

  if (length(unique(as.character(xr()))) > 50) {
    # 两个内容需要联合触发，UI 与 update
    ui_update <- reactiveVal(FALSE)
    output$ui <- shiny::renderUI({
      # shiny::selectizeInput(ns("param"),
      #   label = NULL,
      #   choices = NULL,
      #   selected = shiny::isolate(input$param) %||% c(),
      #   multiple = TRUE,
      #   width = "100%"
      # )
      ui_update(TRUE)
      proportionSelectInput(ns("param"),
        NULL,
        vec = NULL,
        selected = shiny::isolate(input$param) %||% c(),
        multiple = TRUE,
        width = "100%"
      )
    })

    observeEvent(ui_update(),{
      if (isTRUE(ui_update())) {
        vecr_counts <- sort(table(xr()), decreasing = TRUE) |> 
          as.data.frame()
        colnames(vecr_counts) <- c("name","count")
        vecr_counts$prop <- vecr_counts$count/sum(vecr_counts$count)
        
        labels <- sprintf('{
        "name": "%s",
        "prop": %f,
        "count": %d
      }',vecr_counts$name,vecr_counts$prop,vecr_counts$count)
        
        choices <- as.list(as.character(vecr_counts$name))
        names(choices) <- labels
        
        updateSelectizeInput(
          session = session,
          inputId = "param",
          choices = choices,
          server = TRUE
        )
        ui_update(FALSE)
      }
    })
  } else {
    output$ui <- shiny::renderUI({
      proportionSelectInput(ns("param"), NULL,
        vec = x,
        selected = shiny::isolate(input$param) %||% c(),
        multiple = TRUE,
        width = "100%"
      )
    })
  }

  module_return$code <- shiny::reactive({
    if (length(input$param)) {
      bquote(.x %in% .(c(if (filter_na()) c() else NA, input$param)))
    } else if (filter_na()) {
      bquote(!is.na(.x))
    } else {
      TRUE
    }
  })

  module_return$mask <- shiny::reactive({
    eval(do.call(substitute, list(module_return$code(), list(.x = x()))))
  })

  module_return
}
