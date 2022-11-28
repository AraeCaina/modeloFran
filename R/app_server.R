#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

# Preparando solução ------------------------------------------------------

  output$side <- renderUI({
    tagList(
      uiOutput('boxes'),
      actionButton(
        'adicionar',
        'Adicionar outro box'
      )
    )
  })

  contador_boxes <- reactiveVal(1)

  output$boxes <- renderUI({
    tagList(
      shinydashboard::box(
        title = 'Grafico 1',
        width = 12,
        collapsed = TRUE,
        collapsible = TRUE,

        selectInput(
          'eixo_x_1',
          'eixo x',
          choices = c(1:2)
        ),
        selectInput(
          'eixo_y_1',
          'eixo y',
          choices = c(1:2)
        ),
        actionButton(
          'gerar_1',
          'Gerar Gráfico'
        )

      )
    )
  })

  output$body <- renderUI({
    bodys <- list()
    for(i in 1:contador_boxes()){
      bodys[[i]] <- uiOutput(paste0('body_', i))
    }

    bodys
  })


# Gerar novos botões ------------------------------------------------------

  observeEvent(input$adicionar, {

    contador_boxes(contador_boxes() + 1)

    boxes <- list()

    for(i in 1:contador_boxes()){

      boxes[[i]] <- tagList(
        shinydashboard::box(
          title = paste0('Grafico ', i),
          width = 12,
          collapsed = TRUE,
          collapsible = TRUE,

          selectInput(
            paste0('eixo_x_', i),
            'eixo x',
            choices = c(1:2)
          ),
          selectInput(
            paste0('eixo_y_', i),
            'eixo y',
            choices = c(1:2)
          ),
          actionButton(
            paste0('gerar_', i),
            'Gerar Gráfico'
          )
        )
      )
    }

    output$boxes <- renderUI({
      boxes
    })

  })
  observe({

    lapply(
      1:contador_boxes(),
      function(i){
        observeEvent(input[[paste0('gerar_', i)]], {
          isolate({
            output[[paste0('body_', i)]] <- renderUI({
              paste('eixo x:', input[[paste0('eixo_x_', i)]], 'eixo y:', input[[paste0('eixo_y_', i)]])
            })

          })
        })
      }
    )
  })

}
