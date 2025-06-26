module_opt04_cita_UI <- function(id) {
  ns <- NS(id)

  
    div(
      tagList(
        tags$head(
          tags$script(type="text/javascript", src = "busy.js"),
          tags$link(rel="shortcut icon", href="./rmediclogo.jpg"),
          tags$script(type="text/javascript", "var switchTo5x=true"),
          tags$script(type="text/javascript",'stLight.options({publisher: "675b3562-a081-470a-9fc4-3dd6a712209d", doNotHash: true, doNotCopy: true, hashAddressBar: false})')
        )
      ),
      div(id = ns("home"),
          br(),
          fluidRow(
            column(3, img(src = "rmediclogo.jpg", width = 300, height = 300)),
            column(9, div(
              h4(class = "outer", "¿Puedo usar RMedic para los resultados de mis publicaciones?"),
              p(class = "outer", strong("Claro que si!"), br(),
                'Si lo haces, debes:',
                tags$ol(
                  tags$li('Incluir en "Materiales y Métodos" a ', strong("R-Medic"), 'como software estadístico:'),
                  tags$li('Citar en tu "Bibliografía" textualmente la siguiente frase: ', h4(class = "outer", strong('Mangeaud A , Elías Panigo DH. 2018  R-Medic. Un programa de análisis estadísticos sencillo e intuitivo. Revista Methodo 3 (1) 18-22.')))
                ),
                a("Archivo para citar RMedic", target="_blank", href="RMedic_Cita.pdf")
              )
            )
            )
          )
          
         
      )
    )

}



module_opt04_cita_SERVER <- function(id) {
  moduleServer(id, function(input, output, session) {
   
    output$"super01" <- renderUI({
      
    })
    
  })
}

