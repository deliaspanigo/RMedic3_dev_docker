## Segmento del UI
modules_02_control_Control1C_UI <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("SeccionControl1C"))
  
  
}




## Segmento del server
modules_02_control_Control1C_SERVER <- function(input, output, session, 
                             base, 
                             batalla_naval,
                             decimales) {
  
  
  
  
  # NameSpaceasing for the session
  ns <- session$ns
  
  # Caso 1: 1Q
  casoRMedic <- reactive({
    
    if(is.null(batalla_naval())) return(NULL)
    if(is.null(batalla_naval()[[4]])) return(NULL)
    if(length(batalla_naval()[[4]]) == 0) return(NULL)
    if(batalla_naval()[[4]] == '') return(NULL)
    casoRMedic <- batalla_naval()[[4]]
    #casoRMedic <- as.numeric(as.character(as.vector(batalla_naval()[[4]])))
    casoRMedic
    
  })
  
  # Todas las tablas 1C
  Reactive_control_1c_RMedic <- reactive({
    
    if(is.null(casoRMedic())) return(NULL)
    if(casoRMedic() != 2) return(NULL)
    
    
    # salida <-  control_1c_RMedic(base = base(), columna = batalla_naval()[[1]])
    salida <-  control_1c_full(dataframe = base(), selected_col = batalla_naval()[[1]])
    
    
    
    # Return Exitoso
    return(salida)
    
    
  })  
  
  
  # Control 1C - Texto 01 
  output$Texto_Control01 <- renderText({
    Reactive_control_1c_RMedic()[[3]][1]
  })
  
  # Control 1C - Tabla 01      
  output$Tabla_Control01 <- renderTable(rownames = FALSE, align= "c",{
    Reactive_control_1c_RMedic()[[4]]
  })
  
  # Control 1C - Texto 02 
  output$Texto_Control02 <- renderText({
    Reactive_control_1c_RMedic()[[5]]
  })
  
  # Control 1C - Tabla 02      
  output$Tabla_Control02 <- renderTable(align= "c",{
    Reactive_control_1c_RMedic()[[6]]
  })
  
  
  
  
  
  
  output$SeccionControl1C <- renderUI({
    
    # Especificaciones de cumplimiento
    if(is.null(casoRMedic())) return(NULL)
    if(casoRMedic() != 2) return(NULL)
    
    
    
    output$"Intro" <- renderUI({
      fluidRow(
      h2_mod(Reactive_control_1c_RMedic()[["text_title"]]["text01"]),
      h4("- Los valores mínimo y máximo deben tener sentido en el marco de la experiencia."), 
      h4("- Corroborar la presencia o no de celdas vacías.")
      )
    })
    # Si es el caso 2, seguimos!
    div(
      htmlOutput(ns("Intro")),
      br(),
      br(),
      h3_mod("Parte 1 de 2 - Mínimo y Máximo dentro de lo esperado"),
      h4(htmlOutput(ns("Texto_Control01"))),
      br(),
      tableOutput(ns("Tabla_Control01")),
      br(),
      br(),
      h3_mod("Parte 2 de 2 - Celdas vacías"),
      h4(htmlOutput(ns("Texto_Control02"))),
      br(),
      tableOutput(ns("Tabla_Control02"))
      
    )
  })
  
  
  
  
  
  
}


