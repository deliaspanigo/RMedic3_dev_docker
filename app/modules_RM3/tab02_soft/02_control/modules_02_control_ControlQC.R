## Segmento del UI
modules_02_control_ControlQC_UI <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("SeccionControlQC"))
  
  
}




## Segmento del server
modules_02_control_ControlQC_SERVER <- function(input, output, session, 
                             base, 
                             batalla_naval,
                             decimales) {
  
  
  
  
  # NameSpaceasing for the session
  ns <- session$ns
  
  # Caso 3: QC
  casoRMedic <- reactive({
    
    if(is.null(batalla_naval())) return(NULL)
    if(is.null(batalla_naval()[[4]])) return(NULL)
    if(length(batalla_naval()[[4]]) == 0) return(NULL)
    if(batalla_naval()[[4]] == '') return(NULL)
    casoRMedic <- batalla_naval()[[4]]
    #casoRMedic <- as.numeric(as.character(as.vector(batalla_naval()[[4]])))
    casoRMedic
    
  })
  
  # Todas las tablas 1Q
  Reactive_control_qc_RMedic <- reactive({
    
    if(is.null(casoRMedic())) return(NULL)
    if(casoRMedic() != 5) return(NULL)
    
    #print(batalla_naval()[[1]])
    #print(batalla_naval())
    
    vector_selected_vars <- batalla_naval()[[1]]
    if(identical(batalla_naval()[[1]], c(1, 10))) vector_selected_vars <- vector_selected_vars[c(2,1)]
    
    salida <-  control_qc_RMedic(base = base(), columna = vector_selected_vars)
    
    
    
    
    # Return Exitoso
    return(salida)
    
    
  })  
  
  
  
  # Control 1Q - Tabla 01      
  output$Tabla_Control01 <- renderTable(rownames = FALSE, align= "c",{
    Reactive_control_qc_RMedic()[[1]]
  })
  
  # Control 1Q - Texto 01 
  output$Texto_Control01 <- renderText({
    Reactive_control_qc_RMedic()[[2]]
  })
  
  # Control 1Q - Tabla 01      
  output$Tabla_Control02 <- renderTable(rownames = FALSE, align= "c",{
    Reactive_control_qc_RMedic()[[7]]
  })
  
  # Control 1Q - Texto 01 
  output$Texto_Control02 <- renderText({
    Reactive_control_qc_RMedic()[[8]]
  })
  
  # Control 1Q - Tabla 02      
  output$Tabla_Control03 <- renderTable(align= "c",{
    Reactive_control_qc_RMedic()[[3]]
  })
  
  # Control 1Q - Texto 02 
  output$Texto_Control03 <- renderText({
    Reactive_control_qc_RMedic()[[4]]
  })
  
  
  
  
  
  output$SeccionControlQC <- renderUI({
    
    # Especificaciones de cumplimiento
    if(is.null(casoRMedic())) return(NULL)
    if(casoRMedic() != 5) return(NULL)
    
    
    # Si es el caso 1, seguimos!
    div(
      h2_mod("RMedic - Control para 1 Variable Numérica particionada por 1 Variable Categórica"),
      h4("- El control sobre una variable numérica particionada por 1 variable categórica se lleva a cabo solo sobre las filas que 
      poseen simultáneamente datos de ambas variables."),
      h4("- Las categorías y cantidad de categorías de la variable categórica deben tener sentido en el marco de la experiencia."), 
      h4("- Los valores mínimos y máximos de la variable numérica en cada categoría deben tener sentido en el marco de la experiencia."),
      br(),
      h3_mod("Parte 1 de 3 - Mínimos por categoría"),
      h4(htmlOutput(ns("Texto_Control01"))),
      br(),
      tableOutput(ns("Tabla_Control01")),
      br(),
      h3_mod("Parte 2 de 3 - Màximos por categoría"),
      h4(htmlOutput(ns("Texto_Control02"))),
      br(),
      tableOutput(ns("Tabla_Control02")),
      br(),
      h3_mod("Parte 3 de 3 - Celdas vacías"),
      h4(htmlOutput(ns("Texto_Control03"))),
      br(),
      tableOutput(ns("Tabla_Control03"))
      
    )
  })
  
  
  
  
  
  
}


