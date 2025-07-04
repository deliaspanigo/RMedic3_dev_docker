


Ho2C_04_TestRegresionLinealSimple_UI <- function(id) {
  
  ns <- NS(id)
  
  uiOutput(ns("armado_ho"))
  
  
}






## Segmento del server
Ho2C_04_TestRegresionLinealSimple_SERVER <- function(input, output, session, 
                                    minibase, 
                                    decimales,
                                    control_ejecucion,
                                    alfa) {
  
  
  
  # NameSpaceasing for the session
  ns <- session$ns
  
  # Control interno 01
  control_interno01 <- reactive({
    
    if(is.null(control_ejecucion())) return(FALSE)
    else return(control_ejecucion())
  })
  
  
  
  
  
  ##### 2025
  output$tabla22 <- renderTable({
    
    mi_tabla <- data.frame(
      "Detalle" = c("X", "Y"),
      "Variables" = colnames(minibase())
    )
    
    mi_tabla <- mi_tabla[vector_orden()]
    mi_tabla
  })
  
  output$menu_cambio <- renderUI({
    req(minibase())
    
    vector_choices <- c("no invertir", "invertir")
    names(vector_choices) <- colnames(minibase())
    div(
      fluidRow(
        column(4, selectInput(inputId = ns("invertir"), label = "Elegir X", choices = vector_choices)),
        # column(4, actionButton(inputId = ns("activate"), label = "Aplicar")),
        column(4, tableOutput(ns("tabla22")))
      )
    )
  })
  
  
  vector_orden <- reactive({
    req(input$invertir)
    vector_orden <- c(1,2)
    if(input$invertir == "invertir") vector_orden <- c(2,1)
    vector_orden
  })
  
  
  minibase_mod <- reactiveVal()
  observe({
    mi_vector_cambio <- vector_orden() # 2025
    minibase_mod(minibase()[mi_vector_cambio])
  })
  ####2025##############################################
  
  
  
  
  # Menu del opciones para el test de proporciones
  output$opciones_ho <- renderUI({
    
    
    div(
      # Seleccion de una categoria
      fluidRow(
        column(4,
               # Seleccion del valor bajo H0
               numericInput(inputId = ns("valor_bajo_ho"),
                            label = "Media poblacional (Valor esperado bajo hipótesis): ",
                            min = NA,  max = NA, step = 0.01, value = 0)
        ),
        column(4,
               
               # Seleccion del tipo de prueba
               radioButtons(ns("tipo_prueba_ho"), "Tipo de Prueba de Hipótesis:",
                            choices = c("Bilateral" = "two.sided",
                                        "Unilateral izquierda" = "less",
                                        "Unilateral derecha" = "greater")
               )
        )
        
        
      )
    )
    
    
    
  })
  
  # Test de Proporciones
  The_Test <- reactive({
    
    if(!control_interno01()) return(NULL)
    if(is.null(minibase())) return(NULL)
    if(is.null(minibase_mod())) return(NULL)
 #   if(is.null(input$tipo_prueba_ho)) return(NULL)
  #  if(is.null(input$valor_bajo_ho)) return(NULL)
    if(is.null(decimales())) return(NULL)
    if(is.null(alfa())) return(NULL)
    
    
    mi_vector_cambio <- vector_orden() # 2025
    
    Test_2C_TestRegresionLinealSimple( input_base = minibase_mod(),
                                        input_decimales = decimales(),
                                        input_alfa = alfa())
    
    
    
    
    
    
  })
  # #######################################################
  # 
  
  # Tabla Resumen
  observe( output$tabla_resumen <- renderTable(rownames = FALSE, digits=decimales(), align = "c",{
    
    The_Test()$tabla_resumen
    
  }))
  
  
  # Tabla Requisitos
  observe( output$tabla_requisitos <- renderTable(rownames = FALSE, digits=decimales(), align = "c",{
    
    The_Test()$tabla_requisitos
    
  }))
  
  # Frase 1: Sobre los requisitos
  observe(output$frase_requisitos <- renderUI({
    HTML(The_Test()$frase_requisitos)
  }))
  
  # Tabla Requisitos
  observe( output$tabla_regresion <- renderTable(rownames = FALSE, digits=decimales(), align = "c",{
    
    The_Test()$tabla_regresion
    
  }))
  
  # Frase 2: Explicacion Estadistica
  observe(output$frase_estadistica_ordenada <- renderUI({
    HTML(The_Test()$frase_estadistica_ordenada)
  }))
  
  
  
  # Frase 2: Explicacion Estadistica
  observe(output$frase_estadistica_pendiente <- renderUI({
    HTML(The_Test()$frase_estadistica_pendiente)
  }))
  
  # Frase 2: Explicacion Estadistica
  observe(output$frase_estadistica_r2ajus <- renderUI({
    HTML(The_Test()$frase_estadistica_r2ajus)
  }))
  
  
  # Frase 3: Advertencia por redondeo
  observe(output$frase_redondeo <- renderUI({
    HTML(The_Test()$frase_redondeo)
  }))
  
  
  # Frase 4: Juego de Hipotesis - ORdenada
  observe(output$frase_juego_hipotesis_ordenada <- renderUI({
    HTML(The_Test()$frase_juego_hipotesis_ordenada)
  }))
  
  # Frase 4: Juego de Hipotesis - Pendiente
  observe(output$frase_juego_hipotesis_pendiente <- renderUI({
    HTML(The_Test()$frase_juego_hipotesis_pendiente)
  }))
  
  # Frase 4: Juego de Hipotesis - ORdenada
  observe(output$frase_juego_hipotesis_r2ajus <- renderUI({
    HTML(The_Test()$frase_juego_hipotesis_r2ajus)
  }))
  
  
  
  # Frase 3: Advertencia por redondeo
  observe(output$grafico_especial <- renderPlot({
    
    residuos <- The_Test()$residuos
    predichos <- The_Test()$predichos
    
    
    ordenada <-  as.numeric(as.character(The_Test()$tabla_resumen[1,2]))
    pendiente <- as.numeric(as.character(The_Test()$tabla_resumen[2,2]))
    
    variable_x <- The_Test()$tabla_requisitos[1,1]
    variable_y <- The_Test()$tabla_requisitos[1,2]
      
    plot(predichos, residuos, col = "red", cex = 2, lwd = 2,
         xlab = "Predichos", 
         ylab = "Residuos")
    
    
    abline(h=0)
    
    
    
  
    
    
  }))
  
  
  # Frase 3: Advertencia por redondeo
  observe(output$grafico_regresion <- renderPlot({
    
  
    
    
    ordenada <-  as.numeric(as.character(The_Test()$tabla_resumen[1,2]))
    pendiente <- as.numeric(as.character(The_Test()$tabla_resumen[2,2]))
    
   
    plot(minibase_mod()[,1], minibase_mod()[,2], col = "red", cex = 2, lwd = 2,
         xlab = colnames(minibase_mod())[1], 
         ylab = colnames(minibase_mod())[2])
    
    
    abline(ordenada, pendiente)
    
    
    
    
    
    
  }))
  
  # Armado/Salida del test de Proporciones 1Q
  output$armado_ho <- renderUI({
    
    div(
      h2_mod("Test de Regresión Lineal Simple"),
      "Nota: para la utilización del test de Regresión Lineal Simple
             ambas variables debe ser numéricas y no deben ser ordinales (cualitativas representadas con números).", 
      br(),
      br(),
      uiOutput(ns("menu_cambio")),   
    #  h3("Elecciones del usuario"),
    #  uiOutput(ns("opciones_ho")),
      br(),
      # Mensaje de advertencia por redondeo
      span(htmlOutput(ns("frase_redondeo")), style="color:red"),
      br(),
      h3_mod("Tabla de Requisitos del test de Regresión Lineal Simple"),
      tableOutput(ns("tabla_requisitos")),
      htmlOutput(ns("frase_requisitos")),
      br(),
      h3_mod("Juego de Hipótesis de la Ordenada"),
      htmlOutput(ns("frase_juego_hipotesis_ordenada")),
      br(),
    h3_mod("Juego de Hipótesis de la Pendiente"),
    htmlOutput(ns("frase_juego_hipotesis_pendiente")),
    br(),
    h3_mod("Juego de Hipótesis del Coeficiente de Determinación (R2 Ajustado)"),
    htmlOutput(ns("frase_juego_hipotesis_r2ajus")),
    br(),
    tabsetPanel(
      tabPanel("Gráfico Residuos vs. Predicho",
               h3_mod("Gráfico de Residuos vs. Predichos"),
               plotOutput(ns("grafico_especial")),
               "Nota: este gráfico es para que usted compruebe visualmente la homogeneidad de varianzas 
               de los residuos."
               ),
      tabPanel("Test de Regresión Lineal Simple",
               h3_mod("Tabla Resumen del test de Regresión Lineal Simple"),
               tableOutput(ns("tabla_resumen")),
               br(),
               br(),
               h3_mod("Gráfico de Regresión Lineal Simple"),
               plotOutput(ns("grafico_regresion")),
               br(),
               h3_mod("Frases y conclusiones (Pendiente)"),
               htmlOutput(ns("frase_estadistica_pendiente")),
               br(), br(),
               h3_mod("Frases y conclusiones (R2 Ajustado)"),
               htmlOutput(ns("frase_estadistica_r2ajus")),
               br(), br(),
               h3_mod("Frases y conclusiones (Ordenada)"),
               htmlOutput(ns("frase_estadistica_ordenada"))
               )#,
    #   tabPanel("Modelo de Predicción",
    #            "Aquí van:", br(),
    #            "1) Poder calcular un valor de Y a partir de un valor de X.", br(),
    #            "2) Poder calcular un valor de X a partir de un valor de Y.", br(),
    #            "3) Le decimos que el modelo solo es válido en el rango de mínimo y 
    #            máximo del eje X.", br(), 
    #            "4) Si quiere calcular un valor que está por fuera del rango del eje X 
    #            le sale un cartel gigante que le dice que la predicción no es válida de realizar."
    # )
    )
   
    
      
      
    )
    
  })
  
  
  
  
  
  
  
}


