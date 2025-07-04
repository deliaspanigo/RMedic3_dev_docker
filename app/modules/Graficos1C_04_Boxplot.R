


Graficos1C_04_Boxplot_UI <- function(id) {
  
  ns <- NS(id)
  
  uiOutput(ns("armado_grafico"))
  
  
}






## Segmento del server
Graficos1C_04_Boxplot_SERVER <- function(input, output, session, 
                                         minibase,
                                         decimales,
                                         control_ejecucion,
                                         tablas_1c) {
  
  
  
  # NameSpaceasing for the session
  ns <- session$ns
  
  
  
  # Control interno 01
  control_interno01 <- reactive({
    
    if(is.null(control_ejecucion())) return(FALSE)
    else return(control_ejecucion())
  })
  
  
  # xxchange <- reactive({
  #   paste(ylab_interno())
  # })
  
  
#  initialInputs <- isolate(reactiveValuesToList(input))

  
  colores_usuario <- reactive({
    
    # Control interno 01
    if(!control_interno01()) return(NULL)
    
    cantidad <- 1
    armado <- "Color..."
    
    
    
    mis_colores <- rep(NA, cantidad)
    
    
    if(length(mis_colores) == 0) return(NULL)
    
    
    for(i in 1:cantidad){ 
      nombre_input <- paste("col", i, sep="_")
      if(is.null(input[[nombre_input]])) return(NULL)
      
      mis_colores[i] <- input[[nombre_input]]
      
    }
    
    
    
    return(mis_colores)
  })
  
  
  reseteo_logico <- reactiveVal(F)
  aplicador_logico <- reactiveVal(F)
  
  
  observeEvent(minibase(), {
    
    reseteo_logico(!reseteo_logico())
    delay(400,     aplicador_logico(!aplicador_logico()))
    # aplicador_logico(!aplicador_logico())
    
  })
  
  
  # observeEvent(input$controlador01, {
  #   
  #   shinyjs::toggle(ns("James01"), asis = T, anim = TRUE, animType = "fade")
  #   
  # })
  
  
  observeEvent(input$controlador02, {
    
    
    aplicador_logico(!aplicador_logico())
    
  })
  
  
  
  
  observeEvent(input$reset, {
    
    reseteo_logico(!reseteo_logico())
    delay(400,     aplicador_logico(!aplicador_logico()))
  })
  
  
  
  
  medidas_resumen <- reactive({
    
    # Control interno 01
    if(!control_interno01()) return(NULL)
    
    
    
    # Nota: al valor input$x_breaks lo tuve que poner
    #      como na.omit(input$x_breaks)[1] por que algunas veces
    #      otorga un vector con dos valores, pero uno de ellos es NA.
    
    
    
    salida <-  RMedic_1c_tablas(input_base =  minibase(),
                                input_decimales = decimales(),
                                input_min = NULL,
                                input_max = NULL,
                                input_breaks = NULL,
                                input_side = NULL
    )[[1]]
    
    
    
    # Return Exitoso
    return(salida)
    
    
  })  
  
  
  
  
  
  # observeEvent(input$ylab, {
  #   
  #   
  #   if(input$ylab == colnames(minibase())[1]) {
  #     
  #     if(input$ylab != valores_usuario()$ylab) {
  #       
  #       delay(1000, aplicador_logico(!aplicador_logico()))
  #       
  #       #  reseteo_logico(!reseteo_logico())
  #     }
  #   }
  #   
  #   # reseteo_logico(!reseteo_logico())
  #   
  # })
  # 
  
  
  # Variable criterio de inclusion
  observeEvent(reseteo_logico(),{
    
    # freezeReactiveValue(input, "ayuda")
    # updateRadioButtons(session,
    #                    inputId = "ayuda",
    #                    label = "Ayuda en el gráfico...",
    #                    choices = c("Sin detalle" = F,
    #                                "Agregar especificaciones" = T
    #                    ),
    #                    selected = F
    # )
    
    freezeReactiveValue(input, "y_max")
    updateNumericInput(session,
                       inputId = "y_max",
                       label = "Máximo eje Y", 
                       value = valores_iniciales()$y_max,
                       min = valores_iniciales()$y_max,
                       max = NA
    )
    
    freezeReactiveValue(input, "y_min")
    updateNumericInput(session,
                       inputId = "y_min",
                       label = "Mínimo eje Y", 
                       value = valores_iniciales()$y_min,
                       min = NA,
                       max = valores_iniciales()$y_min
    )
    
    
    freezeReactiveValue(input, "ylab")
    updateTextInput(session,
                    inputId = "ylab",
                    label = "Rótulo eje Y",
                    value = valores_iniciales()$ylab
    )
    
    
    freezeReactiveValue(input, "xlab")
    updateTextInput(session,
                    inputId = "xlab",
                    label = "Rótulo eje X",
                    value = valores_iniciales()$xlab
    )
    
    
    # colourpicker::updateColourInput(session,
    #                                 inputId = "col_1",
    #                                 label = "Color...",
    #                                 value =  valores_iniciales()$color[1])
    
    label_armado <- "Color..."
    colores_internos <- valores_iniciales()$color
    cantidad <- length(colores_internos)
    
    lapply(1:cantidad, function(i) {
      
      nombre_input <- paste("col", i, sep="_")
      
      
      freezeReactiveValue(input, nombre_input)
      colourpicker::updateColourInput(session,
                                      inputId = nombre_input,
                                      label = label_armado[i],
                                      value = colores_internos[i])
      
    })
    
    
    #  delay(100,     aplicador_logico(!aplicador_logico()))
    
    
    
  })
  
  
  
  
  # Salida de colores
  output$MODcolor <- renderUI({
    
    # Control interno 01
    if(!control_interno01()) return(NULL)
    
    label_armado <- "Color..."
    colores_internos <- valores_iniciales()$color
    cantidad <- length(colores_internos)
    
    lapply(1:cantidad, function(i) {
      
      nombre_input <- paste("col", i, sep="_")
      div(
        colourpicker::colourInput(inputId = ns(nombre_input),
                                  label = label_armado[i], 
                                  value = colores_internos[i]), br()
      )
      
    })
    
  })
  
  
  
  
  output$texto_ayudaMax_y <- renderText({
    
    # Control interno 01
    if(!control_interno01()) return(NULL)
    
    texto <- "El límite superior del eje Y debe ser igual o mayor al máximo
    valor de la variable."
    
    if(input$y_max < max(minibase()[,1])) return(texto) else return(NULL)
    
  })
  
  
  output$texto_ayudaMin_y <- renderText({
    
    # Control interno 01
    if(!control_interno01()) return(NULL)
    
    texto <- "El límite inferior del eje Y debe ser igual o menor al mínimo 
    valor de la variable."
    
    if(input$y_min > min(minibase()[,1])) return(texto) else return(NULL)
    
  })
  
  
  output$menu_general01 <- renderUI({
    
    # Control interno 01
    if(!control_interno01()) return(NULL)
    
    div(
      fluidRow(
        column(6,
               radioButtons(inputId = ns("ayuda"),
                            label = "Ayuda en el gráfico...",
                            choices = c("Sin detalle" = F,
                                        "Agregar especificaciones" = T
                            )
               )
        ),
        column(6,
               uiOutput(ns("MODcolor"))
        )
      ),
      br(),
      fluidRow(
        column(6,
               numericInput(inputId = ns("y_min"),
                            label = "Mínimo eje Y", 
                            value = valores_iniciales()$y_min,
                            min = NA,
                            max = valores_iniciales()$y_min
               ),
               textOutput(ns("texto_ayudaMin_y"))
        ),
        column(6,
               numericInput(inputId = ns("y_max"),
                            label = "Máximo eje Y", 
                            value = valores_iniciales()$y_max,
                            min = valores_iniciales()$y_max,
                            max = NA
               ),
               textOutput(ns("texto_ayudaMax_y"))
        )
      ),
      br(),
      fluidRow(
        column(6,
               textInput(inputId = ns("xlab"),
                         label = "Rótulo eje X",
                         value = valores_iniciales()$xlab
               )
        ),
        column(6, 
               textInput(inputId = ns("ylab"),
                         label = "Rótulo eje Y",
                         value = valores_iniciales()$ylab
               )
        )
      ),
      br(),
      br(),
      bsButton(ns("reset"), "Resetear Gráfico", type = "toggle", value = TRUE,
               icon("bars"), style = "primary", size = "large"
      ),
      bsButton(ns("controlador02"), "Aplicar todos los cambios", type = "toggle", value = TRUE,
               icon("bars"), style = "primary", size = "large"
      )
    )
    
    
    
    
    
  })
  
  
  
  
  
  
  
  
  valores_iniciales <-  reactive({
    
    # Control interno 01
    if(!control_interno01()) return(NULL)
    if(is.null(minibase())) return(NULL)
    
    
    valores <- list(x_min = NULL,
                    x_max = NULL,
                    y_min = min(minibase()[1]),
                    y_max = max(minibase()[1]),
                    xlab = "",
                    ylab = colnames(minibase())[1],
                    ayuda = F,
                    color = c("#FF0000")
    )
    
    
    
    
    return(valores)
  })
  
  
  
  valores_usuario <-   eventReactive(aplicador_logico(), ignoreNULL = FALSE, {
    
    # Control interno 01
    if(!control_interno01()) return(NULL)
    if(is.null(minibase())) return(NULL)
    if(is.null(valores_iniciales())) return(NULL)
    
    valores <- list()
    
    # Valores X
    if(!is.null(input$x_min)) valores[[1]] <- input$x_min else valores[[1]] <- valores_iniciales()$x_min
    if(!is.null(input$x_max)) valores[[2]] <- input$x_max else valores[[2]] <- valores_iniciales()$x_max
    
    # Valores Y
    if(!is.null(input$y_min)) valores[[3]] <- input$y_min else valores[[3]] <- valores_iniciales()$y_min
    if(!is.null(input$y_max)) valores[[4]] <- input$y_max else valores[[4]] <- valores_iniciales()$y_max
    
    # Rotulos
    if(!is.null(input$xlab))  valores[[5]] <- input$xlab  else valores[[5]] <- valores_iniciales()$xlab
    if(!is.null(input$ylab))  valores[[6]] <- input$ylab  else valores[[6]] <- valores_iniciales()$ylab
    
    # Ayuda
    if(!is.null(input$ayuda)) valores[[7]] <- input$ayuda else valores[[7]] <- valores_iniciales()$ayuda
    
    # Colores
   # if(!is.null(input$col_1)) valores[[8]] <- input$col_1 else valores[[8]] <- valores_iniciales()$color
    if(!is.null(colores_usuario())) valores$color <- colores_usuario() else valores$color <- valores_iniciales()$color
    
    
    # Nombre de la lista, mismo nombre que por defecto
    names(valores) <- names(valores_iniciales())
    
    # Correccion para los valores que min y max de cada eje Y
    if(!is.null(valores$y_min)) if(valores$y_min > min(minibase()[,1])) valores$y_min <- min(minibase()[,1])
    if(!is.null(valores$y_max)) if(valores$y_max < max(minibase()[,1])) valores$y_max <- max(minibase()[,1])
    
    
    
    return(valores)
  })
  
  
  
  
  output$grafico01 <- renderPlot({
    
    # Control interno 01
    if(!control_interno01()) return(NULL)
    # if(is.null(valores_usuario())) return(NULL)
    # 
    # 
    
    texto01 <- c("Mínimo", "Q1", "Q2 (Mediana)", "Q3", "Máximo")
    texto02 <- c("25%", "25%", "25%", "25%")
    la_media <- as.numeric(as.character(tablas_1c()[[2]][1,3]))
    #print(tablas_1c())

    mis_valores <- c(tablas_1c()[[2]][1,2],
                     tablas_1c()[[3]][1,c(2,5)],
                     tablas_1c()[[2]][1,5])
    coordenadas <-   boxplot(minibase()[1],
                             ylim = c(valores_usuario()$y_min, valores_usuario()$y_max),
                             ylab = valores_usuario()$ylab, xlab = valores_usuario()$xlab,
                             col = valores_usuario()$color,
                             range = 0)
    
    # points(x = 1, y = la_media, pch = 19, col = "black", cex = 1.5)
    

    
   # coordenadas <- round2(coordenadas, decimales())
    
    
    
    
    
    if (valores_usuario()$ayuda) {
      
      #   text(1.25, coordenadas$stats, texto01, pos = 4, cex = 1.5)
      coordenadasY <- coordenadas$stats
      mediasY <- c()
      for(k in 1:(length(coordenadasY)-1)) mediasY[k] <- mean(coordenadasY[c(k, (k+1))])
      
      
      text(1.25, coordenadasY, texto01, pos = 4, cex = 1.5)
      text(0.75, coordenadasY, mis_valores, pos = 4, cex = 1.5)
      
      colores <- rep(c("red", "blue"), 2)
      
      pos01 <- 0.70 - 0.09
      pos02 <- pos01 - 0.06
      pos03 <- pos01 - 0.12
      
      for(k in 1:(length(coordenadasY)-1)) {
        
        
        lines(x = c(pos01, pos01), y = coordenadasY[c(k, (k+1))], col = colores[k], lwd = 4)
        lines(x = c(pos03, pos03), y = coordenadasY[c(k, (k+1))], col = colores[k], lwd = 4)
        
        
      }
      
      
      for(k in 1:length(coordenadasY)) {
        
        
        
        lines(x = c(pos01, pos03), y = rep(coordenadasY[k], 2), col = "black", lwd = 4, lty = 2)
        
      }
      
      text(pos02, mediasY, texto02, srt = 90)
      
    }
    
    
    
  })
  
  
  
  
  
  
  
  output$armado_grafico <- renderUI({
    
    # Control interno 01
    if(!control_interno01()) return(NULL)
    
    
    # if(is.null(valores_usuario())) return(NULL)
    
    
    div(
      h2("Gráfico de Boxplot"),
      fluidRow(
        column(6,
               plotOutput(ns("grafico01"))
        ),
        column(6,
               bsButton(inputId = ns("controlador01"), 
                        label = "Mostrar/Ocultar opciones gráficas",
                        icon = icon("bars"), 
                        type = "toggle", 
                     #   value = FALSE,
                     value = TRUE,
                        style = "primary", 
                        size = "large"
               ), br(),br(), br(),
               conditionalPanel(condition = "input.controlador01", ns = ns,
               div(id = ns("James01"), uiOutput(ns("menu_general01")))
               )
               
               
        )
        
      )
    )
  })
  
}