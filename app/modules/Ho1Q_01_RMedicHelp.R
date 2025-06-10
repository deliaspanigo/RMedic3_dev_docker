





Ho1Q_01_RMedicHelp_UI <- function(id) {
  
  ns <- NS(id)
  
  fluidRow(
    column(4, 
           radioButtons(inputId = "help_ho_1q",
                        label = h3("Selección de Ayuda Automática"),
                        choices = c("RMedic Here!" = 1,
                                    "Test de Proporciones" = 2,
                                    "Test de Uniformidad" = 3)
           )
    ),
    column(8,
           br(),
           conditionalPanel(condition = "input.help_ho_1q == 1",
                            div(
                              h3("RMedic Here!"),
                              HTML(
                                "Las pruebas de hipótesis más utilizados aplicados a una variable categórica son:<br/>
                      - <b>Test de proporciones</b>.<br/>
                      - <b>Test de uniformidad</b>.<br/>
                      Seleccionando la ayuda de cada uno encontrarás un resumen con
                      detalles teóricos y estructura de la base de datos.<br/>
                      Estos te ayudarán a determinar si estas herramientas pueden ser
                      aplicadas en tu trabajo."
                              )
                            )
           ),
           conditionalPanel(condition = "input.help_ho_1q == 2",
                            div(
                              h3("Test de proporciones"),
                              HTML(
                                "Cada una variable con al menos 2 categorías, es posible poner a prueba el valor de proporción de la categoría seleccionada."
                              )
                            )
           ),
           conditionalPanel(condition = "input.help_ho_1q == 3",
                            div(
                              h3("Test de Uniformidad"),
                              HTML(
                                "Se determina si dada una variable categórica todas las categorías presentes en dicha variable poseen la misma proporción o al menos una de ellas es diferente."
                              )
                            )
           ),
    )
  )
  
  
  
}






## Segmento del server

Ho1Q_01_RMedicHelp_SERVER <- function(input, output, session,
                                            minibase,
                                            decimales,
                                            control_ejecucion,
                                            tablas_1q,
                                            alfa = alfa) {
  
  
  
  
  
  
  
  
  
  
  
  
}







