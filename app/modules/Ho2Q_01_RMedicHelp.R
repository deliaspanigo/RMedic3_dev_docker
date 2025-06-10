





Ho2Q_01_RMedicHelp_UI <- function(id) {
  
  ns <- NS(id)
  
  fluidRow(
    column(4, 
           radioButtons(inputId = "help_ho_2q",
                        label = h3("Selección de Ayuda Automática"),
                        choices = c("RMedic Here!" = 1,
                                    "Test de Dos Proporciones" = 2,
                                    "Test Chi Cuadrado" = 3,
                                    "Regresión Logística Simple" = 4,
                                    "Otros" = 5)
           )
    ),
    column(8,
           br(),
           conditionalPanel(condition = "input.help_ho_2q == 1",
                            div(
                              h3("RMedic Here!"),
                              HTML(
                                "Las pruebas de hipótesis más utilizados aplicados a una variable categórica son:<br/>
                      - <b>Test de Dos Proporciones</b>.<br/>
                      - <b>Test Chi Cuadrado</b>.<br/>
                      - <b>Regresión Logística Simple</b>.<br/>
                      - <b>Otros</b>.<br/>
                      Seleccionando la ayuda de cada uno encontrarás un resumen con
                      detalles teóricos y estructura de la base de datos.<br/>
                      Estos te ayudarán a determinar si estas herramientas pueden ser
                      aplicadas en tu trabajo."
                              )
                            )
           ),
           conditionalPanel(condition = "input.help_ho_2q == 2",
                            div(
                              h3("Test de Dos Proporciones"),
                              HTML(
                                "Dadas dos variables categóricas, se selecciona una categoría de cada una y se determina si las proporciones de las categorías seleccionada de cada variable son iguales o no."
                              )
                            )
           ),
           conditionalPanel(condition = "input.help_ho_2q == 3",
                            div(
                              h3("Test Chi Cuadrado"),
                              HTML(
                                "Dadas dos variables categóricas, se determina si en su tabla de distribución de frecuencias las variables están relacionadas o no."
                              )
                            )
           ),
           conditionalPanel(condition = "input.help_ho_2q == 4",
                            div(
                              h3("Regresión Logística Simple"),
                              HTML(
                                "Regresión Logística Simple"
                              )
                            )
           ),
           conditionalPanel(condition = "input.help_ho_2q == 5",
                            div(
                              h3("Otros"),
                              HTML(
                                "Otros"
                              )
                            )
           )
    )
  )
  
  
  
}






## Segmento del server
Ho2Q_01_RMedicHelp_SERVER <- function(input, output, session,
                                            minibase,
                                            decimales,
                                            control_ejecucion,
                                            tablas_2q,
                                            alfa) {
  
  
  
  
  
  
  
  
  
  
  
  
}







