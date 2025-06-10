


Ho1C_01_RMedicHelp_UI <- function(id) {
  
  ns <- NS(id)
  
  fluidRow(
    column(4,
           radioButtons(inputId = "help_ho_1c",
                        label = h3("Selección de Ayuda Automática"),
                        choices = c("RMedic Here!" = 1,
                                    "Test t (Una muestra)" = 2,
                                    "Test de Wilcoxon (Una muestra)" = 3,
                                    "Test Normalidad (Shapiro-Wilk)" = 4,
                                    "Test Chi Cuadrado (Una muestra)" = 5)
           )),
    column(8,
           conditionalPanel(condition = "input.help_ho_1c == 1", 
                            div(
                              h3("RMedic Here!"),
                              HTML(
                                "Los gráficos más utilizados aplicados a una variable numérica son:<br/>
                      - <b>Test t (Una muestra)</b>.<br/>
                      - <b>Test de Wilcoxon (Una muestra)</b>.<br/>
                      - <b>Test Normalidad (Shapiro-Wilk)</b>.<br/>
                      - <b>Test Chi Cuadrado (Una muestra)</b>.<br/>
                      Seleccionando la ayuda de cada uno encontrarás un resumen con
                      detalles teóricos y estructura de la base de datos.<br/>
                      Estos te ayudarán a determinar si estas herramientas pueden ser
                      aplicadas en tu trabajo."
                              )
                            )
                            ),
           conditionalPanel(condition = "input.help_ho_1c == 2", 
                            div(
                              h3("Test t (Una muestra)"),
                              HTML(
                                "Se aplica sore una variable cuantitativa. Se realiza una prueba de hipótesis para determinar si la media muestras es igual o no a un valor particular de media poblacional definido por el operador."
                                )
                              )
                            ),
           conditionalPanel(condition = "input.help_ho_1c == 3", 
                            div(
                              h3("Test de Wilcoxon (Una muestra)"),
                              HTML(
                                "Se aplica sobre una variable cuantitativa u ordinal representada con números."
                                )
                            )
           ),
           conditionalPanel(condition = "input.help_ho_1c == 4", 
                            div(
                              h3("Test Normalidad (Shapiro-Wilk)"),
                              HTML(
                                "Se aplica sobre una variable cuantitativa. Pone a prueba la hipótesis de que la variable seleccionada posee distribución normal."
                              )
                            )
           ),
           conditionalPanel(condition = "input.help_ho_1c == 5", 
                            div(
                              h3("Test Chi Cuadrado (Una muestra)"),
                              HTML(
                                "Test Chi Cuadrado (Una muestra)"
                              )
                            )
           )
           )
           )
  
  
  
}






## Segmento del server
Ho1C_01_RMedicHelp_SERVER <- function(input, output, session,
                                            minibase,
                                            decimales,
                                            control_ejecucion,
                                            tablas_1c,
                                            alfa) {
  
  
  
  
  
  
  
  
  
  
  
  
}







