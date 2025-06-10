


Ho2C_01_RMedicHelp_UI <- function(id) {
  
  ns <- NS(id)
  
  fluidRow(
    column(4,
           radioButtons(inputId = "help_ho_2c",
                        label = h3("Selección de Ayuda Automática"),
                        choices = c("RMedic Here!" = 1,
                                    "Test de Correlación de Pearson" = 2,
                                    "Test de Correlación de Spearman" = 3,
                                    "Test de Regresión Lineal Simple" = 4,
                                    "Test t (Dos muestras apareadas)" = 5,
                                    "Test Wilcoxon (Dos muestras apareadas)" = 6,
                                    "Test de Homogeneidad de varianzas de Fisher" = 7,
                                    "Test de Homogeneidad de varianzas de Bartlett" = 8,
                                    "Test de Homogeneidad de varianzas de Levenne" = 9,
                                    "Test de Regresión Logística Simple" = 10)
           )),
    column(8,
           conditionalPanel(condition = "input.help_ho_2c == 1", 
                            div(
                              h3("RMedic Here!"),
                              HTML(
                                "Los gráficos más utilizados aplicados a una variable numérica son:<br/>
                      - <b>Test de Correlación de Pearson</b>.<br/>
                      - <b>Test de Correlación de Spearman</b>.<br/>
                      - <b>Test de Regresión Lineal Simple</b>.<br/>
                      - <b>Test t (Dos muestras apareadas)</b>.<br/>
                      - <b>Test Wilcoxon (Dos muestras apareadas)</b>.<br/>
                      - <b>Test de Homogeneidad de varianzas de Fisher</b>.<br/>
                      - <b>Test de Homogeneidad de varianzas de Bartlett</b>.<br/>
                      - <b>Test de Homogeneidad de varianzas de Levenne</b>.<br/>
                      - <b>Test de Regresión Logística Simple</b>.<br/>
                      Seleccionando la ayuda de cada uno encontrarás un resumen con
                      detalles teóricos y estructura de la base de datos.<br/>
                      Estos te ayudarán a determinar si estas herramientas pueden ser
                      aplicadas en tu trabajo."
                              )
                            )
                            ),
           conditionalPanel(condition = "input.help_ho_2c == 2", 
                            div(
                              h3("Test de Correlación de Pearson"),
                              HTML(
                                "Test de Correlación de Pearson."
                                )
                              )
                            ),
           conditionalPanel(condition = "input.help_ho_2c == 3", 
                            div(
                              h3("Test de Correlación de Spearman"),
                              HTML(
                                "Test de Correlación de Spearman."
                                )
                            )
           ),
           conditionalPanel(condition = "input.help_ho_2c == 4", 
                            div(
                              h3("Test de Regresión Lineal Simple"),
                              HTML(
                                "Test de Regresión Lineal Simple."
                              )
                            )
           ),
           conditionalPanel(condition = "input.help_ho_2c == 5", 
                            div(
                              h3("Test t (Dos muestras apareadas)"),
                              HTML(
                                "Test t (Dos muestras apareadas)."
                              )
                            )
           ),
           conditionalPanel(condition = "input.help_ho_2c == 6", 
                            div(
                              h3("Test de Homogeneidad de varianzas de Fisher"),
                              HTML(
                                "Test de Homogeneidad de varianzas de Fisher.")
                            )
           ),
           conditionalPanel(condition = "input.help_ho_2c == 7", 
                            div(
                              h3("Test de Homogeneidad de varianzas de Bartlett"),
                              HTML(
                                "Test de Homogeneidad de varianzas de Bartlett."
                                )
                            )
           ),
           conditionalPanel(condition = "input.help_ho_2c == 8", 
                            div(
                              h3("Test de Homogeneidad de varianzas de Levenne"),
                              HTML(
                                "Test de Homogeneidad de varianzas de Levenne."
                              )
                            )
           ),
           conditionalPanel(condition = "input.help_ho_2c == 9", 
                            div(
                              h3("Test de Homogeneidad de varianzas de Levenne"),
                              HTML(
                                "Test de Homogeneidad de varianzas de Levenne."
                              )
                            )
           ),
           conditionalPanel(condition = "input.help_ho_2c == 10", 
                            div(
                              h3("Test de Regresión Logística Simple"),
                              HTML(
                                "Test de Regresión Logística Simple."
                              )
                            )
           )
           )
           )
  
  
  
}






## Segmento del server
Ho2C_01_RMedicHelp_SERVER <- function(input, output, session,
                                            minibase,
                                            decimales,
                                            control_ejecucion) {
  
  
  
  
  
  
  
  
  
  
  
  
}