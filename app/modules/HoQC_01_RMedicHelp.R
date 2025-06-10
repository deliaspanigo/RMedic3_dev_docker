


HoQC_01_RMedicHelp_UI <- function(id) {
  
  ns <- NS(id)
  
  fluidRow(
    column(4,
           radioButtons(inputId = "help_ho_qc",
                        label = h3("Selección de Ayuda Automática"),
                        choices = c("RMedic Here!" = 1,
                                    "Test t (Dos muestras independientes)" = 2,
                                    "Test Mann-Whitney-Wilcoxon (Dos muestras independientes)" = 3,
                                    "Test Anova a 1 Factor" = 4,
                                    "Test Kruskal-Wallis" = 5,
                                    "Test de Homogeiendiad de varianzas de Fisher" = 6,
                                    "Test de Homogeiendiad de varianzas de Bartlett" = 7,
                                    "Test de Homogeiendiad de varianzas de Levene" = 8,
                                    "Test de Normalidad Shapiro-Wilk (Particionado)" = 9,
                                    "Test de Regresión Logística Simple" = 10)
           )),
    column(8,
           conditionalPanel(condition = "input.help_ho_qc == 1", 
                            div(
                              h3("RMedic Here!"),
                              HTML(
                                "Las pruebas de hipótesis más utilizados aplicados a dos variables siendo 1 categórica y 1 numérica son:<br/>
                      - <b>Test t (Dos muestras independientes)</b>.<br/>
                      - <b>Test Mann-Whitney-Wilcoxon (Dos muestras independientes)</b>.<br/>
                      - <b>Test Anova a 1 Factor</b>.<br/>
                      - <b>Test Kruskal-Wallis</b>.<br/>
                      - <b>Test de Homogeiendiad de varianzas de Fisher</b>.<br/>
                      - <b>Test de Homogeiendiad de varianzas de Bartlett</b>.<br/>
                      - <b>Test de Homogeiendiad de varianzas de Levene</b>.<br/>
                      - <b>Test de Normalidad Shapiro-Wilk (Particionadi)</b>.<br/>
                      - <b>Test de Regresión Logística Simple</b>.<br/>

                      Seleccionando la ayuda de cada uno encontrarás un resumen con
                      detalles teóricos y estructura de la base de datos.<br/>
                      Estos te ayudarán a determinar si estas herramientas pueden ser
                      aplicadas en tu trabajo."
                              )
                            )
                            ),
           conditionalPanel(condition = "input.help_ho_qc == 2", 
                            div(
                              h3("Test t (Dos muestras independientes)"),
                              HTML(
                                "Test t (Dos muestras independientes)."
                                )
                              )
                            ),
           conditionalPanel(condition = "input.help_ho_qc == 3", 
                            div(
                              h3("Test Mann-Whitney-Wilcoxon (Dos muestras independientes)"),
                              HTML(
                                "Test Mann-Whitney-Wilcoxon (Dos muestras independientes)."
                                )
                            )
           ),
           conditionalPanel(condition = "input.help_ho_qc == 4", 
                            div(
                              h3("Test Anova a 1 Factor"),
                              HTML(
                                "Test Anova a 1 Factor"
                              )
                            )
           ),
           conditionalPanel(condition = "input.help_ho_qc == 5", 
                            div(
                              h3("Kruskal-Wallis"),
                              HTML(
                                "Kruskal-Wallis."
                              )
                            )
           ),
           conditionalPanel(condition = "input.help_ho_qc == 6", 
                            div(
                              h3("Test de Homogeiendiad de varianzas de Fisher"),
                              HTML(
                                "Test de Homogeiendiad de varianzas de Fisher."
                                )
                            )
           ),
           conditionalPanel(condition = "input.help_ho_qc == 7", 
                            div(
                              h3("Test de Homogeiendiad de varianzas de Bartlett"),
                              HTML(
                                "Test de Homogeiendiad de varianzas de Bartlett."
                              )
                            )
           ),
           conditionalPanel(condition = "input.help_ho_qc == 8", 
                            div(
                              h3("Test de Homogeiendiad de varianzas de Levene"),
                              HTML(
                                "Test de Homogeiendiad de varianzas de Levene."
                              )
                            )
           ),
           conditionalPanel(condition = "input.help_ho_qc == 9", 
                            div(
                              h3("Test de Normalidad Shapiro-Wilk (Particionado)"),
                              HTML(
                                "Test de Normalidad Shapiro-Wilk (Particionado)."
                              )
                            )
           ),
           conditionalPanel(condition = "input.help_ho_qc == 10", 
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
HoQC_01_RMedicHelp_SERVER <- function(input, output, session,
                                            minibase,
                                            decimales,
                                            control_ejecucion,
                                            tabla_qc,
                                            alfa) {
  
  
  
  
  
  
  
  
  
  
  
  
}