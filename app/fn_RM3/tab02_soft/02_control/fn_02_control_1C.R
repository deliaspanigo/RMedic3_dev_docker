# Crea una función que calcule el mínimo, su cantidad y las posiciones en una columna
analizar_minimo_1c <- function(dataframe, columna) {
  # Extrae la columna especificada
  col_data <- dataframe[[columna]]
  
  # Determina el mínimo
  valor_minimo <- min(col_data, na.rm = TRUE)
  
  # Determina cuántas veces aparece el mínimo
  cantidad_minimo <- sum(col_data == valor_minimo, na.rm = TRUE)
  
  # Determina las posiciones del mínimo
  posiciones <- which(col_data == valor_minimo)
  
  # Verifica si hay más de 5 posiciones
  if (length(posiciones) > 5) {
    # Solo toma las primeras 5 posiciones y crea el texto correspondiente
    primeras_posiciones <- posiciones[1:5]
    texto_posiciones <- paste(primeras_posiciones, collapse = ", ")
    texto_adicional <- " ... (solo las primeras 5 posiciones)"
  } else {
    texto_posiciones <- paste(posiciones, collapse = ", ")
    texto_adicional <- ""
  }
  
  # Crea un dataframe con los resultados
  resultado <- data.frame(
    Detalle = "Mìnimo",
    Valor = valor_minimo,
    Cantidad = cantidad_minimo,
    Posiciones = paste0(texto_posiciones, texto_adicional),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  
  return(resultado)
}

# Crea una función que calcule el máximo, su cantidad y las posiciones en una columna
analizar_maximo_1c <- function(dataframe, columna) {
  # Extrae la columna especificada
  col_data <- dataframe[[columna]]
  
  # Determina el máximo
  valor_maximo <- max(col_data, na.rm = TRUE)
  
  # Determina cuántas veces aparece el máximo
  cantidad_maximo <- sum(col_data == valor_maximo, na.rm = TRUE)
  
  # Determina las posiciones del máximo
  posiciones <- which(col_data == valor_maximo)
  
  # Verifica si hay más de 5 posiciones
  if (length(posiciones) > 5) {
    # Solo toma las primeras 5 posiciones y crea el texto correspondiente
    primeras_posiciones <- posiciones[1:5]
    texto_posiciones <- paste(primeras_posiciones, collapse = ", ")
    texto_adicional <- " ... (solo las primeras 5 posiciones)"
  } else {
    texto_posiciones <- paste(posiciones, collapse = ", ")
    texto_adicional <- ""
  }
  
  # Crea un dataframe con los resultados
  resultado <- data.frame(
    Detalle = "Máximo",
    Valor = valor_maximo,
    Cantidad = cantidad_maximo,
    Posiciones = paste0(texto_posiciones, texto_adicional),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  
  return(resultado)
}

# La funcion hace lo anterior para el minimo y para el maximo
analizar_minimo_y_maximo_1c <- function(dataframe, columna) {
  
  new_cols <- ""
  df_min <- analizar_minimo_1c(dataframe, columna)
  df_max <- analizar_maximo_1c(dataframe, columna)
  
  df_output <- rbind.data.frame(df_min, df_max)
  
  return(df_output)
}

analizar_frase_min_max_1c <- function(){
  
  output_list <- list(
    "text01" = "Observe si los valores mínimos y máximos de la variable tienen sentido en el marco de la experiencia."
    )
  
  return(output_list)
}

analizar_n_df_1c <- function(dataframe, columna){
  
  
  n_base <- nrow(dataframe)
  
  minibase <- na.omit(dataframe)

  n_minibase <- nrow(minibase)
  
  n_na <- n_base - n_minibase
  
  df_output <- data.frame(
    "Total de filas" = n_base,
    "Filas con datos" = n_minibase,
    "Filas con celdas vacías" = n_na,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  
  return(df_output)
}

analizar_n_frase_1c <- function(df_n){
  
  n_base     <- df_n[1,1]
  n_minibase <- df_n[1,2]
  n_na       <- df_n[1,3]
  
  frase_01 <- "La columna seleccionada no presenta filas con celdas vacias. Al utilizar la columna seleccionada el 'n' será _n_base_."
  frase_02 <- "La columna seleccionada presenta _n_na_ filas con celdas. Al utilizar la columna seleccionada el 'n' será _n_minibase_."
  
  frase_output <- ifelse(n_na == 0, frase_01, frase_02)
  frase_output <- gsub(pattern = "_n_base_", n_base, frase_output)
  frase_output <- gsub(pattern = "_n_minibase_", n_minibase, frase_output)
  
  return(frase_output)
}

text_title_1c <- function(){
  
  output_list <- list(
    "text01" = "RMedic - Control para 1 Variable Numérica"
  )
  
  return(output_list)
}

texto_info_title_1c <- function(){
  
  output_list <- list(
    "text01" = "- Los valores mínimo y máximo deben tener sentido en el marco de la experiencia.",
    "text02" = "- Corroborar la presencia o no de celdas vacías."
  )
  
  return(output_list)
}

control_1c_full <- function(dataframe, columna){
  
  texto_title <- text_title_1c()
  
  texto_info_title  <- texto_info_title_1c()
  
  frase_min_max <- analizar_frase_min_max_1c()$"text01"
  
  df_min_max <- analizar_minimo_y_maximo_1c(dataframe, columna) 
  
  df_n <- analizar_n_df_1c(dataframe, columna)

  frase_n <-     analizar_n_frase_1c(df_n = df_n)
  
  list_output <- list(
    "texto_title"      = texto_title,
    "texto_info_title" = texto_info_title,
    "frase_min_max"    = frase_min_max,
    "df_min_max"       = df_min_max,
    "frase_n"          = frase_n,
    "df_n"             = df_n
  )
  
  return(list_output)
}

