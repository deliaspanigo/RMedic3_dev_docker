# Crea una función que calcule el mínimo, su cantidad y las posiciones en una selected_col
analyze_min_1c <- function(dataframe, selected_col) {
  # Extrae la selected_col especificada
  col_data <- dataframe[[selected_col]]
  
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
    text_posiciones <- paste(primeras_posiciones, collapse = ", ")
    text_adicional <- " ... (solo las primeras 5 posiciones)"
  } else {
    text_posiciones <- paste(posiciones, collapse = ", ")
    text_adicional <- ""
  }
  
  # Crea un dataframe con los resultados
  resultado <- data.frame(
    Detalle = "Mìnimo",
    Valor = valor_minimo,
    Cantidad = cantidad_minimo,
    Posiciones = paste0(text_posiciones, text_adicional),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  
  return(resultado)
}

# Crea una función que calcule el máximo, su cantidad y las posiciones en una selected_col
analyze_max_1c <- function(dataframe, selected_col) {
  # Extrae la selected_col especificada
  col_data <- dataframe[[selected_col]]
  
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
    text_posiciones <- paste(primeras_posiciones, collapse = ", ")
    text_adicional <- " ... (solo las primeras 5 posiciones)"
  } else {
    text_posiciones <- paste(posiciones, collapse = ", ")
    text_adicional <- ""
  }
  
  # Crea un dataframe con los resultados
  resultado <- data.frame(
    Detalle = "Máximo",
    Valor = valor_maximo,
    Cantidad = cantidad_maximo,
    Posiciones = paste0(text_posiciones, text_adicional),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  
  return(resultado)
}

# La funcion hace lo anterior para el minimo y para el maximo
analyze_min_y_max_1c <- function(dataframe, selected_col) {
  
  new_cols <- ""
  df_min <- analyze_min_1c(dataframe, selected_col)
  df_max <- analyze_max_1c(dataframe, selected_col)
  
  df_output <- rbind.data.frame(df_min, df_max)
  
  return(df_output)
}

analyze_phrase_min_max_1c <- function(){
  
  output_list <- list(
    "text01" = "Observe si los valores mínimos y máximos de la variable tienen sentido en el marco de la experiencia."
    )
  
  return(output_list)
}

analyze_n_df_1c <- function(dataframe, selected_col){
  
  
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

analyze_n_phrase_1c <- function(df_n){
  
  n_base     <- df_n[1,1]
  n_minibase <- df_n[1,2]
  n_na       <- df_n[1,3]
  
  phrase_01 <- "La selected_col seleccionada no presenta filas con celdas vacias. Al utilizar la selected_col seleccionada el 'n' será _n_base_."
  phrase_02 <- "La selected_col seleccionada presenta _n_na_ filas con celdas. Al utilizar la selected_col seleccionada el 'n' será _n_minibase_."
  
  phrase_output <- ifelse(n_na == 0, phrase_01, phrase_02)
  phrase_output <- gsub(pattern = "_n_base_", n_base, phrase_output)
  phrase_output <- gsub(pattern = "_n_minibase_", n_minibase, phrase_output)
  
  return(phrase_output)
}

text_title_1c <- function(){
  
  output_list <- list(
    "text01" = "RMedic - Control para 1 Variable Numérica"
  )
  
  return(output_list)
}

text_info_title_1c <- function(){
  
  output_list <- list(
    "text01" = "- Los valores mínimo y máximo deben tener sentido en el marco de la experiencia.",
    "text02" = "- Corroborar la presencia o no de celdas vacías."
  )
  
  return(output_list)
}

control_1c_full <- function(dataframe, selected_col){
  
  text_title <- text_title_1c()
  
  text_info_title  <- text_info_title_1c()
  
  phrase_min_max <- analyze_phrase_min_max_1c()$"text01"
  
  df_min_max <- analyze_min_y_max_1c(dataframe, selected_col) 
  
  df_n <- analyze_n_df_1c(dataframe, selected_col)

  phrase_n <-     analyze_n_phrase_1c(df_n = df_n)
  
  list_output <- list(
    "text_title"      = text_title,
    "text_info_title" = text_info_title,
    "phrase_min_max"    = phrase_min_max,
    "df_min_max"       = df_min_max,
    "phrase_n"          = phrase_n,
    "df_n"             = df_n
  )
  
  return(list_output)
}

