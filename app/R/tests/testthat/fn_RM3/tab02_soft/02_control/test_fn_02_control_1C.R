
library(testthat)

my_dataset <- mtcars
selected_col <- "mpg"
df_test_min <- data.frame(
  "Detalle" = "Mìnimo",
  "Valor" = 10.4,
  "Cantidad" = 2L,
  "Posiciones" = "15, 16",
  stringsAsFactors = FALSE,
  check.names = FALSE)

df_test_max <- data.frame(
  "Detalle" = "Máximo",
  "Valor" = 33.9,
  "Cantidad" = 1L,
  "Posiciones" = "20",
  stringsAsFactors = FALSE,
  check.names = FALSE)

df_test_min_max <- rbind.data.frame(df_test_min, df_test_max)

test_that("Control - 1C - Set de Prueba 01 - OK", {
  
  # Verificar que my_dataset no sea NULL
  expect_false(is.null(my_dataset), info = "my_dataset no debe ser NULL.")
  
  # Verificar que es un DataFrame
  expect_true(is.data.frame(my_dataset), info = "df_output debe ser un objeto de tipo data.frame.")
  
  # Verificar dimensiones del DataFrame
  expect_equal(ncol(my_dataset), 11, info = "my_dataset debe tener exactamente 11 columnas.")
  expect_equal(nrow(my_dataset), 32, info = "my_dataset debe tener exactamente 32 filas.")
  
  # Verificar que selected_col sea un vector
  expect_true(is.vector(selected_col), info = "selected_col debe ser un vector.")

  # Verificar que selected_col tenga exactamente 1 elemento
  expect_equal(length(selected_col), 1, info = "selected_col debe tener exactamente 1 elemento.")
  
  # Verificar que selected_col esté en los nombres de las columnas de my_dataset
  expect_true(selected_col %in% colnames(my_dataset), info = "selected_col debe ser un nombre de columna de my_dataset.")
  
  # Verificar que el DataFrame seleccionado tenga las propiedades correctas
  selected_dataframe <- my_dataset[selected_col]
  expect_true(is.data.frame(selected_dataframe), info = "El resultado de my_dataset[selected_col] debe ser un data.frame.")
  expect_equal(ncol(selected_dataframe), 1, info = "El DataFrame seleccionado debe tener exactamente 1 columna.")
  expect_equal(nrow(selected_dataframe), 32, info = "El DataFrame seleccionado debe tener exactamente 32 filas.")  

})

####----------------------------------------------------------------------------------------------



test_that("Control - 1C - Prueba 01 - analyze_min_1c()", {

  df_output <- analyze_min_1c(dataframe = my_dataset, selected_col)
  
  # Verificar que df_output no sea NULL
  expect_false(is.null(df_output), info = "El DataFrame de salida no debe ser NULL.")
  
  # Verificar que es un DataFrame
  expect_true(is.data.frame(df_output), info = "df_output debe ser un objeto de tipo data.frame.")
  
 
  # Verificar que tiene exactamente 4 columnas
  expect_equal(ncol(df_output), 4, info = "El DataFrame debe tener exactamente 4 columnas.")
  
  # Verificar que tiene exactamente 1 fila
  expect_equal(nrow(df_output), 1, info = "El DataFrame debe tener exactamente 1 fila.")
  
  # Verificar que df_output es idéntico a df_test_min
  expect_identical(df_output, df_test_min, info = "El DataFrame de salida debe ser idéntico a df_test_min.")
})

####----------------------------------------------------------------------------------------------



test_that("Control - 1C - Prueba 01 - analyze_max_1c()", {
  
  df_output <- analyze_max_1c(dataframe = my_dataset, selected_col)

  
  # Verificar que df_output no sea NULL
  expect_false(is.null(df_output), info = "El DataFrame de salida no debe ser NULL.")
  
  # Verificar que es un DataFrame
  expect_true(is.data.frame(df_output), info = "df_output debe ser un objeto de tipo data.frame.")  
  # Verificar que tiene al menos una fila y una columna
 
  # Verificar que tiene exactamente 4 columnas y 1 fila
  expect_equal(ncol(df_output), 4, info = "El DataFrame debe tener exactamente 4 columnas.")
  expect_equal(nrow(df_output), 1, info = "El DataFrame debe tener exactamente 1 fila.")
  
  # Verificar que es idéntico a df_test_max
  expect_identical(df_output, df_test_max, info = "El DataFrame de salida debe ser idéntico a df_test_max.")  
})



####----------------------------------------------------------------------------------------------



test_that("Control - 1C - Prueba 01 - analyze_min_y_max_1c()", {
  
  df_output <- analyze_min_y_max_1c(dataframe = my_dataset, selected_col)

  # Verificación de que df_output no sea NULL
  expect_false(is.null(df_output), info = "El DataFrame de salida no debe ser NULL.")
  
  # Verificación de que es un DataFrame
  expect_true(is.data.frame(df_output), info = "df_output debe ser un objeto de tipo data.frame.")
  

  # Verificación de la cantidad de columnas y filas esperadas
  expect_equal(ncol(df_output), 4, info = "El DataFrame debe tener exactamente 4 columnas.")
  expect_equal(nrow(df_output), 2, info = "El DataFrame debe tener exactamente 2 filas.")
  
  # Verificación de que se corresponde con df_test_min_max
  expect_identical(df_output, df_test_min_max, info = "El DataFrame de salida debe ser idéntico a df_test_min_max.")
  
  
})


