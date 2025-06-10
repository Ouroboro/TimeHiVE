test_that("read_ts_data legge correttamente i file fissi", {
  # Percorso relativo al file di test
  test_file <- "fixtures/series1.txt" 
  
  # Verifica che il file esista (altrimenti salta il test)
  skip_if_not(file.exists(test_file), "File di test non trovato")
  
  # Esegui il test
  result <- read_ts_data(test_file)
  
  # Verifiche
  expect_type(result, "double")
  expect_length(result, 20)
  expect_equal(round(result, 1), c(1.2, 2.6, 3.6, 4.4, 5.4, 6.3, 7.5, 8.1, 9.1, 10.1, 11.6, 12.6, 13.6, 14.5, 15.2, 16.2, 17.6, 18.1, 19.3, 20.3))
})

test_that("read_ts_data gestisce gli errori", {
  expect_error(read_ts_data("file_inesistente.txt"), "File not found")
})