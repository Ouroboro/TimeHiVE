# Funzione per creare una lista di matrici, una per ogni anno
create_data_list <- function(start_year = 1998, end_year = 2015) {
  # Inizializziamo una lista per raccogliere i dati
  all_data <- list()
  
  # Ciclo su ogni anno per estrarre i dati dai file
  for (n in start_year:end_year) {
    # Definisci il percorso del file per fileT e fileH
    fileT <- file.path(getwd(), "Dati_Arpa", sprintf("%d", n), sprintf("bsT%d.txt", n))
    fileH <- file.path(getwd(), "Dati_Arpa", sprintf("%d", n), sprintf("bsH%d.txt", n))
    
    # Leggi i dati dai file
    vecT <- scan(fileT, what = numeric(), quiet = TRUE)  # Vettore dei dati fileT
    vecH <- scan(fileH, what = numeric(), quiet = TRUE)  # Vettore dei dati fileH
    
    # Verifica che i vettori abbiano la stessa lunghezza per l'anno in questione
    if (length(vecT) != length(vecH)) {
      stop(sprintf("I file dell'anno %d non hanno la stessa lunghezza. vecT = %d, vecH = %d", n, length(vecT), length(vecH)))
    }
    
    # Crea una matrice combinando i dati T e H
    matrix_data <- cbind(vecT, vecH)
    
    # Aggiungi la matrice alla lista dei dati
    all_data[[as.character(n)]] <- matrix_data
  }
  
  return(all_data)
}

# Esegui la funzione
data_list <- create_data_list()

# Visualizza i dati per un determinato anno
print(data_list[["2000"]])  # Dati dell'anno 2000

save(data_list, file = "data_list.RData")
