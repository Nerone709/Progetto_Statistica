library(httr)
library(jsonlite)
library(dplyr)

# Funzione per inviare un batch all'API di Ollama (in locale) con retry
process_batch_with_ollama <- function(batch_df, temperature = 0.5, max_tokens = 4096, max_retries = 3) {
  # Converte il batch in JSON
  batch_json <- toJSON(batch_df, dataframe = "rows", pretty = TRUE, auto_unbox = TRUE)
  
  # Crea il prompt
  prompt <- paste(
    "You are a helpful assistant. Given the following dataset in JSON format, create a new dataset with the same structure. The new dataset should:
  1. Retain the statistical properties (e.g., mean, median, mode, standard deviation) of the original dataset.
  2. Introduce slight variations to make it distinct.
  3. Preserve the number of rows and columns.
  4. Match the data types for each column (e.g., continuous, discrete, or categorical).
  5. Ensure that numeric columns in the input remain numeric in the output.
  
Input dataset:",
    batch_json,
    "\n\nRespond ONLY with valid JSON. Do not include explanations or any other text."
  )
  
  # Tentativi di invio della richiesta
  retries <- 0
  success <- FALSE
  generated_json <- NULL
  
  while (retries < max_retries & !success) {
    retries <- retries + 1
    cat("Tentativo", retries, "di", max_retries, "...\n")
    
    # Richiesta all'API di Ollama (in locale)
    response <- tryCatch({
      POST(
        url = "http://localhost:11434/v1/completions", # Endpoint di Ollama locale
        content_type_json(),
        body = list(
          model = "phi4:latest",  # Il modello Phi4 da usare
          prompt = prompt,
          temperature = temperature,
          max_tokens = max_tokens
        ),
        encode = "json"
      )
    }, error = function(e) {
      cat("Errore nella richiesta:", e$message, "\n")
      NULL
    })
    
    # Se la risposta è valida, processa il JSON
    if (!is.null(response) && response$status_code == 200) {
      response_content <- content(response, as = "parsed")
      generated_json <- response_content$choices[[1]]$text
      
      # Pulizia del JSON di risposta
      clean_json <- gsub("^```json|```$", "", generated_json) # Rimuove i delimitatori json
      clean_json <- gsub("\\s+", " ", clean_json)            # Rimuove spazi multipli
      clean_json <- gsub("[^[:print:]]", "", clean_json)     # Rimuove caratteri non stampabili
      clean_json <- trimws(clean_json)                      # Rimuove spazi bianchi iniziali e finali
      
      # Controlla se il JSON estratto è valido
      if (jsonlite::validate(clean_json)) {
        success <- TRUE
      } else {
        cat("Errore nel JSON ricevuto. Riprovo...\n")
      }
    } else {
      cat("Errore nella risposta dell'API. Riprovo...\n")
    }
    
    # Aggiungi una pausa prima del prossimo tentativo per evitare richieste eccessive
    if (!success) {
      Sys.sleep(2)  # Pausa di 2 secondi tra i tentativi
    }
  }
  
  # Se non si è ottenuto un risultato valido dopo il numero massimo di tentativi, interrompe
  if (!success) {
    stop("Impossibile ottenere una risposta valida dall'API dopo", max_retries, "tentativi.")
  }
  
  # Converte l'output in dataframe
  synthetic_df <- fromJSON(clean_json, simplifyDataFrame = TRUE)
  return(synthetic_df)
}

# Funzione principale per processare il dataset suddividendo in batch combinati di righe e colonne
process_large_csv <- function(input_file, output_file, row_batch_size = 100, col_batch_size = 23, temperature = 0.5, max_tokens = 4096) {
  # Carica il file CSV
  input_df <- read.csv(input_file)
  input_df <- input_df %>% distinct()  # Rimuove i duplicati
  
  # Suddivisione del dataset in batch combinati
  row_batches <- split(1:nrow(input_df), (seq_len(nrow(input_df)) - 1) %/% row_batch_size)
  col_batches <- split(names(input_df), (seq_along(names(input_df)) - 1) %/% col_batch_size)
  
  # Inizializza una lista per i risultati
  all_results <- list()
  
  # Itera su batch combinati di righe e colonne
  batch_idx <- 1
  for (row_batch_idx in seq_along(row_batches)) {
    for (col_batch_idx in seq_along(col_batches)) {
      # Sottoselezione del dataset
      row_indices <- row_batches[[row_batch_idx]]
      col_names <- col_batches[[col_batch_idx]]
      batch_df <- input_df[row_indices, col_names, drop = FALSE]
      
      cat("Elaborazione batch righe", row_batch_idx, "e colonne", col_batch_idx, "...\n")
      
      tryCatch({
        # Elabora il batch con l'API di Ollama
        synthetic_batch <- process_batch_with_ollama(batch_df, temperature, max_tokens)
        all_results[[batch_idx]] <- synthetic_batch
        batch_idx <- batch_idx + 1
      }, error = function(e) {
        cat("  Errore nel batch righe", row_batch_idx, "colonne", col_batch_idx, ":", e$message, "\n")
        # Salva il batch problematico per debug
        problematic_file <- paste0("errori_batch/row_batch_", row_batch_idx, "_col_batch_", col_batch_idx, "_error.csv")
        write.csv(batch_df, problematic_file, row.names = FALSE)
        warning(paste("Batch problematico salvato in:", problematic_file))
      })
    }
  }
  
  # Combina tutti i risultati dei batch
  final_df <- do.call(rbind, all_results)
  
  # Salva il dataset sintetico
  write.csv(final_df, output_file, row.names = FALSE)
  cat("Dataset sintetico salvato in:", output_file, "\n")
}

# Specifica i file di input e output
input_file <- "dataset_filtraggio_finale2.csv"
output_file <- "synthetic_dataset/phi4/dataset_sintetico_finale.csv"

# Esegui l'elaborazione
process_large_csv(input_file, output_file, row_batch_size = 50, col_batch_size = 10)