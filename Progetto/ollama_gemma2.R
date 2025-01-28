library(httr)
library(jsonlite)
library(dplyr)

# Funzione per inviare un batch all'API di Ollama (in locale)
process_batch_with_ollama <- function(batch_df, temperature = 0.5, max_tokens = 4096) {
  repeat {
    # Converte il batch in JSON
    batch_json <- toJSON(batch_df, dataframe = "rows", pretty = TRUE, auto_unbox = TRUE)
    
    # Crea il prompt
    prompt <- paste(
      "You are a helpful assistant. Given the following dataset in JSON format, create a new dataset with the same structure. The new dataset should:",
      "1. Retain the statistical properties (e.g., mean, median, mode, standard deviation) of the original dataset.",
      "2. Introduce slight variations to make it distinct.",
      "3. Preserve the number of rows and columns.",
      "4. Match the data types for each column (e.g., continuous, discrete, or categorical).",
      "\n\nInput dataset:",
      batch_json,
      "\n\nRespond ONLY with valid JSON. Do not include explanations or any other text."
    )
    
    # Richiesta all'API di Ollama (in locale)
    response <- POST(
      url = "http://localhost:11434/v1/completions",
      content_type_json(),
      body = list(
        model = "gemma2:latest",  # Modello Gemma2
        prompt = prompt,
        temperature = temperature,
        max_tokens = max_tokens
      ),
      encode = "json"
    )
    
    # Controlla lo stato della risposta
    if (response$status_code != 200) {
      cat("Errore nella richiesta all'API di Ollama. Tentativo di nuovo invio...\n")
      next
    }
    
    # Estrai il contenuto della risposta
    response_content <- content(response, as = "parsed")
    generated_json <- response_content$choices[[1]]$text
    
    # Pulizia del JSON di risposta
    clean_json <- gsub("^  ⁠json|⁠  $", "", generated_json)
    clean_json <- gsub("\\s+", " ", clean_json)
    clean_json <- gsub("[^[:print:]]", "", clean_json)
    clean_json <- trimws(clean_json)
    
    # Controlla se il JSON estratto è valido
    if (jsonlite::validate(clean_json)) {
      return(fromJSON(clean_json, simplifyDataFrame = TRUE))
    } else {
      cat("Il JSON generato non è valido. Tentativo di rigenerazione...\n")
    }
  }
}

# Funzione principale per processare l'intero dataset
process_large_csv <- function(input_file, output_file, batch_size = 25, temperature = 0.5, max_tokens = 4096) {
  # Carica il file CSV
  input_df <- read.csv(input_file)
  input_df <- input_df %>% distinct()  # Rimuove i duplicati
  
  # Divide il dataset in batch
  batches <- split(input_df, (seq(nrow(input_df)) - 1) %/% batch_size)
  
  # Inizializza una lista per i risultati
  results <- list()
  
  # Elabora ogni batch
  for (i in seq_along(batches)) {
    cat("Elaborazione del batch", i, "di", length(batches), "...\n")
    tryCatch({
      synthetic_batch <- process_batch_with_ollama(batches[[i]], temperature, max_tokens)
      results[[i]] <- synthetic_batch
    }, error = function(e) {
      cat("Errore nel batch", i, ":", e$message, "\n")
      # Salva il batch problematico per debug
      write.csv(batches[[i]], paste0("batch_", i, "_error.csv"), row.names = FALSE)
    })
  }
  
  # Combina tutti i risultati
  final_df <- do.call(rbind, results)
  
  # Salva il dataset sintetico
  write.csv(final_df, output_file, row.names = FALSE)
  cat("Dataset sintetico salvato in:", output_file, "\n")
}

# Specifica i file di input e output
input_file <- "dataset_filtraggio_finale2.csv"
output_file <- "synthetic_dataset/gemma2/dataset_sintetico_finale.csv"

# Esegui l'elaborazione
process_large_csv(input_file, output_file)