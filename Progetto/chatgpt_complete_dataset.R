library(httr)
library(jsonlite)
library(dplyr)

# Funzione per inviare un batch all'API di OpenAI
process_batch_with_openai <- function(batch_df, api_key, temperature = 0.5, max_tokens = 2048) {
  # Converte il batch in JSON
  batch_json <- toJSON(batch_df, dataframe = "rows", pretty = TRUE)
  
  # Crea il prompt
  prompt <- paste(
    "You are a helpful assistant. Given the following dataset in JSON format, create a new dataset with the same structure. The new dataset should:
    1. Retain the statistical properties (e.g., mean, median, mode, standard deviation) of the original dataset.
    2. Introduce slight variations to make it distinct.
    3. Preserve the number of rows and columns.
    4. Match the data types for each column (e.g., continuous, discrete, or categorical).
    Provide the new dataset as valid JSON with no additional text or comments. Input dataset:",
    batch_json
  )
  
  # Richiesta all'API
  response <- POST(
    url = "https://api.openai.com/v1/chat/completions",
    add_headers(Authorization = paste("Bearer", api_key)),
    content_type_json(),
    body = list(
      model = "gpt-4",
      messages = list(
        list(role = "system", content = "You are a helpful assistant designed to process and transform datasets."),
        list(role = "user", content = prompt)
      ),
      temperature = temperature,
      max_tokens = max_tokens
    ),
    encode = "json"
  )
  
  # Controlla lo stato della risposta
  if (response$status_code != 200) {
    stop(paste("Errore nella richiesta all'API:", content(response, as = "text")))
  }
  
  # Estrai il contenuto della risposta
  response_content <- content(response, as = "parsed")
  generated_json <- response_content$choices[[1]]$message$content
  
  # Converte l'output in dataframe
  synthetic_df <- fromJSON(generated_json, simplifyDataFrame = TRUE)
  return(synthetic_df)
}

# Funzione principale per processare l'intero dataset
process_large_csv <- function(input_file, output_file, api_key, batch_size = 10, temperature = 0.5, max_tokens = 4096) {
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
      synthetic_batch <- process_batch_with_openai(batches[[i]], api_key, temperature, max_tokens)
      results[[i]] <- synthetic_batch
    }, error = function(e) {
      cat("Errore nel batch", i, ":", e$message, "\n")
    })
  }
  
  # Combina tutti i risultati
  final_df <- do.call(rbind, results)
  
  # Salva il dataset sintetico
  write.csv(final_df, output_file, row.names = FALSE)
  cat("Dataset sintetico salvato in:", output_file, "\n")
}

# Inserisci la tua chiave API
api_key <- "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"

# Specifica i file di input e output
input_file <- "dataset_filtraggio_finale2.csv"
output_file <- "dataframe_chatgpt/gpt4/dataset_sintetico_finale.csv"

# Esegui l'elaborazione
process_large_csv(input_file, output_file, api_key)
