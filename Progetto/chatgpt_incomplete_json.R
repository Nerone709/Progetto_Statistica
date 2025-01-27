library(httr)
library(jsonlite)
library(dplyr)

# Funzione per generare un nuovo dataframe utilizzando OpenAI
generate_synthetic_csv <- function(input_file, output_file, api_key, temperature = 0.5, max_tokens = 4096) {
  # Carica il file CSV e convertilo in formato JSON
  input_df <- read.csv(input_file)
  input_df <- input_df %>% distinct()
  input_df <- as.data.frame(input_df[1:50, 1:7])
  input_json <- toJSON(input_df, dataframe = "rows", pretty = TRUE)
  
  # Crea il prompt per l'API
  prompt <- paste(
    "You are a helpful assistant. Given the following dataset in JSON format, create a new dataset with the same structure. The new dataset should:
    1. Retain the statistical properties (e.g., mean, median, mode, standard deviation) of the original dataset.
    2. Introduce slight variations to make it distinct.
    3. Preserve the number of rows and columns.
    4. Match the data types for each column (e.g., continuous, discrete, or categorical).
    Provide the new dataset as valid JSON with no additional text or comments. Input dataset:",
    input_json
  )
  
  # Esegui la richiesta all'API di OpenAI
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
  
  # Controlla se la richiesta è andata a buon fine
  if (response$status_code != 200) {
    stop(paste("Errore nella richiesta all'API:", content(response, as = "text")))
  }
  
  # Estrai il contenuto della risposta
  response_content <- content(response, as = "parsed")
  generated_json <- response_content$choices[[1]]$message$content
  
  # Debug: Visualizza l'output generato
  cat("Output JSON generato dall'API:\n", generated_json, "\n")
  
  # Prova a convertire l'output in un dataframe
  synthetic_df <- tryCatch(
    fromJSON(generated_json),
    error = function(e) {
      stop("Errore nella conversione del risultato in dataframe:", e)
    }
  )
  
  # Salva il nuovo dataframe in un file CSV
  write.csv(synthetic_df, output_file, row.names = FALSE)
  cat("Nuovo dataset salvato in:", output_file, "\n")
}

# Inserisci la tua chiave API
api_key <- "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"

# Specifica i file di input e output
input_file <- "dataset_filtraggio_finale2.csv"
output_file <- "dataframe_chatgpt/gpt4/dataset_sintetico.csv"

# Genera il nuovo dataset sintetico
generate_synthetic_csv(input_file, output_file, api_key)
