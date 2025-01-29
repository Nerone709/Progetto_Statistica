library(httr)
library(jsonlite)
library(dplyr)

# Funzione per inviare un batch all'API di OpenAI con gestione degli errori e tentativi
process_batch_with_openai <- function(batch_df, api_key, temperature = 0.5, max_tokens = 2048, max_retries = 3) {
  batch_df <- batch_df %>%
    mutate_all(trimws) %>%                  # Rimuove spazi vuoti in testa e coda
    mutate_all(~ ifelse(. == "", NA, .)) %>% # Sostituisce stringhe vuote con NA
    na.omit()                               # Rimuove righe con NA
  
  batch_json <- toJSON(batch_df, dataframe = "rows", pretty = FALSE, auto_unbox = TRUE)
  
  prompt <- paste(
    "You are a helpful assistant. Given the following dataset in JSON format, create a new dataset with the same structure. The new dataset should:",
    "1. Retain the statistical properties (e.g., mean, median, mode, standard deviation) of the original dataset.",
    "2. Introduce slight variations to make it distinct.",
    "3. Preserve the number of rows and columns.",
    "4. Match the data types for each column (e.g., continuous, discrete, or categorical).",
    "Provide the new dataset as valid JSON, compact and without additional text or comments. Input dataset:",
    batch_json
  )
  
  retries <- 0
  while (retries < max_retries) {
    response <- POST(
      url = "https://api.openai.com/v1/chat/completions",
      add_headers(Authorization = paste("Bearer", api_key)),
      content_type_json(),
      body = list(
        model = "gpt-4o",
        messages = list(
          list(role = "system", content = "You are a helpful assistant designed to process and transform datasets."),
          list(role = "user", content = prompt)
        ),
        temperature = temperature,
        max_tokens = max_tokens
      ),
      encode = "json"
    )
    
    if (response$status_code == 200) {
      response_content <- content(response, as = "parsed")
      generated_json <- response_content$choices[[1]]$message$content
      
      tryCatch({
        synthetic_df <- fromJSON(generated_json, simplifyDataFrame = TRUE)
        return(synthetic_df)
      }, error = function(e) {
        cat("Errore durante la conversione del JSON in dataframe:", e$message, "\n")
      })
    } else {
      cat("Errore nella richiesta all'API, tentativo", retries + 1, "di", max_retries, "\n")
      cat("Messaggio di errore:", content(response, as = "text"), "\n")
    }
    retries <- retries + 1
  }
  stop("Errore: impossibile processare il batch dopo", max_retries, "tentativi.")
}

# Funzione principale per processare l'intero dataset
decompose_and_process_csv <- function(input_file, output_file, api_key, column_groups, batch_size = 10, temperature = 0.5, max_tokens = 2048, max_retries = 3) {
  input_df <- read.csv(input_file)
  input_df <- input_df %>%
    distinct() %>%         # Rimuove i duplicati
    mutate_all(trimws)     # Rimuove spazi vuoti in testa e coda
  
  results <- list()
  
  for (group_idx in seq_along(column_groups)) {
    selected_columns <- column_groups[[group_idx]]
    cat("Elaborazione del gruppo di colonne", group_idx, "su", length(column_groups), "...\n")
    
    sub_df <- input_df[, selected_columns]
    batches <- split(sub_df, (seq(nrow(sub_df)) - 1) %/% batch_size)
    
    for (i in seq_along(batches)) {
      cat("Elaborazione del batch", i, "di", length(batches), "nel gruppo di colonne", group_idx, "...\n")
      tryCatch({
        synthetic_batch <- process_batch_with_openai(
          batches[[i]], api_key, temperature, max_tokens, max_retries
        )
        results[[paste0("group", group_idx, "_batch", i)]] <- synthetic_batch
      }, error = function(e) {
        cat("Errore nel batch", i, "nel gruppo", group_idx, ":", e$message, "\n")
      })
    }
  }
  
  final_df <- bind_cols(results)
  write.csv(final_df, output_file, row.names = FALSE)
  cat("Dataset sintetico salvato in:", output_file, "\n")
}

api_key <- "XXXX"
input_file <- "dataset_filtraggio_finale2.csv"
output_file <- "dataframe_chatgpt/gpt4/dataset_sintetico_finale.csv"

column_groups <- list(
  1:7,
  8:14,
  15:23
)

decompose_and_process_csv(input_file, output_file, api_key, column_groups)
