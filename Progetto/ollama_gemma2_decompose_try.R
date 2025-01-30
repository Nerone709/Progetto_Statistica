library(httr)
library(jsonlite)
library(dplyr)

# Funzione per inviare un batch all'API di Ollama (in locale) con tentativi multipli
process_batch_with_ollama <- function(batch_df, temperature = 0.5, max_tokens = 4096, max_retries = 3) {
  attempts <- 0
  
  repeat {
    attempts <- attempts + 1
    batch_json <- toJSON(batch_df, dataframe = "rows", pretty = TRUE, auto_unbox = TRUE)
    
    prompt <- paste(
      "You are a helpful assistant. Given the following dataset in csv format, create a new dataset with the same structure. The new dataset should:",
      "1. Retain the statistical properties (e.g., mean, median, mode, standard deviation) of the original dataset.",
      "2. Introduce slight variations to make it distinct.",
      "3. Preserve the number of rows and columns.",
      "4. Match the data types for each column (e.g., continuous, discrete, or categorical).",
      "\n\nInput dataset:",
      batch_json,
      "\n\nRespond ONLY with valid JSON. Do not include explanations or any other text."
    )
    
    response <- POST(
      url = "http://localhost:11434/v1/completions",
      content_type_json(),
      body = list(
        model = "gemma2:latest",
        prompt = prompt,
        temperature = temperature,
        max_tokens = max_tokens
      ),
      encode = "json"
    )
    
    if (response$status_code == 200) {
      response_content <- content(response, as = "parsed")
      generated_json <- response_content$choices[[1]]$text
      
      clean_json <- gsub("^  ⁠json|⁠  $", "", generated_json)
      clean_json <- gsub("\\s+", " ", clean_json)
      clean_json <- gsub("[^[:print:]]", "", clean_json)
      clean_json <- trimws(clean_json)
      
      if (jsonlite::validate(clean_json)) {
        return(fromJSON(clean_json, simplifyDataFrame = TRUE))
      } else {
        cat("Il JSON generato non è valido. Tentativo di rigenerazione... (", attempts, " su ", max_retries, ")\n")
      }
    } else {
      cat("Errore nella richiesta all'API di Ollama. Tentativo di nuovo invio... (", attempts, " su ", max_retries, ")\n")
    }
    
    if (attempts >= max_retries) {
      stop("Numero massimo di tentativi raggiunto per il batch.")
    }
  }
}

# Funzione principale per processare l'intero dataset
decompose_and_process_csv <- function(input_file, output_file, column_groups, batch_size = 1000, temperature = 0.5, max_tokens = 4096, max_retries = 3) {
  input_df <- read.csv(input_file)
  
  results <- list()
  
  for (group_idx in seq_along(column_groups)) {
    selected_columns <- column_groups[[group_idx]]
    cat("Elaborazione del gruppo di colonne", group_idx, "su", length(column_groups), "...\n")
    
    sub_df <- input_df[, selected_columns]
    batches <- split(sub_df, (seq(nrow(sub_df)) - 1) %/% batch_size)
    
    for (i in seq_along(batches)) {
      cat("Elaborazione del batch", i, "di", length(batches), "nel gruppo di colonne", group_idx, "...\n")
      
      tryCatch({
        synthetic_batch <- process_batch_with_ollama(batches[[i]], temperature, max_tokens, max_retries)
        results[[paste0("group", group_idx, "_batch", i)]] <- synthetic_batch
      }, error = function(e) {
        cat("Errore nel batch", i, "nel gruppo", group_idx, ":", e$message, "\n")
        write.csv(batches[[i]], paste0("batch_group", group_idx, "_", i, "_error.csv"), row.names = FALSE)
      })
    }
  }
  
  final_df <- bind_cols(results)
  write.csv(final_df, output_file, row.names = FALSE)
  cat("Dataset sintetico salvato in:", output_file, "\n")
}

input_file <- "dataset_filtraggio_finale2.csv"
output_file <- "synthetic_dataset/gemma2/dataset_sintetico_gemma.csv"

column_groups <- list(
  1:7,
  8:14,
  15:23
)

decompose_and_process_csv(input_file, output_file, column_groups, max_retries = 5)
