library(httr)
library(jsonlite)
library(dplyr)

# Funzione per inviare richieste all'API di OpenAI
generate_dataframe_with_chatgpt <- function(input_df, api_key, temperature = 0.5, max_tokens = 1024) {
  # Converti il dataframe in formato JSON
  input_json <- toJSON(input_df, dataframe = "rows", pretty = TRUE)
  
  # Crea il prompt per l'API
  prompt <- paste(
    "Dati i seguenti numeri in formato JSON:",
    input_json,
    "Genera un nuovo dataframe in formato JSON con numeri binari (0 e 1) simili ma modificati. Rispondi solo con un JSON valido senza testo aggiuntivo."
  )
  # Prompt engineering strategies applied from OpenAI cookbook:
  # 1) Write clear instruction 2) Provide reference text 3) Split complex task into subtask
  
  prompt_refined <- paste(
    "Using the provided numerical dataset, generate a new numerical dataset that retains the statistical characteristics of the original. Follow these guidelines:
    1. Analyze the provided dataset to determine its key statistical properties, including mean, median, mode, standard deviation, variance, and distribution shape (e.g., normal, skewed).
    2. Create a new dataset that has the same number of entries and columns as the original.
    3. Ensure that the new dataset follows a similar distribution pattern as the original, while introducing slight variations to create a distinct dataset.
    4. Use numeric values that match the type of each column in the input dataset (e.g., continuous, discrete, or categorical).
    Input Dataset:", input_json, 
    "Output format: Respond strictly with a valid JSON array of objects. Ensure:
    - Each object represents a row, with key-value pairs corresponding to column names and their generated values.
    - Match the data types of the input dataset (e.g., continuous for numerical columns, discrete for categorical columns).
    - Do not include any explanations, additional text, or incomplete JSON.
    - If the response exceeds the token limit, split it into multiple parts. Each part must be valid JSON and parsable."
  )
  
  system_prompt_role <- "You are a helpful assistant designed to generate synthetic data."
  
  #4.Implement methods such as adding random noise, applying transformations, or using resampling techniques to modify the original values.
  # Invia la richiesta all'API di OpenAI
  response <- POST(
    url = "https://api.openai.com/v1/chat/completions",
    add_headers(Authorization = paste("Bearer", api_key)),
    content_type_json(),
    body = list(
      model = "gpt-4",
      messages = list(list(role = "system", content = system_prompt_role), list(role = "user", content = prompt)),
      temperature = temperature,
      max_tokens = max_tokens
    ),
    encode = "json"
  )
  
  # Controlla se la richiesta è andata a buon fine
  if (response$status_code != 200) {
    stop(paste("Errore nella richiesta:", content(response, as = "text")))
  }
  
  # Estrai il testo generato dall'API
  response_content <- content(response, as = "parsed")
  generated_text <- response_content$choices[[1]]$message$content

  # Debug: Mostra l'output dell'API
  cat("Output dell'API:\n", generated_text, "\n")
  
  # Rimuovi eventuale testo extra attorno al JSON (ad es., spiegazioni o commenti)
  cleaned_json <- sub("^[^{\\[]*", "", generated_text)  # Rimuove qualsiasi cosa prima del JSON
  cleaned_json <- sub("[^}\\]]*$", "", cleaned_json)    # Rimuove qualsiasi cosa dopo il JSON
  
  # Prova a convertire il testo generato in un dataframe
  generated_df <- tryCatch(
    fromJSON(cleaned_json),
    error = function(e) {
      cat("Errore nella conversione del JSON: ", e$message, "\n")
      stop("Risultato non valido dall'API. Controlla il formato JSON generato.")
    }
  )
  
  return(generated_df)
}

#Carico il dataframe originario
data <- read.csv("dataset_filtraggio_finale2.csv", sep = ",")
data_no_dup <- data %>% distinct()
col_to_generate <- data_no_dup[1:50, 1:7]

# Inserisci la tua chiave API (sostituisci con la tua chiave valida)
api_key <- "xxxx"

# Genera un nuovo dataframe basandoti su quello iniziale
new_df <- generate_dataframe_with_chatgpt(col_to_generate, api_key)

#Salvo il csv generato da openai
write.csv(new_df, "gpt4sp.csv", row.names = FALSE)