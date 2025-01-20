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
  
  # Invia la richiesta all'API di OpenAI
  response <- POST(
    url = "https://api.openai.com/v1/chat/completions",
    add_headers(Authorization = paste("Bearer", api_key)),
    content_type_json(),
    body = list(
      model = "gpt-4",
      messages = list(list(role = "user", content = prompt)),
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
  print(generated_text)
  
  # Prova a convertire il testo generato in un dataframe
  generated_df <- tryCatch(
    fromJSON(generated_text),
    error = function(e) stop("Errore nella conversione del risultato in dataframe:", e)
  )
  
  return(generated_df)
}

#Carico il dataframe originario
data <- read.csv("dataset_filtraggio_finale2.csv", sep = ",")
data_no_dup <- data %>% distinct()
col_to_generate <- data_no_dup$HasFavicon
col_to_generate <- as.data.frame(col_to_generate)
col_to_generate <- col_to_generate[1:50,]

# Inserisci la tua chiave API (sostituisci con la tua chiave valida)
api_key <- "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"

# Genera un nuovo dataframe basandoti su quello iniziale
new_df <- generate_dataframe_with_chatgpt(col_to_generate, api_key)

#Salvo il csv generato da openai
write.csv(new_df, "gpt4sp.csv", row.names = FALSE)