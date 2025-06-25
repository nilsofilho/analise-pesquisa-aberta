#função gemini

#verificar nome dos modelos gemini
# available_gemini_models <- list_gemini_models()

gemini <- function(prompt, 
                   temperature=1,
                   max_output_tokens=1024,
                   api_key=Sys.getenv("GEMINI_API_KEY"),
                   model = "gemini-2.0-flash-lite-preview-02-05") {
  
  if(nchar(api_key)<1) {
    api_key <- readline("Paste your API key here: ")
    Sys.setenv(GEMINI_API_KEY = api_key)
  }
  
  model_query <- paste0(model, ":generateContent")
  
  response <- POST(
    url = paste0("https://generativelanguage.googleapis.com/v1beta/models/", model_query),
    query = list(key = api_key),
    content_type_json(),
    encode = "json",
    body = list(
      contents = list(
        parts = list(
          list(text = prompt)
        )),
      generationConfig = list(
        temperature = temperature,
        maxOutputTokens = max_output_tokens
      )
    )
  )
  
  if(response$status_code>200) {
    stop(paste("Error - ", content(response)$error$message))
  }
  
  candidates <- content(response)$candidates
  outputs <- unlist(lapply(candidates, function(candidate) candidate$content$parts))
  
  return(outputs)
  
}




list_gemini_models <- function(api_key = Sys.getenv("GEMINI_API_KEY")) {
  if (nchar(api_key) < 1) {
    api_key <- readline("Paste your API key here: ")
    Sys.setenv(GEMINI_API_KEY = api_key)
  }
  
  response <- GET(
    url = "https://generativelanguage.googleapis.com/v1beta/models",
    query = list(key = api_key)
  )
  
  if (response$status_code == 200) {
    models_data <- content(response, "parsed")
    # Imprimir informações relevantes dos modelos
    for (model in models_data$models) {
      if ("generateContent" %in% model$supportedGenerationMethods) {
        message(paste("Model Name:", model$name))
        message(paste("  Display Name:", model$displayName))
        message(paste("  Description:", model$description))
        message(paste("  Supported Methods:", paste(model$supportedGenerationMethods, collapse = ", ")))
        message("--------------------------------------------------")
      }
    }
    return(models_data) # Retorna a lista completa se precisar
  } else {
    stop(paste("Error fetching models - ", content(response)$error$message))
  }
}



