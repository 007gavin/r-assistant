#' Quick setup for first-time users
#'
#' Interactive setup wizard for configuring R Assistant.
#' Run this once after installing the package.
#'
#' @param provider Character. Provider name. If NULL, shows selection menu.
#' @param api_key Character. API key. If NULL, prompts for input.
#'
#' @export
assistant_setup <- function(provider = NULL, api_key = NULL) {
  cat("\n=== R Assistant Setup ===\n\n")

  # Select provider
  if (is.null(provider)) {
    providers <- assistant_available_providers()
    cat("Available providers:\n")
    for (i in seq_along(providers)) {
      prov <- PROVIDERS[[providers[i]]]
      cat(sprintf("  %d. %s (%s)\n", i, prov$name, providers[i]))
    }
    cat("\n")
    choice <- utils::menu(choices = providers, title = "Select provider (number):")
    if (choice == 0) stop("Setup cancelled.")
    provider <- providers[choice]
  }

  cat("Selected provider:", provider, "\n")

  # Get API key
  if (is.null(api_key)) {
    cat("\nEnter your API key: ")
    api_key <- readLines(con = stdin(), n = 1)
    if (!nzchar(trimws(api_key))) stop("No API key provided.")
  }

  # Save config
  assistant_config(provider = provider, api_key = api_key)

  # Test connection
  cat("\nTesting connection...\n")
  tryCatch({
    result <- assistant_ask("Say hi in 3 words", use_context = FALSE)
    cat("\nSetup successful! Response:", result, "\n")
    cat("\nYou can now use:\n")
    cat("  library(r.assistant)\n")
    cat("  assistant_chat('your question')\n")
    cat("  addin_chat()  # for Viewer panel\n")
  }, error = function(e) {
    cat("\nSetup saved but test failed:", conditionMessage(e), "\n")
    cat("Please check your API key and try again.\n")
  })

  invisible(NULL)
}


#' Check if R Assistant is properly configured
#'
#' Validates the current configuration and tests API connectivity.
#'
#' @export
assistant_check <- function() {
  cat("\n=== R Assistant Configuration Check ===\n\n")

  config <- assistant_get_config()

  cat("Provider:", config$provider, "\n")
  cat("Model:", config$model, "\n")
  cat("Base URL:", ifelse(nzchar(config$base_url), config$base_url, "(default)"), "\n")
  cat("API Key:", ifelse(nzchar(config$api_key), "****(set)", "(NOT SET)"), "\n")
  cat("Context:", ifelse(config$context_enabled, "ON", "OFF"), "\n")
  cat("Temperature:", config$temperature, "\n")
  cat("Max tokens:", config$max_tokens, "\n")

  # Check provider
  prov <- PROVIDERS[[config$provider]]
  if (is.null(prov)) {
    cat("\n[ERROR] Unknown provider:", config$provider, "\n")
    cat("Run assistant_setup() to reconfigure.\n")
    return(invisible(FALSE))
  }

  # Check API key
  api_key <- config$api_key
  env_key <- Sys.getenv(prov$api_key_env, unset = "")
  if (nzchar(env_key)) api_key <- env_key

  if (!nzchar(api_key)) {
    cat("\n[ERROR] No API key found!\n")
    cat("Set via: assistant_set_key('your-key')\n")
    cat("Or run: assistant_setup()\n")
    return(invisible(FALSE))
  }

  # Check URL
  full_url <- if (nzchar(config$base_url)) config$base_url else paste0(prov$base_url, prov$chat_path)
  

  cat("\nTesting API at:", full_url, "\n")

  tryCatch({
    resp <- httr2::request(full_url) |>
      httr2::req_headers("Authorization" = paste("Bearer", api_key),
                         "Content-Type" = "application/json") |>
      httr2::req_body_json(list(
        model = config$model,
        messages = list(list(role = "user", content = "Say OK")),
        max_tokens = 5
      )) |>
      httr2::req_timeout(15) |>
      httr2::req_perform()

    if (httr2::resp_is_error(resp)) {
      cat("[ERROR] HTTP", httr2::resp_status(resp), "\n")
      cat(httr2::resp_body_string(resp), "\n")
      return(invisible(FALSE))
    }

    cat("[OK] API connection successful!\n")
    invisible(TRUE)
  }, error = function(e) {
    cat("[ERROR]", conditionMessage(e), "\n")
    invisible(FALSE)
  })
}
