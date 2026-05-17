#' Configuration Management for R Assistant
#'
#' Manage API keys, model selection, and provider settings.
#' Configurations are stored in a JSON file at ~/.r-assistant/config.json.

CONFIG_DIR <- file.path(path.expand("~"), ".r-assistant")
CONFIG_FILE <- file.path(CONFIG_DIR, "config.json")
HISTORY_FILE <- file.path(CONFIG_DIR, "history.json")

#' Get or create the configuration file path
#' @noRd
ensure_config_dir <- function() {
  if (!dir.exists(CONFIG_DIR)) {
    dir.create(CONFIG_DIR, recursive = TRUE, showWarnings = FALSE)
  }
}


#' Load configuration from disk
#' @noRd
load_config <- function() {
  ensure_config_dir()
  if (file.exists(CONFIG_FILE)) {
    tryCatch(
      jsonlite::fromJSON(CONFIG_FILE, simplifyVector = FALSE),
      error = function(e) list()
    )
  } else {
    list()
  }
}


#' Save configuration to disk
#' @noRd
save_config <- function(config) {
  ensure_config_dir()
  jsonlite::write_json(config, CONFIG_FILE, pretty = TRUE, auto_unbox = TRUE)
}


#' Get current configuration (with defaults)
#'
#' Returns the full configuration including provider, model, API key status,
#' temperature, max_tokens, and system prompt.
#'
#' @return A list with the current configuration.
#' @export
assistant_get_config <- function() {
  config <- load_config()
  defaults <- list(
    provider = "deepseek",
    model = "deepseek-chat",
    api_key = "",
    base_url = "",
    temperature = 0.3,
    max_tokens = 4096,
    system_prompt = default_system_prompt(),
    context_enabled = TRUE,
    history_max = 50
  )
  # Merge: saved values override defaults
  for (nm in names(defaults)) {
    if (is.null(config[[nm]]) || config[[nm]] == "") {
      config[[nm]] <- defaults[[nm]]
    }
  }
  config
}


#' Default system prompt for R programming assistance
#' @noRd
default_system_prompt <- function() {
  paste(
    "You are an expert R programming assistant embedded in the user's",
    "R/RStudio environment. You help with:\n",
    "- Writing, debugging, and optimizing R code\n",
    "- Explaining R concepts and error messages\n",
    "- Suggesting packages and best practices\n",
    "- Generating documentation and tests\n",
    "- Data analysis and visualization advice\n\n",
    "Guidelines:\n",
    "- Always return code in ```r ... ``` fenced blocks\n",
    "- Be concise but thorough\n",
    "- Explain your reasoning briefly before code\n",
    "- When fixing errors, explain what went wrong\n",
    "- Prefer tidyverse conventions but respect user's style\n",
    "- If the user provides session context (loaded packages,",
    "data structure), use that information\n"
  )
}


#' Configure R Assistant interactively
#'
#' Opens a Shiny gadget to configure provider, model, and API key.
#'
#' @param provider Character. Provider name (see [assistant_available_providers()]).
#' @param model Character. Model identifier.
#' @param api_key Character. API key (will be stored locally).
#' @param base_url Character. Custom base URL (for 'custom' provider).
#' @param temperature Numeric. Sampling temperature (0-2).
#' @param max_tokens Integer. Maximum response tokens.
#' @param system_prompt Character. Custom system prompt.
#' @param context_enabled Logical. Whether to include R session context.
#'
#' @details
#' When called with no arguments, opens an interactive configuration
#' gadget in RStudio. Any named argument will override that setting
#' directly without the gadget.
#'
#' @return Invisibly returns the updated configuration.
#' @export
assistant_config <- function(provider = NULL, model = NULL, api_key = NULL,
                             base_url = NULL, temperature = NULL,
                             max_tokens = NULL, system_prompt = NULL,
                             context_enabled = NULL) {
  config <- assistant_get_config()

  # Direct update if arguments provided
  if (!is.null(provider)) config$provider <- provider
  if (!is.null(model)) config$model <- model
  if (!is.null(api_key)) config$api_key <- api_key
  if (!is.null(base_url)) config$base_url <- base_url
  if (!is.null(temperature)) config$temperature <- temperature
  if (!is.null(max_tokens)) config$max_tokens <- max_tokens
  if (!is.null(system_prompt)) config$system_prompt <- system_prompt
  if (!is.null(context_enabled)) config$context_enabled <- context_enabled

  save_config(config)

  # If no args provided and RStudio available, open gadget
  has_args <- !all(sapply(list(provider, model, api_key, base_url,
                                temperature, max_tokens, system_prompt,
                                context_enabled), is.null))
  if (!has_args && rstudioapi::isAvailable()) {
    return(.assistant_config_gadget(config))
  }

  message("R Assistant configuration updated.")
  message(sprintf("  Provider : %s", config$provider))
  message(sprintf("  Model    : %s", config$model))
  message(sprintf("  API Key  : %s", ifelse(nzchar(config$api_key),
                                              "****(set)", "(not set)")))
  invisible(config)
}


#' Set API key for a provider
#'
#' @param key Character. The API key string.
#' @param provider Character. Provider name (default: current provider).
#' @export
assistant_set_key <- function(key, provider = NULL) {
  if (is.null(provider)) {
    config <- assistant_get_config()
    provider <- config$provider
  }
  key_name <- paste0("R_ASSISTANT_", toupper(provider), "_KEY")
  do.call(Sys.setenv, setNames(list(key), key_name))
  assistant_config(api_key = key)
  message(sprintf("API key for '%s' saved successfully.", provider))
  invisible(NULL)
}


#' Set the active model
#'
#' @param model Character. Model identifier.
#' @export
assistant_set_model <- function(model) {
  assistant_config(model = model)
  invisible(NULL)
}


#' Set the active provider
#'
#' @param provider Character. Provider name.
#' @export
assistant_set_provider <- function(provider) {
  if (!provider %in% assistant_available_providers()) {
    stop(sprintf("Unknown provider '%s'. Available: %s",
                 provider, paste(assistant_available_providers(), collapse = ", ")))
  }
  prov <- PROVIDERS[[provider]]
  config <- assistant_get_config()
  config$provider <- provider
  config$base_url <- ""  # Clear custom URL, use provider default
  if (nzchar(prov$default_model)) {
    config$model <- prov$default_model
  }
  save_config(config)
  message(sprintf("Provider set to '%s', model: %s", provider, config$model))
  invisible(NULL)
}


#' Shiny gadget for interactive configuration
#' @noRd
.assistant_config_gadget <- function(config) {
  if (!requireNamespace("miniUI", quietly = TRUE) ||
      !requireNamespace("shiny", quietly = TRUE)) {
    message("Shiny/miniUI not available. Use assistant_config() with arguments.")
    return(invisible(config))
  }

  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("R Assistant Configuration"),
    miniUI::miniContentPanel(
      shiny::selectInput("provider", "Provider:",
                         choices = assistant_available_providers(),
                         selected = config$provider),
      shiny::textInput("model", "Model:", value = config$model),
      shiny::passwordInput("api_key", "API Key:", value = config$api_key),
      shiny::textInput("base_url", "Base URL (custom provider):",
                       value = config$base_url),
      shiny::sliderInput("temperature", "Temperature:",
                         min = 0, max = 2, value = config$temperature, step = 0.1),
      shiny::numericInput("max_tokens", "Max Tokens:",
                          value = config$max_tokens, min = 256, max = 128000),
      shiny::checkboxInput("context_enabled", "Include R session context",
                           value = config$context_enabled),
      shiny::hr(),
      shiny::helpText("API key is stored locally at: ~/.r-assistant/config.json")
    )
  )

  server <- function(input, output, session) {
    shiny::observeEvent(input$done, {
      new_config <- list(
        provider = input$provider,
        model = input$model,
        api_key = input$api_key,
        base_url = input$base_url,
        temperature = input$temperature,
        max_tokens = input$max_tokens,
        context_enabled = input$context_enabled,
        system_prompt = config$system_prompt,
        history_max = config$history_max
      )
      save_config(new_config)
      shiny::stopApp(new_config)
    })
    shiny::observeEvent(input$cancel, {
      shiny::stopApp(NULL)
    })
  }

  viewer <- shiny::dialogViewer("R Assistant", width = 500, height = 600)
  shiny::runGadget(ui, server, viewer = viewer)
}
