#' API Configuration for LLM Providers
#'
#' Supported providers and their default configurations.
#' @importFrom httr2 request req_headers req_body_json req_perform
#'   req_retry req_timeout resp_body_json resp_is_error
#'   resp_status resp_body_string
#' @importFrom jsonlite toJSON fromJSON
#' @noRd

# Provider registry -----------------------------------------------------------
PROVIDERS <- list(
  openai = list(
    name = "OpenAI",
    base_url = "https://api.openai.com/v1",
    models = c("gpt-4o", "gpt-4o-mini", "gpt-4-turbo", "gpt-3.5-turbo"),
    default_model = "gpt-4o-mini",
    api_key_env = "OPENAI_API_KEY",
    chat_path = "/chat/completions",
    header_fn = function(key) {
      c("Authorization" = paste("Bearer", key))
    }
  ),
  anthropic = list(
    name = "Anthropic",
    base_url = "https://api.anthropic.com/v1",
    models = c("claude-sonnet-4-20250514", "claude-3-5-haiku-20241022",
               "claude-3-opus-20240229"),
    default_model = "claude-sonnet-4-20250514",
    api_key_env = "ANTHROPIC_API_KEY",
    chat_path = "/messages",
    header_fn = function(key) {
      c("x-api-key" = key, "anthropic-version" = "2023-06-01")
    }
  ),
  deepseek = list(
    name = "DeepSeek",
    base_url = "https://api.deepseek.com/v1",
    models = c("deepseek-chat", "deepseek-coder"),
    default_model = "deepseek-chat",
    api_key_env = "DEEPSEEK_API_KEY",
    chat_path = "/chat/completions",
    header_fn = function(key) {
      c("Authorization" = paste("Bearer", key))
    }
  ),
  openrouter = list(
    name = "OpenRouter",
    base_url = "https://openrouter.ai/api/v1",
    models = c("anthropic/claude-sonnet-4",
               "openai/gpt-4o", "deepseek/deepseek-chat"),
    default_model = "deepseek/deepseek-chat",
    api_key_env = "OPENROUTER_API_KEY",
    chat_path = "/chat/completions",
    header_fn = function(key) {
      c("Authorization" = paste("Bearer", key))
    }
  ),
  custom = list(
    name = "Custom (OpenAI-compatible)",
    base_url = "",
    models = character(0),
    default_model = "",
    api_key_env = "R_ASSISTANT_API_KEY",
    chat_path = "/chat/completions",
    header_fn = function(key) {
      c("Authorization" = paste("Bearer", key))
    }
  )
)


#' Get available providers
#'
#' @return A character vector of supported provider names.
#' @export
assistant_available_providers <- function() {
  names(PROVIDERS)
}


#' Build request body depending on provider format
#' @noRd
build_request_body <- function(provider_name, model, messages,
                               temperature, max_tokens, stream) {
  if (provider_name == "anthropic") {
    # Anthropic uses a different format
    system_msg <- NULL
    user_msgs <- list()
    for (msg in messages) {
      if (msg$role == "system") {
        system_msg <- msg$content
      } else {
        user_msgs <- c(user_msgs, list(msg))
      }
    }
    body <- list(
      model = model,
      max_tokens = max_tokens,
      temperature = temperature,
      messages = user_msgs
    )
    if (!is.null(system_msg)) {
      body$system <- system_msg
    }
  } else {
    # OpenAI-compatible format
    body <- list(
      model = model,
      messages = messages,
      temperature = temperature,
      max_tokens = max_tokens,
      stream = FALSE
    )
  }
  body
}


#' Parse response depending on provider format
#' @noRd
parse_response <- function(provider_name, resp) {
  if (provider_name == "anthropic") {
    resp$content[[1]]$text
  } else {
    # OpenAI-compatible
    resp$choices[[1]]$message$content
  }
}
