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
  deepseek = list(
    name = "DeepSeek",
    base_url = "https://api.deepseek.com/v1",
    models = c("deepseek-chat", "deepseek-coder"),
    default_model = "deepseek-chat",
    api_key_env = "DEEPSEEK_API_KEY",
    chat_path = "/chat/completions",
    max_context = 64000,
    header_fn = function(key) c("Authorization" = paste("Bearer", key))
  ),
  siliconflow = list(
    name = "SiliconFlow",
    base_url = "https://api.siliconflow.cn/v1",
    models = c(
      "Qwen/Qwen2.5-72B-Instruct",
      "Qwen/Qwen2.5-32B-Instruct",
      "Qwen/Qwen2.5-7B-Instruct",
      "deepseek-ai/DeepSeek-V3",
      "deepseek-ai/DeepSeek-V2.5",
      "THUDM/glm-4-9b-chat",
      "internlm/internlm2_5-7b-chat",
      "meta-llama/Meta-Llama-3.1-8B-Instruct",
      "meta-llama/Meta-Llama-3.1-70B-Instruct"
    ),
    default_model = "Qwen/Qwen2.5-72B-Instruct",
    api_key_env = "SILICONFLOW_API_KEY",
    chat_path = "/chat/completions",
    max_context = 131072,
    header_fn = function(key) c("Authorization" = paste("Bearer", key))
  ),
  openai = list(
    name = "OpenAI",
    base_url = "https://api.openai.com/v1",
    models = c("gpt-4o", "gpt-4o-mini", "gpt-4-turbo", "gpt-3.5-turbo"),
    default_model = "gpt-4o-mini",
    api_key_env = "OPENAI_API_KEY",
    chat_path = "/chat/completions",
    max_context = 128000,
    header_fn = function(key) c("Authorization" = paste("Bearer", key))
  ),
  anthropic = list(
    name = "Anthropic",
    base_url = "https://api.anthropic.com/v1",
    models = c("claude-sonnet-4-20250514", "claude-3-5-haiku-20241022",
               "claude-3-opus-20240229"),
    default_model = "claude-sonnet-4-20250514",
    api_key_env = "ANTHROPIC_API_KEY",
    chat_path = "/messages",
    max_context = 200000,
    header_fn = function(key) {
      c("x-api-key" = key, "anthropic-version" = "2023-06-01")
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
    max_context = 128000,
    header_fn = function(key) c("Authorization" = paste("Bearer", key))
  ),
  custom = list(
    name = "Custom (OpenAI-compatible)",
    base_url = "",
    models = character(0),
    default_model = "",
    api_key_env = "R_ASSISTANT_API_KEY",
    chat_path = "/chat/completions",
    max_context = 128000,
    header_fn = function(key) c("Authorization" = paste("Bearer", key))
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
    # OpenAI-compatible format (works for deepseek, siliconflow, openrouter, custom)
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


#' Get token usage from response
#' @noRd
parse_usage <- function(provider_name, resp) {
  tryCatch({
    if (provider_name == "anthropic") {
      list(
        prompt_tokens = resp$usage$input_tokens %||% 0,
        completion_tokens = resp$usage$output_tokens %||% 0,
        total_tokens = (resp$usage$input_tokens %||% 0) +
                       (resp$usage$output_tokens %||% 0)
      )
    } else {
      usage <- resp$usage
      if (is.null(usage)) return(NULL)
      list(
        prompt_tokens = usage$prompt_tokens %||% 0,
        completion_tokens = usage$completion_tokens %||% 0,
        total_tokens = usage$total_tokens %||% 0
      )
    }
  }, error = function(e) NULL)
}


#' Get max context window for a provider
#' @noRd
get_max_context <- function(provider_name, model = NULL) {
  prov <- PROVIDERS[[provider_name]]
  if (is.null(prov)) return(128000)
  prov$max_context
}


#' Estimate token count from text (rough: ~4 chars per token for mixed CN/EN)
#' @noRd
estimate_tokens <- function(text) {
  # Rough estimate: 1 token ~ 3.5 chars for mixed Chinese/English
  nchar(text) / 3.5
}


#' Compress conversation history to fit within context window
#'
#' Keeps the system prompt and recent messages, summarizes older ones.
#' @noRd
compress_messages <- function(messages, max_tokens = 100000) {
  # Estimate total tokens
  total_chars <- sum(nchar(vapply(messages, function(m) m$content, character(1))))
  total_tokens <- total_chars / 3.5

  if (total_tokens <= max_tokens) {
    return(messages)
  }

  # Keep system message (first) and last 6 messages
  system_msgs <- Filter(function(m) m$role == "system", messages)
  non_system <- Filter(function(m) m$role != "system", messages)

  if (length(non_system) <= 6) {
    return(messages)
  }

  # Summarize older messages
  old_msgs <- non_system[1:(length(non_system) - 6)]
  recent_msgs <- non_system[(length(non_system) - 5):length(non_system)]

  old_text <- paste(vapply(old_msgs, function(m) {
    paste0(toupper(m$role), ": ", m$content)
  }, character(1)), collapse = "\n")

  summary_text <- paste0(
    "[Previous conversation summary (", length(old_msgs), " messages compressed)]\n",
    "Topics discussed: ",
    substr(old_text, 1, min(500, nchar(old_text))),
    if (nchar(old_text) > 500) "..." else ""
  )

  c(system_msgs, list(list(role = "system", content = summary_text)), recent_msgs)
}
