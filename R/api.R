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
      # Qwen3.6 (latest)
      "Qwen/Qwen3.6-27B", "Qwen/Qwen3.6-35B-A3B",
      # Qwen3.5
      "Qwen/Qwen3.5-397B-A17B", "Qwen/Qwen3.5-122B-A10B",
      "Qwen/Qwen3.5-27B", "Qwen/Qwen3.5-35B-A3B",
      "Qwen/Qwen3.5-9B", "Qwen/Qwen3.5-4B",
      # Qwen3
      "Qwen/Qwen3-32B", "Qwen/Qwen3-14B", "Qwen/Qwen3-8B",
      "Qwen/Qwen3-30B-A3B-Instruct-2507",
      "Qwen/Qwen3-Coder-30B-A3B-Instruct",
      # Qwen2.5
      "Qwen/Qwen2.5-72B-Instruct", "Qwen/Qwen2.5-72B-Instruct-128K",
      "Qwen/Qwen2.5-32B-Instruct", "Qwen/Qwen2.5-14B-Instruct",
      "Qwen/Qwen2.5-7B-Instruct",
      # DeepSeek
      "deepseek-ai/DeepSeek-V4-Flash", "deepseek-ai/DeepSeek-V3.2",
      "deepseek-ai/DeepSeek-V3.1-Terminus", "deepseek-ai/DeepSeek-V3",
      "deepseek-ai/DeepSeek-R1", "deepseek-ai/DeepSeek-R1-0528-Qwen3-8B",
      # GLM
      "THUDM/GLM-4-32B-0414", "THUDM/GLM-4-9B-0414", "THUDM/GLM-Z1-9B-0414",
      "zai-org/GLM-4.5V", "zai-org/GLM-4.5-Air",
      "Pro/zai-org/GLM-5.1", "Pro/zai-org/GLM-5", "Pro/zai-org/GLM-4.7",
      # Kimi
      "Pro/moonshotai/Kimi-K2.6", "Pro/moonshotai/Kimi-K2.5",
      # MiniMax
      "MiniMaxAI/MiniMax-M2.5", "Pro/MiniMaxAI/MiniMax-M2.5",
      # Step
      "stepfun-ai/Step-3.5-Flash",
      # Tencent
      "tencent/Hunyuan-A13B-Instruct",
      # Others
      "ByteDance-Seed/Seed-OSS-36B-Instruct",
      "inclusionAI/Ling-flash-2.0", "inclusionAI/Ling-mini-2.0",
      # Pro versions
      "Pro/deepseek-ai/DeepSeek-V3.2", "Pro/deepseek-ai/DeepSeek-V3.1-Terminus",
      "Pro/deepseek-ai/DeepSeek-V3", "Pro/deepseek-ai/DeepSeek-R1",
      "Pro/Qwen/Qwen2.5-7B-Instruct"
    ),
    default_model = "Qwen/Qwen3-32B",
    api_key_env = "SILICONFLOW_API_KEY",
    chat_path = "/chat/completions",
    max_context = 1000000,
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
    # Anthropic: check for thinking blocks
    thinking <- NULL
    answer <- NULL
    for (block in resp$content) {
      if (block$type == "thinking") {
        thinking <- block$thinking
      } else if (block$type == "text") {
        answer <- block$text
      }
    }
    result <- answer %||% resp$content[[1]]$text
    if (!is.null(thinking)) attr(result, "thinking") <- thinking
    result
  } else {
    # OpenAI-compatible (deepseek, siliconflow, openai, etc.)
    msg <- resp$choices[[1]]$message
    # DeepSeek R1 / V3 reasoning_content
    thinking <- msg$reasoning_content %||% NULL
    # Some providers use thinking_content
    if (is.null(thinking)) thinking <- msg$thinking_content %||% NULL
    result <- msg$content
    if (!is.null(thinking) && nzchar(thinking)) {
      attr(result, "thinking") <- thinking
    }
    result
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
