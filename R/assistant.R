#' R Assistant - Core Functions
#'
#' Main assistant functions for chat, code generation, explanation,
#' refactoring, error fixing, and documentation.

#' Send a message to the AI assistant
#'
#' Core function that handles API communication with context and history.
#'
#' @param messages List of message objects (role + content).
#' @param use_context Logical. Include session context?
#' @param use_history Logical. Include conversation history?
#' @param save Logical. Save this exchange to history?
#'
#' @return The assistant's response text.
#' @noRd
.call_llm <- function(messages, use_context = TRUE, use_history = TRUE,
                      save = TRUE) {
  config <- assistant_get_config()

  # Resolve API key
  api_key <- config$api_key
  provider_cfg <- PROVIDERS[[config$provider]]
  if (!is.null(provider_cfg)) {
    env_key <- Sys.getenv(provider_cfg$api_key_env, unset = "")
    if (nzchar(env_key)) api_key <- env_key
  }

  if (!nzchar(api_key)) {
    stop("No API key configured. Run assistant_set_key('your-key') or ",
         "assistant_config() to set one.")
  }

  # Build system prompt with context
  system_parts <- config$system_prompt

  if (use_context && config$context_enabled) {
    tryCatch({
      ctx <- assistant_context()
      ctx_text <- format_context(ctx)
      system_parts <- paste0(system_parts, "\n\n", ctx_text)
    }, error = function(e) {
      warning("Could not collect context: ", conditionMessage(e))
    })
  }

  # Assemble full message list
  full_messages <- list(list(role = "system", content = system_parts))

  if (use_history) {
    hist <- assistant_history(n = 10)
    full_messages <- c(full_messages, hist)
  }

  full_messages <- c(full_messages, messages)

  # Compress if context exceeds limit
  max_ctx <- get_max_context(config$provider, config$model)
  # Reserve space for response (max_tokens)
  ctx_budget <- max_ctx - config$max_tokens - 1000  # 1000 token safety margin
  if (ctx_budget < 10000) ctx_budget <- 10000

  full_messages <- compress_messages(full_messages, max_tokens = ctx_budget)

  # Calculate context usage
  ctx_chars <- sum(nchar(vapply(full_messages, function(m) m$content, character(1))))
  ctx_tokens_est <- round(ctx_chars / 3.5)
  ctx_pct <- round(ctx_tokens_est / max_ctx * 100, 1)

  # Build request URL - no path manipulation
  provider_name <- config$provider

  if (nzchar(config$base_url %||% "")) {
    # User set custom base_url - use it EXACTLY as-is
    url <- config$base_url
  } else {
    # Use provider default: base_url + chat_path
    url <- paste0(provider_cfg$base_url, provider_cfg$chat_path)
  }

  body <- build_request_body(
    provider_name = provider_name,
    model = config$model,
    messages = full_messages,
    temperature = config$temperature,
    max_tokens = config$max_tokens,
    stream = FALSE
  )

  headers <- provider_cfg$header_fn(api_key)
  headers <- c(headers, "Content-Type" = "application/json")

  # Make the request
  tryCatch({
    resp <- httr2::request(url) |>
      httr2::req_headers(!!!headers) |>
      httr2::req_body_json(body) |>
      httr2::req_retry(max_tries = 3) |>
      httr2::req_timeout(180) |>
      httr2::req_perform()

    if (httr2::resp_is_error(resp)) {
      err_body <- tryCatch(httr2::resp_body_json(resp),
                           error = function(e) httr2::resp_body_string(resp))
      err_msg <- sprintf("API error (HTTP %s): %s",
                         httr2::resp_status(resp),
                         jsonlite::toJSON(err_body, auto_unbox = TRUE))
      # Add helpful hint for common errors
      status <- httr2::resp_status(resp)
      if (status == 400) {
        err_msg <- paste0(err_msg,
          "\nHint: Check model name and API URL. ",
          "Current: model='", config$model, "', url='", url, "'")
      } else if (status == 401) {
        err_msg <- paste0(err_msg, "\nHint: API key is invalid or expired.")
      } else if (status == 429) {
        err_msg <- paste0(err_msg, "\nHint: Rate limited. Wait and retry.")
      }
      stop(err_msg)
    }

    resp_json <- httr2::resp_body_json(resp)
    result <- parse_response(provider_name, resp_json)

    # Parse token usage
    usage <- parse_usage(provider_name, resp_json)

    # Build metadata
    meta <- list(
      context_tokens = ctx_tokens_est,
      context_max = max_ctx,
      context_pct = ctx_pct,
      messages_count = length(full_messages),
      compressed = (ctx_chars / 3.5) > ctx_budget
    )
    if (!is.null(usage)) {
      meta$prompt_tokens <- usage$prompt_tokens
      meta$completion_tokens <- usage$completion_tokens
      meta$total_tokens <- usage$total_tokens
    }

    # Save to history
    if (save) {
      user_text <- paste(vapply(messages, function(m) m$content, character(1)),
                         collapse = "\n")
      add_to_history("user", user_text)
      add_to_history("assistant", result)
    }

    # Attach metadata as attribute
    attr(result, "meta") <- meta
    result
  }, error = function(e) {
    stop("LLM API call failed: ", conditionMessage(e))
  })
}


#' Interactive chat with the AI assistant
#'
#' Send a message and get a response. Maintains conversation history.
#'
#' @param message Character. Your message to the assistant.
#' @param use_context Logical. Include R session context? Default TRUE.
#' @param stream Logical. (Reserved for future streaming support.)
#'
#' @return The assistant's response (invisibly). Also prints to console.
#' @export
#'
#' @examples
#' \dontrun{
#' assistant_chat("How do I create a ggplot2 bar chart?")
#' assistant_chat("Now add error bars to it")
#' }
assistant_chat <- function(message, use_context = TRUE, stream = FALSE) {
  if (!nzchar(trimws(message))) {
    stop("Message cannot be empty.")
  }

  msgs <- list(list(role = "user", content = message))
  response <- .call_llm(msgs, use_context = use_context)

  cat("\n")
  cat(strwrap("--- R Assistant ---", width = 60), "\n")
  cat(response, "\n")
  cat(strwrap("-------------------", width = 60), "\n\n")

  invisible(response)
}


#' Ask a one-shot question (no history)
#'
#' @param question Character. The question to ask.
#' @param use_context Logical. Include R session context?
#'
#' @return The assistant's response text.
#' @export
#'
#' @examples
#' \dontrun{
#' assistant_ask("What is the difference between lapply and sapply?")
#' }
assistant_ask <- function(question, use_context = FALSE) {
  msgs <- list(list(role = "user", content = question))
  response <- .call_llm(msgs, use_context = use_context,
                         use_history = FALSE, save = FALSE)
  cat(response, "\n")
  invisible(response)
}


#' Explain code
#'
#' Get a detailed explanation of selected or provided code.
#'
#' @param code Character. Code to explain. If NULL, uses RStudio selection.
#' @param detail_level Character. "brief", "normal", or "detailed".
#'
#' @return The explanation text.
#' @export
#'
#' @examples
#' \dontrun{
#' assistant_explain("mtcars %>% group_by(cyl) %>% summarise_all(mean)")
#' }
assistant_explain <- function(code = NULL, detail_level = "normal") {
  if (is.null(code)) {
    code <- .get_selection_or_stop()
  }

  prompt <- switch(detail_level,
    brief = paste("Briefly explain this R code in 2-3 sentences:\n\n```r\n",
                   code, "\n```"),
    detailed = paste("Explain this R code in detail, covering:\n",
                     "1. What it does step by step\n",
                     "2. Key functions/packages used\n",
                     "3. Input/output expectations\n",
                     "4. Potential issues or edge cases\n\n",
                     "```r\n", code, "\n```"),
    # default "normal"
    paste("Explain what this R code does:\n\n```r\n", code, "\n```")
  )

  msgs <- list(list(role = "user", content = prompt))
  response <- .call_llm(msgs, use_context = TRUE, use_history = FALSE)
  cat(response, "\n")
  invisible(response)
}


#' Refactor code
#'
#' Rewrite code for better readability, performance, or style.
#'
#' @param code Character. Code to refactor. If NULL, uses RStudio selection.
#' @param style Character. Target style: "tidyverse", "base", "data.table".
#' @param goal Character. Optimization goal: "readability", "performance",
#'   "conciseness".
#'
#' @return The refactored code with explanation.
#' @export
#'
#' @examples
#' \dontrun{
#' assistant_refactor("for(i in 1:nrow(df)) { df$x[i] <- df$y[i] * 2 }")
#' }
assistant_refactor <- function(code = NULL, style = "tidyverse",
                                goal = "readability") {
  if (is.null(code)) {
    code <- .get_selection_or_stop()
  }

  prompt <- paste0(
    "Refactor this R code for better ", goal, ".\n",
    "Target style: ", style, "\n\n",
    "Return the refactored code in a ```r block, followed by a brief ",
    "explanation of what changed and why.\n\n",
    "Original code:\n```r\n", code, "\n```"
  )

  msgs <- list(list(role = "user", content = prompt))
  response <- .call_llm(msgs, use_context = TRUE, use_history = FALSE)
  cat(response, "\n")
  invisible(response)
}


#' Fix code errors
#'
#' Analyze an error message and provide a fix.
#'
#' @param code Character. The code that produced the error. If NULL, uses
#'   RStudio selection.
#' @param error Character. The error message. If NULL, attempts to read from
#'   the R console history.
#'
#' @return The fix with explanation.
#' @export
#'
#' @examples
#' \dontrun{
#' assistant_fix(
#'   code = "library(tidyverse)\ndf %>% filter(x = 5)",
#'   error = "Error in filter(): could not find function '%>%'"
#' )
#' }
assistant_fix <- function(code = NULL, error = NULL) {
  if (is.null(code)) {
    code <- .get_selection_or_stop()
  }

  # Try to get last error from console
  if (is.null(error)) {
    tryCatch({
      error <- geterrmessage()
      if (!nzchar(error)) error <- NULL
    }, error = function(e) {
      error <<- NULL
    })
  }

  prompt <- "Fix the following R code"
  if (!is.null(error)) {
    prompt <- paste0(prompt, " that produces this error:\n\nError: ",
                     error, "\n\n")
  } else {
    prompt <- paste0(prompt, ":\n\n")
  }
  prompt <- paste0(prompt, "```r\n", code, "\n```\n\n",
                   "Provide the corrected code and explain what was wrong.")

  msgs <- list(list(role = "user", content = prompt))
  response <- .call_llm(msgs, use_context = TRUE, use_history = FALSE)
  cat(response, "\n")
  invisible(response)
}


#' Generate documentation
#'
#' Generate roxygen2 documentation for a function.
#'
#' @param code Character. Function code to document. If NULL, uses RStudio
#'   selection.
#' @param style Character. "roxygen2" (default) or "markdown".
#'
#' @return The generated documentation.
#' @export
#'
#' @examples
#' \dontrun{
#' assistant_document("my_func <- function(x, y = 10) { x + y }")
#' }
assistant_document <- function(code = NULL, style = "roxygen2") {
  if (is.null(code)) {
    code <- .get_selection_or_stop()
  }

  prompt <- paste0(
    "Generate ", style, " documentation for this R function.\n",
    "Include: title, description, @param for each parameter, ",
    "@return, @examples, and @export if appropriate.\n\n",
    "```r\n", code, "\n```"
  )

  msgs <- list(list(role = "user", content = prompt))
  response <- .call_llm(msgs, use_context = FALSE, use_history = FALSE)
  cat(response, "\n")
  invisible(response)
}


#' Generate code from description
#'
#' Describe what you want in natural language and get R code.
#'
#' @param description Character. What you want the code to do.
#' @param style Character. Code style: "tidyverse", "base", "data.table".
#'
#' @return The generated code with explanation.
#' @export
#'
#' @examples
#' \dontrun{
#' assistant_complete("Create a histogram of mtcars mpg with density overlay")
#' }
assistant_complete <- function(description, style = "tidyverse") {
  prompt <- paste0(
    "Write R code that does the following (use ", style, " style):\n\n",
    description, "\n\n",
    "Return working code in a ```r block with brief explanation."
  )

  msgs <- list(list(role = "user", content = prompt))
  response <- .call_llm(msgs, use_context = TRUE, use_history = TRUE)
  cat(response, "\n")
  invisible(response)
}


#' Generate unit tests
#'
#' Generate testthat tests for a function.
#'
#' @param code Character. Function code to test. If NULL, uses RStudio
#'   selection.
#' @param test_framework Character. "testthat" (default) or "tinytest".
#'
#' @return The generated test code.
#' @export
#'
#' @examples
#' \dontrun{
#' assistant_test("add <- function(a, b) a + b")
#' }
assistant_test <- function(code = NULL, test_framework = "testthat") {
  if (is.null(code)) {
    code <- .get_selection_or_stop()
  }

  prompt <- paste0(
    "Generate ", test_framework, " unit tests for this R function.\n",
    "Include tests for: normal usage, edge cases, error conditions, ",
    "and type checking.\n\n",
    "```r\n", code, "\n```"
  )

  msgs <- list(list(role = "user", content = prompt))
  response <- .call_llm(msgs, use_context = FALSE, use_history = FALSE)
  cat(response, "\n")
  invisible(response)
}


#' Extract code blocks from AI response
#'
#' @param text Character. AI response text.
#'
#' @return Character vector of code blocks.
#' @noRd
extract_code_blocks <- function(text) {
  pattern <- "```(?:r|R)?\\s*\n(.*?)```"
  m <- gregexpr(pattern, text, perl = TRUE)
  matches <- regmatches(text, m)[[1]]
  gsub("```(?:r|R)?\\s*\n|```$", "", matches)
}


#' Get selected text or stop with informative error
#' @noRd
.get_selection_or_stop <- function() {
  if (!rstudioapi::isAvailable()) {
    stop("No code provided and RStudio is not available.\n",
         "Pass code explicitly, e.g.: assistant_explain('your code here')")
  }

  ctx <- rstudioapi::getActiveDocumentContext()
  sel <- ctx$selection[[1]]$text

  if (!nzchar(trimws(sel))) {
    stop("No code selected in RStudio. Select some code first, ",
         "or pass code explicitly.")
  }

  sel
}
