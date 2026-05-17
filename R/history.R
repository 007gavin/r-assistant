#' Conversation History Management
#'
#' Store and retrieve conversation history for multi-turn dialogues.

HISTORY_MAX_DEFAULT <- 50


#' Load conversation history
#' @noRd
load_history <- function() {
  ensure_config_dir()
  if (file.exists(HISTORY_FILE)) {
    tryCatch(
      jsonlite::fromJSON(HISTORY_FILE, simplifyVector = FALSE),
      error = function(e) list()
    )
  } else {
    list()
  }
}


#' Save conversation history
#' @noRd
save_history <- function(history) {
  ensure_config_dir()
  jsonlite::write_json(history, HISTORY_FILE, pretty = TRUE, auto_unbox = TRUE)
}


#' Add a message to history
#' @noRd
add_to_history <- function(role, content) {
  history <- load_history()
  config <- assistant_get_config()

  entry <- list(
    role = role,
    content = content,
    timestamp = as.character(Sys.time())
  )

  history$messages <- c(history$messages, list(entry))

  # Trim to max length
  max_len <- config$history_max %||% HISTORY_MAX_DEFAULT
  if (length(history$messages) > max_len) {
    history$messages <- history$messages[(length(history$messages) - max_len + 1):
                                           length(history$messages)]
  }

  save_history(history)
  invisible(history)
}


#' Get conversation history
#'
#' @param n Integer. Number of recent messages to return. NULL = all.
#' @param as_messages Logical. If TRUE, returns in API message format.
#'
#' @return A list of messages.
#' @export
assistant_history <- function(n = NULL, as_messages = TRUE) {
  history <- load_history()
  msgs <- history$messages

  if (is.null(msgs) || length(msgs) == 0) {
    return(list())
  }

  if (!is.null(n)) {
    msgs <- tail(msgs, n)
  }

  if (as_messages) {
    # Strip timestamp for API format
    lapply(msgs, function(m) {
      list(role = m$role, content = m$content)
    })
  } else {
    msgs
  }
}


#' Clear conversation history
#'
#' @export
assistant_clear_history <- function() {
  save_history(list())
  message("Conversation history cleared.")
  invisible(NULL)
}


#' Null-coalescing operator
#' @noRd
`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}
