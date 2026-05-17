# Collect R Session Context
#
# Gather information about the current R session to provide
# context-aware assistance.

#' Get session context
#'
#' Collects information about the current R environment including
#' loaded packages, global variables, session info, and (if in RStudio)
#' the active document and selection.
#'
#' @param include_document Logical. Include active document content?
#' @param include_selection Logical. Include selected text?
#' @param include_session Logical. Include session info?
#' @param include_env Logical. Include global environment objects?
#' @param max_vars Integer. Max number of variables to describe.
#'
#' @return A list with context fields.
#' @importFrom utils capture.output head object.size str tail menu
#' @importFrom stats setNames
#' @importFrom rstudioapi getActiveDocumentContext isAvailable
#' @export
assistant_context <- function(include_document = TRUE,
                              include_selection = TRUE,
                              include_session = TRUE,
                              include_env = TRUE,
                              max_vars = 30) {
  ctx <- list(
    timestamp = Sys.time(),
    r_version = R.version.string,
    os = Sys.info()[["sysname"]],
    platform = R.version$platform
  )

  # Loaded packages
  if (include_session) {
    pkgs <- loadedNamespaces()
    base_pkgs <- c("base", "compiler", "datasets", "graphics", "grDevices",
                   "grid", "methods", "parallel", "splines", "stats",
                   "stats4", "tcltk", "tools", "utils")
    user_pkgs <- setdiff(pkgs, base_pkgs)
    ctx$loaded_packages <- sort(user_pkgs)
  }

  # Global environment objects
  if (include_env) {
    objs <- ls(envir = globalenv(), all.names = FALSE)
    if (length(objs) > 0) {
      obj_info <- lapply(head(objs, max_vars), function(nm) {
        val <- get(nm, envir = globalenv())
        list(
          name = nm,
          class = paste(class(val), collapse = "/"),
          type = typeof(val),
          size = tryCatch(format(object.size(val), units = "auto"),
                          error = function(e) "unknown"),
          dims = tryCatch(
            paste(dim(val), collapse = " x "),
            error = function(e) ""
          ),
          preview = tryCatch(
            {
              txt <- capture.output(str(val, max.level = 0, give.attr = FALSE))
              paste(head(txt, 3), collapse = "\n")
            },
            error = function(e) ""
          )
        )
      })
      names(obj_info) <- head(objs, max_vars)
      ctx$environment <- obj_info
    }
  }

  # RStudio context
  if (isAvailable()) {
    tryCatch({
      doc_ctx <- getActiveDocumentContext()

      if (include_selection && nzchar(doc_ctx$selection[[1]]$text)) {
        ctx$selected_code <- doc_ctx$selection[[1]]$text
      }

      if (include_document) {
        ctx$document_path <- doc_ctx$path
        ctx$document_type <- doc_ctx$type
        if (nzchar(doc_ctx$contents[1]) || length(doc_ctx$contents) > 1) {
          full_text <- paste(doc_ctx$contents, collapse = "\n")
          # Limit to 3000 chars to avoid token explosion
          if (nchar(full_text) > 3000) {
            ctx$document_content <- paste0(
              substr(full_text, 1, 3000),
              "\n... [truncated, ",
              nchar(full_text), " chars total]"
            )
          } else {
            ctx$document_content <- full_text
          }
        }
      }

      ctx$working_directory <- getwd()

    }, error = function(e) {
      ctx$rstudio_error <- conditionMessage(e)
    })
  }

  # Search path
  ctx$search_path <- search()

  # Current working directory
  ctx$working_directory <- getwd()

  ctx
}


#' Format context as a text block for inclusion in messages
#'
#' @param ctx A list from assistant_context.
#' @param max_length Integer. Maximum character length.
#'
#' @return A single character string.
#' @noRd
format_context <- function(ctx, max_length = 4000) {
  parts <- character(0)

  parts <- c(parts, sprintf(
    "=== R Session Context ===\nR: %s | OS: %s | Working Dir: %s",
    ctx$r_version, ctx$os, ctx$working_directory
  ))

  if (!is.null(ctx$loaded_packages)) {
    pkg_str <- paste(ctx$loaded_packages, collapse = ", ")
    parts <- c(parts, sprintf("\nLoaded packages: %s", pkg_str))
  }

  if (!is.null(ctx$environment)) {
    env_lines <- vapply(ctx$environment, function(obj) {
      sprintf("  %s [%s] %s%s",
              obj$name, obj$class, obj$size,
              if (nzchar(obj$dims)) paste0(" dims:", obj$dims) else "")
    }, character(1))
    parts <- c(parts, "\nGlobal environment:",
               paste(env_lines, collapse = "\n"))
  }

  if (!is.null(ctx$selected_code)) {
    parts <- c(parts, "\nSelected code:\n```r",
               ctx$selected_code, "```")
  }

  if (is.null(ctx$selected_code) && !is.null(ctx$document_content)) {
    parts <- c(parts, "\nCurrent document:",
               ctx$document_path,
               "```r", ctx$document_content, "```")
  }

  result <- paste(parts, collapse = "\n")
  if (nchar(result) > max_length) {
    result <- paste0(substr(result, 1, max_length), "\n... [context truncated]")
  }
  result
}
