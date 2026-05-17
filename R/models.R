#' List available models for a provider
#'
#' Returns the available models for the specified or current provider.
#'
#' @param provider Character. Provider name. If NULL, uses current provider.
#'
#' @return A character vector of model names.
#' @export
assistant_list_models <- function(provider = NULL) {
  if (is.null(provider)) {
    config <- assistant_get_config()
    provider <- config$provider
  }

  if (!provider %in% names(PROVIDERS)) {
    stop(sprintf("Unknown provider '%s'. Available: %s",
                 provider, paste(names(PROVIDERS), collapse = ", ")))
  }

  PROVIDERS[[provider]]$models
}


#' Interactively select a model
#'
#' Opens a selection menu (or Shiny gadget in RStudio) to choose a model.
#'
#' @param provider Character. Provider name. If NULL, uses current provider.
#'
#' @return The selected model name (invisibly).
#' @export
assistant_select_model <- function(provider = NULL) {
  if (is.null(provider)) {
    config <- assistant_get_config()
    provider <- config$provider
  }

  models <- assistant_list_models(provider)

  if (length(models) == 0) {
    message("No predefined models for provider '", provider, "'.")
    message("Use assistant_config(model = 'your-model-name') to set manually.")
    return(invisible(NULL))
  }

  config <- assistant_get_config()
  current <- config$model

  if (rstudioapi::isAvailable() && requireNamespace("shiny", quietly = TRUE)) {
    # Shiny gadget selector
    .model_selector_gadget(provider, models, current)
  } else {
    # Text menu
    cat(sprintf("\nAvailable models for '%s':\n", provider))
    for (i in seq_along(models)) {
      marker <- if (models[i] == current) " [current]" else ""
      cat(sprintf("  %d. %s%s\n", i, models[i], marker))
    }
    cat("\n")
    choice <- utils::menu(choices = models, title = "Select a model (0 to cancel):")
    if (choice > 0) {
      assistant_set_model(models[choice])
      message("Model set to: ", models[choice])
      invisible(models[choice])
    } else {
      invisible(NULL)
    }
  }
}


#' Shiny gadget for model selection
#' @noRd
.model_selector_gadget <- function(provider, models, current) {
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Select Model"),
    miniUI::miniContentPanel(
      shiny::tags$style(shiny::HTML("
        body { background: #1e1e2e; color: #cdd6f4; }
        .model-item {
          padding: 10px 14px; margin: 4px 0;
          background: #24283b; border: 1px solid #313244;
          border-radius: 8px; cursor: pointer;
          display: flex; align-items: center; justify-content: space-between;
          transition: all 0.15s;
        }
        .model-item:hover { border-color: #89b4fa; background: #2f3549; }
        .model-item.selected { border-color: #a6e3a1; background: #2f3549; }
        .model-name { font-size: 14px; font-weight: 500; }
        .model-badge {
          font-size: 10px; padding: 2px 8px;
          border-radius: 10px; background: #a6e3a1; color: #1e1e2e;
          font-weight: 600;
        }
        .provider-header {
          font-size: 12px; color: #89b4fa;
          text-transform: uppercase; letter-spacing: 1px;
          margin-bottom: 12px;
        }
      ")),
      shiny::div(class = "provider-header",
                 paste0("Provider: ", provider)),
      shiny::div(id = "modelList",
        lapply(models, function(m) {
          is_current <- (m == current)
          cls <- if (is_current) "model-item selected" else "model-item"
          badge <- if (is_current) shiny::span(class = "model-badge", "current") else NULL
          shiny::div(
            class = cls,
            `data-model` = m,
            onclick = sprintf("Shiny.setInputValue('selected_model', '%s')", m),
            shiny::span(class = "model-name", m),
            badge
          )
        })
      )
    )
  )

  server <- function(input, output, session) {
    shiny::observeEvent(input$selected_model, {
      model <- input$selected_model
      assistant_set_model(model)
      shiny::stopApp(model)
    })
    shiny::observeEvent(input$done, {
      shiny::stopApp(NULL)
    })
  }

  viewer <- shiny::dialogViewer("Select Model", width = 450, height = 400)
  result <- shiny::runGadget(ui, server, viewer = viewer)

  if (!is.null(result)) {
    message("Model set to: ", result)
  }
  invisible(result)
}
