#' RStudio Addins for R Assistant
#'
#' Interactive Shiny gadgets that integrate with RStudio's Addin menu.

#' Chat Addin - Interactive conversation with the AI
#'
#' Opens a chat panel as an RStudio gadget.
#'
#' @export
addin_chat <- function() {
  if (!requireNamespace("miniUI", quietly = TRUE) ||
      !requireNamespace("shiny", quietly = TRUE)) {
    stop("This addin requires the 'shiny' and 'miniUI' packages.\n",
         "Install them with: install.packages(c('shiny', 'miniUI'))")
  }

  # Get initial selection as context hint
  init_msg <- ""
  if (rstudioapi::isAvailable()) {
    tryCatch({
      ctx <- rstudioapi::getActiveDocumentContext()
      sel <- ctx$selection[[1]]$text
      if (nzchar(trimws(sel))) {
        init_msg <- paste0("[Selected code]\n```r\n", sel, "\n```\n\nPlease help me with this.")
      }
    }, error = function(e) {})
  }

  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar(
      "R Assistant Chat",
      right = miniUI::miniTitleBarButton("clear", "Clear", primary = FALSE)
    ),
    miniUI::miniContentPanel(
      shiny::tags$style(shiny::HTML("
        .chat-container {
          height: 100%;
          display: flex;
          flex-direction: column;
        }
        .chat-messages {
          flex: 1;
          overflow-y: auto;
          padding: 10px;
          font-size: 13px;
          line-height: 1.5;
        }
        .msg-user {
          background: #e3f2fd;
          border-radius: 8px;
          padding: 8px 12px;
          margin: 5px 0 5px 30px;
          max-width: 85%;
          float: right;
          clear: both;
        }
        .msg-assistant {
          background: #f5f5f5;
          border-radius: 8px;
          padding: 8px 12px;
          margin: 5px 30px 5px 0;
          max-width: 85%;
          float: left;
          clear: both;
        }
        .msg-label {
          font-size: 11px;
          color: #888;
          margin-bottom: 2px;
        }
        .input-area {
          border-top: 1px solid #ddd;
          padding: 10px;
          display: flex;
          gap: 8px;
        }
        .input-area .form-group {
          flex: 1;
          margin-bottom: 0;
        }
        pre code {
          font-size: 12px;
        }
      ")),
      shiny::div(class = "chat-container",
        shiny::div(class = "chat-messages", id = "chatMessages",
          shiny::div(class = "msg-assistant",
            shiny::div(class = "msg-label", "Assistant"),
            shiny::HTML("Hello! I'm your R programming assistant. ",
                        "Ask me anything about R, or select code in the editor ",
                        "and I'll help you with it.<br><br>",
                        "<em>Tips: I can see your session context (loaded packages, ",
                        "variables). Just ask!</em>")
          )
        ),
        shiny::div(class = "input-area",
          shiny::textInput("userInput", label = NULL,
                           placeholder = "Type your message... (Shift+Enter for new line)",
                           width = "100%"),
          shiny::actionButton("send", "Send", class = "btn-primary btn-sm")
        )
      )
    )
  )

  server <- function(input, output, session) {
    # Reactive values
    messages <- shiny::reactiveVal(list())

    # Insert initial message if there was a selection
    if (nzchar(init_msg)) {
      shiny::insertUI("#chatMessages", where = "beforeEnd",
        ui = shiny::div(class = "msg-user",
          shiny::div(class = "msg-label", "You"),
          shiny::HTML(shiny::HTML(gsub("\n", "<br>", shiny::tags$code(init_msg))))
        )
      )
      messages(list(list(role = "user", content = init_msg)))
    }

    # Send message handler
    send_message <- function() {
      user_text <- trimws(input$userInput)
      if (!nzchar(user_text)) return()

      # Display user message
      shiny::insertUI("#chatMessages", where = "beforeEnd",
        ui = shiny::div(class = "msg-user",
          shiny::div(class = "msg-label", "You"),
          shiny::HTML(gsub("\n", "<br>", shiny::HTML(user_text)))
        )
      )

      # Clear input
      shiny::updateTextInput(session, "userInput", value = "")

      # Show "thinking" indicator
      shiny::insertUI("#chatMessages", where = "beforeEnd",
        ui = shiny::div(id = "thinking", class = "msg-assistant",
          shiny::em("Thinking...")
        )
      )

      # Update messages
      current <- messages()
      current <- c(current, list(list(role = "user", content = user_text)))
      messages(current)

      # Call API
      tryCatch({
        response <- .call_llm(
          messages = list(list(role = "user", content = user_text)),
          use_context = TRUE, use_history = TRUE, save = TRUE
        )

        # Remove thinking indicator
        shiny::removeUI("#thinking")

        # Format response (very basic markdown -> HTML)
        html_resp <- gsub("\n", "<br>", response)
        html_resp <- gsub("```r?(.*?)```",
                           "<pre><code>\\1</code></pre>", html_resp)
        html_resp <- gsub("`([^`]+)`", "<code>\\1</code>", html_resp)

        shiny::insertUI("#chatMessages", where = "beforeEnd",
          ui = shiny::div(class = "msg-assistant",
            shiny::div(class = "msg-label", "Assistant"),
            shiny::HTML(html_resp)
          )
        )

        # Scroll to bottom
        shiny::insertUI("#chatMessages", where = "afterEnd",
          ui = shiny::tags$script(shiny::HTML(
            "var el=document.getElementById('chatMessages'); if(el) el.scrollTop=el.scrollHeight;"
          ))
        )

      }, error = function(e) {
        shiny::removeUI("#thinking")
        shiny::insertUI("#chatMessages", where = "beforeEnd",
          ui = shiny::div(class = "msg-assistant",
            shiny::div(class = "msg-label", "Error"),
            shiny::HTML(paste0("<span style='color:red'>",
                               conditionMessage(e), "</span>"))
          )
        )
      })
    }

    # Observe send button
    shiny::observeEvent(input$send, { send_message() })

    # Clear chat
    shiny::observeEvent(input$clear, {
      assistant_clear_history()
      shiny::removeUI(".msg-user, .msg-assistant", multiple = TRUE)
      shiny::insertUI("#chatMessages", where = "beforeEnd",
        ui = shiny::div(class = "msg-assistant",
          shiny::div(class = "msg-label", "Assistant"),
          shiny::HTML("Chat cleared. How can I help you?")
        )
      )
      messages(list())
    })

    # Done button
    shiny::observeEvent(input$done, {
      shiny::stopApp()
    })
  }

  viewer <- shiny::paneViewer(minWidth = 450, minHeight = 500)
  shiny::runGadget(ui, server, viewer = viewer)
}


# --- Explain Code Addin ---

#' Explain Code Addin
#'
#' Explain selected code in the RStudio editor.
#'
#' @export
addin_explain <- function() {
  code <- .get_selection_or_stop()
  result <- assistant_explain(code)
  if (rstudioapi::isAvailable()) {
    tryCatch({
      .show_result_gadget("Code Explanation", result)
    }, error = function(e) {})
  }
}


# --- Refactor Code Addin ---

#' Refactor Code Addin
#'
#' Refactor selected code.
#'
#' @export
addin_refactor <- function() {
  code <- .get_selection_or_stop()
  result <- assistant_refactor(code)
  .offer_code_insertion(result)
}


# --- Fix Code Addin ---

#' Fix Code Addin
#'
#' Fix selected code or the last error.
#'
#' @export
addin_fix <- function() {
  code <- NULL
  error <- NULL

  if (rstudioapi::isAvailable()) {
    tryCatch({
      ctx <- rstudioapi::getActiveDocumentContext()
      sel <- ctx$selection[[1]]$text
      if (nzchar(trimws(sel))) code <- sel
    }, error = function(e) {})

    tryCatch({
      error <- geterrmessage()
      if (!nzchar(error)) error <- NULL
    }, error = function(e) {})
  }

  if (is.null(code)) {
    stop("Select the code that caused the error, then try again.")
  }

  result <- assistant_fix(code, error)
  .offer_code_insertion(result)
}


# --- Document Code Addin ---

#' Document Code Addin
#'
#' Generate documentation for selected function.
#'
#' @export
addin_document <- function() {
  code <- .get_selection_or_stop()
  result <- assistant_document(code)
  .offer_code_insertion(result, insert_before = TRUE)
}


# --- Helper: Show result in a gadget ---

.show_result_gadget <- function(title, text) {
  if (!requireNamespace("miniUI", quietly = TRUE) ||
      !requireNamespace("shiny", quietly = TRUE)) {
    cat(text, "\n")
    return(invisible(NULL))
  }

  # Basic markdown to HTML
  html <- gsub("\n", "<br>", text)
  html <- gsub("```r?(.*?)```", "<pre><code>\\1</code></pre>", html)

  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar(title),
    miniUI::miniContentPanel(
      shiny::HTML(html)
    )
  )

  server <- function(input, output, session) {
    shiny::observeEvent(input$done, { shiny::stopApp() })
  }

  viewer <- shiny::dialogViewer(title, width = 700, height = 600)
  shiny::runGadget(ui, server, viewer = viewer)
}


# --- Helper: Offer to insert code into editor ---

.offer_code_insertion <- function(response, insert_before = FALSE) {
  code_blocks <- extract_code_blocks(response)

  if (length(code_blocks) == 0) {
    cat(response, "\n")
    return(invisible(NULL))
  }

  if (!rstudioapi::isAvailable()) {
    cat(response, "\n")
    cat("\n--- Extracted code ---\n")
    for (cb in code_blocks) cat(cb, "\n---\n")
    return(invisible(NULL))
  }

  # Ask user via dialog
  choice <- utils::menu(
    title = paste0("Found ", length(code_blocks), " code block(s). What to do?"),
    choices = c(
      "Replace selected code with first block",
      "Insert code after selection",
      "Copy to clipboard",
      "Do nothing (just show)"
    )
  )

  switch(choice,
    # Replace selection
    {
      rstudioapi::insertText(code_blocks[[1]])
      message("Code replaced in editor.")
    },
    # Insert after
    {
      ctx <- rstudioapi::getActiveDocumentContext()
      end_pos <- ctx$selection[[1]]$range$end
      rstudioapi::insertText(end_pos, paste0("\n\n", code_blocks[[1]]))
      message("Code inserted after selection.")
    },
    # Copy to clipboard
    {
      if (requireNamespace("clipr", quietly = TRUE)) {
        clipr::write_clip(code_blocks[[1]])
        message("Code copied to clipboard.")
      } else {
        message("clipr package not available.")
      }
    },
    # Show only
    {
      cat(response, "\n")
    }
  )

  invisible(NULL)
}
