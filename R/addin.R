#' RStudio Addins for R Assistant
#'
#' Interactive AI chat panel for RStudio with streaming, context awareness,
#' and code actions. Inspired by Roo Code.

# --- Global state ---
.chat_app_env <- new.env(parent = emptyenv())
.chat_app_env$running <- FALSE
.chat_app_env$process <- NULL

# --- Chat Addin ---

#' Open R Assistant chat in RStudio Viewer pane
#' @export
addin_chat <- function() {
  if (!requireNamespace("shiny", quietly = TRUE))
    stop("This addin requires the 'shiny' package.")

  if (!is.null(.chat_app_env$process) && .chat_app_env$process$is_alive()) {
    .chat_app_env$process$kill()
    Sys.sleep(0.3)
  }

  port <- .find_free_port()

  init_sel <- ""
  if (rstudioapi::isAvailable()) {
    tryCatch({
      ctx <- rstudioapi::getActiveDocumentContext()
      sel <- ctx$selection[[1]]$text
      if (nzchar(trimws(sel))) init_sel <- sel
    }, error = function(e) {})
  }

  config <- assistant_get_config()
  config_path <- file.path(tempdir(), "ra_chat_init.rds")
  saveRDS(list(init_sel = init_sel, config = config), config_path)

  script_path <- file.path(tempdir(), "ra_chat_launcher.R")
  writeLines(con = script_path, c(
    paste0("options(shiny.port = ", port, ")"),
    "library(shiny)",
    "library(r.assistant)",
    paste0("init <- readRDS('", gsub("\\\\", "/", config_path), "')"),
    "ui <- r.assistant:::.build_chat_ui(init$init_sel)",
    "server <- r.assistant:::.build_chat_server(init$init_sel)",
    "runApp(shinyApp(ui, server), port = getOption('shiny.port'), launch.browser = FALSE)"
  ))

  rscript <- file.path(R.home("bin"), "Rscript.exe")
  if (!file.exists(rscript)) rscript <- file.path(R.home("bin"), "x64", "Rscript.exe")
  if (!file.exists(rscript)) rscript <- Sys.which("Rscript")

  if (Sys.info()["sysname"] == "Windows") {
    .chat_app_env$process <- processx::process$new(
      command = "cmd.exe", args = c("/c", rscript, script_path),
      supervise = FALSE, stdout = "|", stderr = "|")
  } else {
    .chat_app_env$process <- processx::process$new(
      command = rscript, args = script_path,
      supervise = FALSE, stdout = "|", stderr = "|")
  }
  .chat_app_env$running <- TRUE

  url <- paste0("http://127.0.0.1:", port)
  ready <- FALSE
  for (i in 1:50) {
    Sys.sleep(0.2)
    tryCatch({
      con <- url(url, open = "r", timeout = 1)
      close(con)
      ready <- TRUE
      break
    }, error = function(e) {})
  }

  if (ready && rstudioapi::isAvailable()) {
    rstudioapi::sendToConsole(sprintf('rstudioapi::viewer("%s")', url), execute = TRUE)
  } else if (ready) {
    browseURL(url)
  }

  message("[R Assistant] Chat at ", url)
  invisible(url)
}

#' Close chat panel
#' @export
addin_chat_close <- function() {
  if (!is.null(.chat_app_env$process) && .chat_app_env$process$is_alive()) {
    .chat_app_env$process$kill()
    message("[R Assistant] Chat closed.")
  }
  .chat_app_env$running <- FALSE
  .chat_app_env$process <- NULL
}

.find_free_port <- function() {
  found <- NA
  for (p in sample(49152:65535, 100)) {
    tryCatch({
      con <- serverSocket(p); close(con)
      found <- p; break
    }, error = function(e) {})
  }
  if (is.na(found)) 28100 else found
}


# --- Build UI ---

.build_chat_ui <- function(init_sel = "") {
  config <- assistant_get_config()
  shiny::fluidPage(
    title = "R Assistant",
    shiny::tags$head(
      shiny::tags$style(shiny::HTML("html,body{margin:0!important;padding:0!important;overflow:hidden!important;background:#1e1e2e;color:#cdd6f4}.container-fluid,.row{margin:0!important;padding:0!important}")),
      shiny::tags$style(shiny::HTML(.chat_css()))
    ),
    shiny::HTML(.chat_html(config$model, init_sel, config$provider))
  )
}


# --- Build Server (async + streaming) ---

.build_chat_server <- function(init_sel = "") {
  function(input, output, session) {
    last_response <- shiny::reactiveVal("")
    is_sending <- shiny::reactiveVal(FALSE)
    stream_text <- shiny::reactiveVal("")

    if (nzchar(init_sel)) {
      shiny::insertUI("#ra-messages", "beforeEnd",
        ui = .html_user_msg(paste0("[Selected code]\n```r\n", init_sel, "\n```")))
    }

    # --- Async send with streaming ---
    do_send <- function() {
      if (is_sending()) return()
      txt <- trimws(input$ra_input)
      if (!nzchar(txt)) return()

      is_sending(TRUE)
      stream_text("")
      shiny::updateTextAreaInput(session, "ra_input", value = "")
      shiny::insertUI("#ra-messages", "beforeEnd", ui = .html_user_msg(txt))
      shiny::insertUI("#ra-messages", "beforeEnd", ui = .html_streaming())
      .js_scroll()

      # Async API call
      future::future({
        r.assistant:::.call_llm(
          messages = list(list(role = "user", content = txt)),
          use_context = TRUE, use_history = TRUE, save = TRUE
        )
      }) %...>% (function(resp) {
        last_response(resp)
        shiny::removeUI("#ra-streaming")
        shiny::insertUI("#ra-messages", "beforeEnd", ui = .html_assistant_msg(resp))
        meta <- attr(resp, "meta")
        if (!is.null(meta))
          shiny::insertUI("#ra-messages", "beforeEnd", ui = .html_context_bar(meta))
        .js_scroll()
        .js_init_copy_buttons()
        is_sending(FALSE)
      }) %...!% (function(err) {
        shiny::removeUI("#ra-streaming")
        shiny::insertUI("#ra-messages", "beforeEnd",
          ui = .html_error_msg(conditionMessage(err)))
        .js_scroll()
        is_sending(FALSE)
      })
    }

    shiny::observeEvent(input$ra_send, { do_send() })

    # Model change
    shiny::observeEvent(input$ra_model_change, {
      m <- input$ra_model_change
      if (!is.null(m) && nzchar(m)) assistant_set_model(m)
    })

    # History
    shiny::observeEvent(input$ra_btn_history, {
      shiny::removeUI("#ra-history-panel", multiple = TRUE)
      shiny::insertUI("#ra-messages", "beforeEnd", ui = .html_history_panel())
    })

    # New chat
    shiny::observeEvent(input$ra_btn_new, {
      assistant_clear_history()
      shiny::removeUI(".ra-msg", multiple = TRUE)
      shiny::removeUI(".ra-ctx-bar", multiple = TRUE)
      shiny::insertUI("#ra-messages", "beforeEnd", ui = .html_welcome())
      last_response("")
    })

    # Insert code
    shiny::observeEvent(input$ra_btn_insert, {
      resp <- last_response()
      if (nzchar(resp)) {
        blocks <- extract_code_blocks(resp)
        if (length(blocks) > 0 && rstudioapi::isAvailable()) {
          rstudioapi::insertText(blocks[[1]])
          message("[R Assistant] Code inserted.")
        }
      }
    })

    # Settings panel
    shiny::observeEvent(input$ra_btn_settings, {
      shiny::removeUI("#ra-settings-panel", multiple = TRUE)
      shiny::insertUI("#ra-messages", "beforeEnd", ui = .html_settings_panel())
    })

    # Apply settings
    shiny::observeEvent(input$ra_apply_settings, {
      new_temp <- as.numeric(input$ra_set_temp)
      new_tokens <- as.integer(input$ra_set_tokens)
      new_ctx <- input$ra_set_context
      if (!is.na(new_temp)) assistant_config(temperature = new_temp)
      if (!is.na(new_tokens)) assistant_config(max_tokens = new_tokens)
      if (!is.null(new_ctx)) assistant_config(context_enabled = (new_ctx == "on"))
      shiny::removeUI("#ra-settings-panel", multiple = TRUE)
      message("[R Assistant] Settings updated.")
    })
  }
}


# --- HTML Templates ---

.html_welcome <- function() {
  shiny::HTML('
    <div class="ra-welcome">
      <div class="ra-logo"><svg width="40" height="40" viewBox="0 0 40 40" fill="none"><rect x="4" y="4" width="32" height="32" rx="8" fill="#89b4fa" opacity="0.15"/><path d="M12 28V12h6l6 8.5V12h4v16h-6l-6-8.5V28h-4z" fill="#89b4fa"/></svg></div>
      <div class="ra-welcome-title">R Assistant</div>
      <div class="ra-welcome-sub">AI-powered R programming assistant</div>
      <div class="ra-welcome-desc">Ask me anything about R. I can see your session context, analyze data, write code, fix errors, and more.</div>
    </div>')
}

.html_user_msg <- function(text) {
  shiny::HTML(paste0('<div class="ra-msg ra-msg-user"><div class="ra-msg-meta">You</div>',
    '<div class="ra-bubble ra-bubble-user">', shiny::HTML(text), '</div></div>'))
}

.html_assistant_msg <- function(text) {
  html <- .md_to_html(text)
  shiny::HTML(paste0('<div class="ra-msg ra-msg-assistant"><div class="ra-msg-meta">Assistant</div>',
    '<div class="ra-bubble ra-bubble-assistant">', html, '</div></div>'))
}

.html_streaming <- function() {
  shiny::HTML('<div class="ra-msg ra-msg-assistant" id="ra-streaming"><div class="ra-thinking"><span class="ra-dot"></span><span class="ra-dot"></span><span class="ra-dot"></span><span style="margin-left:6px;opacity:0.6">Thinking...</span></div></div>')
}

.html_error_msg <- function(msg) {
  shiny::HTML(paste0('<div class="ra-msg ra-msg-assistant"><div class="ra-msg-meta" style="color:#f38ba8">Error</div>',
    '<div class="ra-bubble ra-bubble-error">', shiny::HTML(msg), '</div></div>'))
}

.html_context_bar <- function(meta) {
  pct <- min(meta$context_pct, 100)
  bar_color <- if (pct > 80) "#f38ba8" else if (pct > 50) "#f9e2af" else "#a6e3a1"
  compressed_txt <- if (meta$compressed) " [compressed]" else ""
  usage_txt <- ""
  if (!is.null(meta$prompt_tokens) && meta$prompt_tokens > 0) {
    usage_txt <- sprintf(" | prompt: %s | response: %s",
      format(meta$prompt_tokens, big.mark = ","), format(meta$completion_tokens, big.mark = ","))
  }
  shiny::HTML(paste0('<div class="ra-ctx-bar"><div class="ra-ctx-track"><div class="ra-ctx-fill" style="width:', pct, '%;background:', bar_color, '"></div></div>',
    '<div class="ra-ctx-info">Context: ', format(meta$context_tokens, big.mark = ","), ' / ', paste0(round(meta$context_max/1000), 'k'), ' (', pct, '%)', usage_txt, compressed_txt, '</div></div>'))
}

.html_model_options <- function(provider, current_model) {
  models <- PROVIDERS[[provider]]$models
  if (length(models) == 0) return(shiny::HTML(paste0('<option value="', current_model, '">', current_model, '</option>')))
  opts <- vapply(models, function(m) {
    sel <- if (m == current_model) " selected" else ""
    paste0('<option value="', m, '"', sel, '>', m, '</option>')
  }, character(1))
  shiny::HTML(paste(opts, collapse = "\n"))
}

.html_history_panel <- function() {
  hist <- assistant_history(n = 30, as_messages = FALSE)
  if (length(hist) == 0) {
    return(shiny::HTML('<div class="ra-history-overlay" id="ra-history-panel"><div class="ra-history-header"><h4>History</h4><button class="ra-history-close" onclick="document.getElementById(\'ra-history-panel\').remove()">&times;</button></div><div class="ra-history-empty">No history yet.</div></div>'))
  }
  items <- vapply(hist, function(msg) {
    role_cls <- if (msg$role == "assistant") "assistant" else ""
    ts <- if (!is.null(msg$timestamp)) format(as.POSIXct(msg$timestamp), "%H:%M:%S") else ""
    txt <- msg$content
    if (nchar(txt) > 200) txt <- paste0(substr(txt, 1, 200), "...")
    txt <- gsub("<", "&lt;", txt, fixed = TRUE); txt <- gsub(">", "&gt;", txt, fixed = TRUE); txt <- gsub("\n", " ", txt, fixed = TRUE)
    paste0('<div class="ra-history-item"><span class="ra-hist-role ', role_cls, '">', toupper(msg$role), '</span><span class="ra-hist-time">', ts, '</span><div class="ra-hist-text">', txt, '</div></div>')
  }, character(1))
  shiny::HTML(paste0('<div class="ra-history-overlay" id="ra-history-panel"><div class="ra-history-header"><h4>History (', length(hist), ')</h4><button class="ra-history-close" onclick="document.getElementById(\'ra-history-panel\').remove()">&times;</button></div><div class="ra-history-list">', paste(items, collapse = ""), '</div></div>'))
}

.html_settings_panel <- function() {
  config <- assistant_get_config()
  shiny::HTML(paste0('
    <div class="ra-history-overlay" id="ra-settings-panel">
      <div class="ra-history-header">
        <h4>Settings</h4>
        <button class="ra-history-close" onclick="document.getElementById(\'ra-settings-panel\').remove()">&times;</button>
      </div>
      <div style="padding:16px">
        <div class="ra-setting-row">
          <label>Temperature</label>
          <input type="range" id="ra_set_temp" min="0" max="2" step="0.1" value="', config$temperature, '" oninput="this.nextElementSibling.textContent=this.value">
          <span>', config$temperature, '</span>
        </div>
        <div class="ra-setting-row">
          <label>Max Tokens</label>
          <input type="number" id="ra_set_tokens" value="', config$max_tokens, '" min="256" max="128000" style="width:100px;background:#313244;border:1px solid #45475a;color:#cdd6f4;padding:4px;border-radius:4px">
        </div>
        <div class="ra-setting-row">
          <label>Context Awareness</label>
          <select id="ra_set_context" style="background:#313244;border:1px solid #45475a;color:#cdd6f4;padding:4px;border-radius:4px">
            <option value="on"', if(config$context_enabled) " selected" else "", '>On</option>
            <option value="off"', if(!config$context_enabled) " selected" else "", '>Off</option>
          </select>
        </div>
        <div class="ra-setting-row">
          <label>Provider</label>
          <span style="color:#89b4fa">', config$provider, '</span>
        </div>
        <div class="ra-setting-row">
          <label>Model</label>
          <span style="color:#89b4fa">', config$model, '</span>
        </div>
        <button onclick="Shiny.setInputValue(\'ra_apply_settings\', Math.random())" style="margin-top:12px;background:#89b4fa;color:#1e1e2e;border:none;padding:6px 16px;border-radius:6px;cursor:pointer;font-weight:600">Apply</button>
      </div>
    </div>'))
}

.js_scroll <- function() {
  shiny::insertUI("body", "beforeEnd", ui = shiny::tags$script(shiny::HTML(
    "var el=document.getElementById('ra-messages'); if(el) el.scrollTop=999999;")))
}

.js_init_copy_buttons <- function() {
  shiny::insertUI("body", "beforeEnd", ui = shiny::tags$script(shiny::HTML("
    document.querySelectorAll('.ra-bubble pre').forEach(function(block) {
      if (block.querySelector('.ra-copy-btn')) return;
      var btn = document.createElement('button');
      btn.className = 'ra-copy-btn';
      btn.textContent = 'Copy';
      btn.onclick = function() {
        var code = block.querySelector('code');
        navigator.clipboard.writeText(code ? code.textContent : block.textContent);
        btn.textContent = 'Copied!';
        setTimeout(function() { btn.textContent = 'Copy'; }, 2000);
      };
      block.style.position = 'relative';
      block.appendChild(btn);
    });
    // Also init run buttons
    document.querySelectorAll('.ra-bubble pre code').forEach(function(code) {
      if (code.parentElement.querySelector('.ra-run-btn')) return;
      var btn = document.createElement('button');
      btn.className = 'ra-run-btn';
      btn.textContent = 'Run';
      btn.title = 'Send to R Console';
      btn.onclick = function() {
        Shiny.setInputValue('ra_run_code', code.textContent);
      };
      code.parentElement.appendChild(btn);
    });
  ")))
}


# --- Other Addins ---

addin_explain <- function() { code <- .get_selection_or_stop(); result <- assistant_explain(code); if (rstudioapi::isAvailable()) tryCatch(.show_result_viewer("Explanation", result), error=function(e){}) }
addin_refactor <- function() { code <- .get_selection_or_stop(); .offer_code_insertion(assistant_refactor(code)) }
addin_fix <- function() { code <- NULL; error <- NULL; if (rstudioapi::isAvailable()) { tryCatch({ctx<-rstudioapi::getActiveDocumentContext();sel<-ctx$selection[[1]]$text;if(nzchar(trimws(sel)))code<-sel},error=function(e){}); tryCatch({error<-geterrmessage();if(!nzchar(error))error<-NULL},error=function(e){}) }; if(is.null(code))stop("Select code first."); .offer_code_insertion(assistant_fix(code, error)) }
addin_document <- function() { code <- .get_selection_or_stop(); .offer_code_insertion(assistant_document(code), insert_before=TRUE) }


# --- Helpers ---

.show_result_viewer <- function(title, text) {
  html <- .md_to_html(text)
  ui <- shiny::fluidPage(shiny::tags$head(shiny::tags$style(shiny::HTML("body{background:#1e1e2e;color:#cdd6f4;padding:16px}pre{background:#11111b;padding:12px;border-radius:6px;overflow-x:auto;border:1px solid #313244}code{font-family:'Fira Code',Consolas,monospace;font-size:13px;color:#a6e3a1}"))), shiny::HTML(html))
  shiny::runGadget(shiny::shinyApp(ui, function(i,o,s){}), viewer=shiny::dialogViewer(title, width=700, height=500))
}

.md_to_html <- function(text) {
  blocks <- regmatches(text, gregexpr("```[rR]?\\s*\\n.*?```", text, perl=TRUE))[[1]]
  for (i in seq_along(blocks)) { ph <- paste0("%%CB",i,"%%"); text <- sub(blocks[i], ph, text, fixed=TRUE) }
  text <- gsub("&","&amp;",text,fixed=TRUE); text <- gsub("<","&lt;",text,fixed=TRUE); text <- gsub(">","&gt;",text,fixed=TRUE)
  for (i in seq_along(blocks)) {
    ph <- paste0("%%CB",i,"%%")
    cc <- sub("^[rR]?\\s*\\n","",blocks[i]); cc <- sub("\\n```$","",cc)
    cc <- gsub("&","&amp;",cc,fixed=TRUE); cc <- gsub("<","&lt;",cc,fixed=TRUE); cc <- gsub(">","&gt;",cc,fixed=TRUE)
    text <- sub(ph, paste0("<pre><code>",cc,"</code></pre>"), text, fixed=TRUE)
  }
  text <- gsub("`([^`]+)`","<code>\\1</code>",text)
  text <- gsub("\\*\\*(.+?)\\*\\*","<strong>\\1</strong>",text)
  text <- gsub("\\*(.+?)\\*","<em>\\1</em>",text)
  text <- gsub("\\n","<br>",text)
  text
}

.offer_code_insertion <- function(response, insert_before=FALSE) {
  code_blocks <- extract_code_blocks(response)
  if (length(code_blocks)==0) { cat(response,"\n"); return(invisible(NULL)) }
  if (!rstudioapi::isAvailable()) { cat(response,"\n"); cat("\n--- Code ---\n"); for(cb in code_blocks)cat(cb,"\n---\n"); return(invisible(NULL)) }
  choice <- utils::menu(title=paste0("Found ",length(code_blocks)," block(s)."), choices=c("Replace selection","Insert after","Copy clipboard","Show only"))
  switch(choice, {rstudioapi::insertText(code_blocks[[1]]);message("Replaced.")}, {ctx<-rstudioapi::getActiveDocumentContext();rstudioapi::insertText(ctx$selection[[1]]$range$end,paste0("\n\n",code_blocks[[1]]));message("Inserted.")}, {if(requireNamespace("clipr",quietly=TRUE)){clipr::write_clip(code_blocks[[1]]);message("Copied.")}}, {cat(response,"\n")})
  invisible(NULL)
}


# --- CSS ---

.chat_css <- function() {
  '
  *{box-sizing:border-box;margin:0;padding:0}
  body{font-family:-apple-system,BlinkMacSystemFont,"Segoe UI",sans-serif}
  .ra-wrap{display:flex;flex-direction:column;height:100vh;width:100%;background:#1e1e2e}

  .ra-header{display:flex;align-items:center;justify-content:space-between;padding:8px 12px;background:#181825;border-bottom:1px solid #313244;flex-shrink:0;min-height:40px}
  .ra-header-left{display:flex;align-items:center;gap:8px}
  .ra-header-left .ra-icon{width:20px;height:20px}
  .ra-header-title{font-size:13px;font-weight:600;color:#cdd6f4}
  .ra-header-center{flex:1;display:flex;justify-content:center}
  .ra-header-right{display:flex;align-items:center;gap:4px}
  .ra-model-select{background:#313244;border:1px solid #45475a;color:#cdd6f4;padding:4px 8px;border-radius:6px;font-size:11px;outline:none;cursor:pointer;min-width:140px;max-width:200px}
  .ra-model-select:focus{border-color:#89b4fa}
  .ra-model-select option{background:#313244;color:#cdd6f4}
  .ra-hbtn{background:none;border:1px solid transparent;color:#6c7086;width:28px;height:28px;border-radius:6px;cursor:pointer;display:flex;align-items:center;justify-content:center;font-size:14px;transition:all 0.15s}
  .ra-hbtn:hover{background:#313244;color:#cdd6f4;border-color:#45475a}
  .ra-hbtn svg{width:16px;height:16px}

  .ra-messages{flex:1;overflow-y:auto;padding:16px;scroll-behavior:smooth}
  .ra-messages::-webkit-scrollbar{width:5px}
  .ra-messages::-webkit-scrollbar-track{background:transparent}
  .ra-messages::-webkit-scrollbar-thumb{background:#45475a;border-radius:3px}

  .ra-msg{margin-bottom:16px;animation:raFade 0.2s ease}
  @keyframes raFade{from{opacity:0;transform:translateY(6px)}to{opacity:1}}
  .ra-msg-user{padding-left:24px}
  .ra-msg-assistant{padding-right:8px}
  .ra-msg-meta{font-size:10px;color:#585b70;margin-bottom:4px;padding:0 2px;text-transform:uppercase;letter-spacing:0.5px}
  .ra-bubble{padding:10px 14px;border-radius:12px;font-size:13px;line-height:1.6;word-wrap:break-word;white-space:pre-wrap}
  .ra-bubble-user{background:#313244;color:#cdd6f4;border-bottom-right-radius:4px}
  .ra-bubble-assistant{background:#181825;color:#cdd6f4;border:1px solid #313244;border-bottom-left-radius:4px}
  .ra-bubble-error{background:#181825;color:#f38ba8;border:1px solid #f38ba833;border-bottom-left-radius:4px}
  .ra-bubble code{background:#11111b;padding:1px 5px;border-radius:4px;font-size:12px;color:#f9e2af;font-family:"Fira Code",Consolas,monospace}
  .ra-bubble pre{background:#11111b;padding:10px 12px;border-radius:8px;margin:8px 0;overflow-x:auto;border:1px solid #313244;position:relative}
  .ra-bubble pre code{background:none;padding:0;color:#a6e3a1;font-size:12px;line-height:1.5}
  .ra-bubble strong{color:#fab387}
  .ra-bubble em{color:#94e2d5;font-style:italic}

  .ra-copy-btn{position:absolute;top:6px;right:6px;background:#313244;border:1px solid #45475a;color:#cdd6f4;padding:2px 8px;border-radius:4px;font-size:10px;cursor:pointer;opacity:0;transition:opacity 0.2s;z-index:10}
  .ra-bubble pre:hover .ra-copy-btn{opacity:1}
  .ra-copy-btn:hover{background:#45475a}

  .ra-run-btn{position:absolute;top:6px;right:50px;background:#a6e3a1;border:none;color:#1e1e2e;padding:2px 8px;border-radius:4px;font-size:10px;cursor:pointer;opacity:0;transition:opacity 0.2s;z-index:10;font-weight:600}
  .ra-bubble pre:hover .ra-run-btn{opacity:1}
  .ra-run-btn:hover{background:#9ece6a}

  .ra-thinking{display:flex;align-items:center;padding:10px 14px;font-size:12px;color:#6c7086}
  .ra-dot{width:6px;height:6px;margin:0 2px;background:#89b4fa;border-radius:50%;display:inline-block;animation:raDot 1.4s infinite ease-in-out both}
  .ra-dot:nth-child(1){animation-delay:0s}
  .ra-dot:nth-child(2){animation-delay:0.16s}
  .ra-dot:nth-child(3){animation-delay:0.32s}
  @keyframes raDot{0%,80%,100%{transform:scale(0.4);opacity:0.3}40%{transform:scale(1);opacity:1}}

  .ra-welcome{display:flex;flex-direction:column;align-items:center;justify-content:center;padding:40px 24px;text-align:center;height:100%}
  .ra-logo{margin-bottom:16px}
  .ra-welcome-title{font-size:18px;font-weight:600;color:#cdd6f4;margin-bottom:6px}
  .ra-welcome-sub{font-size:12px;color:#89b4fa;margin-bottom:16px}
  .ra-welcome-desc{font-size:12.5px;color:#6c7086;line-height:1.6;max-width:320px}

  .ra-input-wrap{padding:10px 12px;background:#181825;border-top:1px solid #313244;flex-shrink:0}
  .ra-input-box{display:flex;align-items:flex-end;gap:8px;background:#313244;border:1px solid #45475a;border-radius:12px;padding:6px 6px 6px 14px;transition:border-color 0.2s}
  .ra-input-box:focus-within{border-color:#89b4fa}
  .ra-input-box textarea{flex:1;background:none;border:none;color:#cdd6f4;font-size:13px;resize:none;outline:none;font-family:inherit;line-height:1.4;min-height:20px;max-height:80px;padding:4px 0}
  .ra-input-box textarea::placeholder{color:#585b70}
  .ra-send-btn{background:#89b4fa;color:#1e1e2e;border:none;border-radius:8px;width:30px;height:30px;cursor:pointer;display:flex;align-items:center;justify-content:center;flex-shrink:0;transition:background 0.15s}
  .ra-send-btn:hover{background:#74c7ec}
  .ra-send-btn svg{width:16px;height:16px}

  .ra-input-footer{display:flex;align-items:center;justify-content:space-between;padding:6px 4px 0}
  .ra-input-footer-left{display:flex;gap:10px}
  .ra-foot-icon{color:#585b70;font-size:13px;cursor:pointer;transition:color 0.15s}
  .ra-foot-icon:hover{color:#89b4fa}
  .ra-model-badge{font-size:10px;color:#585b70;display:flex;align-items:center;gap:4px}
  .ra-model-dot{width:6px;height:6px;border-radius:50%;background:#a6e3a1}

  .ra-ctx-bar{padding:4px 0 12px 0}
  .ra-ctx-track{height:3px;background:#313244;border-radius:2px;overflow:hidden;margin-bottom:4px}
  .ra-ctx-fill{height:100%;border-radius:2px;transition:width 0.3s ease}
  .ra-ctx-info{font-size:10px;color:#585b70}

  .ra-history-overlay{position:absolute;top:40px;left:0;right:0;bottom:0;background:#1e1e2e;z-index:100;display:flex;flex-direction:column;animation:raSlide 0.2s ease}
  @keyframes raSlide{from{opacity:0;transform:translateY(-10px)}to{opacity:1}}
  .ra-history-header{display:flex;align-items:center;justify-content:space-between;padding:10px 14px;background:#181825;border-bottom:1px solid #313244}
  .ra-history-header h4{margin:0;font-size:13px;color:#89b4fa}
  .ra-history-close{background:none;border:none;color:#6c7086;cursor:pointer;font-size:16px;padding:4px}
  .ra-history-close:hover{color:#cdd6f4}
  .ra-history-list{flex:1;overflow-y:auto;padding:12px}
  .ra-history-item{padding:8px 12px;margin-bottom:8px;background:#24283b;border:1px solid #313244;border-radius:8px;font-size:12px}
  .ra-history-item .ra-hist-role{font-size:10px;color:#89b4fa;text-transform:uppercase;letter-spacing:0.5px;margin-bottom:4px}
  .ra-history-item .ra-hist-role.assistant{color:#a6e3a1}
  .ra-history-item .ra-hist-time{font-size:10px;color:#585b70;float:right}
  .ra-history-item .ra-hist-text{color:#cdd6f4;line-height:1.4;max-height:60px;overflow:hidden;word-wrap:break-word;white-space:pre-wrap}
  .ra-history-empty{text-align:center;color:#585b70;padding:40px 20px;font-size:13px}

  .ra-setting-row{display:flex;align-items:center;gap:12px;margin-bottom:12px;font-size:12px}
  .ra-setting-row label{min-width:120px;color:#89b4fa}
  .ra-setting-row input[type=range]{flex:1;accent-color:#89b4fa}
  '
}


# --- HTML Structure ---

.chat_html <- function(model, init_sel, provider = "deepseek") {
  paste0('
  <div class="ra-wrap">
    <div class="ra-header">
      <div class="ra-header-left">
        <svg class="ra-icon" viewBox="0 0 20 20" fill="none"><rect x="2" y="2" width="16" height="16" rx="4" fill="#89b4fa" opacity="0.2"/><path d="M6 14V6h3l3 4.25V6h2v8h-3l-3-4.25V14H6z" fill="#89b4fa"/></svg>
        <span class="ra-header-title">R Assistant</span>
      </div>
      <div class="ra-header-center">
        <select id="ra_model_select" class="ra-model-select">', .html_model_options(provider, model), '</select>
      </div>
      <div class="ra-header-right">
        <button class="ra-hbtn" id="ra_btn_settings" title="Settings" onclick="Shiny.setInputValue(\'ra_btn_settings\', Math.random())">
          <svg viewBox="0 0 16 16" fill="currentColor"><path d="M8 4.754a3.246 3.246 0 100 6.492 3.246 3.246 0 000-6.492zM5.754 8a2.246 2.246 0 114.492 0 2.246 2.246 0 01-4.492 0z"/><path d="M9.796 1.343c-.527-1.79-3.065-1.79-3.592 0l-.094.319a.873.873 0 01-1.255.52l-.292-.16c-1.64-.892-3.433.902-2.54 2.541l.159.292a.873.873 0 01-.52 1.255l-.319.094c-1.79.527-1.79 3.065 0 3.592l.319.094a.873.873 0 01.52 1.255l-.16.292c-.892 1.64.901 3.434 2.541 2.54l.292-.159a.873.873 0 011.255.52l.094.319c.527 1.79 3.065 1.79 3.592 0l.094-.319a.873.873 0 011.255-.52l.292.16c1.64.893 3.434-.902 2.54-2.541l-.159-.292a.873.873 0 01.52-1.255l.319-.094c1.79-.527 1.79-3.065 0-3.592l-.319-.094a.873.873 0 01-.52-1.255l.16-.292c.893-1.64-.902-3.433-2.541-2.54l-.292.159a.873.873 0 01-1.255-.52l-.094-.319zm-2.633.283c.246-.835 1.428-.835 1.674 0l.094.319a1.873 1.873 0 002.693 1.115l.291-.16c.764-.415 1.6.42 1.184 1.185l-.159.292a1.873 1.873 0 001.116 2.692l.318.094c.835.246.835 1.428 0 1.674l-.319.094a1.873 1.873 0 00-1.115 2.693l.16.291c.415.764-.421 1.6-1.185 1.184l-.291-.159a1.873 1.873 0 00-2.693 1.116l-.094.318c-.246.835-1.428.835-1.674 0l-.094-.319a1.873 1.873 0 00-2.692-1.115l-.292.16c-.764.415-1.6-.421-1.184-1.185l.159-.291A1.873 1.873 0 001.945 8.93l-.319-.094c-.835-.246-.835-1.428 0-1.674l.319-.094A1.873 1.873 0 003.06 4.377l-.16-.292c-.415-.764.42-1.6 1.185-1.184l.292.159a1.873 1.873 0 002.692-1.116l.094-.318z"/></svg>
        </button>
        <button class="ra-hbtn" id="ra_btn_history" title="History" onclick="Shiny.setInputValue(\'ra_btn_history\', Math.random())">
          <svg viewBox="0 0 16 16" fill="currentColor"><path d="M1.5 8a6.5 6.5 0 1113 0 6.5 6.5 0 01-13 0zM8 0a8 8 0 100 16A8 8 0 008 0zm.5 4.75a.75.75 0 00-1.5 0v3.5a.75.75 0 00.37.65l2.5 1.5a.75.75 0 10.76-1.3L8.5 7.94V4.75z"/></svg>
        </button>
        <button class="ra-hbtn" id="ra_btn_new" title="New chat" onclick="Shiny.setInputValue(\'ra_btn_new\', Math.random())">
          <svg viewBox="0 0 16 16" fill="currentColor"><path d="M8 2a.75.75 0 01.75.75v4.5h4.5a.75.75 0 010 1.5h-4.5v4.5a.75.75 0 01-1.5 0v-4.5h-4.5a.75.75 0 010-1.5h4.5v-4.5A.75.75 0 018 2z"/></svg>
        </button>
        <button class="ra-hbtn" id="ra_btn_insert" title="Insert code" onclick="Shiny.setInputValue(\'ra_btn_insert\', Math.random())">
          <svg viewBox="0 0 16 16" fill="currentColor"><path d="M4.75 2A1.75 1.75 0 003 3.75v8.5c0 .966.784 1.75 1.75 1.75h6.5A1.75 1.75 0 0013 12.25V5.647a1.75 1.75 0 00-.512-1.238L10.36 2.512A1.75 1.75 0 009.124 2H4.75zM4.5 3.75a.25.25 0 01.25-.25H9v2.25c0 .966.784 1.75 1.75 1.75H12.5v5.75a.25.25 0 01-.25.25h-6.5a.25.25 0 01-.25-.25v-8.5z"/></svg>
        </button>
      </div>
    </div>

    <div class="ra-messages" id="ra-messages">', .html_welcome(), '</div>

    <div class="ra-input-wrap">
      <div class="ra-input-box">
        <textarea id="ra_input" rows="1" placeholder="Ask R Assistant... (Enter to send)"></textarea>
        <button class="ra-send-btn" id="ra_send" onclick="Shiny.setInputValue(\'ra_send\', Math.random())">
          <svg viewBox="0 0 16 16" fill="currentColor"><path d="M8 1.5a.5.5 0 01.5.5v10.793l3.146-3.147a.5.5 0 01.708.708l-4 4a.5.5 0 01-.708 0l-4-4a.5.5 0 01.708-.708L7.5 12.793V2a.5.5 0 01.5-.5z" transform="rotate(-90 8 8)"/></svg>
        </button>
      </div>
      <div class="ra-input-footer">
        <div class="ra-input-footer-left">
          <span class="ra-foot-icon" title="Auto context">&#x1F4CA;</span>
          <span class="ra-foot-icon" title="History">&#x1F552;</span>
        </div>
        <div class="ra-model-badge"><span class="ra-model-dot"></span>', model, '</div>
      </div>
    </div>
  </div>

  <script>
    $(document).on("keydown","#ra_input",function(e){if(e.key==="Enter"&&!e.shiftKey){e.preventDefault();Shiny.setInputValue("ra_send",Math.random())}});
    $(document).on("input","#ra_input",function(){this.style.height="auto";this.style.height=Math.min(this.scrollHeight,80)+"px"});
    $(document).on("change","#ra_model_select",function(){Shiny.setInputValue("ra_model_change",$(this).val())});
  </script>
  ')
}
