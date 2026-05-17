# Test Shiny background launch
library(r.assistant)

config <- assistant_get_config()
config_path <- file.path(tempdir(), "ra_bg_init.rds")
saveRDS(list(init_sel = "", config = config), config_path)

port <- 55556
script_path <- file.path(tempdir(), "ra_bg_launcher.R")
writeLines(c(
  paste0("options(shiny.port = ", port, ")"),
  "library(shiny)",
  "library(r.assistant)",
  paste0("init <- readRDS('", gsub("\\\\", "/", config_path), "')"),
  "ui <- r.assistant:::.build_chat_ui(init$init_sel)",
  "server <- r.assistant:::.build_chat_server(init$init_sel)",
  "runApp(shinyApp(ui, server), port = getOption('shiny.port'), launch.browser = FALSE)"
), script_path)

cat("Script:", script_path, "\n")
cat("Content:\n")
cat(readLines(script_path), sep = "\n")
cat("\n\n")

# Use shell() to launch on Windows
rscript <- file.path(R.home("bin"), "Rscript.exe")
cmd <- paste0('start /B "" "', rscript, '" "', script_path, '"')
cat("Launching:", cmd, "\n")
shell(cmd, wait = FALSE)
cat("Launched. Waiting 8s...\n")
Sys.sleep(8)

# Check
url <- paste0("http://127.0.0.1:", port)
cat("Testing:", url, "\n")
tryCatch({
  resp <- httr2::request(url) |> httr2::req_timeout(5) |> httr2::req_perform()
  cat("Status:", httr2::resp_status(resp), "\n")
}, error = function(e) cat("Error:", conditionMessage(e), "\n"))
