#' @noRd
.onAttach <- function(libname, pkgname) {
  config <- tryCatch(assistant_get_config(), error = function(e) NULL)

  if (is.null(config) || !nzchar(config$api_key)) {
    packageStartupMessage(
      "\n[ R Assistant ] Welcome! First time setup:\n",
      "  assistant_setup()     # Interactive setup wizard\n",
      "  assistant_check()     # Verify configuration\n",
      "\n  Quick start:\n",
      "  assistant_config(provider = 'deepseek', api_key = 'your-key')\n"
    )
  } else {
    packageStartupMessage(
      "[ R Assistant ] Ready | ", config$provider, " / ", config$model
    )
  }
}
