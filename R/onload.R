.onLoad <- function(libname, pkgname = "verifieR") {
      .verifier <<- new.env()

      local(envir=.verifier, {
            rule_set <- data.frame(name = character(), x = character(),
                                   type = character(), y = character(),
                                   def = character(), result = character(),
                                   na.rm = character(), stringsAsFactors = F)
      })

      local(envir=.verifier, {
            set_names <- c("name", "x", "type", "y", "def", "result", "na.rm")
      })

      suppressPackageStartupMessages(require("data.table"))
}
