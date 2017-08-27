.verifier <- new.env()

local(env=.verifier, {
      rule_set <- data.frame(name = character(), x = character(),
                             type = character(), y = character(),
                             def = character(), result = character(),
                             other = character(), stringsAsFactors = F)
})

local(env=.verifier, {
      set_names <- c("name", "x", "type", "y", "def", "result", "other")
})
