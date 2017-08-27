#' @title Removes an existing rule
#'
#' @description
#' \code{update_rule} is a function to removes the existing rules. Can remove several rules at once.
#'
#' @usage
#' update_rule(name, all = FALSE)
#'
#' @param name character. Specified name of the rule to be removed
#'
#' @param all logical. Should all the rules be removed?
#'
#' @details If all = TRUE is used, the name parametr name will not be evaluated at all.
#'
#' @return Return a message confirming the removal.
#'
#' @author Michal Kubista
#'
#' @examples
#' \dontrun{
#' remove_rule(name = "test1")
#' # removes rule called "test1"
#'
#' remove_rule(all = TRUE)
#' # removes all rules
#' }
remove_rule <- function(name, all = FALSE){
      rule_set <- local(env = .verifier, rule_set)

      if(all){
            local(env=.verifier, {
                  rule_set <- data.frame(name = character(), x = character(),
                                         type = character(), y = character(),
                                         def = character(), result = character(),
                                         other = character(), stringsAsFactors = F)
            })
            return(paste("All rules were erased"))
      } else{
            assign("rule_set", rule_set[rule_set$name != name,], envir = .verifier)

            return(paste("The rule(s)", name ,"was/were erased"))
      }
}
