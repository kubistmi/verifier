#' @title Removes an existing rule
#'
#' @description
#' \code{remove_rule} is a function to removes the existing rules. Can remove several rules at once.
#'
#' @usage
#' remove_rule(name = NULL, all = FALSE)
#'
#' @param name character. Specified name/s of the rule to be removed.
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
remove_rule <- function(name = NULL, all = FALSE){
      rule_set <- local(envir = .verifier, rule_set)

      if(all){
            local(envir = .verifier, {
                  rule_set <- data.frame(name = character(), x = character(),
                                         type = character(), y = character(),
                                         def = character(), result = character(),
                                         other = character(), stringsAsFactors = F)
            })
            return(paste("All rules were erased"))
      } else{
            if(!name %in% rule_set$name){
                  stop(paste0("There is no rule called ", name,"."))
            }
            assign("rule_set", rule_set[rule_set$name != name,], envir = .verifier)

            return(paste("The rule(s)", name ,"was/were erased"))
      }
}
