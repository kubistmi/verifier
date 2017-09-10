#' @title Updates an existing rule
#'
#' @description
#' \code{update_rule} is a function to update the existing rules. Can update several arguments for a single rule.
#'
#' @usage
#' update_rule(name, update)
#'
#' @param name character. Specified name of the rule.
#'
#' @param update character. Named vector with parameters as names containing updated parameters.
#'
#' @details For more details on format of rules please go to \code{\link{write_rule}}.
#'
#' @return Return a message confirming the update of specified rule.
#'
#' @author Michal Kubista
#'
#' @examples
#' \dontrun{
#' update_rule(name = "test1", update = c(x = "mtcars$cyl", type = "def"))
#' }
update_rule <- function(name, update){
      if(length(name)>1){
            stop("Can update only one rule at a time.")
      }

      rule_set <- local(envir = .verifier, rule_set)

      rule_set[rule_set$name == name, names(update)] <- update

      assign("rule_set", rule_set, envir = .verifier)

      return(paste("The rule", name ,"was updated"))
}
