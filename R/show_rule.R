#' @title Shows existing rule
#'
#' @description
#' \code{show_rule} is a function to show defined rules. Can show all or several rules at once.
#'
#' @usage
#' show_rule(name, all = TRUE)
#'
#' @param name character. Specified name of the rule to be shown.
#'
#' @param all logical. Should all the rules be shown?
#'
#' @details To see all the rules, it is enough to use \code{show_rule}, alternatively you can specify the names of the rules to be shown. If all = TRUE (default value) is used, the name parametr name will not be evaluated at all.
#'
#' @return Returns a data.frame of defined rules.
#'
#' @author Michal Kubista
#'
#' @examples
#' \dontrun{
#' show_rule(name = "test1")
#' # show rule called "test1"
#'
#' show_rule()
#' # show all rules, equivalent to show_rule(all= TRUE)
#' }
show_rule <- function(name, all = TRUE){

      rule_set <- local(envir = .verifier, rule_set)

      if(all){
            return(rule_set)
      } else{
            return(rule_set[rule_set$name %in% name])
      }
}
