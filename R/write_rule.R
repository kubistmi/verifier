#' @title Write new rule to be verified.
#'
#' @description
#' \code{write_rule} is a function to help you write new rules. Can write several rules at once using vectors.
#'
#' @usage
#' write_rule(name, x, type, y, def , result, other)
#'
#' @param name character. Name of the rule. Needs to be unique for every rule.
#'
#' @param x object. Data against which the rule is to be verified.
#'
#' @param type character. Possible types are:
#' \itemize{
#'  \item{\code{integ} checks for presence of all unique values of x in y}
#'  \item{\code{summary} checks summarised data against original, more detailed data}
#'  \item{\code{na} checks the missing values}
#'  \item{\code{numeric} checks outlying (user defined) numeric values}
#'  \item{\code{def} any other type, needs to be defined in def parametr}.
#'}
#' @param y vector. Used in \code{type} \code{integrity} (y is the reference table) or
#' \code{summary} (y is the summarised table).
#'
#' @param def character. Used for user defined rules using any function in evnironment, accepts only he name of the function, e.g. "sum".
#'
#' @param result numeric. Used for \code{type} \code{numeric} or \code{def} to specify bounds or expected value.
#'
#' @param other character. Used to specify other arguments passed into the user defined function, e.g. "na.rm = T".
#'
#' @details You can create several rules at once assigning vectors into the parametrs. When the \code{length} of any parameter is longer than one the function uses cbind to create a data.frame (will repeat values of vectors with smaller size). Names of the rules need to be unique.
#'
#' @return Returns a message confirming the creation of new rule.
#'
#' @author Michal Kubista
#'
#' @examples
#' \dontrun{
#' write_rule(name = "test1", x = mtcars$mpg, type = "def", def = c("mean", "median"),
#'            result = 19.2, other ="na.rm = T")
#' # verify, that the mean and median of mtcars$mpg is 19.2, omitting the missing values
#'
#' write_rule(name = "integrity1", x = data$cities, type = "integ", y = "ref$cities")
#' # verify, that all of the cities used in my data are in the reference table
#'
#' }

write_rule <- function(name, x, type, y = "", def = "",
                       result = "", other = ""){

      x<- deparse(substitute(x))

      unNames <- c(local(env = .verifier, rule_set$name), name)
      # make table
      if(length(unNames) > length(unique(unNames))){
            stop("You need to provide unique names for your rules")
      }

      maxlen <- max(length(x), length(type), length(def),
          length(result), length(other))

      if(maxlen > 1){
            nrule <- cbind(name, x, type, y, def, result, other)
      } else{
            nrule <- c(name, x, type, y, def, result, other)
      }

      # check type and if type = def, then def ....

      # rbind table
      assign("rule_set", rbind(local(env = .verifier, rule_set), nrule, stringsAsFactors = F),
             envir = .verifier)

      local(env= .verifier, colnames(rule_set) <- set_names)

      return(print(paste("New rule(s)", name ,"created")))
}
