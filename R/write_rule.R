#' @title Write new rule to be verified.
#'
#' @description
#' \code{write_rule} is a function to help you write new rules. Can write several rules at once using vectors.
#'
#' @usage
#' write_rule(name, x, type, y = NULL, def = "", result = "", na.rm = "", multiple)
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
#'  \item{\code{def} any other type, needs to be defined in def parametr}.
#'}
#' @param y vector. Used in \code{type} \code{integrity} (y is the reference table),
#' \code{summary} (y is the summarised table), or in some cases of \code{def}.
#'
#' @param def character. Used for user defined rules using any function in environment, accepts only the name of the function, e.g. "sum".
#'
#' @param result numeric. Used for \code{type} \code{numeric}, \code{def}, or \code{summary} to specify bounds, expected value or accuracy for \code{summary}, check \strong{Details} for more explanation.
#'
#' @param na.rm logical or empty (""). Should the \code{NA} values be ommitted in computations of summaries and user defined functions? Empty, if the function does not use na.rm argument (or it should use default value).
#'
#' @param multiple logical. Are multiple rules being written? If TRUE, then x (and y, if used) should be the names of the objects (not the objects themself)
#'
#' @details You can create several rules at once assigning vectors into the parametrs and specifying \code{multiple = TRUE}. When the \code{length} of any parameter is longer than one the function uses cbind to create a data.frame (will repeat values of vectors with smaller size). Names of the rules need to be unique. \cr
#' Special requirements:
#'\itemize{
#'  \item{\code{summary} Both \code{x} and \code{y} are expected to be a data.frame/table object either with columns 'id' and 'value', or with two columns only (in this case, first column is expected to be id and second to contain value)}.
#'  \item{\code{def} The defined function must have a name (you cannot use operators such as \code{<}). If you want to use two parameters (not only \code{x}) in the function (such as rewriting operator as named function \code{function(a,b){a < b}}), then you need to specify the other parameter as \code{y}. If you want to compare a result of a function with some predefined value (suck as \code{mean(x) == number}), you need to specify the parameter \code{result} as this \code{number}. If the function output is vector with lenght > 1, it is expected to contain logical values and will be summarized (not compared with parameter \code{result}) for the final output.}.
#'}
#' There is a \strong{special use} of parameter \code{result} in case of \code{type = summary}, in this case \code{result} can be used to define accuracy (tolerance of difference between values). Default tolerance is 0 (compared using \code{==}.)
#' @return Returns a message confirming the creation of new rule.
#'
#' @author Michal Kubista
#'
#' @examples
#' \dontrun{
#' write_rule(name = "test1", x = mtcars$mpg, type = "def", def = "mean",
#'            result = 19.2, na.rm = TRUE)
#' # verify, that the mean  of mtcars$mpg is 19.2, omitting the missing values
#'
#' write_rule(name = "integrity1", x = data$cities, type = "integ", y = "ref$cities")
#' # verify, that all of the cities used in the data are present in the reference table
#' }

write_rule <- function(name, x, type, y = NULL, def = "",
                       result = "", na.rm = "", multiple = FALSE){

      if(!exists("name")){
            stop("You need to provide a name for your rule.")
      }

      if(!multiple){
            x <- deparse(substitute(x))
      }

      if(!is.null(y)){
            y <- deparse(substitute(y))
      } else{
            y <- ""
      }

      unNames <- c(local(envir = .verifier, rule_set$name), name)
      # make table
      if(length(unNames) > length(unique(unNames))){
            stop("You need to provide unique names for your rules")
      }

      maxlen <- max(length(x), length(type), length(def),
          length(result), length(na.rm))

      if(maxlen > 1){
            nrule <- cbind(name, x, type, y, def, result, na.rm)
      } else{
            nrule <- c(name, x, type, y, def, result, na.rm)
      }

      # rbind table
      assign("rule_set", rbind(local(envir = .verifier, rule_set), nrule, stringsAsFactors = F),
             envir = .verifier)

      local(envir= .verifier, colnames(rule_set) <- set_names)

      return(print(paste("New rule(s)", name ,"created")))
}
