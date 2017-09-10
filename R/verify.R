#' @title Verifies specfified rules
#'
#' @description
#' \code{verify} is a function used to verify your defined rules. Can verify several rules at once.
#'
#' @usage
#' verify(name, all = FALSE)
#'
#' @param name character. Specified name/s of the rule to be removed.
#'
#' @param all logical. Should all the rules be removed?
#'
#' @details If all = TRUE is used, the name parametr name will not be evaluated at all.
#'
#' @return Returns a list of two objects:
#' \itemize{
#' \item{rules} table of all verified rules and results of the verification
#' \item{unsolved} list of vectors/tables, containing problematic values, or their row_indexes (rulename_index)
#' }
#'
#' @author Michal Kubista
#'
#' @examples
#' \dontrun{
#' verify(name = c("a", "b"))
#' verify(all = TRUE)
#' }
verify <- function(name, all = FALSE){
      suppressPackageStartupMessages(require("data.table"))

      rule_set <- local(envir = .verifier, rule_set)

      if(all){
            name <- rule_set$name
      }

      rule_set <- rule_set[rule_set$name %in% name,]

      df <- data.frame(matrix(ncol = 4, nrow = length(name)))
      colnames(df) <- c("name", "data", "expected", "satisfied")

      unsolved <- list()

      for(i in seq_along(rule_set$name)){
            rule <- rule_set[i,]
            x <- eval(parse(text = rule$x))

            if(rule$type == "integ"){
                  y <- eval(parse(text = rule$y))
                  unX <- unique(x)

                  check <-  unX %in% unique(y)

                  e <- sum(check)
                  f <- length(unX)
                  g <- e/f

                  unsolved[[i]] <-unX[!check]
                  names(unsolved)[i] <- paste(name[i], "id", sep = "_")
            }

            if(rule$type == "summary"){
                  x <- as.data.table(x)

                  if(sum(c("id","value") %in% colnames(x))==2){
                        x <- x[,c("id","value")]
                  } else if(ncol(x)==2){
                        colnames(x) <- c("id","value")
                  } else{
                        stop("x needs to be a data.frame/table object either with columns 'id' and 'value', or with two columns only (in this case, first column is expected to be id and second to contain value")
                  }

                  fn <- get(rule$def)

                  if (rule$na.rm == "") {
                        agX <-
                              x[,.(
                                    value = fn(value)
                              ), by = id]

                  } else {
                        agX <-
                              x[,.(
                                    value = fn(value, na.rm = as.logical(rule$na.rm))
                              ), by = id]
                  }

                  y <- as.data.table(eval(parse(text = rule$y)))

                  if (sum(c("id","value") %in% colnames(y)) == 2) {
                        y <- y[,c("id","value")]
                  } else if(ncol(y) == 2) {
                        colnames(y) <- c("id","value")
                  } else{
                        stop("y needs to be a data.frame/table object either with columns 'id' and 'value', or with two columns only (in this case, first column is expected to be id and second to contain value")
                  }

                  xy <- merge(agX, y, by = "id")

                  xy$diff <- abs(xy$value.x - xy$value.y)

                  e <- sum(xy$diff == 0)
                  f <- length(xy$diff)
                  g <- e/f

                  unsolved[[i]] <- xy[xy$check != 0, c("id","diff")]
                  names(unsolved)[i] <- paste(name[i], "id", sep = "_")
            }

            if(rule$type == "na"){
                  naX <- is.na(x)

                  e <- sum(!naX)
                  f <- length(x)
                  g <- e/f

                  unsolved[[i]] <- which(naX, FALSE)
                  names(unsolved)[i] <- paste(name[i], "index", sep = "_")
            }

            if(rule$type == "def"){
                  fn <- get(rule$def)

                  if (rule$y == "") {
                        if(rule$na.rm == ""){
                              defX <- fn(x)
                        } else{
                              defX <- fn(x, na.rm = as.logical(rule$na.rm))
                        }
                  } else {
                        y <- eval(parse(text = rule$y))

                        if(rule$na.rm == ""){
                              defX <- fn(x, y)
                        } else{
                              defX <- fn(x, y, na.rm = as.logical(rule$na.rm))
                        }
                  }

                  if(length(defX)>1){

                        e <- sum(defX)
                        f <- length(defX)
                        g <- e/f

                        unsolved[[i]] <- which(defX, FALSE)
                        names(unsolved)[i] <- paste(name[i], "index", sep = "_")
                  } else{

                        e <- defX
                        f <- as.numeric(rule$result)
                        g <- as.numeric(e == f)

                        unsolved[[i]] <- "no unsolved"
                        names(unsolved)[i] <- name[i]
                  }
            }

            df[i,]$name <- name[i]
            df[i,]$data <- e
            df[i,]$expected <- f
            df[i,]$satisfied <- g

            rm(e, f, g)

      }
      return(list(rules = df, unsolved = unsolved))
}
