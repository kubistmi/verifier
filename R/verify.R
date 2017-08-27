verify <- function(name, all = FALSE){
      require(data.table)
      require(dplyr)

      rule_set <- local(envir = .verifier, rule_set)

      if(all){
            name <- rule_set$name
      }

      rule_set <- rule_set[rule_set$name %in% name,]

      result <- list()

      for(i in seq_along(rule_set$name)){
            rule <- rule_set[i,]
            x <- eval(parse(text = rule$x))

            result[[i]] <- list()
            result[[i]]$name <- rule$name

            if(rule$type == "integ"){
                  y <- eval(parse(text = rule$y))
                  unX <- unique(x)
                  check <-  unX %in% unique(y)
                  result[[i]]$check <- c("holds" = sum(check), "total" = length(unX))
                  result[[i]]$unsolved <-unX[!check]
            }

            if(rule$type == "summary"){
                  x <- as.data.table(x)

                  fn <- get(rule$def)

                  agX <- x[,.(value = fn(value, na.rm = rule$na.rm)), by = id]

                  y <- as.data.table(eval(parse(text = rule$y)))

                  xy <- full_join(agX, y, by = "id")
                  xy$diff <- abs(xy$value.x - xy$value.y)

                  result[[i]]$check <- c("holds" = sum(xy$diff == 0), "total" = length(xy$diff))
                  result[[i]]$unsolved <- xy[xy$check != 0,c("id","diff")]
            }

            if(rule$type == "na"){
                  naX <- is.na(x)

                  result[[i]]$check <- c("holds" = sum(!naX), "total" = length(x))
                  result[[i]]$unsolved_ind <- which(naX, FALSE)
            }

            if(rule$type == "def"){
                  fn <- get(rule$def)

                  if (rule$y == "") {
                        if(rule$na.rm == ""){
                              defX <- fn(x)
                        } else{
                              defX <- fn(x, na.rm = rule$na.rm)
                        }
                  } else {
                        y <- eval(parse(text = rule$y))

                        if(rule$na.rm == ""){
                              defX <- fn(x, res)
                        } else{
                              defX <- fn(x, y, na.rm = rule$na.rm)
                        }
                  }

                  if(length(defX)>1){
                        result[[i]]$check <- c("holds" = sum(defX), "total" = length(defX))
                        result[[i]]$unsolved_ind <- which(defX, FALSE)
                  } else{
                        result[[i]]$check <- c("expected" = rule$result,
                                               "reality" = defX,
                                               "holds" = rule$result == defX)
                  }
            }
      }

}

smaller_than<- function(x, y, na.rm = ){
      x < y
}

#####################################
#### CHECK OTHER -> NA.RM FUCKUP ####
#####################################
