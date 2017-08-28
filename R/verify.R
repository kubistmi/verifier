verify <- function(name, all = FALSE){
      require(data.table)
      require(dplyr)

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

                  fn <- get(rule$def)

                  agX <- x[,.(value = fn(value, na.rm = rule$na.rm)), by = id]

                  y <- as.data.table(eval(parse(text = rule$y)))

                  xy <- full_join(agX, y, by = "id")
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
                              defX <- fn(x, na.rm = rule$na.rm)
                        }
                  } else {
                        y <- eval(parse(text = rule$y))

                        if(rule$na.rm == ""){
                              defX <- fn(x, y)
                        } else{
                              defX <- fn(x, y, na.rm = rule$na.rm)
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

smaller_than<- function(x, y, na.rm = ){
      x < y
}

#####################################
#### CHECK OTHER -> NA.RM FUCKUP ####
#####################################
