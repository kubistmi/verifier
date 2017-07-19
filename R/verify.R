rule_set <- data.frame(name = "", LHS = "", RHS = "",
                       type = "", def = "", result="", other = "",
                       stringsAsFactors = F)

rule_set[1,] <- c("","test$mpg", "", "def", "median", "19.2", "")


if(type == "def"){
      res <- eval(parse(text = paste(rule_set$def, "(", rule_set$LHS , ")", sep="")))
      class(rule_set$result) <- class(res)
      rule_set$result == res
}


data_col <- unlist(strsplit("test$mpg", split = "[$]"))
get(data_col[1])[, data_col[2]]


# eval(parse(text = paste(rule_set$def, "(", rule_set$LHS , ")", sep="")))

# types = "basic", "integrity", "def"

# other: c(a="b",c="d") %>% strsplit(split=",")
