if(type == "def"){
      res <- eval(parse(text = paste(rule_set$def, "(", rule_set$LHS , ")", sep="")))
      class(rule_set$result) <- class(res)
      rule_set$result == res
}

fn = sum
fn(1:4) == sum(1:4)

data_col <- unlist(strsplit("test$mpg", split = "[$]"))
get(data_col[1])[, data_col[2]]

desc <- as.character(substitute(test$mpg))
get(desc[2])[,desc[3]]

# eval(parse(text = paste(rule_set$def, "(", rule_set$LHS , ")", sep="")))

# types = "basic", "integrity", "def"

# other: c(a="b",c="d") %>% strsplit(split=",")

eval(parse(text = "sum(mtcars$cyl)"))
eval(parse(text = "fn(mtcars$cyl)"))

