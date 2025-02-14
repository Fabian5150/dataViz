library(ggplot2)

new_multiply_function <- function(a, b) {
  result <- a * b
  print(result)
}

new_multiply_function(5,3)

list_x <- list(c(5,3),5.8,list(2, 7.6, "Hi"))
myList <- list(tupel(4,5), "Hello there",list("this","is","a","sublist",TRUE))

myFunction <- function(s,n,l){
  print(s)
  print(n)
  print(l)
}

myFunction("Hello World", 5150, myList)

# list available datasets
data(package="ggplot2")
data(package = .packages(all.available = TRUE))

