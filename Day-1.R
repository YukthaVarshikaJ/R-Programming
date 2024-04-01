#1-1
name=readline("Enter name : ")
age=readline("Enter age : ")
cat("Name : ",name ,"\nAge : ",age)
print(R.version.string)

#1-2
# List all objects in memory
objects <- ls()

# Get details of each object
details <- lapply(objects, function(obj) {
  size <- object.size(get(obj))
  class <- class(get(obj))
  list(Name = obj, Size = size, Class = class)
})

# Print the details
for (detail in details) {
  print(detail)
}



#1-3
print("Sequence of numbers from 20 to 50:")
print(seq(20,50))
print("Mean of numbers from 20 to 60:")
print(mean(20:60))
print("Sum of numbers from 51 to 91:")
print(sum(51:91))

#1-4
v = sample(-50:50, 10, replace=FALSE)
print(v)

#1-5
n<-c(10)
{
  a <- 0
  b <- 1
  cat("Fibonacci Sequence : ")
  for (i in 1:n) {
    cat(a, " ")
    new <- a + b
    a <- b
    b <- new
  }
}

#1-6
prime_numbers <- function(n) {
  if (n >= 2) {
    x = seq(2, n)
    prime_nums = c()
    for (i in seq(2, n)) {
      if (any(x == i)) {
        prime_nums = c(prime_nums, i)
        x = c(x[(x %% i) != 0], i)
      }
    }
    return(prime_nums)
  }
  else 
  {
    stop("Input number should be at least 2.")
  }
} 
prime_numbers(12)

#1-7
for (i in 1:100) {
  if (i %% 3 == 0 & i %% 5 == 0) 
    {
    print("FizzBuzz")
  } else if (i %% 3 == 0) 
    {
    print("Fizz")
  } else if (i %% 5 == 0) 
    {
    print("Buzz")
  } else 
    {
    print(i)
  }
}

#1-8
print("First 10 letters in lower case:")
small = head(letters, 10)
print(small)
print("Last 10 letters in upper case:")
capital = tail(LETTERS, 10)
print(capital)
print("Letters between 22nd to 24th letters in upper case:")
caps = tail(LETTERS[22:24])
print(caps)


#1-9
n<-6
  {
  print(paste("The factors of", n,"are : "))
  for(i in 1 : n)
    {
    if(n %% i == 0)
      {
      print(i)
    }
  }
}


#1-10
nums = c(10, 20, 30, 40, 50, 60)
print('Original vector:')
print(nums)   
print(paste("Maximum value of the said vector:",max(nums)))
print(paste("Minimum value of the said vector:",min(nums)))



#anlytical-1
matrix1 <- matrix(1:6, nrow = 2, byrow=TRUE)
print(matrix1)
matrix2 <- matrix(7:12, nrow = 2, byrow=TRUE)
print(matrix2)

combined_cbind <- cbind(matrix1, matrix2)
print("Combined using cbind():")
print(combined_cbind)

combined_rbind <- rbind(matrix1, matrix2)
print("Combined using rbind():")
print(combined_rbind)

#analytical-2
numeric_vec <- c(1:6)
complex_vec <- c(1 + 2i, 2 + 3i, 3 + 4i, 4 + 5i, 5 + 6i, 6 + 7i)
logical_vec <- c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE)
character_vec <- c("a", "b", "c", "d", "e", "f")

print("Numeric Vector:")
print(numeric_vec)
print("Complex Vector:")
print(complex_vec)
print("Logical Vector:")
print(logical_vec)
print("Character Vector:")
print(character_vec)

given_list <- list(g1 = "Item 1", g2 = "Item 2", g3 = "Item 3")

given_list$g4 <- "R Prog"

print("Updated List:")
print(given_list)


#analytical-3
rice_price <- 40.75
sugar_price <- 30

rice_kg <- 2
sugar_kg <- 5

total_amount <- (rice_price * rice_kg) + (sugar_price * sugar_kg)
print("Total Amount of Purchase:")
print(total_amount)


#analytical-4
set.seed(123) 
matrix_data <- matrix(sample(1:100, 12), nrow = 3, byrow = TRUE)

rownames(matrix_data) <- c("x", "y", "z")
colnames(matrix_data) <- c("uno", "dos", "tres", "cuatro")

scaled_matrix <- matrix_data * 10

print("Scaled Matrix:")
print(scaled_matrix)


#analytical-5
my_class <- function(x) 
  {
  obj <- list(data = x)
  class(obj) <- "my_class"
  return(obj)
}
print.my_class <- function(x) 
  {
  cat("This is an object of class 'my_class' with data:\n")
  print(x$data)
}
obj <- my_class(10)
print(obj)









