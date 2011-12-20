#This Script Impliments the Lehmann Primality Test
#There appears to be terrible shortcomings


primeTest <- function(n, iter){
  a <- sample(1:(n-1), 1)
    lehmannTest <- function(y, tries){
    x <- ((y^((n-1)/2)) %% n)
    if (tries == 0) {
      return(TRUE)
            }else{          
      if ((x == 1) | (x == (-1 %% n))){
        lehmannTest(sample(1:(n-1), 1), (tries-1))
        }else{
        return(FALSE)
      }
    }
  }
  lehmannTest(a, iter)
}

primeTest(4, 50) # false
primeTest(3, 50) # true
primeTest(10, 50)# false
primeTest(97, 50) # SHOULD BE TRUE !!!! WTF

prime_test<-c(2,3,5,7,11,13,17 ,19,23,29,31,37)

for (i in 1:length(prime_test)) {
  print(primeTest(prime_test[i], 50))
}

