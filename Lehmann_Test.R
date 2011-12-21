#This Script Impliments the Lehmann Primality Test
#There appears to be terrible shortcomings


modexp<-function(a, b, n){
    r = 1
    for (i in 1:b){
        r = (r*a) %% n
    }
    return(r)
}


primeTest <- function(n, iter){
   a <- sample(1:(n-1), 1)
    lehmannTest <- function(y, tries){
      x <- modexp(y, (n-1)/2, n)   
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
   if( n < 2 ){
     return(FALSE)
     }else if (n ==2) {
       return(TRUE)
       } else{
         lehmannTest(a, iter)
         }
}

primeTest(4, 50) # false
primeTest(3, 50) # true
primeTest(10, 50)# false
primeTest(97, 50) # NOW IS TRUE !!!!

prime_test<-c(5,7,11,13,17 ,19,23,29,31,37,1009)

for (i in 1:length(prime_test)) {
  print(primeTest(prime_test[i], 50))
}



