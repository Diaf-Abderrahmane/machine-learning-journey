l_rate = 0.01
m=3
n=3
a = 0
b = 0

reg_fun <- function(a, b, x) {
  return(a + b*x)
}

sum <- function(a, b, x, y) {
  return((reg_fun(a, b, 1)-2)^2 + (reg_fun(a, b, 2)-1)^2 + (reg_fun(a, b, 4)-5)^2)  
  
}

error <- function(n) {
  
  
  return((1/(2*n)) * sum(a, b, x, y) )
}



for (i in 1:100) {
  #error = (1/(2*n)*(((a+b*1)-2)^2+((a+b*2)-1)^2+((a+b*4)-5)^2))
  err <- error(n)
  print("error : ")
  print(err)
  print("a : ")
  print(a)
  print("b : ")
  print(b)
  print("- - - - - - ")
  a = a - l_rate*(1/m)*(((a+b*1)-2)+((a+b*2)-1)+((a+b*4)-5))
  b = b - l_rate*(1/m)*(((a+b*1)-2)*1+((a+b*2)-1)*2+((a+b*4)-5)*4)
}

