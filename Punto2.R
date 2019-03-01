# Punto 2

f <- function(x){
  tan(pi*x)-cos(pi*x)
}

parteA=function(x0){
  error=1;
  x1=x0;
  x2=0;
  cont=0;
  while( error>10e-4 && cont < 100 ){
    x0 = x1 - ( (f(x1) * (x1-x2)) / ((f(x1)-f(x2))) )
    error = abs(x0-x1) 
    x2=x1
    x1=x0
    cont=cont+1
  }
  cat( "Raiz #1",x0, " Iteraciones = ",cont, "\n")
}
cat ("Metodo 1\n")
parteA(0.4)
parteA(-1.2)

cat("Newton\n")


dx=function(x){
  pi*(1/cos(pi*x))^2+pi*sin(pi*x)
}


df <- function(x){
  pi*(1/cos(pi*x))^2+pi*sin(pi*x)
}

newton = function(x0, err){
  it = 0
  res = x0
  fx = f(x0)
  while (abs(fx) > err) {
    res = res - f(res) / df(res)
    fx = f(res)
    it = it + 1
    }
  cat( "Raiz #1",res, " Iteraciones = ",it, "\n")
}


newton(0.4,10e-4)
newton(-1.2,10e-4)
