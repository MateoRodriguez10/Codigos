
A=matrix(c(2.6,0.3,2.4,6.2,
            7.7,0.4,4.7,1.4,
            5.1,9.9,9.5,1.5,
            6.0,7.0,8.5,4.8),nrow=4,byrow=4)

Ap=matrix(c(2.6,0.3,2.4,6.2,
            7.7,0.4,4.7,1.4,
            5.1,9.9,9.5,1.5,
            6.1,7.0,8.5,4.8),nrow=4,byrow=4)

b=c(50.78,47.36,91.48,98.17)

punto = function(A,Ap) {
  C = Ap-A
  SA = solve(A,b)
  SAp = solve (Ap,b)
  C2 = SAp - SA
  Ea = (norm(C, type = "I")) / (norm(A, type =  "I"))
  Esol = (max(C2)) / (max(SAp))
  cond = norm(A, type = "I")* (norm( inv(A), type = "I"))
  cota = cond *Ea
  cat("La cota de error es: ",cota,"% \n")
  cat(Ea*100," de distorcion en la matriz produjo una distorcion de  ", Esol*100 ," en la solución \n")
}
punto(A,Ap)