#1 pseudoatsitiktinių dydžių generavimo funkciją


a <- 1664525 #       a -- generatoriaus parametras;
c <- 1013904223 #      c -- generatoriaus parametras;
m <- 2^32 #      m -- generatoriaus parametras (modulis);
n <- 10000   #    n -- generuojamų pseudoatsitiktinių dydžių skaičius;
X <-numeric(n)
seed<- 210
LCG <- function(n, a, c, m, seed) {
  X[1] <- seed
  for (i in 2:n) {
    X[i] <- (a * X[i-1] + c) %% m
  }
  return(X / m)  # Normalizuojame į [0, 1]
}

#2 Biufono uzduotis

pi_skaiciavimo <- function(n, r, a_geom){
  U1 <- LCG(n, a, c, m, seed)
  U2 <- LCG(n, a, c, m, 864)
  alfa <- (pi / 2) * U2
  x <- a_geom * U1
  k <- 0
  for(i in 1:n){
    if (r * cos(alfa[i]) >= x[i]){
      k <- k+1
    }
  }

  pi_apx <- (2*r*n) / (k * a_geom)
  
  return(pi_apx)
}

rezultatas <- pi_skaiciavimo(1000, 1, 2)
rezultatas

#3 grafiko braižymas 
metimu_skaicius <- numeric(46)
metimu_skaicius[1] <- 500
for(i in 2:46){
  metimu_skaicius[i]<- metimu_skaicius[i-1] +100
}
metimu_skaicius
pi_aproksimacija <- numeric(length(metimu_skaicius))

for(i in 1:length(metimu_skaicius)){
  pi_aproksimacija[i] <- pi_skaiciavimo(metimu_skaicius[i], 1, 2)
}
print(pi_aproksimacija)
plot(metimu_skaicius, pi_aproksimacija, type = 'b', pch = 19, xlab = "atidėtos n reikšmės", ylab = "skaičiaus pi įvertis", main = 'modeliavimo būdu gautas skaičiaus pi įvertis')
abline(h = pi, col = 2, lwd = 2)