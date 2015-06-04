bridge <- function() {
	
# Parte 1
#Parametros
	sigma2 <- 0.3
	T <- 2
	alpha <- -0.1
	M <- 11
	# M-1 = numero de nivel max
	N <- 2**M
	dt <- T/N
	P<-0
	j <- 2

	argument<- c()
	argument[1]=0
	argument[2]=2
	val<-c()
	#sabemos que el valor en 0 y 2 es de 0 en ambos puntos
	val[1]=0
	val[2]=0
# Parte 2

	for (i in 0:(M-1)) {

		for (k in 0:((2**i)-1)) {

				#indice es el valor de A en P(A)
					indice <- ((2*k + 1)/(2**(i+1)))*T
				# Agrega el valor en el ultimo lugar
					argument[length(argument)+1]=indice	
			
	#Buscamos el valor de P(A)
		# Determina el valor de los indices que se usan para calcular la media de P(A)
			indice1<- ((2*k)/(2**(i+1))) * T
			indice2<- ((2*k + 2)/(2**(i+1))) * T
		#Buscamos el indice 1 y el indice 2 en argument para conocer la posicion de los dos valores en 
		# el vector val
			pos1<-which( argument==indice1 , arr.ind= TRUE)
			pos2<-which( argument==indice2 , arr.ind= TRUE)

		#Al conocer la posicion en la que se encuentran los dos indices, se busca su respectivo P(A)
		#Este valor esta en la misma posicion del vector argument, por lo que no necesita ningun cambio
			
			# Calculamos P
			P =   ((1/2)*(val[pos1]+val[pos2])) + sqrt(sigma2*(T/(2**(i+2))))*rnorm(1,0,1)
			#Guardamos el valor P con su respectivo A (lugar)
			val[length(val)+1]=P	

			
		}
		
	}
# Parte 3
	#ordenar valores y argumentos, newArg son los valores ordenados
	newArg<- sort(argument)
	newVal<- c()
		for (i in 1:length(newArg)) {
			#identificar quien es el primer valor de newArg con respecto a argument
			pos<-which( argument == newArg[i] , arr.ind= TRUE)
			#agregamos en la posicion i el valor correspondiente
			newVal[i] = val[pos]

		}
	
	X <- ts(newVal, start = 0,end=2, deltat = dt)
    return(invisible(X))
	

}



# Parte 4

L <- 1000

# simulation parameters
hits <- 0
P_middle <- c()

for (i in  1:L){
  P <- bridge()
  P_middle <- c(P_middle, P[N/2 + 1])
  P.min <- min(P)
  hits <- hits + ifelse(P.min < alpha, 1, 0)
}

mean_middle <- mean(P_middle)
var_middle <- var(P_middle)
plot(P)
prob.hit <- hits/L
cat("\nprobability below alpha : ", prob.hit)
cat("\nmean_middle : ", mean_middle)
cat("\nvar_middle : ", var_middle)



