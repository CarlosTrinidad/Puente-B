# Parametros

s0<-100
miu<-0.05
sigma2<-0.3
T<-0.5

# L es el numero de muestras y N es el numero de Trayectorias

L <- 200000
N<- 1000
dt<-T/N
pts<-0
s<-0
hits<-0

for (j in 1:L) {

		B<- sqrt(T)*rnorm(1,0,1)
		s <- s0*exp((miu-sigma2/2)*T + sqrt(sigma2)*B)

	if (s > s0) {
		hits<-hits+1
	}
}
prob<-hits/L
print(hits)
print(prob)