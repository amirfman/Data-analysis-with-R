#install.packages("deSolve")
library (deSolve)
library(ggplot2)

#####BUILDING A NVPDZ model###########

# The model (i. e. the right hand side of the ODE)
model = function(t, state, parameters) {
  with(as.list(c(state, parameters)),{
    dN <- e*D - mu*P*(N/(N+kN))
    dP <- mu*P*(N/(N+kN)) - I*P*V - gz*P*Z - mp*P
    dV <- deltaV*I*P*V - mv*V
    dZ <- gz*P*Z - Z*mz
    dD <- mp*P+ Z*mz + V*mv + (1-deltaV)*I*P*V - e*D
    list(c(dN, dP, dV, dZ, dD))         #returns a list of equations
  })
}

# Parameters and their values
parameter<- c( e=0.014 ,      #Remineralization rate
               mu=0.45 ,      #Phytoplankton maximum growth rate
               kN=0.3 ,       #Half-saturation constant
               mp=0.008 ,     #Phytoplankton mortality
               I=0.06  ,      #infection rate
               gz=0.05 ,      #grazing rate 
               deltaV=0.01 , #virus production
               mv=0.001 ,     #Virus mortality
               mz=0.01        #Zooplankton mortality
)

parameter2<- c( e=0.3 ,      #Remineralization rate
                mu=0.8 ,     #Phytoplankton maximum growth rate
                kN=0.4 ,     #Half-saturation constant
                mp=0.01 ,    #Phytoplankton mortality
                I=0.5  ,     #infection rate
                gz=0.1 ,    #grazing rate 
                deltaV=0.01 ,#virus production
                mv=0.001 ,   #Virus mortality
                mz=0.03      #Zooplankton mortality
)

parameter3<- c( e=0.6 ,      #Remineralization rate
                mu=1.2 ,     #Phytoplankton maximum growth rate
                kN=0.5 ,     #Half-saturation constant
                mp=0.06 ,    #Phytoplankton mortality
                I=0.67  ,     #infection rate
                gz=0.69 ,    #grazing rate 
                deltaV=0.01 ,#virus production
                mv=0.001 ,   #Virus mortality
                mz=0.06      #Zooplankton mortality
)




initial_condition = c(N=97.499, P=2, V=0.001, Z=0.5, D=0) #####initial conditions of state variables


# Numerical method
numerical_method = "lsoda"

time = seq(0, 18000, by=0.2)

require(deSolve)

solution1 = ode(initial_condition, time, model, parameter, numerical_method)


plot(solution1)


# check for mass balance
solutiondf1 <-as.data.frame(solution1)
N<-solutiondf1$N
P<-solutiondf1$P
V<-solutiondf1$V
Z<-solutiondf1$Z
D<-solutiondf1$D
massbalance = N+P+Z+V+D
plot(time, massbalance)


 
solution2 = ode(initial_condition, time, model, parameter2, numerical_method)

plot(solution2)



# check for mass balance
solutiondf2 <-as.data.frame(solution2)
N<-solutiondf2$N
P<-solutiondf2$P
V<-solutiondf2$V
Z<-solutiondf2$Z
D<-solutiondf2$D
massbalance = N+P+Z+V+D
plot(time, massbalance)

solution3 = ode(initial_condition, time, model, parameter3, numerical_method)

plot(solution3)



# check for mass balance
solutiondf3 <-as.data.frame(solution3)
N<-solutiondf3$N
P<-solutiondf3$P
V<-solutiondf3$V
Z<-solutiondf3$Z
D<-solutiondf3$D
massbalance = N+P+Z+V+D
plot(time, massbalance)


b1<-ggplot()+ geom_path(data=solutiondf1, aes(x=time, y=V, col = "red"),na.rm = TRUE)+ geom_path(data=solutiondf2, aes(x=time, y=V, col ="green"),na.rm = TRUE)+ geom_path(data=solutiondf3, aes(x=time, y=V, col ="black"),na.rm = TRUE)

