library(deSolve)
pars<-c("alpha"=0.00001, "beta"=0.0714286,"gamma"=0.1,"delta"=0.2,"mu"=0.047619,"eta"=0.047619)
# functions are susceptible (S), infected (I), recovered (R), 
# hospitalized (H), in ICU (T), measured in thousands of people
# "alpha" - individuals moving from S to I
#   10% of contacts result in infections, each susceptible has a 1/10000 chance of
#   close interaction with each infectuous individual
# "beta" - individuals moving from  I to R
#   infection lasts about 14 days, so we assume 1/14=0.0714286 of all infected individuals 
#   recover each day
# "gamma" - individuals moving from I to H
#   about 10% of individuals who are infected must be checked-in to a hospital
# "delta" - individuals moving from H to T
#   about 20% of those checked-into a hospital require ICU
# "eta" - individuals moving from H to R
#   a hospital stay can last 3-6 weeks, so we assume that 1/21 = 0.047619 of
#   those in the hospital recover each day
# "mu" - individuals moving from T to H
#   we treat this like a hospital stay, moving someong in the ICU to R after
#   their stay

times<-seq(0,90,1)
y0<-c(330000,2,0,0,0)
sir<-function(t,y,p){
  # susceptible (sir.out[,2])
  yd1<-(-1)*p["alpha"]*y[1]*y[2]
  # infected (sir.out[,3])
  yd2<-p["alpha"]*y[1]*y[2]-p["beta"]*y[2]-p["gamma"]*y[2]
  # recovered (sir.out[,4])
  yd3<-p["beta"]*y[2]+p["mu"]*y[5]+p["eta"]*y[4]
  # hospitalized  (sir.out[,5])
  yd4<-p["gamma"]*y[2]-p["eta"]*y[4]-p["delta"]*y[4]
  # ICU'd  (sir.out[,6])
  yd5<-p["delta"]*y[4]-p["mu"]*y[5]
  list(c(yd1,yd2,yd3,yd4,yd5),c(N=sum(y)))
}
sir.out<-lsoda(y0,times,sir,pars)

# plot with S, I, R, H, T
plot(sir.out[,1],sir.out[,2],type="l",col="blue",xlab="Time",ylab="Compartment Size")
lines(sir.out[,1],sir.out[,3],col="red")
lines(sir.out[,1],sir.out[,4],col="green")
lines(sir.out[,1],sir.out[,5],col="black")
lines(sir.out[,1],sir.out[,6],col="purple",type="l")
legend(8,90,c("S","I","R","H"),col=c("blue","red","green","black"),lty=c(1,1,1,1))

# plot with H, T
plot(sir.out[,1],sir.out[,5],type="l",col="black",xlab="Time",ylab="Compartment Size")
lines(sir.out[,1],sir.out[,6],col="purple",type="l")
