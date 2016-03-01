# Hand-in Exercise 1
# The stop-loss paradox

S0<-100
r<-0.05
mu<-0.05
sigma<-0.3
sigma_hedge<-0.2

capT<-1
strike<-105 # X

Nhedge<-252
Nrep<-1000

# HEDGE: 

St<-rep(S0, length=Nrep)
dt<-capT/Nhedge

initialoutlay<-S0

a<- if(S0> strike * exp(-r*(capT))) 1 else 0
b<- if(S0> strike * exp(-r*(capT))) -1*strike else 0

Vpf<-a*S0+b*exp(dt*r)

RHS<-numeric()
for(i in 2:Nhedge){
  St<-St*exp((mu-0.5*sigma^2)*dt +sigma*sqrt(dt)*rnorm(Nrep))
  Vpf<-a*St+b*exp(dt*r)
  a<- ifelse(St> strike * exp(-r*(capT-(i-1)*dt)), 1, 0)
  b<- ifelse(St> strike * exp(-r*(capT-(i-1)*dt)), -1*strike, 0)
}

A<-data.frame('a'=a, 'b'=b, 'St'=St, 'Vpf'=Vpf)

# The new portfolio value: 
Vpf<-a*St+b*exp(dt*r)

hedgeerror<-(Vpf-pmax(St-strike,0))
optionpayoff<-pmax(St-strike,0)

# Formula: b(t)*exp(r*dt)+a(t)*S(t)=b(t+1)+a(t+1)*S(t)
# SF<-data.frame('St'=St, 'a'=a, 'b'=b, 'LHS'=Vpf, 'RHS'=RHS)
# for(i in 2:nrow(SF)){SF$RHS[i]<-SF$b[i]+SF$a[i]*SF$St[i-1]}
# plot(St, SF$LHS-SF$RHS, col="blue", xlab="S(T)", ylab=" ", ylim=c(-105,105), xlim=c(50,220), main=" ")
# lines(St, SF$RHS, col="green", type = 'p')
# text(50, -100, paste("b(t)*exp(r*dt)+a(t)*S(t)=b(t+1)+a(t+1)*S(t)"), adj=0)

# View(SF)

# SUMMARY STATS & GRAPHS
# ======================

print(paste("Initial investment =",round(initialoutlay,4)))
print(paste("Average discounted option payoff =",round(exp(-r*capT)*mean(optionpayoff),4)))
print(paste("Average discounted portfolio value =",round(exp(-r*capT)*mean(Vpf),4)))

plot(St,Vpf,col="deepskyblue2",xlab="S(T)",ylab="Value of hedge portfolio",ylim=c(-5,105),xlim=c(50,200),main="Hedge simulation of SLSG", cex=1.4)
text(50,100,paste("# hedge points =",Nhedge),adj=0)
points(50:200,pmax(50:200 - strike,0),type='l',lwd=2)
