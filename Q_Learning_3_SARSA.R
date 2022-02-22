rm(list=ls()); graphics.off(); cat("\014")

setwd()

library(arules)
library(readxl)

# *******************************************************************************
# User's parameters
# *******************************************************************************
# ----
initial.cash <- 5000 # NA for automatic setup (i.e., 10 * max price overall )
initial.shares <- 0 # DO NOT CHANGE!
costs.perc <- 0.019 # usually 1.9%
tick <- 5 # discretization step for the state generation

win <- 20
dt <- 5

modified.reward <- T

episodes <- 100000
refining.episodes <- 5000

lr <- 0.9
lr.lin.decay <- F # suggested: FALSE

eps <- 0.5
eps.lin.decay <- T

df <- 0.9
df.lin.decay <- F # suggested: FALSE

training.perc <- 0.75
seed <- 42

visualize.every <- 5000
# ----

# *******************************************************************************
# Loading data
# *******************************************************************************
# ----
# read data
Fondi_esterni <- read_excel("Fondi esterni.xlsx")
price <- as.numeric( Fondi_esterni$NVALOREQUOTA )
#price <- rev(price) # because the dates are stored in the opposite way
# split into training and test
ix <- round(training.perc*length(price))
price.trn <- price[1:ix]
price.tst <- price[-c(1:ix)]
# just a plot...
plot( price, type="l", lwd=2, col="green3" )
lines( 1:ix, price.trn, lwd=2, col="blue" )
abline(v=ix,col="grey",lty=2)
# ----



# *******************************************************************************
# Initialization
# *******************************************************************************
# ----
set.seed(seed)

if( is.na(initial.cash) )
  initial.cash <- 1.25*max(price.trn)

if( eps.lin.decay ) { eps.step = eps/episodes } else { eps.step = 0 }
if( lr.lin.decay ) { lr.step = lr/episodes } else { lr.step = 0 }
if( df.lin.decay ) { df.step = df/episodes } else { df.step = 0 }

Q <- data.frame( cash = numeric(),
                 r.m = numeric(),
                 r.s = numeric(),
                 a = character(),
                 q = numeric(),
                 stringsAsFactors = F )

all.cash.trn <- matrix(NA,episodes+refining.episodes,length(price.trn))
all.reward.trn <- matrix(NA,episodes+refining.episodes,length(price.trn))
all.actions.trn <- matrix("",episodes+refining.episodes,length(price.trn))
# ----

E <- 1 

while( E <= episodes+refining.episodes ) {
  
  cash <- initial.cash
  shares <- initial.shares
  
  t <- win
  

  s.next <- NULL
  a.next <- NULL
    
  while( t <= length(price.trn) ) {
    
    if( !is.null(s.next) ) {
      s <- s.next 
    } else {
      # construct state
      ret <- diff( price.trn[(t-win+1):t] )
      # s <- round(c( cash, mean(ret), sd(ret) )/tick)
      s <- c( round((cash-initial.cash)/tick), round(mean(ret)/tick,1), round(sd(ret)/tick,1) ) 
      
      ix <- which( Q$cash==s[1] & Q$r.m==s[2] & Q$r.s==s[3] )
      if( length(ix)==0 ) {
        Q <- rbind( Q, data.frame( cash=rep(s[1],3),
                                   r.m=rep(s[2],3),
                                   r.s=rep(s[3],3),
                                   a=c("buy","hold","sell"),
                                   q=numeric(3),
                                   stringsAsFactors=F ))
        ix <- (nrow(Q)-2):nrow(Q)
      }
    }
    
    
    # select action
    if( !is.null(a.next) ) {
      a <- a.next
    } else {
      if( t==length(price.trn) ) {
        a <- "sell"
      } else {
        if( E<=episodes && runif(1)<eps ) {
          a <- sample( c("buy","hold","sell"), 1 )
        } else {
          ii <- which( Q$q[ix]==max(Q$q[ix]))
          if( length(ii)>1 )
            ii <- sample(ii,1)
          a <- Q$a[ix[ii]]
        }
      }
    }
    
    
    # perform action and observe the reward
    if( a=="buy" ) {
      new.shares <- floor(cash/price.trn[t])
      cost <- new.shares * (1+costs.perc) * price.trn[t]
      if( new.shares==0 || cost>cash ) {
        reward <- -Inf
      } else {
        shares <- shares+new.shares
        cash <- cash - cost
        reward <- 0
      }
    } else {
      if( a=="sell" ) {
        if( t==length(price.trn) ) {
          reward <- cash - initial.cash
        } else {
          if( shares==0 ) {
            reward <- -Inf
          } else {
            cost <- shares * costs.perc * price.trn[t]
            cash.prev <- cash
            cash <- cash + shares * price.trn[t] - cost
            shares <- 0
            if( !modified.reward ) {
              reward <- 0
            } else {
              reward <- cash - initial.cash
            }
          }
        }
      } else {
        reward <- 0
      }
    }
    
    # SARSA: observe also new state!
    # construct state
    ret.next <- diff( price.trn[(t-win+1+1):(t+1)] )
    # s.next <- round(c( cash, mean(ret), sd(ret) )/tick)
    s.next <- c( round((cash-initial.cash)/tick), round(mean(ret)/tick,1), round(sd(ret)/tick,1) ) 
    
    # SARSA: add new state if unseen...
    ix.next <- which( Q$cash==s.next[1] & Q$r.m==s.next[2] & Q$r.s==s.next[3] )
    if( length(ix.next)==0 ) {
      Q <- rbind( Q, data.frame( cash=rep(s.next[1],3),
                                 r.m=rep(s.next[2],3),
                                 r.s=rep(s.next[3],3),
                                 a=c("buy","hold","sell"),
                                 q=numeric(3),
                                 stringsAsFactors=F ))
      ix.next <- (nrow(Q)-2):nrow(Q)
    }
    
    # SARSA: choose new action from new state!
    if( (t+1)==length(price.trn) ) {
      a.next <- "sell"
    } else {
      if( E<=episodes && runif(1)<eps ) {
        a.next <- sample( c("buy","hold","sell"), 1 )
      } else {
        ii.next <- which( Q$q[ix.next]==max(Q$q[ix.next]))
        if( length(ii.next)>1 )
          ii <- sample(ii.next,1)
        a.next <- Q$a[ix.next[ii]]
      }
    }
    
    
    # SARSA: update Q table
    jj <- which( Q$cash==s.next[1] & Q$r.m==s.next[2] & Q$r.s==s.next[3] )
      
    if( (t+1)==length(price.trn[t]) || length(jj)==0 ) {
      Q.max <- 0
    } else {
      Q.max <- max(Q$q[jj])
    }
    
    ix <- which( Q$cash==s[1] & Q$r.m==s[2] & Q$r.s==s[3] & Q$a==a )
    if( length(ix)!=1 )
      stop("ERRORE GRAVE!")
    
    if( is.infinite(reward) || is.infinite(Q$q[ix]) ) {
      Q$q[ix] <- -Inf
    } else {
      Q$q[ix] <- Q$q[ix] + lr * ( reward + df * Q.max - Q$q[ix] )
    }
  
    
    all.cash.trn[E,t] <- cash
    all.reward.trn[E,t] <- reward
    all.actions.trn[E,t] <- a
    
    
    if( t<length(price.trn) && t+dt > length(price.trn) ) {
      t <- length(price.trn)
    } else {
      t <- t+dt
    }
  }
  
  eps <- eps - eps.step
  lr <- lr - lr.step
  df <- df - df.step

  
  if( E %% visualize.every == 0 ) {
    cat(E,"episodes finished...\n")
    # graphics.off()
    par(mfrow=c(2,2))
    curr.mar <- par("mar")
    par(mar=c(4.6,4.1,2.1,1.1))
    
    # last 1000 episodes
    plot( all.cash.trn[max(1,E-1000):E,ncol(all.cash.trn)], type="l", lwd=2,
          ylab="final cash", xlab="more recent episodes")
    abline( h=initial.cash, col="red", lty=2, lwd=2 )
    
    # what's happened during the last episode
    plot( price.trn, type="l", lwd=2, ylab="price", xlab="time [days]" )
    iii <- which(all.actions.trn[E,]=="buy")
    z <- numeric()
    if( length(iii)>0 ) {
      for( jjj in 1:length(iii) ) 
        if( is.na(all.cash.trn[E,iii[jjj]-dt]) || all.cash.trn[E,iii[jjj]]<all.cash.trn[E,iii[jjj]-dt] )
          z <- c(z,iii[jjj])
      points( z, price.trn[z], pch=20, col="green3" )
      abline( v=z, col="green3", lty=2 )
    }
    iii <- which(all.actions.trn[E,]=="sell")
    z <- numeric()
    if( length(iii)>0 ) {
      for( jjj in 1:length(iii) ) 
        if( !is.na(all.cash.trn[E,iii[jjj]-dt]) && all.cash.trn[E,iii[jjj]]>all.cash.trn[E,iii[jjj]-dt] )
          z <- c(z,iii[jjj])
      points( z, price.trn[z], pch=20, col="red" )
      abline( v=z, col="red", lty=2 )
    }
    legend("bottomleft",legend=paste("final cash:",round(cash)))
    
    
    # distribution of final cash over the last 1000 episodes
    hist( all.cash.trn[max(1,E-1000):E,ncol(all.cash.trn)], col="skyblue", border="white", main="final cash distribution\nover last 1000 episodes", xlab="" )
    abline( v=initial.cash, col="red", lty=2, lwd=2 ) 

    # comparing density of all the final cash vs the final cash of the last 1000 episodes
    pdf.tot <- density(all.cash.trn[1:E,ncol(all.cash.trn)])
    pdf.last <- density(all.cash.trn[max(1,E-refining.episodes):E,ncol(all.cash.trn)])
    plot( pdf.tot, col="blue", lwd=3, ylim=c(0,max(pdf.last$y,pdf.tot$y)), main="final cash distribution (pdf)" )
    lines( pdf.last, col="green3", lwd=3 )
    abline( v=initial.cash, col="red", lty=2, lwd=2 ) 
    legend( "topleft", legend=c("all episodes","recent episodes"), col=c("blue","green3"), lwd=3 )
    
    par(mar=curr.mar)
    par(mfrow=c(1,1))
  }
  
  E <- E+1
}



## Start testing!
cat("\nTesting on the test period!")

all.cash.tst <- matrix(NA,1,length(price.tst))
all.reward.tst <- matrix(NA,1,length(price.tst))
all.actions.tst <- matrix("",1,length(price.tst))

cash <- initial.cash
shares <- initial.shares

t <- win

while( t <= length(price.tst) ) {
  
  # construct state
  ret <- diff( price.tst[(t-win+1):t] )
  # s <- round(c( cash, mean(ret), sd(ret) )/tick)
  s <- c( round((cash-initial.cash)/tick), round(mean(ret)/tick,1), round(sd(ret)/tick,1) ) 
  
  ix <- which( Q$cash==s[1] & Q$r.m==s[2] & Q$r.s==s[3] )
  # if( length(ix)==0 ) {
  #   cat("Unseen state!")
  #   # stop("....")
  #   # Q <- rbind( Q, data.frame( cash=rep(s[1],3),
  #   #                            r.m=rep(s[2],3),
  #   #                            r.s=rep(s[3],3),
  #   #                            a=c("buy","hold","sell"),
  #   #                            q=numeric(3),
  #   #                            stringsAsFactors=F ))
  #   # ix <- (nrow(Q)-2):nrow(Q)
  # }
  
  
  # select action
  if( t==length(price.tst) ) {
    a <- "sell"
  } else {
    
    # just for the testing period!
    if( length(ix)==0 ) {
      cat("Unseen state!\n")
      a <- "hold"
    } else {
      ii <- which( Q$q[ix]==max(Q$q[ix]))
      if( length(ii)>1 )
        ii <- sample(ii,1)
      a <- Q$a[ix[ii]]
    }
  }
  
  # perform action and observe the reward
  if( a=="buy" ) {
    new.shares <- floor(cash/price.tst[t])
    cost <- new.shares * (1+costs.perc) * price.tst[t]
    if( new.shares==0 || cost>cash ) {
      reward <- -Inf
    } else {
      shares <- shares+new.shares
      cash <- cash - cost
      reward <- 0
    }
  } else {
    if( a=="sell" ) {
      if( t==length(price.tst) ) {
        reward <- cash - initial.cash
      } else {
        if( shares==0 ) {
          reward <- -Inf
        } else {
          cost <- shares * costs.perc * price.tst[t]
          cash.prev <- cash
          cash <- cash + shares * price.tst[t] - cost
          shares <- 0
          if( !modified.reward ) {
            reward <- 0
          } else {
            reward <- cash - initial.cash
          }
        }
      }
    } else {
      reward <- 0
    }
  }

  # No Q table update!
  
  all.cash.tst[1,t] <- cash
  all.reward.tst[1,t] <- reward
  all.actions.tst[1,t] <- a
  
  
  if( t<length(price.tst) && t+dt > length(price.tst) ) {
    t <- length(price.tst)
  } else {
    t <- t+dt
  }
}

cat("Test finished...\n")
cat("> Initial investment (initial cash):",initial.cash,"\n")
cat("> Final cash:",initial.cash,"\n")

par(mfrow=c(2,2))
curr.mar <- par("mar")
par(mar=c(4.6,4.1,2.1,1.1))


plot( all.cash.tst[1,], type="l", lwd=2,
      ylab="cash over time", xlab="test time [days]")
abline( h=initial.cash, col="red", lty=2, lwd=2 )
  
# what's happened during the last episode
plot( price.tst, type="l", lwd=2, ylab="price", xlab="time [days]" )
iii <- which(all.actions.tst[1,]=="buy")
z <- numeric()
if( length(iii)>0 ) {
  for( jjj in 1:length(iii) ) 
    if( is.na(all.cash.tst[1,iii[jjj]-dt]) || all.cash.tst[1,iii[jjj]]<all.cash.tst[1,iii[jjj]-dt] )
      z <- c(z,iii[jjj])
  points( z, price.tst[z], pch=20, col="green3" )
  abline( v=z, col="green3", lty=2 )
}
iii <- which(all.actions.tst[1,]=="sell")
z <- numeric()
if( length(iii)>0 ) {
  for( jjj in 1:length(iii) ) 
    if( !is.na(all.cash.tst[1,iii[jjj]-dt]) && all.cash.tst[1,iii[jjj]]>all.cash.tst[1,iii[jjj]-dt] )
      z <- c(z,iii[jjj])
  points( z, price.tst[z], pch=20, col="red" )
  abline( v=z, col="red", lty=2 )
}
legend("bottomleft",legend=paste("final cash:",round(cash)))

  
# # distribution of final cash over the last 1000 episodes
# hist( all.cash.trn[max(1,E-1000):E,ncol(all.cash.trn)], col="skyblue", border="white", main="final cash distribution\nover last 1000 episodes" )
# abline( v=initial.cash, col="red", lty=2, lwd=2 ) 
# 
# # comparing density of all the final cash vs the final cash of the last 1000 episodes
# pdf.tot <- density(all.cash.trn[1:E,ncol(all.cash.trn)])
# pdf.last <- density(all.cash.trn[max(1,E-refining.episodes):E,ncol(all.cash.trn)])
# plot( pdf.tot, col="blue", lwd=3, ylim=c(0,max(pdf.last$y,pdf.tot$y)), main="final cash distribution (pdf)" )
# lines( pdf.last, col="green3", lwd=3 )
# abline( v=initial.cash, col="red", lty=2, lwd=2 ) 
# legend( "topleft", legend=c("all episodes","recent episodes"), col=c("blue","green3"), lwd=3 )
 
par(mar=curr.mar)
par(mfrow=c(1,1))


