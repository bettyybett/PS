#a
frepcomgen<- function(m,n){
  m1 <- matrix(0,2,n)
  m2 <- matrix(0,2,m)
  x <- 1
  for(i in 1:(n-1)){
    m1[1,i]<- sample(-2:3, 1)
    
    z <-  round(runif(1,0,x),digits=2) #Am creat variabila m1 , scadeam la fiecare pas valoarea lui x, a.i sa nu depaseasca 1
    m1[2,i] <- z
    x <- x-m1[2,i]
  }
  m1[2,n] <- x    #ii atribuim ultimei valori x ramasa pana la 1
  
  x <- 1
  for(i in 1:(m-1)){
    
    m2[1,i]<- sample(-2:3, 1)
    m2[2,i] <- runif(1,0,x)    #La fel pentru m2
    x <- x-m2[2,i]
  }
  m2[2,m] <- x
  matri <- list(m1,m2)
  
  
  matrice <- matrix(0,n,m)
  m1 <- m1[,order(m1[1,])]    #Ordonam matricile dupa prima linie
  m2 <- m2[,order(m2[1,])]
  
  auxm1 <- m1
  auxm2 <- m2
  
  #Creeam matricea comuna
  for(i in 1:n){
    no <- sample(1:m,1)    
    for(j in 1:m)
    {
      if(j!=no) #La fiecare pas , generam o pozitie aleatorie,o lasam libera
      {
        matrice[i,j] <- runif(1,0,min(auxm1[2,i],auxm2[2,j]))  # pentru a nu depasii pe linie sau pe coloana x lui pi , respectiv qi , iau minim dintre cele 2 pentru random
        
        auxm1[2,i] <- auxm1[2,i]-matrice[i,j]  
        auxm2[2,j] <- auxm2[2,j]-matrice[i,j]
      }
    }
    
  }
  
  m1m2 <- list(m1,m2,matrice)
  return(m1m2)
}
#b
fcomplrepcom <- function(x){
  while(1){
    k <- 0
    m=length(x[,1])
    n=length(x[1,])
    
    
    i <- 1
    j <- 1
    auxi <- i
    auxj <- j
    while(auxi<=n&&auxj<=m)   #Parcurgem pe diagonala pentru a vedea daca e vreun element lipsa
      
    {
      ok <- 0
      poz <- 0
      for(c in 1:m)
        if(x[auxi,c]==0){
          ok <- ok+1
          poz <- c
          
        }
      if(ok==1)
      {
        x[auxi,poz]=m1[2,auxi]-sum(x[auxi,]) #ia restul care a ramas pana la valoarea lui pi
        k <- 1
      }
      ok <- 0
      poz <- 0
      for(c in 1:n)
        if(x[c,auxj]==0){
          ok <- ok+1
          poz <- c
          
        } 
      if(ok==1)
      {
        x[poz,auxj]=m2[2,auxj]-sum(x[,auxj])
        k <- 1
      }
      auxi <- auxi +1
      auxj <- auxj+1
      
      
    }
    while(auxi<=n)  #daca sunt mai multe linii decat coloane, parcurgem doar pe linii
    {
      ok <- 0
      poz <- 0
      for(c in 1:m)
        if(x[auxi,c]==0){
          ok <- ok+1
          poz <- c
          
        }
      if(ok==1)
      {
        x[auxi,poz]=m1[2,auxi]-sum(x[auxi,])
        k <- 1
      }
      auxi <- auxi+1
    }
    
    
    while(auxj<=m) #daca avem mai multe coloane decat linii
    {
      ok <- 0
      poz <- 0
      for(c in 1:n)
        if(x[c,auxj]==0){
          ok <- ok+1
          poz <- c
          
        }
      if(ok==1)
      {
        x[poz,auxj]=m2[2,auxj]-sum(x[,auxj])
        k<- 1
      }
      auxj <- auxj+1
    }
    
    if(k==0)
      return(x) 
  }
}
x <- frepcomgen(3,5)

m1 <- x[[1]]

m2 <- x[[2]]

t <- x[[3]]  
print(t)
t <- fcomplrepcom(t)
print(t)
#c1
auxm1 <- 5*m1
auxm2 <- -3*m2
em1 <- 0
em2 <- 0
for(i in 1:length(auxm1[1,]))
  em1 <- em1+auxm1[1,i]*auxm1[2,i]  
for(i in 1:length(m2[1,]))
  em2 <- em2+auxm2[1,i]*auxm2[2,i] 
em1m2 <- 0
for(i in 1:length(auxm1[1,]))
  for(j in 1:length(auxm2[1,]))
    em1m2 <- em1m2+((auxm1[1,i]*auxm2[1,j])*(auxm1[2,i]*auxm2[2,j])) 
covm1m2 <- em1m2-em1*em2 
#c3
summ1 <- 0
summ2 <- 0
for(i in 1:length(m1[1,]))  
  if(m1[1,i]>6)
    summ1 <- summ1+m1[2,i]
for(i in 1:length(m2[1,]))
  if(m2[1,i]<7)       
    summ2 <- summ2+m2[2,i]
b <- summ1*summ2  
#d
fvernecor <- function(auxm1,auxm2){
  em1 <- 0
  em2 <- 0
  for(i in 1:length(auxm1[1,]))
    em1 <- em1+auxm1[1,i]*auxm1[2,i]
  for(i in 1:length(m2[1,]))
    em2 <- em2+auxm2[1,i]*auxm2[2,i]
  em1m2 <- 0
  for(i in 1:length(auxm1[1,]))
    for(j in 1:length(auxm2[1,]))
      em1m2 <- em1m2+((auxm1[1,i]*auxm2[1,j])*(auxm1[2,i]*auxm2[2,j]))
  covm1m2 <- em1m2-em1*em2
  if(covm1m2==0)
    print("necorelate")
  else
    print("corelate")
}
fverind <- function(m1,m2,x){
  ok <- 1
  for(i in 1:length(x[,1]))
    for(j in 1:length(x[1,]))   
     
      if(m1[2,i]*m2[2,j]!=x[i,j])
        ok <- 0
      if(ok==1)
        print("independente")
      else
        print("neindependente")
}
frepcomgen(5,4)