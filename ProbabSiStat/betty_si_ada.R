#media
mean(train$Fare)
mean(train$Survived)
#boxplot
boxplot(train$Age ~ train$Fare, xlab = "Fare", ylab = "Age", col = c("magenta"))
#variance
var(train$Fare)
#quartile
quantile(train$Pclass)

scatter.smooth(x=train$Age,y=train$Survived,main="Age ~ Survived")
cor(train$Survived,train$Parch)
#vrem sa aflam  cati oameni au supravietuit in raport cu varsta
#regresie simpla
lm(Survived~Parch,data=train)

#regresie multipla
x<-lm(Survived~Fare+Parch+Pclass,data=train )
summary(x)



#predictors
set.seed(100)
fareRow<-sample(1:nrow(train),0.8*nrow(train))
rowData<-train[fareRow, ]
testdata<-train[-fareRow, ]
linearmod<-lm(Survived~Age,data=train)
agePred<-predict(linearmod,testdata)
summary(agePred)

train$Height<-c(runif(891,1,2))
train$Weight<-c(runif(891,30,120))
rnorm(891,train$Weight,train$Height)
lm(Height~Weight,data=train)

#chisq este o verificare a independentei dintre doua variabile din train 
chisq.test(train$Survived, train$Pclass)

#functia de densitate a variabilei dupa chisq test 
plot(density(agePred))

#functia de repartitie 
#repartition(train$Pclass,train$Age)


