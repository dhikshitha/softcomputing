library(frbs)
library(caret)
varinp.mf <- matrix(c(1, 0, 0, 30, NA, 1, 30, 45, 60, NA, 1, 60, 100, 100,NA,
                      1, 0, 0, 30, NA, 1, 30, 45, 60, NA, 1, 60, 100, 100,NA),                  
                    nrow = 5, byrow = FALSE)

num.fvalinput <- matrix(c(3, 3), nrow=1)
varinput.1 <- c("small", "medium_dirtiness", "large")
varinput.2 <- c("not_greasy", "medium_type","greasy")
names.varinput <- c(varinput.1, varinput.2)
range.data <- matrix(c(0,100, 0, 100,0,100), nrow=2)
type.defuz <- "COG"
type.tnorm <- "MIN"
type.snorm <- "MAX"
type.implication.func <- "ZADEH"
name <- "Sim-0"
#newdata<- matrix(c(30, 75,  45, 80, 30, 45), nrow= 3, byrow = TRUE)
newdata<-read.csv("dirt.csv")
colnames.var <- c("dirtiness", "type of dirt", "washing time")
num.fvaloutput <- matrix(c(5), nrow=1)
varoutput.1 <- c("very_short", "short", "medium","long","very_long")
names.varoutput <- c(varoutput.1)
varout.mf <- matrix(c(1, 0, 10, 20,NA ,
                      1, 20, 30, 40, NA,
                      1, 40, 50, 60, NA,
                      1,60,70,80,NA,
                      1,80,90,100,NA),
                    nrow = 5, byrow = FALSE)
type.model <- "MAMDANI"
rule <- matrix(c("large","and","greasy","->","very_long",
                 "medium_dirtiness","and","greasy","->","long",
                 "small","and","greasy","->","long",
                 "large","and","medium_type","->","long",
                 "medium_dirtiness","and","medium_type","->","medium",
                 "small","and","medium_type","->","medium",
                 "large","and","not_greasy","->","medium",
                 "medium_dirtiness","and","not_greasy","->","short",
                 "small","and","not_greasy","->","very_short"), 
               nrow=9, byrow=TRUE) 
object <- frbs.gen(range.data, num.fvalinput, names.varinput, num.fvaloutput, varout.mf, 
                   names.varoutput, rule, varinp.mf, type.model, type.defuz, type.tnorm, 
                   type.snorm, func.tsk = NULL, colnames.var, type.implication.func, name)
par(mar=c(1,1,1,1))
summary(object)
plotMF(object)
res <- predict(object, newdata[,-ncol(newdata)])$predicted.val
print(res)
label<-vector()
for(i in 1:length(res)){
  for(j in 1:ncol(varout.mf)){
    lowerbound=varout.mf[2,j]
    upperbound=varout.mf[4,j]
    if(res[i]<=upperbound && res[i]>lowerbound ){
      label=append(label,varoutput.1[j])
    }
  }
}
output<-factor(label)
levels(newdata[,ncol(newdata)])<-varoutput.1
levels(output)<-varoutput.1
cm=confusionMatrix(newdata[,ncol(newdata)],output)
print(cm)

plot(newdata[,1],newdata[,2],xlab="dirtiness",ylab="Type of dirt",col=1:5,main="Dirtiness vs DirtType",pch=2)
legend("topleft",
       legend = varoutput.1, col=c("black","red","green","pink","yellow"),pch=2)




# sugeno


library(frbs)
varinp.mf <- matrix(c(1, 1000, 1200, 1400, NA, 1, 1200, 1600, 1800, NA,
                      1, 2.5, 5, 7.5, NA, 1, 6, 10, 14, NA),                  
                    nrow = 5, byrow = FALSE)

num.fvalinput <- matrix(c(2, 2), nrow=1)
varinput.1 <- c("smallW", "largeW")
varinput.2 <- c("smallT", "largeT")
names.varinput <- c(varinput.1, varinput.2)
range.data <- matrix(c(1000,2000, 0, 15,5000,15000), nrow=2)
type.defuz <- "WAM"
type.tnorm <- "MIN"
type.snorm <- "MAX"
type.implication.func <- "ZADEH"
name <- "Sim-0"
newdata<- matrix(c(1300, 6.5), nrow= 1, byrow = TRUE)
colnames.var <- c("W", "T","AU")
type.model <- "TSK"
func.tsk <- matrix(c(4.6925, -526.2, 2631, 3.4765, -210.5, 2103, 4.6925, -526.2, 2631),
                   nrow = 3, byrow = TRUE)
rule <- matrix(c("largeW", "and", "smallT", "->",
                 "smallW", "or", "largeT", "->",
                 "smallW", "and", "smallT", "->"),
               nrow = 3, byrow = TRUE)
object <- frbs.gen(range.data, num.fvalinput, names.varinput,
                   num.fvaloutput = NULL, varout.mf = NULL, names.varoutput = NULL, rule,
                   varinp.mf, type.model, type.defuz = NULL, type.tnorm, type.snorm,
                   func.tsk, colnames.var, type.implication.func, name)
plotMF(object)
res <- predict(object, newdata)$predicted.val
print(res)