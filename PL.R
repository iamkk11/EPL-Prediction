set.seed(1)
v='PL2'
p=paste('/home/kevin/Downloads/',v,sep='')
#Combine files from trace folders into 1 data frame
library('dplyr',warn.conflicts=FALSE)
#Defining function to merge traces
combine = function(mypath){
filenames=list.files(path=mypath, full.names=TRUE)
datalist = lapply(filenames, function(x){read.csv(file=x,sep=',')})
Reduce(function(x,y) {bind_rows(x,y)}, datalist)}
#Reading each trace file and combining them
a=combine(p)
attach(a)

#teams
library('sqldf',warn.conflicts=FALSE)
t=sqldf("SELECT Date,B365H,B365D,B365A,LBH,LBD,LBA,FTR FROM a WHERE
                        HomeTeam='Chelsea' AND AwayTeam='Man City'ORDER BY Date")

s=sqldf("SELECT FTR AS 'Result',COUNT(FTR) AS 'Counts' FROM t GROUP BY FTR")
p=sqldf("SELECT Result,Counts,ROUND(Counts*100.0/(SELECT SUM(Counts) FROM s),0) AS 'Percentage' 
                  FROM s GROUP BY Result")

#b=t[-10,]
#b=t

#Factorization
b$FTR=factor(b$FTR)

#Training and test
library('varhandle')
gtrain=b
gtest<-data.frame(gtrain[,c(1:6)])
gtestlabel<-c(unfactor(gtrain$FTR))
attach(gtrain)

#naive bayes
library('e1071',warn.conflicts=FALSE)
naive_bayes_model<-naiveBayes(FTR ~ ., data = gtrain)
naive_bayes_predictions<-predict(naive_bayes_model, newdata=gtest)
naive_bayes_accuracy=round(mean(naive_bayes_predictions==gtestlabel),2)*100

#classification tree
library('party',warn.conflicts=FALSE)
ctree_model<- ctree(FTR ~ ., data = gtrain,controls=ctree_control(minsplit=30,minbucket=10,maxdepth=5))
ctree_predictions <- predict(ctree_model,newdata=gtest,type='response')
ctree_accuracy=round(mean(ctree_predictions==gtestlabel),2)*100

odds=data.frame(B365H='0.727',B365D='2.75',B365A='4',LBH='0.727',LBD='2.6',LBA='3.75')
predict(naive_bayes_model,newdata=odds,type = 'class',interval=predict)



