library(plyr)
library(dplyr)
library(rpart)
library(nnet)

train<-read.csv("../input/train_V2.csv")
test<-read.csv("../input/test_V2.csv")

solo.data<-filter(train,(grepl("solo",train$matchType)))
duo.data<-filter(train,(grepl("duo",train$matchType)))
squad.data<-filter(train,(grepl("squad",train$matchType)))
other.data<-filter(train,(grepl("solo",train$matchType)))

solo.model<-nnet(winPlacePerc~killPlace+rideDistance+walkDistance+damageDealt+kills+weaponsAcquired+maxPlace+boosts+longestKill,data=solo.data, size=10)
duo.model<-rpart(winPlacePerc~killPlace+killStreaks+walkDistance+rideDistance+weaponsAcquired,data=duo.data)
squad.model<-rpart(winPlacePerc~killPlace+killStreaks+walkDistance+rideDistance+weaponsAcquired,data=squad.data)
other.model<-rpart(winPlacePerc~killPlace+rideDistance+walkDistance+damageDealt+kills+weaponsAcquired+maxPlace+boosts+longestKill,data=other.data)

#change some data structures for easier calculations
test$matchId<-as.character(test$matchId)
test$Id<-as.character(test$Id)
test$matchType<-as.character(test$matchType)

predictions<-NULL
M.Id<-NULL
matches<-unique(test$matchId)

for(i in 1:length(matches)){
    Ids=NULL
    prediction=NULL
    Ids=filter(test,matchId==matches[i])$Id
    gametype=filter(test,matchId==matches[i])$matchType
    if (any(grepl("solo",gametype))){
        prediction=predict(solo.model,filter(test,matchId==matches[i]))
    } else if (any(grepl("duo",gametype))){
        prediction=predict(duo.model,filter(test,matchId==matches[i]))
    } else if (any(grepl("squad",gametype))){
        prediction=predict(squad.model,filter(test,matchId==matches[i]))
    } else {
        prediction=predict(other.model,filter(test,matchId==matches[i])) 
    }
    M.Id=c(M.Id,Ids)
    predictions=c(predictions,prediction)
}

submission<-data.frame(M.Id,predictions)
colnames(submission)<-c("Id","winPlacePerc")
write.csv(submission,"submission.csv",row.names=FALSE)

