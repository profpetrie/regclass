generalization.error <-
function(MODEL,HOLDOUT,Kfold=FALSE,K=5,R=10,seed=NA) {
    
    if(!is.na(seed)) { set.seed(seed) }
    y.name <- unlist(strsplit(as.character(MODEL$call),split=" ")[[2]])[1]
    y.pos <- which(names(HOLDOUT)==y.name)
    
    
    #Confusion matrix function
    CM <- function(M,DATA=NA) {
        if(missing(DATA)) { DATA <- M$data }
        D <- names(DATA)
        y.label <- as.character(M$terms[[2]])
        y.pos <- which(D==y.label)
        y.levels <- names(table(DATA[,y.pos]))
        y.pred <- ifelse( predict(M,newdata=DATA) > 0, y.levels[2],y.levels[1])
        T<-table(DATA[,y.pos],y.pred,dnn=c())
        if(  dim(T)[2]==1  )  {
            colnames(T) <- paste("Predicted",names(which.max(table(DATA[,y.pos]))))
            rownames(T) <- c(paste("Actual",y.levels))
            cat("Predicted classes same as naive model (majority class)\n");
            return(T) }
        T<-rbind(T,apply(T,2,sum))
        T<-cbind(T,apply(T,1,sum))
        colnames(T) <- c(paste("Predicted",y.levels),"Total")
        rownames(T) <- c(paste("Actual",y.levels),"Total")
        T }

    #Random forest
    if( sum(class(MODEL) %in% c("randomForest"))==1 )  {
        if(MODEL$type=="regression") {
            predicteds <- predict(MODEL,newdata=HOLDOUT)
            actuals <- HOLDOUT[,y.pos]
            RMSE.holdout <- sqrt(mean((actuals-predicteds)^2))
            return(list(RMSE.holdout=RMSE.holdout)) }
        if(MODEL$type=="classification") {
            L <- levels(HOLDOUT[,y.pos])
            predicteds <- predict(MODEL,newdata=HOLDOUT)
            actuals <- HOLDOUT[,y.pos]
            T<-table(actuals,predicteds,dnn=c())
            if(  dim(T)[2]==1  )  {
                colnames(T) <- paste("Predicted",names(which.max(table(DATA[,y.pos]))))
                rownames(T) <- c(paste("Actual",y.levels))
                cat("Predicted classes same as naive model (majority class)\n");
                return(T) }
            T<-rbind(T,apply(T,2,sum))
            T<-cbind(T,apply(T,1,sum))
            colnames(T) <- c(paste("Predicted",L),"Total")
            rownames(T) <- c(paste("Actual",L),"Total")
            if(dim(T)[1] == 3) { misclass.holdout <- (T[2,1]+T[1,2])/T[3,3] }
            if(dim(T)[1] == 2) { misclass.holdout <- min(T)/sum(T) }
            return(list(CM.holdout=T,misclass.holdout=misclass.holdout)) }
    }
    
    #Partition
    if( sum( class(MODEL)%in%c("rpart") )==1 ) {
        if(MODEL$method=="anova") {
            RMSE.train <- sqrt(mean(residuals(MODEL)^2))
            predicteds <- predict(MODEL,newdata=HOLDOUT)
            actuals <- HOLDOUT[,y.pos]
            RMSE.holdout <- sqrt(mean((actuals-predicteds)^2))
            return(list(RMSE.train=RMSE.train,RMSE.holdout=RMSE.holdout))
        }
        if(MODEL$method=="class") {
            y.name <- unlist(strsplit(as.character(MODEL$call),split=" ")[[2]])[1]
            P <- predict(MODEL)
            L <- colnames(P)
            actuals <- ifelse(MODEL$y==1,L[1],L[2])
            predicteds <- as.character( ifelse(P[,1]>.5,L[1],L[2]) )
            T<-table(actuals,predicteds,dnn=c())
            if(  dim(T)[2]==1  )  {
                colnames(T) <- paste("Predicted",names(which.max(table(DATA[,y.pos]))))
                rownames(T) <- c(paste("Actual",y.levels))
                cat("Predicted classes same as naive model (majority class)\n") }
            T<-rbind(T,apply(T,2,sum))
            T<-cbind(T,apply(T,1,sum))
            colnames(T) <- c(paste("Predicted",L),"Total")
            rownames(T) <- c(paste("Actual",L),"Total")
            CM.train <- T
            if(dim(CM.train)[1] == 3) { misclass.train <- (CM.train[2,1]+CM.train[1,2])/CM.train[3,3] }
            if(dim(CM.train)[1] == 2) { misclass.train <- min(CM.train)/sum(CM.train) }
            
            predicteds <- as.character(ifelse(predict(MODEL,newdata=HOLDOUT)[,1]>.5,L[1],L[2]))
            actuals <- HOLDOUT[,y.pos]
            T<-table(actuals,predicteds,dnn=c())
            if(  dim(T)[2]==1  )  {
                colnames(T) <- paste("Predicted",names(which.max(table(DATA[,y.pos]))))
                rownames(T) <- c(paste("Actual",y.levels))
                cat("Predicted classes same as naive model (majority class)\n");
                return(T) }
            T<-rbind(T,apply(T,2,sum))
            T<-cbind(T,apply(T,1,sum))
            colnames(T) <- c(paste("Predicted",L),"Total")
            rownames(T) <- c(paste("Actual",L),"Total")
            CM.holdout <- T
            if(dim(CM.holdout)[1] == 3) { misclass.holdout <- (CM.holdout[2,1]+CM.holdout[1,2])/CM.holdout[3,3] }
            if(dim(CM.holdout)[1] == 2) { misclass.holdout <- min(CM.holdout)/sum(CM.holdout) }
            return(list(CM.train=CM.train,misclass.train=misclass.train,CM.holdout=CM.holdout,misclass.holdout=misclass.holdout))
        }
    }
    
    #Regression
    if( sum( class(MODEL)[1]%in%c("lm","glm")) ==1 ) {
        M <- MODEL
        DATA <- M$model
        n <- nrow(DATA)
        
        if(class(M)[1]=="glm") {
            y.levels <- names(table(DATA[,1]))
            CM.train <- CM(M)
            if(dim(CM.train)[1] == 3) { misclass.train <- (CM.train[2,1]+CM.train[1,2])/CM.train[3,3] }
            if(dim(CM.train)[1] == 2) { misclass.train <- min(CM.train)/sum(CM.train) }
            est.validerror <- rep(0,R)
            if(Kfold==TRUE) {
            for (r in 1:R) {
                indices <- sample(n)
                break.points <- round( seq(1,n,length=K+1) )
                errors <- rep(0,K)
                for (k in 1:K) {
                    chunk <- indices[break.points[k]:(break.points[k+1]-1)]
                    M.train <- glm(formula(M),data=DATA[-chunk,],family=binomial)
                    predicteds <- ifelse( predict(M.train,newdata=DATA[chunk,]) > 0, y.levels[2], y.levels[1] )
                    actuals <- DATA[chunk,1]
                    errors[k] <- sum(predicteds!=actuals)/length(actuals)
                }
                est.validerror[r] <- mean(errors)
            }}
            misclass.valid <- mean(est.validerror)
            misclasssd.valid <- sd(est.validerror)
            CM.holdout <- CM(M,HOLDOUT)
            if(dim(CM.holdout)[1] == 3) { misclass.holdout <- (CM.holdout[2,1]+CM.holdout[1,2])/CM.holdout[3,3] }
            if(dim(CM.holdout)[1] == 2) { misclass.holdout <- min(CM.holdout)/sum(CM.holdout) }
            if(Kfold==TRUE) { 
            return( list(Confusion.Matrices=list(Training=CM.train,Holdout=CM.holdout),Misclassification.Rates=list(Training=misclass.train,Validation=misclass.valid,Holdout=misclass.holdout)) )
            }
            return( list(Confusion.Matrices=list(Training=CM.train,Holdout=CM.holdout),Misclassification.Rates=list(Training=misclass.train,Holdout=misclass.holdout)) )
        }
        
        if(class(M)[1]=="lm") {
            predicteds <- fitted(M)
            actuals <- DATA[,1]
            RMSE.train <- sqrt(mean(residuals(M)^2))
            est.validerror <- rep(0,R)
            if(Kfold==TRUE) {
            for (r in 1:R) {
                indices <- sample(n)
                break.points <- round( seq(1,n,length=K+1) )
                errors <- rep(0,K)
                for (k in 1:K) {
                    chunk <- indices[break.points[k]:(break.points[k+1]-1)]
                    M.train <- lm(formula(M),data=DATA[-chunk,])
                    predicteds <- predict(M.train,newdata=DATA[chunk,])
                    actuals <- DATA[chunk,1]
                    errors[k] <- sqrt(mean(residuals(M.train)^2))
                }
                est.validerror[r] <- mean(errors)
            } }
            RMSE.valid <- mean(est.validerror)
            RMSEse.valid <- sd(est.validerror)
            y.column <- which(names(HOLDOUT)==names(DATA)[1])
            actuals <- HOLDOUT[,y.column]
            predicteds <- predict(M,newdata=HOLDOUT)
            #RMSE.holdout <- sqrt( sum((predicteds-actuals)^2)/(length(actuals) - summary(M)$df[1]) )
            RMSE.holdout <- sqrt( sum((predicteds-actuals)^2)/(length(actuals)) )
            if(Kfold==TRUE) { return(list(RMSE.train=RMSE.train,RMSE.valid=RMSE.valid,RMSE.holdout=RMSE.holdout)) } 
            return(list(RMSE.train=RMSE.train,RMSE.holdout=RMSE.holdout))
        }
        
        
        
    }
    
}
