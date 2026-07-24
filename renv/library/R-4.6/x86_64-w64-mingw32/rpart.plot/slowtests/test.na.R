# test.na.R: test that NA predicted responses are shown with a hatched box
#
# TODO To get NA fitted values we force them into the model's frame

source("test.prolog.R")
data(ptitanic)
options(warn=2) # treat warnings as errors (catch NA warnings, if any)

#--- continuous response ---

par(mfrow=c(2,2))
options(warn=1) # print warnings as they occur (rpart gives a warning: partial argument match of 'length' to 'length.out')
age <- rpart(age~., data=ptitanic, cp=.02)
options(warn=2) # treat warnings as errors
age$frame$yval[2] <- NA # node 2 (internal)
age$frame$yval[6] <- NA # node 5 (leaf)
age$frame$yval[8] <- NA # node 6 (leaf)
rpart.plot(age, type=4, nn=1, clip.right.labs=FALSE, branch=.3,
           digits=-2, roundint=TRUE,
           main="age with na", trace=1) # trace=1 to get message: fitted[6] is NA
print(rpart.rules(age))
print(rpart.predict(age, rules=TRUE)[53:56,])

# --- binary response ---

options(warn=1) # print warnings as they occur (rpart gives a warning: partial argument match of 'length' to 'length.out')
survived <- rpart(survived~., data=ptitanic, control=list(cp=.02))
options(warn=2) # treat warnings as errors
survived$frame$yval[3] <- survived$frame$yval2[3,1] <- survived$frame$yval2[3,5] <- NA # node 4 (leaf)
survived$frame$yval[4] <- survived$frame$yval2[4,1] <- survived$frame$yval2[4,5] <- NA # node 5 (internal)

expect.err(try(rpart.plot(survived, nn=1, type=1, fallen.leaves=FALSE,
               main="survived with na")),
 "Diverging palettes like box.palette=\"BuGn\" cannot be used for this model")

rpart.plot(survived, nn=1, type=1, fallen.leaves=FALSE, box.palette="Blues",
           main="survived with na")
print(rpart.rules(survived, cover=TRUE))
print(rpart.predict(survived, rules=TRUE)[1:5,])

#--- multiclass response ---

options(warn=1) # print warnings as they occur (rpart gives a warning: partial argument match of 'length' to 'length.out')
pclass <- rpart(pclass ~ ., data=ptitanic, control=rpart.control(cp=.01))
options(warn=2) # treat warnings as errors
pclass$frame$yval[3] <- pclass$frame$yval2[3,1] <- NA # node 4
pclass$frame$yval2[3, 6] <- NA # change class probs [.74 .16 .10] to  [.74 NA .10]
rpart.plot(pclass, nn=1, main="pclass with na")
print(rpart.rules(pclass))
print(rpart.predict(pclass, rules=TRUE, clip.facs=TRUE)[5:8,])

par(old.par)

source("test.epilog.R")
