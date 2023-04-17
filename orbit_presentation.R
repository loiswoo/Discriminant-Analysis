data = read.csv("R_orbit.csv")

dim(data)
data = data[, -1 ] #id 빼주기
names(data) = c("semi-major", "eccen", "exlip-pl", "asc nod","node","m_anomoly", "perih", "aph", "orb_pd", "vmag", "moid", "class") #colnames 바꿔주기 
head(data)


summary(data)

#분산 확인
##var-cov matrix
g1 <- subset(data, class =="APO*")
g2 <- subset(data, class =="ATE*")
g1
g2
cov(g1[,-12])
cov(g2[,-12])
cov(g1[,-12])-cov(g2[,-12])



#정규분포 확인
install.packages("ggplot2")
library(ggplot2)
##1.Semi-major
p_1 <- ggplot(data, aes(x=data$class, y=data$`semi-major`, color=data$class)) + 
  geom_boxplot() +theme(axis.title=element_text(size=15)) +
  theme(axis.text.x=element_text(size=13),axis.text.y=element_text(size=13))
p_1 +coord_flip() + geom_jitter(shape=16, position=position_jitter(0.2))

##2.Eccen
p_2 <- ggplot(data, aes(x=data$class, y=data$eccen, color=data$class)) + 
  geom_boxplot() +theme(axis.title=element_text(size=15)) +
  theme(axis.text.x=element_text(size=13),axis.text.y=element_text(size=13))
p_2 +coord_flip() + geom_jitter(shape=16, position=position_jitter(0.2))

##3.Exlip-pl
p_3 <- ggplot(data, aes(x=data$class, y=data$`exlip-pl`, color=data$class)) + 
  geom_boxplot() +theme(axis.title=element_text(size=15)) +
  theme(axis.text.x=element_text(size=13),axis.text.y=element_text(size=13))
p_3 +coord_flip() + geom_jitter(shape=16, position=position_jitter(0.2))

##4. Asc nod
p_4 <- ggplot(data, aes(x=data$class, y=data$`asc nod`, color=data$class)) + 
  geom_boxplot() +theme(axis.title=element_text(size=15)) +
  theme(axis.text.x=element_text(size=13),axis.text.y=element_text(size=13))
p_4 +coord_flip() + geom_jitter(shape=16, position=position_jitter(0.2))


##5. Node
p_5 <- ggplot(data, aes(x=data$class, y=data$node, color=data$class)) + 
  geom_boxplot() +theme(axis.title=element_text(size=15)) +
  theme(axis.text.x=element_text(size=13),axis.text.y=element_text(size=13))
p_5 +coord_flip() + geom_jitter(shape=16, position=position_jitter(0.2))

##6. M-anomoly
p_6 <- ggplot(data, aes(x=data$class, y=data$m_anomoly, color=data$class)) + 
  geom_boxplot() +theme(axis.title=element_text(size=15)) +
  theme(axis.text.x=element_text(size=13),axis.text.y=element_text(size=13))
p_6 +coord_flip() + geom_jitter(shape=16, position=position_jitter(0.2))

##7. perih
p_7 <- ggplot(data, aes(x=data$class, y=data$perih, color=data$class)) + 
  geom_boxplot() +theme(axis.title=element_text(size=15)) +
  theme(axis.text.x=element_text(size=13),axis.text.y=element_text(size=13))
p_7 +coord_flip() + geom_jitter(shape=16, position=position_jitter(0.2))

##8. Aph
p_8 <- ggplot(data, aes(x=data$class, y=data$aph, color=data$class)) + 
  geom_boxplot() +theme(axis.title=element_text(size=15)) +
  theme(axis.text.x=element_text(size=13),axis.text.y=element_text(size=13))
p_8 +coord_flip() + geom_jitter(shape=16, position=position_jitter(0.2))

#9.orb_pd
p_9 <- ggplot(data, aes(x=data$class, y=data$orb_pd, color=data$class)) + 
  geom_boxplot() +theme(axis.title=element_text(size=15)) +
  theme(axis.text.x=element_text(size=13),axis.text.y=element_text(size=13))
p_9 +coord_flip() + geom_jitter(shape=16, position=position_jitter(0.2))

#10. vmag
p_10 <- ggplot(data, aes(x=data$class, y=data$vmag, color=data$class)) + 
  geom_boxplot() +theme(axis.title=element_text(size=15)) +
  theme(axis.text.x=element_text(size=13),axis.text.y=element_text(size=13))
p_10 +coord_flip() + geom_jitter(shape=16, position=position_jitter(0.2))

#11. moid
p_11 <- ggplot(data, aes(x=data$class, y=data$moid, color=data$class)) + 
  geom_boxplot() +theme(axis.title=element_text(size=15)) +
  theme(axis.text.x=element_text(size=13),axis.text.y=element_text(size=13))
p_11 +coord_flip() + geom_jitter(shape=16, position=position_jitter(0.2))


#12. 
p_12 <- ggplot(data, aes(x=data$class, y=data$class, color=data$class)) + 
  geom_boxplot() +theme(axis.title=element_text(size=15)) +
  theme(axis.text.x=element_text(size=13),axis.text.y=element_text(size=13))
p_12 +coord_flip() + geom_jitter(shape=16, position=position_jitter(0.2))
##scaling이 필요없어 보인다 
#min-max scaling
nor_minmax = function(x){
  result = (x-min(x)) / (max(x) - min(x))
}
nor_minmax(data)

#standardization
nor_sd = function(x){
  result = (x-mean(x)) / sd(x)
}

nor_sd(data)


#train, test set 구분
#which = sample(c(TRUE, FALSE), nrow(data), replace = TRUE, prob = c(0.8, 0.2))

which=sample(1:nrow(data), nrow(data)*0.8, replace=FALSE)

train = data[which, ]
test = data[-which, ]

nrow(train)
nrow(test)


######group 정보 할당(필요없음)
class0 
data$class = ifelse(data$class == 'APO*', 1, 2)
group = train$class
##############

library(MASS)

#LDA 시행(CV = F)
train_lda0 = lda(train[, -12], train$class, CV=F) 
class1 = predict(train_lda0)$class
tab1 = table(train$class, class1)
tab1
1 - sum(tab1[row(tab1)==col(tab1)]*(1/(2*apply(tab1,1,sum))))

#train 그래프 그리기 
plot(predict(train_lda0)$x, type="n", xlab="LD1", ylab="LD2", main="TrainSet LDA Results")
text(predict(train_lda0)$x, as.character(predict(train_lda0)$class), col=as.numeric(as.factor(train$class)), cex=0.7)
abline(h=0, col="gray")
abline(v=0, col="gray")


#LDA 시행(CV = T)
train_lda1 = lda(train[, -12], train$class, CV=T)
class2 = train_lda1$class
tab2 = table(train$class, class2)
tab2
1 - sum(tab2[row(tab2)==col(tab2)]*(1/(2*apply(tab2,1,sum))))



plot(train_lda1$x, type="n", xlab="LD1", ylab="LD2", main="TrainSet LDA Results(with CV)")
text(train_lda1$x, as.character(train_lda1$class), col=as.numeric(as.factor(train$class)), cex=0.7)
abline(h=0, col="gray")
abline(v=0, col="gray")


#LDA test set(using CV = F)
test_lda = predict(object = train_lda0, newdata = test[, -12])
test_lda$class
tab3 = table(test$class, test_lda$class)
tab3
1 - sum(tab3[row(tab3)==col(tab3)]*(1/(2*apply(tab3,1,sum))))


#test 그래프 그리기 
plot(test_lda$x, type="n", xlab="LD1", ylab="LD2", main="TestSetLDA Results")
text(test_lda$x, as.character(test_lda$class), col=as.numeric(as.factor(test$class)), cex=1)
abline(h=0, col="gray")
abline(v=0, col="gray")

#misclassification 그래프 그리기
mis=which(test_lda$class!=test$class)
mis
par(mfrow=c(1,1))
plot(test_lda$x[mis], type="n", xlab="LD1", ylab="LD2", main="Misclassification Results")
text(test_lda$x[mis], as.character(test_lda$class[mis]), col=as.numeric(as.factor(test$class[mis])), cex=2)




#QDA (CV = F)
train_qda0 = qda(train[,-12], train$class, CV=F) 
class4 = predict(train_qda0)$class
tab4 = table(train$class, class4)
tab4
1 - sum(tab4[row(tab4)==col(tab4)]*(1/(2*apply(tab4,1,sum))))


#QDA (CV = T)
train_qda1 = qda(train[,-12], train$class, CV=T) 
class5 = train_qda1$class
tab5 = table(train$class, class5)
tab5
1 - sum(tab5[row(tab5)==col(tab5)]*(1/(2*apply(tab5,1,sum))))


#QDA test set(using CV = F)
test_qda = predict(object = train_qda0, newdata = test[, -12])
test_qda$class
tab6 = table(test$class, test_qda$class)
tab6
1 - sum(tab6[row(tab6)==col(tab6)]*(1/(2*apply(tab6,1,sum))))


#test 그래프 그리기 
plot(test_qda$x, type="n", xlab="LD1", ylab="LD2", main="TestSet QDA Results")
text(test_qda$x, as.character(test_qda$class), col=as.numeric(as.factor(test$class)), cex=0.7)
abline(h=0, col="gray")
abline(v=0, col="gray")






#orbit.lda = lda(formula = class ~., data = train, CV = True)
#orbit.lda
#orbit.lda$counts
#orbit.lda$svd #특이값, 선형판별변수들의 그룹 간 표준편차와 그룹 내 표준편차의 비 리턴
#class = orbit.lda$class

#predict
#predict = predict(orbit.lda)
pclass = predict(orbit.lda, train)$class
predict(orbit.lda, train)$posterior




#calculate misclassification
tt = table(train$class, orbit.lda$class..?)







