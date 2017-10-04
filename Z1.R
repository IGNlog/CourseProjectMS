#Задание 1.1
library(readr)
library(moments)
library(car)
r1z1 <- read_csv("D:/R/CP/CourseProject/r1z1.csv")
View(r1z1)

#Объём выборки
n<-nrow(r1z1)
n
#максимум
xvec<-r1z1$X
max(xvec)
#минимум
min(xvec)
#Размах
max(xvec)-min(xvec)
#Среднее
meanx<-sum(xvec)/n
meanx
#Смещённая выборочная дисперсия
biasedvar<-sum(xvec^2)/n-meanx^2
biasedvar
#Несмещённая дисперсия
unbiasedvar<-biasedvar*(n/(n-1))
unbiasedvar
var(xvec)
#Стандартное отклонение (используется смещённая дисперсия)
standardDev<-sqrt(biasedvar)
standardDev
sd(xvec)
#Коэфицент асимметрии
asymmetry<-(sum(xvec^3)/n-(3*meanx*sum(xvec^2))/n+2*meanx^3)/(standardDev^3)
asymmetry
skewness(xvec)
#Коэфицент эксцесса
peakedness<-((sum(xvec^4)/n-(4*meanx*sum(xvec^3))/n+(6*(meanx^2)*sum(xvec^2))/n)-3*(meanx^4))*(1/standardDev^4)-3
peakedness
kurtosis(xvec)-3
#Медиана
med<-median(xvec)
med

#Задание 1.2
#Построение гистограммы
k<-round(n/10,0)
delta<-(max(xvec)-min(xvec))/(k-1)
y0<-min(xvec)-delta/2
y<-y0
for(i in 1:k) {y<-append(y,(y0+i*delta))} 
y
hist(xvec,breaks=y ,
     col='lightblue',main="Гистосрамма", xlab="Выборочные данные", ylab="Плотность", freq = F)

#hist(xvec, breaks=y,freq=F) 


#Задание 1.3
#Построение ЭФР
plot(sort(xvec), (1:n)/n, type="S", col="green", main="ЭФР", xlab="Выборочные данные", ylab="",lwd=2)
x = seq(min(xvec),max(xvec), by=0.5)
#Нормальное распределение
lines(x, pnorm(x, meanx, standardDev), type="l",col="lightblue", lwd=1)

#Задание 1.4
#построение QQ-plot
#qqnorm(xvec)
qqPlot(xvec, main="QQ-plot", xlab="Квантиль нормального распределения", ylab="Выборочные данные", labels=rownames(Prestige))
