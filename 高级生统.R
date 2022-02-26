x=matrix(c(7775,42,2099,49),nrow = 2,byrow=T)
chisq.test(x)

data<-read.csv("C:/Users/HP/Desktop/energy(1).csv")
#或 
library(data.table)
d=fread("C:/Users/HP/Desktop/energy(1).csv")
plot(data)
cor(data)
library(corrplot)
corrplot(cor(d)) #相关性，可以做线性回归
plot(d$energy,d$GDP)

###2021.12.17###
x=c(8.24,6.43,6.2,7.3,9.42,8.37,8.98,8.79)
y=c(77.03,71.44,72.65,72.45,86.2,76.69,84.1,85.47)
plot(y~x)
#或plot(x,y)
#输入par(margin(c(10,3,1,1)+0.1))可解决“Error in plot.new() : figure margins too large”的问题
#也可以用windows()弹出新的窗口绘图
cor(x,y)
cor.test(x,y)
lm(y~x) #结果中Intercept是函数的截距，x是自变量的系数

m=lm(y~x)
summary(m)
# Residuals：残差
# Coefficients:必看的数据

m2=lm(y~x+I(x^2)) #X的三次方再加一个I(x^3)
summary(m2)

m3=lm(y~(x^2))
summary(m3)

#预测,算出来的数值是回归值
y1=predict(m)
y2=predict(m2)
y3=predict(m3)
lines(x,y1) #需要提前plot(x,y)

y2=predict(m2,data.frame(x=c(8.8,9.1))) #用模型算两个新的自变量的值

x1=seq(min(x),max(x),0.01)
y1=predict(m,data.frame(x=x1))
y2=predict(m2,data.frame(x=x1))
y3=predict(m3,data.frame(x=x1))
lines(x1,y2)

summary(lm(y~x+I(x^2)+I(x^3)))


d=fread("C:/Users/HP/Desktop/energy(1).csv")
library(corrplot)
corrplot(cor(d)) #相关性
 
#2021.12.24 回归分析#
library(data.table)
data=fread("C:/Users/Hp/Desktop/energy(1).csv")
#读取csv格式文件，read.table读入的文件会有null或者第一行会有乱码，或二者都有……
#考察GDP和什么数据有关系
data1<-as.data.table(scale(data))
#把data中的所有数据标准化，应该对量纲的数据进行标准化
#as.data.table:能够去掉只运行scale(data)中的attr(,"scaled:center")，并将结果输出到data1中，只要数字，不要其他的标准化结果
plot(data1)
cor(data1) #相关阵
#相关分析
summary(lm(GDP~people+energy+coal+oil+gas+qcurrency+currency))
#七个自变量：七元线性回归模型
#出现报错：Error in eval(predvars, data, env) : object 'people' not found,出现的原因是数据未解锁,应先运行attach(data1)
attach(data1)
summary(lm(GDP~people+energy+coal+oil+gas+qcurrency+currency))
#F检验结果：P值<2.2e-16，R方值（又叫决定系数）
summary(lm(GDP~-1+people+energy+coal+oil+gas+qcurrency+currency))
##修改模型，加上“-1+”之后不显示常数项
summary(lm(GDP~-1+people+coal+oil+gas+qcurrency+currency))
#energy对于解释GDP没有作用，根据结果去掉Coefficients中没有显著差异的自变量
summary(lm(GDP~-1+people+coal+gas+qcurrency+currency))
#oil对于解释GDP也没有作用,根据结果去掉Coefficients中没有显著差异的自变量
#模型通过检验，(GDP-GDP平均值)/sd(GDP)=0.16493(people-people平均值)/sd(people)+0.08806(coal-coal平均值)/sd(coal)+...
m=lm(GDP~-1+people+coal+gas+qcurrency+currency)
#将模型存成m
predict(m)
#预测
summary(lm(GDP~-1+people+coal+gas+qcurrency+currency+I(people^2)))
summary(lm(GDP~-1+people+coal+gas+qcurrency+currency+I(people^3)))
#结合散点图修正模型，因为people和GDP不是线性关系
