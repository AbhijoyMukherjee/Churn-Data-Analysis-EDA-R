#Extraction of Data:
EDA<-read.csv(file='C:\\Users\\Lenovo\\Documents\\EDA_churn.csv',header=T,sep=',');EDA
library(ggplot2)
View(EDA)
df1=EDA
as.data.frame(EDA)
a<-(EDA$rev_Mean);a
summary(na.omit(a))
boxplot(na.omit(a),main='Mean Revenue spent by Users',ylim=c(0,1250),ylab='Revenue')
abline(58.72,0,col='red')
hist(na.omit(EDA$age1),xlim = c(9,100),freq = F,col='green')
EDA$income
ggplot(EDA)+geom_histogram(aes(rev_Mean))+xlim(1,375)+xlab('Mean no. of monthly revenue')+ylab('frequency')#Mean monthly revenue (charge amount)

ggplot(EDA)+geom_histogram(aes(mou_Mean),fill='light green',col='dark green',bins=60)+xlim(0,4000)+xlab('Mean number of monthly minutes')+ylab('Frequency')+facet_wrap(~churn)+theme_light()+labs(title='Distribution of Mean Monthly minutes among Churn and Non-churn',subtitle = 'Histogram 1.2')#Mean number of monthly minutes of use

ggplot(EDA)+geom_histogram(aes(totcalls),fill='sandy brown',col='dark magenta',bins=20)+xlim(0,12000)+xlab('Total Number of Calls')+facet_wrap(~churn)+ylab('Frequency')+theme_light()+labs(title='Distribution of total calls among Churn and Non-churn',subtitle = 'Histogram 1.3')#Mean number of monthly minutes of use

ggplot(EDA)+geom_histogram(aes(totcalls))+xlim(0,37500)#Total number of calls over the life of the customer

ggplot(EDA)+geom_histogram(aes(custcare_Mean),col='green',fill='black') + xlab('Mean number of customer care calls')+ylab('Frequency')+labs(title='Mean number of customer care calls among Churn and Non-churn data',subtitle = 'Histogram 1.5')+facet_wrap(~churn)+xlim(1,80) #Total minutes of use over the life of the customer
ggplot(EDA)+geom_histogram(aes(totrev))+xlim(1,8000)#Total revenue
ggplot(EDA)+geom_histogram(aes(adjrev),col='yellow',fill='green')+geom_freqpoly(aes(adjrev),col='red')+xlim(0,8000)+facet_wrap(~area)#Billing adjusted total revenue over the life of the customer

ggplot(EDA)+geom_histogram(aes(rev_Mean),col='pink',fill='violet')+geom_freqpoly(aes(rev_Mean),col='maroon')+xlim(1,250)+facet_wrap(~area)+theme_light()+xlab('Mean Monthly Revenue')+ylab('Frequency')+labs(title = 'Mean Monthly Revenue Generated across regions',subtitle = 'Histogram and Frequency Polygon 1.1')+theme_classic()#Billing adjusted total revenue over the life of the customer
table(EDA$rev_Mean)
par(mfrow=c(1,2))
hist(EDA$age1,xlim = c(9,100),col ='dark cyan',main='Age of first Household Member',xlab='Age(in years)',ylim = c(0,15000),sub='Histogram 1.4')
hist(EDA$age2,xlim = c(9,100),col ='dark cyan',main='Age of second Household Member',xlab='Age(in years)',ylim=c(0,15000))

ggplot(EDA)+geom_histogram(aes(totcalls),col='yellow',fill='green',col='red')+geom_freqpoly(aes(totcalls),col='red')+xlim(1,27500)+facet_wrap(~area=='NEWYORK CITY AREA')+theme_dark()#Billing adjusted total revenue over the life of the customer
ggplot(EDA)+geom_histogram(aes(totmou),col='yellow',fill='green')+geom_freqpoly(aes(totmou),col='orange')+xlim(0,60500)+facet_wrap(~area=='NEWYORK CITY AREA')+theme_dark()#Billing adjusted total revenue over the life of the customer


ggplot(EDA)+geom_histogram(aes(age1),col='green',fill='black',binwidth = 15)+xlim(1,100)+theme_classic()
ggplot(EDA)+geom_histogram(aes(age2))+xlim(1,100)
library(dplyr)
table(EDA$area)
a<-subset(EDA,area=='MIDWEST AREA')
b<-subset(EDA,area=='CHICAGO AREA')
c<-subset(EDA,area=='SOUTHWEST AREA')
d<-subset(EDA,area=='TENNESSEE AREA')
e<-subset(EDA,area=='SOUTH FLORIDA AREA')
f<-subset(EDA,area=='PHILADELPHIA AREA')
g<-subset(EDA,area=='OHIO AREA')
h<-subset(EDA,area=='NORTH FLORIDA AREA')
i<-subset(EDA,area=='NEW ENGLAND AREA')
j<-subset(EDA,area=='NEW YORK CITY AREA')
k<-subset(EDA,area=='NORTHWEST/ROCKY MOUNTAIN AREA')
l<-subset(EDA,area=='LOS ANGELES AREA')
m<-subset(EDA,area=='GREAT LAKES AREA')
n<-subset(EDA,area=='DALLAS AREA')
o<-subset(EDA,area=='HOUSTON AREA')
p<-subset(EDA,area=='DC/MARYLAND/VIRGINIA AREA')
q<-subset(EDA,area=='CALIFORNIA NORTH AREA')
r<-subset(EDA,area=='ATLANTIC SOUTH AREA')
s<-subset(EDA,area=='CENTRAL/SOUTH AREA')



a<-subset(EDA,area=='MIDWEST AREA')%>%select('totrev')
b<-subset(EDA,area=='CHICAGO AREA')%>%select('totrev')
c<-subset(EDA,area=='SOUTHWEST AREA')%>%select('totrev')
d<-subset(EDA,area=='TENNESSEE AREA')%>%select('totrev')
e<-subset(EDA,area=='SOUTH FLORIDA AREA')%>%select('totrev')
f<-subset(EDA,area=='PHILADELPHIA AREA')%>%select('totrev')
g<-subset(EDA,area=='OHIO AREA')%>%select('totrev')
h<-subset(EDA,area=='NORTH FLORIDA AREA')%>%select('totrev')
i<-subset(EDA,area=='NEW ENGLAND AREA')%>%select('totrev')
j<-subset(EDA,area=='NEW YORK CITY AREA')%>%select('totrev')
k<-subset(EDA,area=='NORTHWEST/ROCKY MOUNTAIN AREA')%>%select('totrev')
l<-subset(EDA,area=='LOS ANGELES AREA')%>%select('totrev')
m<-subset(EDA,area=='GREAT LAKES AREA')%>%select('totrev')

n<-subset(EDA,area=='DALLAS AREA')%>%select('totrev')
o<-subset(EDA,area=='HOUSTON AREA')%>%select('totrev')
p<-subset(EDA,area=='DC/MARYLAND/VIRGINIA AREA')%>%select('totrev')
q<-subset(EDA,area=='CALIFORNIA NORTH AREA')%>%select('totrev')
r<-subset(EDA,area=='ATLANTIC SOUTH AREA')%>%select('totrev')
s<-subset(EDA,area=='CENTRAL/SOUTH AREA')%>%select('totrev')

data<-c(sum(a),sum(b),sum(c),sum(d),sum(e),sum(f),sum(g),sum(h),sum(i),sum(j),sum(k),sum(l),sum(m),sum(n),sum(o),sum(p),sum(q),sum(r),sum(s))
data

pie(as.matrix(data),labels=c('MIDWEST','CHICAGO','SOUTHWEST','TENNESSEE','SOUTHFLORIDA','PHILADELPHIA','OHIO','NORTHFLORIDA','NEWENGLAND','NEWYORKCITY','ROCKYMOUNTAIN','LOSANGELES','GREATLAKES','DALLAS','HOUSTON','VIRGINIA','CALIFORNIANORTH','ATLANTICSOUTH','CENTRAL/SOUTH'),cex.names = 0.3)
?pie
ggplot(EDA,aes(x=rev_Mean))+geom_boxplot(outlier.color='green',outlier.size = 1,outlier.shape = 23,fill='red')+xlim(0,1250)+coord_flip()+facet_wrap(~area)+theme_dark()
?geom_boxplot
?geom_abline
ggplot(EDA,aes(x=rev_Mean))+geom_boxplot(outlier.color='green',outlier.size = 1,outlier.shape = 23,fill='red')+xlim(0,1250)+coord_flip()+facet_wrap(~area)+theme_dark()
ggplot(EDA,aes(x=(rev_Mean)))+geom_histogram(fill='green',col='yellow',bins=30)+xlim(5,600)+facet_wrap(~area)+theme_dark()+geom_freqpoly(aes(x=rev_Mean),col='red')

library(ggplot2)

ggplot(EDA)+geom_histogram(aes(x=rev_Mean),bins=40,col='purple',fill='pink')+facet_wrap(~churn)+xlim(1,500)+theme_bw()+labs(title = 'Mean Revenue Distribution for Churn and Non-Churn Data',x='Revenue Mean',y='Frequency',subtitle = 'Histogram 1.1')

ggplot(EDA)+geom_boxplot(aes(x=rev_Mean),bins=40,col='purple',fill='pink')+facet_wrap(~churn)+xlim(1,750)+theme_bw()


ggplot(EDA)+geom_bar(aes(x=adults,y=..count..),col='green',fill='black')+facet_grid(~churn)+theme_classic()

ggplot(EDA,aes(x=rev_Mean,y=mou_Mean))+geom_point(col='red')+geom_smooth(col='black')+xlim(0,1500)+ylim(0,8000)+labs(title='Mean Monthly Usage(in mins) vs Mean Monthly Revenue',subtitle='Scatterplot 1.1',y='Mean Monthly Usage(in mins)',x='Mean Monthly Revenue')

cor(na.omit(EDA$mou_Mean),na.omit(EDA$rev_Mean))
