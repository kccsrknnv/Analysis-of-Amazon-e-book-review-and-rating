data <-read.csv('bestsellers with categories.csv')
head(data)
str(data)
summary(data)
View(data)
#removing null values
data[is.na(data)] = 0
data_clean<-data
View(data_clean)
head(data_clean)
#Histogram
ggplot(data_clean, aes(x=User.Rating))+
  geom_histogram(color="darkblue", fill="lightblue",bins=30)
#Pie chart
library(lessR)
PieChart(User.Rating,data = data_clean)
#scatterplot
ggplot(data_clean, aes(x=Price, y=User.Rating)) + 
  geom_point()+
  geom_smooth(method =lm)
ggplot(data_clean, aes(x=Price, y=Reviews)) + 
  geom_point()+
  geom_smooth(method =lm)
#Ranking
data_clean%>% group_by(Genre) %>% summarise(n = n()) %>% 
  ggplot(aes(x = reorder(Genre, -n), y = n,fill=Genre)) + 
  geom_col()
PieChart(Genre,data = data_clean)
ggplot(data_clean, aes(x=Year))+
  geom_histogram(color="darkblue", fill="lightblue",bins=30)
data_clean%>% group_by(User.Rating,Genre) %>% summarise(n = n()) %>% 
  ggplot(aes(x = reorder(User.Rating, -n), y = n,fill=Genre)) + 
  geom_col()

data_clean%>% group_by(Year) %>% summarise(n = mean(Price)) %>% 
  ggplot(aes(x = reorder(Year, -n), y = n)) + 
  geom_col()
data_clean%>% group_by(Year) %>% summarise(n = mean(Reviews)) %>% 
  ggplot(aes(x = reorder(Year, -n), y = n)) + 
  geom_col()
data_clean%>% group_by(Year) %>% summarise(n = mean(User.Rating)) %>% 
  ggplot(aes(x = reorder(Year, -n), y = n)) + 
  geom_col()

data_clean%>% group_by(Year,Genre) %>% summarise(n = mean(Price)) %>% 
  ggplot(aes(x = reorder(Year, -n), y = n,fill=Genre)) + 
  geom_col()
data_clean%>% group_by(Genre,Year) %>% summarise(n = mean(Reviews)) %>% 
  ggplot(aes(x = reorder(Year, -n), y = n,fill=Genre)) + 
  geom_col()

data_clean%>% group_by(Year,Genre) %>% summarise(n = mean(User.Rating)) %>% 
  ggplot(aes(x = reorder(Year, -n), y = n,fill=Genre)) + 
  geom_col()
top<-top_n(data_clean,n = 25, wt = Price)
ggplot(top, aes(x=Author, 
                       y=Price)) +   
  geom_bar(stat="identity", color='red',fill='red')
top1<-top_n(data_clean,n = 10, wt = User.Rating)
ggplot(top1, aes(x=Author, 
                y=User.Rating)) +   
  geom_bar(stat="identity", color='red',fill='red')
top2<-top_n(data_clean,n = 20, wt = Reviews)
ggplot(top2, aes(x=Author, 
                 y=Reviews)) +   
  geom_bar(stat="identity", color='red',fill='red')
data_clean%>%filter(Price==0)%>% group_by(Author) %>% summarise(n = n()) %>%
  ggplot(aes(x = reorder(Author, -n), y = n,fill=Author)) + 
  geom_col()
#Corelation
library(ggcorrplot)
numeric<-data_clean[,c(3:6)]#taking only nemeric value
corr <- round(cor(numeric), 1)
print(corr)
p.mat <- cor_pmat(numeric)
ggcorrplot(corr,hc.order=TRUE,type="full",
           lab=TRUE,lab_size=3,method="square",
           colors=c("red","blue","green","grey","orange","pink"),
           title="Correlation of Placement Data",ggtheme = ggplot2::theme_gray)

