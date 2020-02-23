data(iris)
data<- iris
str(data)

#facet_grid/wrap설명하기 위해서 season변수 생성
iris$season <- sample(x=1:4,size=150,replace = T)
iris$season2 <- ifelse(iris$season==1,"봄",
                       ifelse(iris$season==2,"여름",
                              ifelse(iris$season==3,"가을","겨울")))
iris$season2 <- as.factor(iris$season2)
data<- iris
library(ggplot2)

#시각화 시작
ggplot(data,aes(Sepal.Length,Sepal.Width,col=Species,shape=Species))+
  geom_point()+
  ggtitle('iris visualization')+
  xlab('길이')+#x축 제목
  ylab('넓이')+#y축 제목
  coord_fixed()+ #x축y축 비율 1:1설정
  #coord_flip() - x축y축 transpose
  #geom_hline(yintercept = mean(data$variable),col="#FF6600",cex=0.9)
  #geom_vline(xintercept = mean(data$variable),col="#FF6600",cex=0.9)
  scale_y_continuous(limits=c(1, 5))+ #  = ylim 
  #scale_y_continuous(breaks = c(a,b,c,d))로 축을 임의로 지정할 수 있음
  scale_x_continuous(limits=c(3.75,10))+ #  = xlim
  facet_wrap(~season2)+ # 그래프르 격자형식으로 지정하기
  geom_smooth(method=lm, fullrange=TRUE,se=F)+#추세선 그리기
  geom_text(x=5,y=1.5,label = '안녕하세요',size = 5)+#원하는 곳에 text삽입
  theme(plot.title = element_text(size = 35, hjust=0.5,family = "Tahoma", face = "bold"),
        text = element_text(size = 20, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 15,face="bold"),
        legend.title=element_text(size=20), 
        legend.text=element_text(size=20),
        strip.text.x = element_text(size = 20, face="bold"))

#boxplot
ggplot(data,aes(x=Species,y=Sepal.Length,fill=Species))+
  geom_boxplot()

#line plot
library(dplyr)
line_data <- data %>% group_by(Species) %>% summarise(mean = mean(Sepal.Length))
ggplot(line_data,aes(x=Species,y=mean,group=1))+
  geom_line()+
  theme(plot.title = element_text(size = 35, hjust=0.5,family = "Tahoma", face = "bold"),
        text = element_text(size = 20, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 15,face="bold"),
        legend.title=element_text(size=20), 
        legend.text=element_text(size=20),
        strip.text.x = element_text(size = 20, face="bold"))

#범례를 추가하여 그래프를 그리고 싶다면?
line_data <- data %>% group_by(Species,season2) %>% summarise(mean = mean(Sepal.Length))
ggplot(line_data,aes(x=Species,y=mean,col=season2,group=season2))+
  geom_line(size=1)+
  theme(plot.title = element_text(size = 35, hjust=0.5,family = "Tahoma", face = "bold"),
        text = element_text(size = 20, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 15,face="bold"),
        legend.title=element_text(size=20), 
        legend.text=element_text(size=20),
        strip.text.x = element_text(size = 20, face="bold"))

#히스토그램
ggplot(data, aes(data$season2)) + 
  geom_histogram(stat = 'count',col="red", 
                 fill="green", 
                 alpha = .2) + 
  labs(title="Histogram for Age") +
  labs(x="iris종", y="Count")
table(data$season2)  

