## Load the library
library(plyr)
library(dplyr) #filter()
library(corrplot) #corrplot()
library(ggplot2) #ggplot()
library(gridExtra) #grid.arrange()
library(Rmisc) #multiplot()
library(e1071) #naiveBayes()
library(rpart) #raprt()
library(pROC) #roc()
library(rpart.plot) #raprt.plot()
library(randomForest) #randomForest()
library(caret) #confusionMatrix()
library(DT) #datatable()


## Understand Data
hr <- read.csv("HRSurveyData.csv", stringsAsFactors = FALSE)
str(hr)


## Data Pre-processing
# Rename variables
hr <- rename(hr, satisfaction = satisfaction_level, evaluation = last_evaluation, project = number_project, monthlyhour = average_montly_hours, serviceyear = time_spend_company, accident = Work_accident, promotion = promotion_last_5years, dept = sales)

# Transfer the variable “left” into a factor variable
# hr$left <- factor(hr$left, levels = c("9", "1"))

# Add a new variable salarynum
hr$salarynum[hr$salary == "low"] <-1 
hr$salarynum[hr$salary == "medium"] <-2 
hr$salarynum[hr$salary == "high"] <-3

summary (hr)



## Data Analysis and Visualisation: Correlation Matrix
hr_cor <- hr %>% select(satisfaction:promotion, salarynum)
corrmatrix <- cor(hr_cor)
corrplot.mixed(corrmatrix)
corrplot(cor(corrmatrix),type="upper",method = "circle",tl.pos = "tl",tl.offset = 0.05)
corrplot(cor(corrmatrix),add=T,type="lower",method = "number",col="red",diag=F, tl.pos ="n",cl.pos ="n")


## Transfer variable data type into factor data type
hr$accident <- as.factor(hr$accident) 
hr$left <- as.factor(hr$left)
hr$promotion <- as.factor(hr$promotion) 
hr$dept <- as.factor(hr$dept) 
hr$salary <- as.factor(hr$salary)

## Analysing categorical variables
g1 <- ggplot(group_by(hr, dept), aes(x = dept, fill = dept)) + geom_bar(width =1) +coord_polar(theta = "x") + ggtitle("Chart1: Number of employees in differ ent departments")
g2 <- ggplot(group_by(hr, dept), aes(x = dept, fill = salary)) + geom_bar(width = 1) +
  coord_polar(theta = "x") + ggtitle("Chart2: Salary status in different de partments")
multiplot(g1, g2, cols = 2)


## Analysing categorical and numerical variables
g3 <- ggplot(hr, aes(x = dept, y = satisfaction, fill = dept)) + geom_boxplot()+ggtitle("Chart3: Satisfaction level of different departments") +stat_summary(fun.y = mean,size = 3, color = 'white', geom = "point") + theme(legend.position = "none")

g4 <- ggplot(hr, aes(x = dept, y = evaluation, fill = dept)) + geom_boxplot() +ggtitle("Chart4: Last evaluation of different departments") +
  stat_summary(fun.y = mean,size = 3, color = 'white', geom = "point") + theme(legend.position = "none")

grid.arrange(g3, g4, ncol = 2)



## Analysing possible reasons of employee attrition
ggplot(hr, aes(x = dept, y = evaluation, fill = left)) + geom_boxplot() +
  ggtitle("Chart6: Employee attrition status with different last evaluation")

ggplot(hr, aes(x = dept, y = monthlyhour, fill = left)) + geom_boxplot() +
  ggtitle("Chart7: Employee attrition status with different working hours")


b1 <- ggplot(hr, aes(x = project, fill = left))+geom_bar(position = 'fill' ) +theme_bw() + labs(x = 'numbers of project', y ='percentage')

b2 <- ggplot(hr, aes(x = serviceyear, fill = left)) +geom_bar(position = 'fill')+theme_bw() + labs(x ='years of working in the company’, y =’percentage')

b3 <- ggplot(hr, aes(x = accident, fill = left))+geom_bar(position = 'fill')+theme_bw() + labs(x ='accident since joining the company’， y =‘percenta ge')

b4 <- ggplot(hr, aes(x = promotion, fill = left))+geom_bar(position = 'fill')+theme_bw() + labs(x ='promotion in last 5 years', y = 'percentage')

grid.arrange(b1,b2,b3,b4,ncol = 2, nrow = 2)

b6<- ggplot(hr, aes(x = salary, fill = left)) + geom_histogram(stat = "count") + ggtitle ("Relat ionship between salary and employee attrition") 

b7 <-ggplot(hr, aes(x = promotion, fill = left)) + geom_histogram(stat = "count") +ggtitle ("Relationship between promot ion in 5 years and employee attrition" ) 

b8 <- ggplot(hr, aes(x = satisfaction, color = left))+ geom_line (stat = "density") + ggtitle ( "Relationship between satisfaction level and employee attrition ") 

b9 <-ggplot(hr, aes(x = evaluation, color = left)) + geom_point (stat = "count") + ggtitle("Relationship between last evaluation and employee attrition " ) 
grid.arrange(b6,b7,b8,b9,ncol = 2,nrow = 2)

hr %>%
  group_by(dept) %>%
  ggplot(aes(y = dept,fill = accident)) + geom_bar() + coord_flip() +
  theme(axis.text.y = element_blank(), axis.title.y = element_blank(),
        axis.title.x = element_blank()) +
  scale_fill_discrete(labels = c("no accident", "had accident"))







