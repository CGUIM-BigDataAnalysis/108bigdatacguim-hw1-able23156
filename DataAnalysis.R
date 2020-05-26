#install.packages("dplyr")
library(jsonlite)
library(dplyr)
library(stringr)
library(readr)
a104 <- read_csv("C:/Users/ab/Desktop/104a.csv")
a107 <- read_csv("C:/Users/ab/Desktop/107a.csv")


a104$大職業別<-gsub("部門|、","",a104$大職業別)
a107$大職業別<-gsub("_","",a107$大職業別)
a107$大職業別<-gsub("建工程","造業",a107$大職業別)
a107$大職業別<-gsub("出版、影音製作、傳播及資通訊服務業","資訊及通訊傳播業",a107$大職業別)
a107$大職業別<-gsub("教育業","教育服務業",a107$大職業別)
a107$大職業別<-gsub("醫療保健業","醫療保健服務業",a107$大職業別)

a104$`大學-薪資`<-as.numeric(gsub("—|…",NA,a104$`大學-薪資`))
a107$`大學-薪資`<-as.numeric(gsub("—|…",NA,a107$`大學-薪資`))

Compare<-(inner_join(a104,a107,by="大職業別"))
View(Compare)
#1

salary<-select(a107,大職業別)
salary$salaryrate<-((a107$`大學-薪資`)/(a104$`大學-薪資`))
salary<-salary[complete.cases(salary$salaryrate),]
rate<-filter( salary , salaryrate > 1.05 )
highsalaryrate<-arrange(rate,desc(salaryrate))
View(head(highsalaryrate,10))
View(highsalaryrate)

table(sapply (strsplit (rate$大職業別,"-") , "[" ,  1))%>%
  View()

#2

a104$`大學-女/男`<-as.numeric(gsub("—|…",NA,a104$`大學-女/男`))
b104<-a104[complete.cases(a104$`大學-女/男`),]
b104<-select(b104,大職業別,`大學-女/男`)
b104<-b104[order(b104$`大學-女/男`,decreasing = T),]


a107$`大學-女/男`<-as.numeric(gsub("—|…",NA,a107$`大學-女/男`))
c107<-a107[complete.cases(a107$`大學-女/男`),]
c107<-select(c107,大職業別,`大學-女/男`)
c107<-c107[order(c107$`大學-女/男`,decreasing = T),]


b104$`大學-女/男`<-as.character(b104$`大學-女/男`)
c107$`大學-女/男`<-as.character(c107$`大學-女/男`)

tail(b104,10)
tail(c107,10)


head(b104,10)
head(c107,10)

#第三題
a107$`研究所-薪資`<-as.numeric(gsub("—|…",NA,a107$`研究所-薪資`))
a107$`大學-薪資`<-as.numeric(gsub("—|…",NA,a107$`大學-薪資`))
dataset <- data.frame(name = a107$大職業別)
dataset$salaryrate<-a107$`研究所-薪資`/a107$`大學-薪資`
dataset<-dataset[complete.cases(dataset$salaryrate),]
dataseta<-dataset[order(dataset$salaryrate,decreasing = T),]

head(dataseta,10)


#第四題

