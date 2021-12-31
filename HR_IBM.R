# Version_0.1

# them cac thu vien
library(VIM)
library(plotrix)
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(corrplot)
library(shiny)
library(DT)
library(shinydashboard)
library(shinythemes)
library(ggthemes)
library(ggcorrplot)
library(cowplot)
library(caret)
library(rpart)
library(corrplot)
library(pROC)

rm(list = ls())

# Data HR IBM
IBM <-read.csv("E://HR_IBM/HRAnalytics/datasheet/Watson_Analytics_IBM_Sample_data.csv", header = TRUE, stringsAsFactors = TRUE)
desc <-read.csv("E://HR_IBM/HRAnalytics/datasheet/Description_Var_IBM.csv")

summary(IBM)
# Kiem tra su trung lap
anyDuplicated(IBM)

# NA trong data
anyNA(IBM)
apply(is.na(IBM), 2, sum)
#VIM::aggr(IBM)

# Tao bang moi
data <- IBM[,-c(9,10,22,27)] #loai bo bien : EmployeeCount , EmployeeNumber , Over18 ,StandardHours
cols <- c(c(2,3,5,7,8,9,10,12,13,14,15,16,20,22,23,24,27))
data[cols] <- lapply(data[cols], factor)

# Su tuong quan giua cac bien so
numeric.var <- sapply(data, is.numeric)
corr.matrix <- cor(data[,numeric.var])
#corrplot(corr.matrix, main = 'Bieu do tuong quan cho cac bien so',method = "number")


general1 <- data  #Full general1 <- IBM
df <- IBM

# Nhan vien co roi ct hay khong
table(general1$Attrition)

# Khao sat su hai long cua nhan vie: 4 van de
survey_eplot <- gather(general1, Satisfaction, value, EnvironmentSatisfaction, JobSatisfaction, WorkLifeBalance,RelationshipSatisfaction, na.rm = TRUE)


#---------------------------------------------------------#
# Mo hinh 1: Tao cay quyet dinh (Xac dinh tam quan trong cua cac bien)
data_ibm <- data
trainIndex <- createDataPartition(data_ibm$Attrition, p=0.80,list=FALSE, times=1)
train <- data_ibm[trainIndex,]
test <- data_ibm[-trainIndex,]

prop_train <- train %>% select(Attrition) %>%
  group_by(Attrition) %>% summarize(n=n()) %>%
  mutate(pct=round(prop.table(n), 2))

prop_test <- test %>% select(Attrition) %>% 
  group_by(Attrition) %>% summarize(n=n()) %>%
  mutate(pct=round(prop.table(n), 2))
rpart.tree <- rpart(Attrition ~ ., data=train)

var_imp <- data.frame(rpart.tree$variable.importance)
var_imp$features <- rownames(var_imp)
var_imp <- var_imp[, c(2, 1)]
var_imp$importance <- round(var_imp$rpart.tree.variable.importance, 2)
var_imp$rpart.tree.variable.importance <- NULL
colorCount <- length(unique(var_imp$features))

feature_importance <- var_imp %>%
  ggplot(aes(x=reorder(features, importance), y=importance, fill=features)) + geom_bar(stat='identity') + coord_flip() + 
  theme(legend.position = "none",plot.title = element_text(hjust = 0.5))+
  geom_label(aes(label=paste0(importance, "%")), colour = "white", fontface = "italic", hjust=0.6) + 
  labs(title="", x="Features", y="Importance")

# Matran nham lan voi data test
predictions <- predict(rpart.tree, test, type="class")
conf_ibm <- data.frame(table(test$Attrition, predictions))

#---------------------------------------------------------#

# Mo hinh 2 : Phân tích hồi quy logistic
# Converting attrition to numeric

logistic <- IBM
logistic$Att <-as.numeric(IBM$Attrition)

logistic$Att[IBM$Attrition=="Yes"]=1
logistic$Att[IBM$Attrition=="No"]=0

# Train and test data model
set.seed(1000)
ttdata=sample(x=c("Training","Testing"),size=nrow(logistic),replace=T,prob=c(0.8,0.2))
TrainingData=logistic[ttdata=="Training",]
TestingData=logistic[ttdata=="Testing",]


#Phân tích hồi quy logistic Bước 1: 35 Biến
## Xây dựng mô hình theo một số bước đơn giản như sau:
# 1.Xác định các biến độc lập
# 2.Kết hợp biến phụ thuộc "Attrition" trong mô hình
# 3.Tìm các biến quan trọng nhất trong việc xác định mức độ tiêu hao của nhân viên
# 4.Chuyển đổi kiểu dữ liệu của mô hình từ “ký tự” thành “công thức”
# 5.Kết hợp dữ liệu TRAINING vào công thức và xây dựng mô hình

# Đầu tiên, kết hợp 30 biến phụ thuộc để tìm ra các biến có ý nghĩa
independentvariables1=c("Age", "BusinessTravel", "DailyRate", "Department", "DistanceFromHome", "Education", "EducationField", "EnvironmentSatisfaction", "Gender", "HourlyRate", "JobInvolvement", "JobLevel", "JobRole", "JobSatisfaction", "MaritalStatus", "MonthlyIncome", "MonthlyRate", "NumCompaniesWorked", "OverTime", "PercentSalaryHike", "PerformanceRating", "RelationshipSatisfaction", "StockOptionLevel", "TotalWorkingYears", "TrainingTimesLastYear", "WorkLifeBalance", "YearsAtCompany", "YearsInCurrentRole", "YearsSinceLastPromotion", "YearsWithCurrManager")
Model1=paste(independentvariables1,collapse="+")
Model_1=paste("Att~",Model1)
formula=as.formula(Model_1)

# Chạy hồi quy logistic bằng Training Model1
Trainingmodel1=glm(formula=formula,data=TrainingData,family="binomial")
Trainingmodel1=step(object = Trainingmodel1,direction = "both")
summary(Trainingmodel1)

# Phân tích hồi quy logistic Bước 2: 17 Biến 
# Sau lần lặp ghi điểm đầu tiên, chọn giá trị quan trọng nhất dựa trên giá trị P, là 17 biến 
# Kết hợp 17 biến phụ thuộc để tìm ra các biến có ý nghĩa
independentvariables2=c("BusinessTravel", "DistanceFromHome", "EnvironmentSatisfaction","JobInvolvement","JobRole","JobSatisfaction","MaritalStatus","NumCompaniesWorked","OverTime","RelationshipSatisfaction","TotalWorkingYears", "TrainingTimesLastYear","WorkLifeBalance","YearsAtCompany","YearsInCurrentRole", "YearsSinceLastPromotion","YearsWithCurrManager")
Model2=paste(independentvariables2,collapse="+")
Model_2=paste("Att~",Model2)
formula=as.formula(Model_2)

# Chạy hồi quy logistic bằng TrainingModel2
Trainingmodel2=glm(formula=formula,data=TrainingData,family="binomial")
Trainingmodel2=step(object = Trainingmodel2,direction = "both")
summary(Trainingmodel2)

# Phân tích hồi quy logistic Bước 3: 16 Biến
# Sau lần lặp lại tính điểm thứ hai, hãy loại bỏ biến JobRole vì JobRoleSales Đại diện là JobRole duy nhất có ý nghĩa quan trọng
# Vì vậy, chúng tôi loại bỏ JobRole như một biến cho mô hình của chúng tôi
# Kết hợp 16 biến phụ thuộc để tìm ra các biến có ý nghĩa
independentvariables3=c("BusinessTravel", "DistanceFromHome", "EnvironmentSatisfaction","JobInvolvement","JobSatisfaction","MaritalStatus","NumCompaniesWorked","OverTime","RelationshipSatisfaction","TotalWorkingYears", "TrainingTimesLastYear","WorkLifeBalance","YearsAtCompany","YearsInCurrentRole", "YearsSinceLastPromotion","YearsWithCurrManager")
Model3=paste(independentvariables3,collapse="+")
Model_3=paste("Att~",Model3)
formula=as.formula(Model_3)

# Chạy hồi quy logistic bằng TrainingModel3
Trainingmodel3=glm(formula=formula,data=TrainingData,family="binomial")
Trainingmodel3=step(object = Trainingmodel3,direction = "both")
summary(Trainingmodel3)

# Phân tích hồi quy logistic Bước 4: 13 Biến
# Sau lần lặp lại tính điểm thứ ba, WorkLifeBalance, YearsAtCompany, YearsWithCurrManager
# Kết hợp 13 biến phụ thuộc để tìm ra các biến có ý nghĩa
independentvariables4=c("BusinessTravel", "DistanceFromHome", "EnvironmentSatisfaction","JobInvolvement","JobSatisfaction","MaritalStatus","NumCompaniesWorked","OverTime","RelationshipSatisfaction","TotalWorkingYears", "TrainingTimesLastYear","YearsInCurrentRole", "YearsSinceLastPromotion")
Model4=paste(independentvariables4,collapse="+")
Model_4=paste("Att~",Model4)
formula=as.formula(Model_4)

# Chạy hồi quy logistic bằng TrainingModel4
Trainingmodel4=glm(formula=formula,data=TrainingData,family="binomial")
Trainingmodel4=step(object = Trainingmodel4,direction = "both")
summary(Trainingmodel4)

# Lập biểu đồ ROC cho Mô hình đào tạo 1 đến Mô hình đào tạo 4 và xác định AUC
#troc1=roc(response=Trainingmodel1$y,predictor = Trainingmodel1$fitted.values,plot=T)
#troc1$auc

#troc2=roc(response=Trainingmodel2$y,predictor = Trainingmodel2$fitted.values,plot=T)
#troc2$auc

#troc3=roc(response=Trainingmodel3$y,predictor = Trainingmodel3$fitted.values,plot=T)
#troc3$auc

#troc4=roc(response=Trainingmodel4$y,predictor = Trainingmodel4$fitted.values,plot=T)
#troc4$auc

#---------------------------------------------------------#

#---------------------------------------------------------#
# Shiny GUI

ui<-dashboardPage(
  dashboardHeader(title = "Nhân sự IBM"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Tổng quan", tabName = "B", icon = icon("dashboard")),
      menuItem("Thông tin", tabName = "A", icon = icon("calendar")),
      menuItem("Biểu đồ", tabName = "C", icon = icon("list-alt")),
      menuItem("Khảo sát", tabName = "D", icon = icon("bar-chart-o")),
      menuItem("Phân tích", tabName = "PT1", icon = icon("refresh")),
      menuItem("Decision Tree", tabName = "mohinh1", icon = icon("cog", lib = "glyphicon")),
      menuItem("Logistic Regression", tabName = "mohinh2", icon = icon("cog", lib = "glyphicon")),
      menuItem("Đánh giá", tabName = "E", icon = icon("table"))
      #menuItem("Widgets", tabName = "widgets4", icon = icon("table"))
    )
  ),
  
  ## Theme: cerulean,united,flatly
  dashboardBody(
    tabItems(
      tabItem(tabName = "A",
             navbarPage("", theme = shinytheme("flatly"),
                        tabPanel("Giới thiệu",
                                 h2("Dự án: Phân tích dữ liệu quản lý nhân sự của IBM và Hiệu suất của nhân viên"),
                                 h3("( Nguồn dữ liệu:", a(" https://www.kaggle.com/pavansubhasht/ibm-hr-analytics-attrition-dataset", href = "https://www.kaggle.com/pavansubhasht/ibm-hr-analytics-attrition-dataset")," )"),
                                 h3("IBM đã thu thập thông tin về mức độ hài lòng của nhân viên, mức thu nhập, thâm niên và một số dữ liệu về nhân khẩu học. Nó bao gồm dữ liệu của 1470 nhân viên. "),
                                 fluidRow(
                                   box(width = 12,plotOutput("plot24"))
                                 )
                        ),
                        tabPanel("Đặt vấn đề",
                          fluidRow(
                               box(width = 8,
                                 h4(" Sự hao hụt của nhân viên dẫn đến những chi phí đáng kể cho một doanh nghiệp, bao gồm chi phí gián đoạn kinh doanh, thuê nhân viên mới và đào tạo nhân viên mới. Do đó, doanh nghiệp rất quan tâm đến việc hiểu rõ các nguyên nhân, các nhân tố ảnh hưởng và tác động đến kinh doanh, làm giảm thiểu hiệu suất, sự hao hụt của nhân viên."),
                                 h4(" Việc sử dụng các mô hình phân loại để dự đoán xem một nhân viên có khả năng sẽ nghỉ việc hay không."),
                                 h4(" Việc tiếp cận nhân viên, hiểu tình hình hiện tại của nhân viên và hành động để khắc phục các yếu tố có thể kiểm soát được,ngăn chặn sớm.")
                                ),
                               box(width=4,plotOutput("plot200",height = 0),collapsible = T, status = "warning", title = "Note",
                                h4("Yêu cầu:"),
                                h4("1. Đọc và giải thích dữ liệu"),
                                h4("2. Tiền xử lý dữ liệu"),	
                                h4("3. Tạo ra các biến mới"),
                                h4("4. Đặt và trả lời các câu hỏi NC + biểu đồ"),
                                h4("5. Xây dựng ít nhất 1 mô hình"),
                                h4("6. Tạo giao diện"), solidHeader = T)
                               
                        )
                              
                        ),
                        tabPanel("Mô tả",
                                fluidRow(
                                   box(width = 12,dataTableOutput("table1"))
                                 )
                        ),
                        tabPanel("Dữ liệu",
                                 fluidRow(
                                   box(width = 12,dataTableOutput("table2"))
                                 )
                        )
                        ),
              
      ),
      tabItem(tabName = "B",
             fluidRow(
               # A static infoBox
               infoBox("Hiện taị:", 1233, "84%", icon = icon("credit-card")),
               # Dynamic infoBoxes
               infoBoxOutput("progressBox"),
               infoBoxOutput("approvalBox")
             ),
             tabPanel("Dữ liệu",
                      fluidRow(
                        box(width = 10,dataTableOutput("table3"))
                      )
             )
      
      ),
      tabItem(tabName = "C",
              fluidRow(
                box(width=12,plotOutput("plot108",height = 0), status = "primary", title = "Biểu đồ sự tương quan về việc nhân viên rời công ty ( Attrition ) với các yếu tố" ),
              ),
              fluidRow(
                box(width=5,plotOutput("plot1",height = 350),collapsible = T, status = "primary", title = "Nhân viên có rời đi", solidHeader = T ),
                box(width=7,plotOutput("plot2",height = 350),collapsible = T, status = "danger", title = "Tuổi của nhân viên", solidHeader = T),
                
              ),
              fluidRow(
                box(width=6,plotOutput("plot3",height = 350),collapsible = T, status = "warning", title = "Phòng - ban", solidHeader = T),
                box(width=6,plotOutput("plot4",height = 350),collapsible = T, status = "primary", title = "Trình độ học vấn",  solidHeader = T)
              ),
              fluidRow(
                box(width=6,plotOutput("plot5",height = 350),collapsible = T, status = "success", title = "Giới tính", solidHeader = T),
                box(width=6,plotOutput("plot6",height = 350),collapsible = T, status = "primary", title = "Tình trạng hôn nhân", solidHeader = T)
              ),
              fluidRow(
                box(width=4,plotOutput("plot11",height = 350),collapsible = T, status = "primary", title = "Tổng số năm nhân viên đã làm việc", solidHeader = T),
                box(width=4,plotOutput("plot12",height = 350),collapsible = T, status = "warning", title = "Số năm kể từ lần thăng hạng cuối cùng", solidHeader = T),
                box(width=4,plotOutput("plot13",height = 350),collapsible = T, status = "primary", title = "Mức thu nhập hàng tháng",solidHeader = T)
               ),
              fluidRow(
                box(width=7,plotOutput("plot14",height = 350),collapsible = T, status = "primary", title = "Tổng số công ty mà nhân viên đã làm việc", solidHeader = T),
                box(width=5,plotOutput("plot16",height = 350),collapsible = T, status = "warning", title = "Số năm nhân viên làm việc với người quản lý hiện tại",solidHeader = T)
                
              )
              
      ),
      tabItem(tabName = "D",
              navbarPage("", theme = shinytheme("flatly"),
                         tabPanel("Chung",
                                  fluidRow(
                                    box(width=10,plotOutput("plot7",height = 500),collapsible = T, status = "success", title = "Box 1", solidHeader = T)
                                  ),
                         ),
                         tabPanel("Môi trường làm việc",
                                  fluidRow(
                                    box(width=8,plotOutput("plot8",height = 500),collapsible = T, status = "success", title = "Box 2", solidHeader = T)
                                  )
                          ),
                         tabPanel("Mức độ hài lòng với công việc",
                                  fluidRow(
                                    box(width=8,plotOutput("plot9",height = 500),collapsible = T, status = "primary", title = "Box 3",  solidHeader = T)
                                   )
                         ),
                         tabPanel("Cân bằng cuộc sống công việc",
                                  fluidRow(
                                    box(width=8,plotOutput("plot10",height = 500),collapsible = T, status = "success", title = "Box 4", solidHeader = T),
                                   )
                         ),
                         tabPanel("Các mối quan hệ với đồng nghiệp",
                                  fluidRow(
                                    box(width=8,plotOutput("plot17",height = 500),collapsible = T, status = "success", title = "Box 4", solidHeader = T),
                                  )
                         )
              ),
            ),
      
    tabItem(tabName = "PT1",
              navbarPage("", theme = shinytheme("flatly"),
                      tabPanel("#1",
                        fluidRow(
                          box(width=6,plotOutput("plot15",height = 500),collapsible = T, status = "warning", title = "Biểu đồ tương quan cho các biến số", solidHeader = T),
                      )
                      
                      ),
                      tabPanel("#2",
                               fluidPage(titlePanel("Tên biến"),
                                         sidebarLayout(sidebarPanel(
                                           radioButtons(
                                             inputId  = "data",label = "Chọn 1 giá tri:",
                                             choices = c(
                                               "Age" = "Age",
                                               "Attrition" = "Attrition",
                                               "BusinessTravel"="BusinessTravel",
                                               "EducationField"= "EducationField",
                                               "DistanceFromHome" = "DistanceFromHome",
                                               "EnvironmentSatisfaction" = "EnvironmentSatisfaction",
                                               "JobRole" = "JobRole",
                                               "NumCompaniesWorked" = "NumCompaniesWorked",
                                               "RelationshipSatisfaction" = "RelationshipSatisfaction",
                                               "WorkLifeBalance" = "WorkLifeBalance",
                                               "Education" = "Education",
                                               "Gender" = "Gender",
                                               "JobSatisfaction" = "JobSatisfaction"
                                             ),
                                             selected = "Attrition"
                                           )
                                         ),
                                         mainPanel(tabsetPanel(
                                           tabPanel("Summary", verbatimTextOutput("mysummary")),
                                           tabPanel("Histogram", plotOutput("myplot1")),
                                           tabPanel("Boxplot", plotOutput("myplot2"))
                                         ))))
                      )
            )
       ),
      tabItem(tabName = "mohinh1",
              navbarPage("", theme = shinytheme("flatly"),
                      tabPanel("Data Train",
                        fluidRow(
                          box(width=10,plotOutput("plot18",height = 500),collapsible = T, status = "success", title = "Tạo cây quyết định để xác định tầm quan trọng của các biến", solidHeader = T)
                         )
                       ),
                      tabPanel("Data Test",
                         fluidRow(
                          box(width=8,plotOutput("plot19",height = 500),collapsible = T, status = "success", title = "Tạo ma trận nhầm lẫn bằng cách sử dụng dữ liệu thử nghiệm", solidHeader = T)
                          )
                       )
              )
      ),
      tabItem(tabName = "mohinh2",
              navbarPage("", theme = shinytheme("flatly"),
                         tabPanel("Lập biểu đồ ROC cho Mô hình đào tạo 1-> 4 và xác định AUC",
                                  fluidRow(
                                    box(width=6,plotOutput("plot20",height = 350),collapsible = T, status = "primary", title = "Training Model1", solidHeader = T),
                                    box(width=6,plotOutput("plot21",height = 350),collapsible = T, status = "danger", title = "Training Model2", solidHeader = T)
                                  ),
                                  fluidRow(
                                    box(width=6,plotOutput("plot22",height = 350),collapsible = T, status = "warning", title = "Training Model3", solidHeader = T),
                                    box(width=6,plotOutput("plot23",height = 350),collapsible = T, status = "success", title = "Training Model4", solidHeader = T)
                                  )
                         ),
                         tabPanel("Data Test",
                                  
                         )
              )
      ),
     tabItem(tabName = "E",
             fluidRow(
               box(width=6,plotOutput("plot201",height = 400),collapsible = T, status = "primary", title = "Đánh giá 1", solidHeader = T),
               box(width=6,plotOutput("plot211",height = 400),collapsible = T, status = "danger", title = "Đánh giá 2", solidHeader = T)
             ),
             fluidRow(
               
             )
      )
    )
  )
  )

###

server <-function(input,output) {
  
# ----Tong quan ------
  output$progressBox <- renderInfoBox({
    infoBox(
      "Nhân viên rời công ty:", paste0(10 + input$Age, "16%"), icon = icon("list"),
      color = "purple"
    )
  })
  
  output$approvalBox <- renderInfoBox({
    infoBox(
      "Tích cưc", "84%", icon = icon("thumbs-up", lib = "glyphicon"),
      color = "yellow"
    )
  })
 
  # table1
  output$table1<-DT::renderDataTable({ 
    DT::datatable(desc,options = list(
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#1c9099', 'color': '#a1d99b'});",
        "}")
    )) 
  })
  # table3
  output$table3<-DT::renderDataTable({ 
    DT::datatable(data,options = list(scrollX = TRUE),filter='top', editable = 'cell') 
  })
  
  # Data
  output$table2<-DT::renderDataTable({ 
  DT::datatable(IBM, options = list(scrollX = TRUE),filter='top', editable = 'cell') %>%
    formatStyle('Age',  color = 'Purple', backgroundColor = 'lightgreen', fontWeight = 'bold') %>%
    formatStyle('BusinessTravel',  color = 'Indigo', fontWeight = 'bold') %>%
    formatStyle('Attrition', fontWeight = styleInterval(5, c('normal', 'bold')))  %>%
    formatStyle(
      'DailyRate',
      background = styleColorBar(IBM$DailyRate, 'steelblue'),
      backgroundSize = '100% 40%',
      backgroundRepeat = 'no-repeat',
      backgroundPosition = 'center'
    )  %>%
    formatStyle(
      'DistanceFromHome',
      background = styleColorBar(IBM$DistanceFromHome, 'Teal'),
      backgroundSize = '100% 40%',
      backgroundRepeat = 'no-repeat',
      backgroundPosition = 'center'
    ) %>%
    formatStyle(
      'EmployeeNumber',
      background = styleColorBar(IBM$EmployeeNumber, 'Lime'),
      backgroundSize = '100% 70%',
      backgroundRepeat = 'no-repeat',
      backgroundPosition = 'center'
    ) %>%
    formatStyle(
      'HourlyRate',
      background = styleColorBar(IBM$HourlyRate, 'Orange'),
      backgroundSize = '100% 50%',
      backgroundRepeat = 'no-repeat',
      backgroundPosition = 'center'
    ) 
  })
 # Tuong quan voi su roi di cua nhan vien
  output$plot1<-renderPlot({
    ggplot(general1, aes(Attrition)) +
      geom_bar(position = "dodge", aes(y=(..count..)/sum(..count..), fill=Attrition)) + 
      scale_y_continuous(labels=scales::percent) +
      labs(title="Số lượng nhân viên đã rời bỏ công ty")+
      theme(plot.title=element_text(size=16,hjust=0.5))+
      ylab("Số lượng %") +
      xlab("Nhân viên rời công ty ?") +
      geom_text(aes(label = scales::percent((..count..)/sum(..count..)), y=(..count..)/sum(..count..)), stat= "count",vjust =-.5)+
      scale_fill_brewer(palette="Dark2")
  })
  
  output$plot2<-renderPlot({
    ggplot(general1, aes(Age, color=Attrition, fill=Attrition)) +
      geom_density() +
      theme(plot.title=element_text(size=16,hjust=0.5))+
      labs(title = "Độ tuổi của nhân viên")+
      scale_fill_brewer(palette="Set2")
      
  })
  
  output$plot3<-renderPlot({
    ggplot(general1,aes(x=Attrition,group=Department))+
      geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
      facet_grid(~Department)+
      theme(axis.text.x=element_text(angle=90,vjust=0.5),legend.position="none",plot.title=element_text(size=16,hjust=0.5))+
      labs(x="Nhân viên rời công ty ?",y="Percentage",title="Số nhân viên rời công ty ở các phòng ban")+
      geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ),stat= "count",vjust =-.5) +
      scale_y_continuous(labels=scales::percent) +
      ylab("Số lượng %") +
      scale_fill_brewer(palette="Accent")
  })
  
  output$plot4<-renderPlot({
    ggplot(general1,aes(x=Attrition,group=Education))+
      geom_bar(aes(y=..prop..,fill=factor(..group..)),stat="count")+
      facet_grid(~Education)+
      labs(x="Nhân viên rời công ty ?",y="Percentage",title="Trình độ học vấn của nhân viên")+
      geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ),stat= "count",vjust =-.5) +
      scale_y_continuous(labels=scales::percent) +
      theme(plot.title=element_text(size=16,hjust=0.5))+
      ylab("Nhân viên %") +
      scale_fill_discrete(name="Trình độ", label=c ("Dưới đại học", "Đại học", "Cử nhân", "Thạc sĩ", "Tiến sĩ") )
  })
  
  output$plot5<-renderPlot({
    ggplot(general1,aes(x=Attrition,group=Gender))+
      geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
      facet_grid(~Gender)+
      theme(axis.text.x=element_text(angle=90,vjust=0.5),legend.position="none",plot.title=element_text(size=16,hjust=0.5))+
      labs(x="Nhân viên rời công ty?",y="Nhân viên",title="Giới tính nhân viên rời bỏ công ty")+
      scale_y_continuous(labels=scales::percent) +
      geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ),stat= "count",vjust =-.5) +
      scale_fill_brewer(palette="Paired")
     
  })
  
  output$plot6<-renderPlot({
    ggplot(general1,aes(x=Attrition,group=MaritalStatus))+
      geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
      facet_grid(~MaritalStatus)+
      theme(axis.text.x=element_text(angle=90,vjust=0.5),legend.position="none",plot.title=element_text(size=16,hjust=0.5))+
      labs(x="Nhân viên rời công ty?",y="%",title="Tình trạng hôn nhân %")+
      geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ),stat= "count",vjust =-.5) +
      scale_fill_brewer(palette="PiYG")
   })
  
  output$plot11<-renderPlot({
    ggplot(general1, aes(TotalWorkingYears, color=Attrition, fill=Attrition)) +
      geom_density() +
      labs(title = "Tổng số năm làm việc")+
      scale_fill_brewer(palette="Dark2")
   })
  
  output$plot12<-renderPlot({
    ggplot(general1, aes(x=Attrition, YearsSinceLastPromotion, color=Attrition)) +
      geom_boxplot() +
      scale_color_manual(values=c( "#2171B5","#CB181D"))
  })
  
  output$plot16<-renderPlot({
    ggplot(general1, aes(YearsWithCurrManager, color=Attrition, fill=Attrition)) +
      geom_density() +
      labs(title = "Số năm nhân viên làm việc với người quản lý hiện tại")+
      scale_fill_brewer(palette="Set3")
  })
  
  output$plot14<-renderPlot({
    ggplot(general1,aes(x=Attrition,group=NumCompaniesWorked))+
      geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
      facet_grid(~NumCompaniesWorked)+
      theme(axis.text.x=element_text(angle=90,vjust=0.5),legend.position="none",plot.title=element_text(size=16,hjust=0.5))+
      labs(x="Quyết định rời công ty",y="%",title="Tổng số công ty mà nhân viên đã làm việc %")+
      geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ),stat= "count",vjust =-.5) +
      scale_fill_brewer(palette="Paired")
    
  })

  output$plot13<-renderPlot({
    ggplot(general1, aes(MonthlyIncome, color=Attrition, fill=Attrition)) +
      geom_density() +
      labs(title = "Mức thu nhập hàng tháng")+
      scale_fill_brewer(palette="GnBu")

  })
  
  # Khao sat
  output$plot7<-renderPlot({
    ggplot(survey_eplot, aes(factor(Satisfaction), fill=factor(value))) +
      theme(plot.title=element_text(size=16,hjust=0.5))+
      geom_bar(position = "dodge", aes(y=(..count..)/sum(..count..))) + 
      scale_y_continuous(labels=scales::percent) +
      labs(y ="tỷ lệ % ",x="Vấn đề khảo sát",title="Khảo sát mức độ hài lòng của nhân viên")+
      scale_fill_brewer(palette = "Paired")
  })
  
  output$plot8<-renderPlot({
    ggplot(general1,aes(x=EnvironmentSatisfaction,group=Attrition))+
      geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
      facet_grid(~Attrition)+
      scale_y_continuous(labels=scales::percent) +
      theme(axis.text.x=element_text(angle=90,vjust=0.5),legend.position="none",plot.title=element_text(size=16,hjust=0.5))+
      labs(y ="%",x="Quyết định rời công ty",title="Sự hài lòng về môi trường làm việc %")+
      geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ),stat= "count",vjust =-.5) +
      scale_fill_brewer(palette="Set1")
 })
  
  output$plot9<-renderPlot({
    ggplot(general1,aes(x=JobSatisfaction,group=Attrition))+
      geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
      facet_grid(~Attrition)+
      scale_y_continuous(labels=scales::percent) +
      theme(axis.text.x=element_text(angle=90,vjust=0.5),legend.position="none",plot.title=element_text(size=16,hjust=0.5))+
      labs(x="Quyết định rời công ty",y="%",title="Mức độ hài lòng với công việc được giao %")+
      geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ),stat= "count",vjust =-.5) +
      scale_fill_brewer(palette="Set1")
    
  })
  
  output$plot10<-renderPlot({
    ggplot(general1,aes(x=WorkLifeBalance,group=Attrition))+
      geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
      facet_grid(~Attrition)+
      scale_y_continuous(labels=scales::percent) +
      theme(axis.text.x=element_text(angle=90,vjust=0.5),legend.position="none",plot.title=element_text(size=16,hjust=0.5))+
      labs(x="Quyết định rời công ty",y="%",title="Cân bằng giữa cuộc sống và công việc %")+
      geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ),stat= "count",vjust =-.5) +
      scale_fill_brewer(palette="Set1")
   })
  
  output$plot17<-renderPlot({
    ggplot(general1,aes(x=RelationshipSatisfaction,group=Attrition))+
      geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
      facet_grid(~Attrition)+
      scale_y_continuous(labels=scales::percent) +
      theme(axis.text.x=element_text(angle=90,vjust=0.5),legend.position="none",plot.title=element_text(size=16,hjust=0.5))+
      labs(x="Quyết định rời công ty",y="%",title="Các mối quan hệ với đồng nghiệp %")+
      geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ),stat= "count",vjust =-.5) +
      scale_fill_brewer(palette="Set1")
    
    
  })

  ### Phan tich  
  # su tuong quan cac bien
  output$plot15<-renderPlot({
    corrplot(corr.matrix, method = "number")
  })
  
  # Mo hinh 1
  # Train
  output$plot18<-renderPlot({
    
    feature_importance
   })
  # Test
  output$plot19<-renderPlot({
    ggplot(data =  conf_ibm, mapping = aes(x = predictions, y = Var1)) +
      geom_tile(aes(fill = Freq), colour = "white") +
      geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1) +
      scale_fill_gradient(low = "deepskyblue", high = "deepskyblue4") +
      theme_minimal() + theme(plot.title = element_text(size=16,hjust = 0.5))+
      labs(title="Confusion Matrix", y="Attrition Status", x="Predictions")
  })

  # Mo hinh 2 : Logistic
  output$plot20<-renderPlot({
    troc1=roc(response=Trainingmodel1$y,predictor = Trainingmodel1$fitted.values,plot=T)
  })
  
  output$plot21<-renderPlot({
    troc2=roc(response=Trainingmodel2$y,predictor = Trainingmodel2$fitted.values,plot=T)
  })
  
  output$plot22<-renderPlot({
    troc3=roc(response=Trainingmodel3$y,predictor = Trainingmodel3$fitted.values,plot=T)
  })
  
  output$plot23<-renderPlot({
    troc4=roc(response=Trainingmodel4$y,predictor = Trainingmodel4$fitted.values,plot=T)
  })
  
  output$plot24<-renderPlot({
    VIM::aggr(IBM)
  })

  # summary data
  output$mysummary = renderPrint({
    
    summary(data[, input$data])
  })
  output$myplot1  = renderPlot({
    hist(data[, input$data], breaks=100, col="green", xlab="x", main="Histogram")
    
  })
  output$myplot2  = renderPlot({
    boxplot(data[, input$data], main = "Boxplot")
  })
  
}

shinyApp(ui,server) 
  
