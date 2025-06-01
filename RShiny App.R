library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(janitor)
library(readxl)
library(shinythemes)

file_path<-"C:/Users/lenovo/OneDrive/Desktop/206-project/salaries_data.xlsx"
sheet_names<-excel_sheets(file_path)
sheet1<-read_excel(file_path, sheet = sheet_names[1])
sheet2<-read_excel(file_path, sheet = sheet_names[2])
sheet3<-read_excel(file_path, sheet = sheet_names[3])
sheet4<-read_excel(file_path, sheet = sheet_names[4])
sheet1<-clean_names(sheet1)
sheet2<-clean_names(sheet2)
sheet3<-clean_names(sheet3)
sheet4<-clean_names(sheet4)
colnames(sheet1)<-tolower(colnames(sheet1))
colnames(sheet2)<-tolower(colnames(sheet2))
colnames(sheet3)<-tolower(colnames(sheet3))
colnames(sheet4)<-tolower(colnames(sheet4))
sheet3<-sheet3 %>% rename("experience_level" = "experience")
full_data<-bind_rows(sheet1, sheet2, sheet3, sheet4)
full_data<-na.omit(full_data)


full_data<-full_data %>% 
  mutate(salary_in_pkr=salary_in_usd*285)

ui<-fluidPage(theme=shinytheme("cyborg"),
                titlePanel("206 Project-Dashboard"),
                sidebarLayout(
                  sidebarPanel(h4("Display options"),
                               sliderInput("num_rows", "Number of rows to display in table:", min = 1, max = 20, value = 10, step = 1),
                               selectInput("selected_job", "Select job title for bar chart ('Company_size' vs 'salary'):",
                                           choices = unique(full_data$job_title), selected = unique(full_data$job_title)[1]),
                               selectInput("selected_residence", "Select residence for salary comparison:",
                                           choices = unique(full_data$employee_residence), selected = unique(full_data$employee_residence)[1]),
                               h4("convert salary to PKR"),
                               numericInput("salary_input", "Enter the salary in USD:", value = 1, min = 0),
                               verbatimTextOutput("converted_salary")),
                  mainPanel(tabsetPanel(tabPanel("Table", DTOutput("summary_table")),
                            tabPanel("Company Size vs Salary", plotOutput("company_size_plot")),
                            tabPanel("Salary Comparison by Residence", plotOutput("residence_salary_plot"))))))

                
server<-function(input,output){
  
#pg1: summary_table 
output$summary_table<-renderDT({
  summary_table<-full_data%>%
    group_by(job_title)%>%
    summarise(avg_salary_usd=round(mean(salary_in_usd,na.rm=T),3),
                sd_salary_usd=round(sd(salary_in_usd,na.rm=T),3),
                avg_remote_ratio=round(mean(remote_ratio,na.rm=T),2),
                sd_remote_ratio=round(sd(remote_ratio,na.rm=T),3))%>%
    arrange(desc(avg_salary_usd))
    colnames(summary_table)<-c("Job Title", "Avg Salary USD", "SD Salary USD", "Avg Remote Ratio", "SD Remote Ratio")
    datatable(head(summary_table, input$num_rows), options = list(pageLength = 10))})
  
#pg2: barplot for avg salary and company size
output$company_size_plot<-renderPlot({
  filtered_data<-full_data %>%
    filter(job_title==input$selected_job)
  ggplot(filtered_data, aes(x = company_size, y=salary_in_usd, fill=company_size))+
    geom_bar(stat = "summary", width = 0.8)+
      labs(title = paste("Average salary by company size", input$selected_job),
           x = "company size", y = "average salary in USD")+theme_minimal()})
  
  
#pg3: barplot for salary in different employee residents
output$residence_salary_plot<-renderPlot({
  filtered_data<-full_data %>%
    filter(employee_residence==input$selected_residence)
  ggplot(filtered_data, aes(x=employee_residence, y=salary_in_usd, fill=employee_residence))+
    geom_bar(stat = "summary", width = 0.4) +
    labs(title = paste("Average salary for different resident",input$selected_residence),
         x="employee residence", y="average salary in USD")+theme_minimal()})
  
  
#$ to pkr conversion
output$converted_salary<-renderText({
  if (is.numeric(input$salary_input)&&input$salary_input>= 0){
    converted_value<-input$salary_input*285
    paste("salary in PKR:", round(converted_value,2))} 
  else{"kindly enter a valid salary amount in USD."}})}


shinyApp(ui = ui, server = server)