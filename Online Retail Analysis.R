#Prepare Data
library(tidyverse)
library(hms)
library(shiny)
library(plotly)
library(skimr)

#Prepare
data = read.csv('D:/Course/Project Data/Online Retail/Data Fix Sales/customer_shopping_data.csv')
glimpse(data)
str(data)

nrow(data)
ncol(data)

colnames(data)

is.data.frame(data)
# view(data)
#Ada tipe data yang tidak sesuai formatnya (Invoice date)

#Process

#1. Remove duplicated data
data <- data %>% distinct(invoice_no,customer_id,gender,age,category,quantity,price,payment_method,
                          invoice_date,shopping_mall,.keep_all = TRUE)
nrow(data)
#(No Duplicated data)

#2. Handling whitespace
data <-  data %>%
  mutate_if(is.character, str_trim)
glimpse(data)

#3. Handling Null values and empty string
colSums(is.na(data))
# view(data)

# data <- data %>% 
#   filter(Order.ID!="Order ID")
view(data)
#4. Change data types 
data$invoice_date <- dmy(data$invoice_date)
view(data)
data$invoice_date <- as.Date(data$invoice_date, format = "%d/%m/%y")
glimpse(data)
view(data)

column_name <- data %>% names() %>%
  str_to_title()

colnames(data) <- column_name
glimpse(data)

data <- data %>%
  filter(Invoice_date >= as.Date('2021-01-01'), Invoice_date <= as.Date('2022-12-31'))
#Extract month
data$Month <- month(data$Invoice_date)
data$Month <- as.character(data$Month)
data$Month_num <- as.numeric(data$Month)
#Extract year
data$Year <- year(data$Invoice_date)
clean_data <- data %>% mutate(Month= case_when(Month == "1" ~ "January",
                                                Month == "2" ~ "February",
                                                Month == "3" ~ "March",
                                                Month == "4" ~ "April",
                                                Month == "5" ~ "May",
                                                Month == "6" ~ "June",
                                                Month == "7" ~ "July",
                                                Month == "8" ~ "August",
                                                Month == "9" ~ "September",
                                                Month == "10" ~ "October",
                                                Month == "11" ~ "November",
                                                Month == "12" ~ "December",
                                                TRUE ~ Month))
clean_data$Age <- as.character(clean_data$Age)
write.csv(clean_data, 'D:/Course/Project Data/Online Retail/customer_shopping_data_clean.csv')
#Analyze data
glimpse(clean_data)

summary(clean_data)

#Tren produk
#category order
clean_data %>% group_by(Category) %>% 
  summarize(sumOrder = sum(Quantity)) %>%
  arrange(desc(sumOrder))

#order per year
print(n=30,clean_data %>% group_by(Year) %>%
  summarize(sumOrder = sum(Quantity)))
clean_data %>% group_by(Year,Month, Month_num) %>%
  summarize(sumOrder = sum(Quantity)) %>%
  arrange(Month_num)
clean_data %>% group_by(Year, Category) %>% 
  summarize(sumOrder = sum(Quantity)) %>%
  arrange(desc(sumOrder))
clean_data %>% group_by(Month, Category) %>% 
  summarize(sumOrder = sum(Quantity)) %>%
  arrange(desc(sumOrder))
#category price
clean_data %>% group_by(Category) %>% 
  summarize(avgPrice = mean(Price)) %>%
  arrange(desc(avgPrice))
clean_data %>% group_by(Year,Category) %>% 
  summarize(avgPrice = mean(Price)) %>%
  arrange(desc(avgPrice))
clean_data %>% group_by(Month,Month_num,Category) %>% 
  summarize(avgPrice = mean(Price)) %>%
  arrange(Month_num,desc(avgPrice))
#customer segmentation
#Gender
clean_data %>% group_by(Gender) %>% 
  summarize(total = n()) %>%
  mutate(percentage = paste(round((total / sum(total)*100)),"%"))

#Age
clean_data %>% group_by(Age) %>% 
  summarize(total = n()) %>%
  arrange(desc(total))

#pola beli konsumen
clean_data %>% group_by(Payment_method) %>% 
  summarize(total = n()) %>%
  mutate(percentage = paste(round((total / sum(total)*100)),"%"))
clean_data %>% group_by(Shopping_mall) %>% 
  summarize(total = n()) %>%
  mutate(percentage = paste(round((total / sum(total)*100)),"%")) %>%
  arrange(desc(total))
clean_data %>% group_by(Shopping_mall, Category) %>% 
  summarize(total = n()) %>%
  mutate(percentage = paste(round((total / sum(total)*100)),"%")) %>%
  arrange(desc(total))
clean_data %>% group_by(Year, Shopping_mall, Category) %>% 
  summarize(avgPrice = mean(Price)) %>%
  arrange(desc(avgPrice))
clean_data %>% group_by(Year, Month, Shopping_mall, Category) %>% 
  summarize(avgPrice = mean(Price)) %>%
  arrange(desc(avgPrice))

#Korelasi Price dan Quantity
cor(clean_data$Quantity, clean_data$Price) #rendah

#Visualize in tableau