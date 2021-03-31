#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(jsonlite)
library(dplyr)
library(DT)


#import trained model
githubURL <- ("https://raw.githubusercontent.com/patlittle/Ryerson-Big-Data-Analytics-Final-Project/master/final-report/saved_model.Rds")
download.file(githubURL,"trained_model.Rds", method="curl")
trained_model <- readRDS("trained_model.Rds")

#import json for 10 newest datasets
API_call<-("https://open.canada.ca/data/api/action/package_search?q=&sort=metadata_created+desc&fq=collection:primary")
lines<-fromJSON(API_call)
result<-lines[['result']]
data<-result[['results']]

ID<-data$id[1]
org<-data$organization$name[1]
desc<-as.numeric(length(unlist(strsplit(data$notes[1]," "))))
collection<-data$collection[1]
freq<-data$frequency[1]
jurisdiction<-data$jurisdiction[1]
key1<-data$keywords$en[[1]][1]
key2<-data$keywords$en[[1]][2]
key3<-data$keywords$en[[1]][3]
num_keys<-as.numeric(length(data$keywords$en[1]))
num_res<-as.numeric(data$num_resources[1])
subj1<-data$subject[[1]][1]
subj2<-data$subject[[1]][2]
subj3<-data$subject[[1]][3]
subj4<-data$subject[[1]][4]
date_created<-data$metadata_created[1]
date_last_mod<-data$metadata_modified[1]
datadata<-data.frame(ID,org,desc,collection,freq,jurisdiction,key1,key2,key3,num_keys,num_res,subj1,subj2,subj3,subj4,date_created,date_last_mod)
names(datadata)<-c("ID","org","desc","collection","freq","jurisdiction","key1","key2","key3","num_keys","num_res","subj1","subj2","subj3","subj4","date_created","date_last_mod")

for(i in 2:10){ #loop over this for each line of json - except the 1st line
    ID<-data$id[i]
    org<-data$organization$name[i]
    desc<-as.numeric(length(unlist(strsplit(data$notes[i]," "))))
    collection<-data$collection[i]
    freq<-data$frequency[i]
    jurisdiction<-data$jurisdiction[i]
    key1<-data$keywords$en[[i]][1]
    key2<-data$keywords$en[[i]][2]
    key3<-data$keywords$en[[i]][3]
    num_keys<-as.numeric(length(data$keywords$en[i]))
    num_res<-as.numeric(data$num_resources[i])
    subj1<-data$subject[[i]][1]
    subj2<-data$subject[[i]][2]
    subj3<-data$subject[[i]][3]
    subj4<-data$subject[[i]][4]
    date_created<-data$metadata_created[i]
    date_last_mod<-data$metadata_modified[i]
 
    datadata<- datadata %>% add_row(ID,org,desc,collection,freq,jurisdiction,key1,key2,key3,num_keys,num_res,subj1,subj2,subj3,subj4,date_created,date_last_mod)
}



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Predict Popularity of Datasets"),

    
    # Show a plot of the generated distribution
        mainPanel(
           DT::dataTableOutput(outputId = "new10")
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {

output$new10<-renderDataTable({datadata})    

    }

# Run the application 
shinyApp(ui = ui, server = server)
