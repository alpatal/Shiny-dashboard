## app.R ##
library(shinydashboard)
library(shiny)

library(rjstat)
library(plyr) 
library(zoo)
library(dplyr)
library(reshape2)
library(dygraphs)
library(forecast)
library(TSA)


cso.url <- "http://www.cso.ie/StatbankServices/StatbankServices.svc/jsonservice/responseinstance/HPM01"
cso.url2 <- "http://www.cso.ie/StatbankServices/StatbankServices.svc/jsonservice/responseinstance/FIM06"
cso.list <- fromJSONstat(cso.url)
cso.list2<- fromJSONstat(cso.url2)

# Housing -->Percentage Change over 12 months for Residential Property Price Index (%) 
#(Base Jan 2005 = 100)
Housing <- cso.list[[1]]
#Money Supply in Euro Milllon
MONEY_SUPPLY<- cso.list2[[1]]

#Transform date from 2005M1 to 2005-01-01
library(anytime)

anydate("1973M01")



#################################### HOUSING  #################################################
###############################################################################################

month=NULL
for (i in 1:4896){
  
  date<-anydate(Housing[i,2])
  
  month=rbind(month,date)
  
}
MONTH_YEAR<-as.data.frame(as.Date.numeric(month[,1]))


Housing<- cbind (Housing, MONTH_YEAR)

#Deleting original month data 
Housing <- subset( Housing, select = -c( 2 ))
colnames(Housing)<-c("Type_property","statistic","value","Month" )



#################################### MONEY SUPPLY ######################################
########################################################################################

MONEY_SUPPLY<- cso.list2[[1]]
month=NULL
for (i in 1:2690){
  
  date<-anydate(MONEY_SUPPLY[i,2])
  
  month=rbind(month,date)
  
}
MONTH_YEAR1<-as.data.frame(as.Date.numeric(month[,1]))


MONEY_SUPPLY<- cbind(MONEY_SUPPLY, MONTH_YEAR1)

#Deleting original month data 
MONEY_SUPPLY <- subset( MONEY_SUPPLY, select = -c( 2 ))
colnames(MONEY_SUPPLY)<-c("Selected Money Supply","statistic","value","Month" )

#subset with same time range as HOUSING. From 2005 to 2017

M_SUPPLY <- subset(MONEY_SUPPLY, Month > "2004-12-01" & Month < "2017-11-01")


# We need to transform the data from M_Supply into percentage change, Base Jan 2005=100
# to compare both data, housing and Money supply.


# From row 1 to 152

a<-filter(M_SUPPLY, M_SUPPLY$`Selected Money Supply`=="Official external reserves")

Month<- a$Month 

# two ways of doig the same
#library(delt)
#delt(M_SUPPLY$`Selected Money Supply`=="Official external reserves") 

#Change_12M=NULL
#for(i in 1:152){
#  Change= ((M_SUPPLY$value[(i+11),3]-M_SUPPLY$value[i,3])/M_SUPPLY$value[i,3])*100
#  Change_12M=rbind(Change_12M, Change)
#}

# Offical exteral reserves

OER<-subset(M_SUPPLY,M_SUPPLY$`Selected Money Supply`== "Official external reserves")

#Creating data frame of two columns, one named OER and the ther one Month

OER<-subset(OER, select=-c(1,2))
colnames(OER)<-c("OER", "Month")

# Money Supply M1

M1<-subset(M_SUPPLY, M_SUPPLY$`Selected Money Supply`== "Money supply M1 to Euro area")

which(M_SUPPLY$`Selected Money Supply`=="Money supply M1 to Euro area")
#From row 153 to 304

#Creating data frame of two columns, one named M1 and the ther one Month

M1<-subset(M1, select=-c(1,2))
colnames(M1)<-c("M1", "Month")

# Money Supply M2

M2<-subset(M_SUPPLY, M_SUPPLY$`Selected Money Supply`== "Money supply M2 to Euro area")

which(M_SUPPLY$`Selected Money Supply`=="Money supply M2 to Euro area")
#From row 305 to 456

#Creating data frame of two columns, one named M2 and the ther one Month

M2<-subset(M2, select=-c(1,2))
colnames(M2)<-c("M2", "Month")

# Money Supply M3

M3<-subset(M_SUPPLY, M_SUPPLY$`Selected Money Supply`== "Money supply M3 to Euro area")


which(M_SUPPLY$`Selected Money Supply`=="Money supply M3 to Euro area")
#From row 457to 608

#Creating data frame of two columns, one named M3 and the ther one Month

M3<-subset(M3, select=-c(1,2))
colnames(M3)<-c("M3", "Month")

# Private sector

PCredit<-subset(M_SUPPLY, M_SUPPLY$`Selected Money Supply`== "Private Sector credit")

which(M_SUPPLY$`Selected Money Supply`=="Private Sector credit")

# Creating a data frame of two colums, one named Private_Credit and he other Month

PCredit<-subset(PCredit, select=-c(1,2))
colnames(PCredit)<-c("Private_Credit", "Month")

#From row 609 to 760

# Puting togther all the different components of the Money Supply


MS<-cbind(OER,M1,M2,M3,PCredit)
MS<-MS[c(1,3,5,7,9,10)]

# Transforming values from Euro millions into an index=100 which reference starts
# the year 2005, Index Jan 2005=100.
## Creating an index base 1 January 2005 for Money Supply 
################################# BASE JAN 2005 MONEY SUPPLY    #############################
#############################################################################################

OER_BASE2005=NULL
for (i in 1:153){
  
  Base= round((MS[i,1]*100)/ MS[1,1],2)
  OER_BASE2005<- rbind(OER_BASE2005, Base)
}

M1_BASE2005=NULL
for (i in 1:153){
  
  Base= round((MS[i,2]*100)/ MS[1,2],2)
  M1_BASE2005<- rbind(M1_BASE2005, Base)
}

M2_BASE2005=NULL
for (i in 1:153){
  
  Base= round((MS[i,3]*100)/ MS[1,3],2)
  M2_BASE2005<- rbind(M2_BASE2005, Base)
}

M3_BASE2005=NULL
for (i in 1:153){
  
  Base= round((MS[i,4]*100)/ MS[1,4],2)
  M3_BASE2005<- rbind(M3_BASE2005, Base)
}

PC_BASE2005=NULL
for (i in 1:153){
  
  Base= round((MS[i,5]*100)/ MS[1,5],2)
  PC_BASE2005<- rbind(PC_BASE2005, Base)
}

BASES<-cbind(OER_BASE2005, M1_BASE2005, M2_BASE2005,M3_BASE2005, PC_BASE2005)
colnames(BASES)<-c("OER", "M1", "M2", "M3", "PC")

########  "National - all residential properties"############
Housing <- cso.list[[1]]

Nat_All_Base2005<-filter(Housing, Housing$`Type of Residential Property`=="National - all residential properties"
                         & Housing$Statistic=="Residential Property Price Index (Base Jan 2005 = 100)")

Nat_All_Base2005<-Nat_All_Base2005[,4] # Now is a numeric vector


############# Creating a data frame with all Monetary Supply components ###############################
###################        and all prop. types price index, base 2005 ###########################

BASE_2005<- cbind(Nat_All_Base2005, BASES)
ts<-ts(BASE_2005, start=c(2005,1), end = c(2017,9), frequency = 12)
postBuble<-BASE_2005[49:153,] #after the bubble collapse (2009-2017)
Bubble<- BASE_2005[1:48,] #During the bubble (2005-2008)

## Mean
meant<-NULL
for (i in 1:6) {
  
  me<-mean(BASE_2005[,i])
  meant<- rbind(meant, me)
}

## Standard deviation
strdev<-NULL
for (i in 1:6) {
  std<-sd(BASE_2005[,i])
  strdev<- rbind(strdev, std)
  
}

#computation of the standard error of the mean
## The standard error of the mean can provide a rough estimate of the interval 
##in which the population mean is likely to fall
stdErrorMean<-NULL
for (i in 1:6) {
  
  sem<-strdev[i]/sqrt(length(BASE_2005[,1]))
  stdErrorMean<-rbind(stdErrorMean, sem)
}

#95% confidence intervals of the mean
interval<-NULL
for (i in 1:6) {
  
  inter<-c(meant[i]-2*stdErrorMean[i],meant[i]+2*stdErrorMean[i])
  
  interval<-rbind(interval, inter)
  
}

#### matrix containing all data calculated above 

explo<- cbind (meant, strdev,stdErrorMean,interval)
colnames(explo)<-c("Mean", "SD", "SEM", "Low Interval 95%", "Upper interval 95%")
row.names(explo)<-c("Prop Price Index", "OER", "M1", "M2", "M3", "PC")

##### Skewness and Kurtosis
library(moments)
skurt<-NULL
for (i in 1:6){
  
  sk<-skewness(BASE_2005[,i])
  kt<-kurtosis(BASE_2005[,i])
  skt<-c(sk,kt)
  skurt<- rbind(skurt,skt)
  colnames(skurt)<-c("skewness", "kurtosis")
}
row.names(skurt)<-c("Prop Price Index", "OER", "M1", "M2", "M3", "PC")
as.data.frame(skurt)

### Best ARIMA model

lambda1 = BoxCox.lambda(postBubble[,1])  # -0.2033014
lambda5 = BoxCox.lambda(postBubble[,5])  ##  -0.9999242
#lambda6 = BoxCox.lambda(ts[,6])  #  0.897981

BoxPrices<-BoxCox(postBubble[,1], lambda1)
BoxM3<-BoxCox(postBubble[,5], lambda5)

BoxPrices<-ts(BoxPrices, start=c(2009,1), frequency=12)
BoxM3<-ts(BoxM3, start=c(2009,1), frequency=12)

## Making time series stationary 
tsdisplay(diff(BoxM3, differences = 1),main="M3=Broad Money (1 diff)")
tsdisplay(diff(BoxPrices, differences = 2),main="Property Price Index (2 diff)")


# ARIMA models

fitM3_010<-Arima(BoxM3, order=c(0,1,0))  ### aic = -1332.99 vs AIC=-1330.99
fitPrices227<-Arima(BoxPrices, order=c(2,2,7)) ### aic = -850.5  vs AIC=-848.5

## Residuals

lag.plot(fitM3_010$residuals, lags = 1, main = "lag. plot from ARIMA(0,1,0) M3=Broad Money")
lag.plot(fitPrices227$residuals, lags=1, main = "lag. plot from ARIMA(2,2,7) Prop. Price Index")

checkresiduals(fitM3_010, lag= 100)
checkresiduals(fitPrices227, lag=24)

###### Forecast

forM3<- forecast(fitM3_010,h=12 ,level= c(80,95))
forPPI<-forecast(fitPrices227, h=12,level= c(80,95))

plot(forM3, main= "Forecast M3= Broad Money")
plot(forPPI, main= "Forecast Property price index")


forM3<- forecast(fitM3_010,h=12 ,level= c(80,95))
forPPI<-forecast(fitPrices227, h=12,level= c(80,95))

plot(forM3, main= "Forecast M3=Broad Money")
plot(forPPI, main= "Forecast Property price index")

summary(forPPI)
forPPI$lower

forPPI$Forecast<-rbind(2.981903,2.984254,2.986555,2.988046,2.989695, 2.991566, 2.993704,
                       2.996084,2.998617,3.001182, 3.003658, 3.005956)
Prediction_PPI=InvBoxCox(forPPI$Forecast, lambda1)
ts(Prediction_PPI, start=c(2017, 8), frequency=12)

LowerPre_PPI<-InvBoxCox(forPPI$lower, lambda1)
UpperPre_PPI<-InvBoxCox(forPPI$upper, lambda1)

Pre_PPI<-cbind(LowerPre_PPI, Prediction_PPI, UpperPre_PPI)

Pre_PPI_ts<-ts(Pre_PPI, start=c(2009,1), frequency=12)





summary(forM3)
InvBoxCox(0.9943601, lambda5)
#### predicted value in euro mill
Prediction_M3<-rep((126580*175.0397)/100, 12)

as.data.frame(Prediction_M3)

LowerM3<-((InvBoxCox(forM3$lower, lambda5))*126580/100)
UpperM3<-((InvBoxCox(forM3$upper, lambda5))*126580/100)

Pre_M3<-cbind(LowerM3, Prediction_M3,UpperM3)
Pre_M3_ts<-ts(Pre_M3, start=c(2009,1), frequency=12)
##### Dashboard


ui <- dashboardPage(
  dashboardHeader(title = "Predicting P.Price"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Correlations", tabName = "Correlations", icon = icon("cc")),
      menuItem("Statistics", tabName = "Table", icon = icon("table")),
      menuItem("Stationary", tabName = "Stationary", icon = icon("line-chart")),
      menuItem("Arima", tabName="Arima", icon=icon("building")),
      menuItem("Forecast", tabName= "Forecast", icon=icon("money"))
    )
  ),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
        fluidRow(
          box(plotOutput("plot1", height = 250)),
          
          box(
            title = "Controls",
            
            
            selectInput("dataset", "Choose a dataset:", 
                        choices=c('Nat_All_Base2005', 'OER', "M1","M2","M3","PC"))
          )
        ),
        fluidRow(
          column(12,
    
               dygraph(ts, main = "Property price index and Monetary supply index", ylab="index") %>% dyRangeSelector()
               
               
          )
        )
      ),
      tabItem(tabName = "Correlations",
        fluidRow(
          column(12,
               plotOutput("correlations"),
               plotOutput("correlationsPost"),
               plotOutput("correlationsBubble")
          )     
        )
      ),
      tabItem(tabName = "Table",
        fluidRow(
          column(12,
                 tableOutput("statistics"),
                 tableOutput("kurtosis")
          )
        )
      ),
      tabItem(tabName = "Stationary",
              fluidRow(
                column(12,
                       plotOutput("broadMoney"),
                       plotOutput("Property")
          )
        )
         ),
      tabItem(tabName = "Arima",
              fluidRow(
                column(12,
                box(
                  title= "M3=Broad Money ARIMA model (0,1,0)",width = NULL,
                  plotOutput("ResidualsM3"),
                  plotOutput("CheckResidualsM3")
                )
                    ),
                column(12,
                box (
                title = "Property Price Index ARIMA model (2,2,7)",width = NULL,
                plotOutput("ResidualsPPI"),
                plotOutput("CheckResidualsPPI")
                      )
                )
              )
              ),
      tabItem(tabName = "Forecast",
              fluidRow(
                column(12,
              
                  plotOutput("ForecastM3")
                )
                ),
                fluidRow (
                  column(12,
              
                  plotOutput("ForecastPPI")
                )
              ),
              
              fluidRow(
                column(12,
                       box(
                         title = "M3=Broad Money Forecasted values. Euro Millions.
                         \n 09/2017 to 08/2018",width = NULL,
                         tableOutput("ValuesM3")
                       )
                )
                ),
              fluidRow(
                column(12,
                       box(
                         title = "Property Price Index Forecasted values.
                         \n 09/2017 to 08/2018",width = NULL,
                         tableOutput("ValuesPPI") 
                       )
                )
                )
              
      )
              #fluidRow(
                #column(12,
                       
                       #plotOutput<-("ForecastM3"),
                       #plotOutput<-("ForecastPPI")     
                     #) 
                      #)
                   #)
    )
  )
)


server <- function(input, output) {
 
  #histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    #BASE_2005 <- BASE_2005[seq_len(input$slider)]
    hist(ts[,input$dataset], main = colnames(ts)[input$dataset])
   
  })
  datasetInput <- reactive({
    switch(input$dataset,
           "Raw Data" = obatch,
           "Normalised Data - Pre QC" = eset.spike)
  })
  
  output$correlations <- renderPlot({pairs(~Nat_All_Base2005+OER+M1+M2+M3+PC,data=BASE_2005, 
                                    main="2005-2017")})
  output$correlationsPost<-renderPlot({pairs(~Nat_All_Base2005+OER+M1+M2+M3+PC,data=postBuble, 
                                             main="After the Bubble (2009-2017")})
  output$correlationsBubble<-renderPlot({pairs(~Nat_All_Base2005+OER+M1+M2+M3+PC,data=Bubble, 
                                               main="During the Bubble (2005-2009")})
  
  output$statistics <-renderTable(
                                {explo})
  output$kurtosis<-renderTable({skurt})
  output$broadMoney<-renderPlot({tsdisplay(diff(BoxM3, differences = 1),main="M3=Broad Money (1 diff)")})
  output$Property<-renderPlot({tsdisplay(diff(BoxPrices, differences = 2),main="Property Price Index (2 diff)")})
  output$ResidualsM3<-renderPlot({lag.plot(fitM3_010$residuals, lags = 1, main = "lag. plot from ARIMA(0,1,0) M3=Broad Money")})
  output$ResidualsPPI<-renderPlot({lag.plot(fitPrices227$residuals, lags=1, main = "lag. plot from ARIMA(2,2,7) Prop. Price Index")})
  output$CheckResidualsM3<-renderPlot({checkresiduals(fitM3_010, lag= 100)})
  output$CheckResidualsPPI<-renderPlot({checkresiduals(fitPrices227, lag=24)})
  output$ForecastM3<-renderPlot({plot(forM3, main= "Forecast M3=Broad Money")})
  output$ForecastPPI<-renderPlot({plot(forPPI, main= "Forecast Property price index")})
  output$ValuesPPI<-renderTable({Pre_PPI_ts})
  output$ValuesM3<-renderTable({Pre_M3_ts})
  
}


shinyApp(ui, server)

