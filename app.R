library(shiny)
library(tidyverse)
options(scipen=999)
ui <- fluidPage(
  titlePanel(title="",windowTitle = "Property Investment Calculator"),
  tags$head(tags$link(rel="shortcut icon",href="favicon.ico")),
  fixedRow(wellPanel(fixedRow(column(6,tags$h1(tags$strong("Property Investment Calculator"))),column(1,offset=4,tags$img(src="logo.png",width="160px",height="160px"))),
            fixedRow(column(2,tags$em("Jasper Wiblin © 2022"))))),
  tags$hr(),
  tags$h5(tags$strong("Calculator Notes:")),
  tags$ul(tags$li("Enter raw values, without symbols."),
          tags$li("An error will occur if the investment's yield is negative."),
          tags$li("The maximum value of HMRC's 'Property Allowance' is £1000."),
          tags$li("All profits and yields calculated follow UK taxation law and include", tags$a(href="https://www.which.co.uk/money/tax/income-tax/tax-on-property-and-rental-income/buy-to-let-mortgage-tax-relief-changes-explained-atnsv0j6j782","mortgage interest tax relief.",target="_blank"))),
  tags$hr(),
  fluidRow(column(3,numericInput("price","House Price (£)",value=NULL)),
           column(3,sliderInput("deposit","Deposit (%)",value=0,0,100)),
           column(3,sliderInput("term","Mortgage Term (Years)",value=1,1,45)),
           column(3,numericInput("apr","Your APR %",value=NULL))),
  fluidRow(column(3,numericInput("income","Monthly Rental Income (£)",value=NULL)),
           column(3,sliderInput("increase","Predicted Rental Increase (Annual) %",value=0,0,20)),
           column(3,numericInput("tax","Marginal Tax Bracket %",value=NULL,max=100)),
           column(3,numericInput("taxsave","Remaining HMRC 'Property Allowance' (£)",value=NULL,min=0,max=1000))),
  fluidRow(column(3,selectInput("type","Mortgage Type",c("Capital Repayment"="cr", "Interest Only"="int")))),
  actionButton("button","Apply"),
  tags$hr(), 
  tabsetPanel(tabPanel("Return on Investment",fluidRow(column(3,tags$h4(textOutput("loanamount"))),
           column(3,tags$h4(textOutput("mortgagepayments")))),
  fluidRow(column(3,tags$h4(textOutput("month"))),
           column(3,tags$h4(textOutput("year"))))),
  tabPanel("Full Breakdown",tags$em("If the full title/table is not shown, scroll left/right on the object."),splitLayout(tags$h4(tags$strong("Mortgage Breakdown:")),tags$h4(tags$strong("Annual Profit Breakdown:"))),splitLayout(tableOutput("amortisation"),
              tableOutput("profit"))),
  tabPanel("About/Help",tags$h4(tags$strong(tags$u("Computation"))),tags$p("This calculator takes inputs to display mutliple figures including yields and mortgage breakdowns.
                                             This is achieved intially by calculating the mortgage that is associated with the investment property,
                                             it will breakdown the mortgage repayments into capital and interest components which allows it to accurately deduct any tax paid from
                                             the rental income; meaning more accurate yields and profit forcasts."),tags$p("The calculator follows UK tax law and as mentioned
                                             above mortgage interest tax reilef is automatically included in all profit and yield figues. This is in addition to the inital £1000 tax
                                             break that is given to investors by HMRC."),tags$p("The annual profit table is calculated as follows: Annual Rental Income - Mortgage Payments - Tax Paid + Tax Relief"),tags$h4(tags$strong(tags$u("Getting an error? / Can't see certain elements?"))),tags$p("An error will occur if not 
                                             all sections are filled out, or if there is no/negative yield on the investment. If a capital repayment mortgage is selected then you might find that switching to an interest only mortgage produces positive yields.")
                                             ,tags$h4(tags$strong(tags$u("Any Feedback"))),tags$p("All feedback and improvements are very welcome 
                                             so please E-mail all suggestions to:",tags$a(rel="E-mail",href="mailto:jasperwiblin@gmail.com","jasperwiblin@gmail.com")),tags$p("Alternatively, you can
                                             connect with us via the calculator's instagram page:", tags$a(rel="instagram",href="https://www.instagram.com/property_investment_calculator/","@property_investment_calculator",target="_blank")))),
  tags$hr()
  
)

server <- function(input, output){
  
  observeEvent(input$button,{switch(input$type, cr ={ 
    output$loanamount <- renderText({
      L0 <- isolate(input$price) - ((isolate(input$deposit))/100)*isolate(input$price)
      paste(sep='',"Mortgage Loan Amount: £",L0)
    })
    output$mortgagepayments <- renderText({
      intrate <- ((isolate(input$apr))/(100*12))
      L0 <- isolate(input$price) - ((isolate(input$deposit))/100)*isolate(input$price) #need to define L0 in every output render
      bottomdiscount <- L0*intrate
      topdiscount <- 1- 1/(1+intrate)^(12*isolate((input$term)))
      mortgagepayments <- bottomdiscount/topdiscount
      paste(sep='',"Monthly Mortgage Payments: £",round(mortgagepayments,2))
    })
    output$amortisation <- renderTable({
      L0 <- isolate(input$price) - ((isolate(input$deposit))/100)*isolate(input$price)
      months <- 12*isolate(input$term)
      intrate <- ((isolate(input$apr))/(100*12))
      bottomdiscount <- L0*intrate
      topdiscount <- 1- 1/(1+intrate)^(12*isolate((input$term)))
      mortgagepayments <- bottomdiscount/topdiscount
      loanoutstanding <- intpart <- cappart <- balance <- year <- rep(0, months)
      for (j in 1:months){
        loanoutstanding[j] <- ifelse(j==1, L0, balance[j-1])
        intpart[j] <- loanoutstanding[j]*intrate
        cappart[j] <- mortgagepayments - intpart[j]
        balance[j] <- loanoutstanding[j] - cappart[j]
      }
      amor <- data.frame('Loan Outstanding'=round(loanoutstanding,2), 'Interest Paid'= round(cumsum(intpart),2),
                     'Capital Paid'= round(cumsum(cappart),2), 'Balance Remaining'= round(balance,2)) #data frame so it can be easily subsetted below, the cappart and intpart here are not needed now
      year2 <- seq(1,isolate(input$term))
      amor2 <- tibble(Year=year2, 'Loan Outstanding (Start)'=amor[seq(1,nrow(amor),12),1],'Capital Paid'=amor[seq(1,nrow(amor),12),1]-amor[seq(0,nrow(amor),12),4],
                      'Interest Paid'=mortgagepayments*12 -(amor[seq(1,nrow(amor),12),1]-amor[seq(0,nrow(amor),12),4]),'Balance Remaining (End)'=amor[seq(0,nrow(amor),12),4])
      amor2 
    })
    output$profit <- renderTable({
      taxdeduction <- (isolate((input$taxsave))/12)
      taxrate<- 1-((isolate(input$tax))/100) 
      rentalamount <- rentalamount2 <- rentalamount3 <- rentalamount4 <- mthlyprofit <- year<- rep(0,times=12*isolate(input$term))
      nummonths <- 12*isolate(input$term)
      L0 <- isolate(input$price) - ((isolate(input$deposit))/100)*isolate(input$price)
      intrate <- ((isolate(input$apr))/(100*12))
      bottomdiscount <- L0*intrate
      topdiscount <- 1- 1/(1+intrate)^(12*(isolate(input$term)))
      mortgagepayments <- bottomdiscount/topdiscount
      
      loanoutstanding <- intpart <- cappart <- balance <- year <- rep(0, nummonths)
      for (j in 1:nummonths){
        loanoutstanding[j] <- ifelse(j==1, L0, balance[j-1])
        intpart[j] <- loanoutstanding[j]*intrate
        cappart[j] <- mortgagepayments - intpart[j]
        balance[j] <- loanoutstanding[j] - cappart[j]
      }
      
      for(i in 1:nummonths){
        year[i] <- floor((i-1)/12)
        rentalamount[i]<- ((isolate(input$income))*(((isolate(input$increase))/100)+1)^year[i])
        rentalamount2[i] <- rentalamount[i]*taxrate 
        rentalamount3[i] <- rentalamount2[i]- ifelse(rentalamount[i]>=taxdeduction,taxdeduction,rentalamount[i])*taxrate
        rentalamount4[i] <- rentalamount3[i] + ifelse(rentalamount[i]>=taxdeduction,taxdeduction,rentalamount[i])
        mthlyprofit[i] <- rentalamount4[i] + ifelse(rentalamount[i]-rentalamount4[i]>0.2*intpart[i],0.2*intpart[i],rentalamount[i]-rentalamount4[i]) - mortgagepayments
        #mortgage payments are not tax deductible, tax credit on 20% of interest
      }
      mthlyprofittable <- data.frame(mthlyprofit)
      yearlyprofit <- mthlyprofittable[c(rep(FALSE, 11),TRUE),]*12
      year2 <- seq(1,isolate(input$term))
      tibble(Year=year2,'Yearly Profit'=round(yearlyprofit,2))
    })
    output$month <- renderText({
      taxdeduction <- isolate((input$taxsave))/12
      taxrate<- 1-((isolate(input$tax))/100) 
      rentalamount <- rentalamount2 <- rentalamount3 <- rentalamount4 <- mthlyprofit <- year<- rep(0,times=12*isolate(input$term))
      nummonths <- 12*isolate(input$term)
      L0 <- isolate(input$price) - ((isolate(input$deposit))/100)*isolate(input$price)
      intrate <- ((isolate(input$apr))/(100*12))
      bottomdiscount <- L0*intrate
      topdiscount <- 1- 1/(1+intrate)^(12*(isolate(input$term)))
      mortgagepayments <- bottomdiscount/topdiscount
      
      loanoutstanding <- intpart <- cappart <- balance <- year <- rep(0, nummonths)
      for (j in 1:nummonths){
        loanoutstanding[j] <- ifelse(j==1, L0, balance[j-1])
        intpart[j] <- loanoutstanding[j]*intrate
        cappart[j] <- mortgagepayments - intpart[j]
        balance[j] <- loanoutstanding[j] - cappart[j]
      }
      
      for(i in 1:nummonths){
        year[i] <- floor((i-1)/12)
        rentalamount[i]<- ((isolate(input$income))*(((isolate(input$increase))/100)+1)^year[i])
        rentalamount2[i] <- rentalamount[i]*taxrate 
        rentalamount3[i] <- rentalamount2[i]- ifelse(rentalamount[i]>=taxdeduction,taxdeduction,rentalamount[i])*taxrate
        rentalamount4[i] <- rentalamount3[i] + ifelse(rentalamount[i]>=taxdeduction,taxdeduction,rentalamount[i])
        mthlyprofit[i] <- rentalamount4[i] + ifelse(rentalamount[i]-rentalamount4[i]>0.2*intpart[i],0.2*intpart[i],rentalamount[i]-rentalamount4[i]) - mortgagepayments
        #mortgage payments are not tax deductible, tax credit on 20% of interest
      }
      deposit <- ((isolate(input$deposit))/100)*isolate(input$price)
      f <- function(r){
        discountfactors <- (1+r)^{-(1:nummonths)}
        profitdiscount<- sum(mthlyprofit*discountfactors)
        return(deposit-profitdiscount)
      }
      mthlyyield<- uniroot(f,c(0,1),extendInt = "yes")
      ifelse((mthlyyield$root)>0,paste(sep='',"Monthly Yield: ",round(mthlyyield$root, 4),"%"),
             paste("Invalid Monthly Yield"))
    })
    output$year <- renderText({
      taxdeduction <- isolate((input$taxsave))/12
      taxrate<- 1-((isolate(input$tax))/100) 
      rentalamount <- rentalamount2 <- rentalamount3 <- rentalamount4 <- mthlyprofit <- year<- rep(0,times=12*isolate(input$term))
      nummonths <- 12*isolate(input$term)
      L0 <- isolate(input$price) - ((isolate(input$deposit))/100)*isolate(input$price)
      intrate <- ((isolate(input$apr))/(100*12))
      bottomdiscount <- L0*intrate
      topdiscount <- 1- 1/(1+intrate)^(12*(isolate(input$term)))
      mortgagepayments <- bottomdiscount/topdiscount
      
      loanoutstanding <- intpart <- cappart <- balance <- year <- rep(0, nummonths)
      for (j in 1:nummonths){
        loanoutstanding[j] <- ifelse(j==1, L0, balance[j-1])
        intpart[j] <- loanoutstanding[j]*intrate
        cappart[j] <- mortgagepayments - intpart[j]
        balance[j] <- loanoutstanding[j] - cappart[j]
      }
      
      for(i in 1:nummonths){
        year[i] <- floor((i-1)/12)
        rentalamount[i]<- ((isolate(input$income))*(((isolate(input$increase))/100)+1)^year[i])
        rentalamount2[i] <- rentalamount[i]*taxrate 
        rentalamount3[i] <- rentalamount2[i]- ifelse(rentalamount[i]>=taxdeduction,taxdeduction,rentalamount[i])*taxrate
        rentalamount4[i] <- rentalamount3[i] + ifelse(rentalamount[i]>=taxdeduction,taxdeduction,rentalamount[i])
        mthlyprofit[i] <- rentalamount4[i] + ifelse(rentalamount[i]-rentalamount4[i]>0.2*intpart[i],0.2*intpart[i],rentalamount[i]-rentalamount4[i]) - mortgagepayments
        #mortgage payments are not tax deductible, tax credit on 20% of interest
      }
      deposit <- ((isolate(input$deposit))/100)*isolate(input$price)
      f <- function(r){
        discountfactors <- (1+r)^{-(1:nummonths)}
        profitdiscount<- sum(mthlyprofit*discountfactors)
        return(deposit-profitdiscount)
      }
      mthlyyield<- uniroot(f,c(0,1),extendInt = "yes")
      yearyield<- (((1+mthlyyield$root)^12)-1)
      percentageyear <- (yearyield*100)
      rounded <- round(percentageyear, 2)
      ifelse(rounded>0, paste(sep='',"Annual Yield: ", rounded,"%"),paste("Invalid Annual Yield"))
    })}, int={ output$loanamount <- renderText({
      L0 <- isolate(input$price) - ((isolate(input$deposit))/100)*isolate(input$price)
      intrate <- ((isolate(input$apr))/(100*12))
      paste(sep='',"Total Interest to Pay: £",L0*intrate*isolate(input$term)*12)
    })
    output$mortgagepayments <- renderText({
      intrate <- ((isolate(input$apr))/(100*12))
      L0 <- isolate(input$price) - ((isolate(input$deposit))/100)*isolate(input$price) #need to define L0 in every output render
      mortgagepayments <- L0*intrate
      paste(sep='',"Monthly Mortgage Payments: £",round(mortgagepayments,2))
    })
    output$amortisation <- renderTable({
      L0 <- isolate(input$price) - ((isolate(input$deposit))/100)*isolate(input$price)
      months <- 12*isolate(input$term)
      intrate <- ((isolate(input$apr))/(100*12))
      mortgagepayments <- L0*intrate
      loanoutstanding <- intpart <- cappart <- balance <- year <- rep(0, months)
      for (j in 1:months){
        loanoutstanding[j] <- ifelse(j==1, L0, balance[j-1])
        intpart[j] <- loanoutstanding[j]*intrate
        cappart[j] <- 0
        balance[j] <- loanoutstanding[j] - cappart[j]
      }
      amor <- data.frame('Loan Outstanding'=round(loanoutstanding,2), 'Interest Paid'= round(cumsum(intpart),2),
                     'Capital Paid'= round(cumsum(cappart),2), 'Balance Remaining'= round(balance,2))
      year2 <- seq(1,isolate(input$term))
      amor2 <- tibble(Year=year2, 'Loan Outstanding (Start)'=amor[seq(1,nrow(amor),12),1],'Capital Paid'=amor[seq(1,nrow(amor),12),1]-amor[seq(0,nrow(amor),12),4],
                      'Interest Paid'=mortgagepayments*12 -(amor[seq(1,nrow(amor),12),1]-amor[seq(0,nrow(amor),12),4]),'Balance Remaining (End)'=amor[seq(0,nrow(amor),12),4])
      amor2 
    })
    output$profit <- renderTable({
      taxdeduction <- isolate((input$taxsave))/12
      taxrate<- 1-((isolate(input$tax))/100) 
      rentalamount <- rentalamount2 <- rentalamount3 <-rentalamount4 <- mthlyprofit <- year<- rep(0,times=12*isolate(input$term))
      nummonths <- 12*isolate(input$term)
      L0 <- isolate(input$price) - ((isolate(input$deposit))/100)*isolate(input$price)
      intrate <- ((isolate(input$apr))/(100*12))
      mortgagepayments <- L0*intrate
      for(i in 1:nummonths){
        year[i] <- floor((i-1)/12)
        rentalamount[i]<- ((isolate(input$income))*(((isolate(input$increase))/100)+1)^year[i])
        rentalamount2[i] <- rentalamount[i]*taxrate 
        rentalamount3[i] <- rentalamount2[i]- ifelse(rentalamount[i]>=taxdeduction,taxdeduction,rentalamount[i])*taxrate
        rentalamount4[i] <- rentalamount3[i] + ifelse(rentalamount[i]>=taxdeduction,taxdeduction,rentalamount[i])
        mthlyprofit[i] <- rentalamount4[i] + ifelse(rentalamount[i]-rentalamount4[i]>0.2*mortgagepayments, 0.2*mortgagepayments,rentalamount[i]-rentalamount4[i]) - mortgagepayments
        #mortgage payments are not tax deductible
      }
      mthlyprofittable <- data.frame(mthlyprofit)
      yearlyprofit <- mthlyprofittable[c(rep(FALSE, 11),TRUE),]*12
      year2 <- seq(1,isolate(input$term))
      tibble(Year=year2,'Yearly Profit'=round(yearlyprofit,2))
    })
    output$month <- renderText({
      taxdeduction <- isolate((input$taxsave))/12
      taxrate<- 1-((isolate(input$tax))/100) 
      rentalamount <- rentalamount2 <- rentalamount3 <-rentalamount4 <- mthlyprofit <- year<- rep(0,times=12*isolate(input$term))
      nummonths <- 12*isolate(input$term)
      L0 <- isolate(input$price) - ((isolate(input$deposit))/100)*isolate(input$price)
      intrate <- ((isolate(input$apr))/(100*12))
      mortgagepayments <- L0*intrate
      for(i in 1:nummonths){
        year[i] <- floor((i-1)/12)
        rentalamount[i]<- ((isolate(input$income))*(((isolate(input$increase))/100)+1)^year[i])
        rentalamount2[i] <- rentalamount[i]*taxrate 
        rentalamount3[i] <- rentalamount2[i]- ifelse(rentalamount[i]>=taxdeduction,taxdeduction,rentalamount[i])*taxrate
        rentalamount4[i] <- rentalamount3[i] + ifelse(rentalamount[i]>=taxdeduction,taxdeduction,rentalamount[i])
        mthlyprofit[i] <- rentalamount4[i] + ifelse(rentalamount[i]-rentalamount4[i]>0.2*mortgagepayments, 0.2*mortgagepayments,rentalamount[i]-rentalamount4[i]) - mortgagepayments
        #mortgage payments are not tax deductible
      }
      deposit <- ((isolate(input$deposit))/100)*isolate(input$price)
      f <- function(r){
        discountfactors <- (1+r)^{-(1:nummonths)}
        profitdiscount<- sum(mthlyprofit*discountfactors)
        return(deposit-profitdiscount)
      }
      mthlyyield<- uniroot(f,c(0,1),extendInt = "yes")
      ifelse((mthlyyield$root)>0,paste(sep='',"Monthly Yield: ",round(mthlyyield$root, 4),"%"),
             paste("Invalid Monthly Yield"))
    })
    output$year <- renderText({
      taxdeduction <- isolate((input$taxsave))/12
      taxrate<- 1-((isolate(input$tax))/100) 
      rentalamount <- rentalamount2 <- rentalamount3 <-rentalamount4 <- mthlyprofit <- year<- rep(0,times=12*isolate(input$term))
      nummonths <- 12*isolate(input$term)
      L0 <- isolate(input$price) - ((isolate(input$deposit))/100)*isolate(input$price)
      intrate <- ((isolate(input$apr))/(100*12))
      mortgagepayments <- L0*intrate
      for(i in 1:nummonths){
        year[i] <- floor((i-1)/12)
        rentalamount[i]<- ((isolate(input$income))*(((isolate(input$increase))/100)+1)^year[i])
        rentalamount2[i] <- rentalamount[i]*taxrate 
        rentalamount3[i] <- rentalamount2[i]- ifelse(rentalamount[i]>=taxdeduction,taxdeduction,rentalamount[i])*taxrate
        rentalamount4[i] <- rentalamount3[i] + ifelse(rentalamount[i]>=taxdeduction,taxdeduction,rentalamount[i])
        mthlyprofit[i] <- rentalamount4[i] + ifelse(rentalamount[i]-rentalamount4[i]>0.2*mortgagepayments, 0.2*mortgagepayments,rentalamount[i]-rentalamount4[i]) - mortgagepayments
        #mortgage payments are not tax deductible
      }
      deposit <- ((isolate(input$deposit))/100)*isolate(input$price)
      f <- function(r){
        discountfactors <- (1+r)^{-(1:nummonths)}
        profitdiscount<- sum(mthlyprofit*discountfactors)
        return(deposit-profitdiscount)
      }
      mthlyyield<- uniroot(f,c(0,1),extendInt = "yes")
      yearyield<- (((1+mthlyyield$root)^12)-1)
      percentageyear <- (yearyield*100)
      rounded <- round(percentageyear, 2)
      ifelse(rounded>0, paste(sep='',"Annual Yield: ", rounded,"%"),paste("Invalid Annual Yield"))
    })})})}

shinyApp(ui=ui,server=server)
#reactive
#isolate
#observeEvent
#observe
#I should have used eventReactive instead of observe event so I don't have to isolate everything individually