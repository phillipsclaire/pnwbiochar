## Biochar Cost-Benefit Analysis Tool ##
# Adapted from a tool shared on soilquality.org.au
# developed by the Department of Agriculture and Food Western Australia
# the University of Western Australia and
# the Grains Research & Development Corporation

rm(list=ls())

library(shiny)
library(bsplus)
library(DT)

# Vars and Functions ####
Instructions<-{div(p(paste("Progress through the tabs in the sidebar to assess the potential financial benefits of applying biochar. ",
                            "Each tab requires you to estimate expenses and benefits associated with biochar.",
                            "Modify values to test the effect of changing assumptions.")),
                    p("This tool assumes that biochar is added once with benefits extending up to five years."),
                    p("This tool was adapted from a biochar calculator developed by the Australian Dept of Agriculture and Food."))}
CostHelp.Text1<-{div(p(paste("Biochar costs vary widely. Since biochar is typically priced by volume, ",
                      "keep in mind that 1 cubic yard is about 200 dry pounds. A cubic meter of biochar is about 110 dry kg.")),
            p(paste("A 2014 survey by the International Biochar Initiative found the average wholesale ",
              "price among 28 U.S. producers was $0.68/lb ($1360/ton), while the average retail price was $1.94/lb ($3872 per ton).")),
            p("In metric units, these equate to $1.50/kg ($1500/metric ton) for wholesale and $4.26/kg ($4260/metric ton) for retail."),
            hr(),
            strong("Reference"),
            p(paste("S. Jirka and T. Tomlinson (2015), '2014 State of the Biochar Industry: A Survey of Commercial Activity in the Biochar Sector',",
              "International Biochar Initiative report. (http://www.biochar-international.org/node/8367)")))}
CostHelp.Text2<-{div(p(paste("Default fertilizer prices shown here are average U.S. prices for 2008-2013.",
                        "Note that prices are reported here by nutrient mass, rather than fertilizer mass.")),
                p(paste("Default irrigation prices reflect average costs of off-farm water.",
                        "In 2013, average cost by state was: ID = $17.65, WA = $32.01, OR = $20.18 per acre-foot.")),
                hr(),
                strong("References"),
                p("USDA Economic Research Service (2013) 'Agricultural Prices', Table 7.  Average U.S. farm prices of selected fertilizers. (https://www.ers.usda.gov/data-products/fertilizer-use-and-price/)"),
                p("USDA Economic Research Service (2017) 'Irrigated Agriculture in the United States', Table 3-13.(https://www.ers.usda.gov/data-products/irrigated-agriculture-in-the-united-states/)")
)}
CropCost.US<-data.frame(Crop=c("Apples","Beans, green","Blackberries","Blueberries","Grapes","Hazelnuts","Hay, alfalfa","Hay, grass","Hops","Potato","Wheat"),
                           Yield=c(19.5,6.75,4.15,4.9,2.9,1.2,4.7,2.7,0.8,29.5,1.5),
                           Price=c(614,212,910,1800,2140,2700,167,173,10500,163,153))
colnames(CropCost.US)<-c("Crop","Yield (t/a)","Price ($/t)")
CropCost.Metric<-data.frame(Crop=CropCost.US$Crop,Yield=round(CropCost.US$Yield*0.446,1),Price=CropCost.US$Price*1.10)
colnames(CropCost.Metric)<-c("Crop","Yield (Metric ton/ha)","Price ($/metric ton)")
OffsetHelp.Text1<-{div(p("Biochar may initially tie-up nitrogen, increasing nitrogen fertilizer requirement."),
                p(paste("Biochar can be a source for P, K, S, Mg, and Ca, and may act as a liming agent.",
                "Use the Biochar Selection Tool to determine your crop's nutrient and lime requirements and amounts provided by different biochars."),
                p("Also use the Biochar Selection Tool to see if biochar may increase or decrease water retention in your soil.")))}
OffsetHelp.Text2<-{div(p("Biochar changes soil slowly. In may increase cation exchange capacity and alter soil biology and tilth gradually over several years."),
                      p("Modify the anticipated change in inputs for years 2-5 as a percentage of the year 1 change."),
                      p(paste("Example 1: You plan to add an additional 50 lbs/acre nitrogen with your biochar to maintain an appropriate C:N ratio. ",
                              "In subsequent years to plan to resume normal levels of nitrogen amendment.",
                              "Enter '50' for the first year in the row labeled 'Change in Input', and reduce the offset to 0 thereafter by entering '-100%' for years 2-5.")),
                      p(paste("Example 2: You estimate a one-time biochar application will reduce phosphorous requirement by 30 lbs/acre in the first year, ",
                        "but you expect diminishing returns in subsequent years as phosphorous is removed with the crop. ",
                        "Enter '-30' for the first year in the row labeled 'Change in Input', and reduce the offset by 10% a year by entering '-10%' for years 2-5.")))}
OffsetHelp.Text3<-{div(p(paste("Irrigation quantity is usually specified as a depth of uniformally applied water.",
                              "Irrigation cost, however, may be computed per unit volume.")),
                      tags$u("U.S."),
                      p("An inch of water applied over 1 acre is equivalent to 1/12 (or 0.083) acre-feet."),
                      p("1 acre-foot of water = 43,560 cubic feet = 325851.4 gallons"),
                      tags$u("Metric"),
                      p(HTML("A cm of water applied over a hectare is equivalent to 100m<sup>3</sup>.")))}
FutureDiscount.Text<-{div(p(paste("In economics, the value of future income is often discounted to represent the idea that ",
                                  "a dollar earned today is worth more than a dollar earned tomorrow. ",
                                  "This is because money earned today could be invested to generate interest and additional income over time. ")),
                          p(paste("Future income is discounted by the interest rate you could expect to earn if you were to receive that ",
                                  "income today and invest it in stocks, bonds, or some other venture. ",
                                  "The interest rate is used to calculate the 'net present value' of future income.")),
                          p(paste("The stock market has a return of about 7%, but a common estimate of 'risk-free' interest rate is only 2%, ",
                                  "based on the interest rate on a 10-year Treasury Inflation Protected Security (TIPS).")),
                          p(paste("For natural resource investments, an important consideration ",
                                  "is that a lack of investment today may actually result in degradation of the natural system. ",
                                  "Very low or even negative interest rates may be applied to account for this perspective.")))}
# Delta.N=50; Delta.P= -30; Delta.K= -10; Delta.Irr= -1;Delta.Lime= -0.5
# Adj.N= -100; Adj.P = -10; Adj.K = -10; Adj.Irr= -10; Adj.Lime = -10
# Cost.N=750; Cost.P=750;Cost.K=750;Cost.Irr=100;Cost.Lime=300
# Units<-"U.S."
CostsThroughTime<-function(Delta.N,Adj.N,Cost.N,Delta.P,Adj.P,Cost.P,Delta.K,Adj.K,Cost.K,
                           Delta.Lime,Adj.Lime,Cost.Lime,Delta.Irr,Adj.Irr,Cost.Irr,Units){
 InTable<-data.frame(Requirement=c("Nitrogen","Phosphorous","Potassium","Lime","Irrigation"),
                     Delta=c(Delta.N,Delta.P,Delta.K,Delta.Lime,Delta.Irr),
                     Adj=c(Adj.N,Adj.P,Adj.K,Adj.Lime,Adj.Irr),
                     Cost=c(Cost.N,Cost.P,Cost.K,Cost.Lime,Cost.Irr))
 if(Units=="U.S.") CostUnits<-c(2000,2000,2000,1,12) else CostUnits<-c(1000,1000,1000,1,0.01)
 AdjXTime<-(100+InTable$Adj*matrix(data=rep(0:4,each=length(InTable$Adj)),ncol=5))/100
 AdjXTime[AdjXTime<0]<-0
 AmountChange<-InTable$Delta*AdjXTime
 CostChange<-round(AmountChange*InTable$Cost/CostUnits,1)
 TotCostChange<-apply(CostChange,2,sum)
 OutTable<-data.frame(rbind(CostChange,TotCostChange),row.names=c(as.character(InTable$Requirement),"Total"))
 OutTable<-cbind(OutTable,c(0,0,0,0,0,1)) #Add a marker for styling the last row. Patch for wonky DT style function
 colnames(OutTable)<-c(paste("Year",1:5),"Marker")
 OutTable

}

# Temp<-CostsThroughTime(50,-100,750,-30,-10,750,-10,-10,750,-1,-10,100,-0.5,-10,300,"U.S.")
# Temp
#CostsThroughTime(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,"U.S.")
# SummaryTable<-function(input$BiocharCost,input$Transport,input$Application,input$BiocharRate,
#                        input$Units)
# UI ####
ui <- fluidPage(
        titlePanel("Biochar Cost-Benefit Analysis Tool"),
        navlistPanel(
        {tabPanel("Instructions",
                  h3("Instructions"),
                  Instructions,
                  selectInput("Units","Select Units for this Tool",choices=c("U.S.","Metric"),width='250px')
        )},
        {tabPanel("Biochar Costs",
                        h3("Biochar Costs"),
            numericInput("BiocharRate","Biochar Application Rate",value=1, width='250px'),
            fluidRow(column(width=2),
                  column(width=3,htmlOutput("UnitsBiochar1"))),
            fluidRow(column(width=2, strong("Biochar")),
                   column(width=3,numericInput("BiocharCost",NULL,value=1360)),
                   column(width=2,bs_modal(id="CostInfo1", title="Biochar Costs", body=CostHelp.Text1, size = "med"),
                          tags$button(type="button",class = "btn btn-default", icon("info-circle"))%>%
                            bs_attach_modal(id_modal="CostInfo1"))),
            fluidRow(column(width=2,strong("Transport")),
                   column(width=3,numericInput("TransportCost",NULL,value=25))),
            fluidRow(column(width=2,strong("Application")),
                  column(width=3,numericInput("ApplicationCost",NULL,value=25))),
            hr(),
            htmlOutput("BiocharCost.mass"),
            htmlOutput("BiocharCost.area")
          )},
        {tabPanel("Crop Value",
                  h3("Changes in Crop Margin"),
                  fluidRow(column(width=4,strong("On-farm Crop Price ($)"))),
                  fluidRow(column(width=4,numericInput("CropPrice",NULL,value=250)),
                           column(width=2,bs_modal(id="CropInfo", title="Typical PNW Crop Yields and Prices", body=dataTableOutput("TypicalCropCost"), size = "med"),
                                  tags$button(type="button",class = "btn btn-default", icon("info-circle"))%>%
                                    bs_attach_modal(id_modal="CropInfo"))),
                  fluidRow(column(width=4, numericInput("Yield","Average Yield",value=2))),
                  conditionalPanel(condition = "input.checkbox == false", 
                    numericInput("YieldChange","Expected Change in Yield (%)",value=10)),
                  checkboxInput("checkbox", label = "Vary change in yield over time?", value = FALSE),
                  conditionalPanel(condition = "input.checkbox == true", 
                    strong("Expected Change in Yield (%)"),
                    fluidRow(column(width=2,numericInput("Yr1.DYield","Year 1",value=0)),
                             column(width=2,numericInput("Yr2.DYield","Year 2",value=0)),
                             column(width=2,numericInput("Yr3.DYield","Year 3",value=0)),
                             column(width=2,numericInput("Yr4.DYield","Year 4",value=0)),
                             column(width=2,numericInput("Yr5.DYield","Year 5",value=0)))
                  ),
                  hr(),
                  htmlOutput("CropIncome"),
                  htmlOutput("CropIncomeChange")
        )},
        {tabPanel("Other Crop Inputs",
           h3("Changes in Other Crop Inputs"),
           p(paste("Here you can specify changes in fertilizer, lime, and irrigation costs",
                "anticipated from biochar amendment. This is an optional step.")),
           p("Other biochar benefits relating to tilth and soil health should be estimated through changes in crop yield (previous tab)."),
           hr(),
           
           fluidRow(column(width=2),
                    column(width=2,strong("Nitrogen")),
                    column(width=2,strong("Phosphorous")),
                    column(width=2,strong("Potassium")),
                    column(width=2,strong("Lime")),
                    column(width=2,strong("Irrigation"))),
           fluidRow(column(width=2,
                           bs_modal(id="CostInfo2", title="Cost of Inputs", body=CostHelp.Text2, size = "med"),
                           tags$button(type="button",class = "btn btn-default", icon("info-circle"))%>%
                             bs_attach_modal(id_modal="CostInfo2")),
                    column(width=2,htmlOutput("UnitsCostN")),
                    column(width=2,htmlOutput("UnitsCostP")),
                    column(width=2,htmlOutput("UnitsCostK")),
                    column(width=2,htmlOutput("UnitsCostLime")),
                    column(width=2,htmlOutput("UnitsCostIrr"))),
            fluidRow(column(width=2,strong("Input Costs")),
                    column(width=2,numericInput("CostN",NULL,value=1179)),
                    column(width=2,numericInput("CostP",NULL,value= 1467)),
                    column(width=2,numericInput("CostK",NULL,value= 1047)),
                    column(width=2,numericInput("CostLime",NULL,value= 60)),
                    column(width=2,numericInput("CostIrr",NULL,value= 20))),
           br(),br(),
           fluidRow(column(width=2),
                    column(width=2,strong("Nitrogen")),
                    column(width=2,strong("Phosphorous")),
                    column(width=2,strong("Potassium")),
                    column(width=2,strong("Lime")),
                    column(width=2,strong("Irrigation"))),
           fluidRow(column(width=2,
                           bs_modal(id="OffsetHelp1", title="Biochar Offsets of Other Inputs", body=OffsetHelp.Text1, size = "med"),
                           tags$button(type="button",class = "btn btn-default", icon("info-circle"))%>%
                             bs_attach_modal(id_modal="OffsetHelp1")),
                    column(width=2,htmlOutput("UnitsInputsN")),
                    column(width=2,htmlOutput("UnitsInputsP")),
                    column(width=2,htmlOutput("UnitsInputsK")),
                    column(width=2,htmlOutput("UnitsInputsLime")),
                    column(width=2,htmlOutput("UnitsInputsIrr"))),
           fluidRow(column(width=2, strong("Change in Input")),
                    column(width=2,numericInput("ChangeN.Yr1",NULL,value=50)),
                    column(width=2,numericInput("ChangeP.Yr1",NULL,value= -30)),
                    column(width=2,numericInput("ChangeK.Yr1",NULL,value= -10)),
                    column(width=2,numericInput("ChangeLime.Yr1",NULL,value= -0.5)),
                    column(width=2,numericInput("ChangeIrr.Yr1",NULL,value= 0))),
           
           checkboxInput("checkbox2", label = "Vary biochar benefits over time?", value = FALSE),

           conditionalPanel(condition = "input.checkbox2 == true", 
                            strong("Increase or decrease the change in inputs for years 2-5"),br(),br(),
                            fluidRow(column(width=2, bs_modal(id="OffsetHelp2", title="Change Biochar Offsets After Year 1", body=OffsetHelp.Text2, size = "med"),
                                            tags$button(type="button",class = "btn btn-default", icon("info-circle"))%>%
                                              bs_attach_modal(id_modal="OffsetHelp2")),
                                     column(width=2,strong("Nitrogen")),
                                     column(width=2,strong("Phosphorous")),
                                     column(width=2,strong("Potassium")),
                                     column(width=2,strong("Lime")),
                                     column(width=2,strong("Irrigation"))),
                            fluidRow(column(width=2, strong("Percent Difference")),
                                     column(width=2,numericInput("ChangeN.After",NULL,value= 0)),
                                     column(width=2,numericInput("ChangeP.After",NULL,value= 0)),
                                     column(width=2,numericInput("ChangeK.After",NULL,value= 0)),
                                     column(width=2,numericInput("ChangeLime.After",NULL,value= 0)),
                                     column(width=2,numericInput("ChangeIrr.After",NULL,value=0)))
           ),
        
        br(),
        ##Show table of annual offsets for years 1-5   
        actionButton(inputId="ShowInputChange",label="Calculate Change in Input Costs",style = "color: white; 
                     background-color: #00008B;"),
        htmlOutput("UnitsInputs6"),
        dataTableOutput("Offsets"),br(),br()
     )},
     {tabPanel("Results",
            h3("Impact of Biochar over 5 Years"),
            checkboxInput("applyinputs", label = "Include changes in other crop inputs?", value = FALSE),
            fluidRow(column(width=5,checkboxInput("applyfuturediscount", label = "Discount benefits in the future?", value = FALSE)),
                     column(width=2, bs_modal(id="FutureDiscount", title="Discount Future Value", body=FutureDiscount.Text, size = "med"),
                            tags$button(type="button",class = "btn btn-default", icon("info-circle"))%>%
                              bs_attach_modal(id_modal="FutureDiscount"))),br(),br(),
            conditionalPanel(condition = "input.applyfuturediscount == true", 
                             numericInput("InterestRate","Interest Rate (%)", value=2.0)),
            #tableOutput("Summary"),br(),
            fluidRow(column(dataTableOutput("BenefitSummary"), width = 10)),br(),
            em("Note: This table will only populate after you go through the previous tabs."),br()
          )}
        )
)

#server ####
server <- function(input, output, session) {
  UnitsText<-reactiveValues(mass="Nada",massabr="Nada",mass2="Nada",area="Nada",areaabr="Nada",waterdepth="Nada",watervol="Nada")
  GetUnits<-reactive({
    if(input$Units=="U.S.") {
      Mass<-"ton"
      Mass.abr<-"t"
      Mass2<-"lbs"
      Area<-"acre"
      Area.abr<-"a"
      WaterDepth<-"inches"
      WaterVol<-"acre-foot"} else {
      Mass<-"metric ton"
      Mass.abr<-"Mg"
      Mass2<-"Kg"
      Area<-"hectare"
      Area.abr<-"ha"
      WaterDepth<-"cm"
      WaterVol<-"m3"}
    return(list(Mass,Mass.abr,Mass2,Area,Area.abr,WaterDepth,WaterVol))
  })
  observeEvent(input$Units,{
    if(input$Units=="U.S.") {
      Temp1<-"Biochar Application Rate (t/a)"
      Temp2<-"On-farm Crop Price ($/t)"
      Temp3<-"Average Yield (t/a)"
      UnitsText$mass<-GetUnits()[[1]]
      UnitsText$massabr<-GetUnits()[[2]]
      UnitsText$mass2<-GetUnits()[[3]]
      UnitsText$area<-GetUnits()[[4]]
      UnitsText$areaabr<-GetUnits()[[5]]
      UnitsText$waterdepth<-GetUnits()[[6]]
      UnitsText$watervol<-GetUnits()[[7]]
      updateNumericInput(session,"BiocharRate",label=Temp1)
      updateNumericInput(session,"BiocharCost",value=1360)
      updateNumericInput(session,"CropPrice",label=Temp2)
      updateNumericInput(session,"Yield",label=Temp3)
      updateNumericInput(session,"CostN",value=1179)
      updateNumericInput(session,"CostP",value=1467)
      updateNumericInput(session,"CostK",value=1047)
      updateNumericInput(session,"CostLime",value=60)
      updateNumericInput(session,"CostIrr",value=20)}  else {
        Temp1<-"Biochar Application Rate (metric t/ha)"
        Temp2<-"On-farm Crop Price ($/metric t)"
        Temp3<-"Average Yield (metric t/ha)"
        UnitsText$mass<-GetUnits()[[1]]
        UnitsText$massabr<-GetUnits()[[2]]
        UnitsText$mass2<-GetUnits()[[3]]
        UnitsText$area<-GetUnits()[[4]]
        UnitsText$areaabr<-GetUnits()[[5]]
        UnitsText$waterdepth<-GetUnits()[[6]]
        UnitsText$watervol<-GetUnits()[[7]] 
        updateNumericInput(session,"BiocharRate",label=Temp1)
        updateNumericInput(session,"BiocharCost",value=1500)
        updateNumericInput(session,"CropPrice",label=Temp2)
        updateNumericInput(session,"Yield",label=Temp3)
        updateNumericInput(session,"CostN",value=1072)
        updateNumericInput(session,"CostP",value=1333)
        updateNumericInput(session,"CostK",value=952)
        updateNumericInput(session,"CostLime",value=55)
        updateNumericInput(session,"CostIrr",value=2.22)}
  })
  CostsOrValues<-reactiveValues(BCCost=rep(NA,5),CropIncomeAnn=rep(NA,5),CropIncomeTot=NA,InputValue=rep(NA,5),NetValue=rep(NA,5))
  
#Tab 1 ####
  BCCostAnn<-reactive({
    Cost<-sum(input$BiocharCost,input$Transport,input$Application)*input$BiocharRate
    Cost5Year<-c(Cost*-1,0,0,0,0)
    return(Cost5Year)
  })
  
  output$UnitsBiochar1<-renderUI({
    #UnitsText$mass<-GetUnits()[[1]]
    strong("$ per", UnitsText$mass)
  })
  
  output$BiocharCost.mass<-renderUI({
    CostsOrValues$BCCost=BCCostAnn()
    Cost<-sum(input$BiocharCost,input$Transport,input$Application)
    Temp<-paste("Total Cost ($/",UnitsText$mass,"): ",Cost, sep="")
   strong(Temp)
  })
  
  output$BiocharCost.area<-renderUI({
    #UnitsText$area<-GetUnits()[[4]]
    Cost<-sum(input$BiocharCost,input$Transport,input$Application)*input$BiocharRate
    Temp<-paste("Total Cost ($/",UnitsText$area,"): ",Cost,sep="")
    strong(Temp)
  })


  
  
#Tab 2####
  output$CropIncome<-renderUI({
    Income<-input$CropPrice*input$Yield*5
    Temp<-paste("Crop income over 5 years ($/",UnitsText$area,"): ",Income, sep="")
    strong(Temp)
  })

  CropValueChangeTot<-reactive({
    if(input$checkbox==FALSE) TotIncomeChange<-input$CropPrice*input$Yield*(input$YieldChange/100)*5 else {
    IncomeChange<-input$CropPrice*input$Yield*(c(input$Yr1.DYield,input$Yr2.DYield,input$Yr3.DYield,input$Yr4.DYield,input$Yr5.DYield)/100)
    TotIncomeChange<-sum(IncomeChange)}
    return(TotIncomeChange)
  })  
  
  CropValueChangeAnn<-reactive({
    if(input$checkbox==FALSE) IncomeChange<-rep(input$CropPrice*input$Yield*(input$YieldChange/100),5) else
      IncomeChange<-input$CropPrice*input$Yield*(c(input$Yr1.DYield,input$Yr2.DYield,input$Yr3.DYield,input$Yr4.DYield,input$Yr5.DYield)/100)
    return(IncomeChange)
  })  
  
  output$CropIncomeChange<-renderUI({
     CostsOrValues$CropIncomeAnn<-CropValueChangeAnn()
     CostsOrValues$CropIncomeTot<-CropValueChangeTot()
    Temp<-paste("Change in crop income over 5 years ($/",UnitsText$area,"): ",CostsOrValues$CropIncomeTot, sep="")
    strong(Temp)  
  })
  
  output$TypicalCropCost<-renderDataTable({
    if(input$Units=="U.S.") {
    datatable (CropCost.US, class="compact stripe",rownames=FALSE,
    caption=htmltools::tags$caption(style = 'caption-side: left; text-align: left; color:black; font-size:100% ;', 
      tags$b('2016 Oregon Crop Yields and Prices, from USDA-NASS')),
    options=list(dom='t',ordering=F,autoWidth = TRUE,pageLength=11,columnDefs=list(list(width = '150px', targets = 0),
                                                          list(width = '100px', targets = c(1,2)),
                                                          list(className = 'dt-center', targets = c(1,2))))) %>% formatCurrency(3, digits=0) } else {
     datatable (CropCost.Metric,
      class="compact stripe",rownames=FALSE,
      caption=htmltools::tags$caption(style = 'caption-side: left; text-align: left; color:black; 
      font-size:100% ;', tags$b('2016 Oregon Crop Yields and Prices, from USDA-NASS')),
      options=list(dom='t',ordering=F,autoWidth = TRUE,pageLength=11,columnDefs=list(list(width = '150px', targets = 0),
      list(width = '100px', targets = c(1,2)),list(className = 'dt-center', targets = c(1,2))))) %>% formatCurrency(3, digits=0)}
    })
  
#Tab 3 #### 
# Change in other inputs
  output$UnitsInputsN<-renderUI({
    Text<-paste("(",UnitsText$mass2," N/",UnitsText$area,")",sep="")
    strong(Text)
  })
  output$UnitsInputsP<-renderUI({
    Text1<-paste("(",UnitsText$mass2,sep="")
    Text2<-HTML(" P<sub>2</sub>O<sub>5</sub>/")
    Text3<-paste(UnitsText$area,")",sep="")
    strong(Text1,Text2,Text3)
  })
  output$UnitsInputsK<-renderUI({
    Text1<-paste("(",UnitsText$mass2,sep="")
    Text2<-HTML(" K<sub>2</sub>O/")
    Text3<-paste(UnitsText$area,")",sep="") 
    strong(Text1,Text2,Text3)
  })
  output$UnitsInputsLime<-renderUI({
    Text<-paste("(",UnitsText$mass,"/",UnitsText$area,")",sep="")
    strong(Text)
  }) 
  output$UnitsInputsIrr<-renderUI({
    Text<-paste("(",UnitsText$waterdepth," water)",sep="")
    strong(Text)
  }) 
  output$UnitsCostN<-renderUI({
    Text<-paste("($/",UnitsText$mass," N)",sep="")
    strong(Text)
  })  
  output$UnitsCostP<-renderUI({
    Text1<-paste("($/",UnitsText$mass,sep="")
    Text2<-HTML(" P<sub>2</sub>O<sub>5</sub>)")
    strong(Text1,Text2)
  })  
  output$UnitsCostK<-renderUI({
    Text1<-paste("($/",UnitsText$mass,sep="")
    Text2<-HTML(" K<sub>2</sub>O)")
    strong(Text1,Text2)
  })  
  output$UnitsCostLime<-renderUI({
    Text<-paste("($/",UnitsText$mass,")",sep="")
    strong(Text)
  })  
  output$UnitsCostIrr<-renderUI({
    Text<-paste("($/",UnitsText$watervol,")",sep="")
    strong(Text)
  }) 
  output$UnitsInputs6<-renderUI({
    #Text<-paste("Increase or Decrease in Annual Costs ($/",UnitsText$area,")",sep="")
    h4(TableHolder$caption)
  }) 
 
TableHolder <- reactiveValues(caption=NULL,df = CostsThroughTime(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,"U.S."))

#Compute benefit from changes in other inputs over time
CostInputChange<-reactive({
  Temp<-CostsThroughTime(input$ChangeN.Yr1,input$ChangeN.After,input$CostN,
                         input$ChangeP.Yr1,input$ChangeP.After,input$CostP,
                         input$ChangeK.Yr1,input$ChangeK.After,input$CostK,
                         input$ChangeLime.Yr1,input$ChangeLime.After,input$CostLime,
                         input$ChangeIrr.Yr1,input$ChangeIrr.After,input$CostIrr,input$Units)
  Temp<-unlist(Temp["Total",1:5])
  return(Temp)
})

observeEvent(input$checkbox2,{
  if(input$checkbox2==FALSE){
    updateNumericInput(session,"ChangeN.After",value=0)
    updateNumericInput(session,"ChangeP.After",value=0)
    updateNumericInput(session,"ChangeK.After",value=0)
    updateNumericInput(session,"ChangeLime.After",value=0)
    updateNumericInput(session,"ChangeIrr.After",value=0)} 
})



observeEvent(input$ShowInputChange,{
   TableHolder$df<- CostsThroughTime(input$ChangeN.Yr1,input$ChangeN.After,input$CostN,
                                    input$ChangeP.Yr1,input$ChangeP.After,input$CostP,
                                     input$ChangeK.Yr1,input$ChangeK.After,input$CostK,
                                     input$ChangeLime.Yr1,input$ChangeLime.After,input$CostLime,
                                     input$ChangeIrr.Yr1,input$ChangeIrr.After,input$CostIrr,input$Units)
   TableHolder$caption<-paste("Increase or Decrease in Annual Costs ($/",UnitsText$area,")",sep="")
   CostsOrValues$InputValue<-CostInputChange()*-1  
   
   output$Offsets<-renderDataTable({
     datatable(TableHolder$df,
               class='compact stripe',rownames=TRUE,
               caption='',
               options=list(dom='t',ordering=F,autoWidth = TRUE,columnDefs=list(list(width = '200px', targets = 0),
                  list(width = '100px', targets = c(1,2,3,4,5)),
                  list(visible=FALSE,targets=6),
                  list(className = 'dt-center', targets = c(1,2,3,4,5))))) %>% formatCurrency(1:6, digits=0) %>% formatStyle(6, target='row',fontWeight=styleEqual(c(1,0), c('bold','none')))
   })
  
})


# Tab 4 ####
# Summary #

NetBenefits<-reactive({
  if(input$applyinputs==TRUE){
      Temp<-rbind(CostsOrValues$BCCost,CostsOrValues$CropIncomeAnn,CostsOrValues$InputValue) 
      Net<-apply(Temp,2,sum)
        if(input$applyfuturediscount == TRUE) {
          Years<-0:4
          PV<-round(Net/((1+input$InterestRate/100)^Years),2) #0.045
          Temp<-rbind(Temp,Net,PV)
          Temp<-cbind(Temp,Marker=c(0,0,0,1,1))
          rownames(Temp)<-c("Biochar Cost","Change in Crop Margin","Change in Other Inputs","Net Benefit","Present Value of Net Benefit")
        } else {
          Temp<-rbind(Temp,Net)
          Temp<-cbind(Temp,Marker=c(0,0,0,1))
          rownames(Temp)<-c("Biochar Cost","Change in Crop Margin","Change in Other Inputs","Net Benefit")
         }
  } else {
      Temp<-rbind(CostsOrValues$BCCost,CostsOrValues$CropIncomeAnn)
      Net<-apply(Temp,2,sum)
      if(input$applyfuturediscount == TRUE) {
        Years<-0:4
        PV<-round(Net/((1+input$InterestRate/100)^Years),2) #0.045
        Temp<-rbind(Temp,Net,PV)
        Temp<-cbind(Temp,Marker=c(0,0,1,1))
        rownames(Temp)<-c("Biochar Cost","Change in Crop Margin","Net Benefit","Present Value of Net Benefit")
      } else {
        Temp<-rbind(Temp,Net)
        Temp<-cbind(Temp,Marker=c(0,0,1))
        rownames(Temp)<-c("Biochar Cost","Change in Crop Margin","Net Benefit")
      }
  }
    Temp<-cbind(Temp,apply(Temp,1,sum))
    colnames(Temp)<-c(paste("Year",1:5),"Marker","Total")
    return(Temp)
})


output$BenefitSummary<-renderDataTable({
  datatable(NetBenefits(),
                   class = 'compact', rownames=TRUE,
                   caption='',
                   options=list(dom='t',ordering=F, autoWidth = TRUE,columnDefs=list(list(width = '200px', targets = 0),
                       list(width = '100px', targets = c(1,2,3,4,5)),
                       list(visible=FALSE,targets=6),
                       list(className = 'dt-center', targets = c(1,2,3,4,5))))) %>% formatCurrency(1:7, digits=0) %>% formatStyle(
    6, target = 'row',
    fontWeight = styleEqual(c(1,0), c('bold','none')))})

}                  
shinyApp(ui = ui, server = server) 
