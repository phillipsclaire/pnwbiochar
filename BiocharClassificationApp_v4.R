rm(list=ls())
library(shiny) 
library(shinythemes)
library(bsplus)
library(DT)

# UI Functions ####
BC.df<-read.csv("C:/Users/claire.phillips/Dropbox/ARS Lab mine/CSC Project/Atlas/BiocharDatabase/BiocharData_PNWComputed_v2.csv")
BC.df$ID<-as.character(BC.df$ID)
Biochars<-{list("Soft wood"=c("Douglas fir, 300C"="Douglas fir 300 C",
                              "Douglas fir, 500C"="Douglas fir 500 C",
                              "Douglas fir, 700C"="Douglas fir 700 C",
                              "Conifer wood, 1250C (BioLogical Carbon)" = "Conifer Wood BioLogical Carbon",
                              "Ponderosa pine, 427C (Karrbonizer)" = "Pine Karrbonizer",
                              "Ponderosa pine, gasified (Waste to Energy, LLC)"="Gasified Ponderosa Pine",
                              "Juniper, gasified (Waste to Energy, LLC)"="Gasified Juniper"),
                "Hard wood"=c("Oregon white oak, 300C"="Oregon White Oak 300 C",
                              "Oregon white oak, 500C"="Oregon White Oak 500 C",
                              "Oregon white oak, 700C"="Oregon White Oak 700 C"),
                "Straw"=c("Wheat straw, gasified (AgEnergy)" = "Gasified Straw AgEnergy"),
                "Nuts"=c("Hazelnut shells, 300C"="Hazelnut shells 300 C",
                         "Hazelnut shells, 500C"="Hazelnut shells 500 C",
                         "Hazelnut shells, 700C"="Hazelnut shells 700 C"),
                "Manure"=c("Poultry litter pellets, 350C"="Poultry Litter Pellets 350 C",
                           "Poultry litter pellets, 500C"="Poultry Litter Pellets 500 C",
                           "Poultry litter pellets, 700C"="Poultry Litter Pellets 700 C"),
                "Other"=c("Yard debris, 350C"="Yard Debris 350 C",
                          "Yard debris, 500C"="Yard Debris 500 C",
                          "Yard debris, 700C"="Yard Debris 700 C",
                          "Spent brewer's grain, 300C"="Spent Brewers Grain 300 C",
                          "Spent brewer's grain, 500C"="Spent Brewers Grain 500 C",
                          "Spent brewer's grain, 700C"="Spent Brewers Grain 700 C")
)}
BiocharDataEntry<-function(){
  tabPanel(h3("Enter data"),
           br(),
  em(h4("Enter as much data as you have about your biochar.")),
  br(),
  textInput(inputId="Name",label="Name", value = "My biochar", width = NULL, placeholder = ""),
  wellPanel(style="background-color: LightSkyBlue;", strong("Carbon Storage Class"),br(),br(),
      fluidRow(column(width=2,numericInput(inputId="H", label="Total H (%)", value=NA, min = 0, max = 50, step = NA, width = NA))),
       br(),br(),
      fluidRow(column(width=2,numericInput(inputId="C", label="Total C (%)", value=NA, min = 0, max = 100, step = NA, width = NA)),
              column(width=1,em(style="color:red"," ")),
              column(width=2,numericInput(inputId="CaCO3", label="Carbonates (%)", value=NA, min = 0, max = 100, step = NA, width = NA)),
              column(width=1,em(style="color:red","or")),
              column(width=2,numericInput(inputId="OrgC", label="Organic C (%)", value=NA, min = 0, max = 100, step = NA, width = NA))),
      "Note: Total C is used in calculation if carbonates or organic C are unavailable."
),
  wellPanel(style="background-color: PaleGreen;", strong("Fertilizer Class"),br(),br(),
  # fluidRow(column(width=3,numericInput(inputId="TN", label="Total N (%)", value=NA, min = 0, max = 50, step = NA, width = NA)),
  #          column(width=3,numericInput(inputId="NO3N", label=HTML(paste("NO", tags$sub(3), "-N (ppm)", sep = "")), value=NA, min = 0, max = NA, step = NA, width = NA)),
  #          column(width=3,numericInput(inputId="NH4N", label=HTML(paste("NH", tags$sub(4), "-N (ppm)", sep = "")), value=NA, min = 0, max = NA, step = NA, width = NA))),
  fluidRow(column(width=2,numericInput(inputId="TK", label="Total K (ppm)", value=NA, min = 0, max = NA, step = NA, width = NA)),
           column(width=2,numericInput(inputId="AvailK", label="Avail. K (ppm)", value=NA, min = 0, max = NA, step = NA, width = NA))),
  fluidRow(column(width=2,numericInput(inputId="TP", label="Total P (ppm)", value=NA, min = 0, max = NA, step = NA, width = NA)),
           column(width=2,numericInput(inputId="AvailP", label="Avail. P (ppm)", value=NA, min = 0, max = NA, step = NA, width = NA))),
  fluidRow(column(width=2,numericInput(inputId="TMg", label="Total Mg (ppm)", value=NA, min = 0, max = NA, step = NA, width = NA)),
           column(width=2,numericInput(inputId="AvailMg", label="Avail. Mg (ppm)", value=NA, min = 0, max = NA, step = NA, width = NA))),
  # fluidRow(column(width=3,numericInput(inputId="TCa", label="Total Ca (ppm)", value=NA, min = 0, max = NA, step = NA, width = NA)),
  #          column(width=3,numericInput(inputId="AvailCa", label="Avail. Ca (ppm)", value=NA, min = 0, max = NA, step = NA, width = NA))),
  fluidRow(column(width=2,numericInput(inputId="TS", label="Total S (ppm)", value=NA, min = 0, max = NA, step = NA, width = NA)),
           column(width=2,numericInput(inputId="AvailS", label="Avail. S (ppm)", value=NA, min = 0, max = NA, step = NA, width = NA)))),
  wellPanel(style="background-color: PaleTurquoise;",strong("Liming Class"),br(),br(),
  numericInput(inputId="CCE", label=HTML(paste("CaCO", tags$sub(3), " equivalent (%)", sep = "")), value=NA, min = 0, max = 100, step = 1, width = '20%')),
  wellPanel(style="background-color: PaleGoldenRod;", strong("Particle Size Class"),br(),br(),
  fluidRow(column(width=2,numericInput(inputId="PS0.5", label="<0.5 mm (%)", value=NA, min = 0, max = 100, step = 1, width = NA)),
           column(width=2,numericInput(inputId="PS.1", label="0.5 - <1 mm (%)", value=NA, min = 0, max = 100, step = 1, width = NA)),
           column(width=2,numericInput(inputId="PS.2", label="1 - <2 mm (%)", value=NA, min = 0, max = 100, step = 1, width = NA))),
  fluidRow(column(width=2,numericInput(inputId="PS.4", label="2 - <4 mm (%)", value=NA, min = 0, max = 100, step = 1, width = NA)),
           column(width=2,numericInput(inputId="PS.8", label="4 - <8 mm (%)", value=NA, min = 0, max = 100, step = 1, width = NA)),
           column(width=2,numericInput(inputId="PS.16", label="8 - <16 mm (%)", value=NA, min = 0, max = 100, step = 1, width = NA))),
  fluidRow(column(width=2,numericInput(inputId="PS.25", label="16 - <25 mm (%)", value=NA, min = 0, max = 50, step = 1, width = NA)),
           column(width=2,numericInput(inputId="PS.50", label="25 - <50 mm (%)", value=NA, min = 0, max = 100, step = 1, width = NA)),
           column(width=2,numericInput(inputId="PS.Above50", label=">50 mm (%)", value=NA, min = 0, max = 100, step = 1, width = NA)))),
  hr(),
  actionButton(inputId="Classify", label="Classify", icon = NULL, width = '150px'), br(),br(),br(),
  value="NewData")
}
AmendmentUnits.Help<-{div(p(paste("Fertilizer Class 'Details' shows the biochar application rate that would needed to meet the nutrient requirements of an average corn crop.",
                              "Choose whether you want this amount reported in U.S. tons/acre or metric tons/ha.")),
                          p(HTML("In U.S. units, Ca<sub>5t</sub> means that 5 U.S. tons/acre are needed to provide enough Ca to meet the corn requirement."),
                            HTML("In metric units, this would be equivalent to Ca<sub>2t</sub>, or 2 metric tons/ha.")),
                          p("Click 'Classify' to recompute."))}
CClassMeaning<-"Scale from 1 to 5 based on quantity of carbon estimated to persist >100 years."
FertClassMeaning<-'Number of plant nutrients (P, K, S, and Mg) sufficiently available to meet the demand of a corn crop.'
LimeClassMeaning<-HTML('Scale from 0 to 3 based on CaCO<sub>3</sub> equivalence.')
PartClassMeaning<-"Four main classes: Lump (>50% large particles), Kernel (>50% medium), Powder (>50% fine), and Blended (mixture)."
ClassTableInitial<-{data.frame(Class=rep(NA,4),Details=rep(NA,4),Color=c(1,2,3,4),Meaning=c(CClassMeaning,FertClassMeaning,LimeClassMeaning,PartClassMeaning),
                               row.names=c("Carbon Storage Class","Fertilizer Class","Liming Class","Particle Size Class"))}
PlotVars<-{list("Carbon"=c("Carbon Class"="CClass",
                          "Total carbon"="TC",
                          "Fixed carbon"="FC",
                          "sBC100"="sBC100"),
               "Fertility"=c("Fertilizer Class"="FertClass",
                             "Available phosphorous"="AvP2O5.percent",
                             "Available potassium"="AvK2O.percent",
                             "Available magnesium"="AvMgO.percent",
                             "Available sulfur"="AvS.percent",
                             "Available calcium"="AvCaO.percent"),
               "Liming"=c("Liming class"="LimeClass",
                          "CaCO3-eq"="CCE",
                          "Ash content"="Ash",
                          "pH"="pH"),
               "Particle Size"=c("Mean particle size"="PS.mean"),
               "Other"=c("Electrical conductivity"="EC",
                         "Volatile matter"="VM"))}
PlotSpecs<-{data.frame(Var=c("CClass","TC","FC","sBC100","FertClass","AvP2O5.percent","AvK2O.percent","AvMgO.percent","AvS.percent","AvCaO.percent",
                            "LimeClass","CCE","Ash","pH","PS.mean","EC","VM"),
                      YLab=c("Carbon Class (1-5)","Total C (% mass)","Fixed carbon (% mass)", "sBC100 (g/kg)","Fertilizer Class (0-4)","P2O5 (% mass)", "K2O (% mass)","MgO (% mass)",
                             "Sulfur (% mass)","CaO (% mass)","Lime Class (0-3)","Lime equivalence (% CaCO3)","Ash content (% mass)","pH",
                             "Mean particle size (mm)","EC (dS/m)","Volatile matter (% mass)"))}

# Classification Calculations #####
CClass<-function(H,C,OrgC,H_OrgC,CaCO3){
  if(is.na(OrgC)) OrgC = C-CaCO3
  if(is.na(OrgC) & is.na(CaCO3)) OrgC = C 
  if(is.na(C) | is.na(H)) return(list(Class=NA,Label=NA)) else{
  H_OrgC = H/OrgC*12
  sBC100 = round(-74.3 * H_OrgC + 110.2,1)*OrgC/10
  CClass = as.numeric(cut(sBC100,breaks=c(0,300,400,500,600,1000),labels=c(1,2,3,4,5),right=TRUE))
  Label=paste("sBC<sub>+100</sub> = ",round(sBC100,1),"g kg<sup>-1</sup>",sep="")
  return(list(Class=CClass,Label=Label,sBC100=sBC100))}
}
#TestCClass<-CClass(1.19,88.6,NA,NA,NA)
#CClass(NA,NA,NA,NA,NA)

#AvailP=1816;AvailK=1676; AvailS=24; AvailMg=62;AvailCa=14367
FertClass<-function(AvailP,AvailK,AvailS,AvailMg,ReportUnits="metric tons/ha"){
  if(sum(is.na(c(AvailP,AvailK,AvailS,AvailMg)))==4) return(list(Class=NA,Requirement=NA,Label=NA)) else {
  CropDemand<-data.frame(P=45,K=45,S=17,Mg=20) #Corn grain demand, kg/ha
  
  # Convert ppm of each element to kg/Mg: 1 mg/kg = 0.001 kg/Mg
  T_ha_P<-ceiling(CropDemand$P/(AvailP/1000)) #kg required / kg of element in a Mg biochar
  T_ha_K<-ceiling(CropDemand$K/(AvailK/1000)) 
  T_ha_S<-ceiling(CropDemand$S/(AvailS/1000)) 
  T_ha_Mg<-ceiling(CropDemand$Mg/(AvailMg/1000)) 
  Requirement<-data.frame(P=T_ha_P,K=T_ha_K,S=T_ha_S,Mg=T_ha_Mg) #metric tons/ha required to meet demand
  #row.names(Requirement)<-"metric tons/ha"
  
  # Sum number of nutrients meeting <= 10 metric tons/ha cutoff
  Class<-sum(c(T_ha_P <= 10,T_ha_K <= 10,T_ha_S <= 10, T_ha_Mg <= 10),na.rm=TRUE)

  # Condense to label format
  Elements=c("P","K","S","Mg")
  Elements2<-Elements[which(Requirement<10)]
  Requirement2<-Requirement[which(Requirement<10)]
  
  if (ReportUnits=="U.S. tons/acre")  {Requirement2<-ceiling(Requirement2/0.446)
  Requirement<-ceiling(Requirement/0.446)
  #row.names(Requirement)<-"U.S. tons/acre"
  }
  
  if(length(Elements2)==1){
    FertHTML<-paste(Elements2[1],"<sub>",Requirement2[1],"t</sub>",sep="")
  } else
  if(length(Elements2)>0){
    FertHTML<-paste(Elements2[1],"<sub>",Requirement2[1],"t</sub>",sep="")
    for(i in 2:length(Elements2)){
      temp<-paste(Elements2[i],"<sub>",Requirement2[i],"t</sub>",sep="")
      FertHTML<-paste(FertHTML,temp)}
 } else FertHTML<-"None"
 
  return(list(Class=Class,Requirement=Requirement,Label=FertHTML))}
}
#FertClass(1748,115000,25,2500,2500,"U.S. tons/acre")
#TestFertClass<-FertClass(1816,1676,24,62,14367,"U.S. tons/acre")
#FertClass(NA,NA,NA,NA,NA)

LimeClass<-function(CCE){
  if(is.na(CCE)) return(list(Class=NA,Label=NA)) else{
  Class<-as.numeric(as.character(cut(CCE,breaks=c(0,1.0,10.0,20.0,100.0),labels=c(0,1,2,3),right=FALSE)))
  Label<-paste("CaCO<sub>3</sub> - eq = ",round(CCE,1),"%",sep="")
  return(list(Class=Class,Label=Label))}
}
#TestLimeClass<-LimeClass(0.5)

PartClass<-function(PS0.5,PS.1,PS.2,PS.4,PS.8,PS.16,PS.25,PS.50,PS.Above50){
  if(sum(is.na(c(PS0.5,PS.1,PS.2,PS.4,PS.8,PS.16,PS.25,PS.50,PS.Above50)))==9){return(list(Class=NA,Label=NA))} else 
  if(sum(c(PS0.5,PS.1,PS.2,PS.4,PS.8,PS.16,PS.25,PS.50,PS.Above50),na.rm=TRUE)<90) return(list(Class = NA, Label = "Classes should sum to 100%")) else {
  if(sum(c(PS0.5,PS.1,PS.2)) >= 50) {Class ="Pd" 
      if(PS0.5>50) Long="Fine powder" else
      if(PS.1>50) Long="Medium powder" else
      if(PS.2>50) Long="Coarse powder" else Long="Blended powder"
  } else
  if(sum(c(PS.4,PS.8,PS.16)) >= 50) {Class = "Kn"
    Long = "Kernel"} else
  if(sum(c(PS.25,PS.50,PS.Above50)) > 50) {Class = "Lp" 
    Long= "Lump" } else {
        Class = "Bd"
        Long = "Blended"
  }
  return(list(Class=Class,Label=Long))}
}
# PartClass(1,4,3,5,10,0,0,0,0)
#TestPartClass<-PartClass(70.4,20.1,6.7,2.3,0.5,0,0,0,0)
# PartClass(1,NA,NA,NA,NA,NA,NA,NA,NA)

PSMean<-function(PS0.5,PS.1,PS.2,PS.4,PS.8,PS.16,PS.25,PS.50,PS.Above50){
  PS.mean<-c(PS0.5,PS.1,PS.2,PS.4,PS.8,PS.16,PS.25,PS.50,PS.Above50)/100*c(0.25,0.75,1.5,3,6,12,20.5,37.5,50)
  return(PS.mean)
}

# RShiny Functions ####
ui <- fluidPage(theme = shinytheme("spacelab"),
  titlePanel("Biochar Classification Tool"),
conditionalPanel(condition="input.tabs != 'plot'", 
      em(h4("Choose a biochar from our database of PNW feedstocks, or enter data for your own biochar to view its classification.")),
      hr(),
      fluidRow(column(width=7,dataTableOutput("DT"))),
      br(),
      fluidRow(column(width=2,"Select amendment rate units")),
      fluidRow(column(width=2,selectInput("AmendmentUnits",NULL,list("U.S. tons/acre","metric tons/ha"))),
         column(width=1,bs_modal(id="UnitsInfo", title="Amendment Rate Units", body=AmendmentUnits.Help, size = "med"),
                tags$button(type="button",class = "btn btn-default", icon("info-circle"))%>%
                  bs_attach_modal(id_modal="UnitsInfo")))),
conditionalPanel(condition="input.tabs =='plot'",
       plotOutput("CompareChars")),          
 tabsetPanel(id="tabs",
       tabPanel(h3("Choose a biochar"),
                selectInput(inputId = "BC", label = "", choices = Biochars,selected="Douglas fir 300 C"),
                actionButton(inputId="Update", label="Classify", icon = NULL, width = '150px'),
                value="Database"),
       BiocharDataEntry(),
       tabPanel(h3("Compare biochars"),
                em(h4("Select a variable to plot")),
                selectInput(inputId = "Var", label = "Select a variable", choices = PlotVars),
                br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                value="plot"),
      type="tabs")
)
 
#server <- function(input, output) {}
server <- function(input, output) {

  #If user enters their own data, use ClassCompute. If user selects from database, use GetBiochar
  ClassCompute<-reactive({
    Table1<-cbind(CClass=CClass(input$H,input$C,input$OrgC,input$H_OrgC,input$CaCO3)$Class,
                  FertClass=FertClass(input$AvailP,input$AvailK,input$AvailS,input$AvailMg,input$AmendmentUnits)$Class,
                  LimeClass=LimeClass(input$CCE)$Class,
                  PClass=PartClass(input$PS0.5,input$PS.1,input$PS.2,input$PS.4,input$PS.8,input$PS.16,
                                   input$PS.25,input$PS.50,input$PS.Above50)$Class)
    Table2<-cbind(CClass=CClass(input$H,input$C,input$OrgC,input$H_OrgC,input$CaCO3)$Label,
                  FertClass=FertClass(input$AvailP,input$AvailK,input$AvailS,input$AvailMg,input$AmendmentUnits)$Label,
                  LimeClass=LimeClass(input$CCE)$Label,
                  PClass=PartClass(input$PS0.5,input$PS.1,input$PS.2,input$PS.4,input$PS.8,input$PS.16,
                                   input$PS.25,input$PS.50,input$PS.Above50)$Label)

    #Output data as two columns
    HTMLTable<-t(rbind(Table1,Table2))
    HTMLTable<-cbind(HTMLTable,c(1,2,3,4),c(CClassMeaning,FertClassMeaning,LimeClassMeaning,PartClassMeaning)) #Add a column to define colors
    rownames(HTMLTable)<-c("Carbon Storage Class","Fertilizer Class","Liming Class","Particle Size Class")
    colnames(HTMLTable)<-c("Class","Details","Color","Meaning")
    return(HTMLTable)
  })
 
  GetBiochar<-reactive({
    GetVars<-BC.df[BC.df$ID==input$BC,] #"Oregon White Oak 300 C"
    
    Table1<-cbind(CClass=CClass(GetVars$TH,GetVars$TC,NA,NA,NA)$Class,
                  FertClass=FertClass(GetVars$AvP.ppm,GetVars$AvK.ppm,GetVars$AvS.ppm,GetVars$AvMg.ppm,input$AmendmentUnits)$Class, #"U.S. tons/acre"
                  LimeClass=LimeClass(GetVars$CCE)$Class,
                  PClass=PartClass(GetVars$PS0.5,GetVars$PS1,GetVars$PS2,GetVars$PS4,GetVars$PS8,GetVars$PS16,
                                   GetVars$PS25,GetVars$PS50,GetVars$PSabove50)$Class)
    Table2<-cbind(CClass=CClass(GetVars$TH,GetVars$TC,NA,NA,NA)$Label,
                  FertClass=FertClass(GetVars$AvP.ppm,GetVars$AvK.ppm,GetVars$AvS.ppm,GetVars$AvMg.ppm,input$AmendmentUnits)$Label, #input$AmendmentUnits
                  LimeClass=LimeClass(GetVars$CCE)$Label,
                  PClass=PartClass(GetVars$PS0.5,GetVars$PS1,GetVars$PS2,GetVars$PS4,GetVars$PS8,GetVars$PS16,
                                   GetVars$PS25,GetVars$PS50,GetVars$PSabove50)$Label)
    
    #Output data as two columns
    HTMLTable<-t(rbind(Table1,Table2,c(1,2,3,4),c(CClassMeaning,FertClassMeaning,LimeClassMeaning,PartClassMeaning)))#Add a column to define colors
    rownames(HTMLTable)<-c("Carbon Storage Class","Fertilizer Class","Liming Class","Particle Size Class")
    colnames(HTMLTable)<-c("Class","Details","Color","Meaning")
    return(HTMLTable)
  })
 
  do<-reactiveValues(tab="Database",tbl=ClassTableInitial,caption="")
  #observeEvent(input$tabs, {do$tab<-input$tabs})
  
  #Plot an empty classification table initially, and then update it when either button is clicked.
  #If user selects a biochar from the database, will click on "Update" button
  observeEvent(input$Update,{do$tbl=GetBiochar()
      do$caption=input$BC})
  #If user enters their own data, will click on "Classify" button
  observeEvent(input$Classify,{do$tbl=ClassCompute()
  do$caption=input$Name})
  
  #If user enters thier own data, add that biochar to BC.df
  observeEvent(input$Classify,{
    UserData<-c(NA,24,NA,NA,input$Name,input$H,input$C,input$H_OrgC,input$OrgC,NA,input$CCE,NA,NA,NA,NA,NA, #1-16
      input$AvailP,NA,input$AvailK,input$AvailMg,NA,input$AvailS, #17-22
      input$AvailP/10000/0.436,NA,input$AvailK/10000/0.833,input$AvailMg/10000/0.602,NA,input$AvailS/10000, # 23-28
      rep(NA,17), #29-45
      input$PS.Above50,input$PS.50,input$PS.25,input$PS.16,input$PS.8,input$PS.4,input$PS.2,input$PS.1,input$PS0.5, #46-54
      PSMean(input$PS0.5,input$PS.1,input$PS.2,input$PS.4,input$PS.8,input$PS.16,
              input$PS.25,input$PS.50,input$PS.Above50), #55
      PartClass(input$PS0.5,input$PS.1,input$PS.2,input$PS.4,input$PS.8,input$PS.16,
                input$PS.25,input$PS.50,input$PS.Above50)$Class,NA,NA,NA,#56-59
      CClass(input$H,input$C,input$OrgC,input$H_OrgC,input$CaCO3)$sBC100, #60
      CClass(input$H,input$C,input$OrgC,input$H_OrgC,input$CaCO3)$Class, #61
      FertClass=FertClass(input$AvailP,input$AvailK,input$AvailS,input$AvailMg,input$AmendmentUnits)$Class, #62
      LimeClass=LimeClass(input$CCE)$Class,rep(NA,5)) #63-68
    BC.df<-rbind(BC.df[1:23,],UserData)
  })
  
  output$DT<-renderDataTable({
      datatable(do$tbl,escape=FALSE,
              caption= htmltools::tags$caption(
                style = 'caption-side: left; text-align: left; color:black; 
                font-size:150% ;', do$caption),
                options=list(dom='t',ordering=F, autoWidth = TRUE,
                   columnDefs=list(list(width = '200px', targets = c(0,1,2)),list(width = '400px', targets = c(4)),
                   list(className = 'dt-center', targets = c(1,2,4)),
                   list(visible=FALSE,targets=3))))  %>% formatStyle('Color',target = 'row',
                          backgroundColor = styleEqual(c(1,2,3,4), c('LightSkyBlue', 'PaleGreen','PaleTurquoise','PaleGoldenRod')))
  })

  output$CompareChars<-renderPlot({
    Y<-BC.df[order(BC.df$Order),input$Var] #Order rows so similar feedstocks group together
    Labels<-BC.df[order(BC.df$Order),"ID"]
    par(mar=c(9,4,2,2))
    plot(1:nrow(BC.df),Y,ylab=PlotSpecs[PlotSpecs$Var==input$Var,"YLab"], tck=0.03, cex.lab=1.4,xaxt = "n",  xlab = "",pch=16, cex=1.2, col="blue")
    axis(side=1, at=1:nrow(BC.df),labels=FALSE, tck=0.03)
    text(1:nrow(BC.df), par("usr")[3] - 0.25, srt = 35, adj = 1,labels = Labels, xpd = TRUE, cex=1.2)
    abline(v=c(7.5,10.5,11.5,14.5,17.5, 20.5))
    if(nrow(BC.df)==23){
    mtext(c("Softwood","Hardwood","Straw","Nuts","Manure","Yard Debris","Spent Grain"),side=3,line=0,
          at=c(3.5,9,11,13,16,19,22),cex=1.4,adj=0.5)} else {
    mtext(c("Softwood","Hardwood","Straw","Nuts","Manure","Yard Debris","Spent Grain",input$Name),side=3,line=0,
          at=c(3.5,9,11,13,16,19,22,24),cex=1.4,adj=0.5)        
          }
  })  
  

}



shinyApp(ui = ui, server = server) 



