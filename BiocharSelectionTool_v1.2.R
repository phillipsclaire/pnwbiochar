# Biochar Selection Tool
# Monster tool for Atlas 

#rm(list=ls())
library(shiny)
library(bsplus)
library(shinythemes)
library(soiltexture)
library(DT)

# File Upload and Functions ####
CropDemand<-read.delim("C:/Users/claire.phillips/Dropbox/ARS Lab mine/CSC Project/Atlas/NutrientDemand/CropFertilizerGuide_Aug_26_2017.txt")
#dim(CropDemand); names(CropDemand)
BC.df<-read.csv("C:/Users/claire.phillips/Dropbox/ARS Lab mine/CSC Project/Atlas/BiocharDatabase/BiocharData_PNWComputed.csv")
#dim(BC.df);names(BC.df)

# UI Functions ####

# Soil Input Tab ####
SoilDataEntry<-function(){
  fluidRow(column(width=3,wellPanel(h4("Carbon"),
                                    numericInput(inputId="OM", label="Organic Matter (%)", value=NA, min = 0, max = 100, step = NA, width = NA))),
                                    #numericInput(inputId="TC", label="Total Carbon (%)", value=NA, min = 0, max = 100, step = NA, width = NA))),
           column(width=3,wellPanel(h4("Fertility"),em("Exchangable Concentrations"), br(),br(),
                                    numericInput(inputId="SoilP", label="Phosphorous (ppm)", value=NA, min = 0, max =NA, step = NA, width = NA),
                                    numericInput(inputId="SoilK", label="Potassium (ppm)", value=NA, min = 0, max =NA, step = NA, width = NA),
                                    numericInput(inputId="SoilS", label="Sulfur (ppm)", value=NA, min = 0, max = NA, step = NA, width = NA),
                                    numericInput(inputId="SoilMg", label="Magnesium (ppm)", value=NA, min = 0, max =NA, step = NA, width = NA),
                                    numericInput(inputId="SoilMgCEC", label="Magnesium (% of CEC or base saturation)", value=NA, min = 0, max =NA, step = NA, width = NA))),
           
           column(width=3,wellPanel(h4("Acidity/Buffering"),
                                    numericInput(inputId="SoilpH", label="pH", value=NA, min = 0, max =NA, step = NA, width = NA),
                                    numericInput(inputId="SoilSMP", label="pH in SMP buffer", value=NA, min = 0, max =NA, step = NA, width = NA),
                                    numericInput(inputId="SoilCEC", label="Cation Exchange Capacity-CEC (meq charge/100 g soil)", value=NA, min = 0, max =NA, step = NA, width = NA))),
          column(width=3,wellPanel(h4("Texture"),
                           numericInput(inputId="Sand", label="Sand (%)", value=NA, min = 0, max = 100, step = NA, width = NA),
                           numericInput(inputId="Silt", label="Silt (%)", value=NA, min = 0, max = 100, step = NA, width = NA),
                           numericInput(inputId="Clay", label="Clay (%)", value=NA, min = 0, max = 100, step = NA, width = NA))))
  
  }

# Soil Interp Tab ####
Texture<-function(clay,silt,sand){
  texture.data<-data.frame(CLAY=clay,SILT=silt,SAND=sand)
  tex<-TT.points.in.classes(tri.data = texture.data, class.sys = "USDA.TT",PiC.type = "l")
  Classes<-c("Clay","Silty clay","Sandy clay","Clay loam","Silty clay loam","Sandy clay loam",
                      "Loam","Silt loam","Sandy loam","Silt","Loamy sand","Sand")
  YourClass<-Classes[tex]
  return(YourClass)
}
PAWTable<-function(YourClass){
  SoilTexture=c("Sand","Loamy sand","Sandy loam","Loam","Silt loam",
                "Silt","Sandy clay loam","Sandy clay",
                "Clay loam"," Silty clay loam","Silty clay","Clay")
  AvailMoist.std<-c(0.7,1.1,1.4,1.8,1.8,1.8,1.3,1.6,1.7,1.9,2.4,2.2)
  #AvailMoist.metric<-round(AvailMoist.std*25.4/0.304,1)
  PAW<-data.frame(SoilTexture,AvailMoist.std,Flag=rep(0,12)) #AvailMoist.metric,
  if(YourClass!="ERROR") PAW$Flag[which(SoilTexture %in% YourClass)]<-1
  rownames(PAW)<-NULL
  colnames(PAW)<-c("Soil Texture","Avail. Soil Water (in/ft)","Your Soil") #"Avail. Soil Moisture (mm/m)",
  return(PAW)
}
#PAWTable("Sand")
# Texture(15,35,50)
MoistureText1<-{div(p(paste("Soil moisture properties are determined by organic matter content and by soil texture.",
    "Soil texture refers to the size of soil particles.",
    "Soils with an abundance of large particles have more large pores,",
    "and drain water more readily than soils with an abundance of small particles.",
    "In contrast, soils with an abundance of small particles and small pores tend to evaporate more slowly",
    "and have higher water retention than soils with large particles.")),
    p("Soil particles are divided into 3 size classes:"),
    tags$ul(
      tags$li("sand (>2000 micrometers)"), 
      tags$li("silt (2-50 micrometers)"), 
      tags$li("and clay (<2 micrometers).")
    ),
  p("A ",strong("loam")," refers to a soil that is not dominated by any one particle size class, but has relatively equal influences from sand, silt, and clay particles.",
    "Loam soils have a balance of both relatively good drainage and water retention.")
    #,
    # p(paste("Note that silt and loam soils tend to have the most available water. ",
    # "Sandy soils, which have larger pores, tend to lose water to drainage, ",
    # "while clay soils, which have very small pores, tend to hold water more tightly than can be extracted by plants."))
)}
MoistureText2<-p(paste("The table below shows representative data for the amount of water contained in soils ",
  "when they are well-drained, ordered from most coarse to most fine soil textures."))
MoistureText3<-{div(p("Note that available water content is generally higher in soils with finer particles, because fine-textured soils evaporate and drain water more slowly."),
                # p(paste("However, clay soils, which have the finest particles, have somewhat reduced available water content.", 
                #     "This is because the pores in clays are so small, they hold water with sufficient suction to make it unavailable to plants.")),
                p("Adding sources of organic matter, such as biochar, can improve water drainage", strong(em("or")), "water retention.",
                  "Choose a biochar with large particles to increase drainage, and one with a lot of fine particles to increase retention."))}
CarbonText1<-{
  div(p(paste("Carbon, in the form of soil organic matter, is an essential component of soil health that ", 
              "helps to prevent erosion and provides essential functions including soil water storage, ",
              "fertility, and biological activity.")),
    p("Soil organic matter content is typically between 0.5-5% mass in most soils."),
    p(paste("Soils with a history of tillage often have lower organic matter content than uncultivated or reduced-tillage soils. ",
            #"Soils with organic matter contents >5% are likely to have high amounts of biomass inputs, or limited rates of decomposition (as in water-logged soils). ",
            "Soils with organic matter contents >10% are referred to as organic soils, as their properties are dominated by organic matter rather than by mineral soil.")),
    strong("Your soil level is highlighted in the table below.")
  )}
CarbonText2<-{div(h4("Building Soil Carbon"),
              p("Soil organic matter content can be increased by leaving plant residues, by applying biochar, compost, or other types of organic matter, and by reducing tillage. "),
              p(paste("Note that very high organic matter contents can present challenges for crop growth. ",
              "In organic soils, you may need to provide mineral amendments to account for the reduced mineral content of the soil.")))}
CarbonTable<-function(OM){
  CTable<-data.frame(Levels=c("Low","Med","High","Very High","Organic Soil"),
                     PercentCarbon=c("0-1","1-3","3-5","5-10",">10"),
                       Flag=rep(0,5))
  Cutoffs<-c(1,3,5,10)
  CTable$Flag[findInterval(OM,Cutoffs,rightmost.closed = TRUE)+1]<-1
  rownames(CTable)<-NULL
  colnames(CTable)<-c("Level","Organic Matter (%)","Your Soil")
  # CTable<-formattable(CTable,list(
  #   area(col="Your soil")~normalize_bar("pink",0.2)
  # ))
  return(CTable)
}
#CarbonTable(10)
Crops<-{list("Forage and Seed"=c("Alfalfa, irrigated"="Alfalfa irrigated",
                                 "Alfalfa, dryland"="Alfalfa dryland",
                                 "Crimson clover", "Field peas",
                                 "Pasture, established (west side)"="Pasture established west side",
                                 "Pasture, new seeding (west side)"="Pasture new seeding west side",
                                 "Perennial ryegrass, established"="Perennial ryegrass established",
                                 "Perennial ryegrass, new seeding"="Perennial ryegrass new seeding",
                                 "Tall fescue, established"="Tall fescue established",
                                 "Tall fescue, new seeding"="Tall fescue new seeding",
                                 "Winter wheat (east side, high precip)"="Winter wheat east side high precip",
                                 "Winter wheat (east side, intermediate precip)"="Winter wheat east side intermediate precip",
                                 "Winter wheat (east side, low precip)"="Winter wheat east side low precip",
                                 "Winter wheat (west side)"="Winter wheat west side",
                                 "Vetch"),
             "Berries"=c("Blackberries, established"="Blackberries established",
                         "Blueberries, mature (>4 yrs)"="Blueberries mature",
                         "Raspberries, established"="Raspberries established"),               
             "Vegetables"=c("Asparagus","Broccoli","Brussel Sprouts","Cabbage","Carrots",
                            "Cauliflower","Cucumber","Green or snap beans","Lettuce","Melon",
                            "Onions, dry bulb"="Onions",
                            "Peas, shelled (west side)"="Peas shelled west side",
                            "Peas, shelled (east side)"="Peas shelled east side",
                            "Potatoes (east side)"="Potatoes east side",
                            "Radish",
                            "Squash, winter"="Squash winter",
                            "Sweet corn","Table beets")
)}
FindLevel<-function(Cutoffs,TestValue){
  Cutoffs<-Cutoffs+0.4
  Flag<-findInterval(TestValue,Cutoffs[!is.na(Cutoffs)],rightmost.closed = TRUE)+1
  return(Flag)
}
PTable<-function(CropInput,TestValue){
  Table<-data.frame(Level=c("Low","Med","High","Very High","Not Limited"),
                    TestValues=unlist(CropDemand[CropDemand$Crop2==CropInput,c("Plow.test","Pmed.test","Phigh.test","Pvhigh.test","Pabove.test")]),
                    AmendmentRate=t(CropDemand[CropDemand$Crop2==CropInput,c("Plow.rate","Pmed.rate","Phigh.rate","Pvhigh.rate","Pabove.rate")]),
                    Flag=rep(0,5))
  Table[,2]<-as.character(Table[,2])
  Table[,3]<-as.character(Table[,3])
  Table[Table==""]=NA
  Cutoffs<-CropDemand[CropDemand$Crop2==CropInput,c("Plow.cutoff","Pmed.cutoff","Phigh.cutoff","Pvhigh.cutoff")]
  Table$Flag[FindLevel(Cutoffs,TestValue)]<-1
  Table$Flag[is.na(Table$Flag)]<-0
  Table<-na.omit(Table)
  Table$Flag[Table$Flag==0]<-NA
  rownames(Table)<-NULL
  colnames(Table)<-c("Level","Test Values (ppm)","Amendment Rate (lbs P2O5/acre)","Your Soil")
  return(Table)
}
#PTable("Perennial ryegrass established",168)
CropInput="Perennial ryegrass established"
KTable<-function(CropInput,TestValue){
  Table<-data.frame(Level=c("Low","Med","High","Very High","Not Limited"),
                    TestValues=unlist(CropDemand[CropDemand$Crop2==CropInput,c("Klow.test","Kmed.test","Khigh.test","Kvhigh.test","Kabove.test")]),
                    AmendmentRate=t(CropDemand[CropDemand$Crop2==CropInput,c("Klow.rate","Kmed.rate","Khigh.rate","Kvhigh.rate","Kabove.rate")]),
                    Flag=rep(0,5))
  Table[,2]<-as.character(Table[,2])
  Table[,3]<-as.character(Table[,3])
  Table[Table==""]=NA
  Cutoffs<-CropDemand[CropDemand$Crop2==CropInput,c("Klow.cutoff","Kmed.cutoff","Khigh.cutoff","Kvhigh.cutoff")]
  Table$Flag[FindLevel(Cutoffs,TestValue)]<-1
  Table$Flag[is.na(Table$Flag)]<-0
  Table<-na.omit(Table)
  Table$Flag[Table$Flag==0]<-NA
  rownames(Table)<-NULL
  colnames(Table)<-c("Level","Test Values (ppm)","Amendment Rate (lbs K2O/acre)","Your Soil")
  return(Table)
}
STable<-function(CropInput,TestValue){
  if(!is.na(CropDemand[CropDemand$Crop2==CropInput,"Slow.cutoff"])){
    Table<-data.frame(Level=c("Low","Med","High","Very High"),
                      TestValues=t(CropDemand[CropDemand$Crop2==CropInput,c("Slow.test","Smed.test","Shigh.test","Svhigh.test")]),
                      AmendmentRate=t(CropDemand[CropDemand$Crop2==CropInput,c("Slow.rate","Smed.rate","Shigh.rate","Svhigh.rate")]),
                      Flag=rep(0,4))
    Table[,2]<-as.character(Table[,2])
    Table[,3]<-as.character(Table[,3])
    Table[Table==""]=NA
    Cutoffs<-CropDemand[CropDemand$Crop2==CropInput,c("Slow.cutoff","Smed.cutoff","Shigh.cutoff")]
    Table$Flag[FindLevel(Cutoffs,TestValue)]<-1
    Table$Flag[is.na(Table$Flag)]<-0
    Table<-na.omit(Table)
    Table$Flag[Table$Flag==0]<-NA
    rownames(Table)<-NULL
    colnames(Table)<-c("Level","Test Values (ppm)","Amendment Rate (lbs S/acre)","Your Soil")
    return(Table)
  } else return(NULL)
}
SText<-function(CropInput,TestValue){
  if(CropDemand[CropDemand$Crop2==CropInput,"S.singlerate"]!=""){
    SingleRate<-CropDemand[CropDemand$Crop2==CropInput,"S.singlerate"]
    Text<-div(strong("Sulfur",style="color:black"),
              p(paste("The recommendation for this crop is an annual application of ", SingleRate, " pounds S per acre. No soil test limits are specified.",sep="")))
    return(Text)
    } else 
    if(CropDemand[CropDemand$Crop2==CropInput,"S.singlerate"]=="" & is.na(CropDemand[CropDemand$Crop2==CropInput,"Slow.cutoff"])) {
      Text<-div(strong("Sulfur",style="color:black"),p("No sulfur additions are recommended for this crop in most soils."))
      return(Text)
    } else return(NULL)
}
MgText<-function(CropInput,SoilMgCEC,SoilCEC){
  TestValue<-SoilMgCEC/100*SoilCEC
  if(CropDemand[CropDemand$Crop2==CropInput,"Mg.rate"]!=""){
    Rate<-CropDemand[CropDemand$Crop2==CropInput,"Mg.rate"]
    Cutoff<-CropDemand[CropDemand$Crop2==CropInput,"Mg.cutoff"]
    if(TestValue<=Cutoff) {
      Text<-paste("It is recommended that you apply ", Rate," pounds/acre (as MgO) for the crop above. Your soil magnesium levels are at or below the critical threshold of ",Cutoff," meq/100 g soil.",sep="")
    } else if(TestValue>Cutoff) {
      Text<-paste("It is not recommended that you apply additional magnesium for the crop selected above. Your soil magnesium levels are above the critical threshold of ",Cutoff," meq/100 g soil.",sep="")
    } 
  } else Text<-"No magnesium additions are recommended for this crop in most soils."
  return(Text)
}
CropRef<-function(CropInput){
  Ref<-CropDemand[CropDemand$Crop2==CropInput,"Reference"]
  return(Ref)
}
pHResult<-function(CropInput,TestValue=NULL){
  pHMin<-CropDemand[CropDemand$Crop2==CropInput,"pH.min"]
  pHMax<-CropDemand[CropDemand$Crop2==CropInput,"pH.max"]
  if(is.na(pHMax)) {
    if(is.null(TestValue)) {
      Text<-paste("The lower pH limit recommended for this crop is ",pHMin,".",sep="")
    } else if(TestValue<=(pHMin+0.2)){
      Text<-paste("Your soil pH is ", TestValue,", which is near or below the lower limit of ", pHMin, " recommended for optimum growth of the crop selected above. ",
        "You can increase soil pH by applying lime (calcium carbonate) or other liming agents such as a high-ash biochar. ",
        "Obtain a liming test, such as the SMP or Sikora buffer tests to determine appropriate liming rates.", sep="")
    } else if(TestValue>(pHMin+0.2) & TestValue<7.8) {
        Text<-paste("Your soil pH is ", TestValue,", which is higher than the lower limit of ", pHMin, " recommended for optimum growth of the crop selected above. You likely do not need to adjust soil pH.", sep="") 
    } else if(TestValue>=7.8){
        Text<-paste("Your soil pH is ", TestValue,", which is alkaline (>7.5). Although upper pH limits are not reported for the crop selected above, most plants grow best at pH levels near 7.",
                " You may benefit from decreasing soil pH to a more neutral level.", sep="")
    } 
  } else if(!is.na(pHMax)){
    if(is.null(TestValue)) {
      Text<-paste("The pH range reccomended for this crop is ",pHMin,"-", pHMax,".",sep="")
    } else if(TestValue<=(pHMin+0.2)){
      Text<-paste("Your soil pH is ", TestValue,", which is near or below the lower limit of ", pHMin, "-", pHMax, " recommended for optimum growth of the crop selected above. ",
                  "You can increase soil pH by applying lime (calcium carbonate) or liming agents such as a high-ash biochar. ",
                  "Obtain a liming test, such as the SMP or Sikora buffer tests to determine appropriate liming rates.", sep="")
    } else if(TestValue>(pHMin+0.2) & TestValue<=(pHMax-0.2)){
        Text<-paste("Your soil pH is ", TestValue,", which is within the range of ", pHMin,"-", pHMax," recommended for optimum growth of the crop selected above.",
                " Your soil likely does not require pH adjustments.", sep="")
    } else if(TestValue>(pHMax-0.2)){
    Text<-paste("Your soil pH is ", TestValue,", which is near or above the upper limit of ", pHMin,"-", pHMax," recommended for optimum growth of the crop selected above.",
                " You may benefit from decreasing soil pH, by using elemental sulfur or acidic amendments, such as bokashi biochar.", sep="")
    }
  }
  return(Text)
}
pHText1<-{div(p(HTML("Soil pH can be increased by applying lime (CaCO<sub>3</sub>) or other materials that neutralize acidity, "),
                    "including biochars with high ash contents."),
             p(paste("Determining the amount of liming amendment to add requires soils tests in addition to pH, ",
                     "such as an SMP buffer test, Sikora buffer test, or measures of base saturation. ",
              "These tests indicate not only how much acidity is in soil solution, ",
              " but also how much acidity is bound to soil exchange sites.")))}
pHText2<-{div(p("Below are tables showing lime application rates for Western and Eastern Oregon, based on the SMP buffer test. "),
             p("Additional liming information for the inland PNW can be found through", 
               tags$a(target="_blank", href="http://extension.wsu.edu/publications/pubs/fs212e/?p-page=5","WSU Extension.")))}
LimeTable.Western<-function(SMPValue){
  Table<-data.frame(SMP=seq(6.7, 4.8,-0.1),
                    pH5.6=c(0,0,0,0,0,1.0,1.4,1.7,2.1,2.5,2.8,3.2,3.6,3.9,4.3,4.7,5.0,5.4,5.8,6.2),
                    pH6.0=c(0,0,1.0,1.1,1.5,2.0,2.4,2.9,3.3,3.7,4.2,4.6,5.1,5.5,6.0,6.4,6.9,7.3,7.7,8.3),
                    pH6.4=c(0,1.0,1.7,2.2,2.7,3.2,3.7,4.2,4.7,5.3,5.8,6.3,6.8,7.3,7.8,8.3,8.9,9.4,9.9,10.4),
                    Flag=rep(0,20))
  Table$Flag[Table$SMP==round(SMPValue,1)]<-1
  rownames(Table)=NULL
  colnames(Table)=c("SMP Test Value","tons/acre to reach pH 5.6","tons/acre to reach pH 6.0","tons/acre to reach pH 6.4","Your Soil")
  return(Table)
}
LimeTable.Eastern<-function(SMPValue){
  Table<-data.frame(SMP=c("6.6+","6.4","6.2","6.0 or below"),
                    pH5.6=c(0,0,1.0,2.0),
                    pH6.4=c(0,1.1,2.0,3.0),
                    Flag=rep(0,4))
  Row<-5-findInterval(round(SMPValue,1),c(6.01,6.2,6.4,6.59),rightmost.closed = TRUE)
  #Row
  if(is.na(Row)) Row<-NA else if(Row==5) Row<-4
  Table$Flag[Row]<-1
  rownames(Table)=NULL
  colnames(Table)=c("SMP Test Value","tons/acre to reach pH 5.6","tons/acre to reach pH 6.4","Your Soil")
  return(Table)
}
ShowLime<-function(CropInput,TestValue=NA){
  pHMin<-CropDemand[CropDemand$Crop2==CropInput,"pH.min"]
  pHMax<-CropDemand[CropDemand$Crop2==CropInput,"pH.max"]
  if(is.na(pHMax)) {
    if(is.na(TestValue)) { Ret=FALSE
    } else if(TestValue<=(pHMin+0.2)){ Ret=TRUE
    } else if(TestValue>(pHMin+0.2) & TestValue<7.8) { Ret=FALSE
    } else if(TestValue>=7.8){ Ret=FALSE} 
  } else if(!is.na(pHMax)){
    if(is.na(TestValue)) { Ret=FALSE
    } else if(TestValue<=(pHMin+0.2)){ Ret=TRUE
    } else if(TestValue>(pHMin+0.2) & TestValue<=(pHMax-0.2)){ Ret=FALSE
    } else if(TestValue>(pHMax-0.2)){ Ret=FALSE}
  }
  return(Ret)
}
#KTable("Blueberries mature",20.3)
#STable("Carrots",NA)
 #SText("Alfalfa irrigated",10)
# KTable("Carrots",75)
#MgText("Carrots");MgText("Alfalfa dryland")
#pHResult("Alfalfa dryland",)
#ShowLime("Alfalfa dryland",NA)
#MgText("Alfalfa irrigated",5)
#LimeTable.Eastern(5.9)

# Choose Priorities Tab ####
Goals<-{list("Carbon"=c("Sequester carbon"="Fn1",""),
             "Moisture"=c("Increase water retention"="Fn2",
                          "Increase water infiltration"="Fn3"),
             "pH"=c("Increase soil pH"="Fn4",
                    "Decrease soil pH"="Fn5"),
             "Fertility"=c("Increase plant nutrients (any)"="Fn6",
                           "Increase phosphorous"="Fn7",
                           "Increase potassium"="Fn8",
                           "Increase calcium"="Fn9",
                           "Increase sulfur"="Fn10",
                           "Increase microbial activity"="Fn11"),
             "Remediation"=c("Reduce salts"="Fn12",
                             "Tie-up heavy metals"="Fn13")
)}
RecomNames<-{list(Fn1="Sequester carbon",Fn2="Increase water retention",Fn3="Increase water infiltration",
                  Fn4="Increase soil pH",Fn5= "Decrease soil pH",Fn6="Increase plant nutrients (any)",
                  Fn7="Increase phosphorous",Fn8="Increase potassium",Fn9="Increase calcium",
                  Fn10="Increase sulfur",Fn11="Increase microbial activity",Fn12="Reduce salts",Fn13="Tie-up heavy metals")
}

# Give Recommendation Tab #####
SequesterC<-function(){
  BCs<-as.character(unlist(BC.df[order(BC.df$sBC100,decreasing=TRUE),"ID"][1:3]))
  Prop<-BC.df[order(BC.df$sBC100,decreasing=TRUE),"sBC100"][1:3]
  Text1<-p("Good choices for carbon sequestration are biochars that have high carbon content, such as wood, and that are thoroughly charred.")
  Text2<-div(p("The biochars from our database with the largest content of highly-charred carbon, (i.e. highest",
               tags$a(target="_blank", href="http://54.186.197.37/tools/properties/", HTML("sBC<sub>100</sub>")), ") include:"),
             tags$ul(
               tags$li(HTML(BCs[1]," (sBC<sub>100</sub> = ",Prop[1],"g/kg)")), 
               tags$li(HTML(BCs[2]," (sBC<sub>100</sub> = ",Prop[2],"g/kg)")), 
               tags$li(HTML(BCs[3]," (sBC<sub>100</sub> = ",Prop[3],"g/kg)"))
             ))
  Text<-div(Text1,Text2)
  return(list(BCs,Text))
}
WaterRet<-function(){
  BCs<-as.character(unlist(BC.df[order(BC.df$PS0.5,decreasing=TRUE),"ID"][1:3]))
  Prop<-BC.df[order(BC.df$PS0.5,decreasing=TRUE),"PS0.5"][1:3]
  Text1<-p("Good choices for moisture retention are biochars with a fine",tags$a(target="_blank", href="http://54.186.197.37/tools/properties/", "particle size"), 
            "which help to create tight pores that can hold water against gravity.")
  Text2<-p("Particle size depends mainly on how the feedstock is prepared, for instance by chipping, grinding, or sieving.")
  Text3<-div(p("The biochars from our database with the most fine particles include:"),
             tags$ul(
               tags$li(HTML(BCs[1]," (",Prop[1],"% of mass has a particle size <0.5 mm.)")), 
               tags$li(HTML(BCs[2]," (",Prop[2],"% of mass has a particle size <0.5 mm.)")), 
               tags$li(HTML(BCs[3]," (",Prop[3],"% of mass has a particle size <0.5 mm.)"))
             ))
  Text<-div(Text1,Text2,Text3)
  return(list(BCs,Text))
}
WaterInf<-function(){
  BCs<-as.character(unlist(BC.df[order(BC.df$PS.mean,decreasing=TRUE),"ID"][1:3]))
  Prop<-BC.df[order(BC.df$PS.mean,decreasing=TRUE),"PS.mean"][1:3]
  Text1<-p("Good choices for increasing infiltration are biochars with a large",
           tags$a(target="_blank", href="http://54.186.197.37/tools/properties/", "particle size"), 
            "which help to create large pores that drain readily.")
  Text2<-p("Particle size depends mainly on how the feedstock is prepared, for instance by chipping, grinding, or sieving.")
  Text3<-div(p("The biochars from our database with the largest mean particle sizes are:"),
             tags$ul(
               tags$li(HTML(BCs[1]," (Mean particle size is ",Prop[1],"mm.)")), 
               tags$li(HTML(BCs[2]," (Mean particle size is ",Prop[2],"mm.)")), 
               tags$li(HTML(BCs[3]," (Mean particle size is ",Prop[3],"mm.)"))
             ))  
  Text<-div(Text1,Text2,Text3)
  return(list(BCs,Text))
}
IncpH<-function(){
  BCs<-as.character(unlist(BC.df[order(BC.df$CCE,decreasing=TRUE),"ID"][1:3]))
  Prop<-BC.df[order(BC.df$CCE,decreasing=TRUE),"CCE"][1:3]
  Text1<-p("Good choices for increasing pH are biochars that have a high ash content and high",
           tags$a(target="_blank", href="http://54.186.197.37/tools/properties/", "liming equivalence."))
  Text2<-p(paste("High-ash biochars are produced by charring at high temperature and in the presence of oxygen, ",
          "or by using feedstocks with lower carbon content, such as food waste."))
  Text3<-div(p("The biochars from our database with the highest liming equivalence include:"),
             tags$ul(
               tags$li(HTML(BCs[1]," (CaCO<sub>3</sub>-eq = ",Prop[1],"%)")), 
               tags$li(HTML(BCs[2]," (CaCO<sub>3</sub>-eq = ",Prop[2],"%)")), 
               tags$li(HTML(BCs[3]," (CaCO<sub>3</sub>-eq = ",Prop[3],"%)"))
             ))
  Text<-div(Text1,Text2,Text3)
  return(list(BCs,Text))
}
DecpH<-function(){
  BCs<-as.character(unlist(BC.df[order(BC.df$pH,decreasing=FALSE),"ID"][1:3]))
  Prop<-BC.df[order(BC.df$pH,decreasing=FALSE),"pH"][1:3]
  Text1<-p(paste("Most raw biochars are alkaline (pH>7), and will not lower soil pH.",
            "However, some low-temperature biochars contain quantities of acidic wood vinegar, a by-product of pyrolysis.",
            "Biochars may also be blended with wood vinegar, compost or bokashi to reduce pH."))
  Text2<-p("If using raw biochar, avoid increasing soil pH by chosing a biochar with an acidic or near-neutral pH.")
  Text3<-div(p("The biochars from our database with the lowest pH include:"),
             tags$ul(
               tags$li(HTML(BCs[1]," (pH = ",Prop[1],")")), 
               tags$li(HTML(BCs[2]," (pH = ",Prop[2],")")), 
               tags$li(HTML(BCs[3]," (pH = ",Prop[3],")"))
             ))     
  Text<-div(Text1,Text2,Text3)
  return(list(BCs,Text))
}
IncNutr<-function(){
  BCs<-as.character(unlist(BC.df[order(BC.df$FertClass,decreasing=TRUE),"ID"][1:3]))
  Prop<-BC.df[order(BC.df$FertClass,decreasing=TRUE),"FertClass"][1:3]
  Text1<-p("Most raw biochars do not provide substantial nitrogen, but some may provide phosphorous, potassium, calcium, magnesium, or sulfur.")
  Text2<-p("The IBI", tags$a(target="_blank", href="http://54.186.197.37/tools/properties/", "Nutrient Classification"),
           "describes the number of nutrient types that a biochar provides at substantial levels.")
  Text3<-div(p("The biochars from our database that provide the most types of nutrients include:"),
             tags$ul(
               tags$li(HTML(BCs[1],"( Fertizer Class = ",Prop[1]," )")),  
               tags$li(HTML(BCs[2],"( Fertizer Class = ",Prop[2]," )")), 
               tags$li(HTML(BCs[3],"( Fertizer Class = ",Prop[3]," )"))
             ))  
  Text<-div(Text1,Text2,Text3)
  return(list(BCs,Text))
}
IncP<-function(){
  BCs<-as.character(unlist(BC.df[order(BC.df$AvP2O5.percent,decreasing=TRUE),"ID"][1:3]))
  Prop<-BC.df[order(BC.df$AvP2O5.percent,decreasing=TRUE),"AvP2O5.percent"][1:3]
  Text<-div(p("The biochars from our database with the highest content of available phosphorous include:"),
             tags$ul(
               tags$li(HTML(BCs[1]," (",Prop[1],"% P<sub>2</sub>O<sub>5</sub>)")), 
               tags$li(HTML(BCs[2]," (",Prop[2],"% P<sub>2</sub>O<sub>5</sub>)")), 
               tags$li(HTML(BCs[3]," (",Prop[3],"% P<sub>2</sub>O<sub>5</sub>)"))
             ))  
  return(list(BCs,Text))
}
IncK<-function(){
  BCs<-as.character(unlist(BC.df[order(BC.df$AvK2O.percent,decreasing=TRUE),"ID"][1:3]))
  Prop<-BC.df[order(BC.df$AvK2O.percent,decreasing=TRUE),"AvK2O.percent"][1:3]
  Text<-div(p("The biochars from our database with the highest content of available potassium include:"),
            tags$ul(
              tags$li(HTML(BCs[1]," (",Prop[1],"% K<sub>2</sub>O )")), 
              tags$li(HTML(BCs[2]," (",Prop[2],"% K<sub>2</sub>O )")), 
              tags$li(HTML(BCs[3]," (",Prop[3],"% K<sub>2</sub>O )"))
            ))         
  return(list(BCs,Text))
}
IncCa<-function(){
  BCs<-as.character(unlist(BC.df[order(BC.df$AvCaO.percent,decreasing=TRUE),"ID"][1:3]))
  Prop<-BC.df[order(BC.df$AvCaO.percent,decreasing=TRUE),"AvCaO.percent"][1:3]
  Text<-div(p("The biochars from our database with the highest content of available calcium include:"),
              tags$ul(
                  tags$li(HTML(BCs[1]," (",Prop[1],"% CaO )")), 
                  tags$li(HTML(BCs[2]," (",Prop[2],"% CaO )")), 
                  tags$li(HTML(BCs[3]," (",Prop[3],"% CaO )"))
              ))  
 return(list(BCs,Text))
}
IncS<-function(){
  BCs<-as.character(unlist(BC.df[order(BC.df$AvS.percent,decreasing=TRUE),"ID"][1:3]))
  Prop<-BC.df[order(BC.df$AvS.percent,decreasing=TRUE),"AvS.percent"][1:3]
  Text<-div(p("The biochars from our database with the highest sulfur include:"),
            tags$ul(
              tags$li(HTML(BCs[1]," (",Prop[1],"% S )")), 
              tags$li(HTML(BCs[2]," (",Prop[2],"% S )")), 
              tags$li(HTML(BCs[3]," (",Prop[3],"% S )"))            ))  
  return(list(BCs,Text))
}
IncMicrobes<-function(){
  BCs<-NULL
  Text<-p(p("Raw biochars are sterile, and generally provide a poor energy source for soil microbes to grow on."),
    p("To stimulate microbial activity, use biochar blended with compost or other labile C and N sources."))
  return(list(BCs,Text))
  }
DecSalts<-function(){
  BCsub<-BC.df[BC.df$EC<=3,]
  BCs<-as.character(unlist(BCsub[order(BCsub$PS.mean,decreasing=TRUE),"ID"][1:3]))
  Prop<-BCsub[order(BCsub$PS.mean,decreasing=TRUE),"PS.mean"][1:3]
  Text1<-p("Good choices for reducing soil salinity are biochars that have low salt content and large particle size to increase water infiltration.")
  Text2<-div(p(paste("Salt content in biochars generally increases with ash content.",
              "The biochars in our database with the largest particle sizes and low salt content (electrical conductivity <3 dS/m) include:")),
             tags$ul(
               tags$li(HTML(BCs[1]," (Mean particle size is ",Prop[1],"mm.)")), 
               tags$li(HTML(BCs[2]," (Mean particle size is ",Prop[2],"mm.)")), 
               tags$li(HTML(BCs[3]," (Mean particle size is ",Prop[3],"mm.)"))
             ))  
  Text<-div(Text1,Text2)
  return(list(BCs,Text))
}
DecMetals<-function(){
  BCs<-as.character(unlist(BC.df[order(BC.df$CCE,decreasing=TRUE),"ID"][1:3]))
  Prop<-BC.df[order(BC.df$CCE,decreasing=TRUE),"CCE"][1:3]
  Text1<-p("Biochar can reduce availability of heavy metals by increasing soil pH, which makes many metals less soluble. Many biochars also directly bind metals.")
  Text2<-p("The biochars in our database have not been evaluated for metal binding ability, but only for pH impacts.")
  Text3<-div(p("Good choices for increasing soil pH include:"),
             tags$ul(
               tags$li(HTML(BCs[1]," (CaCO<sub>3</sub>-eq = ",Prop[1],"%)")), 
               tags$li(HTML(BCs[2]," (CaCO<sub>3</sub>-eq = ",Prop[2],"%)")), 
               tags$li(HTML(BCs[3]," (CaCO<sub>3</sub>-eq = ",Prop[3],"%)"))
             ))     
  Text<-div(Text1,Text2,Text3)
  return(list(BCs,Text))
}
RecomFunctions<-{list(Fn1=SequesterC(),Fn2=WaterRet(),Fn3=WaterInf(),Fn4=IncpH(),Fn5=DecpH(),Fn6=IncNutr(),
                     Fn7=IncP(),Fn8=IncK(),Fn9=IncCa(),Fn10=IncS(),Fn11=IncMicrobes(),Fn12=DecSalts(),Fn13=DecMetals())
}


# Amendment Rate Tab ####
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
SinghBiochars<-{list("Wood"=c("Eucalyptus wood, 450C"="Eucalyptus wood 450C",
                                  "Eucalyptus wood, 550C"="Eucalyptus wood 550C",
                                  "Pine chips, 400C"="Pine chips 400C",
                                  "Pine chips, 550C"="Pine chips 550C",
                                  "Mixed softwood, 550C"="Mixed softwood 550C",
                                  "Mixed softwood, 700C"="Mixed softwood 700C"),
                "Manure"=c("Digestate, 700C"= "Digestate 700C",
                                  "Poultry litter, 550C"="Poultry litter 550C"),
                "Grass"=c("Miscanthus straw, 550C"="Miscanthus straw 550C",
                                   "Miscanthus straw, 700C"="Miscanthus straw 700C",
                                   "Switchgrass, 400C"="Switchgrass 400C",
                                   "Switchgrass, 550C"="Switchgrass 550C",
                                   "Rice husk, 550C"="Rice husk 550C",
                                   "Rice husk, 700C"="Rice husk 700C",
                                   "Wheat straw, 550C"="Wheat straw 550C",
                                   "Wheat straw, 700C"="Wheat straw 700C"),
                "Other"=c("Durian shell, 400C"="Durian shell 400C",
                          "Greenhouse (tomato) waste , 550C"="Greenhouse waste 550C",
                          "Municipal greenwaste, 500C"="Municipal greenwaste 550C")
)}
BiologyText<-{div(p(paste("The benefits shown in the table above relate only to readily-measurable chemical properties of biochars.",
                         "Biochar is also considered to be a soil conditioner, meaning it enhances soil nutrient retention gradually over time.",
                         "These effects, as well as changes in soil biology that may result from biochar addition, are inconsistent and more difficult to predict.",
                         "It is also not possible to predict the synergistic benefits plants may experience from simultaneous changes in soil pH, fertility, moisture retention, and microbial activity.")),
                  p(paste("A sound approach for selecting an amendment rate is to base your decision on readily-measurable properties.",
                          "This will help to ensure predictable benefits from biochar, particularly in the first year following application.")))}
BenefitsTable<-function(BiocharChoice,Units){
  if(Units=="U.S. tons/acre"){ 
    #1 ton = 907.185 kg
    RateTons<-c(0.5,1,2,4,10)
    # sBC100=g C/kg biochar * 907.185kg/ton * Tons * 453.59 g/lb
    StableC<-round(BC.df[BC.df$ID==BiocharChoice,"sBC100"]*907.185*RateTons/435.59,1) #Gives pounds per acre
    # Convert ppm extractable P --> ppm P2O5 --> g/g P2O5 --> pounds P2O5
    AvailP<-round(BC.df[BC.df$ID==BiocharChoice,"AvP.ppm"]* 2.29 / 1000000 * RateTons * 2000,1) #Gives pounds per acre as P2O5
    AvailK<-round(BC.df[BC.df$ID==BiocharChoice,"AvK.ppm"]* 1.20 / 1000000 * RateTons * 2000,1) #Gives pounds per acre as K2O
    AvailS<-round(BC.df[BC.df$ID==BiocharChoice,"AvS.ppm"] / 1000000 * RateTons * 2000,1) #Gives pounds per acre as S
    AvailMg<-round(BC.df[BC.df$ID==BiocharChoice,"AvMg.ppm"]*1.66 / 1000000 * RateTons * 2000,1) #Gives pounds per acre as MgO
    AvailCa<-round(BC.df[BC.df$ID==BiocharChoice,"AvCa.ppm"]/ 1000000 *RateTons * 2000,1) #Gives pounds per acre as CaO
    LimeEq<-round(BC.df[BC.df$ID==BiocharChoice,"CCE"]/100*RateTons * 2000,1) # Gives pounds per acre as lime
    # MW P2O5/MW P2 = 2.29
    # MW K2O/MW K2 = (39.1 * 2 + 16) / (39.1 * 2) =1.20
    # MW MgO/MW Mg = (24.3 + 16) / 24.3 = 1.66
    Table<-data.frame(rbind(StableC,AvailP,AvailK,AvailS,AvailMg,AvailCa,LimeEq))
    rownames(Table)<-c("Stable Carbon (lbs/acre)","Phosphorous (lbs P2O5/acre)","Potassium (lbs K2O/acre)","Sulfur (lbs elemental S/acre)",
                       "Magnesium (lbs MgO/acre)","Calcium (lbs CaO/acre)","Lime (lbs CaCO3/acre)")
    colnames(Table)<-c("0.5 ton/acre","1 ton/acre","2 tons/acre","4 tons/acre","10 tons/acre")
    
  } else if (Units=="metric tons/hectare"){
    RateMg<-c(0.5,1,2,4,10)
    StableC<-BC.df[BC.df$ID==BiocharChoice,"sBC100"]*RateMg #Gives kg/ha
    # Convert ppm extractable P --> ppm P2O5 --> g/g P2O5 --> pounds P2O5
    AvailP<-round(BC.df[BC.df$ID==BiocharChoice,"AvP.ppm"]*2.29/1000000*RateMg*1000,1) #Gives kg/ha as P2O5
    AvailK<-round(BC.df[BC.df$ID==BiocharChoice,"AvK.ppm"]*1.20/1000000*RateMg*1000,1) #Gives kg/ha as K2O
    AvailS<-round(BC.df[BC.df$ID==BiocharChoice,"AvS.ppm"]/1000000*RateMg*1000,1) #Gives kg/ha as S
    AvailMg<-round(BC.df[BC.df$ID==BiocharChoice,"AvMg.ppm"]*1.66/1000000*RateMg*1000,1) #Gives kg/ha as MgO
    AvailCa<-round(BC.df[BC.df$ID==BiocharChoice,"AvCa.ppm"]/1000000*RateMg*1000,1) #Gives kg/ha as CaO
    LimeEq<-round(BC.df[BC.df$ID==BiocharChoice,"CCE"]/100*RateMg*1000,1) # Gives kg/ha as lime
    Table<-data.frame(rbind(StableC,AvailP,AvailK,AvailS,AvailMg,AvailCa,LimeEq))
    rownames(Table)<-c("Stable Carbon (kg/ha)","Phosphorous (kg P2O/ha)","Potassium (kg K2O/ha)","Sulfur (kg elemental S/ha)",
                       "Magnesium (kg MgO/ha)","Calcium (kg CaO/ha)","Lime (kg CaCO3/ha)")
    colnames(Table)<-c("0.5 metric ton/ha","1 metric ton/ha","2 metric tons/ha","4 metric tons/ha","10 metric tons/ha")
  } 
  # } else if (Units=="pounds/100 square feet"){
  #   RateLbs<-c(2.5,5,10,20,50)
  #   # sBC100=g C/kg biochar *(1 kg/2.2 lbs) * RateLbs * (1 lb/453.59 g)
  #   StableC<-round(BC.df[BC.df$ID==BiocharChoice,"sBC100"]/2.2*RateLbs/435.59,1) #Gives pounds per 100 ft2
  #   AvailP<-round(BC.df[BC.df$ID==BiocharChoice,"AvP"]/100*RateLbs,1) #Gives pounds per 100 ft2 as P2O5
  #   AvailK<-round(BC.df[BC.df$ID==BiocharChoice,"AvK"]/100*RateLbs,1) #Gives pounds per 100 ft2 as K2O
  #   AvailS<-round(BC.df[BC.df$ID==BiocharChoice,"AvS"]/100*RateLbs,1) #Gives pounds per 100 ft2 as S
  #   AvailMg<-round(BC.df[BC.df$ID==BiocharChoice,"AvMg"]/100*RateLbs,1) #Gives pounds per 100 ft2 as MgO
  #   AvailCa<-round(BC.df[BC.df$ID==BiocharChoice,"AvCa"]/100*RateLbs,1) #Gives pounds per 100 ft2 as CaO
  #   LimeEq<-round(BC.df[BC.df$ID==BiocharChoice,"CCE"]/100*RateLbs,1) # Gives pounds per 100 ft2 as lime
  #   Table<-data.frame(rbind(StableC,AvailP,AvailK,AvailS,AvailMg,AvailCa,LimeEq))
  #   rownames(Table)<-c("Stable C","Phosphorous (as P2O5)","Potassium (as K2O)","Sulfur (elemental)","Magnesium (as MgO)","Calcium (as CaO)","Lime (as CaCO3)")
  #   colnames(Table)<-RateLbs
  # }
  return(Table)
}
#BenefitsTable("Poultry Litter Pellets 350 C","U.S. tons/acre")

# BiocharChoice="Rice husk 550C";Units="U.S. tons/acre";CropInput="Alfalfa irrigated";PTestValue=50
#KTestValue=50;STestValue=50;pH=6.8;CEC=112
BenefitsTable2<-function(BiocharChoice,Units,CropInput,PTestValue,KTestValue,STestValue,pH,CEC){
    RateTons<-c(0.5,1,2,4,10)
    
    PCutoffs<-CropDemand[CropDemand$Crop2==CropInput,c("Plow.cutoff","Pmed.cutoff","Phigh.cutoff","Pvhigh.cutoff")]
    PRates<-CropDemand[CropDemand$Crop2==CropInput,c("Plow.rate","Pmed.rate","Phigh.rate","Pvhigh.rate","Pabove.rate")]
    PRequir<-as.character(unlist(PRates[FindLevel(PCutoffs,PTestValue)]))
    PRequir.num<-as.numeric(unlist(strsplit(PRequir,"-")))
    
    KCutoffs<-CropDemand[CropDemand$Crop2==CropInput,c("Klow.cutoff","Kmed.cutoff","Khigh.cutoff","Kvhigh.cutoff")]
    KRates<-CropDemand[CropDemand$Crop2==CropInput,c("Klow.rate","Kmed.rate","Khigh.rate","Kvhigh.rate","Kabove.rate")]
    KRequir<-as.character(unlist(KRates[FindLevel(KCutoffs,KTestValue)]))
    KRequir.num<-as.numeric(unlist(strsplit(KRequir,"-")))
    
    if(!is.na(CropDemand[CropDemand$Crop2==CropInput,"Slow.cutoff"])){
    SCutoffs<- Cutoffs<-CropDemand[CropDemand$Crop2==CropInput,c("Slow.cutoff","Smed.cutoff","Shigh.cutoff")]
    SRates<-CropDemand[CropDemand$Crop2==CropInput,c("Slow.rate","Smed.rate","Shigh.rate","Svhigh.rate")]
    SRequir<-as.character(unlist(SRates[FindLevel(SCutoffs,STestValue)]))
    SRequir.num<-as.numeric(unlist(strsplit(SRequir,"-")))} else 
      if(CropDemand[CropDemand$Crop2==CropInput,"S.singlerate"]!="") {
        SRequir<-as.character(unlist(CropDemand[CropDemand$Crop2==CropInput,"S.singlerate"]))
        SRequir.num<-as.numeric(unlist(strsplit(SRequir,"-")))} else SRequir.num<-0
      
    if(CropDemand[CropDemand$Crop2==CropInput,"Mg.rate"]!="") {
      MgRequir<-as.character(unlist(CropDemand[CropDemand$Crop2==CropInput,"Mg.rate"]))
      MgRequir.num<-as.numeric(unlist(strsplit(MgRequir,"-")))
      } else MgRequir.num<-0
    
    ## Make table of requirements. 
    Requirements<-data.frame(Element=c("P","K","S","Mg"),
                    Min=c(min(PRequir.num),min(KRequir.num),min(SRequir.num),min(MgRequir.num)),
                    Max=c(max(PRequir.num),max(KRequir.num),max(SRequir.num),max(MgRequir.num)))
    
    
    #Divide Table 1 by requirement table, report as percentage ##
    Benefits<-BenefitsTable(BiocharChoice,Units)[-c(1,6,7),]
    PercMin<-as.matrix(round(Benefits/Requirements$Max*100,0))
    PercMax<-as.matrix(round(Benefits/Requirements$Min*100,0))
    PercMin[PercMin>100]<-100
    PercMax[PercMax>100]<-100
    PercRange<-PercMin
    PercRange[which(PercMin!=PercMax)]<-paste(PercMin[which(PercMin!=PercMax)],"-",PercMax[which(PercMin!=PercMax)],sep="")
    PercRange[Requirements$Max==0,]<-"Not required"
    
    rownames(PercRange)<-c("Phosphorous","Potassium","Sulfur","Magnesium")
  return(PercRange)
}
#BenefitsTable2("Poultry Litter Pellets 350 C","U.S. tons/acre","Alfalfa irrigated",50,50,50,6.8,112)


# RShiny Functions ####

# ui ####
ui <- navbarPage(theme = shinytheme("spacelab"),
    id="NavBar",
     title="",position="fixed-top",
     header=tags$style(type="text/css", "body {padding-top: 60px;}"),
     {tabPanel(strong("Instructions"),value="Home",
                h1("Biochar Selection Tool"),
               wellPanel(
                 h2("How to Use This Tool"),
                 div(p("Progress through the tabs on the top bar to select the best type of biochar and amendment rate:"),
                     tags$ul(
                       tags$li("First, enter data about your soil's properties."), 
                       tags$li("Read about potential deficiencies in your soil that biochar could ameliorate."), 
                       tags$li("Based on your soil's needs, select goals you would like to achieve through biochar amendment."),
                       tags$li("View the PNW biochars in our database that best meet your goals."),
                       tags$li("Finally, determine the biochar amendment rate that most closely meets your goals.")
                     ))),
               fluidRow(column(width=3,offset=6,actionButton(inputId="GetStarted",label="Get Started")))
      )},
      {tabPanel(strong("Soil Properties"),value="Panel1",
              h2("Step 1: Provide as much information as possible from your soil testing or the ", 
                   tags$a(target="_blank", href="http://54.186.197.37/tools/soil-data-explorer/", tags$span("soil explorer map.", style="color:#1E90FF"))),
              em("Suggestion: populate data from soil map first and then edit to match test results."),
              br(),br(),
              fluidRow(column(width=3,actionButton(inputId="ShowSoil",label="Populate from soil map",style = "color: white; 
                     background-color: #00008B;"))),br(),
              SoilDataEntry(),
              hr(),
              fluidRow(column(width=3,actionButton(inputId="ShowInstr",label="Back: Show instructions")),
                       column(width=3,offset=6,actionButton(inputId="ShowInterp",label="Next step: View interpretation")))
                 )}, 
      {tabPanel(strong("Soil Interpretation"),value="Panel2",
            h2("Step 2: Read about the test results to determine your soil's limitations."),
            em("Click through each soil property listed in the sidebar."),tags$hr(),
             navlistPanel(id="SoilInterpPanel",
              {tabPanel("Carbon",
                       h4("Carbon"),
                       p(CarbonText1),
                       fluidRow(column(dataTableOutput("CTable"), width = 4)),
                       br(),
                       p(CarbonText2),br(), #,
                       fluidRow(column(width=3,offset=0,actionButton(inputId="ShowFertil",label="Next: Fertility",style = "color: white; 
                     background-color: #00008B;"
                       )))
              )},
              {tabPanel("Fertility",
                      h4("Fertility"),
                       p(paste("Below are fertilizer recommendations published by Oregon State University for some field and vegetable crops.",
                          "Select a crop to see whether you would benefit from adding nutrients from biochar or other sources.",
                          "Biochars can be a source for phosphorous, potassium, sulfur, and magnesium.")),
                       p(paste("Biochars generally do not provide nitrogen, and due to their high carbon content may reduce nitrogen availability in soil.",
                          "It may be necessary to provide additional nitrogen when applying biochar.")),
                       selectInput("Crop","Select a Crop",Crops),
                       p(em("Note: These recommendations are shown for illustrative purposes only. You should also consult a local extension office or farm advisor for more detailed recommendations.")),
                       strong("Use the tables below to see how much of each nutrient is needed for the crop you chose. Your soil level is highlighted."),
                       fluidRow(column(dataTableOutput("PInterp"), width = 5)), br(),
                       fluidRow(column(dataTableOutput("KInterp"), width = 5)), br(),
                       conditionalPanel("output.slogic == false", 
                              fluidRow(column(dataTableOutput("SInterp"),width=5))), br(),
                       htmlOutput("SInterpTxt"), br(),
                       strong("Magnesium",style="color:black"),
                       htmlOutput("MgInterp"),br(),
                       strong("Reference",style="color:black"),
                       htmlOutput("CropRef"),br(),
                      fluidRow(column(width=3,offset=0,actionButton(inputId="ShowAcid",label="Next: Acidity")))
              )},
              {tabPanel("Acidity",
                       h4("Soil Acidity"),
                       htmlOutput("pHInterp"),br(),
                      conditionalPanel("output.phlogic == true", 
                          pHText2,br(),
                          fluidRow(column(dataTableOutput("SMP.Western"), width = 5)),
                          p(paste("Reference: Table 3 in 'Applying Lime to Raise Soil pH for Crop Production (Western Oregon)",
                              "(2013) N.P. Anderson et al., OSU Extension Publication EM 9057")),br(),
                          fluidRow(column(dataTableOutput("SMP.Eastern"), width = 5)),
                          p(paste("Reference: Table 4 in 'Eastern Oregon Liming Guide",
                              "(2013) D.M. Sullivan et al., OSU Extension Publication EM 9060")),br()),
                          fluidRow(column(width=3,offset=0,actionButton(inputId="ShowMoist",label="Next: Moisture")))
               )},
              {tabPanel("Moisture", 
                       h4("Soil Moisture"),
                       p(MoistureText1),
                       conditionalPanel("input.Sand + input.Silt + input.Clay > 0", htmlOutput("TextureText")),
                       p(MoistureText2),
                       fluidRow(column(dataTableOutput("TextureTable"), width = 5)),
                       p(MoistureText3),
                       br())}),
            hr(),
            fluidRow(column(width=3,actionButton(inputId="BackInput",label="Last step: Input data")),
                     column(width=3,offset=6,actionButton(inputId="ShowGoals",label="Next step: Choose biochar goals")))
             )},
      {tabPanel(strong("Biochar Goals"), value="Panel3",
               h2("Step 3: Choose three goals for applying biochar."),  tags$hr(),
               selectInput("Priority1",strong("First Priority"),Goals),
               selectInput("Priority2",strong("Second Priority"),Goals),
               selectInput("Priority3",strong("Third Priority"),Goals),
               hr(),
               fluidRow(column(width=3,actionButton(inputId="BackInterp",label="Last step: Soil interpretation")),
                        column(width=3,offset=6,actionButton(inputId="ShowBiochars",label="Next step: View recommendations"))),br()
                )},
      {tabPanel(strong("Recommendations"), value="Panel4",
              h2("Step 4: Read about which biochars can meet your goals."), 
              em("Click through each goal in the sidebar."),
              em("See more about these biochars using the",
                tags$a(target="_blank", href="http://54.186.197.37/tools/properties/", tags$em("Biochar Property Explorer."))),tags$hr(),
              navlistPanel(id="BiocharRecomPanel",  
                {tabPanel("First Priority",htmlOutput("Prior1"),htmlOutput("Reco1"))},
                {tabPanel("Second Priority",htmlOutput("Prior2"),htmlOutput("Reco2"))},
                {tabPanel("Third Priority",htmlOutput("Prior3"),htmlOutput("Reco3"))},
                {tabPanel("Summary",
                    h4("Putting it together"),
                    p(paste("A single biochar may not meet all of your goals.",
                      "Here is a summary of how the biochars in our database meet your needs.")),
                    p(paste("How do you choose? The ranking suggested below is based on assigning 3 points to biochars that meet your first priority,",
                            "2 points to those meeting your second priority, and 1 point to those meeting your third priority.")),
                    fluidRow(column(dataTableOutput("RecomTbl"), width = 8)),br()
                )}),
              hr(),
              fluidRow(column(width=3,actionButton(inputId="BackGoals",label="Last step: Choose goals")),
                       column(width=3,offset=6,actionButton(inputId="ShowAmendRate",label="Next step: Determine amendment rate")))
          )},
     
      {tabPanel(strong("Amendment Rates"), value="Panel5",
              h2("Step 5: Determine an appropriate amendment rate."),  tags$hr(),
              navlistPanel(id="AmendRatePanel",  
                {tabPanel("Choose a biochar",
                  h4("Estimated Carbon, Fertility, and Liming Benefits"),
                  p("Select a biochar and use the table below to see estimated benefits at several amendment rates."),
                  p("Visit the next tab to compare the benefits provided to your crop requirements."),
                  selectInput("Biochar","Select a Biochar",Biochars),
                  selectInput("AmendmentUnits","Select amendment rate units",list("U.S. tons/acre","metric tons/hectare")),
                  strong("Stable carbon, nutrients, and lime equivalents provided by this biochar.",style="color:black"),
                  fluidRow(column(dataTableOutput("BiocharRates1"), width = 8)),br(),
                  h4("Soil Conditioning Impacts"),
                  BiologyText,br()
                  )},
                {tabPanel("Choose a crop",
                  h4("Meeting Anticipated Crop Requirements"),
                  selectInput("Crop2","Update Crop Choice",Crops),
                  p("Here are estimates for the percentage of fertilizer requirements that could be met with this biochar amendment."),
                  p("'Not required' indicates that your test levels are high or the crop does not require additions of that nutrient."),
                  fluidRow(column(dataTableOutput("BiocharRates2"), width = 7)),br(),
                  h4("How to Use this Information"),
                  p(paste("You will probably find that most biochars meets only a small fraction of crop nutrient requirements.",
                          "Although there are some exceptions, most biochars have low fertility value, and have to be blended with compost or other fertility sources.")),
                  p(paste("Use this information to guide your experimentation with biochar.",
                    "These tables can help you anticipate the additional nutrients you will need to add with biochar, and to estimate how soil pH, carbon content, and water may also be impacted.",
                    "Although every biochar has somewhat unique properties, the biochars shown here should help you anticipate the range of benefits you might expect.")),
                  br(),br()
                  )}#,
                #{tabPanel("Estimate Soil Water Impacts")
              ),
              fluidRow(column(width=3,actionButton(inputId="BackBiochars",label="Last step: View recommendations")))
        )} 
)

#server ####
server <- function(input, output,session) {

#Home Panel ####
  observeEvent(input$GetStarted, {
    updateNavbarPage(session, "NavBar",
                     selected = "Panel1")
  })
  
#Tab panel 1, Enter soil data ####
  observeEvent(input$ShowInstr, {
    updateNavbarPage(session, "NavBar",
                     selected = "Home")
  })
  observeEvent(input$ShowInterp, {
    updateNavbarPage(session, "NavBar",
                      selected = "Panel2")
  })
    
  observeEvent(input$ShowSoil,{
    updateNumericInput(session,"Sand",value=34)
    updateNumericInput(session,"Silt",value=48)
    updateNumericInput(session,"Clay",value=18)
    updateNumericInput(session,"OM",value=2)
    updateNumericInput(session,"SoilP",value=57)
    updateNumericInput(session,"SoilK",value=168)
    updateNumericInput(session,"SoilS",value=23)
    updateNumericInput(session,"SoilMg",value=144)
    updateNumericInput(session,"SoilMgCEC",value=11.6)
    updateNumericInput(session,"SoilpH",value=5.2)
    updateNumericInput(session,"SoilSMP",value=5.9)
    updateNumericInput(session,"SoilCEC",value=10.2)
    
  })   

#Tab panel 2, Soil Interpretation ####
  #Initialize reactive values to hold text for output
  SoilInterp<-reactiveValues(Texture="Nada",Sulfur="Nada",Mg="Nada",pH="Nada",CropRef="Nada")
  
  #Nav buttons #
  observeEvent(input$ShowFertil, {
    updateNavlistPanel(session, inputId="SoilInterpPanel",
                     selected = "Fertility")
  })
  
  observeEvent(input$BackC, {
    updateNavlistPanel(session, "SoilInterpPanel",
                       selected = "Carbon")
  })
  
  observeEvent(input$ShowAcid, {
    updateNavlistPanel(session, "SoilInterpPanel",
                       selected = "Acidity")
  })
  
  observeEvent(input$BackFertil, {
    updateNavlistPanel(session, "SoilInterpPanel",
                       selected = "Fertility")
  })
  
  observeEvent(input$ShowMoist, {
    updateNavlistPanel(session, "SoilInterpPanel",
                       selected = "Moisture")
  })
  
  GetTexture<-reactive({
    if(sum(input$Clay,input$Silt,input$Sand)==100) Class<-Texture(input$Clay,input$Silt,input$Sand) else Class<-"ERROR"
    return(Class) 
  })
  output$TextureText<-renderUI({
    SoilInterp$Texture<-GetTexture()
    if(sum(input$Clay,input$Silt,input$Sand)==100) {
    strong(paste("Your soil classifies as a ",SoilInterp$Texture,".",sep="")) } else { #,style="color:#FF4500"
      strong("Sand, silt, and clay content should sum to 100%. Check input.") #,style="color:#FF4500"
    }
  })
  output$TextureTable<-renderDataTable({
    datatable(PAWTable(SoilInterp$Texture), class="compact",rownames=FALSE,
              caption= htmltools::tags$caption(
                style = 'caption-side: left; text-align: left; color:black; 
                font-size:100% ;', tags$b('Available Water for Different Soil Textures.')), 
              options=list(dom='t',ordering=F, autoWidth = TRUE,pageLength=12,columnDefs=list(list(width = '200px', targets = c(0,1)),
                                                                    list(visible=FALSE,targets=2),
                                                                    list(className = 'dt-center', targets = 1)))) %>% formatStyle(
                                                                      'Your Soil', target = 'row',
                                                                      backgroundColor = styleEqual(c(0, 1), c('none', 'yellow')))
  })
  
  output$CTable<-renderDataTable({
    datatable(CarbonTable(input$OM), class="compact",rownames=FALSE,
              caption= htmltools::tags$caption(
                style = 'caption-side: left; text-align: left; color:black; 
                font-size:100% ;', tags$b('Typical Soil Organic Matter Contents.')), 
              options=list(dom='t',ordering=F, autoWidth = TRUE,columnDefs=list(list(width = '200px', targets = c(0,1)),
                                                                                  list(visible=FALSE,targets=2),
                                                                                  list(className = 'dt-center', targets = 1)))) %>% formatStyle(
                                                                                    'Your Soil', target = 'row',
                                                                                    backgroundColor = styleEqual(c(0, 1), c('none', 'yellow')))
  })
  
  output$PInterp<-renderDataTable({
    datatable(PTable(input$Crop,input$SoilP), class="compact",rownames=FALSE,
              caption= htmltools::tags$caption(
                style = 'caption-side: left; text-align: left; color:black; 
                font-size:100% ;', tags$b('Phosphorous')), 
              options=list(dom='t',ordering=F, autoWidth = TRUE,columnDefs=list(list(width = '200px', targets = c(0,1,2)),
                                                                    list(visible=FALSE,targets=3),
                                                                    list(className = 'dt-center', targets = c(1,2))))) %>% formatStyle(
                                                                      'Your Soil', target = 'row',
                                                                     backgroundColor = styleEqual(c(0, 1), c('none', 'yellow')))
  })    
  output$KInterp<-renderDataTable({
    datatable(KTable(input$Crop,input$SoilK), class="compact",rownames=FALSE,
              caption= htmltools::tags$caption(
                style = 'caption-side: left; text-align: left; color:black; 
                font-size:100% ;', tags$b('Potassium')), 
              options=list(dom='t',ordering=F, autoWidth = TRUE,columnDefs=list(list(width = '200px', targets = c(0,1,2)),
                                                                    list(visible=FALSE,targets=3),
                                                                    list(className = 'dt-center', targets = c(1,2))))) %>% formatStyle(
                                                                      'Your Soil', target = 'row',
                                                                      backgroundColor = styleEqual(c(0, 1), c('none', 'yellow')))
  })   
  output$SInterp<-renderDataTable({
    if(is.null(STable(input$Crop,input$SoilS))){
      datatable(STable(input$Crop,input$SoilS), class="compact",rownames=FALSE,
                caption= htmltools::tags$caption(
                  style = 'caption-side: left; text-align: left; color:black; 
                  font-size:100% ;', tags$b('Sulfur')), 
                options=list(dom='t',ordering=F, autoWidth = TRUE,columnDefs=list(list(width = '200px', targets = c(0,1,2)),
                                                                      list(visible=FALSE,targets=3),
                                                                     list(className = 'dt-center', targets = c(1,2)))))} else {
    datatable(STable(input$Crop,input$SoilS), class="compact",rownames=FALSE,
              caption= htmltools::tags$caption(
                style = 'caption-side: left; text-align: left; color:black; 
                font-size:100% ;', tags$b('Sulfur')),
              options=list(dom='t',ordering=F, autoWidth = TRUE,columnDefs=list(list(width = '200px', targets = c(0,1,2)),
                                                                    list(visible=FALSE,targets=3),
                                                                    list(className = 'dt-center', targets = c(1,2))))) %>% formatStyle(
                                                                      'Your Soil', target = 'row',
                                                                      backgroundColor = styleEqual(c(0, 1), c('none', 'yellow'))) }
  })     
  
  output$slogic <- reactive({
    is.null(STable(input$Crop,input$SoilS))
  })
  
  outputOptions(output, "slogic", suspendWhenHidden = FALSE)
  
  GetSulfurText<-reactive({
    Temp<-SText(input$Crop,input$SoilS)
    return(Temp) 
  })
  output$SInterpTxt<-renderUI({SoilInterp$Sulfur<-GetSulfurText()
    div(SoilInterp$Sulfur)
  })
  
  GetMgText<-reactive({
    Temp<-MgText(input$Crop,input$SoilMgCEC,input$SoilCEC)
    return(Temp) 
  })
  output$MgInterp<-renderUI({SoilInterp$Mg<-GetMgText()
    div(SoilInterp$Mg)
  })
  
  GetCropRef<-reactive({
    Temp<-CropRef(input$Crop)
    return(Temp)
  })
  
  output$CropRef<-renderUI({SoilInterp$CropRef<-GetCropRef()
    div(SoilInterp$CropRef)})
  
  GetpHTest<-reactive({
    if(is.na(input$SoilpH)) Temp<-em("No soil pH provided.") else Temp<-pHResult(input$Crop,input$SoilpH)
    return(Temp) 
  })
  output$pHInterp<-renderUI({SoilInterp$pH<-GetpHTest()
  div(SoilInterp$pH)
  })
  
  output$SMP.Western<-renderDataTable({
    datatable(LimeTable.Western(input$SoilSMP), class="compact",rownames=FALSE,
              caption= htmltools::tags$caption(
                style = 'caption-side: left; text-align: left; color:black; 
                font-size:100% ;', tags$b('Lime requirement test (SMP) interpretation for Western Oregon.')),
                options=list(dom='t',ordering=F, autoWidth = TRUE,pageLength=20, columnDefs=list(list(width = '100px', targets = c(0,1,2,3)),
                                                                    list(visible=FALSE,targets=4),
                                                                    list(className = 'dt-center', targets = c(0,1,2,3))))) %>% formatStyle(
                                                                    'Your Soil', target = 'row',
                                                                     backgroundColor = styleEqual(c(0, 1), c('none', 'yellow')))                                                                    
   
  })
  
  output$SMP.Eastern<-renderDataTable({
    datatable(LimeTable.Eastern(input$SoilSMP), class="compact",rownames=FALSE,
              caption= htmltools::tags$caption(
                style = 'caption-side: left; text-align: left; color:black; 
                font-size:100% ;', tags$b('Lime requirement test (SMP) interpretation for Eastern Oregon.')),
              options=list(dom='t',ordering=F, autoWidth = TRUE,columnDefs=list(list(width = '100px', targets = c(0,1,2)),
                                                                    list(visible=FALSE,targets=3),
                                                                    list(className = 'dt-center', targets = c(0,1,2))))) %>% formatStyle(
                                                                      'Your Soil', target = 'row',
                                                                      backgroundColor = styleEqual(c(0, 1), c('none', 'yellow')))
    
  })
  
  output$phlogic <- reactive({
    ShowLime(input$Crop,input$SoilpH)
  })
  
  outputOptions(output, "phlogic", suspendWhenHidden = FALSE)
  
  observeEvent(input$ShowGoals, {
    updateNavbarPage(session, "NavBar",
                     selected = "Panel3")
  })
  
  observeEvent(input$BackInput, {
    updateNavbarPage(session, "NavBar",
                     selected = "Panel1")
  })
  
#Tab panel 3, Choose goals ####
  observeEvent(input$ShowBiochars, {
    updateNavbarPage(session, "NavBar",
                     selected = "Panel4")
  })
  
  observeEvent(input$BackInterp, {
    updateNavbarPage(session, "NavBar",
                     selected = "Panel2")
  })
#Tab panel 4, Recommendations ####
  GetReco1<-reactive({
    Temp<-eval(RecomFunctions[input$Priority1])
    names(Temp)<-NULL
    return(Temp[[1]][2]) 
  })
  
  GetReco2<-reactive({
    Temp<-eval(RecomFunctions[input$Priority2])
    names(Temp)<-NULL
    return(Temp[[1]][2]) 
  })
  
  GetReco3<-reactive({
    Temp<-eval(RecomFunctions[input$Priority3])
    names(Temp)<-NULL
    return(Temp[[1]][2]) 
  })
  
  FindCommonChars<-reactive({
    Temp1<-as.character(unlist(eval(RecomFunctions[input$Priority1])[[1]][1]))
    Temp2<-as.character(unlist(eval(RecomFunctions[input$Priority2])[[1]][1]))
    Temp3<-as.character(unlist(eval(RecomFunctions[input$Priority3])[[1]][1]))
    All<-sort(unique(c(Temp1,Temp2,Temp3)))
    Table<-data.frame(All,Priority1=rep(0,length(All)),Priority2=rep(0,length(All)),Priority3=rep(0,length(All)),Rank=rep(0,length(All)))
    Table$Priority1[which(All %in% Temp1)]<-1
    Table$Priority2[which(All %in% Temp2)]<-1
    Table$Priority3[which(All %in% Temp3)]<-1
    Score<-Table$Priority1*3+Table$Priority2*2+Table$Priority3
    Best<-order(Score,decreasing=TRUE)[1:3]
    Table[Best,"Rank"]<-c("First","Second","Third")
    Table[Table==0]<-""
    Table[Table==1]<-"X"
    colnames(Table)<-c("Biochars","Priority 1","Priority 2","Priority 3","Rank")
    return(Table)
  })
  
  Recommendations<-reactiveValues(text1="Nada",text2="Nada",text3="Nada")
  
  output$Prior1<-renderUI({div(h4(paste("Priority 1:",RecomNames[input$Priority1])))
  })
  output$Reco1<-renderUI({Recommendations$text1<-GetReco1()
    div(Recommendations$text1)
  })
  
  output$Prior2<-renderUI({div(h4(paste("Priority 2:",RecomNames[input$Priority2])))
  })
  output$Reco2<-renderUI({Recommendations$text2<-GetReco2()
    div(Recommendations$text2)
  })
  
  output$Prior3<-renderUI({div(h4(paste("Priority 3:",RecomNames[input$Priority3])))
  })
  output$Reco3<-renderUI({Recommendations$text3<-GetReco3()
      div(Recommendations$text3)
  })
  
  output$RecomTbl<-renderDataTable({
      datatable(FindCommonChars(), class="compact",rownames=FALSE,
                caption= htmltools::tags$caption(
                  style = 'caption-side: left; text-align: left; color:black; 
                  font-size:100% ;', tags$b('Biochar Recommendations')),
                options=list(dom='t',ordering=F,autoWidth = TRUE,columnDefs=list(list(width = '100px', targets = 0),
                                                                      list(width = '40px', targets = c(1,2,3,4)),
                                                                      list(className = 'dt-center', targets = c(1,2,3))))) 
    #%>% formatStyle('Your Soil', target = 'row', backgroundColor = styleEqual(c(0, 1), c('none', 'yellow')))
    })  
 
  observeEvent(input$ShowAmendRate, {
    updateNavbarPage(session, "NavBar",
                     selected = "Panel5")
  })
  
  observeEvent(input$BackGoals, {
    updateNavbarPage(session, "NavBar",
                     selected = "Panel3")
  })
  
  
#Tab panel 5, Amendment Rates ####
  #renderTable({BenefitsTable(input$Biochar,input$AmendmentUnits)},include.rownames=TRUE)
  output$BiocharRates1<-renderDataTable({ 
    datatable(BenefitsTable(input$Biochar,input$AmendmentUnits), class="compact stripe",rownames=TRUE,
            options=list(dom='t',ordering=F,autoWidth = TRUE,columnDefs=list(list(width = '120px', targets = 0),
                                                                  list(width = '80px', targets = c(1,2,3,4,5)),
                                                                  list(className = 'dt-center', targets = c(1,2,3,4,5)))))
    })
  output$BiocharRates2<-renderDataTable({ 
    datatable(BenefitsTable2(input$Biochar,input$AmendmentUnits,input$Crop2,input$SoilP,input$SoilK,input$SoilS,input$SoilpH,input$SoilCEC), 
              class="compact stripe",rownames=TRUE,
              caption= htmltools::tags$caption(
                style = 'caption-side: left; text-align: left; color:black; 
                font-size:100% ;', tags$b('Percent of Crop Fertilizer Requirement Met by Biochar')),
              options=list(dom='t',ordering=F,autoWidth = TRUE,columnDefs=list(list(width = '120px', targets = 0),
                                                                    list(width = '80px', targets = c(1,2,3,4,5)),
                                                                    list(className = 'dt-center', targets = c(1,2,3,4,5)))))
  })

  observe({
    x <- input$Crop
    updateSelectInput(session, "Crop2", selected = x)
  })
  
  observe({
    x <- input$Crop2
    updateSelectInput(session, "Crop", selected = x)
  })

  observeEvent(input$BackBiochars, {
    updateNavbarPage(session, "NavBar",
                     selected = "Panel4")
  })
  
}
shinyApp(ui = ui, server = server) 

