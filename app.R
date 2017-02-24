
library(shiny)

#Cities:
cityCSV <- read.csv2("city.csv", header = FALSE, encoding = "UTF-8", sep = ",")
colnames(cityCSV) <- c("ID", "Name", "CountryCode", "Disctrict", "Population")

#Countries:
countryCSV <- read.csv2("country.csv", header = FALSE, encoding = "UTF-8", sep = ",")
colnames(countryCSV) <- c("Code", "Name", "Continent", "Region", "SurfaceArea", "IndepYear", "Population", "LifeExpentancy", "GNP", "GNPOld", "LocalName", "GovernmentForm", "HeadOfState", "Capital", "Code2")

#Languages:
idiomaCSV <- read.csv2("countrylanguage.csv", header = FALSE, encoding = "UTF-8",sep = ",")
colnames(idiomaCSV) <- c("CountryCode", "Language", "IsOfficial", "Percentage")

# Define UI:
ui <-  pageWithSidebar(
         headerPanel("World data consult: "), 
         sidebarPanel(
           selectInput("despFirstChoiceL", "Chose your data", choices = c("Cities by countries", "Countries by regions", "Countries by language")),
           uiOutput("secondChoiceL"),
           br(),
           h4("Filter"),
           uiOutput("slidAnioCompL"),
           uiOutput("slidSurfaceL"),
           uiOutput("slidGNPL")

         ),
         mainPanel(
           tabsetPanel(
             tabPanel("List:",
                      tableOutput("listResult")),
             tabPanel("Charts:",
                      plotOutput("plotResult"))
           )
         
       
      )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  

  ####### Input selections and render: LIST    #######
  
  #Second filter
  output$secondChoiceL <- renderUI({
    
    chosenList <- input$despFirstChoiceL
    
    if(chosenList == "Cities by countries"){

      data <- countryCSV$Name
      
      selectInput("despSecondChoice", "Countries", choices = sort(data))
      
    }else if(chosenList == "Countries by regions"){
      
      data <- unique(countryCSV$Region)

      selectInput("despSecondChoice", "Regions", choices = sort(data))
      
    }else if(chosenList == "Countries by language"){
      
      idiomas <- as.vector(unique(idiomaCSV$Language))
      data <-c()
      for(idioma in idiomas){
        
        if(length(subset(idiomaCSV, Language == idioma)[,1])>=2){
          
          data <-c(data, idioma)

        }
        
      }
      
      selectInput("despSecondChoice", "Languages", choices = sort(data))
      
    }
    

  })
  
  #Population slider: DONE
  output$slidAnioCompL <- renderUI({

    sec <- input$despSecondChoice
    chosenList <- input$despFirstChoiceL
    
    if(chosenList == "Cities by countries"){
      
      countryCSV$Code <- as.character(countryCSV$Code)
      datosPais <- subset(countryCSV, Name == sec)
      
      codigoPais <- as.character(datosPais[1])
      
      ciudades <- subset(cityCSV, CountryCode == codigoPais)
      
      population <- ciudades$Population

    }else if(chosenList == "Countries by regions"){

      paisesXregion <- subset(countryCSV, Region == sec)
      
      population <- paisesXregion$Population
      
    }else if(chosenList == "Countries by language"){
      

      codPaisesXidioma <- as.vector(subset(idiomaCSV, Language == sec))
      codPaisesXidioma <- as.vector(codPaisesXidioma[,1])
      
      paisesXidioma <- subset(countryCSV, Code %in% codPaisesXidioma)
      population <- paisesXidioma$Population
      
    }
    maximum <- max(population)
    minimum <- min(population)
    
    sliderInput("slidAnio", label = "Population", min = minimum, max = maximum, value = c(minimum,maximum))

  })
  
  #Suerface slider: DONE
  output$slidSurfaceL <- renderUI({
    
    
    sec <- input$despSecondChoice
    chosenList <- input$despFirstChoiceL

    if(chosenList == "Countries by regions"){
      
      paisesXregion <- subset(countryCSV, Region == sec)
      
      surface <- as.integer(as.vector(paisesXregion$SurfaceArea))
      
      
    }else if(chosenList == "Countries by language"){
      
      codPaisesXidioma <- as.vector(subset(idiomaCSV, Language == sec))
      codPaisesXidioma <- as.vector(codPaisesXidioma[,1])
      
      paisesXidioma <- subset(countryCSV, Code %in% codPaisesXidioma)
      
      surface <- as.integer(as.vector(paisesXidioma$SurfaceArea))
      
      
    }
    
    if(chosenList != "Cities by countries"){
      
      maximum <- max(surface)
      minimum <- min(surface)
      
      sliderInput("slidSurface", label = "Surface", min = minimum, max = maximum, value = c(minimum,maximum))
      
    }
    
  })
  
  #GNP filter: DONE
  output$slidGNPL <- renderUI({
    
    
    sec <- input$despSecondChoice
    chosenList <- input$despFirstChoiceL
    
    if(chosenList == "Countries by regions"){
      
      paisesXregion <- subset(countryCSV, Region == sec)
      
      GNP <- as.integer(as.vector(paisesXregion$GNP))
      
      
    }else if(chosenList == "Countries by language"){
      
      codPaisesXidioma <- as.vector(subset(idiomaCSV, Language == sec))
      codPaisesXidioma <- as.vector(codPaisesXidioma[,1])
      
      paisesXidioma <- subset(countryCSV, Code %in% codPaisesXidioma)
      
      GNP <- as.integer(as.vector(paisesXidioma$GNP))
      
      
    }
    
    if(chosenList != "Cities by countries"){
      
      maximum <- max(GNP)
      minimum <- min(GNP)
      
      sliderInput("slipGNP", label = "GNP", min = minimum, max = maximum, value = c(minimum,maximum))
      
    }
    
  })


  ####### Output: result: Table or Plot ####### 

  #Result (List): the result, whit the filters, as a list
  output$listResult <- renderTable({
    
    chosenList <- input$despFirstChoiceL
    
    if(chosenList == "Cities by countries"){
      pais <- input$despSecondChoice
      countryCSV$Code <- as.character(countryCSV$Code)
      countryCSV$Name <- as.character(countryCSV$Name)
      datosPais <- subset(countryCSV, Name == pais)
      
      codigoPais <- as.character(datosPais[1])
      
      finalList <- subset(cityCSV, CountryCode == codigoPais)
      
    }else if(chosenList == "Countries by regions"){
      
      region <- input$despSecondChoice
      
      finalList <- as.vector(subset(countryCSV, Region == region))
      
      
    }else if(chosenList == "Countries by language"){
      
      idioma =input$despSecondChoice
      
      codPaisesXidioma <- as.vector(subset(idiomaCSV, Language == idioma))
      codPaisesXidioma <- as.vector(codPaisesXidioma[,1])
      
      finalList <- subset(countryCSV, Code %in% codPaisesXidioma)
      
    }

    #Population filter:
    finalList <- subset(finalList, Population >= input$slidAnio[1] & Population <= input$slidAnio[2])
    
    if(chosenList != "Cities by countries"){
      
      #Pre-treatment:
      finalList$SurfaceArea <- as.numeric(as.character(finalList$SurfaceArea))
      finalList$GNP <- as.numeric(as.character(finalList$GNP))
      
      #Surface filter:
      finalList <- subset(finalList, SurfaceArea >=  input$slidSurface[1] & SurfaceArea <= input$slidSurface[2])
      
      # #GNP filter:
      finalList <- subset(finalList, GNP >= input$slipGNP[1] & GNP <= input$slipGNP[2])
      
    }
    
    finalList
    
  })
  
  #Result (Plot): the result, with the filters, as a chart
  output$plotResult <- renderPlot({
    
    chosenList <- input$despFirstChoiceL
    
    if(chosenList == "Cities by countries"){
      pais <- input$despSecondChoice
      countryCSV$Code <- as.character(countryCSV$Code)
      datosPais <- subset(countryCSV, Name == pais)
      
      codigoPais <- as.character(datosPais[1])
      xlabel <- "Cities"
      
      finalList <- subset(cityCSV, CountryCode == codigoPais)
      
    }else if(chosenList == "Countries by regions"){
      
      region <- input$despSecondChoice
      
      xlabel <- "Countries"
      
      finalList <- as.vector(subset(countryCSV, Region == region))
      
      
    }else if(chosenList == "Countries by language"){
      
      idioma =input$despSecondChoice
      
      codPaisesXidioma <- as.vector(subset(idiomaCSV, Language == idioma))
      codPaisesXidioma <- as.vector(codPaisesXidioma[,1])
      
      
      xlabel <- "Countries"
      finalList <- subset(countryCSV, Code %in% codPaisesXidioma)
      
    }
    
    #Population filter:
    finalList <- subset(finalList, Population >= input$slidAnio[1] & Population <= input$slidAnio[2])
    
    if(chosenList != "Cities by countries"){
      
      #Pre-treatment:
      finalList$SurfaceArea <- as.numeric(as.character(finalList$SurfaceArea))
      finalList$GNP <- as.numeric(as.character(finalList$GNP))
      
      #Surface filter:
      finalList <- subset(finalList, SurfaceArea >=  input$slidSurface[1] & SurfaceArea <= input$slidSurface[2])
      
      # #GNP filter:
      finalList <- subset(finalList, GNP >= input$slipGNP[1] & GNP <= input$slipGNP[2])
      
    }
    #Matrix result:
    ciudades <- finalList
    
    #Range used at the Y axe, adding a 10% margin at the top
    g_range = range(0,max(ciudades$Population)*1.1)
    
    #Plot's Parameters
    par(mar=c(7,8,1,1),mgp=c(5,1,0))
    
    #Bar plot, without axes or labels, will lately be defines
    mp <-barplot(ciudades$Population,border="blue", density=30, angle = 45,ylim = g_range, axes = FALSE, ann = FALSE)
    
    #X axe definition:
    label = as.vector(ciudades$Name)
    axis(1,at = mp, lab=label, las=2)
    
    #Y axe definition:
    incremento <- (g_range[2]-g_range[1])/8
    axis(2, las=1, at=seq(g_range[1],g_range[2], incremento), 
         labels = round(seq(g_range[1],g_range[2], incremento),digits = 1))
    
    #Text on top of teh value:
    text(x = mp, y = ciudades$Population, label = ciudades$Population, pos = 3, cex = 1, col = "blue", srt=45, xpd=TRUE)
    
    #Titles:
    title(xlab = xlabel)
    title(ylab = "Population")
    
    #Box to put all together
    box()
    
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
