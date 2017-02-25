#Version in develop
library(shiny)

###ToDo:

# Revisar el código para optimizar los textos y la salida 
# de las tablas en las que haya cruce de datos, que telita 
# la de mierda que tiene que haber ahí de código sin optimizar


#Data import, refactoring the default "factor" type

#Cities:
cityCSV <- read.csv2("city.csv", header = FALSE, encoding = "UTF-8", sep = ",")
colnames(cityCSV) <- c("ID", "Name", "CountryCode", "Disctrict", "Population")
cityCSV$Name <- as.character(cityCSV$Name)
cityCSV$Disctrict <- as.character(cityCSV$Disctrict)
cityCSV$CountryCode <- as.character(cityCSV$CountryCode)

#Countries:
countryCSV <- read.csv2("country.csv", header = FALSE, encoding = "UTF-8", sep = ",")
colnames(countryCSV) <- c("Code", "Name", "Continent", "Region", "SurfaceArea", "IndepYear", "Population", "LifeExpentancy", "GNP", "GNPOld", "LocalName", "GovernmentForm", "HeadOfState", "Capital", "Code2")
countryCSV$Code <- as.character(countryCSV$Code)
countryCSV$Name <- as.character(countryCSV$Name)
countryCSV$Continent <- as.character(countryCSV$Continent)
countryCSV$Region <- as.character(countryCSV$Region)
countryCSV$SurfaceArea <- as.numeric(as.character(countryCSV$SurfaceArea))
countryCSV$IndepYear <- as.numeric(as.character(countryCSV$IndepYear))
countryCSV$Population <- as.numeric(as.character(countryCSV$Population))
countryCSV$LifeExpentancy <- as.numeric(as.character(countryCSV$LifeExpentancy))
countryCSV$GNP <- as.numeric(as.character(countryCSV$GNP))
countryCSV$GNPOld <- as.numeric(as.character(countryCSV$GNPOld))
countryCSV$LocalName <- as.character(countryCSV$LocalName)
countryCSV$GovernmentForm <- as.character(countryCSV$GovernmentForm)
countryCSV$HeadOfState <- as.character(countryCSV$HeadOfState)
countryCSV$Capital <- as.numeric(as.character(countryCSV$Capital))
countryCSV$Code2<- as.character(countryCSV$Code2)

#Languages:
idiomaCSV <- read.csv2("countrylanguage.csv", header = FALSE, encoding = "UTF-8",sep = ",")
colnames(idiomaCSV) <- c("CountryCode", "Language", "IsOfficial", "Percentage")
idiomaCSV$CountryCode <-as.character(idiomaCSV$CountryCode)
idiomaCSV$Language <- as.character(idiomaCSV$Language)
idiomaCSV$IsOfficial <- as.logical(idiomaCSV$IsOfficial)
idiomaCSV$Percentage <- as.numeric(as.character(idiomaCSV$Percentage))

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
               htmlOutput("relatedDataL"),
               br(),
               dataTableOutput("listResult")),
      tabPanel("Charts:",
               htmlOutput("relatedDataP"),
               br(),
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
      selectInput("despSecondChoice", "Cuontries", choices = sort(data))
      
    }else if(chosenList == "Countries by regions"){
      
      data <- unique(countryCSV$Region)
      selectInput("despSecondChoice", "Regions", choices = sort(data))
      
    }else if(chosenList == "Countries by language"){
      
      idiomas <- unique(idiomaCSV$Language)
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

      datosPais <- subset(countryCSV, Name == sec)
      codigoPais <- as.character(datosPais[1])
      ciudades <- subset(cityCSV, CountryCode == codigoPais)
      
      population <- ciudades$Population
      
    }else if(chosenList == "Countries by regions"){
      
      paisesXregion <- subset(countryCSV, Region == sec)
      population <- paisesXregion$Population
      
    }else if(chosenList == "Countries by language"){
      
      
      codPaisesXidioma <- subset(idiomaCSV, Language == sec)
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
      surface <-paisesXregion$SurfaceArea
      
    }else if(chosenList == "Countries by language"){
      
      codPaisesXidioma <- subset(idiomaCSV, Language == sec)
      codPaisesXidioma <- as.vector(codPaisesXidioma[,1])
      
      paisesXidioma <- subset(countryCSV, Code %in% codPaisesXidioma)
      
      surface <- paisesXidioma$SurfaceArea
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
      GNP <- paisesXregion$GNP
      
      
    }else if(chosenList == "Countries by language"){
      
      codPaisesXidioma <- subset(idiomaCSV, Language == sec)
      codPaisesXidioma <- as.vector(codPaisesXidioma[,1])
      
      paisesXidioma <- subset(countryCSV, Code %in% codPaisesXidioma)
      
      GNP <- paisesXidioma$GNP
    }
    
    if(chosenList != "Cities by countries"){
      
      maximum <- max(GNP)
      minimum <- min(GNP)
      
      sliderInput("slipGNP", label = "GNP", min = minimum, max = maximum, value = c(minimum,maximum))
    }
    
  })
  
  
  ####### Output: result: Table or Plot ####### 
  
  #Result: Country's info - Lo que se cambie aqui cambiar en el de abajo que se usa en los gráficos:
  output$relatedDataL <- renderText({
    
    chosenList <- input$despFirstChoiceL
    secChoice <- input$despSecondChoice
    
    if(chosenList == "Cities by countries"){
      
      chosenCountry <- subset(countryCSV, Name == secChoice)
      codCapital <- chosenCountry$Capital
      capital <- subset(cityCSV, ID == codCapital)
      
    
      HTML(paste("<font size=3>","<b>","About", secChoice,"</b>","</font>", ": ",
                 "<b>","Continent: ","</b>", chosenCountry$Continent,"|",
                 "<b>", "Region: ", "</b>", chosenCountry$Region,"|",
                 "<b>", "Country code: ","</b>"  ,chosenCountry$Code,"|",
                 "<b>", "Capital: ", "</b>", capital$Name,"|",
                 "<b>", "Surface: ", "</b>", chosenCountry$Surface,"|",
                 "<b>", "Population: ", "</b>", chosenCountry$Population,"|",
                 "<b>", "Life expentancy: ", "</b>", chosenCountry$LifeExpentancy
                  )
           )
      
    }else if(chosenList == "Countries by regions"){
      
      #Countries per region:
      chosenRegion <- subset(countryCSV, Region == secChoice)
      
      continent <- unique(chosenRegion$Continent)
      population <- sum(chosenRegion$Population)
      surface <- sum(chosenRegion$SurfaceArea)
      averLifeExpentancy <- mean(chosenRegion$LifeExpentancy,na.rm = TRUE)
      averLifeExpentancy <- round(averLifeExpentancy, digits = 2)
      
      
      HTML(paste("<font size=3>","<b>","About", secChoice,"</b>","</font>", ": ",
                 "<b>","Continent: ","</b>", continent,"|",
                 "<b>","Population: ","</b>", population,"|",
                 "<b>","Surface: ","</b>", surface,"|",
                 "<b>","Average life expentancy: ","</b>", averLifeExpentancy
      ))

      
      #Optimizao
    }else if(chosenList == "Countries by language"){
      
      #All the languages and CodeCountry from the choice
      languages <- subset(idiomaCSV, Language == secChoice)
      
      #Codes for the countries with the language
      codesCountr <- as.vector(languages[,1])
      
      #Countries where that language is spoken:
      countriesByLang <- subset(countryCSV, Code %in% codesCountr)
      
      #Rename to merge
      colnames(languages)[1] <- "Code"
      finalList <- merge(languages, countriesByLang, by = "Code")
      
      finalList$new <- ((finalList$Percentage)/100)*finalList$Population
      
      totalSpeakers <- sum(finalList$new)
      
      #Countries where is official language:
      officialLanguage <- subset(finalList, IsOfficial == TRUE)
      
      officialLanguage <- as.vector(officialLanguage$Name)
      
      if(length(officialLanguage) ==0){
        HTML(
          paste("<font size=3>","<b>","About", secChoice,"</b>","</font>", ": ",
                "<b>", "Not an official language", "</b>", "|",
                "<b>", "Number of speakers: " , "</b>", totalSpeakers 
                
          )
        )
      }else{
        HTML(
          paste("<font size=3>","<b>","About", secChoice,"</b>","</font>", ": ",
                "<b>", "Official in: ", "</b>", paste(officialLanguage, sep ="", collapse = ", "), "|",
                "<b>", "Number of speakers: " , "</b>", totalSpeakers 
                
          )
        )
      }

    }
 
  })
  
  #Result: Country's info, only with "cities by country"
    output$relatedDataP <- renderText({
    
      chosenList <- input$despFirstChoiceL
      secChoice <- input$despSecondChoice
      
      if(chosenList == "Cities by countries"){
        
        chosenCountry <- subset(countryCSV, Name == secChoice)
        codCapital <- chosenCountry$Capital
        capital <- subset(cityCSV, ID == codCapital)
        
        
        HTML(paste("<font size=3>","<b>","About", secChoice,"</b>","</font>", ": ",
                   "<b>","Continent: ","</b>", chosenCountry$Continent,"|",
                   "<b>", "Region: ", "</b>", chosenCountry$Region,"|",
                   "<b>", "Country code: ","</b>"  ,chosenCountry$Code,"|",
                   "<b>", "Capital: ", "</b>", capital$Name,"|",
                   "<b>", "Surface: ", "</b>", chosenCountry$Surface,"|",
                   "<b>", "Population: ", "</b>", chosenCountry$Population,"|",
                   "<b>", "Life expentancy: ", "</b>", chosenCountry$LifeExpentancy
        )
        )
        
      }else if(chosenList == "Countries by regions"){
        
        #Countries per region:
        chosenRegion <- subset(countryCSV, Region == secChoice)
        
        continent <- unique(chosenRegion$Continent)
        population <- sum(chosenRegion$Population)
        surface <- sum(chosenRegion$SurfaceArea)
        averLifeExpentancy <- mean(chosenRegion$LifeExpentancy,na.rm = TRUE)
        averLifeExpentancy <- round(averLifeExpentancy, digits = 2)
        
        
        HTML(paste("<font size=3>","<b>","About", secChoice,"</b>","</font>", ": ",
                   "<b>","Continent: ","</b>", continent,"|",
                   "<b>","Population: ","</b>", population,"|",
                   "<b>","Surface: ","</b>", surface,"|",
                   "<b>","Average life expentancy: ","</b>", averLifeExpentancy
        ))
        
        
        #Optimizao
      }else if(chosenList == "Countries by language"){
        
        #All the languages and CodeCountry from the choice
        languages <- subset(idiomaCSV, Language == secChoice)
        
        #Codes for the countries with the language
        codesCountr <- as.vector(languages[,1])
        
        #Countries where that language is spoken:
        countriesByLang <- subset(countryCSV, Code %in% codesCountr)
        
        #Rename to merge
        colnames(languages)[1] <- "Code"
        finalList <- merge(languages, countriesByLang, by = "Code")
        
        finalList$new <- ((finalList$Percentage)/100)*finalList$Population
        
        totalSpeakers <- sum(finalList$new)
        
        #Countries where is official language:
        officialLanguage <- subset(finalList, IsOfficial == TRUE)
        
        officialLanguage <- as.vector(officialLanguage$Name)
        
        if(length(officialLanguage) ==0){
          HTML(
            paste("<font size=3>","<b>","About", secChoice,"</b>","</font>", ": ",
                  "<b>", "Not an official language", "</b>", "|",
                  "<b>", "Number of speakers: " , "</b>", totalSpeakers 
                  
            )
          )
        }else{
          HTML(
            paste("<font size=3>","<b>","About", secChoice,"</b>","</font>", ": ",
                  "<b>", "Official in: ", "</b>", paste(officialLanguage, sep ="", collapse = ", "), "|",
                  "<b>", "Number of speakers: " , "</b>", totalSpeakers 
                  
            )
          )
        }
        
      }
    
  })
  
  #Result (List): the result, whit the filters, as a list
  output$listResult <- renderDataTable({
    
    chosenList <- input$despFirstChoiceL
    
    if(chosenList == "Cities by countries"){
      pais <- input$despSecondChoice
      countryCSV$Code <- as.character(countryCSV$Code)
      countryCSV$Name <- as.character(countryCSV$Name)
      datosPais <- subset(countryCSV, Name == pais)
      
      codigoPais <- as.character(datosPais[1])
      
      finalList <- subset(cityCSV, CountryCode == codigoPais)
      
      #Erase ID and country code, not relevant:
      finalList <- finalList[,-c(1,3)]
      
    }else if(chosenList == "Countries by regions"){
      
      region <- input$despSecondChoice
      
      finalList <- as.vector(subset(countryCSV, Region == region))
      #Erase continent, region, 
      finalList <- finalList[,-c(3,4,6,10,15)]
      
      #Add capital as name, not code:
      
      codigoCapitales <- as.vector(finalList$Capital)
      codigoCapitales <- as.integer(codigoCapitales)
      capitalName <- subset(cityCSV, ID %in% codigoCapitales)
      
      finalList <- finalList[order(finalList$Code),]
      capitalName <- capitalName[order(capitalName$CountryCode),]
      finalList <- cbind.data.frame(finalList, capitalName$Name)
      finalList$Capital <- NULL
      finalList$Code <- NULL
      names(finalList)[9] <- "Capital"
      
      
    }else if(chosenList == "Countries by language"){
      
      language =input$despSecondChoice
      
      countriesByLanguage <- as.vector(subset(idiomaCSV, Language == language))
      codesCBL <- as.vector(countriesByLanguage[,1])
      
      finalList <- subset(countryCSV, Code %in% codesCBL)
      
      finalList <- finalList[,-c(6,8,10,12,13,15)]
      capitalsCodes <- as.integer(as.vector(finalList$Capital))
      capitalNames <- subset(cityCSV, ID %in% capitalsCodes)
      
      # #Order by code before bind all:
      finalList <- finalList[order(finalList$Code),]
      capitalNames <- capitalNames[order(capitalNames$CountryCode),]
      countriesByLanguage <- countriesByLanguage[order(countriesByLanguage$CountryCode),]

      finalList <- cbind.data.frame(finalList, capitalNames$Name,  countriesByLanguage$Percentage)
      
      finalList$Capital <- NULL
      finalList$Code <- NULL
      
      names(finalList)[8] <- "Capital"
      names(finalList)[9] <- "Percentage"
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
    
  }, options = list(pageLength = 10))
  
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

