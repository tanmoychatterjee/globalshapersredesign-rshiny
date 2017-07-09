#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(hash)
library(rgdal)
library(geojson)
library(stringdist)
library(scales)
library(stringr)
library(gridExtra)
library(ggparallel)
library(RColorBrewer)
library(igraph)
library(GGally)
library(ggnet)
library(network)
library(sna)
library(leaflet)

setwd(".")
df=read.csv(file="GS_AS2016_DataSet.csv",encoding="UTF-8",header=TRUE,stringsAsFactors=FALSE)
df = df[2:nrow(df),]

df$Country <- as.character(df$Country)
df$Country[df$Country == "United States"] = "United States of America"
df$Country[df$Country == "United Kingdom"] = "United Kingdom of Great Britain and Northern Ireland"
df$Country[df$Country == "Bolivia"] = "Bolivia (Plurinational State of)"
df$Country[df$Country == "Brunei"] = "Brunei Darussalam"
df$Country[df$Country == "Russia"] = "Russian Federation"
df$Country[df$Country == "Vietnam"] = "Viet Nam"
df$Country[df$Country == "Venezuela"] = "Venezuela (Bolivarian Republic of)"
df$Country[df$Country == "Tanzania"] = "United Republic of Tanzania"
df$Country[df$Country == "Syria"] = "Syrian Arab Republic"
df$Country[df$Country == "Serbia"] = "Republic of Serbia"
df$Country[df$Country == "South Korea"] = "Republic of Korea"
df$Country[df$Country == "Burma (Myanmar)"] = "Myanmar"
df$Country[df$Country == "Laos"] = "Lao People's Democratic Republic"
df$Country[df$Country == "Iran"] = "Iran (Islamic Republic of)"
df$Country[df$Country == "French Guiana (France)"] = "French Guiana"
df$Country[df$Country == "Cape Verde"] = "Cabo Verde"
df$Country[df$Country == "Ivory Coast"] = "Côte d'Ivoire"
df$Country[df$Country == "Cape Verde"] = "Cabo Verde"
df$Country[df$Country == "Cape Verde"] = "Cabo Verde"
surveycountres = df%>%
  group_by(Country) %>%
  summarize()

countries <- readOGR("world.geo.json")


regions = df %>%
  distinct(Region_UN)
typeof(regions)
regions$Region_UN[1]

regions
subregion_Europe = df%>%
  filter(Region_UN == "Europe") %>%
  distinct(Region_Sub_UN) %>%
  arrange(Region_Sub_UN)
subregion_Africa = df%>%
  filter(Region_UN == "Africa") %>%
  distinct(Region_Sub_UN) %>%
  arrange(Region_Sub_UN)
subregion_Americas = df%>%
  filter(Region_UN == "Americas") %>%
  distinct(Region_Sub_UN)%>%
  arrange(Region_Sub_UN)
subregion_Oceania = df%>%
  filter(Region_UN == "Oceania") %>%
  distinct(Region_Sub_UN) %>%
  arrange(Region_Sub_UN)
subregion_Asia = df%>%
  filter(Region_UN == "Asia") %>%
  distinct(Region_Sub_UN) %>%
  arrange(Region_Sub_UN)
subregion_NA = df%>%
  filter(Region_UN == "#NA") %>%
  distinct(Region_Sub_UN) %>%
  arrange(Region_Sub_UN)


getage = function(input){
  agelist = list("1" = 18, "2" = 22, "3"=27, "4"=31)
  #print(agelisthash[[as.character(input[1])]])
  #print(agelisthash[[as.character(input[2])]])
  ret = ""
  if(input[1] == 0){
    if(input[2] == 1){
      ret = c("18-21")
    }else if(input[2] == 2){
      ret = c("18-21","22-26")
    }else if(input[2] == 3){
      ret = c("18-21","22-26", "27-30")
    }else if(input[2] == 4){
      ret = c("18-21","22-26", "27-30" ,"31-35")
    }
  }else if(input[1] == 1){
    if(input[2] == 2){
      ret = c("22-26")
    }else if(input[2] == 3){
      ret = c("22-26", "27-30")
    }else if(input[2] == 4){
      ret = c("22-26", "27-30", "31-35")
    }
  }else if(input[1] == 2){
    if(input[2] == 3){
      ret = c("27-30")
    }else if(input[2] == 4){
      ret = c("27-30", "31-35")
    }
  }else if(input[1] == 3){
    if(input[2] == 4){
      ret = c("31-35")
    }
  }
  return (ret)
}

getregions = function(input){
  selected = get_selected(input, format = "names")
  return (unlist(selected))
  
}

getIssues = function(input,type){
  colnames(input)
  temp = unlist(strsplit(as.character(colnames(input)[1:length(colnames(input))]),"....", fixed=T))
  is.odd <- function(x) x %% 2 != 0
  temp = temp[!is.odd(seq_along(temp))]
  temp = gsub("\\.\\.\\.", " ", temp)
  temp = gsub("\\.", " ", temp)
  temp
  colnames(input)[1:length(colnames(input))] = temp
  issues_nonadf <-as.data.frame(sapply(input, function(y) sum(length(which(y != "")))))
  colnames(issues_nonadf)[1] = "count"
  issues_nonadf$names<-rownames(issues_nonadf)
  issues_nonadf$type = type
  return (issues_nonadf)
}


get_wraper <- function(width) {
  function(x) {
    lapply(strwrap(x, width = width, simplify = FALSE), paste, collapse="\n")
  }
}
#ref: https://stackoverflow.com/questions/2631780/r-ggplot2-can-i-set-the-plot-title-to-wrap-around-and-shrink-the-text-to-fit-t
wrapper <- function(x, ...) 
{
  paste(strwrap(x, ...), collapse = "\n")
}


# Define server logic
shinyServer(function(input, output, session) {
   
  output$regionsubregion <- renderTree({ 
    
    regionsubregion=list(  'ALL'= list(
      'Africa'   =  structure(list('Eastern Africa' = '1', 'Middle Africa'= '2', 'Northern Africa'= '3', 'Southern Africa'= '4', 'Western Africa'= '5'),stopened=TRUE, stselected=TRUE),
      'Americas'   =  structure(list('Caribbean' = '6', 'Central America'= '7', 'Northern America'= '8', 'South America'= '9'),stopened=TRUE, stselected=TRUE),
      'Asia'   =  structure(list('Central Asia' = '10', 'Eastern Asia'= '11', 'South-Eastern Asia'= '12', 'Southern Asia'= '13', 'Western Asia'= '14'),stopened=TRUE, stselected=TRUE),
      'Europe'   =  structure(list('Eastern Europe' = '15', 'Northern Europe'= '16', 'Southern Europe'= '17', 'Western Europe'= '18'),stopened=TRUE, stselected=TRUE),
      'Ocenia'   =  structure(list('Australia and New Zealand' = '19', 'Melanesia'= '20', 'Micronesia'= '21'),stopened=TRUE, stselected=TRUE)
      ))
    attr(regionsubregion[[1]],"stopened")=TRUE 
    regionsubregion
    
  })
  
  output$slider <- renderUI({
    args       <- list(inputId="age", label="Age Distribution:", ticks=c(18,22,27,31,35), value=c(22,31))
    args$min   <- 18
    args$max   <- 35
    if (sessionInfo()$otherPkgs$shiny$Version>="0.11") {
      ticks <- paste0(args$ticks, collapse=',')
      args$ticks <- T
      html  <- do.call('sliderInput', args)
      html
      #html$children[[2]]$attribs[['data-values']]
      html$children[[2]]$attribs[['data-values']] <- ticks;
    } else {
      html  <- do.call('sliderInput', args)    
    }
    html
  })
  
  output$myMap = renderLeaflet({
   
    #leaflet(countries) %>% addTiles() %>% setView(0, 0, zoom = 2)
    dftemp = df %>%
      filter(Please.select.your.age.group. %in% input$age & What.is.your.gender. %in% input$gender & Region_Sub_UN %in% getregions(input$regionsubregion))
      
    
    dfcount = dftemp %>%
      group_by(Country) %>%
      summarize(totalno = n())
    dfcount
    
    newcountries <- merge(countries, dfcount, by.x='name', by.y='Country')
    qpal <- colorQuantile("Blues", newcountries$totalno, n = 10)
    pal = colorNumeric('OrRd', newcountries$totalno)
    leaflet(newcountries) %>%
      addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.6,
                  color = ~pal(totalno))%>%
                setView(0, 0, zoom = 2)%>%
      addTiles() %>%
      addLegend(pal = pal, values = ~totalno)
  })
  
  output$agedist = renderPlot({
    df %>%
      filter(Please.select.your.age.group. %in% input$age & What.is.your.gender. %in% input$gender & Region_Sub_UN %in% getregions(input$regionsubregion)) %>%
      #qplot(data=., Please.select.your.age.group., fill=What.is.your.gender., geom="bar", xlab="Age Group", ylab="No. of respondents")
      #ggplot(., aes(x = Please.select.your.age.group., fill=What.is.your.gender.))
      ggplot(., aes(x=Please.select.your.age.group.)) +
      geom_bar(aes(fill=What.is.your.gender.), position = "dodge", stat = "count") +
      ggtitle(wrapper("Distribution with respect to Age-Group and Gender", width = 30)) +
      labs(x="Age Group", y="Total No. of respondents", fill="Gender")+
      theme(text = element_text(size=15),
            plot.title = element_text(size=15, hjust = 0.5))
      #geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count")
  })
  
  output$identity = renderPlot({
    
    identitydf = df%>%
        filter(Please.select.your.age.group. %in% input$age & What.is.your.gender. %in% input$gender & Region_Sub_UN %in% getregions(input$regionsubregion)) %>%
        mutate(Would.you.say.the.world.is...= replace(Would.you.say.the.world.is...,Would.you.say.the.world.is...=="","NO RESPONSE"))%>%
        group_by(Would.you.say.the.world.is...,As.far.as.my.identity.is.concerned..what.defines.me.most.is...)%>%
        summarize(count = n())

      ggplot(identitydf, aes(As.far.as.my.identity.is.concerned..what.defines.me.most.is...,Would.you.say.the.world.is...)) +
        geom_tile(aes(fill = count), , color = "white") +
        scale_fill_gradient(low = "red", high = "green") +
        ylab("") +
        xlab("") + 
        theme(text = element_text(size=15),
              legend.title = element_text(size = 15),
              legend.text = element_text(size = 15),
              plot.title = element_text(size=16),
              axis.title=element_text(size=14),
              axis.text.x = element_text(angle = 25, hjust = 1)) +
        labs(fill = "Number of Respondents")
  })
  
  output$issues = renderPlot({
    dftemp = df%>%
      filter(Please.select.your.age.group. %in% input$age & What.is.your.gender. %in% input$gender & Region_Sub_UN %in% getregions(input$regionsubregion))
      
    localissuesdf = dftemp[c(25:41)]
    localissuesdf = getIssues(localissuesdf, "Locally")
    localissuesdf
    globalissuesdf = dftemp[c(42:58)]
    globalissuesdf = getIssues(globalissuesdf, "Globally")
    globalissuesdf
    issues = rbind(localissuesdf,globalissuesdf)
    issues
    localissues = issues %>%
      filter(type == "Locally")%>%
      arrange(desc(count))%>%
      slice(1:5)
    
    q1 = ggplot(localissues, aes(x= reorder(names, -count), y = count)) +
      geom_col() + xlab("Category") +
      ylab("Number of Respondents") + ggtitle("Top 5 Serious Issues in your Country") +
      theme(text = element_text(size=15),
            plot.title = element_text(size=16, hjust=0.5),
            axis.title=element_text(size=14),
            axis.text.x = element_text(angle = 45, hjust = 1)) +
            scale_x_discrete(labels = get_wraper(20)) +
      ylim(0,11000)
    
    globalissues = issues %>%
      filter(type == "Globally")%>%
      arrange(desc(count))%>%
      slice(1:5)
    
    q2 = ggplot(globalissues, aes(x= reorder(names, -count), y = count)) +
      geom_col() + xlab("Category") +
      ylab("Number of Respondents") + ggtitle("Top 5 Serious Issues Globally") +
      theme(text = element_text(size=15),
            plot.title = element_text(size=16, hjust=0.5),
            axis.title=element_text(size=14),
            axis.text.x = element_text(angle = 45, hjust = 1)) + 
            scale_x_discrete(labels = get_wraper(20)) +
      ylim(0,11000)
    grid.arrange(q1,q2,nrow=1,ncol=2)
    
  })
  
  output$socialmedia = renderPlot({
    dftemp = df%>%
      filter(Please.select.your.age.group. %in% input$age & What.is.your.gender. %in% input$gender & Region_Sub_UN %in% getregions(input$regionsubregion))
    
    socialmediadf = dftemp[c(275:277)]
    colnames(socialmediadf)[1:length(colnames(socialmediadf))] = c("Social_Media.Personal_Life", "Social_Media.Professional_Life", "Social_Media.Productivity")
    socialmediadf = socialmediadf %>%
      filter((Social_Media.Personal_Life != "" ) | (Social_Media.Professional_Life !="") | (Social_Media.Productivity != ""))
    ggparallel(list("Social_Media.Personal_Life", "Social_Media.Productivity", "Social_Media.Professional_Life"), data=socialmediadf, method="hammock",ratio=0.2)+
      scale_fill_manual(values = rep(c("Orange", "Steelblue"), 14)) +
      scale_colour_manual(values = rep(c("Orange", "Steelblue"), 14))+
      ggtitle("Importance of Social Media - Productivity") + 
      theme(text = element_text(size=15, angle = 0),
            legend.position="none",
            axis.text.x = element_text(angle = 45, hjust = 1)
            )
  })
  
  
  output$socialmedia2 = renderPlot({
    dftemp = df%>%
      filter(Please.select.your.age.group. %in% input$age & What.is.your.gender. %in% input$gender & Region_Sub_UN %in% getregions(input$regionsubregion))
    
    socialmediadf = dftemp[c(275:277)]
    cat(input$rb)
    colnames(socialmediadf)[1:length(colnames(socialmediadf))] = c("Importance_in_Personal_Life", "Importance_in_Professional_Life", "Productivity")
    socialmediadf = socialmediadf %>%
      filter(Productivity == input$rb)
    str(socialmediadf)
    tmp = socialmediadf %>%
      filter((Importance_in_Personal_Life != "" ))%>%
      group_by(Importance_in_Personal_Life)%>%
      summarize(countofrows = n())
    total = sum(tmp$countofrows)
    tmp$countofrows = tmp$countofrows / total *100
    tmp$Importance_in_Personal_Life = factor(tmp$Importance_in_Personal_Life, levels=c("Extremely important", "Somewhat important", "Neutral", "Not important"))
    q1 = qplot(x = as.factor(tmp$Importance_in_Personal_Life), y = tmp$countofrows, data = tmp, geom = "col") +
      labs(title = wrapper("Importance in Personal Life", width = 30), x = "", y = "% of Respondents who answered this question") +
      ylim(0,100) + 
      theme(text = element_text(size=15),
            axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(size=16, hjust=0.5))
    tmpprof = socialmediadf %>%
      filter((Importance_in_Professional_Life != "" ))%>%
      group_by(Importance_in_Professional_Life)%>%
      summarize(countofrows = n())
    total = sum(tmpprof$countofrows)
    tmpprof$countofrows = tmpprof$countofrows / total *100
    tmpprof$Importance_in_Professional_Life = factor(tmpprof$Importance_in_Professional_Life, levels=c("Extremely important", "Somewhat important", "Neutral", "Not important"))
    q2 = qplot(x = as.factor(tmpprof$Importance_in_Professional_Life), y = tmpprof$countofrows, data = tmpprof, geom = "col") +
      labs(title = wrapper("Importance in Professional Life", width = 40), x = "", y = "% of Respondents who answered this question") +
      ylim(0,100) + 
      theme(text = element_text(size=15),
            axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(size=16, hjust=0.5))
    
    grid.arrange(q1,q2,nrow=1,ncol=2)
    
  })
  
  
  output$internetdevice = renderPlot({
    
    dftemp = df%>%
      filter(Please.select.your.age.group. %in% input$age & What.is.your.gender. %in% input$gender & Region_Sub_UN %in% getregions(input$regionsubregion))
    
    alldevices = dftemp[c(102:104)] 
    colnames(alldevices) = c("socialmedia", "writingreadingemails", "onlineshopping")
    socialmediadf = alldevices %>%
      filter(socialmedia != "Not applicable" & socialmedia != "Not appliccable" & socialmedia != "")%>%
      select(socialmedia)
    g1 = ggplot(socialmediadf, aes(x = as.factor(socialmedia))) +
      geom_bar(aes(y = (..count..)/sum(..count..))) +
      geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
      scale_y_continuous(labels = percent, limits=c(0,1)) +
      labs(title = wrapper("Device Usage for accessing Internet for Social Media", width = 30), y = "", x = "") + 
      theme(text = element_text(size=15),
            axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(size=16, hjust=0.5))

    writingreadingemailsdf = alldevices %>%
      filter(writingreadingemails != "Not applicable" & writingreadingemails != "Not appliccable" & writingreadingemails != "")%>%
      select(writingreadingemails)
    g2 = ggplot(writingreadingemailsdf, aes(x = as.factor(writingreadingemails))) +
      geom_bar(aes(y = (..count..)/sum(..count..))) +
      geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
      scale_y_continuous(labels = percent, limits=c(0,1)) +
      labs(title = wrapper("Device Usage for accessing Internet for Reading & Writing", width = 40), y = "", x = "") + 
      theme(text = element_text(size=15),
            axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(size=16, hjust=0.5))

    onlineshoppingdf = alldevices %>%
      filter(onlineshopping != "Not applicable" & onlineshopping != "Not appliccable" & onlineshopping != "")%>%
      select(onlineshopping)
    g3 = ggplot(onlineshoppingdf, aes(x = as.factor(onlineshopping))) +
      geom_bar(aes(y = (..count..)/sum(..count..))) +
      geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
      scale_y_continuous(labels = percent, limits=c(0,1)) +
      labs(title = wrapper("Device Usage for accessing Internet for Online Shopping", width = 30), y = "", x = "")  + 
      theme(text = element_text(size=15),
            axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(size=16, hjust=0.5))

    grid.arrange(g1,g2,g3,nrow=1,ncol=3)
    
      
  })
  
  output$viewsaboutwomen = renderPlot({
    dftemp = df%>%
      filter(Please.select.your.age.group. %in% input$age & What.is.your.gender. %in% input$gender & Region_Sub_UN %in% getregions(input$regionsubregion))
    womenviewsdf = dftemp[c(205:207)]
    str(womenviewsdf)
    colnames(womenviewsdf) = c("Woman_Manager", "Woman_CEO", "Woman_President")
    
    wm = womenviewsdf %>%
      group_by(Woman_Manager)%>%
      summarize(total = n())
    wm$total = round(wm$total/sum(wm$total)*10, 2)
    wm
    wceo = womenviewsdf %>%
      group_by(Woman_CEO)%>%
      summarize(total = n())
    wceo$total = round(wceo$total/sum(wceo$total)*10, 2)
    
    wp = womenviewsdf %>%
      group_by(Woman_President)%>%
      summarize(total = n())
    wp$total = round(wp$total/sum(wp$total)*10, 2)
    
    wp$total[5]
    bip= data.frame(Woman_as_Manager= c(wm$total[2], wm$total[3], wm$total[4], wm$total[5], wm$total[6]),
                    Woman_as_CEO= c(wceo$total[2], wceo$total[3], wceo$total[4], wceo$total[5], wceo$total[6]),
                    Woman_as_President= c(wp$total[2], wp$total[3], wp$total[4], wp$total[5], wp$total[6]),
                    row.names= c("Extremely uncomfortable", "Neutral", "Somewhat comfortable", "Somewhat uncomfortable", "Very comfortable"))
    # weighted bipartite network
    bip = network(bip,
                  matrix.type = "bipartite",
                  ignore.eval = FALSE,
                  names.eval = "weights")
    col = c("actor" = "grey40", "event" = "pink")
    
    ggnet2(bip, color = "mode", palette = col, label = TRUE, edge.label = "weights", edge.size="weights") +
      theme(legend.position="none",
            plot.title = element_text(size=16, hjust=0.5))+ ggtitle(wrapper("How comfortable are you with women as..?  (% of responses * 10)", width=40))
    
  })
    
  output$totalparticipants <- renderValueBox({
    valueBox(
      value = 26615,
      subtitle = "Total Number of Participants",
      color = "aqua"
    )
  })
  
  output$totalcompleted <- renderValueBox({
    valueBox(
      value = nrow(df),
      subtitle = "Completed the Survey"
      )
  })
  output$numofcountries <- renderValueBox({
    
    valueBox(
      value = nrow(surveycountres),
      subtitle = "Total Number of Countries & Territories"
    )
  })
  output$totalmaleparticipants <- renderValueBox({
    
    valueBox(
      value = 9044,
      subtitle = "Total Number of Male participants"
    )
  })
  output$totalfemaleparticipants <- renderValueBox({
    
    valueBox(
      value = 10974,
      subtitle = "Total Number of Female participants"
    )
  })
  output$totalotherparticipants <- renderValueBox({
    
    valueBox(
      value = 61,
      subtitle = "Total Number of Other participants"
    )
  })  
  output$selectionresults <- renderText({ 
    paste ("Selection Results")
  })
  
  output$totalcompleted1 <- renderValueBox({
    valueBox(
      value = nrow(df),
      subtitle = "Completed the Survey"
    )
  })
  
  output$selectedcountries <- renderValueBox({
    dftemp = df%>%
      filter(Region_Sub_UN %in% getregions(input$regionsubregion))%>%
      group_by(Country) %>%
      summarize()
    valueBox(
      value = nrow(dftemp),
      subtitle = "Number of Distinct Countries Filtered"
    )
  })
  
})
