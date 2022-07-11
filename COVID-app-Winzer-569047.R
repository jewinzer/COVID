#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# author: Jan-Erik Winzer
#


library(ggplot2)
library(lubridate)

DEMOS<-read.csv("einwohner_2019.csv", header= TRUE, sep=";")
COVID <-read.csv("RKI_COVID19-20210120.csv", header= TRUE, dec = ",")
COVID[,"Meldedatum"] <-as.Date(COVID$Meldedatum)

# extract cases
neuinfektionen <-aggregate(COVID$AnzahlFall, by = list(COVID$Bundesland,COVID$Meldedatum), FUN=sum)
colnames(neuinfektionen) <- c("Bundesland","Datum","Neuinfektionen")
#View(neuinfektionen)

# extract deaths
todesfälle <-aggregate(COVID$AnzahlTodesfall, by = list(COVID$Bundesland,COVID$Meldedatum), FUN=sum)
colnames(todesfälle) <- c("Bundesland","Datum","Todesfälle")

#create empty df
FINAL <- data.frame(Datum= Date(), Bundesland=character(), Neuinfektionen= numeric(),Todesfälle= numeric(),Neuinfektionen_7d_avg= numeric(),Todesfälle_7d_avg = numeric(), Neuinfektionen_7d_Inzidenz=numeric(),Todesfälle_7d_Inzidenz= numeric())

# Loop over Bundesländer
for (value in sort(unique(COVID$Bundesland))) {
    
    #create data frame with all dates
    dates <-data.frame(seq(as.Date(min(COVID$Meldedatum)), as.Date(max(COVID$Meldedatum)), by = "days"))
    colnames(dates) <- c("Datum")
    #View (dates)
    
    #subset Neuinfektionen per bundesland
    neuinfektionen_bl <- neuinfektionen[ which(neuinfektionen$Bundesland==value), ]
    
    #left outer join with subset_bl
    subset_bl1 <- merge(dates,neuinfektionen_bl,by="Datum", all.x = TRUE)
    #View(subset_bl1)
    
    #Filling NAs
    subset_bl1$Bundesland[is.na(subset_bl1$Bundesland)]<- value
    subset_bl1$Neuinfektionen[is.na(subset_bl1$Neuinfektionen)]<- 0
    #View(subset_bl1)
    
    #subset todesfällefälle per bundesland
    todesfälle_bl <- todesfälle[ which(todesfälle$Bundesland==value), ][-c(1)]
    
    #left outer join with subset_bl2
    subset_bl2 <- merge(subset_bl1,todesfälle_bl,by="Datum", all.x = TRUE)
    
    #Filling NAs
    subset_bl2$Todesfälle[is.na(subset_bl2$Todesfälle)]<- 0
    
    #add Neuinfektionen_7d_avg
    subset_bl2[,"Neuinfektionen_7d_avg"]<- filter(subset_bl2$Neuinfektionen, c(1/7,1/7,1/7,1/7,1/7,1/7,1/7), sides=1)
    subset_bl2[is.na(subset_bl2)] <- 0
    subset_bl2$Neuinfektionen_7d_avg<- as.numeric(subset_bl2$Neuinfektionen_7d_avg)
    
    #add Todesälle_7d_avg
    subset_bl2[,"Todesfälle_7d_avg"]<- filter(subset_bl2$Todesfälle, c(1/7,1/7,1/7,1/7,1/7,1/7,1/7), sides=1)
    subset_bl2[is.na(subset_bl2)] <- 0
    subset_bl2$Todesfälle_7d_avg<- as.numeric(subset_bl2$Todesfälle_7d_avg)
    
    #get Einwohner per bundesland
    einwohner <-DEMOS[ which(DEMOS$Bundesland==value), ]$Einwohner*1000
    
    #add Fälle_7d_Inzidenz
    subset_bl2[,"Neuinfektionen_7d_Inzidenz"]<- subset_bl2[,"Neuinfektionen_7d_avg"]*7/einwohner*100000
    subset_bl2$Neuinfektionen_7d_Inzidenz<- as.numeric(subset_bl2$Neuinfektionen_7d_Inzidenz)
    
    #add Todesälle_7d_Inzidenz
    subset_bl2[,"Todesfälle_7d_Inzidenz"]<- subset_bl2[,"Todesfälle_7d_avg"]*7/einwohner*100000
    subset_bl2$Todesfälle_7d_Inzidenz<- as.numeric(subset_bl2$Todesfälle_7d_Inzidenz)
    
    #append Bundesland df to FINAL df
    FINAL <- rbind(FINAL,subset_bl2)
}
features <-colnames(FINAL)[3:4]
colors <-c("deeppink4","black")
limits<-c(max(FINAL[3]),max(FINAL[4]) )
features_df <-data.frame(features, colors,limits)
bundesländer <-sort(unique(FINAL$Bundesland))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("COVID-19: Bundesländer im Vergleich"),
    p("Jan-Erik Winzer | 569047"),
    br(),

    # Sidebar with a select input for Bundesländer
    sidebarLayout(
        sidebarPanel(
            selectInput("bundesland",
                        "",
                        choices = bundesländer),
            radioButtons("feature",
                         "",
                         choices = features, selected= features[1]),
        ),

        # Show plots
        mainPanel(
            tabsetPanel(type = "tabs",
                            tabPanel("7-Tage-Inzidenz",
                                 br(),
                                 plotOutput("relative")),
                        tabPanel("Fälle pro Tag",
                                 br(),
                                 plotOutput("absolute"),
                        )
        )
           
        )
    )
)

# Define server logic required to draw plots
server <- function(input, output) {

    output$absolute <- renderPlot({
       
         # create subset
        to_plot <- FINAL[ which(FINAL$Bundesland==input$bundesland),]
        
        # draw the absolute plot for the selected Bundesland
        ggplot(data=to_plot, aes(x=Datum))+
            coord_cartesian(clip="off")+
            theme_classic()+
            labs(
                title = paste(input$bundesland,": ",input$feature," pro Tag", sep=""),
                subtitle = "Quelle: RKI, Stand: 20.01.2021",
                x="Meldedatum",
                y= paste(input$feature))+
            theme(
                plot.margin = unit(c(0,8,0,0), "lines"),
                plot.title = element_text(size=16, hjust =0.5, margin = margin(t = 12, r = 0, b = 12, l = 0)),
                plot.subtitle = element_text(size=10, hjust =0.5, margin = margin(t = 0, r = 0, b = 24, l = 0)),
                axis.text.x = element_text(size= 10, hjust =-0.3),
                axis.text.y = element_text(size= 10),
                axis.title.y = element_text(size= 12, margin = margin(t = 0, r = 12, b = 0, l = 0)),
                axis.title.x = element_text(size =12, margin = margin(t = 12, r = 0, b = 0, l = 0)),
                panel.grid.major.y = element_line(size = 0.3, linetype = 'solid', colour = "grey"),
                axis.line = element_blank(),
                axis.ticks.x = element_line(size = 0.3, linetype = 'solid', colour = "grey"),
                axis.ticks.y = element_blank())+
            geom_bar(stat="identity", fill=features_df[which(features_df$features == input$feature),2], aes_string(y=input$feature), alpha =0.25)+
            scale_x_date(date_breaks = "1 months", date_labels ="%b", limits= c(min(to_plot$Datum)-6,max(to_plot$Datum)+1))+
            geom_line(aes_string(y=paste(input$feature,"_7d_avg", sep="")),color=features_df[which(features_df$features == input$feature),2], size=0.75)+
            scale_y_continuous(expand = c(0, 0), limits = c(0, features_df[which(features_df$features == input$feature),3]))+
            geom_text(label="7-Tage-Durchschnitt",
                      data= to_plot[which(to_plot$Datum ==max(to_plot$Datum)),],
                      aes_string(y=paste(input$feature,"_7d_avg", sep="")),
                      size=3.5, hjust = -.05, vjust =-0.5,
                      fontface ="bold", 
                      colour = features_df[which(features_df$features == input$feature),2])
        })
    
    output$relative <- renderPlot({
        
        #create styled plot
        p <- ggplot()+
            coord_cartesian(clip="off")+
            theme_classic()+
            labs(
                title = paste(input$feature," innerhalb 7 Tagen je 100.000 Einwohner", sep=""),
                subtitle = "Quellen: RKI, Destatis | Stand: 20.01.2021",
                x="Meldedatum",
                y= "")+
            theme(
                plot.margin = unit(c(0,8,0,0), "lines"),
                plot.title = element_text(size=16, hjust =0.5, margin = margin(t = 12, r = 0, b = 12, l = 0)),
                plot.subtitle = element_text(size=10, hjust =0.5, margin = margin(t = 0, r = 0, b = 24, l = 0)),
                axis.text.x = element_text(size= 10, hjust =-0.3),
                axis.text.y = element_text(size= 10),
                axis.title.y = element_text(size= 12, margin = margin(t = 0, r = 12, b = 0, l = 0)),
                axis.title.x = element_text(size =12, margin = margin(t = 12, r = 0, b = 0, l = 0)),
                panel.grid.major.y = element_line(size = 0.3, linetype = 'solid', colour = "grey"),
                axis.line = element_blank(),
                axis.ticks.x = element_line(size = 0.3, linetype = 'solid', colour = "grey"),
                axis.ticks.y = element_blank())+
            scale_y_continuous(expand = c(0, 0), limits = c(0, NA))+
            scale_x_date(date_breaks = "1 months", date_labels ="%b", limits= c(min(FINAL$Datum)-6,max(FINAL$Datum)+1))
        
        #loop over Bundesländer, draw faded geom_lines
        for (value in bundesländer){
            p <- p + geom_line(data = FINAL[ which(FINAL$Bundesland==value),],
                            aes_string(x="Datum", y=paste(input$feature,"_7d_Inzidenz", sep="")),
                            color="gray80", size=0.3)
        }
        #subset user-selection
        to_show <- FINAL[ which(FINAL$Bundesland==input$bundesland),]
        
        #add current geom_line
        p <- p + geom_line(data= to_show,
                            aes_string(x="Datum", y=paste(input$feature,"_7d_Inzidenz", sep="")),
                            color=features_df[which(features_df$features == input$feature),2],
                            size= 0.75)
        
        #label current geom_line
        p <- p + geom_text(label=input$bundesland,
                            data= to_show[which(to_show$Datum ==max(to_show$Datum)),],
                            aes_string(x="Datum",y=paste(input$feature,"_7d_Inzidenz", sep="")),
                            size=3.5, hjust = -.05, vjust =-0.5,
                            fontface ="bold", 
                            colour = features_df[which(features_df$features == input$feature),2])
        p
    })
    

}

# Run the application 
shinyApp(ui = ui, server = server)
