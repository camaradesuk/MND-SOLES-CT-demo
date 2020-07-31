

shinyServer(function(input, output, session) {
  
  showModal(modalDialog(
    title="Disclaimer: This web application is under development and has not been officially released yet."
  ))
  ##############DEMO VERSION###################################
  #drug recommendations-------------------------------------
  demodrugranklist <- reactive({
    mydf<-demodrugSummary %>%
      select(
        "Drug",
        "productScore",
        "nPublication",
        "efficacyScore"  ,
        "safetyScore"  ,
        "studySizeScore" ,
        "qualityScore"  ,
        "invivoSurvivalSMD",
        "nInvivoStudies"   ,
        "nCellDeathSMD"   ,
        "nInVitroStudies"
      ) 
    
    cols <- c(    "productScore",
                  "efficacyScore"  ,
                  "safetyScore"  ,
                  "studySizeScore" ,
                  "qualityScore"  ,
                  "invivoSurvivalSMD",
                  "nCellDeathSMD"   )
    
    mydf[,cols] <-round(mydf[,cols],2)
    
    return(mydf)
  })
  ########drugtable-------------------------------------
  output$demodrugranklist <- DT::renderDataTable(DT::datatable(
    demodrugranklist(),
    colnames=c("Drug",
               "Clinical Product Score",
               "Clinical n(Pub)", 
               "[E]",
               "[S]",
               "[SS]",
               "[Q]",
               "In vivo survival SMD",
               "in vivo n(Pub)",
               "In vitro cell death SMD",
               "in vitro n(Pub)"),
    filter="top",
    caption= htmltools::tags$caption(
      style='caption-side:bottom; text-align:center;', 
      "Current results summary of (i) clinical review: publications for interventions were scored against predefined criteria (see the About tab for more information). Clinical publications were assigned scores for efficacy[E], safety[S], study size [SS] and quality [Q] (1: worst, 4: best). For each intervention, using the median scores from publications, a product score was generated. (Product score = log10(1 + number of clinical publications)*[E]*[S]*[SS]*[Q]); (ii)in vivo and (iii) in vitro review are reported here by the standard mean deviation (SMD) in survival and cell death studies respectively calculated from our meta-analysis. n(Pub) refers to number of publications annotated for each drug in our review.")
  ))
  #######download drug table-------------------------------------
  output$demodownloadDrugTable <- downloadHandler(
    filename = function() {
      file<-paste("drugtable-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(demodrugranklist(), 
                file=file,
                quote=T,
                row.names=F,
                na="")
    }
  )
  
  ########
  
  output$demodownloadDrugPublications <- downloadHandler(
    filename = function() {
      file <- paste("clinicalpublications", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(
        demoselecteddrugclinicalpublications(),
        file = file,
        quote = T,
        row.names = F,
        na = ""
      )
    }
  )
  ###########
  
  #######drugbubblechart-------------------------------------
  output$demodrugrankchart <- renderPlotly({
    p<- plot_ly(
      demodrugSummary,
      x = ~ invivoSurvivalSMD,
      y = ~ nCellDeathSMD,
      hoverinfo = 'text',
      
      text = ~ paste(
        '</br> Drug:',
        Drug,
        '</br> Clinical Score:',
        round(productScore, digits = 2),
        '</br> n(clinical publications):',
        nPublication,
        '</br> in vivo SMD:',
        round(invivoSurvivalSMD, digits = 2),
        '</br> in vitro SMD:',
        round(nCellDeathSMD),
        digits = 2
      ),
      type = 'scatter',
      mode = 'markers',
      
      marker = list(
      size = ~nPublication,
        sizeref=0.1,
        sizemode="area",
        opacity = 0.5,
        color = ~ productScore,
        colorscale = 'Viridis', 
        reversescale=TRUE,
        showscale=T,
        colorbar=list(title='Product Score'),
        sizemin= 3, 
        showlegend=T)
    )%>%
      layout(
        xaxis=(list(title="In vivo Survival SMD")),
        yaxis=(list(title="In vitro cell death SMD"))
      )
    
    
    s<-c(1,seq(10,100, by= 10))
    
    legend.plot<-plot_ly(y =1, x= ~s)%>%
      add_markers(y=1,
                  x= ~s,
                  size=  ~s,
                  color= 'rgba(255, 182, 193, .9)',
                  showlegend=F,
                  hoverinfo="none",
                  marker=list(sizeref=0.1, sizemode="area"))%>%
      layout(
        annotations=list(text="n(clinical publications)", x=-10, y=1, showarrow=F ),
        xaxis=list(
          title='',
          tick0=0,
          dtick=10,
          zeroline=F,
          showline=F,
          showgrid=F,
          tickvals= list(1,10,20,30,40,50,60,70,80,90,100),
          tickmode="array"),
        yaxis=list(
          showgrid=F,
          zeroline=F,
          showline=F,
          showticklabels=F
          
        )
        
      )
    
    demodrugrankchart<-subplot(p, legend.plot, nrows=2, heights= c(0.8,0.2), margin=0.1, titleX=T, titleY = T)%>%layout(height=600)
    return(demodrugrankchart)
    
    
  })
  
  
  
  
  
  ###############
  #drug heatmap-------------------------------------
  demoproductscoredata <- demodrugSummary %>%
    select("productScore")%>%
    arrange(desc(productScore))
  demoproductscoredata <- as.matrix(demoproductscoredata)
  rownames(demoproductscoredata) <- demodrugSummary$Drug
  
  democlinicaldata <- demodrugSummary %>%
    select("efficacyScore"  ,
           "safetyScore"  ,
           "studySizeScore" ,
           "qualityScore")
  democlinicaldata <- as.matrix(democlinicaldata)
  rownames(democlinicaldata) <- demodrugSummary$Drug
  
  
  demopreclinicaldata <- demodrugSummary %>%
    select("invivoSurvivalSMD", "nCellDeathSMD")
  demopreclinicaldata <- as.matrix(demopreclinicaldata)
  rownames(demopreclinicaldata) <- demodrugSummary$Drug
  
  
  output$demohm<-renderPlotly({
    fig3<-plot_ly(z=demoproductscoredata, x=colnames(demoproductscoredata), y=rownames(demoproductscoredata)
                  , type='heatmap', height=1300, width= 1000, colorscale="Viridis", reversescale=TRUE, 
                  colorbar=list(title='Clinical Product Score'))%>%
      layout(yaxis=list(autorange='reversed'),
             xaxis=(list(title="Clinical Product Score")))
    
    fig1<-plot_ly(z=democlinicaldata, x=colnames(democlinicaldata), y=rownames(democlinicaldata)
                  , type='heatmap', colorscale="magma", colorbar=list(title='Clinical subscores'))
    
    fig2<-plot_ly(z=demopreclinicaldata, x=colnames(demopreclinicaldata), y=rownames(demopreclinicaldata)
                  , type='heatmap', colorscale= "Blues", reversescale=TRUE, colorbar=list(title='Preclinical SMD'))
    
    demohm<-subplot(fig3,fig1,fig2, shareY=T, widths=c(1/7, 4/7, 2/7))%>%layout(height=1300, width=800)
    
  })
  
  
  #progress-------------------------------------
  
  
  #####plotlysunburst-----------------------------
  
  
  demosunburstdata <- demopublicationList %>%
    select(Disease, studyType, phase, Drug, StudyIdStr)%>%
    unique() %>%
    group_by(Disease, studyType, phase, Drug, StudyIdStr) %>%
    count() %>%
    rename(value = n) %>%
    ungroup()
  
  demoDF0 <- demosunburstdata %>% 
    group_by(Disease) %>% 
    unique() %>%
    summarise(value=sum(value))
  
  demoDF1 <- demosunburstdata %>% 
    group_by(Disease, studyType) %>% 
    summarise(value=sum(value))
  
  demoDF2 <- demosunburstdata %>% 
    group_by(Disease, studyType, phase) %>%
    summarise(value=sum(value))
  
  demoDF3 <- demosunburstdata %>% 
    group_by(Disease, studyType, phase, Drug) %>%
    summarise(value=sum(value))
  
  demodf0 <- data.frame(
    ids = paste(demoDF0$Disease),
    labels = demoDF0$Disease,
    parents = "",
    values = demoDF0$value,
    stringsAsFactors = F
  )
  
  demodf1 <- data.frame(
    ids = paste(demoDF1$Disease, "-", demoDF1$studyType),
    labels = demoDF1$studyType,
    parents = paste(demoDF1$Disease),
    values = demoDF1$value,
    stringsAsFactors = F
  )
  
  demodf2 <- data.frame(
    ids = paste(demoDF2$Disease, "-", demoDF2$studyType, "-", demoDF2$phase),
    labels = demoDF2$phase,
    parents = paste(demoDF2$Disease, "-", demoDF2$studyType),
    values = demoDF2$value,
    stringsAsFactors = F
  )
  
  demodf3 <- data.frame(
    ids = paste(demoDF3$Disease, "-", demoDF3$studyType, "-", demoDF3$phase, "-", demoDF3$Drug),
    labels = demoDF3$Drug,
    parents = paste(demoDF3$Disease, "-", demoDF3$studyType, "-", demoDF3$phase),
    values = demoDF3$value,
    stringsAsFactors = F
  )
  
  demodf <- rbind(demodf0, demodf1, demodf2, demodf3)
  
  
  
  output$demosb2 <- renderPlotly({
    
    p <- plot_ly(demodf,
                 ids = ~ids,
                 labels = ~labels,
                 parents = ~parents,
                 type = 'sunburst',
                 values =  ~values,
                 branchvalues = "total", 
                 insidetextorientation='auto',
                 insidetextfont = list(size=12),
                 marker = list(colors = "Viridis", 
                               line = list(color = "white", width=2)),
                 source = "sunburstPlot")%>%
      layout (autosize = F, 
              paper_bgcolor="transparent",
              plot_bgcolor='transparent',
              height=600,
              width=600,
              margin = list(b = 0, l = 0, r = 0, t = 0, pad = 0, autoexpand = TRUE))
    
    event_register(p, 'plotly_click')
    
    p
    
  })
  
  
  
  
  
  
  
  ########clinicalprogress-------------------------------------
  output$demoClinicalUniquePubs <- renderInfoBox({
    infoBox(
      "Unique publications identified",
      as.numeric(demonClinicalUniquePubs),
      icon = icon("file-text-o"),
      color = "red"
    )
  })
  
  output$demoClinicalIncludedPubs <- renderInfoBox({
    infoBox(
      "Included publications",
      as.numeric(demonClinicalIncludedPubs),
      icon = icon("file-text-o"),
      color = "orange"
    )
  })
  
  output$demoClinicalDrugMeetLogic <- renderInfoBox({
    infoBox(
      "Drugs meeting inclusion logic",
      as.numeric(demonClinicalDrugMeetLogic),
      icon = icon("tablets"),
      color = "aqua"
    )
  })
  
  output$demoClinicalPublicationsMeetLogic <- renderInfoBox({
    infoBox(
      "Publications for included drugs",
      as.numeric(demonClinicalPublicationsMeetLogic),
      icon = icon("file-text-o"),
      color = "aqua"
    )
  })
  
  output$demoClinicalCoreDrugs <- renderInfoBox({
    infoBox(
      "Longlisted Drugs",
      as.numeric(demonClinicalCoreDrugs),
      icon = icon("tablets"),
      color = "blue"
    )
  })
  
  output$demoClinicalCorePubs <- renderInfoBox({
    infoBox(
      "Publications for longlisted drugs",
      as.numeric(demonClinicalCoreDrugsPubs),
      icon = icon("file-text-o"),
      color = "blue"
    )
  })
  
  output$demoClinicalSingleAnnot <- renderInfoBox({
    infoBox(
      "Single Annotated Publications",
      as.numeric(demonClinicalSingleAnnotated),
      icon = icon("user"),
      color = "green"
    )
  })
  
  output$demoClinicalDualAnnot <- renderInfoBox({
    infoBox(
      "Dual Annotated Publications",
      as.numeric(demonClinicalDualAnnotated),
      icon = icon("user-friends"),
      color = "green"
    )
  })
  
  
  output$demoClinicalReconciled <- renderInfoBox({
    infoBox(
      "Reconciled publications",
      as.numeric(demonClinicalReconciled),
      icon = icon("check"),
      color = "olive"
    )
  })
  
  ######in vivo progress-------------------------------------
  output$demoInvivoUniquePubs <- renderInfoBox({
    infoBox(
      "Unique publications identified",
      as.numeric(demonInvivoUniquePubs),
      icon = icon("file-text-o"),
      color = "red"
    )
  })
  
  output$demoInvivoIncludedPubs <- renderInfoBox({
    infoBox(
      "Included publications",
      as.numeric(demonInvivoIncludedPubs),
      icon = icon("file-text-o"),
      color = "orange"
    )
  })
  
  output$demoInvivoDrugMeetLogic <- renderInfoBox({
    infoBox(
      "Drugs meeting inclusion logic",
      as.numeric(demonInvivoDrugMeetLogic),
      icon = icon("tablets"),
      color = "aqua"
    )
  })
  
  output$demoInvivoPublicationsMeetLogic <- renderInfoBox({
    infoBox(
      "Publications for included drugs",
      as.numeric(demonInvivoPublicationsMeetLogic),
      icon = icon("file-text-o"),
      color = "aqua"
    )
  })
  
  output$demoInvivoCoreDrugs <- renderInfoBox({
    infoBox(
      "Longlisted Drugs",
      as.numeric(demonInvivoCoreDrugs),
      icon = icon("tablets"),
      color = "blue"
    )
  })
  
  output$demoInvivoCorePubs <- renderInfoBox({
    infoBox(
      "Publications for longlisted drugs",
      as.numeric(demonInvivoCoreDrugsPubs),
      icon = icon("file-text-o"),
      color = "blue"
    )
  })
  
  output$demoInvivoSingleAnnot <- renderInfoBox({
    infoBox(
      "Single Annotated Publications",
      as.numeric(demonInvivoSingleAnnotated),
      icon = icon("user"),
      color = "green"
    )
  })
  
  output$demoInvivoDualAnnot <- renderInfoBox({
    infoBox(
      "Dual Annotated Publications",
      as.numeric(demonInvivoDualAnnotated),
      icon = icon("user-friends"),
      color = "green"
    )
  })
  
  
  output$demoInvivoReconciled <- renderInfoBox({
    infoBox(
      "Reconciled publications",
      as.numeric(demonInvivoReconciled),
      icon = icon("check"),
      color = "olive"
    )
  })
  
  #######invitro progress-------------------------------------
  
  output$demoInvitroUniquePubs <- renderInfoBox({
    infoBox(
      "Unique publications identified",
      as.numeric(demonInvitroUniquePubs),
      icon = icon("file-text-o"),
      color = "red"
    )
  })
  
  output$demoInvitroIncludedPubs <- renderInfoBox({
    infoBox(
      "Included publications",
      as.numeric(demonInvitroIncludedPubs),
      icon = icon("file-text-o"),
      color = "orange"
    )
  })
  
  output$demoInvitroDrugMeetLogic <- renderInfoBox({
    infoBox(
      "Drugs meeting inclusion logic",
      as.numeric(demonInvitroDrugMeetLogic),
      icon = icon("tablets"),
      color = "aqua"
    )
  })
  
  output$demoInvitroPublicationsMeetLogic <- renderInfoBox({
    infoBox(
      "Publications for included drugs",
      as.numeric(demonInvitroPublicationsMeetLogic),
      icon = icon("file-text-o"),
      color = "aqua"
    )
  })
  
  output$demoInvitroCoreDrugs <- renderInfoBox({
    infoBox(
      "Longlisted Drugs",
      as.numeric(demonInvitroCoreDrugs),
      icon = icon("tablets"),
      color = "blue"
    )
  })
  
  output$demoInvitroCorePubs <- renderInfoBox({
    infoBox(
      "Publications for longlisted drugs",
      as.numeric(demonInvitroCoreDrugsPubs),
      icon = icon("file-text-o"),
      color = "blue"
    )
  })
  
  output$demoInvitroSingleAnnot <- renderInfoBox({
    infoBox(
      "Single Annotated Publications",
      as.numeric(demonInvitroSingleAnnotated),
      icon = icon("user"),
      color = "green"
    )
  })
  
  output$demoInvitroDualAnnot <- renderInfoBox({
    infoBox(
      "Dual Annotated Publications",
      as.numeric(demonInvitroDualAnnotated),
      icon = icon("user-friends"),
      color = "green"
    )
  })
  
  
  output$demoInvitroReconciled <- renderInfoBox({
    infoBox(
      "Reconciled publications",
      as.numeric(demonInvitroReconciled),
      icon = icon("check"),
      color = "olive"
    )
  })
  
  
  #drugCV-------------------------------------
  
  output$demoselecteddrugrankchart<-renderPlotly({
    demodrugrankchart <- plot_ly(
      demodrugSummary,
      x = ~ invivoSurvivalSMD,
      y = ~ nCellDeathSMD,
      hoverinfo = 'text',
      
      text = ~ paste(
        '</br> Drug:',
        Drug,
        '</br> Clinical Score:',
        round(productScore, digits = 2),
        '</br> n(clinical publications):',
        nPublication,
        '</br> in vivo SMD:',
        round(invivoSurvivalSMD, digits = 2),
        '</br> in vitro SMD:',
        round(nCellDeathSMD),
        digits = 2
      ),
      type = 'scatter',
      mode = 'markers',
      
      marker = list(size = ~ nPublication,
                    opacity = 0.5,
                    color = ~ productScore,
                    colorscale = 'Viridis', 
                    reversescale=TRUE,
                    showscale=T,
                    colorbar=list(title='Product Score'),
                    sizemin= 3
      )
    )%>%
      layout(title="Clinical, in vivo and in vitro scores by drug",
             xaxis=(list(title="In vivo Survival SMD")),
             yaxis=(list(title="In vitro cell death SMD")))
    demodrugrankchart
    
    demodrugbubble<-demodrugrankchart%>%add_markers(showlegend=F)
    demodrugbubble<-demodrugbubble%>%layout(annotations=demodrugannot(input$demodrug))
    demodrugbubble
    
    s<-c(1,seq(10,100, by= 10))
    
    legend.plot<-plot_ly(y =1, x= ~s)%>%
      add_markers(y=1,
                  x= ~s,
                  size=  ~s,
                  color= 'rgba(255, 182, 193, .9)',
                  showlegend=F,
                  hoverinfo="none",
                  marker=list(sizeref=0.1, sizemode="area"))%>%
      layout(
        annotations=list(text="n(clinical publications)", x=-10, y=1, showarrow=F ),
        xaxis=list(
          title='',
          tick0=0,
          dtick=10,
          zeroline=F,
          showline=F,
          showgrid=F,
          tickvals= list(1,10,20,30,40,50,60,70,80,90,100),
          tickmode="array"),
        yaxis=list(
          showgrid=F,
          zeroline=F,
          showline=F,
          showticklabels=F
          
        )
        
      )
    
    selecteddrugchart<-subplot(demodrugbubble, legend.plot, nrows=2, heights= c(0.8,0.2), margin=0.1, titleX=T, titleY = T)%>%layout(height=600)
    return(selecteddrugchart)
  })
  ###########################
  ##
  demoselecteddrugranklist <- reactive({
    mydf<-demodrugSummary %>%
      filter(Drug%in% input$demodrug)%>%
      select(
        "Drug",
        "productScore",
        "nPublication",
        "efficacyScore"  ,
        "safetyScore"  ,
        "studySizeScore" ,
        "qualityScore"  ,
        "invivoSurvivalSMD",
        "nInvivoStudies"   ,
        "nCellDeathSMD"   ,
        "nInVitroStudies"
      ) %>%
      arrange(desc(productScore))
    cols<- c(
      "productScore",
      "efficacyScore"  ,
      "safetyScore"  ,
      "studySizeScore" ,
      "qualityScore"  ,
      "invivoSurvivalSMD",
      "nCellDeathSMD")
    mydf[,cols] <-round(mydf[,cols],2)
    
    return(mydf)
    
  })
  ########drugtable-------------------------------------
  output$demoselecteddrugranklist <- DT::renderDataTable(DT::datatable(
    demoselecteddrugranklist(),
    colnames=c("Drug",
               "Clinical Product Score",
               "Clinical n(Pub)", 
               "[E]",
               "[S]",
               "[SS]",
               "[Q]",
               "In vivo survival SMD",
               "in vivo n(Pub)",
               "In vitro cell death SMD",
               "in vitro n(Pub)"),
    caption= htmltools::tags$caption(
      style='caption-side:bottom; text-align:center;', 
      "Current results summary of (i) clinical review: publications for interventions were scored against predefined criteria (see the About tab for more information). Clinical publications were assigned scores for efficacy[E], safety[S], study size [SS] and quality [Q] (1: worst, 4: best). For each intervention, using the median scores from publications, a product score was generated. (Product score = log10(1 + number of clinical publications)*[E]*[S]*[SS]*[Q]); (ii)in vivo and (iii) in vitro review are reported here by the standard mean deviation (SMD) in survival and cell death studies respectively calculated from our meta-analysis. n(Pub) refers to number of publications annotated for each drug in our review."),
    
    list(dom='t',
         ordering=F),
    rownames=F
  ))
  
  #####ClinicalPubSummary-------------------------------------
  
  demoselectedclinpubsummary <- reactive({
    demodrugpubdata <- demodrugSummary %>%
      filter(Drug %in% input$demodrug) %>%
      select(
        "nPublication",
        "nMND",
        "nAD",
        "nFTD",
        "nHD",
        "nMS",
        "nPD"
      )
    
    demotdrugpubdata <- t(as.matrix(demodrugpubdata))
    return(demotdrugpubdata)
  })
  
  output$demoselectedclinpubsummary <-
    DT::renderDataTable(DT::datatable(
      demoselectedclinpubsummary(),
      rownames=c("All diseases of interest", 
                 "MND", 
                 "AD",
                 "FTD",
                 "HD",
                 "MS",
                 "PD"),
      colnames="Number of publications",
      list(dom = 't',
           ordering=F)
      
    )%>%
      formatStyle(
        0,
        target = "row",
        fontWeight = styleEqual("All diseases of interest", "bold"),
        backgroundColor = styleEqual("All diseases of interest", "lightblue")
      )
    )
  #####ClinicalScoreSummary-------------------------------------
  demoselectedclinscoresummary <- reactive({
    demodrugscoredata <- demodrugSummary %>%
      filter(Drug %in% input$demodrug) %>%
      select(
        "productScore",
        "efficacyScore",
        "safetyScore",
        "studySizeScore",
        "qualityScore"
      )
    
    demotdrugscoredata <- t(as.matrix(demodrugscoredata))
    return(demotdrugscoredata)
  })
  
  output$demoselectedclinscoresummary <-
    DT::renderDataTable(DT::datatable(
      demoselectedclinscoresummary(),
      rownames=c("Product Score",
                 "Efficacy Score",
                 "Safety Score",
                 "Study Size Score",
                 "Quality Score"),
      colnames="Score",
      list(
        dom = 't',
        ordering=F
      ))%>%
        formatStyle(
          0,
          target = "row",
          fontWeight = styleEqual("Product Score", "bold"),
          backgroundColor = styleEqual("Product Score", "lightblue")
        )
    )
  
  
  ######drugsunburst-------------------------------------
  
  
  demosdsunburstdata <- reactive({
    demodrugsbdata<-demopublicationList %>%
      filter(Drug %in% input$demodrug)%>%
      select(Disease, studyType, phase, StudyIdStr)%>%
      unique() %>%
      group_by(Disease, studyType, phase, StudyIdStr) %>%
      count() %>%
      rename(value = n) %>%
      ungroup()
    
    
    demoDF0 <- demodrugsbdata %>% 
      group_by(Disease) %>% 
      unique() %>%
      summarise(value=sum(value))
    
    demoDF1 <- demodrugsbdata %>% 
      group_by(Disease, studyType) %>% 
      summarise(value=sum(value))
    
    demoDF2 <- demodrugsbdata %>% 
      group_by(Disease, studyType, phase) %>%
      summarise(value=sum(value))
    
    demodf0 <- data.frame(
      ids = paste(demoDF0$Disease),
      labels = demoDF0$Disease,
      parents = "",
      values = demoDF0$value,
      stringsAsFactors = F
    )
    
    demodf1 <- data.frame(
      ids = paste(demoDF1$Disease, "-", demoDF1$studyType),
      labels = demoDF1$studyType,
      parents = paste(demoDF1$Disease),
      values = demoDF1$value,
      stringsAsFactors = F
    )
    
    demodf2 <- data.frame(
      ids = paste(demoDF2$Disease, "-", demoDF2$studyType, "-", demoDF2$phase),
      labels = demoDF2$phase,
      parents = paste(demoDF2$Disease, "-", demoDF2$studyType),
      values = demoDF2$value,
      stringsAsFactors = F
    )
    
    
    
    demodf <- rbind(demodf0, demodf1, demodf2)
    
    return(demodf)
  })
  
  
  
  output$demosb3 <- renderPlotly({
    
    p <- plot_ly(demosdsunburstdata(),
                 ids = ~ids,
                 labels = ~labels,
                 parents = ~parents,
                 type = 'sunburst',
                 values =  ~values,
                 branchvalues = "total", 
                 insidetextorientation='auto',
                 insidetextfont = list(size=12),
                 marker = list(colors = "Viridis", 
                               line = list(color = "white", width=2)),
                 source = "sunburstPlot")%>%
      layout(autosize = T, 
             paper_bgcolor="transparent",
             plot_bgcolor='transparent',
             height=300,
             width=300,
             margin = list(b = 0, l = 0, r = 0, t = 30, pad = 0, autoexpand = TRUE))
    
    event_register(p, 'plotly_click')
    
    p
    
  })
  
  #####drugptsunburst---------------------
  
  d_sdptsunburstdata <- reactive({
    d_drugptsbdata<-demopublicationList %>%
      filter(Drug %in% input$demodrug)%>%
      select(Disease, studyType, phase, nPatients)%>%
      ungroup()
    
    
    d_DF0 <- d_drugptsbdata %>% 
      group_by(Disease) %>% 
      unique() %>%
      summarise(nPatients=sum(nPatients))
    
    d_DF1 <- d_drugptsbdata %>% 
      group_by(Disease, studyType) %>% 
      summarise(nPatients=sum(nPatients))
    
    d_DF2 <- d_drugptsbdata %>% 
      group_by(Disease, studyType, phase) %>%
      summarise(nPatients=sum(nPatients))
    
    d_df0 <- data.frame(
      ids = paste(d_DF0$Disease),
      labels = d_DF0$Disease,
      parents = "",
      values = d_DF0$nPatients,
      stringsAsFactors = F
    )
    
    d_df1 <- data.frame(
      ids = paste(d_DF1$Disease, "-", d_DF1$studyType),
      labels = d_DF1$studyType,
      parents = paste(d_DF1$Disease),
      values = d_DF1$nPatients,
      stringsAsFactors = F
    )
    
    d_df2 <- data.frame(
      ids = paste(d_DF2$Disease, "-", d_DF2$studyType, "-", d_DF2$phase),
      labels = d_DF2$phase,
      parents = paste(d_DF2$Disease, "-", d_DF2$studyType),
      values = d_DF2$nPatients,
      stringsAsFactors = F
    )
    
    
    
    d_df <- rbind(d_df0, d_df1, d_df2)
    
    return(d_df)
  })
  
  
  
  output$demoptsb <- renderPlotly({
    
    p <- plot_ly(d_sdptsunburstdata(),
                 ids = ~ids,
                 labels = ~labels,
                 parents = ~parents,
                 type = 'sunburst',
                 values =  ~values,
                 branchvalues = "total", 
                 insidetextorientation='auto',
                 insidetextfont = list(size=12),
                 marker = list(colors = "Viridis", 
                               line = list(color = "white", width=2)),
                 source = "sunburstPlot")%>%
      layout(autosize = T, 
             paper_bgcolor="transparent",
             plot_bgcolor='transparent',
             height=300,
             width=300,
             margin = list(b = 0, l = 0, r = 0, t = 30, pad = 0, autoexpand = TRUE))
    
    event_register(p, 'plotly_click')
    
    p
    
  })
  
  #####clinicalpubs-------------------------------------
  
  
  
  demoselecteddrugclinicalpubtable <- reactive({
    demodrugclinicalpubtable <- demopublicationList %>%
      filter(Drug %in% input$demodrug) %>%
      select(
        "Title",
        "Disease",
        "Drug",
        "Year",
        "Author",
        "Journal",
        "DOI",
        "studyType",
        "phase",
        "nPatients"
      )
    
    return(demodrugclinicalpubtable)
  })
  
  output$demodrugclinicalpublications <-
    DT::renderDataTable(DT::datatable(
      demoselecteddrugclinicalpubtable(),
      colnames=c("Title",
                 "Disease",
                 "Drug",
                 "Year",
                 "Author",
                 "Journal",
                 "DOI",
                 "Type of study",
                 "Study Phase",
                 "n(patients)"),
      filter="top"
    ))
  
  #####downloadclinicalpubs-------------------------------------
  
  demoselecteddrugclinicalpublications <- reactive({
    demodrugclinicalpublications <- demopublicationList %>%
      filter(Drug %in% input$demodrug) %>%
      select(
        "Title",
        "Disease",
        "Drug",
        "Year",
        "Author",
        "Journal",
        "DOI",
        "studyType",
        "phase",
        "nPatients",
        "efficacyScore",
        "safetyScore",
        "studySizeScore",
        "qualityScore"
      )
    
    return(demodrugclinicalpublications)
  })
  
  
  output$demodownloadDrugPublications <- downloadHandler(
    filename = function() {
      file <- paste("clinicalpublications", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(
        demoselecteddrugclinicalpublications(),
        file = file,
        quote = T,
        row.names = F,
        na = ""
      )
    }
  )
  
  
  ######invivodrugCV
  
  #survival
  demoinvivosurvivalforestdata<-reactive({
    df<-as.data.frame(demoinvivoPublicationList, stringsasfactors=T
    )
    df[df=="NA"]<-NA
    
    
    df<-df%>%
      filter(Drug %in% input$demodrug)%>%
      filter(Model %in% input$demomodels)
    df<-if(input$sod1) df else df%>%filter(SOD1 == "No")
    df<-df[!is.na(df$mortalityTE),]
    
    df<-data.frame(
      x<-as.numeric(df$mortalityTE),
      y<-as.numeric(df$mortalityseTE))
    
    return(df)
  })
  
  demoinvivosurvivalstudyNames<-reactive({
    df<-as.data.frame(demoinvivoPublicationList, stringsasfactors=T
    )
    df[df=="NA"]<-NA
    
    
    df<-df%>%
      filter(Drug %in% input$demodrug)%>%
      filter(Model %in% input$demomodels)
    df<-if(input$sod1) df else df%>%filter(SOD1 == "No")
    df<-df[!is.na(df$mortalityTE),]
    
    
    studyNames<-paste(df$Author, "", df$Year)
    
    return(studyNames)
  })    
  
  
  ##behavioural
  demoinvivobehavioralforestdata<-reactive({
    df<-as.data.frame(demoinvivoPublicationList, stringsasfactors=T
    )
    df[df=="NA"]<-NA
    
    
    df<-df%>%
      filter(Drug %in% input$demodrug)%>%
      filter(Model %in% input$demomodels)
    df<-if(input$sod1) df else df%>%filter(SOD1 == "No")
    df<-df[!is.na(df$behavioralTE),]
    
    
    df<-data.frame(
      x<-as.numeric(df$behavioralTE),
      y<-as.numeric(df$behavioralseTE))
    
    return(df)
  })
  
  demoinvivobehavioralstudyNames<-reactive({
    df<-as.data.frame(demoinvivoPublicationList, stringsasfactors=T
    )
    df[df=="NA"]<-NA
    
    
    df<-df%>%
      filter(Drug %in% input$demodrug)%>%
      filter(Model %in% input$demomodels)
    df<-if(input$sod1) df else df%>%filter(SOD1 == "No")
    df<-df[!is.na(df$behavioralTE),]
    
    
    
    studyNames<-paste(df$Author, "", df$Year)
    
    return(studyNames)
  })    
  
  
  ###biochemical
  demoinvivobiochemicalforestdata<-reactive({
    df<-as.data.frame(demoinvivoPublicationList, stringsasfactors=T
    )
    df[df=="NA"]<-NA
    
    
    df<-df%>%
      filter(Drug %in% input$demodrug)%>%
      filter(Model %in% input$demomodels)
    df<-if(input$sod1) df else df%>%filter(SOD1 == "No")
    df<-df[!is.na(df$biochemicalTE),]
    
    df<-data.frame(
      x<-as.numeric(df$biochemicalTE),
      y<-as.numeric(df$biochemicalseTE))
    
    return(df)
  })
  
  demoinvivobiochemicalstudyNames<-reactive({
    df<-as.data.frame(demoinvivoPublicationList, stringsasfactors=T
    )
    df[df=="NA"]<-NA
    
    
    df<-df%>%
      filter(Drug %in% input$demodrug)%>%
      filter(Model %in% input$demomodels)
    df<-if(input$sod1) df else df%>%filter(SOD1 == "No")
    df<-df[!is.na(df$biochemicalTE),]
    
    
    studyNames<-paste(df$Author, "", df$Year)
    
    return(studyNames)
  })    
  
  
  ###histological
  demoinvivohistologicalforestdata<-reactive({
    df<-as.data.frame(demoinvivoPublicationList, stringsasfactors=T
    )
    df[df=="NA"]<-NA
    
    
    df<-df%>%
      filter(Drug %in% input$demodrug)%>%
      filter(Model %in% input$demomodels)
    df<-if(input$sod1) df else df%>%filter(SOD1 == "No")
    df<-df[!is.na(df$histologicalTE),]
    
    df<-data.frame(
      x<-as.numeric(df$histologicalTE),
      y<-as.numeric(df$histologicalseTE))
    
    return(df)
  })
  
  demoinvivohistologicalstudyNames<-reactive({
    df<-as.data.frame(demoinvivoPublicationList, stringsasfactors=T
    )
    df[df=="NA"]<-NA
    
    
    df<-df%>%
      filter(Drug %in% input$demodrug)%>%
      filter(Model %in% input$demomodels)
    df<-if(input$sod1) df else df%>%filter(SOD1 == "No")
    df<-df[!is.na(df$histologicalTE),]
    
    studyNames<-paste(df$Author, "", df$Year)
    
    return(studyNames)
  })    
  
  
  
  
  
  output$demoinvivosurvivalforest<-renderPlotly({
    
    p<- viz_forest(demoinvivosurvivalforestdata(), study_labels = 
                     demoinvivosurvivalstudyNames(), text_size=5, xlab = "Survival")
    return(p)
  })
  
  output$demoinvivobehavioralforest<- renderPlotly({
    p<-viz_forest(demoinvivobehavioralforestdata(), 
                  study_labels = demoinvivobehavioralstudyNames(),
                  text_size = 5, xlab = "Behavoural outcomes")
    return(p)
  })
  output$demoinvivobiochemicalforest<-renderPlotly({   
    p<-viz_forest(demoinvivobiochemicalforestdata(), 
                  study_labels = demoinvivobiochemicalstudyNames(),
                  text_size = 5, xlab = "Biochemical outcomes")
    return(p)
  })
  
  
  output$demoinvivohistologicalforest<-renderPlotly({      
    p<-viz_forest(demoinvivohistologicalforestdata(), 
                  study_labels = demoinvivohistologicalstudyNames(),
                  text_size = 5, xlab = "Histological outcomes")
    return(p)
  })
  
  ####invitroforest
  
  demoinvitrocelldeathforestdata<-reactive({
    df<-as.data.frame(demoinvitroPublicationList, stringsasfactors=T
    )
    df[df=="NA"]<-NA
    
    
    df<-df%>%
      filter(Drug %in% input$demodrug)
    df<-df[!is.na(df$celldeathTE),]
    
    df<-data.frame(
      x<-as.numeric(df$celldeathTE),
      y<-as.numeric(df$celldeathseTE))
    
    return(df)
  })
  
  demoinvitrocelldeathstudyNames<-reactive({
    df<-as.data.frame(demoinvitroPublicationList, stringsasfactors=T
    )
    df[df=="NA"]<-NA
    
    
    df<-df%>%
      filter(Drug %in% input$demodrug)
    df<-df[!is.na(df$celldeathTE),]
  
    studyNames<-paste(df$Author, "", df$Year)
    
    return(studyNames)
  })    
  
  output$demoinvitrocelldeathforest<-renderPlotly({      
    p<-viz_forest(demoinvitrocelldeathforestdata(), 
                  study_labels = demoinvitrocelldeathstudyNames(),
                  text_size = 5, xlab = "Cell Death")
    return(p)
  })
  
  
  demoinvitrootherforestdata<-reactive({
    df<-as.data.frame(demoinvitroPublicationList, stringsasfactors=T
    )
    df[df=="NA"]<-NA
    
    
    df<-df%>%
      filter(Drug %in% input$demodrug)
    df<-df[!is.na(df$otherTE),]
    
    df<-data.frame(
      x<-as.numeric(df$otherTE),
      y<-as.numeric(df$otherseTE))
    
    return(df)
  })
  
  demoinvitrootherstudyNames<-reactive({
    df<-as.data.frame(demoinvitroPublicationList, stringsasfactors=T
    )
    df[df=="NA"]<-NA
    
    
    df<-df%>%
      filter(Drug %in% input$demodrug)
    df<-df[!is.na(df$otherTE),]
    
    studyNames<-paste(df$Author, "", df$Year)
    
    return(studyNames)
  })    
  
  output$demoinvitrootherforest<-renderPlotly({      
    p<-viz_forest(demoinvitrootherforestdata(), 
                  study_labels = demoinvitrootherstudyNames(),
                  text_size = 5, xlab = "Other outcome measures")
    return(p)
  })
  
  
  demoselecteddruginvivotable <- reactive({
    demodruginvivopublications <- demoinvivoPublicationList %>%
      filter(Drug %in% input$demodrug) %>%
      select(
        "Title",
        "Drug",
        "Year",
        "Author",
        "Journal",
        "DOI",
        "Model",
        "SOD1",
        "mortalityOutcome",	
        "behavioralOutcome",	
        "biochemicalOutcome",	
        "histologicalOutcome")%>%
      as.data.frame()
    demodruginvivopublications$mortalityOutcome<-factor(ifelse(demodruginvivopublications$mortalityOutcome == 1,"Mortality", ""))
    demodruginvivopublications$behavioralOutcome<-factor(ifelse(demodruginvivopublications$behavioralOutcome == 1,"Behavioural",""))
    demodruginvivopublications$biochemicalOutcome<-factor(ifelse(demodruginvivopublications$biochemicalOutcome == 1,"Biochemical",""))
    demodruginvivopublications$histologicalOutcome<-factor(ifelse(demodruginvivopublications$histologicalOutcome == 1,"Histological",""))
    
    outcomeType<-paste(demodruginvivopublications$mortalityOutcome, "",
                       demodruginvivopublications$behavioralOutcome, "",
                       demodruginvivopublications$biochemicalOutcome, "",
                       demodruginvivopublications$histologicalOutcome)
    
    
    demodruginvivopubtable<-cbind(demodruginvivopublications[,1:8],outcomeType)
    
    return(demodruginvivopubtable)
  })
  
  
  
  output$demodruginvivopublications <-
    DT::renderDataTable(DT::datatable(
      demoselecteddruginvivotable(),
      colnames=c("Title",
                 "Disease",
                 "Year",
                 "Author",
                 "Journal",
                 "DOI",
                 "Model Species",
                 "SOD1 model",
                 "Type of Outcome"),
      filter="top"
    ))
  #####downloadinvivopubs-------------------------------
  
  demoselecteddruginvivopublications <- reactive({
    df <- demoinvivoPublicationList %>%
      filter(Drug %in% input$demodrug) %>%
      select(
        "Title",
        "Drug",
        "Year",
        "Author",
        "Journal",
        "DOI",
        "Model",
        "SOD1",
        "mortalityOutcome",	
        "behavioralOutcome",	
        "biochemicalOutcome",	
        "histologicalOutcome")%>%
      as.data.frame()
    df$mortalityOutcome<-factor(ifelse(df$mortalityOutcome == 1,"Yes","No"))
    df$behavioralOutcome<-factor(ifelse(df$behavioralOutcome == 1,"Yes","No"))
    df$biochemicalOutcome<-factor(ifelse(df$biochemicalOutcome == 1,"Yes","No"))
    df$histologicalOutcome<-factor(ifelse(df$histologicalOutcome == 1,"Yes","No"))
    
    
    return(df)
  })
  
  output$demodownloadinvivoDrugPublications <- downloadHandler(
    filename = function() {
      file <- paste("invivopublications", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(
        demoselecteddruginvivopublications(),
        file = file,
        quote = T,
        row.names = F,
        na = ""
      )
    }
  )
  
  ######invitrodrugCV
  
  demoselecteddruginvitropublications <- reactive({
    demodruginvitropublications <- demoinvitroPublicationList %>%
      filter(Drug %in% input$demodrug) %>%
      select(
        "Title",
        "Drug",
        "Year",
        "Author",
        "Journal",
        "DOI",
        "cdOutcome",	
        "otherOutcome")%>%
      as.data.frame()
    demodruginvitropublications$cdOutcome<-factor(ifelse(demodruginvitropublications$cdOutcome == 1,"Yes","No"))
    demodruginvitropublications$otherOutcome<-factor(ifelse(demodruginvitropublications$otherOutcome == 1,"Yes","No"))
    
    
    
    return(demodruginvitropublications)
  })
  
  
  
  output$demodruginvitropublications <-
    DT::renderDataTable(DT::datatable(
      demoselecteddruginvitropublications(),
      colnames=c("Title",
                 "Disease",
                 "Year",
                 "Author",
                 "Journal",
                 "DOI",
                 "Cell Death Outcome",
                 "Other Outcome"),
      filter = "top"
    ))
  #####downloadinvitropubs-------------------------------
  output$demodownloadinvitroDrugPublications <- downloadHandler(
    filename = function() {
      file <- paste("invitropublications", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(
        demoselecteddruginvitropublications(),
        file = file,
        quote = T,
        row.names = F,
        na = ""
      )
    }
  )
  
  
  
  
  
  #download---------------------------------------------
  #
  demopList<-    as.data.frame(demopublicationList)
  
  output$democatclinicalpubs<-downloadHandler(
    filename=function() {
      file<-paste("cat_clinicalpubs", Sys.Date(), ".csv", sep="")},
    content=function(file){
      write.csv(
        demopList,
        file=file, 
        quote=T,
        row.names=F,
        na="")})
  
  output$democatinvivopubs<-downloadHandler(
    filename=function(){
      file<-paste("cat_invivopubs", Sys.Date(), ".csv", sep="")},
    content=function(file){
      write.csv(
        demoinvivoPublicationList(),
        file=file, 
        quote=T,
        row.names=F,
        na="")})
  
  output$democatinvitropubs<-downloadHandler(
    filename=function(){
      file<-paste("cat_invitropubs", Sys.Date(), ".csv", sep="")},
    content=function(file){
      write.csv(
        demoinvitroPublicationList(),
        file=file, 
        quote=T,
        row.names=F,
        na="")})
  
  
  
  
  
  ##########LIVE VERSION#######################################
  #drug recommendations-------------------------------------
  
  
  ####live drug recommendations---------------------
  #live drug recommendations-------------------------------------
  livedrugranklist <- reactive({
    mydf <- drugSummary %>%
      select(
        "Drug",
        "productScore",
        "nPublication",
        "efficacyScore"  ,
        "safetyScore"  ,
        "studySizeScore" ,
        "qualityScore"  ,
        "invivoSurvivalSMD",
        "nInvivoStudies"   ,
        "nCellDeathSMD"   ,
        "nInVitroStudies"
      ) %>%
      arrange(desc(productScore))
    
    cols <- c(    "productScore",
                  "efficacyScore"  ,
                  "safetyScore"  ,
                  "studySizeScore" ,
                  "qualityScore"    )
    
    mydf[,cols] <-round(mydf[,cols],2)
    return(mydf)
  })
  ########drugtable-------------------------------------
  output$livedrugranklist <- DT::renderDataTable(DT::datatable(
    livedrugranklist(),
    colnames=c("Drug",
               "Clinical Product Score",
               "Clinical n(Pub)", 
               "[E]",
               "[S]",
               "[SS]",
               "[Q]",
               "In vivo survival SMD",
               "in vivo n(Pub)",
               "In vitro cell death SMD",
               "in vitro n(Pub)"),
    filter="top",
    caption= htmltools::tags$caption(
      style='caption-side:bottom; text-align:center;', 
      "Current results summary of (i) clinical review: publications for interventions were scored against predefined criteria (see the About tab for more information). Clinical publications were assigned scores for efficacy[E], safety[S], study size [SS] and quality [Q] (1: worst, 4: best). For each intervention, using the median scores from publications, a product score was generated. (Product score = log10(1 + number of clinical publications)*[E]*[S]*[SS]*[Q]); (ii)in vivo and (iii) in vitro review are reported here by the standard mean deviation (SMD) in survival and cell death studies respectively calculated from our meta-analysis. n(Pub) refers to number of publications annotated for each drug in our review.")
  ))
  #######download drug table-------------------------------------
  output$downloadLiveDrugTable <- downloadHandler(
    filename = function() {
      file<-paste("drugtable-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(livedrugranklist(), 
                file=file,
                quote=T,
                row.names=F,
                na="")
    }
  )
  
  
  
  ##### enable when invivo/invitro data available.
  ####### LIVE drugbubblechart-------------------------------------
  # output$livedrugrankchart <- renderPlotly({
  #    livedrugrankchart <- plot_ly(
  #      NewDrugSummary,
  #      x = ~ invivoSurvivalSMD,
  #      y = ~ nCellDeathSMD,
  #      hoverinfo = 'text',
  
  #      text = ~ paste(
  #        '</br> Drug:',
  #        Drug,
  #        '</br> Clinical Score:',
  #        round(productScore, digits = 2),
  #        '</br> n(clinical publications):',
  #        nPublication,
  #        '</br> in vivo SMD:',
  #        round(invivoSurvivalSMD, digits = 2),
  #        '</br> in vitro SMD:',
  #        round(nCellDeathSMD),
  #        digits = 2
  #      ),
  #      type = 'scatter',
  #      mode = 'markers',
  #      
  #      marker = list(size = ~ nPublication,
  #                    opacity = 0.5,
  #                    color = ~ productScore,
  #                    colorscale = 'Viridis', 
  #                    reversescale=TRUE,
  #                    showscale=T,
  #                    colorbar=list(title='Product Score'),
  #                    sizemin= 3
  #     )
  #    )%>%
  #      layout(title="Clinical, in vivo and in vitro scores by drug",
  #             xaxis=(list(title="In vivo Survival SMD")),
  #             yaxis=(list(title="In vitro cell death SMD")))
  #    livedrugrankchart
  #  })
  
  
  
  ###############
  #drug heatmap-------------------------------------
  productscoredata <- drugSummary %>%
    select("productScore")%>%
    arrange(desc(productScore))
  productscoredata <- as.matrix(productscoredata)
  rownames(productscoredata) <- drugSummary$Drug
  
  clinicaldata <- drugSummary %>%
    select("efficacyScore"  ,
           "safetyScore"  ,
           "studySizeScore" ,
           "qualityScore")
  clinicaldata <- as.matrix(clinicaldata)
  rownames(clinicaldata) <- drugSummary$Drug
  
  
  preclinicaldata <- drugSummary %>%
    select("invivoSurvivalSMD", "nCellDeathSMD")
  preclinicaldata <- as.matrix(preclinicaldata)
  rownames(preclinicaldata) <- drugSummary$Drug
  
  
  output$hm<-renderPlotly({
    fig3<-plot_ly(z=productscoredata, x=colnames(productscoredata), y=rownames(productscoredata)
                  , type='heatmap', height=1300, width= 1000, colorscale="Viridis", reversescale=TRUE, 
                  colorbar=list(title='Clinical Product Score'))%>%
      layout(yaxis=list(autorange='reversed'),
             xaxis=(list(title="Clinical Product Score")))
    
    fig1<-plot_ly(z=clinicaldata, x=colnames(clinicaldata), y=rownames(clinicaldata)
                  , type='heatmap', colorscale="magma", colorbar=list(title='Clinical subscores'))
    
    fig2<-plot_ly(z=preclinicaldata, x=colnames(preclinicaldata), y=rownames(preclinicaldata)
                  , type='heatmap', colorscale= "Blues", reversescale=TRUE, colorbar=list(title='Preclinical SMD'))
    
    hm<-subplot(fig3,fig1,fig2, shareY=T, widths=c(1/7, 4/7, 2/7))%>%layout(height=1300, width=900)
    
  })
  
  
  #progress-------------------------------------
  
  
  #####plotlysunburst-----------------------------
  
  
  sunburstdata <- publicationList %>%
    select(Disease, studyType, phase, Drug, StudyIdStr)%>%
    unique() %>%
    group_by(Disease, studyType, phase, Drug, StudyIdStr) %>%
    count() %>%
    rename(value = n) %>%
    ungroup()
  
  DF0 <- sunburstdata %>% 
    group_by(Disease) %>% 
    unique() %>%
    summarise(value=sum(value))
  
  DF1 <- sunburstdata %>% 
    group_by(Disease, studyType) %>% 
    summarise(value=sum(value))
  
  DF2 <- sunburstdata %>% 
    group_by(Disease, studyType, phase) %>%
    summarise(value=sum(value))
  
  DF3 <- sunburstdata %>% 
    group_by(Disease, studyType, phase, Drug) %>%
    summarise(value=sum(value))
  
  df0 <- data.frame(
    ids = paste(DF0$Disease),
    labels = DF0$Disease,
    parents = "",
    values = DF0$value,
    stringsAsFactors = F
  )
  
  df1 <- data.frame(
    ids = paste(DF1$Disease, "-", DF1$studyType),
    labels = DF1$studyType,
    parents = paste(DF1$Disease),
    values = DF1$value,
    stringsAsFactors = F
  )
  
  df2 <- data.frame(
    ids = paste(DF2$Disease, "-", DF2$studyType, "-", DF2$phase),
    labels = DF2$phase,
    parents = paste(DF2$Disease, "-", DF2$studyType),
    values = DF2$value,
    stringsAsFactors = F
  )
  
  df3 <- data.frame(
    ids = paste(DF3$Disease, "-", DF3$studyType, "-", DF3$phase, "-", DF3$Drug),
    labels = DF3$Drug,
    parents = paste(DF3$Disease, "-", DF3$studyType, "-", DF3$phase),
    values = DF3$value,
    stringsAsFactors = F
  )
  
  df <- rbind(df0, df1, df2, df3)
  
  
  
  output$sb2 <- renderPlotly({
    
    p <- plot_ly(df,
                 ids = ~ids,
                 labels = ~labels,
                 parents = ~parents,
                 type = 'sunburst',
                 values =  ~values,
                 branchvalues = "total", 
                 insidetextorientation='auto',
                 insidetextfont = list(size=12),
                 marker = list(colors = "Viridis", 
                               line = list(color = "white", width=2)),
                 source = "sunburstPlot")%>%
      layout(autosize = F, 
             paper_bgcolor="transparent",
             plot_bgcolor='transparent',
             height=700,
             width=700,
             margin = list(b = 80, l = 80, r = 80, t = 100, pad = 0, autoexpand = TRUE))
    
    event_register(p, 'plotly_click')
    
    p
    
  })
  
  
  
  
  
  
  
  ########clinicalprogress-------------------------------------
  output$ClinicalUniquePubs <- renderInfoBox({
    infoBox(
      "Unique publications identified",
      as.numeric(nClinicalUniquePubs),
      icon = icon("file-text-o"),
      color = "red"
    )
  })
  
  output$ClinicalIncludedPubs <- renderInfoBox({
    infoBox(
      "Included publications",
      as.numeric(nClinicalIncludedPubs),
      icon = icon("file-text-o"),
      color = "orange"
    )
  })
  
  output$ClinicalDrugMeetLogic <- renderInfoBox({
    infoBox(
      "Drugs meeting inclusion logic",
      as.numeric(nClinicalDrugMeetLogic),
      icon = icon("tablets"),
      color = "aqua"
    )
  })
  
  output$ClinicalPublicationsMeetLogic <- renderInfoBox({
    infoBox(
      "Publications for included drugs",
      as.numeric(nClinicalPublicationsMeetLogic),
      icon = icon("file-text-o"),
      color = "aqua"
    )
  })
  
  output$ClinicalCoreDrugs <- renderInfoBox({
    infoBox(
      "Longlisted Drugs",
      as.numeric(nClinicalCoreDrugs),
      icon = icon("tablets"),
      color = "blue"
    )
  })
  
  output$ClinicalCorePubs <- renderInfoBox({
    infoBox(
      "Publications for longlisted drugs",
      as.numeric(nClinicalCoreDrugsPubs),
      icon = icon("file-text-o"),
      color = "blue"
    )
  })
  
  output$ClinicalSingleAnnot <- renderInfoBox({
    infoBox(
      "Single Annotated Publications",
      as.numeric(nClinicalSingleAnnotated),
      icon = icon("user"),
      color = "green"
    )
  })
  
  output$ClinicalDualAnnot <- renderInfoBox({
    infoBox(
      "Dual Annotated Publications",
      as.numeric(nClinicalDualAnnotated),
      icon = icon("user-friends"),
      color = "green"
    )
  })
  
  
  output$ClinicalReconciled <- renderInfoBox({
    infoBox(
      "Reconciled publications",
      as.numeric(nClinicalReconciled),
      icon = icon("check"),
      color = "olive"
    )
  })
  
  ######in vivo progress-------------------------------------
  output$InvivoUniquePubs <- renderInfoBox({
    infoBox(
      "Unique publications identified",
      as.numeric(nInvivoUniquePubs),
      icon = icon("file-text-o"),
      color = "red"
    )
  })
  
  output$InvivoIncludedPubs <- renderInfoBox({
    infoBox(
      "Included publications",
      as.numeric(nInvivoIncludedPubs),
      icon = icon("file-text-o"),
      color = "orange"
    )
  })
  
  output$InvivoDrugMeetLogic <- renderInfoBox({
    infoBox(
      "Drugs meeting inclusion logic",
      as.numeric(nInvivoDrugMeetLogic),
      icon = icon("tablets"),
      color = "aqua"
    )
  })
  
  output$InvivoPublicationsMeetLogic <- renderInfoBox({
    infoBox(
      "Publications for included drugs",
      as.numeric(nInvivoPublicationsMeetLogic),
      icon = icon("file-text-o"),
      color = "aqua"
    )
  })
  
  output$InvivoCoreDrugs <- renderInfoBox({
    infoBox(
      "Longlisted Drugs",
      as.numeric(nInvivoCoreDrugs),
      icon = icon("tablets"),
      color = "blue"
    )
  })
  
  output$InvivoCorePubs <- renderInfoBox({
    infoBox(
      "Publications for longlisted drugs",
      as.numeric(nInvivoCoreDrugsPubs),
      icon = icon("file-text-o"),
      color = "blue"
    )
  })
  
  output$InvivoSingleAnnot <- renderInfoBox({
    infoBox(
      "Single Annotated Publications",
      as.numeric(nInvivoSingleAnnotated),
      icon = icon("user"),
      color = "green"
    )
  })
  
  output$InvivoDualAnnot <- renderInfoBox({
    infoBox(
      "Dual Annotated Publications",
      as.numeric(nInvivoDualAnnotated),
      icon = icon("user-friends"),
      color = "green"
    )
  })
  
  
  output$InvivoReconciled <- renderInfoBox({
    infoBox(
      "Reconciled publications",
      as.numeric(nInvivoReconciled),
      icon = icon("check"),
      color = "olive"
    )
  })
  
  #######invitro progress-------------------------------------
  
  output$InvitroUniquePubs <- renderInfoBox({
    infoBox(
      "Unique publications identified",
      as.numeric(nInvitroUniquePubs),
      icon = icon("file-text-o"),
      color = "red"
    )
  })
  
  output$InvitroIncludedPubs <- renderInfoBox({
    infoBox(
      "Included publications",
      as.numeric(nInvitroIncludedPubs),
      icon = icon("file-text-o"),
      color = "orange"
    )
  })
  
  output$InvitroDrugMeetLogic <- renderInfoBox({
    infoBox(
      "Drugs meeting inclusion logic",
      as.numeric(nInvitroDrugMeetLogic),
      icon = icon("tablets"),
      color = "aqua"
    )
  })
  
  output$InvitroPublicationsMeetLogic <- renderInfoBox({
    infoBox(
      "Publications for included drugs",
      as.numeric(nInvitroPublicationsMeetLogic),
      icon = icon("file-text-o"),
      color = "aqua"
    )
  })
  
  output$InvitroCoreDrugs <- renderInfoBox({
    infoBox(
      "Longlisted Drugs",
      as.numeric(nInvitroCoreDrugs),
      icon = icon("tablets"),
      color = "blue"
    )
  })
  
  output$InvitroCorePubs <- renderInfoBox({
    infoBox(
      "Publications for longlisted drugs",
      as.numeric(nInvitroCoreDrugsPubs),
      icon = icon("file-text-o"),
      color = "blue"
    )
  })
  
  output$InvitroSingleAnnot <- renderInfoBox({
    infoBox(
      "Single Annotated Publications",
      as.numeric(nInvitroSingleAnnotated),
      icon = icon("user"),
      color = "green"
    )
  })
  
  output$InvitroDualAnnot <- renderInfoBox({
    infoBox(
      "Dual Annotated Publications",
      as.numeric(nInvitroDualAnnotated),
      icon = icon("user-friends"),
      color = "green"
    )
  })
  
  
  output$InvitroReconciled <- renderInfoBox({
    infoBox(
      "Reconciled publications",
      as.numeric(nInvitroReconciled),
      icon = icon("check"),
      color = "olive"
    )
  })
  
  
  #drugCV-------------------------------------
  
  ####selected drug chart activate when invivo invitro available
  # output$selecteddrugrankchart<-renderPlotly({
  #    drugrankchart <- plot_ly(
  #      drugSummary,
  #      x = ~ invivoSurvivalSMD,
  #      y = ~ nCellDeathSMD,
  #      hoverinfo = 'text',
  #      
  #      text = ~ paste(
  #        '</br> Drug:',
  #        Drug,
  #        '</br> Clinical Score:',
  #        round(productScore, digits = 2),
  #        '</br> n(clinical publications):',
  #        nPublication,
  #        '</br> in vivo SMD:',
  #        round(invivoSurvivalSMD, digits = 2),
  #        '</br> in vitro SMD:',
  #        round(nCellDeathSMD),
  #        digits = 2
  #      ),
  #      type = 'scatter',
  #      mode = 'markers',
  #      
  #      marker = list(size = ~ nPublication,
  #                    opacity = 0.5,
  #                    color = ~ productScore,
  #                    colorscale = 'Viridis', 
  #                    reversescale=TRUE,
  #                    showscale=T,
  #                    colorbar=list(title='Product Score'),
  #                    sizemin= 3
  #      )
  #    )%>%
  #      layout(title="Clinical, in vivo and in vitro scores by drug",
  #             xaxis=(list(title="In vivo Survival SMD")),
  #             yaxis=(list(title="In vitro cell death SMD")))
  #    drugrankchart
  #    
  #    drugbubble<-drugrankchart%>%add_markers()
  #    drugbubble<-drugbubble%>%layout(annotations=drugannot(input$drug), showlegend=F)
  #    drugbubble
  #  })
  ###########################
  ##
  selecteddrugranklist <- reactive({
    drugSummary %>%
      filter(Drug%in% input$drug)%>%
      select(
        "Drug",
        "productScore",
        "nPublication",
        "efficacyScore"  ,
        "safetyScore"  ,
        "studySizeScore" ,
        "qualityScore"  ,
        "invivoSurvivalSMD",
        "nInvivoStudies"   ,
        "nCellDeathSMD"   ,
        "nInVitroStudies"
      ) %>%
      arrange(desc(productScore))
    
  })
  ########drugtable-------------------------------------
  output$selecteddrugranklist <- DT::renderDataTable(DT::datatable(
    selecteddrugranklist(),
    colnames=c("Drug",
               "Clinical Product Score",
               "Clinical n(Pub)", 
               "[E]",
               "[S]",
               "[SS]",
               "[Q]",
               "In vivo survival SMD",
               "in vivo n(Pub)",
               "In vitro cell death SMD",
               "in vitro n(Pub)"),
    caption= htmltools::tags$caption(
      style='caption-side:bottom; text-align:center;', 
      "Current results summary of (i) clinical review: publications for interventions were scored against predefined criteria (see the About tab for more information). Clinical publications were assigned scores for efficacy[E], safety[S], study size [SS] and quality [Q] (1: worst, 4: best). For each intervention, using the median scores from publications, a product score was generated. (Product score = log10(1 + number of clinical publications)*[E]*[S]*[SS]*[Q]); (ii)in vivo and (iii) in vitro review are reported here by the standard mean deviation (SMD) in survival and cell death studies respectively calculated from our meta-analysis. n(Pub) refers to number of publications annotated for each drug in our review."),
    list(dom='t',
         ordering=F),
    rownames=F
  ))
  
  #####ClinicalPubSummary-------------------------------------
  
  selectedclinpubsummary <- reactive({
    drugpubdata <- drugSummary %>%
      filter(Drug %in% input$drug) %>%
      select(
        "nPublication",
        "nMND",
        "nAD",
        "nFTD",
        "nHD",
        "nMS",
        "nPD"
      )
    
    tdrugpubdata <- t(as.matrix(drugpubdata))
    return(tdrugpubdata)
  })
  
  output$selectedclinpubsummary <-
    DT::renderDataTable(DT::datatable(
      selectedclinpubsummary(),
      rownames=c("All diseases of interest", 
                 "MND", 
                 "AD",
                 "FTD",
                 "HD",
                 "MS",
                 "PD"),
      colnames="Number of publications",
      list(dom = 't',
           ordering=F)
      
    )%>%
      formatStyle(
        0,
        target = "row",
        fontWeight = styleEqual("All diseases of interest", "bold"),
        backgroundColor = styleEqual("All diseases of interest", "lightblue")
      )
    )
  #####ClinicalScoreSummary-------------------------------------
  selectedclinscoresummary <- reactive({
    drugscoredata <- drugSummary %>%
      filter(Drug %in% input$drug) %>%
      select(
        "productScore",
        "efficacyScore",
        "safetyScore",
        "studySizeScore",
        "qualityScore"
      )
    
    tdrugscoredata <- t(as.matrix(drugscoredata))
    return(tdrugscoredata)
  })
  
  output$selectedclinscoresummary <-
    DT::renderDataTable(DT::datatable(
      selectedclinscoresummary(),
      rownames=c("Product Score",
                 "Efficacy Score",
                 "Safety Score",
                 "Study Size Score",
                 "Quality Score"),
      colnames="Score",
      list(
        dom = 't',
        ordering=F
      ))%>%
        formatStyle(
          0,
          target = "row",
          fontWeight = styleEqual("Product Score", "bold"),
          backgroundColor = styleEqual("Product Score", "lightblue")
        )
    )
  
  
  ######drugsunburst-------------------------------------
  
  
  sdsunburstdata <- reactive({
    drugsbdata<-publicationList %>%
      filter(Drug %in% input$drug)%>%
      select(Disease, studyType, phase, StudyIdStr)%>%
      unique() %>%
      group_by(Disease, studyType, phase, StudyIdStr) %>%
      count() %>%
      rename(value = n) %>%
      ungroup()
    
    
    DF0 <- drugsbdata %>% 
      group_by(Disease) %>% 
      unique() %>%
      summarise(value=sum(value))
    
    DF1 <- drugsbdata %>% 
      group_by(Disease, studyType) %>% 
      summarise(value=sum(value))
    
    DF2 <- drugsbdata %>% 
      group_by(Disease, studyType, phase) %>%
      summarise(value=sum(value))
    
    df0 <- data.frame(
      ids = paste(DF0$Disease),
      labels = DF0$Disease,
      parents = "",
      values = DF0$value,
      stringsAsFactors = F
    )
    
    df1 <- data.frame(
      ids = paste(DF1$Disease, "-", DF1$studyType),
      labels = DF1$studyType,
      parents = paste(DF1$Disease),
      values = DF1$value,
      stringsAsFactors = F
    )
    
    df2 <- data.frame(
      ids = paste(DF2$Disease, "-", DF2$studyType, "-", DF2$phase),
      labels = DF2$phase,
      parents = paste(DF2$Disease, "-", DF2$studyType),
      values = DF2$value,
      stringsAsFactors = F
    )
    
    
    
    df <- rbind(df0, df1, df2)
    
    return(df)
  })
  
  
  
  output$sb3 <- renderPlotly({
    
    p <- plot_ly(sdsunburstdata(),
                 ids = ~ids,
                 labels = ~labels,
                 parents = ~parents,
                 type = 'sunburst',
                 values =  ~values,
                 branchvalues = "total", 
                 insidetextorientation='auto',
                 insidetextfont = list(size=12),
                 marker = list(colors = "Viridis", 
                               line = list(color = "white", width=2)),
                 source = "sunburstPlot")%>%
      layout(autosize = F, 
             paper_bgcolor="transparent",
             plot_bgcolor='transparent',
             height=300,
             width=300,
             margin = list(b = 0, l = 0, r = 0, t = 30, pad = 0, autoexpand = TRUE))
    
    event_register(p, 'plotly_click')
    
    p
    
  })
  
  #####drugptsunburst---------------------
  
  sdptsunburstdata <- reactive({
    drugptsbdata<-publicationList %>%
      filter(Drug %in% input$drug)%>%
      select(Disease, studyType, phase, nPatients)%>%
      unique() %>%
      ungroup()
    
    
    DF0 <- drugptsbdata %>% 
      group_by(Disease) %>% 
      unique() %>%
      summarise(nPatients=sum(nPatients))
    
    DF1 <- drugptsbdata %>% 
      group_by(Disease, studyType) %>% 
      summarise(nPatients=sum(nPatients))
    
    DF2 <- drugptsbdata %>% 
      group_by(Disease, studyType, phase) %>%
      summarise(nPatients=sum(nPatients))
    
    df0 <- data.frame(
      ids = paste(DF0$Disease),
      labels = DF0$Disease,
      parents = "",
      values = DF0$nPatients,
      stringsAsFactors = F
    )
    
    df1 <- data.frame(
      ids = paste(DF1$Disease, "-", DF1$studyType),
      labels = DF1$studyType,
      parents = paste(DF1$Disease),
      values = DF1$nPatients,
      stringsAsFactors = F
    )
    
    df2 <- data.frame(
      ids = paste(DF2$Disease, "-", DF2$studyType, "-", DF2$phase),
      labels = DF2$phase,
      parents = paste(DF2$Disease, "-", DF2$studyType),
      values = DF2$nPatients,
      stringsAsFactors = F
    )
    
    
    
    df <- rbind(df0, df1, df2)
    
    return(df)
  })
  
  
  
  output$ptsb <- renderPlotly({
    
    p <- plot_ly(sdptsunburstdata(),
                 ids = ~ids,
                 labels = ~labels,
                 parents = ~parents,
                 type = 'sunburst',
                 values =  ~values,
                 branchvalues = "total", 
                 insidetextorientation='auto',
                 insidetextfont = list(size=12),
                 marker = list(colors = "Viridis", 
                               line = list(color = "white", width=2)),
                 source = "sunburstPlot")%>%
      layout(autosize = F, 
             paper_bgcolor="transparent",
             plot_bgcolor='transparent',
             height=300,
             width=300,
             margin = list(b = 0, l = 0, r = 0, t = 30, pad = 0, autoexpand = TRUE))
    
    event_register(p, 'plotly_click')
    
    p
    
  })
  
  
  
  
  
  
  
  #####clinicalpubs-------------------------------------
  
  
  
  
  selecteddrugclinicalpubtable <- reactive({
    drugclinicalpubtable <- publicationList %>%
      filter(Drug %in% input$drug) %>%
      select(
        "Title",
        "Disease",
        "Drug",
        "Year",
        "Author",
        "Journal",
        "DOI",
        "studyType",
        "phase",
        "nPatients"
      )
    
    return(drugclinicalpubtable)
  })
  
  output$drugclinicalpublications <-
    DT::renderDataTable(DT::datatable(
      selecteddrugclinicalpubtable(),
      colnames=c("Title",
                 "Disease",
                 "Drug",
                 "Year",
                 "Author",
                 "Journal",
                 "DOI",
                 "Type of study",
                 "Study Phase",
                 "n(patients)"),
      filter="top"
    ))
  
  selecteddrugclinicalpublications <- reactive({
    drugclinicalpublications <- publicationList %>%
      filter(Drug %in% input$drug) %>%
      select(
        "Title",
        "Disease",
        "Drug",
        "Year",
        "Author",
        "Journal",
        "DOI",
        "studyType",
        "phase",
        "nPatients",
        "efficacyScore",
        "safetyScore",
        "studySizeScore",
        "qualityScore"
      )%>%unique()
    
    return(drugclinicalpublications)
  })
  
  
  
  #####downloadclinicalpubs-------------------------------------
  output$downloadDrugPublications <- downloadHandler(
    filename = function() {
      file <- paste("clinicalpublications", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(
        selecteddrugclinicalpublications(),
        file = file,
        quote = T,
        row.names = F,
        na = ""
      )
    }
  )
  
  
  ######invivodrugCV
  selecteddruginvivotable <- reactive({
    druginvivopublications <- invivoPublicationList %>%
      filter(Drug %in% input$drug) %>%
      select(
        "Title",
        "Drug",
        "Year",
        "Author",
        "Journal",
        "DOI",
        "Model",
        "SOD1",
        "mortalityOutcome",	
        "behavioralOutcome",	
        "biochemicalOutcome",	
        "histologicalOutcome")%>%
      as.data.frame()
    druginvivopublications$mortalityOutcome<-factor(ifelse(druginvivopublications$mortalityOutcome == 1,"Mortality", ""))
    druginvivopublications$behavioralOutcome<-factor(ifelse(druginvivopublications$behavioralOutcome == 1,"Behavioural",""))
    druginvivopublications$biochemicalOutcome<-factor(ifelse(druginvivopublications$biochemicalOutcome == 1,"Biochemical",""))
    druginvivopublications$histologicalOutcome<-factor(ifelse(druginvivopublications$histologicalOutcome == 1,"Histological",""))
    
    outcomeType<-paste(druginvivopublications$mortalityOutcome, "",
                       druginvivopublications$behavioralOutcome, "",
                       druginvivopublications$biochemicalOutcome, "",
                       druginvivopublications$histologicalOutcome)
    
    
    druginvivopubtable<-cbind(druginvivopublications[,1:8],outcomeType)
    
    return(druginvivopubtable)
  })
  
  
  
  output$druginvivopublications <-
    DT::renderDataTable(DT::datatable(
      selecteddruginvivotable(),
      colnames=c("Title",
                 "Disease",
                 "Year",
                 "Author",
                 "Journal",
                 "DOI",
                 "Model Species",
                 "SOD1 model",
                 "Type of Outcome"),
      filter="top"
    ))
  #####downloadinvivopubs-------------------------------
  
  selecteddruginvivopublications <- reactive({
    df <- invivoPublicationList %>%
      filter(Drug %in% input$drug) %>%
      select(
        "Title",
        "Drug",
        "Year",
        "Author",
        "Journal",
        "DOI",
        "Model",
        "SOD1",
        "mortalityOutcome",	
        "behavioralOutcome",	
        "biochemicalOutcome",	
        "histologicalOutcome")%>%
      as.data.frame()
    df$mortalityOutcome<-factor(ifelse(df$mortalityOutcome == 1,"Yes","No"))
    df$behavioralOutcome<-factor(ifelse(df$behavioralOutcome == 1,"Yes","No"))
    df$biochemicalOutcome<-factor(ifelse(df$biochemicalOutcome == 1,"Yes","No"))
    df$histologicalOutcome<-factor(ifelse(df$histologicalOutcome == 1,"Yes","No"))
    
    
    return(df)
  })
  
  output$downloadinvivoDrugPublications <- downloadHandler(
    filename = function() {
      file <- paste("invivopublications", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(
        selecteddruginvivopublications(),
        file = file,
        quote = T,
        row.names = F,
        na = ""
      )
    }
  )
  
  ######invitrodrugCV
  
  selecteddruginvitropublications <- reactive({
    druginvitropublications <- invitroPublicationList %>%
      filter(Drug %in% input$drug) %>%
      select(
        "Title",
        "Drug",
        "Year",
        "Author",
        "Journal",
        "DOI",
        "cdOutcome",	
        "otherOutcome")%>%
      as.data.frame()
    druginvitropublications$cdOutcome<-factor(ifelse(druginvitropublications$cdOutcome == 1,"Yes","No"))
    druginvitropublications$otherOutcome<-factor(ifelse(druginvitropublications$otherOutcome == 1,"Yes","No"))
    
    
    
    return(druginvitropublications)
  })
  
  
  
  output$druginvitropublications <-
    DT::renderDataTable(DT::datatable(
      selecteddruginvitropublications(),
      colnames=c("Title",
                 "Disease",
                 "Year",
                 "Author",
                 "Journal",
                 "DOI",
                 "Cell Death Outcome",
                 "Other Outcome"),
      filter = "top"
    ))
  #####downloadinvitropubs-------------------------------
  output$downloadinvitroDrugPublications <- downloadHandler(
    filename = function() {
      file <- paste("invitropublications", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(
        selecteddruginvitropublications(),
        file = file,
        quote = T,
        row.names = F,
        na = ""
      )
    }
  )
  
  
  
  
  
  #download---------------------------------------------
  #
  pList<-    as.data.frame(publicationList)
  
  output$catclinicalpubs<-downloadHandler(
    filename=function() {
      file<-paste("cat_clinicalpubs", Sys.Date(), ".csv", sep="")},
    content=function(file){
      write.csv(
        pList,
        file=file, 
        quote=T,
        row.names=F,
        na="")})
  
  output$catinvivopubs<-downloadHandler(
    filename=function(){
      file<-paste("cat_invivopubs", Sys.Date(), ".csv", sep="")},
    content=function(file){
      write.csv(
        invivoPublicationList(),
        file=file, 
        quote=T,
        row.names=F,
        na="")})
  
  output$catinvitropubs<-downloadHandler(
    filename=function(){
      file<-paste("cat_invitropubs", Sys.Date(), ".csv", sep="")},
    content=function(file){
      write.csv(
        invitroPublicationList(),
        file=file, 
        quote=T,
        row.names=F,
        na="")})
  
  
  #####download all publications-----------------------
  #output$allclinicalpubs
  #output$allinvivopubs
  #output$allinvitropubs
  
})