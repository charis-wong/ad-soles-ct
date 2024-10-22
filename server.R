shinyServer(function(input, output, session) {
  showModal(modalDialog(
    title="Disclaimer: This web application is under development and has not been officially released yet."
  ))
  
  #live drug recommendations-------------------------------------

  drugSummary <- reactive({     
    drugSummary <- googlesheets4::read_sheet(googleSheetId, sheet = "drugSummary")
    drugSummary
  })

  publicationList <- reactive({ googlesheets4::read_sheet(googleSheetId, sheet = "publicationList")  })

  progressSummary <-reactive({googlesheets4::read_sheet(googleSheetId, sheet = "progressSummary")  })
  
  clinicalFTInclusionSummary <-reactive({googlesheets4::read_sheet(googleSheetId, sheet = "clinicalFTInclusionSummary")  })
  
  reviewerSession <-reactive({
    sessions <- googlesheets4::read_sheet(googleSheetId, sheet = "reviewerSession")
    sessions$DateCreated<-as.Date(sessions$DateTimeCreated)
    sessions <- sessions[!is.na(sessions$Name),]
    
    return(sessions)
  })
  
  reviewerList <- reactive({reviewerSession() %>% select(Name)%>%unique()})
  
  createStatistics <- function(irow){
    reviewSummary = progressSummary()[irow, ]
    list(
      UpdateDate = reviewSummary$lastUpdate,
      nUniquePubs = reviewSummary$nUniquePublications,
      nIncludedPubs =      reviewSummary$nIncludedPublications,
      nDrugMeetLogic = reviewSummary$nDrugMeetLogic,
      nPublicationsMeetLogic = reviewSummary$nPublicationsMeetLogic,
      nCoreDrugs = reviewSummary$nCoreDrugs,
      nCoreDrugsPubs =  reviewSummary$nCoreDrugPublications,
      nSingleAnnotated = reviewSummary$nSingleAnnotated,
      percentSingleAnnotated =  round(reviewSummary$nSingleAnnotated / reviewSummary$nCoreDrugPublications * 100,2),
      nDualAnnotated = reviewSummary$nDualAnnotated,
      percentDualAnnotated =  round(reviewSummary$nDualAnnotated / reviewSummary$nCoreDrugPublications * 100,2),
      nReconciled = reviewSummary$nReconciled,
      percentReconciled = round(reviewSummary$nReconciled / reviewSummary$nCoreDrugPublications * 100,2)
    )
  }
  
  clinicalStatistics <- reactive({    createStatistics(1)  })

  
  invivoStudies<- reactive({RMySQL::dbReadTable(con, "ad_invivo_citations")})
  

  ########drugtable-------------------------------------
  drugRankList <-  reactive({
    mydf <- drugSummary() %>%
      select(
        "Drug",
        "drugScore",
        "nPublication",
        "efficacyScore"  ,
        "safetyScore"  ,
        "nParticipants" ,
        "qualityScore"
      ) %>%
      arrange(desc(drugScore))
    
    cols <- c(    "drugScore",
                  "efficacyScore"  ,
                  "safetyScore"  ,
                  "nParticipants" ,
                  "qualityScore"    )
    
    mydf[,cols] <- round(mydf[,cols], 2)
    return(mydf)
  })
  
  
  output$livedrugranklist <- DT::renderDataTable(DT::datatable(
    drugRankList()
    , rownames=F
    , colnames=c("Drug","Clinical Drug Score","Clinical n(Pub)","median efficacy (-2 - 4)","median safety score (0-3)","median number of participants","median quality score (out of 24)")
    , caption= htmltools::tags$caption(style = 'caption-side:bottom; text-align:left;',   c("Current results summary of (i) clinical review: For each publication, we calculated a distance score based on Euclidean distance of efficacy and safety scores weighted by quality and study size. For each drug, we calculate a drug score using the number of publications describing the drug (n) and median publication distance score for all publications describing data for that drug:", withMathJax("$$\\text{drug score}\\ = log10{(n+1)} \\times {(\\text{median distance score})}$$"), "Separately, we calculate median subscores for efficacy, safety, quality and number of participants across all publications for each drug. See 'About' tab for more details on scoring; (ii) in vivo and (iii) in vitro review are reported here by the standard mean deviation (SMD) in survival and cell death studies respectively calculated from our meta-analysis. n(Pub) refers to number of publications annotated for each drug in our review."), escape=F) 
    , filter="top"
    , options = list(pageLength = 10, dom = 'Bflrtip', 
                     #buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                     buttons = list(list(extend = "copy"), 
                                    list(extend = "csv", filename = paste0("AD-SOLES-CT Drug recommendations", Sys.Date())), 
                                    list(extend = "excel", filename = paste0("AD-SOLES-CT Drug recommendations", Sys.Date())),  
                                    list(extend = "pdf", filename = paste0("AD-SOLES-CT Drug recommendations", Sys.Date())), 
                                    list(extend = "print")
                     ))
    , extensions = c('Buttons','Responsive'),
    escape=F
  ))

  
  
  
  
  #drug heatmap-------------------------------------
  output$heatmap<-renderPlotly({
    drugScoredata <- 
      drugScoredata <- drugSummary() %>%
      select("Drug","drugScore")%>%
      arrange(desc(drugScore))
    rownames(drugScoredata) <- drugScoredata$Drug
    drugScores <- as.matrix(drugScoredata$drugScore)
    colnames(drugScores) <-"Clinical Drug Score"
    
    efficacydata<-drugSummary()%>%
      select("efficacyScore")
    rownames(efficacydata)<-drugSummary()$Drug
    efficacydata<-as.matrix(efficacydata)
    
    
    safetydata<-drugSummary()%>%
      select("safetyScore")
    rownames(safetydata)<-drugSummary()$Drug
    safetydata<-as.matrix(safetydata)
    
    qualitydata<-drugSummary()%>%
      select("qualityScore")
    rownames(qualitydata)<-drugSummary()$Drug
    qualitydata<-as.matrix(qualitydata)
    
    
    studysizedata<-drugSummary()%>%
      select("nParticipants")
    rownames(studysizedata)<-drugSummary()$Drug
    studysizedata<-as.matrix(studysizedata)
    
    
    
    fig3<-plot_ly(z=drugScores, x= colnames(drugScores), y=rownames(drugScoredata),type="heatmap",colorscale="Viridis", reversescale=T,
                  height=800, width= 1000, 
                  colorbar=list(title='Clinical Product Score'))%>%
      layout(yaxis=list(autorange='reversed'), xaxis=(list(title="Clinical Product Score")))

    fig1a<-plot_ly(z=efficacydata, x=colnames(efficacydata), y=rownames(efficacydata), type='heatmap', colorscale="magma", colorbar=list(title='efficacy score')) 
    fig1b<-plot_ly(z=safetydata, x=colnames(safetydata), y=rownames(safetydata), type='heatmap', colorscale="magma", colorbar=list(title='safety score')) 
    fig1c<-plot_ly(z=studysizedata, x=colnames(studysizedata), y=rownames(studysizedata), type='heatmap', colorscale="magma", colorbar=list(title='nParticipants')) 
    fig1d<-plot_ly(z=qualitydata, x=colnames(qualitydata), y=rownames(qualitydata), type='heatmap', colorscale="magma", colorbar=list(title='quality score')) 
    
    hm <- subplot(fig3,fig1a, fig1b, fig1c, fig1d, shareY=T
                  , widths=c(1/7, 1/7, 1/7, 1/7, 1/7)
                  , heights = 1)
    
  })
  #progress-------------------------------------
  
  #####plotlysunburst-----------------------------
  output$sb2 <- renderPlotly({
    sunburstdata <- publicationList()
    sunburstdata["phase"][sunburstdata["phase"] == "not reported"] <- "Not specified"
    sunburstdata["phase"][sunburstdata["phase"] == ""]<- NA
    sunburstdata["phase"][sunburstdata["phase"] == "NA"]<- NA
    sunburstdata["studyType"][sunburstdata["studyType"] == "Interventional trial"]<-"interventional"
    sunburstdata["studyType"][sunburstdata["studyType"] == "Observational trial"]<-"observational"
    sunburstdata<-sunburstdata%>%
      select(Disease, studyType, phase, Drug, StudyIdStr)%>%
      unique() %>%
      group_by(Disease, studyType, phase, Drug, StudyIdStr) %>%
      count() %>%
      rename(value = n) %>%
      ungroup()
    
    DF0 <- sunburstdata %>% 
      group_by(Disease) %>% 
      unique() %>%
      summarise(value=sum(value),.groups="drop")
    
    DF1 <- sunburstdata %>% 
      group_by(Disease, studyType) %>% 
      summarise(value=sum(value),.groups="drop")
    
    DF2 <- sunburstdata %>% 
      group_by(Disease, studyType, phase) %>%
      summarise(value=sum(value),.groups="drop")
    
    DF3 <- sunburstdata %>% 
      group_by(Disease, studyType, phase, Drug) %>%
      summarise(value=sum(value),.groups="drop")
    
    df0 <- data.frame(
      ids = paste(DF0$Disease),
      labels = DF0$Disease,
      parents = "total",
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
    dfroot<-data.frame(
      ids = c("total"),
      labels = c("total"),
      parents=c(""),
      values=sum(df0$values),
      stringsAsFactors=F
    )
    
    df<-rbind(dfroot,df)
    
    p <- plot_ly(df,
                 ids = ~ids,
                 labels = ~labels,
                 parents = ~parents,
                 type = 'sunburst',
                 height=700,
                 width=700,
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
             margin = list(b = 80, l = 80, r = 80, t = 100, pad = 0, autoexpand = TRUE))
    
    event_register(p, 'plotly_click')
    
    p
  })
  
  ########clinicalprogress-------------------------------------
  nUniquePublications <- reactive({paste0("ReLiSyR Living Search:\\n", paste( clinicalStatistics()$nUniquePubs, "publications")) })
  nIncludedPublications <- reactive({ paste("ML-assisted citation screening using Title/Abstract:\\n", paste( clinicalStatistics()$nIncludedPubs, "publications included")) })
  nPublicationsMeetLogic <- reactive({ paste("Filtering by ReLiSyR drug/disease logic:<br></br><i>(≥1 clinical publication in AD OR<br></br>clinical publications in ≥2 other diseases of interest)<br></br></i>", clinicalStatistics()$nDrugMeetLogic, "drugs;", clinicalStatistics()$nPublicationsMeetLogic, "publications") })
  nCoreDrugPublications <- reactive({ paste("Longlisting of drugs by AD trial investigators:\\n", clinicalStatistics()$nCoreDrugs, "drugs;", clinicalStatistics()$nCoreDrugsPubs, "publications") })
  
  annotationProgress <- reactive({ paste0(
    "Annotation Progress\\nSingle Annotated: ", clinicalStatistics()$nSingleAnnotated, " publications (", clinicalStatistics()$percentSingleAnnotated, "%)\\nDual Annotated: ", clinicalStatistics()$nDualAnnotated, " publications (", clinicalStatistics()$percentDualAnnotated, "%)\\nReconciled: ", clinicalStatistics()$nReconciled, " publications (", clinicalStatistics()$percentReconciled, "%)") })
  
  clinicalInclusionSummaryStats <- reactive({
    clinicalInclusionSummaryStats <- clinicalFTInclusionSummary()%>%
      group_by(inclusionStatus, excluded_reason)%>%
      summarise(nStudies = length(unique(StudyIdStr)))
  })
  
  

  
  nFTExcluded <- reactive({ paste("Excluded on full text screening:", filter(clinicalInclusionSummaryStats(), inclusionStatus == "exclude")$nStudies%>%sum(), "publications") })
  
  nFTExcludedDetails <- reactive({ paste("\\nIncluded in error:", filter(clinicalInclusionSummaryStats(), inclusionStatus == "exclude" & excluded_reason =="Included in error")$nStudies, 
                                         "publications<br></br>Wrong drug:", filter(clinicalInclusionSummaryStats(), inclusionStatus == "exclude" & excluded_reason =="wrong drug")$nStudies,
                                         "publications<br></br>Does not describe disease of interest:", filter(clinicalInclusionSummaryStats(), inclusionStatus == "exclude" & excluded_reason =="does not describe disease of interest")$nStudies,
                                         "publications<br></br>Insufficient data:", filter(clinicalInclusionSummaryStats(), inclusionStatus == "exclude" & excluded_reason =="Insufficient data provided")$nStudies,
                                         "publications<br></br>Other reasons:", filter(clinicalInclusionSummaryStats(), inclusionStatus == "exclude" & excluded_reason =="other")$nStudies, "publications") })
  nFTIncluded <- reactive({ paste("Included in analysis:\\n", filter(clinicalInclusionSummaryStats(),inclusionStatus == "include")$nStudies, "publications") })
  
  
  
  
  prisma <- reactive({
    prisma<-glue("
                 digraph {{
                 
                 graph [overlap = true, fontsize = 10, splines=ortho, nodesep=1]
                 
                 node [shape = box,
                 fontname = Helvetica]
                 A [label = '@@1'];
                 B [label = '@@2'];
                 C [label = '@@3'];
                 D [label = <@@4>];
                 E [label = '@@5'];
                 F [label = '@@6'];
                 G [label = <@@7<br></br><FONT POINT-SIZE='10'><i>
@@8
</i></FONT>
>];
                 H [label = '@@9']
                 
                 node [shape = point, width = 0, height = 0]
                 a
                 
                 A->B
                 B->C
                 C->D
                 D->E
                 E->F
                 F->G
                 F->H
                 
                 subgraph {{
                 rank = same; F ; G;
                 }}
                 
                 }}
                 
                 [1]: '{nUniquePublications()}'
                 [2]: '{nIncludedPublications()}'
                 [3]: 'Drug and disease annotation using RegEx'
                 [4]: '{nPublicationsMeetLogic()}'
                 [5]: '{nCoreDrugPublications()}'
                 [6]: '{annotationProgress()}'
                 [7]: '{nFTExcluded()}'
                 [8]: '{nFTExcludedDetails()}' 
                 [9]: '{nFTIncluded()}'
                 
                 "
    )
  })

  
  output$prismaDiagram <- renderGrViz({
    grViz({
      prisma()
    })
  })
  
  
  ########
  output$ClinicalProgressDateTitle <- renderText(paste("last updated:", clinicalStatistics()$UpdateDate))
  
  output$ClinicalLivingSearch <- renderText(paste("From our automated living search of PubMed, we have identified a total number of", clinicalStatistics()$nUniquePubs , "unique publications as of", clinicalStatistics()$UpdateDate,"."))
  
  output$ClinicalCitationScreening <- renderText(paste("Using a combination of human and machine learning citation screening via the","<a href = 'https://app.syrf.org.uk/home' target='_target'>Systematic Review Facility (SyRF) platform </a>"        ,"we identified",clinicalStatistics()$nIncludedPubs,"publications meeting our inclusion criteria on title and abstract screening."))
  
  
  output$ClinicalFilteringDrugs <- renderText(paste( "Using a combination of human and machine learning drug and disease annotation via the","<a href = 'https://app.syrf.org.uk/home' target='_target'>Systematic Review Facility (SyRF) platform </a>"  ,"we have identified",clinicalStatistics()$nDrugMeetLogic,"drugs described in",clinicalStatistics()$nPublicationsMeetLogic,"publications where the drugs have been studied in at least one study in AD or two or more other diseases of interest (Motor neuron disease, Frontotemporal dementia, Huntington's disease, Multiple Sclerosis, Parkinson's disease) which may share similar pivotal pathways."))
  
  output$ClinicalLongList <- renderText(paste("These drugs have been reviewed by our clinical trial investigators, who have longlisted",clinicalStatistics()$nCoreDrugs,"drugs for prioritisation of data extraction taking into account feasibility for repurposing in clinical trials in near future, previous clinical trials and biological plausibility."))
  
  output$ClinicalDataExtraction <- renderText(paste("Our team of reviewers are extracting data from these publications. Currently,",clinicalStatistics()$nSingleAnnotated ,"(",      clinicalStatistics()$percentSingleAnnotated,"%) of publications for longlisted drugs have been single annotated,",clinicalStatistics()$nDualAnnotated,"(",clinicalStatistics()$percentDualAnnotated ,"%) have been dual annotated and",clinicalStatistics()$nReconciled,"(",clinicalStatistics()$percentReconciled,"%) have been fully reconciled."))
  
  output$ClinicalUniquePubsInfoBox <- renderInfoBox({
    infoBox(
      "Unique publications identified",
      as.numeric(clinicalStatistics()$nUniquePubs),
      icon = icon("file-text-o"),
      color = "red"
    )
  })
  output$ClinicalIncludedPubsInfoBox <- renderInfoBox({
    infoBox(
      "Included publications",
      as.numeric(clinicalStatistics()$nIncludedPubs),
      icon = icon("file-text-o"),
      color = "orange"
    )
  })
  output$ClinicalDrugMeetLogicInfoBox <- renderInfoBox({
    infoBox(
      "Drugs meeting inclusion logic",
      as.numeric(clinicalStatistics()$nDrugMeetLogic),
      icon = icon("tablets"),
      color = "aqua"
    )
  })
  output$ClinicalPublicationsMeetLogicInfoBox <- renderInfoBox({
    infoBox(
      "Publications for included drugs",
      as.numeric(clinicalStatistics()$nPublicationsMeetLogic),
      icon = icon("file-text-o"),
      color = "aqua"
    )
  })
  output$ClinicalCoreDrugsInfoBox <- renderInfoBox({
    infoBox(
      "Longlisted Drugs",
      as.numeric(clinicalStatistics()$nCoreDrugs),
      icon = icon("tablets"),
      color = "blue"
    )
  })
  output$ClinicalCorePubsInfoBox <- renderInfoBox({
    infoBox(
      "Publications for longlisted drugs",
      as.numeric(clinicalStatistics()$nCoreDrugsPubs),
      icon = icon("file-text-o"),
      color = "blue"
    )
  })
  output$ClinicalSingleAnnotInfoBox <- renderInfoBox({
    infoBox(
      "Single Annotated Publications",
      as.numeric(clinicalStatistics()$nSingleAnnotated),
      icon = icon("user"),
      color = "green"
    )
  })
  output$ClinicalDualAnnotInfoBox <- renderInfoBox({
    infoBox(
      "Dual Annotated Publications",
      as.numeric(clinicalStatistics()$nDualAnnotated),
      icon = icon("user-friends"),
      color = "green"
    )
  })
  output$ClinicalReconciledInfoBox <- renderInfoBox({
    infoBox(
      "Reconciled publications",
      as.numeric(clinicalStatistics()$nReconciled),
      icon = icon("check"),
      color = "olive"
    )
  })
  
  
  #drugCV-------------------------------------
  
  
  drugScores<-reactive({
    drugScores <- drugSummary() %>%
      select(x = Drug, 
             y = drugScore)
    return(drugScores)
  })
  
  
  selectedDrugScore <- reactive({
    selectedDrugScore <- drugScores()%>% 
      filter(x %in% input$drug)%>%
      select(y)%>%
      as.numeric()
    
    return(selectedDrugScore)
  })
  
  output$selecteddrugrankchart<- renderPlot({
    drugrankchart<-ggplot(drugScores(), aes(y=y, x=""))+
      labs(y="Drug Score", x="")+
      geom_violin(trim=F, fill="cadetblue2")+
      geom_dotplot(dotsize=0.5, binaxis='y', stackdir='center', binwidth=0.25)+
      annotate(geom="point", x="", y=selectedDrugScore(), shape=13, colour="red3", size=5,stroke=2)
    drugrankchart
  })
  
  drugSafety <- reactive({
    drugSafety<- drugSummary()%>%filter(Drug %in% input$drug)%>%select(safetyScore)%>%as.numeric()
    return(drugSafety)
  })
  
  drugEfficacy <- reactive({
    drugEfficacy<- drugSummary()%>%filter(Drug %in% input$drug)%>%select(efficacyScore)%>%as.numeric()
    return(drugEfficacy)
  })
  
  
  output$selecteddrugbubblechart<- renderPlot({
    drugbubblechart<- ggplot(
      drugSummary(),
      aes(
        x = safetyScore,
        y = efficacyScore,
        size= nParticipants,
        color= qualityScore
      ))+ 
      scale_color_gradient(low="yellow", high="red", limits=c(0,24))+ 
      geom_point(alpha=1)+
      # scale_size("studySizeScore", range=c(1,4))
      theme(legend.position = "right")+
      labs(x="Safety Score",
           y="Efficacy Score",
           color="Quality Score",
           size="nParticipants")+
      xlim(0,3)+
      ylim(-2,4)+
      annotate(geom = "point", x =  drugSafety(), y =  drugEfficacy(), shape=13, size=5,stroke=1, color="black")
    drugbubblechart
  })
  
  
  
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
  #        round(drugScore, digits = 2),
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
  #                    color = ~ drugScore,
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
  
  #  output$selectDrug <- shiny::renderUI({
  #   shiny::selectInput("drug","Select Drug", choices = sort(unique(publicationList()$Drug)), multiple = FALSE)
  # })
  
  selecteddrugranklist <- reactive({
    df1<-drugSummary() %>%
      filter(Drug %in% input$drug)%>%
      select(
        "Drug",
        "drugScore",
        "nPublication",
        "efficacyScore"  ,
        "safetyScore"  ,
        "nParticipants" ,
        "qualityScore"
      ) 
    cols <- c(    "drugScore",
                  "efficacyScore"  ,
                  "safetyScore"  ,
                  "nParticipants" ,
                  "qualityScore"    )
    
    df1[,cols] <- round(df1[,cols], 3)
    df1
  })
  
  ########drugtable-------------------------------------
  output$selecteddrugranklist <- 
    DT::renderDataTable(DT::datatable(
      selecteddrugranklist() 
      , rownames=F
      , colnames=c("Drug","Clinical Drug Score","Clinical n(Pub)","median efficacy (-2 - 4)","median safety score (0-3)","median number of participants","median quality score (out of 24)"), 
      caption= htmltools::tags$caption(style = 'caption-side:bottom; text-align:left;',   c("Current results summary of (i) clinical review: For each publication, we calculated a distance score based on Euclidean distance of efficacy and safety scores weighted by quality and study size. For each drug, we calculate a drug score using the number of publications describing the drug (n) and median publication distance score for all publications describing data for that drug:", withMathJax("$$\\text{drug score}\\ = log10{(n+1)} \\times {(\\text{median distance score})}$$"), "Separately, we calculate median subscores for efficacy, safety, quality and number of participants across all publications for each drug. See 'About' tab for more details on scoring; (ii) in vivo and (iii) in vitro review are reported here by the standard mean deviation (SMD) in survival and cell death studies respectively calculated from our meta-analysis. n(Pub) refers to number of publications annotated for each drug in our review."), escape=F) 
      , options = list(dom = 't')
      , extensions = 'Responsive'
    ))
  

  
  #####ClinicalScoreSummary-------------------------------------
  selectedclinscoresummary <- reactive({
    drugscoredata <- drugSummary() %>%
      filter(Drug %in% input$drug) %>%
      select(
        "drugScore",
        "nPublication",
        "efficacyScore",
        "safetyScore",
        "nParticipants",
        "qualityScore"
      )
    
    cols <- c(    "drugScore",
                  "nPublication",
                  "efficacyScore"  ,
                  "safetyScore"  ,
                  "nParticipants" ,
                  "qualityScore"    )
    
    drugscoredata[,cols] <- round(drugscoredata[,cols], 3)
    
    
    
    tdrugscoredata <- t(as.matrix(drugscoredata))
    return(tdrugscoredata)
  })
  
  output$selectedclinscoresummary <-
    DT::renderDataTable(DT::datatable(
      selectedclinscoresummary(),
      rownames=c("Drug Score",
                 "Number of Clinical Publications",
                 "Median Efficacy Score (-2-4)",
                 "Median Safety Score (0-3)",
                 "Median number of participants",
                 "Median Quality Score (out of 24)"),
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
    drugsbdata<-publicationList() %>%
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
      summarise(value=sum(value),.groups="drop")
    
    DF1 <- drugsbdata %>% 
      group_by(Disease, studyType) %>% 
      summarise(value=sum(value),.groups="drop")
    
    DF2 <- drugsbdata %>% 
      group_by(Disease, studyType, phase) %>%
      summarise(value=sum(value),.groups="drop")
    
    df0 <- data.frame(
      ids = paste(DF0$Disease),
      labels = DF0$Disease,
      parents = "total",
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
    
    dfroot<-data.frame(
      ids = c("total"),
      labels = c("total"),
      parents=c(""),
      values=sum(df0$values),
      stringsAsFactors=F
    )
    
    df<-rbind(dfroot,df)
    
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
                 height=300,
                 width=300,
                 # insidetextorientation='auto',
                 insidetextfont = list(size=12),
                 marker = list(colors = "Viridis", 
                               line = list(color = "white", width=2)),
                 source = "sunburstPlot")%>%
      layout(autosize = F, 
             paper_bgcolor="transparent",
             plot_bgcolor='transparent',
             margin = list(b = 0, l = 0, r = 0, t = 30, pad = 0, autoexpand = TRUE))
    
    event_register(p, 'plotly_click')
    
    p
  })
  
  #####drugptsunburst---------------------
  sdptsunburstdata <- reactive({
    drugptsbdata<-publicationList() %>%
      filter(Drug %in% input$drug)%>%
      select(Disease, studyType, phase, nPatients)%>%
      unique() %>%
      ungroup()
    
    DF0 <- drugptsbdata %>% 
      group_by(Disease) %>% 
      unique() %>%
      summarise(nPatients=sum(nPatients),.groups="drop")
    
    DF1 <- drugptsbdata %>% 
      group_by(Disease, studyType) %>% 
      summarise(nPatients=sum(nPatients),.groups="drop")
    
    DF2 <- drugptsbdata %>% 
      group_by(Disease, studyType, phase) %>%
      summarise(nPatients=sum(nPatients),.groups="drop")
    
    df0 <- data.frame(
      ids = paste(DF0$Disease),
      labels = DF0$Disease,
      parents = "total",
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
    dfroot<-data.frame(
      ids = c("total"),
      labels = c("total"),
      parents=c(""),
      values=sum(df0$values),
      stringsAsFactors=F
    )
    
    df<-rbind(dfroot,df)
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
                 height=300,
                 width=300,
                 #insidetextorientation='auto',
                 insidetextfont = list(size=12, orientation='auto'),
                 marker = list(colors = "Viridis", 
                               line = list(color = "white", width=2)),
                 source = "sunburstPlot")%>%
      layout(autosize = F, 
             paper_bgcolor="transparent",
             plot_bgcolor='transparent',
             margin = list(b = 0, l = 0, r = 0, t = 30, pad = 0, autoexpand = TRUE))
    
    event_register(p, 'plotly_click')
    
    p
  })
  
  #####clinicalpubs-------------------------------------
  selecteddrugclinicalpubtable <- reactive({
    drugclinicalpubtable <- publicationList() %>%
      filter(Drug %in% input$drug) %>%
      select(
        "Title",
        "Disease",
        "Drug",
        "Year",
        "Author",
        "DOI",
        "studyType",
        "phase",
        "nPatients",
        "efficacyScores", 
        "safteyScores",
        "qualityScore"
      )

    return(drugclinicalpubtable)
  })
  
  output$drugclinicalpublications <-
    DT::renderDataTable(DT::datatable(
      selecteddrugclinicalpubtable(), 
      rownames = FALSE,
      colnames=c("Title","Disease","Drug","Year","Author","DOI","Type of study","Study Phase","n(patients)", "efficacy", "safety", "quality")
      , filter="top", options = list(pageLength = 5, dom = 'Bflrtip', 
                                     buttons = list(list(extend = "copy"), 
                                                    list(extend = "csv", filename = paste0("AD-SOLES-CT_SelectedDrugClinicalPublications", Sys.Date())), 
                                                    list(extend = "excel", filename = paste0("AD-SOLES-CT_SelectedDrugClinicalPublications", Sys.Date())),  
                                                    list(extend = "pdf", filename = paste0("AD-SOLES-CT_SelectedDrugClinicalPublications", Sys.Date())), 
                                                    list(extend = "print")
                                     )),
      extensions = c('Buttons','Responsive')
    ))
  
  ###drug animal pubs -----
  
  selecteddruganimalpubtable <- reactive({
    druganimalpubtable <- invivoStudies() %>%
      filter(tolower(intervention) %in% input$drug) %>%
      select(
        title, 
        author, 
        journal,
        year, 
        doi
      )
  })
  output$druganimalpublications <-
    DT::renderDataTable(DT::datatable(
      selecteddruganimalpubtable(), 
      rownames = FALSE,
      colnames=c("Title","Author","Journal","Year","DOI")
      , filter="top", options = list(pageLength = 5, dom = 'Bflrtip', 
                                     buttons = list(list(extend = "copy"), 
                                                    list(extend = "csv", filename = paste0("AD-SOLES-CT_SelectedDrugAnimalPublications", Sys.Date())), 
                                                    list(extend = "excel", filename = paste0("AD-SOLES-CT_SelectedDrugAnimalPublications", Sys.Date())),  
                                                    list(extend = "pdf", filename = paste0("AD-SOLES-CT_SelectedDrugAnimalPublications", Sys.Date())), 
                                                    list(extend = "print")
                                     )),
      extensions = c('Buttons','Responsive')
    ))
  
  
  
  

  #####drugcvpdf----------------
  output$drugCV <- downloadHandler(
    filename= function() {
      file <- paste(input$drug, "_drugCV", Sys.Date(),".pdf", sep="")
    },
    content= function(file){
      params <- list(x=input$drug)
      
      rmarkdown::render("drugCV.Rmd", output_file=file, params=params, envir= new.env(parent=globalenv()))
    }
  )
  
   #####clinical trial registry--------------------------------
  # 
  
  clinicaltrials<-reactive({
    drug <- str_replace(input$drug, "\\s", "+")
    
    url1<- paste0("https://clinicaltrials.gov/api/v2/studies?query.cond=%28alzheimer+dementia%29+OR+%28alzheimer+disease%29+OR+%28vascular+dementia%29+OR+%28cerebral+small+vessel+disease%29+OR+%28neurodegenerative+disease%29+OR+%28stroke%29&query.intr=", 
                  drug, 
                  "&fields=NCTId%7CBriefTitle%7CCondition%7CInterventionName%7CInterventionDescription%7CStudyType%7CPhase%7COverallStatus",
                  "&countTotal=true&pageSize=1000") 
    
    
    xmldata<-GET(url1)
    data <- fromJSON(httr::content(xmldata, "text"))
    results <- data$studies%>%as.data.frame()
    
    results<-results%>%
      unnest(cols=c("protocolSection"))
    results<-results%>%
      unnest(cols=c(names(results)))
    
    results[,4] <- apply(results[,4], 2, function(y) sapply(y, function(x) paste(unlist(x), collapse=" | ")))
    
    results <- results%>%
      mutate(row = row_number())
    interventions <-results$interventions
    
    interventionDescription = data.frame(row = c(), description = c())
    for(i in 1:length(interventions)){
      dat <- interventions[[i]]
      if(any(names(dat)%in%'description')==TRUE){
        interventionDescription_i <- data.frame(row = i, description = paste(dat$description, collapse = "|"))
        interventionDescription = rbind(interventionDescription, interventionDescription_i)
      }
    }
    
    results <- left_join(results, interventionDescription, by = "row")%>%
      select(-row, -interventions)
    
    results <- results%>%
      rename(`intervention description` = description)
    
    return(results)
    
  })
  
  output$clinicaltrialsdata<-
    DT::renderDataTable(
      DT::datatable(
        clinicaltrials(),
        rownames = FALSE,
        filter="top", options = list(pageLength = 10,
                                     lengthMenu = list(c(10, 25, 50, 100,-1), c("10", "25", "50", "100", "All" )),
                                     dom = 'Bflrtip',
                                     buttons = list(list(extend = "copy"),
                                                    list(extend = "csv", filename = paste0("AD-SOLES-CT_SelectedDrugTrials", Sys.Date())),
                                                    list(extend = "excel", filename = paste0("AD-SOLES-CT_SelectedDrugMNDTrials", Sys.Date())),
                                                    list(extend = "pdf", filename = paste0("AD-SOLES-CT_SelectedDrugMNDTrials", Sys.Date())),
                                                    list(extend = "print")
                                     )),
        extensions = c('Buttons','Responsive')
      )
    )

  output$noclinicaltrialsdata<-renderUI({
    if(is.null(clinicaltrials())){
      p("No clinical trials listed for selected drug.")
    }

  })
  #   
  # 
  

  ######chembldata ----------------------
  
  
  getChemblData <- function(drug){
    url <- paste0("https://www.ebi.ac.uk/chembl/api/data/molecule?pref_name__icontains=", drug)
    data <- read_html(url)
    molecules <- data %>%html_nodes("molecules")%>%html_children()
    
    for(n in 1:length(molecules)){
      data<-molecules[n]
      name <- data %>%html_nodes("pref_name")%>%html_text()
      chemblIDs <- data %>%html_nodes("molecule_chembl_id")%>%html_text()%>%unique()
      synonyms <- data %>%html_nodes("molecule_synonym")%>%html_text()%>%unique()%>%paste(collapse="; ")
      SMILES <- data %>% html_node("canonical_smiles")%>%html_text()  
      mol_formula<-data%>%html_node("full_molformula")%>%html_text()
      mol_weight<-data%>%html_node("full_mwt")%>%html_text()
      mol_inchi<- data%>%html_node("standard_inchi")%>%html_text()
      mol_type<-data%>%html_node("molecule_type")%>%html_text()
      natural_product <- data %>%html_node("natural_product")%>%html_text()
      natural_product <- ifelse(natural_product =="", FALSE, TRUE)
      oral <- data %>%html_node("oral")%>%html_text()
      oral <- ifelse(oral =="", FALSE, TRUE)
      class <- data%>%html_node("usan_stem_definition")%>%html_text()
      ro5Violations <- data%>%html_node("num_ro5_violations")%>%html_text()
      ro5 <- ifelse(ro5Violations == "", TRUE, FALSE)
      ro3_pass <- data %>%html_node("ro3_pass")%>%html_text()
      ro3 <- ifelse(ro3_pass == "Y", TRUE, FALSE)
      max_phase <- data %>% html_node("max_phase")%>%html_text()
      availability_type <- data %>%html_node("availability_type")%>%html_text()
      availability_valid_type<-c(0,1,2)
      type<-pmatch(availability_type, availability_valid_type)
      availability <- switch(type, 
                             availability = "discontinued",
                             availability = "prescription only",
                             availability = "over the counter"
      )
      
      
      outputi<- paste0("<h4>", name, "</h3>",
                       "<p>ChemBLID: ", chemblIDs, 
                       "<br>Synonyms: ", synonyms,
                       "<br>SMILES: ", SMILES,
                       "<br>Molecule type: ", mol_type,
                       "<br>Molecule class: ", class,
                       "<br>Molecular formula: ", mol_formula,
                       "<br>Molecular weight: ", mol_weight,
                       "<br>Natural product: ", natural_product,
                       "<br>Oral formulation: ", oral, 
                       "<br>Rule of 5 met: ", ro5,
                       "<br>Rule of 3 met: ", ro3,
                       "<br>Max Clinical Trial Phase: ", max_phase,
                       "<br>Availability type: ", availability,
                       "</p>"
      )
      
      if(!exists("combinedOutput")) combinedOutput <- outputi else combinedOutput <- paste(combinedOutput, outputi, collapse = "<br>")
      
    }
    
    
    combinedChemblOutput <- paste0("<!DOCTYPE html><html><body style = 'background-color:white;'>",
                                   combinedOutput, "</body></html>")
    
    return(combinedChemblOutput)
  }
  
  
  chemblData <- reactive({
    
    drug <- ifelse(input$drug == "aripriprazole", "aripiprazole", 
                   ifelse(input$drug == "ciclosporin", "cyclosporin", 
                          ifelse(input$drug == "isoprinosine/inosiplex", "inosine%20pranobex",
                                 ifelse(input$drug == "salbutamol", "albuterol",  
                                        ifelse(input$drug == "ursodeoxycholic", 
                                               "ursodiol", input$drug)))))
    drug<- gsub(" ", "%20", drug)
    chemblData <- getChemblData(drug)
    return(chemblData)
  })
 
  
  output$chemblInfo <- renderText(chemblData())

})