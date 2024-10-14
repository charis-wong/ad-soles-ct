header <- dashboardHeader(title = "AD-SOLES-CT")

dashboardSidebar <- dashboardSidebar(sidebarMenu(
  menuItem(
    "Drug recommendation",
    tabName = "liveDR",
    icon = icon("clipboard-list")),
  menuItem(
    "Progress",
    tabName = "progress",
    icon = icon("chart-pie"),
    menuSubItem("Clinical review", tabName = "clinical")
    # menuSubItem("Animal In vivo review", tabName = "invivo")
    #,
    #menuSubItem("In vitro review", tabName = "invitro")
  ),
  menuItem("Drug CVs", 
           tabName = "drugCV", 
           icon = icon("tablets")),
  # menuItem("Contributors",
  #          tabName="contributors",
  #          icon = icon("users")),
  menuItem("About", tabName = "about", icon = icon("info"))
))

body <- dashboardBody(
  # h2("DISCLAIMER: THIS IS A DEMO VERSION"),
  tabItems(
    # liveDR---------
    tabItem(tabName = "liveDR",
            fluidRow(
              box(
                h1("AD-SOLES-CT"),
                h3("Alzheimer's Disease Systematic Online Living Evidence Summary for Clinical Trials"),
                p(strong("AD-SOLES-CT"), "aims to provide a living summary of evidence to guide prioritisation of drugs for evaluation in Alzheimer's disease (AD) clinical trials. Using the", strong("Systematic Living Evidence for Clinical Trials (SyLECT) framework"), "we generate, synthesise and report data for longlisted drugs from different domains of data. These includes:"), 
                
                tags$li(tags$a(href = "https://camarades.shinyapps.io/ReLiSyR-MND","Repurposing Living Systematic Review (ReLiSyR)"), "clinical review: a machine learning assisted living systematic review of Clinical literature of AD and other neurodegenerative diseases which may share common pivotal pathways, namely, motor neuron disease (MND), Frontotemporal dementia (FTD), Huntington's disease (HD), Multiple Sclerosis (MS) and Parkinson's disease (PD)."),
                tags$li(tags$a(href = "https://camarades.shinyapps.io/AD-SOLES", "AD-SOLES"), ", Systematic Online Living Evidence Summary of Experimental Alzheimer's Disease research, (", tags$a(href = "https://doi.org/10.1016/j.jneumeth.2024.110209", "doi.org/10.1016/j.jneumeth.2024.110209"), ")."),
                #tags$li("Experimental drug screening"),
                #tags$li("Pathway and network analysis"),
                tags$li("Mining of drug and trial databases, such as", tags$a(href = "https://www.ebi.ac.uk/chembl/", "ChEMBL database"), "and"
                        , tags$a(href = "https://www.clinicaltrials.gov/", "clinicaltrials.gov"), "."),
                p("More information on our methodology can be found under the About tab.")
                , width=12)),
            fluidRow(
              tabBox(
                side = "left", height = 1000, width = 12,
                selected = "Drug Table",
                tabPanel("Drug Table", DT::dataTableOutput("livedrugranklist")  %>% shinycssloaders::withSpinner(color="#0dc5c1")),
                tabPanel("Heatmap",  plotlyOutput("heatmap")  %>% withSpinner(color="#0dc5c1"))
              )
            )
    ),
    # clinical progress tab-------------------------------------
    tabItem(
      tabName = "clinical",
      h1("Clinical review"),
      fluidRow(
        tabBox(
          side = "left", height = 1000, width = 12,
          selected = "PRISMA Diagram",
          tabPanel("PRISMA Diagram", 
                   h4(textOutput("ClinicalProgressDateTitle")),
                   grVizOutput('prismaDiagram', width = "100%", height = "760px")%>% withSpinner(color="#0dc5c1"),
                   p(),
                   p(),
                   p("Using a living search of PubMed, we retrieve clinical publications in AD and other neurodegenerative diseases of interest which may share similar pathways (motor neuron disease, Parkinson's disease, Huntington's disease, frontotemporal dementia, multiple sclerosis). With a trained and validated machine learning (ML) algorithm, taking title and abstract as source material, citations are screened for inclusion. Included citations are annotated for drug and disease described using Regular Expressions. Each included paper is annotated by two independent reviewers, with differences reconciled by a third reviewer. See the 'About' tab for more information on our methodology.")
          ),
          
          tabPanel("Study characteristics",
                   p("This interactive sunburst chart displays all annotated papers according to disease studied, study design type, study phase, and drug. Hover over each chunk to see how many records fit each category. Select a category to expand and view subcategories in more details. Click the centre of chart to return to the previous level. "),
                   plotlyOutput("sb2", height="100%", width="100%")  %>% withSpinner(color="#0dc5c1"), width=NULL)
        )
      )
    ),
    
    # # invivo progress------------------------------------- 
    # tabItem(tabName = "invivo",
    #         h1("In vivo review"),
    #         h4(textOutput("InvivoProgressDateTitle" )),
    #         fluidRow(
    #           tabBox(
    #             side = "left", height = 1000, width = 12,
    #             selected = "PRISMA Diagram",
    #             tabPanel("PRISMA Diagram", 
    #                      fluidRow(
    #                        infoBoxOutput("InvivoUniquePubsBox"),
    #                        infoBoxOutput("InvivoIncludedPubsBox"),
    #                        infoBoxOutput("InvivoCorePubsBox")),
    #                      fluidRow(
    #                        infoBoxOutput("InvivoSingleAnnotBox"),
    #                        infoBoxOutput("InvivoDualAnnotBox"),
    #                        infoBoxOutput("InvivoReconciledBox")),
    #                      
    #                      fluidRow(
    #                        box(
    #                          title = "1. Living search",
    #                          solidHeader = TRUE,
    #                          status = "danger",
    #                          textOutput("InvivoLivingSearch"),
    #                          width = NULL
    #                        ),
    #                        
    #                        box(
    #                          title = "2. Citation screening",
    #                          solidHeader = TRUE,
    #                          status = "warning", htmlOutput("InvivoCitationScreening"),
    #                          width = NULL
    #                        ),
    #                        box(
    #                          title = "3. Filtering drugs of interest based on inclusion logic",
    #                          solidHeader = TRUE,
    #                          status = "info", htmlOutput("InvivoFilteringDrugs"),
    #                          width = NULL
    #                        ),
    #                        
    #                        box(
    #                          title = "4. Longlisting by clinical trial investigators",
    #                          solidHeader = TRUE,
    #                          status = "primary", textOutput("InvivoLongList"),
    #                          width = NULL
    #                        ),
    #                        
    #                        box(
    #                          title = "5. Data extraction",
    #                          solidHeader = TRUE,
    #                          status = "success", textOutput("InvivoDataExtraction"),
    #                          width = NULL
    #                        ))
    #             ),
    #             
    #             tabPanel("Study characteristics",
    #                      fluidRow(
    #                        column(width =6,
    #                               box(title = "Studies by drug tested",
    #                                   DT::dataTableOutput("invivoDrugSummaryTable")  %>% withSpinner(color="#0dc5c1"), width = NULL)),
    #                        column(width = 6,
    #                               box(title = "Studies by animal models used",
    #                                   p("This interactive sunburst chart displays all reconciled papers according to disease model used categorised by animal, mode of induction (genetic/pharmacological), protein or gene targeted (if specified), and SOD1 copy number (for SOD1 G93A mice). Hover over each chunk to see how many records fit each category. Select a category to expand and view subcategories in more details. Click the centre of chart to return to the previous level."),
    #                                   plotlyOutput("invivoModelSB", height="100%", width="100%")  %>% withSpinner(color="#0dc5c1"), width = NULL)
    #                        ))),
    #             
    #             tabPanel("Risk of Bias",
    #                      fluidRow(
    #                        tabBox(side = "left", 
    #                               width = 12,
    #                               selected = "Overview", 
    #                               tabPanel("Overview", h3("Risk of bias overview for reconciled experiments"),
    #                                        textOutput("invivoExperimentCountText"),
    #                                        plotlyOutput("invivoRobPlot", width = "100%")%>%
    #                                          withSpinner(color = "#0dc5c1")),
    #                               tabPanel("Table", 
    #                                        tags$head(tags$style(HTML('#faith table {border-collapse:collapse; } 
    #                          #faith table th {
    #                            height: 100px;
    #                            transform: 
    #                              translate(10px, -5px)
    #                              rotate(315deg);
    #                          }'))),
    #                                        tags$style(".yes {color:#2dc937}
    #                                        .no {color:#cc3232}
    #                                        .nc {color:#ffbf00}"),
    #                                        
    #                                        DT::dataTableOutput("invivoRobExperimentTable")%>%
    #                                          withSpinner(color = "#0dc5c1"))
    #                               
    #                               
    #                        )))
    #             
    #             
    #             
    #           )))
    # ,
    
    
    # # invitro progress-------------------------------------
    # tabItem(tabName = "invitro",
    #         h1("In vitro review"),
    #         h4(textOutput("InvitroProgressDateTitle" )),
    #         
    #         fluidPage(
    #           infoBoxOutput("InvitroUniquePubsBox"),
    #           infoBoxOutput("InvitroIncludedPubsBox"),
    #           infoBoxOutput("InvitroCorePubsBox"),
    #           infoBoxOutput("InvitroSingleAnnotBox"),
    #           infoBoxOutput("InvitroDualAnnotBox"),
    #           infoBoxOutput("InvitroReconciledBox"),
    #           box(
    #             title = "1. Living search",
    #             solidHeader = TRUE,
    #             status = "danger", textOutput("InvitroLivingSearch")
    #           ),
    #           
    #           box(
    #             title = "2. Citation screening",
    #             solidHeader = TRUE,
    #             status = "warning",htmlOutput("InvitroCitationScreening")
    #           ),
    #           
    #           box(
    #             title = "3. Filtering drugs of interest based on inclusion logic",
    #             solidHeader = TRUE,
    #             status = "info",htmlOutput("InvitroFilteringDrugs")
    #           ),
    #           
    #           box(
    #             title = "4. Longlisting by clinical trial investigators",
    #             solidHeader = TRUE,
    #             status = "primary",textOutput("InvitroLongList")
    #           ),
    #           
    #           box(
    #             title = "5. Data extraction",
    #             solidHeader = TRUE,
    #             status = "success",textOutput("InvitroDataExtraction")
    #           )
    #         )
    # ),
    
    # drugCVtab-------------------------------------
    tabItem(
      tabName="drugCV",
      h1("Drug CV"), 
      fluidPage(
        selectInput("drug", "Select Drug", drugList, multiple=FALSE),
        # downloadButton("drugCV", label="Download Drug CV pdf"),
        
        tabsetPanel(type="tabs",
                    tabPanel("Overview",
                             box(title="Clinical scores overview",width=12, height=500,
                                 p("Violin plot of drug scores, selected drug highlighted in red; bubble plot of clinical subscores with selected drug highlighted in black."),
                                 fluidRow(
                                   column(width = 6,
                                          plotOutput("selecteddrugrankchart") ),
                                   column(width = 6,
                                          plotOutput("selecteddrugbubblechart")))
                             ),
                             fluidRow(
                               box(
                                 width = 12,
                                 DT::dataTableOutput("selecteddrugranklist")
                               )
                             )
                             
                             
                    ),
                    # 
                    # tabPanel("Overview Plot",
                    #          fluidRow(
                    #            box(plotlyOutput("selecteddrugrankchart"),
                    #                "This bubble chart plots drugs in our review according to Clinical Product Score (colour scale), number of clinical publications represented by size of bubble, standard mean difference (SMD) of survival in in vivo studies on the x-axis, and SMD of cell death in in vitro studies on the y-axis.", width=12, status = "primary") %>% withSpinner(color="#0dc5c1")),
                    #          fluidRow(
                    #            box(
                    #              width = 12, status = "info",
                    #              DT::dataTableOutput("selecteddrugranklist")  %>% withSpinner(color="#0dc5c1")
                    #            )
                    #          )),
                    tabPanel("Clinical Summary",
                             fluidRow(
                               column(width=4,
                                      box(width= NULL, height = 400, status = "warning", 
                                          h4( "Score Summary"),
                                          DT::dataTableOutput("selectedclinscoresummary")  %>% withSpinner(color="#0dc5c1"))),
                               
                               column(width=4, 
                                      box(width= NULL, height=400,status = "danger", 
                                          title= "Study details", plotlyOutput("sb3") %>% withSpinner(color="#0dc5c1"))),
                               
                               column(width=4,
                                      box(width=NULL, height=400, 
                                          title = "Number of participants",status = "success", 
                                          plotlyOutput("ptsb") %>% withSpinner(color="#0dc5c1")))
                             ),
                             
                             fluidRow(box(width=12,
                                          h4("Publications for selected drug")
                                          ,status = "info", 
                                          DT::dataTableOutput("drugclinicalpublications") %>% withSpinner(color="#0dc5c1")
                             )
                             )),
                    tabPanel("in vivo Summary",
                             fluidRow(box(width=12,
                                          h4("Animal in vivo Publications for selected drug")
                                          ,status = "info", 
                                          p("source:", tags$a(href = "https://camarades.shinyapps.io/AD-SOLES", "AD-SOLES")),
                                          DT::dataTableOutput("druganimalpublications") %>% withSpinner(color="#0dc5c1")
                             )
                             )),
                             
                             
                             
                    
                    # tabPanel("in vitro Summary",
                    #          fluidRow(
                    #            box(width=12, height = 500, h4("Forest Plot"),status = "success"),
                    #            box(width=12,status = "info", 
                    #                h4("Publications for selected drug"),
                    #                DT::dataTableOutput("druginvitropublications") %>% withSpinner(color="#0dc5c1")
                    #            )
                    #          )),
                    # tabPanel("Pathway Analysis",
                    #          
                    #          fluidRow(
                    #            box(width = 12, height = 700,
                    #                h4("Network Analysis"), status = "success",
                    #                selectInput("networkImage","Network analysis", choices = c("Tc", "druggability", "brainprotein", "ADevidence")),
                    #                uiOutput("noPathwayAnalysis"),
                    #                imageOutput("networkImage")
                    #            )),
                    #          
                    #          
                    #          
                    #          
                    #          fluidRow(
                    #            box(width =12, status = "info",
                    #                h4("String Enrichment Analysis"),
                    #                uiOutput("noStringAnalysis"),
                    #                DT::dataTableOutput("stringAnalysis") 
                    #                %>% withSpinner(color="#0dc5c1")
                    #                
                    #            )
                    #          )
                    #          
                    # ),
                    
                    tabPanel("ClinicalTrials.gov",
                             fluidRow(
                               box(width=12, status = "info",
                                   h4("Clinical Trials in Alzheimer's disease, cerebral small vessel disease, neurodegenerative diseases and stroke listed on clinicaltrials.gov for selected drug"),
                                   uiOutput("noclinicaltrialsdata"),
                                   DT::dataTableOutput("clinicaltrialsdata")%>%withSpinner(color = "#0dc5c1")
                               )
                             )
                    ),
                    
                    tabPanel("ChEMBL Database",
                             fluidRow(
                               h3("Molecules listed matching drug name:"),
                               htmlOutput("chemblInfo"),
                               p(tags$a(href="https://www.ebi.ac.uk/chembl/", "Go to ChEMBL"))
                               
                             ))

                    
                    
                    
                    
                    
        ))
    ),
    #---- quickdownload tab ----
    # tabItem(
    #   tabName="download",
    #   fluidRow(
    #     box(title="Download all fully categorised references", status="success", width=4,
    #         p("Download data from our dataset of fully categorised papers only (annotated by at least 2 reviewers)"),
    #         downloadBttn("catclinicalpubs", "Download categorised clinical references", color = "default"),
    #         downloadBttn("catinvivopubs", "Download categorised in vivo references", color = "default"),
    #         downloadBttn("catinvitropubs", "Download categorised in vitro references", color = "default")),
    #     
    #     box(title= "Download all unique included references", status="primary", width=4,
    #         downloadBttn("allclinicalpubs",
    #                      label= "Download all clinical references", color = "success"),
    #         downloadBttn("allinvivopubs",
    #                      label="Download all in vivo references", color = "success"),
    #         downloadBttn("allinvitropubs", "Download all in vitro references", color = "success"))
    #   )
    # ),
    
    # ---- contributor tab------------------------------
    # tabItem(
    #   tabName="contributors",
    #   fluidPage(
    #     fluidRow(
    #       box(width=12,status = "primary",
    #           h3("Reviewers"),
    #           p("The ReLiSyR project includes a crowd-sourcing element to the systematic reviews. We have an expanding group of reviewers who have contributed in various stages of this project including screening and annotation. In the current stage of the project, we are extracting data for our clinical and animal in vivo reviews."))),
    #     h3("Data extraction leaderboards"),
    #     fluidRow(
    #       column(width=12,height=1010,
    #              box(width=4,status = "danger", height=800,
    #                  title="Reward Scheme Leaderboard",
    #                  p("Between 02 June 2021 and 30 July 2021, we are running a reward scheme to thank our reviewers. We will be offering reviewers rewards as follows based on the number of papers reviewed during this period."),
    #                  p(strong("10 papers: £20 voucher")
    #                  ),
    #                  p(strong("50 papers: £100 voucher")),
    #                  p(strong("100 papers: £250 voucher")),
    #                  DT::dataTableOutput('rewardleaderboard') %>% withSpinner(color="#0dc5c1")),
    #              box(width=4,status = "warning", height=800,
    #                  title="Monthly leaderboard",
    #                  p("Our top reviewers for the month of", month(Sys.time(), label=TRUE, abbr = FALSE), " ", year(Sys.time())),
    #                  DT::dataTableOutput('monthleaderboard') %>% withSpinner(color="#0dc5c1")),
    #              # )
    #              box(width=4, status = "success", height=800,
    #                  title="All-time Leaderboard",
    #                  DT::dataTableOutput('alltimeleaderboard') %>% withSpinner(color="#0dc5c1"))
    #       ))
    #     ,
    #     fluidRow(
    #       box(width=12,status = "primary",
    #           p("All registered reviewers past and present are listed here."),
    #           p(strong("Clinical Reviewers:"), textOutput("clinicalReviewers")),
    #           p(strong("Preclinical Reviewers:"), textOutput("preclinicalReviewers"))
    #       )))
    # ),
    
    # ---- about tab-------------------------------------
    tabItem(
      tabName = "about",
      fluidRow(
        column(width=7,
               box(
                 title="What is AD-SOLES-CT?", width=NULL,  status="info",
                 p(strong("AD-SOLES-CT"), "is a", 
                   tags$a(href="https://www.ed.ac.uk/clinical-brain-sciences/research/camarades/", "CAMARADES"),
                   "SOLES (Systematic Online Living Evidence Summary) project aiming to provide a living summary of evidence to guide prioritisation of drugs for evaluation in AD clinical trials, specificially",
                   tags$a(href="https://www.mrcctu.ucl.ac.uk/our-research/neurodegenerative-diseases/pre-mastodon-ad/", "PRE-MASTODON-AD"),
                   "Using the", strong("Systematic Living Evidence for Clinical Trials (SyLECT) framework"), "we generate, synthesise and report data for longlisted drugs from different domains of data. These includes:"), 
                 tags$ol(
                   tags$li("Published literature, via:"),
                   tags$ul(
                     tags$li("Repurposing Living Systematic Review (ReLiSyR) clinical review: a machine learning assisted living systematic review of: Clinical literature of MND and other neurodegenerative diseases which may share common pivotal pathways, namely, motor neuron disease (MND), Frontotemporal dementia (FTD), Huntington's disease (HD), Multiple Sclerosis (MS) and Parkinson's disease (PD)."),
                     tags$li("AD-SOLES (AD-Systematic Online Living Evidence Summary) for animal in vivo literature of AD models.")
                     
                   )
                 ,
                 # tags$li("Experimental drug screening"),
                 # tags$li("Pathway and network analysis"),
                 tags$li("Mining of drug and trial databases, such as", tags$a(href = "https://www.ebi.ac.uk/chembl/", "ChEMBL database"), "and"
                         , tags$a(href = "https://www.clinicaltrials.gov/", "clinicaltrials.gov"), ".")
               ),
               img(src = "drug selection framework lay slide.png", height = 350, align = "center"),
               p("SyLECT: Systematic Living Evidence for Clinical Trials framework")),
               
               box(title="ReLiSyR Methodology", width=NULL,status="info",
                   p("Our methodology adapted from work to inform drug selection for MND clinical trials, namely MND-SMART, is detailed in our",
                     tags$a(href="https://mfr.de-1.osf.io/render?url=https://osf.io/5jp43/?direct%26mode=render%26action=download%26mode=render", "protocol."),
                     "We adopted a systematic approach of evaluating drug candidates which we have previously used to guide drug selection for the Multple Sclerosis Secondary Progressive Multi-Arm Randomisation Trial (MS-SMART) a  multi-arm phase IIb randomised controlled trial comparing the efficacy of three neuroprotective drugs in secondary progressive multiple sclerosis. These principles of drug selection were published by",
                     tags$a(href="https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0117705","Vesterinen et al."),
                     "in 2015."),
                   p("This approach, which adopts a structured, systematic method combined with independent expert(s) evaluation, was designed to identify candidate drugs for evaluation in clinical trials for people with neurodegenerative diseases, including MND, on account of the compelling evidence for shared dysregulated pathways and processes across neurodegenerative disorders.
                Critically, the structured evaluation takes into account not only biological plausibility and efficacy but also safety and quality of previous studies. This includes adopting benchmark practice such as Delphi and PICOS framework."),
                   p("1.", strong("Living Search"),
                     ": We use the",
                     tags$a(href="https://syrf.org.uk","Systematic Review Facility (SyRF) platform"),
                     ", taking as its starting point automatic updating of the PubMed search."),
                   p("2.", strong("Citation Screening"),
                     ": Using a machine learning algorithm which has been trained and validated using human decisions, publications are screened for inclusion based on title and abstract."),
                   p("3.", strong("Filtering drugs by inclusion logic"),
                     ": Text mining approaches (Regular Expressions deployed in R and taking as source material title and abstract) are used to identify disease and drug studied. A second algorithm is used to identify drugs which have been tested in at least one clinical study in MND; or have been tested clinically in two of the other specified conditions."),
                   p("4.", strong("Longlisting by trial investigators"),
                     ": Trial investigators reviewed the drugs filtered, excluding drugs which met the following critera: (i) previously considered unsuitable by expert panel due to lack of biological plausibility, drugs with unfavourable safety profiles in MND patients and drugs tested more than 3 times in MND population; (ii) drugs available over-the-counter as these may affect trial integrity; (iii) compounds which are not feasible for the next arms due to supply issues, such as compopunds not listed in the current version of the British National Formulary; (iv) drugs without oral preparations; and (v) drugs that are deemed by investigators to be unsafe/inappropriate for clinical trial in the current setting."),
                   p("5.", strong("Data extraction"),
                     ": Our team of reviewers extract data specified in our protocol on the",
                     tags$a(href="https://syrf.org.uk", "SyRF platform"),
                     "from all included publications for longlisted drugs. Each publication will be annotated by at least two reviewers, with any differences reconciled by a third reviewer."),
                   p("6.", strong("Data Analysis"),
                     ": We will analyse the results as follows:",
                     tags$ul(
                       tags$li("Clinical review:","For each publication, we calculated a",
                               # includeHTML("Drug-Scoring-Method.html"),
                               tags$a(href = "https://mfr.de-1.osf.io/render?url=https://osf.io/8k4h2/?direct%26mode=render%26action=download%26mode=render", "distance score"),
                               "based on Euclidean distance of efficacy and safety scores weighted by quality and study size. For each drug, we calculate a drug score using the number of publications describing the drug (n) and median publication distance score for all publications describing data for that drug:", withMathJax("$$\\text{drug score}\\ = log10{(n+1)} \\times {(\\text{median distance score})}$$"),
                               
                               "Separately, we will calculate median subscores for efficacy, safety, quality and study size across all publications for each drug."),
                       tags$li("Animal invivo review and in vitro review: An individual meta‐analysis will be carried out for each intervention identified. We will summarise the effects of interventions where there are 3 or more publications in which that intervention has been tested reporting findings from at least 5 experiments. Depending on the nature of the outcomes reported we will use either standardised mean difference (SMD) or normalised mean difference (NMD) random effects meta-analysis with REML estimates of tau. Specifically, if fewer than 70% of outcomes are suitable for NMD analysis we will use SMD. Differences between groups of studies will be identified using meta-regression."
                       ))
                   ))
               
               
        ),
        
        
        
        
        
       
        column(width=4,
               box(title = "CAMARADES", width=NULL, status="danger", 
                   p("The", tags$a(href="https://www.ed.ac.uk/clinical-brain-sciences/research/camarades/", "CAMARADES"), "(Collaborative Approach to Meta-Analysis and Review of 
                     Animal Data from Experimental Studies) group specialise in performing", strong("systematic review and meta-analysis"), "of data
                     from experimental studies. Our interests range from identifying potential sources of bias in in vivo and in vitro studies; 
                     developing automation tools for evidence synthesis; developing recommendations for improvements in the design and
                     reporting; through to developing meta-analysis methodology to better apply to in basic research studies."),
                   p("Follow us on twitter", tags$a(href="https://twitter.com/camarades_?", "@CAMARADES_"))),
               box(title="CAMARADES Evidence Summary Projects", width=NULL, status="danger",
                   p("CAMARADES have produced other projects providing curated online evidence summaries in other disease areas including the", 
                     tags$a(href="https://camarades.shinyapps.io/AD-SOLES", "Animal Models of Alzheimer's Disease"), ",", 
                     tags$a(href="https://camarades.shinyapps.io/COVID-19-SOLES", "COVID-19 Systematic Online Living Evidence Summary (SOLES) project"), ",",
                     
                     "and",
                     tags$a(href="https://khair.shinyapps.io/CIPN", "Chemotherapy induced peripheral neuropathy"), ".")),
               
               box(title = "AD-SMART", width=NULL, status="primary",
                   p("insert AD-SMART info here")
               )
        )
      ))
  ))

shinyUI(dashboardPage(skin = "blue",
                      header,
                      dashboardSidebar,
                      body))

