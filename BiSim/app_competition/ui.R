options(digits=4)

## Set up a Shiny app to use shinyjs
## This function must be called from a Shiny app's UI in order 
## for all other shinyjs functions to work
library(shinydashboard)
library(DT)
library(shinyjs)
library(shinyWidgets)
library(plotly)
library(xlsx)
library(DT) 
library(webshot) 



#useShinyjs()


fluidPage(
  style = " background-color: #FFFFFF",
useShinyjs(),
## A page with a top level navigation bar
navbarPage(
  
  ## Create a logo
  div(img(src = "Binding Simulator logo together.png", height = "70%", width = "70%")
      ),
     
  ## Landing tab ----
  tabPanel(
    
   
    "Intro", ## tab title
    
    ## Create a dashboard page inside of the tab
    dashboardPage(
      
      
      ## Disable header
      dashboardHeader(
        disable = TRUE
      ), 
      
      ## Create a sidebar
      dashboardSidebar(
        disable = TRUE
      ), ## end upstream sidebar
      
      ## Main body
      dashboardBody(
        style = " background-color: #FFFFFF",
        includeCSS("www/style.css"), ## add custom style
      
        tags$style(HTML(" 
        
          .navbar-nav li a:hover, .navbar-nav > .active > a {
          color: #fff !important;
          
          background-color:#1e90ff  !important;
          background-image: none !important;
          }
                  
        ")),
        
        ## Create the page layout to make sure that the page is properly
        ## sized with regard to the length of content
        fluidRow(
          # style = " background-color: #FFFFFF",
          ## Create plot box
          box(
            # style = " background-color: #FFFFFF",
            #style = " background-color: #293887",
            width = 12,
            column(
              width = 12,
              align = "center",
              style = " background-color: #293887",
              div(
                br(),
                img(src = "Binding Simulator logo together.png", width = "200px"),
                style = " background-color: #293887"
              ),
              div(HTML("WELCOME"), style = "margin-top: 20px; font-size: 14px; margin-bottom: 20px; color: #FFFFFF; color: #718096; font-style: Lato;"),
              div(HTML("This simulation tool should help you simulate reversible, non-covalent <br> interactions between two or three partners."), 
                  style = "margin-top: 20px; font-size: 14px; margin-bottom: 0px; color: #FFFFFF; text=align: center; color: #718096; font-style: Lato;"),
              br(),
              div(HTML("DISCLAIMERS"), style = "margin-top: 20px; font-size: 14px; margin-bottom: 20px; color: #FFFFFF; color: #718096; font-style: Lato;"),
              div(HTML("Interactions are assumed to be 1:1; multivalent interactions are not considered."),
                  style = "margin-top: 20px; font-size: 14px; margin-bottom: 0px; color: #FFFFFF; text=align: center; color: #718096; font-style: Lato;"),
              div(HTML("Interactions are assumed to occur in solution."),
                  style = "margin-top: 20px; font-size: 14px; margin-bottom: 0px; color: #FFFFFF; text=align: center; color: #718096; font-style: Lato;"),
              div(HTML("Interaction are assumed to be in equilibrium."),
                  style = "margin-top: 20px; font-size: 14px; margin-bottom: 0px; color: #FFFFFF; text=align: center; color: #718096; font-style: Lato;"),
              br()
            ),
            
            column(
              width = 6,
              align = "center",
              #div(
                div(HTML("Binding"), style = "margin-top: 20px; font-size: 18px; margin-bottom: 20px; color: #2D3748; font-style: Lato;"),
                div(HTML("Interactions between two partners"), style = "margin-top: 20px; font-size: 14px; margin-bottom: 20px; color: #2D3748; font-style: Lato;"),
                br(),
                div(HTML("Simple binding refers to <b> reversible non-covalent interactions between two different interaction partners </b>. 
                         The binding kinetics are based on the association of two interaction partners that have a certain affinity 
                         for each other. The rate at which the partners associate is proportional to their concentration. 
                         Once binding has occurred, both partners remain bound together for a random amount of time influenced by the affinity, 
                         after which dissociation can occur. Equilibrium is reached when the rate at which new complexes are formed, 
                         equals the rate at which the complexes dissociate." 
                         ), 
                    style = "margin-top: 20px; font-size: 14px; margin-bottom: 20px; color: #718096; font-style: Lato;"),
              br(),
              br(),
              br(),
              img(src = "Binding explanation.png", width = "200px"),
              br(),
              br(),
              br(),
              br(),
              div(HTML("Understanding the binding kinetics by performing simulations using this tool is important to 
                       understand the interactions that occur between the two binding partners of interest. 
                       Most common applications are:" 
              ), 
              HTML("<ul><li> <i>In vivo </i> interactions between for example drug and target or ADA. It can be relevant to 
                   understand how much drug is occupied by either target or ADA, or the other way round, how much 
                   target is occupied by drug (at baseline and after accumulation). In case of PK assay set-up optimization,
                   this can be helpful to evaluate whether the free drug fraction approximates the total fraction, 
                   for example when the target levels are negligible in comparison with the drug levels. However,
                   if a substantial fraction of the drug is bound to target (free â  total), it is important to understand 
                   how target might affect the PK assay set-up. Additionally, when diluting your sample 
                   (e.g. when applying your MRD), the drug-target binding will likely be affected, which can also 
                   be simulated using this tool. </li> </ul>"),
              HTML("<ul><li> <i> In vitro </i> interactions between analyte of interest and assay tools. It can be for example 
                   interesting to simulate binding curves at different coating tool concentrations using different tools with 
                   varying affinity for the analyte.  </li> </ul>"),
              style = "margin-top: 20px; font-size: 14px; margin-bottom: 20px; color: #718096; font-style: Lato;")
            ),
            
            column(
              width = 6,
              align = "center",
              #div(
              div(HTML("Competition"), style = "margin-top: 20px; font-size: 18px; margin-bottom: 20px; color: #2D3748; font-style: Lato;"),
              div(HTML("Interactions between three partners"), style = "margin-top: 20px; font-size: 14px; margin-bottom: 20px; color: #2D3748; font-style: Lato;"),
              br(),
              div(HTML("Competition refers to <b> reversible non-covalent interactions between three different interaction
                       partners </b>. Similar as for the simple binding between two interaction partners, competition involves
                       two independent binding interactions between two partners, all of which are depending on
                       concentrations and affinity, and these share 1 common interaction partners, partner 1 as depicted
                       in the figure below."),
                  style = "margin-top: 20px; font-size: 14px; margin-bottom: 20px; color: #718096; font-style: Lato;"),
              br(),
              br(),
              br(),
              img(src = "Competition explanation.png", width = "200px"),
              br(),
              br(),
              br(),
              br(),
              div(HTML("A common application of this part of the simulation tool is to understand the impact 
                       of <i> in vivo </i> binding of an interaction partner (e.g. target being partner 3) with the 
                       analyte of interest (e.g. drug being partner 1) on the <i> in vitro </i> binding of the analyte 
                       with an assay tool (e.g. coating tool being partner 2). As the interaction partner 3 will 
                       be competing with partner 2 for binding to partner 1, it is interesting to understand how 
                       this could impact your assay binding curves."),
                  style = "margin-top: 20px; font-size: 14px; margin-bottom: 20px; color: #718096; font-style: Lato;")
            )
          ) ## end plot box
       ) ## end fluid row
      ) ## end dashboard body
    ) ## end landing dashboard page
  ), ## end landing tab
  
  # ## Binding tab ----
  # tabPanel(
  #   # "Binding", ## tab title
  #   title = div(img(src = "bindingLogo.png", height = "15%", width = "15%"), "Binding"
  #   ),
  #   ## Create a dashboard page inside of the tab
  #   dashboardPage(
  # 
  #     ## Disable header
  #     dashboardHeader(
  #       disable = TRUE
  #     ),
  # 
  #     ## Create a sidebar
  #     dashboardSidebar(
  #       disable = TRUE,
  #       width = "0px"
  #     ), ## end upstream sidebar
  # 
  #     ## Main body binding
  #     dashboardBody(
  #       style = " background-color: #f0f0f0",
  #       includeCSS("www/style.css"), ## add custom style
  # 
  #       tags$head(
  #         tags$style(HTML("
  #         .shiny-output-error-validation {
  #           color: #1e90ff;
  #           font-weight: bold;
  #           font-size: 16px;
  #           }
  #         "))
  #       ),
  # 
  #       tags$style(type='text/css',
  #                  ".selectize-input { font-size: 14px; line-height: 16px;} .selectize-dropdown { font-size: 14px; line-height: 16px; }"),
  # 
  # 
  #       div(style = "display:inline-block;vertical-align:top; margin-bottom:0px;font-size:16px;margin-top:-10px",
  #           div(
  #             style = "margin-top:0px; margin-bottom:20x; font-size: 16px; width:300px",
  #             textInput("receptor_name", label = div (style = "font-size:16px; ", "Name interaction partner 1"),
  #                       value = "Partner 1 name")
  #           ),
  # 
  #           div(
  #             style = "width:200px; margin-top:0px;font-size:16px;margin-bottom:20px;",
  #             radioButtons("p1concunit", label = div (style = "font-size:14px; ","Concentration unit for partner 1"),
  #                          choices = c("metric units", "molar units"),
  #                          selected = "metric units")
  #           ),
  # 
  #           # div(
  #           #   style = "margin-top:0px;",
  #           #   selectInput("p1unit",  label = div (style = "font-size:14px;", "Select unit for partner 1"),
  #           #               choices = list("g" = 1, "ng" = 2),
  #           #               selected = 1)
  #           # ),
  # 
  #           div(
  #             style = "margin-top:0px;",
  #             selectInput("p1unit_metric", HTML('<FONT size="2pt">Select unit for partner 1'),
  #                         choices = list( "mg/ml", "ug/ml", "ng/ml","pg/ml"),
  #                         selected = "pg/ml")
  #           ),
  # 
  #           div(
  #             style = "margin-top:0px;",
  #             selectInput("p1unit_molar", HTML('<FONT size="2pt">Select unit for partner 1'),
  #                         choices = list("fM" , "pM" , "nM" , "uM" , "mM" ),
  #                         selected = "pM")
  #           ),
  # 
  # 
  #           div(
  #             style = "margin-top:0px;",
  #             numericInput("c_total_receptor_start", label = div (style = "font-size:14px;","Concentration of partner 1"),
  #                          value = 100)
  #           ),
  #           div(
  #             style = "margin-top:-20px;width:127px; display:inline-block",
  #             shinyjs::hidden(
  #               numericInput("c_total_receptor", label = div (style = "font-size:14px;", "Conversion of partner 1 (pM)"),
  #                            value = 100)
  #             )
  #           ),
  # 
  #           div(
  #             style = "margin-top:-20px;",
  #             numericInput("mol_weight_p1",
  #                          label = div (style = "font-size:14px;","Molecular weight partner 1 (1dalton=g/mol)"),
  #                          value = 100)
  #           ),
  # 
  #           div(
  #             style = "margin-top:0px;margin-bottom: 30px",
  #             sliderInput("rec_range",  label = div (style = "font-size:14px;","Concentration range to plot (log units)"),
  #                         min=-6, max=6, value=c(-2,2), step=1 )
  #           ),
  # 
  #           # advanced sliders
  #           div(
  #             style = "margin-top:-20px;margin-bottom:20px",
  #             # shinyjs::hidden(
  #               materialSwitch(
  #                 inputId = "adv_slider_p1",
  #                 label = "Advanced options",
  #                 status = "primary",
  #                 right = TRUE
  #               )
  #             # )
  #           ),
  # 
  # 
  #           div(
  #             style = "width:75px; display:inline-block;margin-top:-10px;",
  #             numericInput("plot_start_p1", label = HTML("Plot start"), value = 84)
  #           ),
  # 
  #           div(
  #             style = "width:75px; display:inline-block;margin-top:-10px;margin-right:20px",
  #             numericInput("plot_end_p1", label = HTML("Plot end"), value = 84000 )
  #           ),
  #           div(
  #             style = "width:130px; display:inline-block;margin-top:-10px;margin-bottom:100px;
  #             vertical-align:top;margin-right:20px",
  #             # numericInput("plot_interval_p1", label = HTML("No. of points to plot"), value = 0.2 )
  #             # numericInput("plot_interval_p1", label = HTML("No. of data points per line"), value = 5 , max = Inf, min = 1, step = 1)
  #             # sliderInput("plot_interval_p1", label = HTML("Plot interval (x-axis)"), min = 0.2,
  #             #             max = 1, value = 1, step = 0.1)
  #             numericInput("plot_interval_p1", label = HTML("Plot interval (x-axis)"), min = 0.2,
  #                         max = 1, value = 1, step = 0.1)
  #           ),
  # 
  #           div(
  #             style = "width:120px; display:inline-block; height:000px;margin-top:-20px;
  #             margin-bottom:10px",
  #             actionBttn(
  #               inputId = "submit_slider_range_p1",
  #               label = "Apply changes",
  #               style = "pill",
  #               color = "primary",
  #               size = "xs"
  #             ),
  #           ),
  # 
  # 
  #           # div(
  #           #   style = "margin-top:0px;margin-bottom:0px; ",
  #           #   materialSwitch(
  #           #     inputId = "adv_options",
  #           #     label = "Advanced options",
  #           #     status = "primary",
  #           #     right = TRUE
  #           #   )
  #           # ),
  #           # div (
  #           #   style = "width:50px; display:inline-block; margin-top:-20px;margin-bottom:10px",
  #           #   # shinyjs::hidden(
  #           #     actionBttn(
  #           #       inputId = "submit_slider_p1_lines",
  #           #       label = "Apply changes",
  #           #       style = "pill",
  #           #       color = "primary",
  #           #       size = "xs"
  #           #     )
  #           #   # )
  #           # ),
  # 
  #           # div(
  #           #   style = "margin-bottom:5px;font-size:16px",
  #           #   awesomeRadio(
  #           #     inputId = "adv_options_choices",
  #           #     label = "Please make a selection:",
  #           #     choices = c ("Change no. of data points on each curve",
  #           #                  "Change no. of curves (simulations)",
  #           #                  "Change both no. of data points and curves"),
  #           #     status = "primary",
  #           #     selected = ""
  #           #   )
  #           # ),
  # 
  #           # div(
  #           #   style = "margin-bottom:5px;font-size:16px",
  #           #   hidden(
  #           #     selectInput("choose_lines", label = "Please select one:",
  #           #                    choices = c("Partner 2", "Kd",""),selected = "")
  #           #     # awesomeCheckboxGroup(
  #           #     #   inputId = "choose_lines",
  #           #     #   label = "Please select one or both:",
  #           #     #   choices = c("Partner 2", "Kd"),
  #           #     #   inline = TRUE,
  #           #     #   status = "primary"
  #           #     # )
  #           #   )
  #           # )
  #       ),
  # 
  #       div( style = "display: inline-block;vertical-align:top; margin-bottom:0px;font-size:16px;margin-top:-10px",
  # 
  #           div(
  #             style = "margin-top:0px; margin-bottom:20x; font-size: 16px; width:300px",
  #             textInput("ligand_name", label = div (style = "font-size:16px; ", "Name interaction partner 2"),
  #                       value = "Partner 2 name")
  #           ),
  # 
  #           div(
  #             style = "margin-top:10px;",
  #             radioButtons("p2concunit", label = div (style = "font-size:14px; ","Concentration unit for partner 2"), choices = c("metric units", "molar units"),
  #                          selected = "metric units")
  #           ),
  # 
  #           # div(
  #           #   style = "margin-top:0px;",
  #           #   selectInput("p2unit", 'Select unit for partner 2',
  #           #               choices = list("g" = 1, "ng" = 2),
  #           #               selected = 1)
  #           # ),
  # 
  #           div(
  #             style = "margin-top:0px;",
  #             selectInput("p2unit_metric", HTML('<FONT size="2pt">Select unit for partner 2'),
  #                         choices = list( "mg/ml", "ug/ml", "ng/ml","pg/ml"),
  #                         selected = "pg/ml")
  #           ),
  # 
  #           div(
  #             style = "margin-top:0px;",
  #             selectInput("p2unit_molar", HTML('<FONT size="2pt">Select unit for partner 2'),
  #                         choices = list("fM" , "pM" , "nM" , "uM" , "mM" ),
  #                         selected = "pM")
  #           ),
  #           div(
  #             style = "margin-top:0px;",
  #             numericInput("c_total_ligand_start", label = div (style = "font-size:14px; ","Concentration of partner 2"),
  #                          value = 100)
  #           ),
  # 
  #           div(
  # 
  #             style = "margin-top:0px;width:125px;display:inline-block",
  # 
  #             shinyjs::hidden(
  #               numericInput("c_total_ligand", label = div (style = "font-size:14px; ","Conversion of partner 2 (pM)"),
  #                            value = 100)
  #             )
  #           )
  #           ,
  # 
  #           div(
  #             style = "margin-top:-20px;",
  #             numericInput("mol_weight_p2",
  #                          label = div (style = "font-size:14px; ","Molecular weight partner 2 (1dalton=g/mol)"),
  #                          value = 100)
  #           ),
  # 
  #           div(
  #             style = "margin-top:0px;margin-bottom:30px",
  #             sliderInput("lig_range", label = div (style = "font-size:14px; ","Concentration range to plot (log units)"), min = -6,
  #                         max = 6, value = c(-4, 2))
  #           ),
  # 
  # 
  #           # advanced sliders
  #           div(
  #             style = "margin-top:-20px;margin-bottom:20px",
  #             # shinyjs::hidden(
  #               materialSwitch(
  #                 inputId = "adv_slider_p2",
  #                 label = "Advanced options",
  #                 status = "primary",
  #                 right = TRUE
  #               )
  #             # )
  #           ),
  #           #
  #           div(
  #             style = "width:75px; display:inline-block;margin-top:-10px;",
  #             shinyjs::hidden(
  #               numericInput("plot_start_p2", label = HTML("Plot start"), value = 84)
  #             )
  #           ),
  # 
  #           div(
  #             style = "width:75px; height:10px;display:inline-block;margin-top:-10px; margin-right:20px",
  #             shinyjs::hidden(
  #               numericInput("plot_end_p2", label = HTML("Plot end"), value = 84000 )
  #             )
  #           ),
  #           div(
  #             style = "width:130px; display:inline-block;margin-top:-10px;margin-bottom:100px;
  #             vertical-align:top;margin-right:20px",
  #             # shinyjs::hidden(
  #             # # numericInput("plot_interval_p2", label = HTML("No. of points to plot"), value = 10 )
  #             numericInput("plot_interval_p2", label = HTML("Plot interval"),  value = 1 , max = 1, min = 0.1, step = 0.1)
  #             # )
  #           ),
  # 
  #           div(
  #             style = "width:120px; display:inline-block; margin-top:-20px;margin-bottom:10px",
  #             actionBttn(
  #               inputId = "submit_slider_range_p2",
  #               label = "Apply changes",
  #               style = "pill",
  #               color = "primary",
  #               size = "xs"
  #             )
  #           )
  #         ),
  # 
  # 
  #       div(style = "display: inline-block;vertical-align:top; margin-bottom:0px;font-size:16px;margin-top:-10px",
  # 
  #           div(
  #             style = "margin-top:0px; margin-bottom:20x; font-size: 20px; width:300px",
  #             selectInput("constants", label = div (style = "font-size:16px; ","Which constants to use:"),
  #                         choices=c("KD & kon", "KD & koff", "kon & koff", "only KD"))
  #           ),
  # 
  #           div(
  #             style = "margin-top:0px;",
  #             numericInput("KD", label = div (style = "font-size:14px;", HTML("K<sub>D</sub> (pM)")),
  #                          value = format(100, scientific = T))
  #           ),
  # 
  #           div(
  #             style = "margin-top:0px;",
  #             numericInput("koff", label = div (style = "font-size:14px;", HTML("Dissociation rate (s<sup>-1</sup>)")),
  #                          value = format(1e8, scientific = T))
  #           ),
  #           div(
  #             style = "margin-top:0px;",
  #             numericInput("kon", label = div (style = "font-size:14px;", HTML("Association rate (M<sup>-1</sup> s <sup>-1</sup>)")),
  #                          value = format(1e6, scientific = T))
  #           ),
  # 
  #           div(
  #             style = "margin-top:80px;margin-bottom:30px",
  #             sliderInput("kd_range", label = div (style = "font-size:14px;", HTML("K<sub>D</sub> range to plot (log units)")), min = -6,
  #                         max = 6, value = c(-4, 2), step=1)
  #           ),
  # 
  # 
  #           # advanced sliders
  #           div(
  #             style = "margin-top:-20px;margin-bottom:20px",
  #             # shinyjs::hidden(
  #               materialSwitch(
  #                 inputId = "adv_slider_kd",
  #                 label = "Advanced options",
  #                 status = "primary",
  #                 right = TRUE
  #               )
  #             # )
  #           ),
  # 
  #           div(
  #             style = "width:75px; display:inline-block;margin-top:-10px;",
  #             numericInput("plot_start_kd", label = HTML("Plot start"), value = 84)
  #           ),
  # 
  #           div(
  #             style = "width:75px; display:inline-block;margin-top:-10px;margin-right:20px",
  #             shinyjs::hidden(
  #               numericInput("plot_end_kd", label = HTML("Plot end"), value = 84000 )
  #             )
  #           ),
  #           div(
  #             style = "width:130px; display:inline-block;margin-top:-10px;margin-bottom:100px;
  #             vertical-align:top;margin-right:20px",
  #             # shinyjs::hidden(
  #             # numericInput("plot_interval_kd", label = HTML("No. of points to plot"), value = 10 )
  #             # numericInput("plot_interval_kd", label = HTML("No. of data points"),  value = 1 , max = Inf, min = 1, step = 1)
  #              numericInput("plot_interval_kd", label = HTML("Plot interval Kd"),  value = 1 , max = 1, min = 0.1, step = 0.1)
  #             # )
  #           ),
  # 
  #           div(
  #             style = "width:120px; display:inline-block; margin-top:-20px;margin-bottom:10px",
  #             actionBttn(
  #               inputId = "submit_slider_range_kd",
  #               label = "Apply changes",
  #               style = "pill",
  #               color = "primary",
  #               size = "xs"
  #             )
  #           )
  # 
  # 
  #         ),
  # 
  # 
  #       div( style = "display: inline-block;vertical-align:top; margin-bottom:0px;font-size:16px;margin-top:-10px",
  #         # selectInput("unit", label = div (style = "font-size:16px;","Change units"),
  #         #             #choices = list("fM", "pM", "nM",  "uM", "mM"),
  #         #             choices = list("pM"),
  #         #             selected = "pM"),
  # 
  #         selectInput("scale", label = div (style = "font-size:16px;", "Change y axis scale"),
  #                     choices = list("linear scale" = 1, "logarithmic scale" = 2),
  #                     selected = 1)
  # 
  # 
  #       ),
  # 
  #       # hr(style = "border-color:darkgray; height:2px;margin-top:-15px;"),
  #       #Dashboard and plots
  # 
  #       uiOutput("bindingDashboard")#,
  # 
  #     ) ## end dashboard body
  #   ) ## end binding dashboard page
  # ) ## end binding tab
  
  
  # Competition tab ----
  tabPanel(
    # "Competition", ## tab title
    title = div(img(src = "competitionLogo.png", height = "15%", width = "15%"), "Competition"
    ),
    ## Create a dashboard page inside of the tab
    dashboardPage(

      ## Disable header
      dashboardHeader(
        disable = TRUE
      ),

      ## Create a sidebar
      dashboardSidebar(
        disable = TRUE
      ), # end Competition sidebar

      ## Main body competition
      dashboardBody(
        fluidPage(
        style = " background-color: #f0f0f0",
        includeCSS("www/style.css"), ## add custom style

        tags$head(
          tags$style(HTML("
          .shiny-output-error-validation {
            color: #1e90ff;
            font-weight: bold;
            font-size: 16px;
            }
          "))
        ),

        tags$style(type='text/css',
                   ".selectize-input { font-size: 14px; line-height: 16px;} .selectize-dropdown { font-size: 14px; line-height: 16px; }"),

        # tags$head(
        #   tags$style(HTML('#Go{background-color:#000000}'))
        # ),
        # submitButton("Go", icon("thumbs-up")),
        div (style = "display:inline-block; vertical-align:top; padding-top: 0px;margin-bottom:35px; margin-left:0px; ",
             actionBttn('submit_query_competition', 'Go', size = "xs",
                        style = "unite", color = "success",  icon = icon("thumbs-up")),
                   actionBttn('clear_filters_competition', 'Reset all', size = "xs", style = "jelly", color = "danger",
                              icon = icon("refresh"))
        ),

        br(),

        #block 1
        # column(
          # width = 2,
          div(style = "display:inline-block;vertical-align:top; margin-bottom:0px;font-size:16px;margin-top:0px; margin-right:10px",

              div(
                style = "margin-top:0px; margin-bottom:20x; font-size: 16px; width:300px",
                  textInput("receptor_name_CT", label = div (style = "font-size:16px;", "Name interaction partner 1"), value = "Partner 1 name")
                ),

              div(
                style = "width:200px; margin-top:0px;font-size:16px;margin-bottom:20px;",
                radioButtons("p1concunit_CT", label = div (style = "font-size:14px; ", "Concentration unit for partner 1"),
                             choices = c("metric units", "molar units"), selected = "metric units")
                ),

              # div(
              #   style = "margin-top:0px;",
              #   selectInput("p1unit_CT", HTML('<FONT size="2pt">Select unit for partner 1'),
              #         choices = list("g" = 1, "ng" = 2),
              #         selected = 1)
              #   ),

              div(
                style = "margin-top:0px;",
                selectInput("p1unit_CT_metric", HTML('<FONT size="2pt">Select unit for partner 1'),
                            choices = list( "mg/ml", "ug/ml", "ng/ml","pg/ml"),
                            selected = "pg/ml")
              ),

              div(
                style = "margin-top:0px;",
                selectInput("p1unit_CT_molar", HTML('<FONT size="2pt">Select unit for partner 1'),
                            choices = list("fM" , "pM" , "nM" , "uM" , "mM" ),
                            selected = "pM")
              ),


              div(
                style = "margin-top:0px;",
                 numericInput("c_total_receptor_CT_start", label = div (style = "font-size:14px; ", "Concentration of partner 1"), value = 100)
                ),

              div(
                style = "margin-top:-20px;width:127px; display:inline-block",
                shinyjs::hidden(
                  numericInput("c_total_receptor_CT", label = div (style = "font-size:14px;", "Conversion of partner 1 (pM)"),
                               value = 100)
                )
              ),

              div(
                style = "margin-top:-20px;",
                  numericInput("mol_weight_p1_CT", label = div (style = "font-size:14px; ", "Molecular weight partner 1 (1dalton=g/mol)"), value = 100)
                ),

              div(
                style = "margin-top:0px;margin-bottom: 30px",
                  sliderInput("rec_range_CT", label = div (style = "font-size:14px; ", "Concentration range to plot (log units)"), min=-6, max=6, value=c(-2,2), step=1 )
                ),


              # advanced sliders
              div(
                style = "margin-top:-20px;margin-bottom:20px",
                # shinyjs::hidden(
                materialSwitch(
                  inputId = "adv_slider_p1_CT",
                  label = "Advanced options",
                  status = "primary",
                  right = TRUE
                )
                # )
              ),
              
              
              div(
                style = "width:60px; display:inline-block;margin-top:-10px;margin-right:10px;",
                numericInput("plot_start_p1_CT", label = HTML("Plot start"), value = 84)
              ),
              
              div(
                style = "width:60px; display:inline-block;margin-top:-10px;margin-right:10px",
                numericInput("plot_end_p1_CT", label = HTML("Plot end"), value = 84000 )
              ),
              div(
                style = "width:60px; display:inline-block;margin-top:-10px;margin-bottom:100px;
              vertical-align:top;margin-right:20px",
                # numericInput("plot_interval_p1", label = HTML("No. of points to plot"), value = 0.2 )
                # numericInput("plot_interval_p1", label = HTML("No. of data points per line"), value = 5 , max = Inf, min = 1, step = 1)
                # sliderInput("plot_interval_p1", label = HTML("Plot interval (x-axis)"), min = 0.2,
                #             max = 1, value = 1, step = 0.1)
                numericInput("plot_interval_p1_CT", label = HTML( "Interval"), min = 0.1,
                             max = 1, value = 1, step = 0.1)
              ),
              
              div(
                style = "width:120px; display:inline-block; height:000px;margin-top:-20px;
              margin-bottom:10px",
                actionBttn(
                  inputId = "submit_slider_range_p1_CT",
                  label = "Apply changes",
                  style = "pill",
                  color = "primary",
                  size = "xs"
                ),
              ),
              
              
              # )
          ),


        #BLOCK 2
        # column(
          # width = 2,
          div( style = "display: inline-block;vertical-align:top; margin-bottom:0px;font-size:16px;margin-top:0px;margin-right:10px",

              div(
                 style = "margin-top:0px; margin-bottom:20x; font-size: 16px; width:300px",
                textInput("ligand_name_CT", label = div (style = "font-size:16px; ", "Name interaction partner 2"), value = "Partner 2 name")
                ),

              div(
                style = "width:200px; margin-top:0px;font-size:16px;margin-bottom:20px;",
                radioButtons("p2concunit_CT", label = div (style = "font-size:14px;", "Concentration unit for partner 2"),
                             choices = c("metric units", "molar units"),
                           selected = "metric units")
                ),



              # div(
              #   style = "margin-top:0px;",
              #   selectInput("p2unit_CT", HTML('<FONT size="2pt">Select unit for partner 2'),
              #             choices = list("g" = 1, "ng" = 2),
              #             selected = 1)
              #   ),

              div(
                style = "margin-top:0px;",
                selectInput("p2unit_CT_metric", HTML('<FONT size="2pt">Select unit for partner 2'),
                            choices = list( "mg/ml", "ug/ml", "ng/ml","pg/ml"),
                                                  selected = "pg/ml")
                ),

              div(
                style = "margin-top:0px;",
                selectInput("p2unit_CT_molar", HTML('<FONT size="2pt">Select unit for partner 2'),
                            choices = list("fM" , "pM" , "nM" , "uM" , "mM" ),
                          selected = "pM")
                ),



              div(
                style = "margin-top:0px;",
                numericInput("c_total_ligand_CT_start", label = div (style = "font-size:14px;", "Concentration of partner 2"), value = 100)
                ),

              div(
                style = "margin-top:-20px;width:127px; display:inline-block",
                shinyjs::hidden(
                  numericInput("c_total_ligand_CT", label = div (style = "font-size:14px;", "Conversion of partner 1 (pM)"),
                               value = 100)
                )
              ),

              div(
                style = "margin-top:-20px;",
                numericInput("mol_weight_p2_CT", label = div (style = "font-size:14px;", "Molecular weight partner 2 (1dalton=g/mol)"), value = 100)
                ),

              div(
                style = "margin-top:0px;margin-bottom: 30px",
                  sliderInput("lig_range_CT", label = div (style = "font-size:14px;", "Concentration range to plot (log units)"), min = -6,
                          max = 6, value = c(-2, 2))
                ),
              
              # advanced sliders
              div(
                style = "margin-top:-20px;margin-bottom:20px",
                # shinyjs::hidden(
                materialSwitch(
                  inputId = "adv_slider_p2_CT",
                  label = "Advanced options",
                  status = "primary",
                  right = TRUE
                )
                # )
              ),
              #;
              div(
                style = "width:60px; display:inline-block;margin-top:-10px;margin-right:10px",
                shinyjs::hidden(
                  numericInput("plot_start_p2_CT", label = HTML("Plot start"), value = 84)
                )
              ),
              
              div(
                style = "width:60px; height:10px;display:inline-block;margin-top:-10px; margin-right:10px",
                shinyjs::hidden(
                  numericInput("plot_end_p2_CT", label = HTML("Plot end"), value = 84000 )
                )
              ),
              div(
                style = "width:60px; display:inline-block;margin-top:-10px;margin-bottom:100px;
              vertical-align:top;margin-right:20px",
                # shinyjs::hidden(
                # # numericInput("plot_interval_p2", label = HTML("No. of points to plot"), value = 10 )
                numericInput("plot_interval_p2_CT", label = HTML("Interval"),  value = 1 , max = 1, min = 0.1, step = 0.1)
                # )
              ),
              
              div(
                style = "width:120px; display:inline-block; height:000px;margin-top:20px;
              margin-bottom:10px",
                actionBttn(
                  inputId = "submit_slider_range_p2_CT",
                  label = "Apply changes",
                  style = "pill",
                  color = "primary",
                  size = "xs"
                ),
              )

          # )
        ),

        #block 3
        # column(
          # width = 2,
          div( style = "display: inline-block;vertical-align:top; margin-bottom:0px;font-size:16px;margin-top:0px;margin-right:10px",

               div(
                 style = "margin-top:1px; margin-bottom:25x; font-size: 16px; width:300px",
                 numericInput("Kd_CT", label = HTML('<FONT size="2pt"> K<sub>D</sub> (pM)'), value = 100)
                 ),

               # div(
               #   style = "width:200px; margin-top:0px;font-size:16px;margin-bottom:20px;",
               #   numericInput("kon_CT", label = (HTML('<FONT size="2pt"> Association rate (M<sup>-1</sup> s <sup>-1</sup>)')), value = 1e6)
               #   ),

               div(
                 style = "margin-top:0px;margin-bottom:20px",
                 sliderInput("kd_range_CT", label = HTML('<FONT size="2pt"> K<sub>D</sub> range to plot (log units)'), min = -6,
                        max = 6, value = c(-2, 2), step=1)
               ),
               
               # advanced sliders
               div(
                 style = "margin-top:0px;margin-bottom:20px",
                 # shinyjs::hidden(
                 materialSwitch(
                   inputId = "adv_slider_kd_CT",
                   label = "Advanced options",
                   status = "primary",
                   right = TRUE
                 )
                 # )
               ),
               
               div(
                 style = "width:60px; display:inline-block;margin-top:-10px;margin-right:10px",
                 numericInput("plot_start_kd_CT", label = HTML("Plot start"), value = 84)
               ),
               
               div(
                 style = "width:60px; display:inline-block;margin-top:-10px;margin-right:10px",
                 shinyjs::hidden(
                   numericInput("plot_end_kd_CT", label = HTML("Plot end"), value = 84000 )
                 )
               ),
               div(
                 style = "width:60px; display:inline-block;margin-top:-10px;margin-bottom:100px;
              vertical-align:top;margin-right:20px",
                 # shinyjs::hidden(
                 # numericInput("plot_interval_kd", label = HTML("No. of points to plot"), value = 10 )
                 # numericInput("plot_interval_kd", label = HTML("No. of data points"),  value = 1 , max = Inf, min = 1, step = 1)
                 numericInput("plot_interval_kd_CT", label = HTML("Interval"),  value = 1 , max = 1, min = 0.1, step = 0.1)
                 # )
               ),
               
               div(
                 style = "width:120px; display:inline-block; margin-top:-20px;margin-bottom:10px",
                 actionBttn(
                   inputId = "submit_slider_range_kd_CT",
                   label = "Apply changes",
                   style = "pill",
                   color = "primary",
                   size = "xs"
                 )
               )
          # )
          ),



        #block 4
        # column(
          # width = 2,
            div( style = "display: inline-block;vertical-align:top; margin-bottom:0px;font-size:16px;margin-top:0px; margin-right:10px",

                 div(
                   style = "margin-top:1px; margin-bottom:20x; font-size: 16px; width:300px",
                   textInput("coating_name_CT", label = div (style = "font-size:16px;", "Name competitor (Interaction partner 3)"),
                             "Competing partner name")
                   ),

                 div(
                   style = "width:300px; margin-top:0px;font-size:16px;margin-bottom:20px;",
                   radioButtons("coat_concunit_CT", label = div (style = "font-size:14px;", "Concentration unit for the competition partner"),
                                choices = c("metric units", "molar units"), selected = "metric units")
                   ),

                 # div(
                 #   style = "margin-top:-5px;",
                 #   selectInput("coat_unit_CT", "Select unit for the competition partner",
                 #          choices = list("g" = 1, "ng" = 2),
                 #          selected = 1)
                 #   ),

                 div(
                   style = "margin-top:0px;",
                   selectInput("coat_unit_CT_metric", HTML('<FONT size="2pt">Select unit for Competitor'),
                               choices = list( "mg/ml", "ug/ml", "ng/ml","pg/ml"),
                               selected = "pg/ml")
                 ),

                 div(
                   style = "margin-top:0px;",
                   selectInput("coat_unit_CT_molar", HTML('<FONT size="2pt">Select unit for Competitor'),
                               choices = list("fM" , "pM" , "nM" , "uM" , "mM" ),
                               selected = "pM")
                 ),

                 div(
                   style = "margin-top:0px;",
                   numericInput("c_total_coating_CT_start", label = div (style = "font-size:14px;", "Concentration of competing partner"), value = 100)
                   ),

                 div(
                   style = "margin-top:0px;",
                   shinyjs::hidden(
                     numericInput("c_total_coating_CT", label = div (style = "font-size:14px;", "Concentration of competing partner"), value = 100)
                   )
                 ),
                div(

                    style = "margin-top:-1px;",
                  numericInput("mol_weight_coat", label =  div (style = "font-size:14px;", "Molecular weight of competitor (1dalton=g/mol)"), value = 100)
                  ),

                div(
                  style = "margin-top:5px;",
                  sliderInput("coat_range_CT",  label = div (style = "font-size:14px;", "Concentration range to plot (log units)"), min = -6,
                            max = 6, value = c(-2, 2))
                  ),
                
                # advanced sliders
                div(
                  style = "margin-top:0px;margin-bottom:20px",
                  # shinyjs::hidden(
                  materialSwitch(
                    inputId = "adv_slider_comp",
                    label = "Advanced options",
                    status = "primary",
                    right = TRUE
                  )
                  # )
                ),
                
                div(
                  style = "width:60px; display:inline-block;margin-top:-10px;margin-right:10px;",
                  numericInput("plot_start_comp", label = HTML("Plot start"), value = 84)
                ),
                
                div(
                  style = "width:60px; display:inline-block;margin-top:-10px;margin-right:10px",
                  shinyjs::hidden(
                    numericInput("plot_end_comp", label = HTML("Plot end"), value = 84000 )
                  )
                ),
                div(
                  style = "width:60px; display:inline-block;margin-top:-10px;margin-bottom:100px;
              vertical-align:top;margin-right:20px",
                  # shinyjs::hidden(
                  # numericInput("plot_interval_kd", label = HTML("No. of points to plot"), value = 10 )
                  # numericInput("plot_interval_kd", label = HTML("No. of data points"),  value = 1 , max = Inf, min = 1, step = 1)
                  numericInput("plot_interval_comp", label = HTML("Interval"),  value = 1 , max = 1, min = 0.1, step = 0.1)
                  # )
                ),
                
                div(
                  style = "width:120px; display:inline-block; margin-top:-20px;margin-bottom:10px",
                  actionBttn(
                    inputId = "submit_slider_range_comp",
                    label = "Apply changes",
                    style = "pill",
                    color = "primary",
                    size = "xs"
                  )
                )

                
            # )
        ),
        #block 5
        # column(
        #   width = 2,

          div( style = "display: inline-block;vertical-align:top; margin-bottom:0px;font-size:16px;margin-top:1px",
            # selectInput("unit_CT",  label = div (style = "font-size:14px;","Change units"),
            #             choices = list("fM" = 1, "pM" = 2, "nM" = 3, "mM" = 4),
            #             selected = 2)

            div(
              style = "margin-top:1px;margin-bottom: 20px",
              numericInput("ki_CT", label = HTML('<FONT size = "2pt"> K<sub>I</sub> (K<sub>D</sub> of competition partner for partner 1) (pM))'), value = 1000)
            ),
            
            div(
              style = "margin-top:-5px;margin-bottom: 20px",
              sliderInput("pKdcoat_CT",  label = HTML ('<FONT size = "2pt"> K<sub>I</sub> range to plot (pM)'), min=-6, max=6, value=c(-2,2), step=1)
            ),
            
            # advanced sliders
            div(
              style = "margin-top:30px;margin-bottom:20px",
              # shinyjs::hidden(
              materialSwitch(
                inputId = "adv_slider_ki",
                label = "Advanced options",
                status = "primary",
                right = TRUE
              )
              # )
            ),
            
            div(
              style = "width:60px; display:inline-block;margin-top:-10px;margin-right:10px;",
              numericInput("plot_start_ki", label = HTML("Plot start"), value = 84)
            ),
            
            div(
              style = "width:60px; display:inline-block;margin-top:-10px;margin-right:10px;",
              shinyjs::hidden(
                numericInput("plot_end_ki", label = HTML("Plot end"), value = 84000 )
              )
            ),
            div(
              style = "width:60px; display:inline-block;margin-top:-10px;margin-bottom:100px;
              vertical-align:top;margin-right:20px",
              # shinyjs::hidden(
              # numericInput("plot_interval_kd", label = HTML("No. of points to plot"), value = 10 )
              # numericInput("plot_interval_kd", label = HTML("No. of data points"),  value = 1 , max = Inf, min = 1, step = 1)
              numericInput("plot_interval_ki", label = HTML("Interval"),  value = 1 , max = 1, min = 0.1, step = 0.1)
              # numericInput("plot_interval_kd_CT", label = HTML("Interval"),  value = 1 , max = 1, min = 0.1, step = 0.1)
              # )
            ),
            
            div(
              style = "width:130px; display:inline-block; margin-top:-20px;margin-bottom:10px",
              actionBttn(
                inputId = "submit_slider_range_ki",
                label = "Apply changes",
                style = "pill",
                color = "primary",
                size = "xs"
              )
            ),
            
            div(
              style = "margin-top:-70px;margin-bottom: 0px; margin-left:60px",
              img(src = "Competition explanation.png", width = "135px"),
            ),
            
            div(
              style = "margin-top:20px;margin-bottom: 0px",
            selectInput("scale_CT", label = div (style = "font-size:16px;", "Change plot scale"),
                        choices = list("linear scale" = 1, "logarithmic scale" = 2),
                        selected = 1)
            ),
            # )
        
          ),

        uiOutput("competitionDashboard"),
        uiOutput("controls_header"),


        fluidRow(
          box(width=12,

                # box(width=12,
                    fluidRow(
                      div (style = "display:inline-block;  padding-top: 0px; margin-left:15px; margin-bottom:5px; ",
                           actionBttn('submit_query_local_graph1', 'Go', size = "xs",
                                      style = "unite", color = "success",  icon = icon("thumbs-up"))
                      ),
                      div( 
                           column(3, selectInput("A_selected_first_var", label = 'Change fixed variable',
                                                 choices = list("KD", "Competing partner", "Partner 2","KI"),
                                                              selected = "KD")),
                           column(3, selectInput("A_change_first_var", label = 'Change plotted variable',
                                                 choices = list("KD", "Competing partner", "Partner 2", "KI"),
                                                 selected = "Partner 2")),


                           column(3, numericInput('A_first_var_value', label  = HTML("Value 1 for K<sub>D</sub> (pM)"),
                                                  value= 1000)),
                           column(3, numericInput('A_first_var_value2', label = HTML("Value 2 for K<sub>D</sub> (pM)"), value= 10000)),

                      )
                    ),# end fluidrow

                    column(width=6,

                     plotlyOutput("firstPlot_A", width = "100%"),
                     dataTableOutput("firstTable_A")
                    ),
                    column(width=6,
                         plotlyOutput("firstPlot_B", width = "100%"),
                         dataTableOutput("firstTable_B")
                    )
                # )
              )

        ),
        uiOutput("controls_header_graph2"),
        fluidRow(
          box(width=12,
              
              # box(width=12,
              fluidRow(
                div (style = "display:inline-block;  padding-top: 0px; margin-left:15px; margin-bottom:5px; ",
                     actionBttn('submit_query_local_graph2', 'Go', size = "xs",
                                style = "unite", color = "success",  icon = icon("thumbs-up"))
                ),
                div( 
                  column(3, selectInput("A_selected_first_var_graph2", label = 'Change fixed variable',
                                        choices = list("KD", "Competing partner", "Partner 2","KI"),
                                        selected = "KD")),
                  column(3, selectInput("A_change_first_var_graph2", label = 'Change plotted variable',
                                        choices = list("KD", "Competing partner", "Partner 2", "KI"),
                                        selected = "Partner 2")),
                  
                  
                  column(3, numericInput('A_first_var_value_graph2', label  = HTML("Value 1 for K<sub>D</sub> (pM)"),
                                         value= 1000)),
                  column(3, numericInput('A_first_var_value2_graph2', label = HTML("Value 2 for K<sub>D</sub> (pM)"), value= 10000)),
                  
                )
              ),# end fluidrow
              
              column(width=6,

                     plotlyOutput("secondPlot_A_graph2", width = "100%"),
                     dataTableOutput("firstTable_A_graph2")
              ),
              column(width=6,
                     plotlyOutput("secondPlot_B_graph2", width = "100%"),
                     dataTableOutput("firstTable_B_graph2")
              )
              
          )
          
        )

        # uiOutput("controls")
      )
      )#end fluid row
    ) ## end competition dashboard page
  ) ## end competition tab
)  ## end navbarPage
)

