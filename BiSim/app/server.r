## Clean up ----
rm(list = ls())
gc()
#options(bitmapType ='cairo')
options(scipen = 999)
options(stringsAsFactors = FALSE)
options(warn=-1)

## Define relative paths ----
data_path <- "../data" # data for the project (incl libraries)
lib_path <- file.path(data_path, "library") # lib path
# .libPaths(lib_path) # assign lib path\

library(shiny)
library(ggplot2)
library(reshape2)
library(rootSolve)
library(shinyjs)
library(tidyr)
library(DT)
#library(htmltools)
library(dplyr)
library(deSolve)
#library(htmlwidgets)
#library(shinycssloaders)
#library(highcharter)

#Custom Table Container
createContainer <- function(name_partner1 = 'Receptor', name_partner2='Ligand', thinned_spread=thinned_spread){
  sketch = htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        th(rowspan = 1, name_partner1),
        th(colspan =dim(thinned_spread)[2]-1, name_partner2)
      ),
      tr(
        lapply(names(thinned_spread), th)
      )
    )))
  
  return(sketch)
}

function(input, output,session) {
  ## Log user info
  observe({
    
    message(sprintf("Welcoming user: %s. Timestamp: %s.", session$user, Sys.time()))
  })
  
# the modal dialog that shows the disclaimer.
disclaimer_modal <- modalDialog(
  title = "Disclaimer",
  HTML("Copyright (c) 2023 UCB Biopharma Srl"),
  
  HTML(" <br>&nbsp;&nbsp;&nbsp;&nbsp;The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Binding Simulation (BiSim) tool,  a binding simulation tool to aid and simplify ligand binding assay design and development. This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License.
    <br>&nbsp;&nbsp;&nbsp;&nbsp;This program is distributed in the hope that it will be useful. TO THE MAXIMUM EXTENT PERMITTED BY APPLICABLE LAW NO LIABILITY IS ACCEPTED IN RESPECT OF THE USE MADE OR ANY OUTPUT GENERATED, AND WITHOUT ANY WARRANTY OF ANY KIND; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. See the GNU General Public License for more details.
    <br>&nbsp;&nbsp;&nbsp;&nbsp;You should have received a copy of the GNU General Public License along with this program. If not, see "),
  tags$a(href="https://www.gnu.org/licenses/", "https://www.gnu.org/licenses/"),
  hr(),
  easyClose = FALSE,
  footer = modalButton(
    label = "Accept", icon ("check")
  )
  
)

# Show the model on start up ...
showModal(disclaimer_modal)


  ival <- reactiveVal(0)  
  
  onclick("receptor_name",updateTextInput(session,"receptor_name",value="", placeholder = NULL))
  onclick("receptor_name_CT",updateTextInput(session,"receptor_name_CT",value="", placeholder = NULL))
  onclick("ligand_name",updateTextInput(session,"ligand_name",value="", placeholder = NULL))
  onclick("ligand_name_CT",updateTextInput(session,"ligand_name_CT",value="", placeholder = NULL))
  onclick("coating_name_CT",updateTextInput(session,"coating_name_CT",value="", placeholder = NULL))


#binding tab
  
  #switch p1 and p2
  observeEvent(input$switch_p1p2, {
    
    if (input$switch_p1p2){
      
        #if metric units 
        if (input$p1concunit == "metric units"){
          
          my.units <- input$p1unit_metric
          my.conc <- input$c_total_receptor_start
          
          temp_units <- my.units
          temp_conc <- my.conc
          temp_molw <- input$mol_weight_p1
          
          
          #update p1
          updateSelectInput(session, "p1unit_metric", selected = input$p2unit_metric)
          updateNumericInput(session, "c_total_receptor_start", value = input$c_total_ligand_start)
          updateNumericInput(session,  "mol_weight_p1", value = input$mol_weight_p2)
          
          #update p2
          updateSelectInput(session, "p2unit_metric", selected = temp_units)
          updateNumericInput(session, "c_total_ligand_start", value = temp_conc)
          updateNumericInput(session,  "mol_weight_p2", value = temp_molw)
          
        }else if (input$p1concunit == "molar units"){
          my.units <- input$p1unit_molar
          my.conc <- input$c_total_receptor_start
          
          temp_units <- my.units
          temp_conc <- my.conc
          
          #update p1
          updateSelectInput(session, "p1unit_molar", selected = input$p2unit_molar)
          updateNumericInput(session, "c_total_receptor_start", value = input$c_total_ligand_start)
         
          #update p2
          updateSelectInput(session, "p2unit_molar", selected = temp_units)
          updateNumericInput(session, "c_total_ligand_start", value = temp_conc)
          
        }

    }
  })
  
  observeEvent(c(input$constants, input$KD, input$kon, input$koff), {
    if(input$constants =="KD & kon"){
      shinyjs::disable("koff")
      ival(input$KD)
      ival(input$kon)
      ival(input$koff)
      print(input$koff)
      temp <- as.numeric(input$KD * 10^-12)*as.numeric(input$kon) 
      #if (temp)
      print(input$KD)
      print(input$kon)
      print(temp)
      str(temp)
      str(input$koff)
      updateNumericInput(session, "koff", value = format(temp, scientific=TRUE))
      #shinyjs::disable("koff")
      shinyjs::enable("KD")
      shinyjs::enable("kon")
      #shinyjs::disable("koff")
    } else if(input$constants =="KD & koff"){
      ival(input$KD)
      ival(input$koff)
      temp <- as.numeric(input$koff)/as.numeric(input$KD) * 10^12
      updateNumericInput(session, "kon", value = format(temp, scientific=TRUE))
      shinyjs::enable("KD")
      shinyjs::enable("koff")
      shinyjs::disable("kon")
    } else if(input$constants =="kon & koff"){
      ival(input$kon)
      ival(input$koff)
      temp <-  (as.numeric(input$koff)/as.numeric(input$kon)) * 10^12
      updateNumericInput(session, "KD", value = format(temp, scientific=TRUE))
      shinyjs::enable("kon")
      shinyjs::enable("koff")
      shinyjs::disable("KD")
    } else if(input$constants=="only KD"){
      ival(input$KD)
      updateNumericInput(session, "kon", value = 0)
      updateNumericInput(session, "koff", value = 0)
      shinyjs::enable("KD")
      shinyjs::disable("kon")
      shinyjs::disable("koff")
    }
  })
  
  kon_pM <- reactive({
    kon_pM <- input$kon/10^12
    return(kon_pM)
  })
  #print(kon)
  
  koff_pM <- reactive({
    koff_pM <- input$koff/10^12
    return(koff_pM)
  })
  
  
  observeEvent(c(input$p1concunit,
                 input$c_total_receptor_start, input$mol_weight_p1, input$p1unit_metric, input$p1unit_molar),  {
                   
                   if (input$p1concunit == "metric units"){
                     
                     shinyjs::show("mol_weight_p1")
                     shinyjs::show("p1unit_metric")
                     shinyjs::hide("p1unit_molar")
                     # output$converted_concentration <-  renderText(
                     updateRadioButtons(session, "p2concunit", selected = "metric units" )
                     
                     temp <- input$c_total_receptor_start
                     if (input$p1unit_metric == "ug/ml"){
                       
                       temp = format  (input$c_total_receptor_start / 1e6 / input$mol_weight_p1 * 10^12 * 1000, scientific = T)
                     }else if (input$p1unit_metric == "mg/ml"){
                       
                       temp = format  (input$c_total_receptor_start / 1e3 / input$mol_weight_p1 * 10^12 * 1000, scientific = T)
                     }else if (input$p1unit_metric == "ng/ml"){
                       
                       temp = format  (input$c_total_receptor_start / 1e9 / input$mol_weight_p1 * 10^12 * 1000, scientific = T)
                     }else if (input$p1unit_metric == "pg/ml"){
                       
                       temp = format  (input$c_total_receptor_start / 1e12/ input$mol_weight_p1 * 10^12 * 1000, scientific = T)
                     }
                     
                     
                     updateNumericInput(session, "c_total_receptor", label = ("Conversion of partner 1 (pM)"),
                                        value = temp)
                     
                     
                     # )
                     
                   } else if (input$p1concunit == "molar units"){
                     
                     shinyjs::hide("mol_weight_p1")
                     shinyjs::hide("p1unit_metric")
                     shinyjs::show("p1unit_molar")
                     updateRadioButtons(session, "p2concunit", selected = "molar units" )
                     
                     temp <- input$c_total_receptor_start
                     
                     if (input$p1unit_molar == "pM"){
                       
                       temp = format  (input$c_total_receptor_start, scientific = T)
                       
                     } else  if (input$p1unit_molar == "uM"){
                       print ("uM")
                       
                       temp = format  (input$c_total_receptor_start / 1e-6 , scientific = T)
                       
                     } else if (input$p1unit_molar == "fM"){
                       
                       temp = format  (input$c_total_receptor_start / 1e3 , scientific = T)
                       
                     } else if (input$p1unit_molar == "nM"){
                       
                       temp = format  (input$c_total_receptor_start / 1e-3, scientific = T)
                     } else if (input$p1unit_molar == "mM"){
                       
                       temp = format  (input$c_total_receptor_start / 1e-9, scientific = T)
                       print(temp)
                     }
                     
                     print(temp)
                     updateNumericInput(session, "c_total_receptor", label = ("Conversion of partner 1 (pM) "),
                                        value = format(temp, scientific=TRUE))
                   }
                   
                 })
  
  observeEvent (input$adv_slider_p1, { 
    
    if (input$adv_slider_p1){
      
      shinyjs::show("plot_start_p1")
      shinyjs::show("plot_end_p1")
      shinyjs::show("plot_interval_p1")
      shinyjs::show("submit_slider_range_p1")
      
      updateNumericInput(session, "plot_start_p1", value = input$rec_range[1])
      updateNumericInput(session, "plot_end_p1" , value = input$rec_range[2])
      # updateNumericInput(session, "plot_interval", value = input$plot_interval_p1 )
      
      shinyjs::disable("rec_range")
    }else {
      shinyjs::hide ("plot_start_p1")
      shinyjs::hide ("plot_end_p1")
      shinyjs::hide ("plot_interval_p1")
      shinyjs::hide("submit_slider_range_p1")
      shinyjs::enable("rec_range")
    }
    
  })
  
  observeEvent (input$submit_slider_range_p1, { 
    
    if (input$submit_slider_range_p1){
      
      shinyjs::hide ("plot_start_p1")
      shinyjs::hide ("plot_end_p1")
      shinyjs::hide ("plot_interval_p1")
      shinyjs::hide("submit_slider_range_p1")
      shinyjs::enable("rec_range")
      
      # print (input$plot_start_p1)
      # print (input$plot_end_p1)
      # print(input$plot_interval_p1)
      
      updateSliderInput(session, "rec_range", label = "Concentration range to plot (log units)",
                        # min = input$plot_start_p1, max = input$plot_end_p1, step =
                        #   input$plot_interval_p1,
                        # value = c(input$plot_start_p1, input$plot_end_p1))
                        step = input$plot_interval_p1,
                        value = c(input$plot_start_p1, input$plot_end_p1))
      
      
      updateMaterialSwitch(session, "adv_slider_p1", value = FALSE)
      
      # print (input$plot_start_p1)
      # print (input$plot_end_p1)
      # print(input$plot_interval_p1)
      
    }else {
      
      
      shinyjs::show("plot_start_p1")
      shinyjs::show("plot_end_p1")
      shinyjs::show("plot_interval_p1")
      shinyjs::show("submit_slider_range_p1")
      shinyjs::disable("rec_range")
    }
    
  })
  
  observeEvent (input$adv_slider_p2, { 
    
    if (input$adv_slider_p2){
      
      shinyjs::show("plot_start_p2")
      shinyjs::show("plot_end_p2")
      shinyjs::show("plot_interval_p2")
      shinyjs::show("submit_slider_range_p2")
      
      updateNumericInput(session, "plot_start_p2", value = input$lig_range[1])
      updateNumericInput(session, "plot_end_p2" , value = input$lig_range[2])
      updateNumericInput(session, "plot_interval_p2", value = input$plot_interval_p2 )
      shinyjs::disable("lig_range")
      
    }else {
      shinyjs::hide ("plot_start_p2")
      shinyjs::hide ("plot_end_p2")
      shinyjs::hide ("plot_interval_p2")
      shinyjs::hide("submit_slider_range_p2")
      shinyjs::enable("lig_range")
    }
    
  })
  
  observeEvent (input$submit_slider_range_p2, { 
    
    if (input$submit_slider_range_p2){
      
      shinyjs::hide ("plot_start_p2")
      shinyjs::hide ("plot_end_p2")
      shinyjs::hide ("plot_interval_p2")
      shinyjs::hide("submit_slider_range_p2")
      shinyjs::enable("lig_range")
      
      updateSliderInput(session, "lig_range", label = "Concentration range to plot (log units)",
                        # min = input$plot_start_p2, max = input$plot_end_p2,
                        step = input$plot_interval_p2,
                        value = c(input$plot_start_p2, input$plot_end_p2))
      # sliderInput("rec_range", "Concentration range to plot (log units)", min=-6, max=6, value=c(-2,2), step=1 ), 
      
      updateMaterialSwitch(session, "adv_slider_p2", value = FALSE)
      
      print (input$plot_start_p2)  
      print (input$plot_end_p2)
      print(input$plot_interval_p2)
      str(input$lig_range)
      str(input$plot_interval_p2)
    }else {
      
      
      shinyjs::show("plot_start_p2")
      shinyjs::show("plot_end_p2")
      shinyjs::show("plot_interval_p2")
      shinyjs::show("submit_slider_range_p2")
      shinyjs::disable("lig_range")
    }
    
  })
  
  observeEvent (input$adv_slider_kd, { 
    
    if (input$adv_slider_kd){
      
      shinyjs::show("plot_start_kd")
      shinyjs::show("plot_end_kd")
      shinyjs::show("plot_interval_kd")
      shinyjs::show("submit_slider_range_kd")
      
      updateNumericInput(session, "plot_start_kd", value = input$kd_range[1])
      updateNumericInput(session, "plot_end_kd" , value = input$kd_range[2])
      updateNumericInput(session, "plot_interval", value = input$plot_interval_kd)
      
      # print (input$rec_range[1])
      # str (input$rec_range)
      
      shinyjs::disable("kd_range")
    }else {
      shinyjs::hide ("plot_start_kd")
      shinyjs::hide ("plot_end_kd")
      shinyjs::hide ("plot_interval_kd")
      shinyjs::hide("submit_slider_range_kd")
      shinyjs::enable("kd_range")
    }
    
  })
  
  observeEvent (input$submit_slider_range_kd, { 
    
    if (input$submit_slider_range_kd){
      
      shinyjs::hide ("plot_start_kd")
      shinyjs::hide ("plot_end_kd")
      shinyjs::hide ("plot_interval_kd")
      shinyjs::hide("submit_slider_range_kd")
      shinyjs::enable("kd_range")
      
      updateSliderInput(session, "kd_range", label = "Concentration range to plot (log units)",
                        # min = input$plot_start_kd, max = input$plot_end_kd, 
                        step = input$plot_interval_kd,
                        value = c(input$plot_start_kd, input$plot_end_kd))
      # sliderInput("rec_range", "Concentration range to plot (log units)", min=-6, max=6, value=c(-2,2), step=1 ), 
      
      updateMaterialSwitch(session, "adv_slider_kd", value = FALSE)
      
    }else {
      
      
      shinyjs::show("plot_start_kd")
      shinyjs::show("plot_end_kd")
      shinyjs::show("plot_interval_kd")
      shinyjs::show("submit_slider_range_kd")
      shinyjs::disable("kd_range")
    }
    
  })
  
  #unit conversions   
  observeEvent(c(input$p2concunit,
                 input$c_total_ligand_start, input$p2unit_metric, input$p2unit_molar, input$mol_weight_p2),  {
                   
                   if (input$p2concunit == "metric units"){
                     
                     shinyjs::show("mol_weight_p2")
                     shinyjs::show("p2unit_metric")
                     shinyjs::hide("p2unit_molar")
                     updateRadioButtons(session, "p1concunit", selected = "metric units")
                     # output$converted_concentration <-  renderText(
                     
                     
                     temp <- input$c_total_ligand_start
                     
                     if (input$p2unit_metric == "ug/ml"){
                       
                       temp = format  (input$c_total_ligand_start / 1e6 / input$mol_weight_p2 * 10^12 * 1000, scientific = T)
                     }else if (input$p2unit_metric == "mg/ml"){
                       
                       temp = format  (input$c_total_ligand_start / 1e3 / input$mol_weight_p2 * 10^12 * 1000, scientific = T)
                     }else if (input$p2unit_metric == "ng/ml"){
                       
                       # temp = format  (input$c_total_ligand_start / 1e9 / input$mol_weight_p2 * 10^12 * 1000, scientific = T)
                       temp = format  (input$c_total_ligand_start / 1e9 / input$mol_weight_p2 * 10^12 * 1000, scientific = T)
                       
                     }else if (input$p2unit_metric == "pg/ml"){
                       
                       temp = format  (input$c_total_ligand_start / 1e12/ input$mol_weight_p2 * 10^12 * 1000, scientific = T)
                     }
                     
                     updateNumericInput(session, "c_total_ligand", label = ("Conversion of partner 2 "),
                                        value = format(temp, scientific=TRUE))
                     
                     # print ("checking ligand")
                     # print (temp)
                     # print (input$c_total_ligand)
                     
                     # )
                   } else if (input$p2concunit == "molar units"){
                     
                     shinyjs::hide("mol_weight_p2")
                     shinyjs::hide("p2unit_metric")
                     shinyjs::show("p2unit_molar")
                     updateRadioButtons(session, "p1concunit", selected = "molar units")
                     
                     temp <- input$c_total_ligand_start
                     
                     if (input$p2unit_molar == "pM"){
                       
                       temp = format  (input$c_total_ligand_start, scientific = T)
                       
                     } else  if (input$p2unit_molar == "uM"){
                       print ("uM")
                       
                       temp = format  (input$c_total_ligand_start / 1e-6 , scientific = T)
                       
                     } else if (input$p2unit_molar == "fM"){
                       
                       temp = format  (input$c_total_ligand_start / 1e3 , scientific = T)
                       
                     } else if (input$p2unit_molar == "nM"){
                       
                       temp = format  (input$c_total_ligand_start / 1e-3, scientific = T)
                     } else if (input$p2unit_molar == "mM"){
                       
                       temp = format  (input$c_total_ligand_start / 1e-9, scientific = T)
                       print(temp)
                     }
                     
                     # print(temp)
                     
                     updateNumericInput(session, "c_total_ligand", label = ("Conversion of partner 2 "),
                                        value = format(temp, scientific=TRUE))
                   }
                   
                 })
 
  output$downloadBindingPage <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "binding_dashboard.html",


    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      
      #get units
      if (input$p1concunit == "metric units"){

        my.units <- input$p1unit_metric
        my.conc <- input$c_total_receptor_start
        p1mol = input$mol_weight_p1
      }else if (input$p1concunit == "molar units"){
        my.units <- input$p1unit_molar
        my.conc <- input$c_total_receptor
        p1mol = ""
      }
      if (input$p2concunit == "metric units"){

        my.units2 <- input$p2unit_metric
        my.conc2 <- input$c_total_ligand_start
        p2mol = input$mol_weight_p2
      }else  if (input$p2concunit == "molar units"){
        my.units2 <- input$p2unit_molar
        my.conc2 <- input$c_total_ligand
        p2mol = ""
      }

      shiny::withProgress(
        message = "Downloading page to html file", {
          # value = 0,

          # shiny::incProgress(1/10)
          # Sys.sleep(1)
          # shiny::incProgress(1/20)
          # for (i in 1:15) {
          #   incProgress(1/15)
          #   Sys.sleep(0.25)
          # }
          # tempReport <- file.path(tempdir(), "report.Rmd")
          # file.copy("report.Rmd", tempReport, overwrite = TRUE)


          

          # Set up parameters to pass to Rmd document
          # params <- list(my.table1 = inputTable2())
          params <- list(p1_name = input$receptor_name,
                         p1_conc = my.conc,
                         p1_mol_weight = p1mol,
                         p1unit = my.units,
                         p2unit = my.units2,
                         p2_name = input$ligand_name,
                         p2_conc = my.conc2,
                         p2_mol_weight = p2mol,
                         kd = input$KD,
                         koff = input$koff,
                         kon = input$kon,
                         table1 = inputTable1(),
                         table2 = inputTable2(),
                         table3= inputTable3(),
                         table4 = inputTable4(),

                         plot1 = inputCxbyligrangerecplot(),
                         plot2 = inputRoccbyrecrangeligplot(),
                         plot3 = inputCxbyligrangekdplot(),
                         plot4 = inputRoccbyrecrangeligplot2(),
                         plot5 = inputTime_to_equilibrium_plot(),
                         container = createContainer
          )

          # Knit the document, passing in the `params` list, and eval it in a
          # child of the global environment (this isolates the code in the document
          # from the code in this app).
          rmarkdown::render(input = "report.Rmd", output_file = file,
                            params = params,
                            envir = new.env(parent = globalenv())
          )
        })
    }
  )
  
  data_list <- reactive({
    list(
      sheet_data = inputTable1(),
      sheet_data = inputTable2(),
      sheet_data = inputTable3(),
      sheet_data = inputTable4()
    )
  })
  
  output$downloadBindingPageData <- downloadHandler(
    filename = function() {'bindingDashboard.xlsx'},
    content = function(file) {
      
      #get units
      if (input$p1concunit == "metric units"){
        
        my.units <- input$p1unit_metric
        my.conc <- input$c_total_receptor_start
      }else if (input$p1concunit == "molar units"){
        my.units <- input$p1unit_molar
        my.conc <- input$c_total_receptor
      }
      if (input$p2concunit == "metric units"){
        
        my.units2 <- input$p2unit_metric
        my.conc2 <- input$c_total_ligand_start
      }else  if (input$p2concunit == "molar units"){
        my.units2 <- input$p2unit_molar
        my.conc2 <- input$c_total_ligand
      }
      
      
      my_workbook <- createWorkbook()
      
      #sheet 1
      addWorksheet(
        wb = my_workbook,
        sheetName = "cxbyligrangerecplot"
      )
      writeData(
        my_workbook,
        sheet = 1,
        c(
          paste("Concentration", input$receptor_name, "- ", input$ligand_name, 
                "complex (pM) at a fixed KD of", input$KD, "pM")
        ),
        startRow = 1,
        startCol = 1
      )
      writeData(
        my_workbook,
        sheet = 1,
        c(
          paste("Concentration of ", input$receptor_name, " (", my.units, ")")
        ),
        startRow = 4,
        startCol = 1
      )
      writeData(
        my_workbook,
        sheet = 1,
        c(
          paste("Concentration of ", input$ligand_name, " (", my.units2, ")")
        ),
        startRow = 4,
        startCol = 2
      )
      writeData(
        my_workbook,
        sheet = 1,
        inputTable1(),
        startRow = 5,
        startCol = 1
      )
      
      #sheet 2
      addWorksheet(
        wb = my_workbook,
        sheetName = "roccbyrecrangeligplot"
      )
      writeData(
        my_workbook,
        sheet = 2,
        c(
          paste("Fractional occupancy of ", input$ligand_name, " at a fixed KD of ", input$KD, "pM")
        ),
        startRow = 1,
        startCol = 1
      )
      writeData(
        my_workbook,
        sheet =2,
        c(
          paste("Concentration of ", input$receptor_name, " (", my.units, ")")
        ),
        startRow = 4,
        startCol = 1
      )
      writeData(
        my_workbook,
        sheet = 2,
        c(
          paste("Concentration of ", input$ligand_name, " (", my.units2, ")")
        ),
        startRow = 4,
        startCol = 2
      )
      writeData(
        my_workbook,
        sheet = 2,
        inputTable2(),
        startRow = 5,
        startCol = 1
      )
      
      #sheet 3
      addWorksheet(
        wb = my_workbook,
        sheetName = "cxbyligrangekdplot"
      )
      writeData(
        my_workbook,
        sheet = 3,
        c(
          paste("Concentration", input$receptor_name, " - ", input$ligand_name,  
               "complex (", "pM", ") in function of KD at fixed concentration of ", input$ligand_name)
        ),
        startRow = 1,
        startCol = 1
      )
      writeData(
        my_workbook,
        sheet = 3,
        c(
          paste("Concentration of ", input$receptor_name, " (", my.units, ")")
        ),
        startRow = 4,
        startCol = 1
      )
      writeData(
        my_workbook,
        sheet = 3,
        c(
          paste("KD(pM)")
        ),
        startRow = 4,
        startCol = 2
      )
      writeData(
        my_workbook,
        sheet = 3,
        inputTable3(),
        startRow = 5,
        startCol = 1
      )
      
      #sheet 4
      addWorksheet(
        wb = my_workbook,
        sheetName = "roccbyrecrangeligplot2"
      )
      writeData(
        my_workbook,
        sheet = 4,
        c(
          paste("Fractional occupancy of", input$ligand_name,"at a fixed",  input$ligand_name, "concentration of", 
                input$c_total_ligand_start ,input$p2unit_metric)
        ),
        startRow = 1,
        startCol = 1
      )
      writeData(
        my_workbook,
        sheet = 4,
        c(
          paste("KD(pM)")
        ),
        startRow = 4,
        startCol = 2
      )
      writeData(
        my_workbook,
        sheet = 4,
        c(
          paste("Concentration of ", input$receptor_name, " (", my.units, ")")
        ),
        startRow = 4,
        startCol = 1
      )
      writeData(
        my_workbook,
        sheet = 4,
        inputTable4(),
        startRow = 5,
        startCol = 1
      )
      
      saveWorkbook(my_workbook, file)
      # tbl1 <- rbind ("hello", inputTable1())
      # tbl2 <- inputTable2()
      # tbl3 <- inputTable3()
      # tbl4 <- inputTable4()
      
      # sheets <- mget(ls(pattern = "tbl")) # getting all objects in your environment with tbl in the name
      # names(sheets) <- c ("cxbyligrangerecplot", "roccbyrecrangeligplot", "cxbyligrangekdplot", "roccbyrecrangeligplot2")
      # # names(sheets) <- paste0("sheet", seq_len(length(sheets))) # changing the names in your list
      # writexl::write_xlsx(sheets, path = file) # saving the file
      # 
      # # write.xlsx(x =data_list(), file)
      # # write.xlsx(x =data_list(), file, names(x), row.names= F, col.names = T, append=T)
      # # htmlwidgets::saveWidget(as_widget(inputBindingDashboard()), file)
      
    }
  )
  
  output$bindingDashboard <- renderUI({ 
    print(inputBindingDashboard())
  })
  
  inputBindingDashboard <- reactive ({
    
    fluidRow(
      width=12,
      
      # selectInput("download", "Select Data to download", choices = c("euro", "mtcars", "iris")),
      # downloadButton("downloadData"),
      div( style = "margin-right:15px; margin-top:-10px;margin-bottom:30px;float:right",
           downloadButton('downloadBindingPage', 'Download Page'),
           # ),
           # div( style = "margin-right:15px; margin-top:-10px;margin-bottom:30px;float:right",
           downloadButton('downloadBindingPageData', 'Download Data')
      ),
      box(
        width=12,
        style = " background-color: #FFFFFF; margin-top:-25px",
        fluidRow(
          column(width = 6,
                 align = "left",
                 
                 div( style = "margin:0px; padding:5px;background-color: #F7FAFC;height:200px;",
                      
                      div (
                        HTML("Values"), style = "color:dodgerblue;margin-top: 0px; font-size: 20px; 
                       font-weight: bold;font-style: Lato;"),
                      
                      if (input$p1concunit == "molar units" | input$p2concunit == "molar units"){
                        
                        fluidRow(
                          # div(
                          column(
                            width = 6,
                            
                            div(HTML(input$receptor_name),
                                style = "margin-top: 5px; font-weight:bold;
                                     font-size: 14px;
                                     margin-bottom: 0px;
                                     color: #718096; font-style: Lato;"),
                            
                            
                            div(input$ligand_name,
                                style = "margin-top: 0px; font-size: 14px;font-weight:bold;
                               margin-bottom: 0px;
                               color: #718096; font-style: Lato;"),
                            
                            div(
                              HTML("K<sub>D</sub>"), br(),
                              HTML("k<sub>off</sub>" ), br(),
                              HTML("k<sub>on</sub>"),
                              style = "margin-top: 0px; font-size: 14px; margin-bottom: 50px;font-weight:bold;
                           color: #718096; font-style: Lato;"
                            )
                            
                            
                          ),
                          
                          column(width = 6,
                                 
                                 div(HTML(format(input$c_total_receptor+0,scientific = TRUE, decimal.mark = ","), "(pM)"),
                                     style = "margin-top: 5px; font-size: 14px;
                                 margin-bottom: 0px;
                                 color: #718096; font-style: Lato;"),
                                 
                                 div(format(input$c_total_ligand+0, scientific = TRUE, decimal.mark = ","), "(pM)",
                                     style = "margin-top: 0px; font-size: 14px;
                               margin-bottom: 0px;
                               color: #718096; font-style: Lato;"),
                                 
                                 div(
                                   HTML(input$KD, "(pM)"), br(),
                                   HTML(input$koff, "(s<sup>-1</sup>)"), br(),
                                   HTML( input$kon, "(M<sup>-1</sup> s <sup>-1</sup>)"),
                                   style = "margin-top: 0px; font-size: 14px; margin-bottom: 70px;
                           color: #718096; font-style: Lato;"
                                 )
                          )
                          
                        )#end fluidrow
                      } else {
                        
                        fluidRow(
                          # div(
                          column(
                            width = 6,
                            
                            div(HTML(input$receptor_name),
                                style = "margin-top: 5px; font-weight:bold;
                                     font-size: 14px;
                                     margin-bottom: 0px;
                                     color: #718096; font-style: Lato;"),
                            div(
                              HTML("Molecular weight of partner 1"),
                              style = "margin-top: 0px; font-size: 14px;font-weight:bold;
                            margin-bottom: 0px;
                           color: #718096; font-style: Lato;"
                            ),
                            
                            div(input$ligand_name,
                                style = "margin-top: 0px; font-size: 14px;font-weight:bold;
                               margin-bottom: 0px;
                               color: #718096; font-style: Lato;"),
                            div("Molecular weight of partner 2",
                                style = "margin-top: 0px; font-size: 14px;font-weight:bold;
                            margin-bottom: 0px;
                           color: #718096; font-style: Lato;"
                            ),
                            
                            div(
                              HTML("K<sub>D</sub>"), br(),
                              HTML("k<sub>off</sub>" ), br(),
                              HTML("k<sub>on</sub>"), 
                              style = "margin-top: 0px; font-size: 14px; margin-bottom: 30px;font-weight:bold;
                           color: #718096; font-style: Lato;"
                            )
                          ),
                          
                          column(
                            width = 6,
                            
                            div(HTML(format(input$c_total_receptor+0, scientific = TRUE), "(pM)"),
                                style = "margin-top: 5px; font-size: 14px;
                                 margin-bottom: 0px;
                                 color: #718096; font-style: Lato;"),
                            
                            div(format(input$mol_weight_p1, decimal.mark = ","), "(dalton)",
                                style = "margin-top: 0px; font-size: 14px;
                            margin-bottom: 0px;
                           color: #718096; font-style: Lato;"
                            ),
                            
                            div(format(input$c_total_ligand+0, scientific = TRUE, decimal.mark = ","), "(pM)",
                                style = "margin-top: 0px; font-size: 14px;
                               margin-bottom: 0px;
                               color: #718096; font-style: Lato;"),
                            
                            div(format(input$mol_weight_p2, decimal.mark = ","), "(dalton)",
                                style = "margin-top: 0px; font-size: 14px;
                            margin-bottom: 0px;
                           color: #718096; font-style: Lato;"
                            ),
                            
                            div(
                              HTML(input$KD, "(pM)"), br(),
                              HTML(input$koff, "(s<sup>-1</sup>)"), br(),
                              HTML( input$kon, "(M<sup>-1</sup> s <sup>-1</sup>)"), 
                              style = "margin-top: 0px; font-size: 14px; margin-bottom: 0px;
                           color: #718096; font-style: Lato;"
                            )
                          )
                        )#end fluidrow
                        
                      }
                 ) #end container   
          ), #end values column
          
          
          column(
            width = 6,
            
            div(
              style = "padding:5px;background-color: #F7FAFC;margin-top:0px; height:200px;
                   margin-left:-20px",
              
              div(
                HTML("Observations"), style = "color:dodgerblue;margin-left:0px; margin-top: 0px;
                   font-size: 20px; margin-bottom: 0px;
                         font-weight: bold; font-style: Lato;"),
              
              div(style = "margin-left:0px;margin-top: 0px;",
                  textAreaInput("Comments", label = (""), value = "Add comments or notes for other colleagues...", height='100px')
              )
              
              
            )
          )#end container
          
          
        ),#end fluidrow
        
        
        fluidRow(
          width = 12,
          
          column(
            width = 6,
            div(
              style = "padding:5px;background-color: #F7FAFC;margin:0px;height:150px;",
              
              div(
                HTML("At equilibrium"), style = "margin-top: 0px; font-size: 16px; margin-bottom: 15px; 
                  font-style: Lato; font-weight:bold;"
              ),
              
              fluidRow(
                div(
                  column(width = 6,
                         align = "left",
                         div(HTML("FREE", toupper(input$receptor_name)), style = "margin-top: 0px; font-size: 11px; 
                        margin-bottom: 0px; color: #718096; font-style: Lato;"),
                         h3(strong(textOutput("freePartner1"), style = "font-size: 42px; margin-top: 0px")))
                ),
                column(width = 6,
                       align = "left",
                       div(HTML("FREE", toupper(input$ligand_name)), style = "margin-top: 0px; font-size: 11px; 
                                      margin-bottom: 0px; color: #718096; font-style: Lato;"),
                       h3(strong(textOutput("freePartner2"), style = "font-size: 42px;margin-top: 0px")))
              )
              
            ) #end container
          ),#end column
          
          column(
            width = 6,
            
            div(
              style = "padding:5px;background-color: #F7FAFC;margin:0px;margin-left:-20px;height:150px;",
              
              div(HTML("In Complex Concentration"), style = "margin-top: 0px; font-size: 16px; margin-bottom: 15px; 
                font-weight:bold; ont-style: Lato;"),
              
              fluidRow(
                column(
                  width = 6,
                  div(HTML(toupper(input$receptor_name),"-", toupper(input$ligand_name)), style = "margin-top: 0px; font-size: 11px; 
                         margin-bottom: 0px; color: #718096; font-style: Lato;"),
                  h3(strong(textOutput("InComplexConcentration"), style = "font-size: 42px;"))
                )
              )
            )
            
          ) #end container
        ), #end fluiidRow
        fluidRow(width=12,
                 
                 column(
                   width = 6,
                   
                   div(
                     style = "padding:5px; background-color: #F7FAFC; margin-top:10px;height:150;",
                     
                     div(HTML("Fractional Occupancy"), style = "margin-top: 0px; font-size: 16px; margin-bottom: 7px; 
                font-weight:bold; font-style: Lato;"),
                     
                     fluidRow(
                       div(
                         column(width = 6,
                                align = "left",
                                div(HTML("FRACTION", toupper(input$receptor_name), "BOUND TO", toupper(input$ligand_name)), 
                                    style = "margin-top: 0px; font-size: 11px; margin-bottom: 20px; color: #718096; font-style: Lato;"),
                                h3(strong(textOutput("fractionPartner1") , style = "font-size: 42px;")))
                       ),
                       
                       column(width = 6,
                              align = "left",
                              div(HTML("FRACTION", toupper(input$ligand_name), "BOUND TO", toupper(input$receptor_name)), 
                                  style = "margin-top: 0px; font-size: 11px; margin-bottom: 20px; color: #718096; font-style: Lato;"),
                              h3(strong(textOutput("fractionPartner2"), style = "font-size: 42px;")))
                     )
                   )
                 ),
                 
                 
                 column(
                   width = 6,
                   
                   div(
                     style = "padding:5px; background-color: #F7FAFC; margin-top:10px; margin-left:-20px;
            height:150;",
                     
                     div(HTML("Time to equilibrium"), style = "margin-top: 0px; font-size: 16px; margin-bottom: 7px;
              font-weight: bold; font-style: Lato;"),
                     fluidRow(
                       column(width = 12,
                              align = "left",
                              div(HTML("COMPLEX FORMATION WILL BE ACHIEVED IN"), style = "margin-top: 0px; font-size: 11px; margin-bottom: 0px; color: #718096; font-style: Lato;"),
                              # HTML("<br/>"),
                              # temp <- textOutput("TimeToEql")),
                              # my.minutes <- (temp - my.hours) * 60
                              
                              # validate(
                              #   need(input$kon != 0 & input$koff != 0, "Time to equilibrium cannot be plotted when only KD option is selected")
                              # ),
                              
                              # if (input$kon != 0 & input$koff != 0){
                              # h3(textOutput("TimeToEql")),
                              # # h3(floor(get_TimeToEql()), style = "display:inline-block; font-size: 42px; font-weight:bold"),
                              # h3 ("hour(s)", style = "display:inline-block; font-size: 18px; font-weight:bold"),
                              # h3 ((get_TimeToEql() - floor(get_TimeToEql())) * 60, style = "display:inline-block;font-size: 42px; font-weight:bold"),
                              h3 (htmlOutput("TimeToEql"))
                              # h3 ("hr", style = "display:inline-block; font-size: 18px; font-weight:bold")
                              # }
                              
                       )
                     )
                   ))
        ),
        
        
      ),#end box 
      fluidRow(
        
        tags$head(
          tags$style(HTML('#switchTab{background-color:#f0f0f0}'))
        ),
        column(width=12, 
               div(
                 style = "padding:0px; padding-left:0px;background-color: #f0f0f03; 
               margin-top:0px; display:inline-block;
               margin-bottom:10px; margin-left:15px",
                 actionLink("switchTab", "See impact of sample dilution",
                            style ="color:dodgerblue;border-style: none;")),
               
               div(
                 style = "padding:0px; padding-left:0px;background-color: #f0f0f03; 
                 margin-top:0px; display:inline-block;
                 margin-bottom:10px; margin-left:0px",
                 shinyjs::hidden(
                   actionLink("backToTop2", "Back to Binding Curves",
                              style ="color:dodgerblue;border-style: none;")),
                 
               )
        )
      ),
      uiOutput("plots")
      #),
      #)
      
    )
  })
  
  #binding go and clear control
  #go
  observeEvent(input$submit_query_binding, {
    confirmSweetAlert(
      session = session,
      inputId = "myconfirmation_submit_binding",
      type = "warning",
      title = "Submit with selected inputs?",
      danger_mode = TRUE
    )
  })
  
  # filters
  observeEvent(input$clear_filters_binding, {
    confirmSweetAlert(
      session = session,
      inputId = "myconfirmation_filters_binding",
      type = "warning",
      title = "Clear current selections?",
      danger_mode = TRUE
    )
  })
  
  observeEvent (input$myconfirmation_filters_binding, {
    
    if (input$myconfirmation_filters_binding == TRUE) {
      shinyjs::reset ("submit_query_binding")
      
      updateTextInput (session, "receptor_name", value = "Partner 1 name")
      updateRadioButtons(session,"p1concunit", selected = "metric units")
      updateSelectInput(session,"p1unit_metric", selected = "pg/ml")
      updateNumericInput(session, "c_total_receptor_start", value = 100)
      # updateNumericInput(session, "c_total_receptor", value = 100)
      updateNumericInput(session, "mol_weight_p1", value = 100)
      updateSliderInput(session, "rec_range",value=c(-2,2), min = -6, max = 6)
      updateMaterialSwitch(session, "adv_slider_p1", value = FALSE)
      # 
      updateTextInput (session, "ligand_name", value = "Partner 2 name")
      updateRadioButtons(session,"p2concunit", selected = "metric units")
      updateSelectInput(session,"p2unit_metric", selected = "pg/ml")
      updateNumericInput(session, "c_total_ligand_start", value = 100)
      updateNumericInput(session, "c_total_ligand", value = 100)
      updateNumericInput(session, "mol_weight_p2", value = 100)
      updateSliderInput(session, "lig_range",value=c(-4, 2), min = -6, max = 6)
      updateMaterialSwitch(session, "adv_slider_p2", value = FALSE)
      # T
      # 
      updateNumericInput(session, "KD", value = 100)
      updateSliderInput(session, "kd_range",value = c(-4, 2), min = -6, max = 6)
      updateMaterialSwitch(session, "adv_slider_kd", value = FALSE)
      
    }
  })
  
  output$downloadplot1 <- downloadHandler(
    filename = function() {'Cxbyligrangerecplot.html'},
    content = function(file) {
      
      htmlwidgets::saveWidget(as_widget(inputCxbyligrangerecplot()), file)
      
    }
  )
  output$downloadplot2 <- downloadHandler(
    filename = function() {'Roccbyrecrangeligplot.html'},
    content = function(file) {
      
      htmlwidgets::saveWidget(as_widget(inputRoccbyrecrangeligplot()), file)
      
    }
  )
  output$downloadplot3 <- downloadHandler(
    filename = function() {'Cxbyligrangekdplot.html'},
    content = function(file) {
      
      htmlwidgets::saveWidget(as_widget(inputCxbyligrangekdplot()), file)
      
    }
  )
  output$downloadplot4 <- downloadHandler(
    filename = function() {'Roccbyrecrangeligplot2.html'},
    content = function(file) {
      
      htmlwidgets::saveWidget(as_widget(inputRoccbyrecrangeligplot2()), file)
      
    }
  )
  output$downloadplot5 <- downloadHandler(
    filename = function() {'Time_to_equilibrium_plot.html'},
    content = function(file) {
      
      htmlwidgets::saveWidget(as_widget(inputTime_to_equilibrium_plot()), file)
      
    }
  )
  
  output$plots <- renderUI( {
    fluidRow(
      
      column( 
        width=12,
        style = "background-color: #f0f0f0;",
        column(
          width = 12,
          div(
            style = "padding:5px;background-color: #293887;margin:0px;height:75px;",
            
            div(
              HTML(paste("Concentration", em(input$receptor_name), "- ", em(input$ligand_name), "complex (pM) at a fixed K<sub>D</sub> of", em(input$KD), "pM")), 
              style = "margin-top: 20px; font-size: 16px; margin-bottom: 15px; color:white;
                  font-style: Lato; font-weight:bold;"
            )
          )
        )
      ),
      column( 
        width=12,
        style = "background-color: #f0f0f0;",
        column(
          width = 12,
          
          div(
            style = "padding:10px;background-color: #FFFFFF;margin:0px;height:465px;",
            
            column(width=6,
                   
                   div(style = "margin-top:5px; margin-left:15px;",
                       downloadButton('downloadplot1', 'Download Graph')
                   ),
                   div(style = "margin-top:5px; margin-left:15px;",
                       
                       plotlyOutput("cxbyligrangerecplot"),
                   )
                   #        #tableOutput("Table1")
            ),
            column(width=6,
                   div(style = "margin-top:25px; margin-left:0px; margin-bottom:5px",
                       dataTableOutput("Table1")
                   )
                   #tableOutput("Table1")
                   
            )
            
          )
        )
      ),
      #FIRST PLOT
      # column( 
      #   width=12,
      #   style = "background-color: #f0f0f0;",
      #   column(
      #     width = 12,
      #     
      #     div(
      #       style = "padding:5px;background-color: #FFFFFF;margin:0px;height:450px;",
      #       
      #         column(width=6,
      # 
      #                div(style = "margin-top:15px; margin-left:15px;display:inline-block",
      #                 plotlyOutput("cxbyligrangerecplot")
      #                 
      #                )
      #         #        #tableOutput("Table1")
      #         ),
      #         column(width=6,
      #                div(style = "margin-top:15px; margin-left:0px;",
      #                 dataTableOutput("Table1")
      #                )
      #                #tableOutput("Table1")
      #       
      #         )
      #       
      #      )
      #   )
      #   ),
      
      #SECOND PLOT
      column( 
        width=12,
        style = "background-color: #f0f0f0;",
        column(
          width = 12,
          div(
            style = "padding:5px;background-color: #293887;margin:0px;height:75px;",
            
            div(
              HTML(paste("Fractional occupancy of ", em(input$ligand_name), " at a fixed K<sub>D</sub> of ", em(input$KD), "pM")), 
              style = "margin-top: 20px; font-size: 16px; margin-bottom: 15px; color:white;
                  font-style: Lato; font-weight:bold;"
            )
          )
        )
      ),
      column( 
        width=12,
        style = "background-color: #f0f0f0;",
        column(
          width = 12,
          
          div(
            style = "padding:5px;background-color: #FFFFFF;margin:0px;height:450px;",
            
            column(width=6,
                   div(style = "margin-top:5px; margin-left:15px;",
                       downloadButton('downloadplot2', 'Download Graph')
                   ),
                   
                   div(style = "margin-top:5px; margin-left:15px;",
                       plotlyOutput("roccbyrecrangeligplot"),
                   )
                   #        #tableOutput("Table1")
            ),
            column(width=6,
                   div(style = "margin-top:25px; margin-left:0px;",
                       dataTableOutput("Table2")
                   )
                   #tableOutput("Table1")
                   
            )
            
          )
        )
      ),
      #THIRD PLOT
      column( 
        width=12,
        style = "background-color: #f0f0f0;",
        column(
          width = 12,
          div(
            style = "padding:5px;background-color: #293887;margin:0px;height:75px;",
            
            div(
              # HTML(paste("Concentration", em(input$receptor_name), " - ", em(input$ligand_name),  "complex (pM) at a fixed", em(input$ligand_name),  "concentration of", 
              #      em(input$c_total_ligand_start) ,input$p2unit)), 
              HTML("Concentration", input$receptor_name, " - ", input$ligand_name,  "complex (", "pM", ") in function of K<sub>D</sub> at fixed concentration of ", input$ligand_name), 
              style = "margin-top: 20px; font-size: 16px; margin-bottom: 15px; color:white;
                  font-style: Lato; font-weight:bold;"
            )
          )
        )
      ),
      column( 
        width=12,
        style = "background-color: #f0f0f0;",
        column(
          width = 12,
          
          div(
            style = "padding:5px;background-color: #FFFFFF;margin:0px;height:450px;",
            
            column(width=6,
                   
                   div(style = "margin-top:5px; margin-left:15px;",
                       downloadButton('downloadplot3', 'Download Graph')
                   ),
                   div(style = "margin-top:5px; margin-left:15px;",
                       plotlyOutput("cxbyligrangekdplot"),
                   )
                   #        #tableOutput("Table1")
            ),
            column(width=6,
                   div(style = "margin-top:25px; margin-left:0px;",
                       dataTableOutput("Table3")
                   )
                   #tableOutput("Table1")
                   
            )
            
          )
        )
      ),
      #FOURTH PLOT
      column( 
        width=12,
        style = "background-color: #f0f0f0;",
        column(
          width = 12,
          div(
            style = "padding:5px;background-color: #293887;margin:0px;height:75px;",
            
            div(
              HTML(paste("Fractional occupancy of", em(input$ligand_name),"at a fixed",  em(input$ligand_name), "concentration of", 
                         em(input$c_total_ligand_start) ,input$p2unit_metric)), 
              style = "margin-top: 20px; font-size: 16px; margin-bottom: 15px; color:white;
                  font-style: Lato; font-weight:bold;"
            )
          )
        )
      ),
      column( 
        width=12,
        style = "background-color: #f0f0f0;",
        column(
          width = 12,
          
          div(
            style = "padding:5px;background-color: #FFFFFF;margin:0px;height:450px;",
            
            column(width=6,
                   
                   div(style = "margin-top:5px; margin-left:15px;",
                       downloadButton('downloadplot4', 'Download Graph')
                   ),
                   div(style = "margin-top:5px; margin-left:15px;",
                       plotlyOutput("roccbyrecrangeligplot2"),
                   )
                   #        #tableOutput("Table1")
            ),
            column(width=6,
                   div(style = "margin-top:25px; margin-left:0px;",
                       dataTableOutput("Table4")
                   )
                   #tableOutput("Table1")
                   
            )
            
          )
        )
      ),
      #fifth plot
      column( 
        width=12,
        style = "background-color: #f0f0f0;",
        column(
          width = 12,
          div(
            style = "padding:5px;background-color: #293887;margin:0px;height:75px;",
            
            div(
              HTML("Time to equilibrium"), 
              style = "margin-top: 20px; font-size: 16px; margin-bottom: 15px; color:white;
                  font-style: Lato; font-weight:bold;"
            )
          )
        )
      ),
      column( 
        width=12,
        style = "background-color: #f0f0f0;",
        column(
          width = 12,
          
          div(
            style = "padding:5px;background-color: #FFFFFF;margin:0px;height:450px;",
            
            column(width=12,
                   shinyjs::hidden(
                     div(style = "margin-top:5px; margin-left:15px;",
                         downloadButton('downloadplot5', 'Download Graph')
                     )
                   ),
                   div(style = "margin-top:5px; margin-left:15px;",
                       plotlyOutput("time_to_equilibrium_plot"),
                   )
                   #        #tableOutput("Table1")
            )
            
          )
        )
      )
      
      # style = "background-color: #ffffff",
      # box(width=12,
      #     style = "background-color: #f0f0f0",
      #     fluidRow(
      #       # box(width=12,
      #       #     #row(width=12,
      #       #     style = "background-color: #293887",
      #       #     div(HTML("Concentration", input$receptor_name, "- ", input$ligand_name, "complex (pM) for a fixed K<sub>D</sub> of", input$KD, input$unit), style = "margin-top: 20px; font-size: 14px; margin-bottom: 20px; color: #FFFFFF; font-style: Lato;")
      #       # ),
      #       box(width=12
      #         # column(width=6,
      #         #        plotlyOutput("cxbyligrangerecplot", width = "100%")
      #         #        #tableOutput("Table1")
      #         # ),
      #         # column(width=6,
      #         #        dataTableOutput("Table1")
      #         #        #tableOutput("Table1")
      #         # )
      #         #)
      #       )
      #     )),
      
      # box(width=12,
      #     style = "background-color: #f0f0f0",
      #     fluidRow(
      #       box(width=12,
      #           #row(width=12,
      #           style = "background-color: #293887",
      #           div(HTML("Fractional occupancy of ", input$ligand_name , " in function of concentration of ", input$ligand_name , " at a fixed K<sub>D</sub> of ", input$KD, input$unit), style = "margin-top: 20px; font-size: 14px; margin-bottom: 20px; color: #FFFFFF; font-style: Lato;"),
      #           #str(input$unit)
      #           ),
      #       box(width=12,
      #         column(width=6,
      #                plotlyOutput("roccbyrecrangeligplot", width = "100%")
      #         ),
      #         column(width=6,
      #                #tableOutput("Table2"),
      #                dataTableOutput("Table2")
      #         )
      #         #)
      #       )
      #     )),
      
      # box(width=12,
      #     style = "background-color: #f0f0f0",
      #     fluidRow(
      #       box(width=12,
      #           #row(width=12,
      #           style = "background-color: #293887",
      #           div(HTML("Concentration", input$receptor_name, " - ", input$ligand_name,  "complex (pM) for a fixed", input$ligand_name,  "concentration of", 
      #                    input$c_total_ligand ,input$unit), style = "margin-top: 20px; font-size: 14px; margin-bottom: 20px; color: #FFFFFF; font-style: Lato;")
      #       ),
      #       box(width=12,
      #         column(width=6,
      #                plotlyOutput("cxbyligrangekdplot", width = "100%")
      #         ),
      #         column(width=6,
      #                dataTableOutput("Table3")
      #         )
      #         #)
      #       )
      #     )),
      
      # box(width=12,
      #     style = "background-color: #f0f0f0",
      #     fluidRow(
      #       box(width=12,
      #           #row(width=12,
      #           style = "background-color: #293887",
      #           div(HTML("Fractional occupancy of", input$ligand_name,  "in function of ", "K<sub>D</sub> at fixed concentration of", input$ligand_name), style = "margin-top: 20px; font-size: 14px; margin-bottom: 20px; color: #FFFFFF; font-style: Lato;")
      #       ),
      #       box(width=12,
      #         column(width=6,
      #                plotlyOutput("roccbyrecrangeligplot2", width = "100%")
      #         ),
      #         column(width=6,
      #                dataTableOutput("Table4")
      #         )
      #         #)
      #       )
      #     )),
      
      # box(width=12,
      #     style = "background-color: #f0f0f0",
      #     fluidRow(
      #       box(width=12,
      #           #row(width=12,
      #           style = "background-color: #293887",
      #           div(HTML("Time to equilibrium"), style = "margin-top: 20px; font-size: 14px; margin-bottom: 20px; color: #FFFFFF; font-style: Lato;")
      #       ),
      #       box(width=12,
      #         column(width=12,
      #                plotlyOutput("time_to_equilibrium_plot")
      #         )
      #         #)
      #       )
      #     ) # end fluid row
      # ) # end box
    )
  })
  
  #switch between tab
  observeEvent (c(input$backToTop2, input$switchTab, input$backToTop), { 
    
    if ( input$switchTab){
      
      shinyjs::hide ("switchTab")
      shinyjs::show ("backToTop2")
    }
    
    
    req(input$backToTop2)
    if( input$backToTop2){
      
      shinyjs::show("switchTab")
      shinyjs::hide ("backToTop2")
    }
    
    
    # {
    #   shinyjs::hide("switchTab")
    #   shimyjs::show("backtoTop2")
    # }
    # 
    # 
    req(input$backToTop)
    if( input$backToTop){
      
      shinyjs::show("switchTab")
      # shinyjs::hide ("backToTop")
    }
  })
  
  observeEvent(input$switchTab,{
    #req(input$switchTab)
    #switch(input$switchTab,
    # Values =
    # {
    output$plots <- renderUI({
      fluidRow(
        
        style = "background-color: #f0f0f0",
        ## Create plot box
        
        # box(width=12,
        # column(width=12, 
        #        div(
        #          style = "padding:0px; padding-left:0px;background-color: #f0f0f03; 
        #           margin-top:-30px; display:inline-block;
        #           margin-bottom:10px; margin-left:15px",
        #          actionLink("backToTop2", "Back to Binding Curves",
        #                     style ="color:dodgerblue;border-style: none;"))
        # ),
        
        #Plot 1
        column( 
          width=12,
          style = "background-color: #f0f0f0;",
          column(
            width = 12,
            div(
              style = "padding:5px;background-color: #293887;margin:0px;height:75px;",
              
              div(
                HTML(input$receptor_name, " - ", input$ligand_name, " in complex (pM) following sample dilution"), 
                style = "margin-top: 20px; font-size: 16px; margin-bottom: 15px; color:white;
                  font-style: Lato; font-weight:bold;"
              )
            )
          )
        ),
        column( 
          width=12,
          style = "background-color: #f0f0f0;",
          column(
            width = 12,
            
            div(
              style = "padding:5px;background-color: #FFFFFF;margin:0px;height:450px;",
              
              column(width=6,
                     
                     div(style = "margin-top:15px; margin-left:15px;",
                         plotlyOutput("cxdilutionplot"),
                     )
                     #        #tableOutput("Table1")
              ),
              column(width=6,
                     div(style = "margin-top:15px; margin-left:0px;",
                         dataTableOutput("dilutionTable1")
                     )
                     #tableOutput("Table1")
                     
              )
              
            )
          )
        ),
        
        #Plot 2
        column( 
          width=12,
          style = "background-color: #f0f0f0;",
          column(
            width = 12,
            div(
              style = "padding:5px;background-color: #293887;margin:0px;height:75px;",
              
              div(
                HTML("Fractional occupancy of  ", input$ligand_name, " following sample dilution"), 
                style = "margin-top: 20px; font-size: 16px; margin-bottom: 15px; color:white;
                  font-style: Lato; font-weight:bold;"
              )
            )
          )
        ),
        column( 
          width=12,
          style = "background-color: #f0f0f0;",
          column(
            width = 12,
            
            div(
              style = "padding:5px;background-color: #FFFFFF;margin:0px;height:450px;",
              
              column(width=6,
                     
                     div(style = "margin-top:15px; margin-left:15px;",
                         plotlyOutput("occdilutionplot"),
                     )
                     #        #tableOutput("Table1")
              ),
              column(width=6,
                     div(style = "margin-top:15px; margin-left:0px;",
                         dataTableOutput("dilutionTable2")
                     )
                     #tableOutput("Table1")
                     
              )
              
            )
          )
        ),
        
        #Plot 3
        column( 
          width=12,
          style = "background-color: #f0f0f0;",
          column(
            width = 12,
            div(
              style = "padding:5px;background-color: #293887;margin:0px;height:75px;",
              
              div(
                HTML("Fractional occupancy of  ", input$receptor_name, " following sample dilution"), 
                style = "margin-top: 20px; font-size: 16px; margin-bottom: 15px; color:white;
                  font-style: Lato; font-weight:bold;"
              )
            )
          )
        ),
        column( 
          width=12,
          style = "background-color: #f0f0f0;",
          column(
            width = 12,
            
            div(
              style = "padding:5px;background-color: #FFFFFF;margin:0px;height:450px;",
              
              column(width=6,
                     
                     div(style = "margin-top:15px; margin-left:15px;",
                         plotlyOutput("roccdilutionplot"),
                     )
                     #        #tableOutput("Table1")
              ),
              column(width=6,
                     div(style = "margin-top:15px; margin-left:0px;",
                         dataTableOutput("dilutionTable3")
                     )
                     #tableOutput("Table1")
                     
              )
              
            )
          )
        ),
        
        # box(width=12,
        #     #row(width=12,
        #     style = "background-color: #293887",
        #     div(HTML(input$receptor_name, " - ", input$ligand_name, " in complex (pM) following sample dilution"), 
        #             style = "margin-top: 20px; font-size: 14px; margin-bottom: 20px; color: #FFFFFF; font-style: Lato;")
        # ),
        
        # box(width=12,
        #     column(width=6,
        #            plotlyOutput("cxdilutionplot", width = "100%")
        #     ),
        #     column(width=6,
        #            dataTableOutput("dilutionTable1")
        #     )
        #     #)
        # ),
        
        # box(width=12,
        #     #row(width=12,
        #     style = "background-color: #293887",
        #     div(HTML("Fractional occupancy of  ", input$ligand_name, " following sample dilution"), style = "margin-top: 20px; font-size: 14px; margin-bottom: 20px; color: #FFFFFF; font-style: Lato;")
        # ),
        # 
        # box(width=12,
        #     column(width=6,
        #            plotlyOutput("occdilutionplot", width = "100%")
        #     ),
        #     column(width=6,
        #            dataTableOutput("dilutionTable2")
        #     )
        #     #)
        # ),
        
        
        # box(width=12,
        #     #row(width=12,
        #     style = "background-color: #293887",
        #     div(HTML("Fractional occupancy of  ", input$receptor_name, " following sample dilution"), style = "margin-top: 20px; font-size: 14px; margin-bottom: 20px; color: #FFFFFF; font-style: Lato;")
        # ),
        # 
        # box(width=12,
        #     column(width=6,
        #            plotlyOutput("roccdilutionplot", width = "100%")
        #     ),
        #     column(width=6,
        #            dataTableOutput("dilutionTable3")
        #     )
        #     #)
        # ),
        
        column(width=12, actionLink("backToTop", "Back to Binding Curves", style = "color:dodgerblue; margin-left:20px"))
      ) ## end fluid row
    })
    #}
    #)
  })
  
  # output$plots = renderUI({
  #   initialPlots()
  # })
  
  
  observeEvent(input$backToTop, {
    #input$switchTab <- 0
    #req(input$backToTop)
    #switch(input$backToTop,
    #Values = 
    #{
    output$plots <- renderUI( {
      fluidRow(
        
        #Plot 1
        column( 
          width=12,
          style = "background-color: #f0f0f0;",
          column(
            width = 12,
            div(
              style = "padding:5px;background-color: #293887;margin:0px;height:75px;",
              
              div(
                HTML("Concentration", input$receptor_name, "- ", input$ligand_name, "complex (pM) for a fixed K <sub> D </sub> of", input$KD,  " ","pM"), 
                style = "margin-top: 20px; font-size: 16px; margin-bottom: 15px; color:white;
                  font-style: Lato; font-weight:bold;"
              )
            )
          )
        ),
        column( 
          width=12,
          style = "background-color: #f0f0f0;",
          column(
            width = 12,
            
            div(
              style = "padding:5px;background-color: #FFFFFF;margin:0px;height:450px;",
              
              column(width=6,
                     div(style = "margin-top:5px; margin-left:15px;",
                         downloadButton('downloadplot1', 'Download Graph')
                     ),
                     div(style = "margin-top:5px; margin-left:15px;",
                         plotlyOutput("cxbyligrangerecplot"),
                     )
                     #        #tableOutput("Table1")
              ),
              column(width=6,
                     div(style = "margin-top:25px; margin-left:0px;",
                         dataTableOutput("Table1")
                     )
                     #tableOutput("Table1")
                     
              )
              
            )
          )
        ),
        
        #Plot 2
        column( 
          width=12,
          style = "background-color: #f0f0f0;",
          column(
            width = 12,
            div(
              style = "padding:5px;background-color: #293887;margin:0px;height:75px;",
              
              div(
                HTML("Fractional occupancy of ", input$ligand_name , " in function of concentration of ", input$ligand_name , " at a fixed K <sub> D </sub> of ", input$KD,  " ",input$unit), 
                style = "margin-top: 20px; font-size: 16px; margin-bottom: 15px; color:white;
                  font-style: Lato; font-weight:bold;"
              )
            )
          )
        ),
        column( 
          width=12,
          style = "background-color: #f0f0f0;",
          column(
            width = 12,
            
            div(
              style = "padding:5px;background-color: #FFFFFF;margin:0px;height:450px;",
              
              column(width=6,
                     
                     div(style = "margin-top:5px; margin-left:15px;",
                         downloadButton('downloadplot2', 'Download Graph')
                     ),
                     div(style = "margin-top:5px; margin-left:15px;",
                         plotlyOutput("roccbyrecrangeligplot"),
                     )
                     #        #tableOutput("Table1")
              ),
              column(width=6,
                     div(style = "margin-top:25px; margin-left:0px;",
                         dataTableOutput("Table2")
                     )
                     #tableOutput("Table1")
                     
              )
              
            )
          )
        ),
        #Plot 3
        column( 
          width=12,
          style = "background-color: #f0f0f0;",
          column(
            width = 12,
            div(
              style = "padding:5px;background-color: #293887;margin:0px;height:75px;",
              
              div(
                # HTML("Concentration", input$receptor_name, " - ", input$ligand_name,  "complex (", "pM", ") for a fixed", input$ligand_name, "concentration"), 
                HTML("Concentration", input$receptor_name, " - ", input$ligand_name,  "complex (", "pM", ") in function of K<sub>D</sub> at fixed concentration of ", input$ligand_name), 
                style = "margin-top: 20px; font-size: 16px; margin-bottom: 15px; color:white;
                  font-style: Lato; font-weight:bold;"
              )
            )
          )
        ),
        column( 
          width=12,
          style = "background-color: #f0f0f0;",
          column(
            width = 12,
            
            div(
              style = "padding:5px;background-color: #FFFFFF;margin:0px;height:450px;",
              
              column(width=6,
                     
                     div(style = "margin-top:5px; margin-left:15px;",
                         downloadButton('downloadplot3', 'Download Graph')
                     ),
                     div(style = "margin-top:5px; margin-left:15px;",
                         plotlyOutput("cxbyligrangekdplot"),
                     )
                     #        #tableOutput("Table1")
              ),
              column(width=6,
                     div(style = "margin-top:25px; margin-left:0px;",
                         dataTableOutput("Table3")
                     )
                     #tableOutput("Table1")
                     
              )
              
            )
          )
        ),
        #Plot 4
        column( 
          width=12,
          style = "background-color: #f0f0f0;",
          column(
            width = 12,
            div(
              style = "padding:5px;background-color: #293887;margin:0px;height:75px;",
              
              div(
                HTML("Fractional occupancy of", input$ligand_name,  "in function of ", "K<sub>D</sub> at fixed concentration of", input$ligand_name), 
                style = "margin-top: 20px; font-size: 16px; margin-bottom: 15px; color:white;
                  font-style: Lato; font-weight:bold;"
              )
            )
          )
        ),
        column( 
          width=12,
          style = "background-color: #f0f0f0;",
          column(
            width = 12,
            
            div(
              style = "padding:5px;background-color: #FFFFFF;margin:0px;height:450px;",
              
              column(width=6,
                     
                     div(style = "margin-top:5px; margin-left:15px;",
                         downloadButton('downloadplot4', 'Download Graph')
                     ),
                     div(style = "margin-top:5px; margin-left:15px;",
                         plotlyOutput("roccbyrecrangeligplot2"),
                     )
                     #        #tableOutput("Table1")
              ),
              column(width=6,
                     div(style = "margin-top:25px; margin-left:0px;",
                         dataTableOutput("Table4")
                     )
                     #tableOutput("Table1")
                     
              )
              
            )
          )
        ),
        
        #Plot 5
        column( 
          width=12,
          style = "background-color: #f0f0f0;",
          column(
            width = 12,
            div(
              style = "padding:5px;background-color: #293887;margin:0px;height:75px;",
              
              div(
                HTML("Time to equilibrium"), 
                style = "margin-top: 20px; font-size: 16px; margin-bottom: 15px; color:white;
                  font-style: Lato; font-weight:bold;"
              )
            )
          )
        ),
        column( 
          width=12,
          style = "background-color: #f0f0f0;",
          column(
            width = 12,
            
            div(
              style = "padding:5px;background-color: #FFFFFF;margin:0px;height:450px;",
              
              column(width=12,
                     
                     shinyjs::hidden(
                       div(style = "margin-top:5px; margin-left:15px;",
                           downloadButton('downloadplot5', 'Download Graph')
                       )
                     ),
                     div(style = "margin-top:5px; margin-left:15px;",
                         plotlyOutput("time_to_equilibrium_plot"),
                     )
                     #        #tableOutput("Table1")
              )
              
            )
          )
        )
        # box(width=12,
        #     fluidRow(
        #       box(width=12,
        #           #row(width=12,
        #           style = "background-color: #293887",
        #           div(HTML("Concentration", input$receptor_name, "- ", input$ligand_name, "complex (pM) for a fixed K <sub> D </sub> of", input$KD,  " ",input$unit), style = "margin-top: 20px; font-size: 14px; margin-bottom: 20px; color: #FFFFFF; font-style: Lato;")
        #       ),
        #       box(width=12,
        #         column(width=6,
        #                plotlyOutput("cxbyligrangerecplot", width = "100%")
        #         ),
        #         column(width=6,
        #                dataTableOutput("Table1")
        #         )
        #         #)
        #       )
        #     )),
        
        # box(width=12,  
        #     fluidRow(
        #       box(width=12,
        #           #row(width=12,
        #           style = "background-color: #293887",
        #           div(HTML("Fractional occupancy of ", input$ligand_name , " in function of concentration of ", input$ligand_name , " at a fixed K <sub> D </sub> of ", input$KD,  " ",input$unit), style = "margin-top: 20px; font-size: 14px; margin-bottom: 20px; color: #FFFFFF; font-style: Lato;")
        #       ),
        #       box(width=12,
        #         column(width=6,
        #                plotlyOutput("roccbyrecrangeligplot", width = "100%")
        #         ),
        #         column(width=6,
        #                dataTableOutput("Table2")
        #         )
        #         #)
        #       )
        #     )),
        # 
        # box(width=12, 
        #     fluidRow(
        #       box(width=12,
        #           #row(width=12,
        #           style = "background-color: #293887",
        #           div(HTML("Concentration", input$receptor_name, " - ", input$ligand_name,  "complex (", input$unit, ") for a fixed", input$ligand_name, "concentration"), style = "margin-top: 20px; font-size: 14px; margin-bottom: 20px; color: #FFFFFF; font-style: Lato;")
        #       ),
        #       box(width=12,
        #         column(width=6,
        #                plotlyOutput("cxbyligrangekdplot", width = "100%")
        #         ),
        #         column(width=6,
        #                dataTableOutput("Table3")
        #         )
        #         #)
        #       )
        #     )),
        
        # box(width=12,
        # fluidRow(
        #   box(width=12,
        #       #row(width=12,
        #       style = "background-color: #293887",
        #       div(HTML("Fractional occupancy of", input$ligand_name,  "in function of ", "K<sub>D</sub> at fixed concentration of", input$ligand_name), style = "margin-top: 20px; font-size: 14px; margin-bottom: 20px; color: #FFFFFF; font-style: Lato;")
        #   ),
        #   box(width=12,
        #     column(width=6,
        #            plotlyOutput("roccbyrecrangeligplot2", width = "100%")
        #     ),
        #     column(width=6,
        #            dataTableOutput("Table4")
        #     )
        #     #)
        #   )
        # )),
        
        # box(width=12,
        #     fluidRow(
        #       box(width=12,
        #           #row(width=12,
        #           style = "background-color: #293887",
        #           div(HTML("Time to equilibrium"), style = "margin-top: 20px; font-size: 14px; margin-bottom: 20px; color: #FFFFFF; font-style: Lato;")
        #       ),
        #       box(width=12,
        #         column(width=12,
        #                plotlyOutput("time_to_equilibrium_plot")
        #         )#,
        #         # column(width=6,
        #         #        tableOutput("Table5")
        #         # )
        #         #)
        #       )
        #     ) # end fluid row
        # ) # end box
      )
    })
    #}
    #)
  })
  
  observeEvent(input$backToTop2, {
    #input$switchTab <- 0
    #req(input$backToTop)
    #switch(input$backToTop,
    #Values = 
    #{
    output$plots <- renderUI( {
      fluidRow(
        
        #Plot 1
        column( 
          width=12,
          style = "background-color: #f0f0f0;",
          column(
            width = 12,
            div(
              style = "padding:5px;background-color: #293887;margin:0px;height:75px;",
              
              div(
                HTML("Concentration", input$receptor_name, "- ", input$ligand_name, "complex (pM) for a fixed K <sub> D </sub> of", input$KD,  " ","pM"), 
                style = "margin-top: 20px; font-size: 16px; margin-bottom: 15px; color:white;
                  font-style: Lato; font-weight:bold;"
              )
            )
          )
        ),
        column( 
          width=12,
          style = "background-color: #f0f0f0;",
          column(
            width = 12,
            
            div(
              style = "padding:5px;background-color: #FFFFFF;margin:0px;height:450px;",
              
              column(width=6,
                     
                     div(style = "margin-top:5px; margin-left:15px;",
                         downloadButton('downloadplot1', 'Download Graph')
                     ),
                     div(style = "margin-top:5px; margin-left:15px;",
                         plotlyOutput("cxbyligrangerecplot"),
                     )
                     #        #tableOutput("Table1")
              ),
              column(width=6,
                     div(style = "margin-top:25px; margin-left:0px;",
                         dataTableOutput("Table1")
                     )
                     #tableOutput("Table1")
                     
              )
              
            )
          )
        ),
        
        #Plot 2
        column( 
          width=12,
          style = "background-color: #f0f0f0;",
          column(
            width = 12,
            div(
              style = "padding:5px;background-color: #293887;margin:0px;height:75px;",
              
              div(
                HTML("Fractional occupancy of ", input$ligand_name , " in function of concentration of ", input$ligand_name , " at a fixed K <sub> D </sub> of ", input$KD,  " ","pM"), 
                style = "margin-top: 20px; font-size: 16px; margin-bottom: 15px; color:white;
                  font-style: Lato; font-weight:bold;"
              )
            )
          )
        ),
        column( 
          width=12,
          style = "background-color: #f0f0f0;",
          column(
            width = 12,
            
            div(
              style = "padding:5px;background-color: #FFFFFF;margin:0px;height:450px;",
              
              column(width=6,
                     
                     div(style = "margin-top:5px; margin-left:15px;",
                         downloadButton('downloadplot2', 'Download Graph')
                     ),
                     div(style = "margin-top:5px; margin-left:15px;",
                         plotlyOutput("roccbyrecrangeligplot"),
                     )
                     #        #tableOutput("Table1")
              ),
              column(width=6,
                     div(style = "margin-top:25px; margin-left:0px;",
                         dataTableOutput("Table2")
                     )
                     #tableOutput("Table1")
                     
              )
              
            )
          )
        ),
        
        #Plot 3
        column( 
          width=12,
          style = "background-color: #f0f0f0;",
          column(
            width = 12,
            div(
              style = "padding:5px;background-color: #293887;margin:0px;height:75px;",
              
              div(
                HTML("Concentration", input$receptor_name, " - ", input$ligand_name,  "complex (", "pM", ") in function of K<sub>D</sub> at fixed concentration of ", input$ligand_name), 
                style = "margin-top: 20px; font-size: 16px; margin-bottom: 15px; color:white;
                  font-style: Lato; font-weight:bold;"
              )
            )
          )
        ),
        column( 
          width=12,
          style = "background-color: #f0f0f0;",
          column(
            width = 12,
            
            div(
              style = "padding:5px;background-color: #FFFFFF;margin:0px;height:450px;",
              
              column(width=6,
                     
                     div(style = "margin-top:5px; margin-left:15px;",
                         downloadButton('downloadplot3', 'Download Graph')
                     ),
                     div(style = "margin-top:5px; margin-left:15px;",
                         plotlyOutput("cxbyligrangekdplot"),
                     )
                     #        #tableOutput("Table1")
              ),
              column(width=6,
                     div(style = "margin-top:25px; margin-left:0px;",
                         dataTableOutput("Table3")
                     )
                     #tableOutput("Table1")
                     
              )
              
            )
          )
        ),
        
        #Plot 4
        column( 
          width=12,
          style = "background-color: #f0f0f0;",
          column(
            width = 12,
            div(
              style = "padding:5px;background-color: #293887;margin:0px;height:75px;",
              
              div(
                HTML("Fractional occupancy of", input$ligand_name,  "in function of ", "K<sub>D</sub> at fixed concentration of", input$ligand_name), 
                style = "margin-top: 20px; font-size: 16px; margin-bottom: 15px; color:white;
                  font-style: Lato; font-weight:bold;"
              )
            )
          )
        ),
        column( 
          width=12,
          style = "background-color: #f0f0f0;",
          column(
            width = 12,
            
            div(
              style = "padding:5px;background-color: #FFFFFF;margin:0px;height:450px;",
              
              column(width=6,
                     
                     div(style = "margin-top:5px; margin-left:15px;",
                         downloadButton('downloadplot4', 'Download Graph')
                     ),
                     div(style = "margin-top:5px; margin-left:15px;",
                         plotlyOutput("roccbyrecrangeligplot2"),
                     )
                     #        #tableOutput("Table1")
              ),
              column(width=6,
                     div(style = "margin-top:25px; margin-left:0px;",
                         dataTableOutput("Table4")
                     )
                     #tableOutput("Table1")
                     
              )
              
            )
          )
        ),
        
        #Plot 5
        column( 
          width=12,
          style = "background-color: #f0f0f0;",
          column(
            width = 12,
            div(
              style = "padding:5px;background-color: #293887;margin:0px;height:75px;",
              
              div(
                HTML("Time to equilibrium"), 
                style = "margin-top: 20px; font-size: 16px; margin-bottom: 15px; color:white;
                  font-style: Lato; font-weight:bold;"
              )
            )
          )
        ),
        column( 
          width=12,
          style = "background-color: #f0f0f0;",
          column(
            width = 12,
            
            div(
              style = "padding:5px;background-color: #FFFFFF;margin:0px;height:450px;",
              
              column(width=12,
                     
                     shinyjs::hidden(
                       div(style = "margin-top:5px; margin-left:15px;",
                           downloadButton('downloadplot5', 'Download Graph')
                       )
                     ),
                     div(style = "margin-top:5px; margin-left:15px;",
                         plotlyOutput("time_to_equilibrium_plot"),
                     )
                     #        #tableOutput("Table1")
              )
              
            )
          )
        )
        # box(width=12,
        #     fluidRow(
        #       box(width=12,
        #           #row(width=12,
        #           style = "background-color: #293887",
        #           div(HTML("Concentration", input$receptor_name, "- ", input$ligand_name, "complex (pM) for a fixed K <sub> D </sub> of", input$KD,  " ",input$unit), style = "margin-top: 20px; font-size: 14px; margin-bottom: 20px; color: #FFFFFF; font-style: Lato;")
        #       ),
        #       box(width=12,
        #           column(width=6,
        #                  plotlyOutput("cxbyligrangerecplot", width = "100%")
        #           ),
        #           column(width=6,
        #                  dataTableOutput("Table1")
        #           )
        #           #)
        #       )
        #     )),
        
        # box(width=12,  
        #     fluidRow(
        #       box(width=12,
        #           #row(width=12,
        #           style = "background-color: #293887",
        #           div(HTML("Fractional occupancy of ", input$ligand_name , " in function of concentration of ", input$ligand_name , " at a fixed K <sub> D </sub> of ", input$KD,  " ",input$unit), style = "margin-top: 20px; font-size: 14px; margin-bottom: 20px; color: #FFFFFF; font-style: Lato;")
        #       ),
        #       box(width=12,
        #           column(width=6,
        #                  plotlyOutput("roccbyrecrangeligplot", width = "100%")
        #           ),
        #           column(width=6,
        #                  dataTableOutput("Table2")
        #           )
        #           #)
        #       )
        #     )),
        
        # box(width=12, 
        #     fluidRow(
        #       box(width=12,
        #           #row(width=12,
        #           style = "background-color: #293887",
        #           div(HTML("Concentration", input$receptor_name, " - ", input$ligand_name,  "complex (", input$unit, ") for a fixed", input$ligand_name, "concentration"), style = "margin-top: 20px; font-size: 14px; margin-bottom: 20px; color: #FFFFFF; font-style: Lato;")
        #       ),
        #       box(width=12,
        #           column(width=6,
        #                  plotlyOutput("cxbyligrangekdplot", width = "100%")
        #           ),
        #           column(width=6,
        #                  dataTableOutput("Table3")
        #           )
        #           #)
        #       )
        #     )),
        
        # box(width=12,
        #     fluidRow(
        #       box(width=12,
        #           #row(width=12,
        #           style = "background-color: #293887",
        #           div(HTML("Fractional occupancy of", input$ligand_name,  "in function of ", "K<sub>D</sub> at fixed concentration of", input$ligand_name), style = "margin-top: 20px; font-size: 14px; margin-bottom: 20px; color: #FFFFFF; font-style: Lato;")
        #       ),
        #       box(width=12,
        #           column(width=6,
        #                  plotlyOutput("roccbyrecrangeligplot2", width = "100%")
        #           ),
        #           column(width=6,
        #                  dataTableOutput("Table4")
        #           )
        #           #)
        #       )
        #     )),
        
        # box(width=12,
        #     fluidRow(
        #       box(width=12,
        #           #row(width=12,
        #           style = "background-color: #293887",
        #           div(HTML("Time to equilibrium"), style = "margin-top: 20px; font-size: 14px; margin-bottom: 20px; color: #FFFFFF; font-style: Lato;")
        #       ),
        #       box(width=12,
        #           column(width=12,
        #                  plotlyOutput("time_to_equilibrium_plot")
        #           )#,
        #           # column(width=6,
        #           #        tableOutput("Table5")
        #           # )
        #           #)
        #       )
        #     ) # end fluid row
        # ) # end box
      )
    })
    #}
    #)
  })
  
  
  output$bindingPlots <- renderUI( {
    fluidRow(
      
      box(width=12,
          fluidRow(
            box(width=12,
                #row(width=12,
                style = "background-color: #293887",
                div(HTML("Concentration", input$receptor_name, "- ", input$ligand_name, "complex (", input$unit, ") for a fixed K<sub>D</sub> of", input$KD, "pM"), style = "margin-top: 20px; font-size: 14px; margin-bottom: 20px; color: #FFFFFF; font-style: Lato;")
            ),
            fluidRow(width=12,
                     column(width=12,
                            plotlyOutput("cxbyligrangerecplot", width = "100%"),
                            dataTableOutput("Table1")
                     )
            )
          )),
      
      box(width=12,  
          fluidRow(
            box(width=12,
                #row(width=12,
                style = "background-color: #293887",
                div(HTML(input$receptor_name, "Fractional occupancy for fixed K<sub>D</sub>"), style = "margin-top: 20px; font-size: 14px; margin-bottom: 20px; color: #FFFFFF; font-style: Lato;")
            ),
            box(width=12,
                column(width=6,
                       plotlyOutput("roccbyrecrangeligplot", width = "100%")
                ),
                column(width=6,
                       dataTableOutput("Table2")
                )
                #)
            )
          )),
      
      box(width=12, 
          fluidRow(
            box(width=12,
                #row(width=12,
                style = "background-color: #293887",
                div(HTML("Concentration", input$receptor_name,  "- ", input$ligand_name, "complex (pM) for a fixed", input$ligand_name, "concentration"), style = "margin-top: 20px; font-size: 14px; margin-bottom: 20px; color: #FFFFFF; font-style: Lato;")
            ),
            box(width=12,
                column(width=6,
                       plotlyOutput("cxbyligrangekdplot", width = "100%")
                ),
                column(width=6,
                       dataTableOutput("Table3")
                )
                #)
            )
          )),
      
      box(width=12,
          fluidRow(
            box(width=12,
                #row(width=12,
                style = "background-color: #293887",
                div(HTML("Fractional occupancy of", input$receptor_name, "in function of concentration of", input$receptor_name, "and K<sub>D</sub> at fixed concentration of", input$ligand_name), style = "margin-top: 20px; font-size: 14px; margin-bottom: 20px; color: #FFFFFF; font-style: Lato;")
            ),
            box(width=12,
                column(width=6,
                       plotlyOutput("roccbyrecrangeligplot2", width = "100%")
                ),
                column(width=6,
                       dataTableOutput("Table4")
                )
                #)
            )
          )),
      
      box(width=12,
          fluidRow(
            box(width=12,
                #row(width=12,
                style = "background-color: #293887",
                div(HTML("Time to equilibrium"), style = "margin-top: 20px; font-size: 14px; margin-bottom: 20px; color: #FFFFFF; font-style: Lato;")
            ),
            box(width=12,
                column(width=12,
                       plotlyOutput("time_to_equilibrium_plot")
                )
                #)
            )
          ) # end fluid row
      ) # end box
    )
  })
  
  
  output$dilutionPlots <- renderUI({
    fluidRow(
      ## Create plot box
      
      box(width=12, 
          column(width=6,
                 plotlyOutput("cxdilutionplot", width = "100%")
          ),
          column(width=6,
                 dataTableOutput("dilutionTable1")
          )
          #)
      ),
      
      box(width=12,
          column(width=6,
                 plotlyOutput("occdilutionplot", width = "100%")
          ),
          column(width=6,
                 dataTableOutput("dilutionTable2")
          )
          #)
      ),
      
      
      box(width=12,
          column(width=6,
                 plotlyOutput("roccdilutionplot", width = "100%")
          ),
          column(width=6,
                 dataTableOutput("dilutionTable3")
          )
          #)
      )
      
      #column(width=12, actionButton("backToTop", "Back to the top"))
    ) ## end fluid row
  })
  
  # Free Partner 1 - no Competitor (Binding tab)
  observeEvent(input$myconfirmation_submit_binding, {
    if (!is.null (input$myconfirmation_submit_binding) ){
      output$freePartner1 <- renderText({
        isolate ({
          volume <- 1
          kdreclig <- input$KD #input$koff/input$kon*1e12
          kdcoating <- 10^(12-0) 
          #gfun <- function(x) {c(input$c_total_receptor - x[1] - x[2]*x[1]/kdcoating - x[1]*x[3]/kdreclig, input$c_total_coating-x[2]-x[2]*x[1]/kdcoating, input$c_total_ligand-x[3]-x[1]*x[3]/kdreclig)}
          gfun <- function(x) {c(input$c_total_receptor - x[1] - x[2]*x[1]/kdcoating - x[1]*x[3]/kdreclig, 0-x[2]-x[2]*x[1]/kdcoating, input$c_total_ligand-x[3]-x[1]*x[3]/kdreclig)}
          #fred <- multiroot( gfun, start=c(input$c_total_receptor/2,input$c_total_coating/2,input$c_total_ligand/2), positive=TRUE )
          fred <- multiroot( gfun, start=c(input$c_total_receptor/2,0,input$c_total_ligand/2), positive=TRUE )
          c_free_receptor <- fred$root[1]
          c_free_coat <- fred$root[2]
          c_free_ligand <- fred$root[3]
          pc_occ_ligand <- 100*(1-c_free_ligand/input$c_total_ligand)
          #paste("Free ",input$receptor_name," is ",signif(c_free_receptor,digits=3)," pM.",sep="")
          # paste(format(signif(c_free_receptor,digits=3 ), decimal.mark=",")," ", input$unit, sep="")
          paste(format(signif(c_free_receptor,digits=3 ), decimal.mark=",")," (pM)", input$unit, sep="")
          # paste(input$c_total_receptor_start, ",", input$c_total_receptor)
        })
      })
    }
  })
  
  # Free Partner 2 - no Competitor (Binding tab)
  observeEvent(input$myconfirmation_submit_binding, {
    if (!is.null (input$myconfirmation_submit_binding) ){
      output$freePartner2 <- renderText({
        isolate({
          volume <- 1
          kdreclig <- input$KD #input$koff/input$kon*1e12
          kdcoating <- 10^(12-0)
          
          #gfun <- function(x) {c(input$c_total_receptor - x[1] - x[2]*x[1]/kdcoating - x[1]*x[3]/kdreclig, input$c_total_coating-x[2]-x[2]*x[1]/kdcoating, input$c_total_ligand-x[3]-x[1]*x[3]/kdreclig)}
          gfun <- function(x) {c(input$c_total_receptor - x[1] - x[2]*x[1]/kdcoating - x[1]*x[3]/kdreclig, 0-x[2]-x[2]*x[1]/kdcoating, input$c_total_ligand-x[3]-x[1]*x[3]/kdreclig)}
          #fred <- multiroot( gfun, start=c(input$c_total_receptor/2,input$c_total_coating/2,input$c_total_ligand/2), positive=TRUE )
          fred <- multiroot( gfun, start=c(input$c_total_receptor/2,0,input$c_total_ligand/2), positive=TRUE )
          c_free_receptor <- fred$root[1]
          c_free_coat <- fred$root[2]
          c_free_ligand <- fred$root[3]
          pc_occ_ligand <- 100*(1-c_free_ligand/input$c_total_ligand)
          #paste("Free ",input$receptor_name," is ",signif(c_free_receptor,digits=3)," pM.",sep="")
          paste(format(signif(c_free_ligand,digits=3), decimal.mark=",")," (pM)", input$unit,sep="")
        })
      })
    }
  })
  
  # In Complex Concentration (Binding tab)
  observeEvent(input$myconfirmation_submit_binding, {
    if (!is.null (input$myconfirmation_submit_binding) ){
      output$InComplexConcentration <- renderText({
        isolate({
          volume <- 1
          kdreclig <- input$KD #input$koff/input$kon*1e12
          kdcoating <-  10^(12-0)
          
          
          #gfun <- function(x) {c(input$c_total_receptor - x[1] - x[2]*x[1]/kdcoating - x[1]*x[3]/kdreclig, input$c_total_coating-x[2]-x[2]*x[1]/kdcoating, input$c_total_ligand-x[3]-x[1]*x[3]/kdreclig)}
          gfun <- function(x) {c(input$c_total_receptor - x[1] - x[2]*x[1]/kdcoating - x[1]*x[3]/kdreclig, 0-x[2]-x[2]*x[1]/kdcoating, input$c_total_ligand-x[3]-x[1]*x[3]/kdreclig)}
          #fred <- multiroot( gfun, start=c(input$c_total_receptor/2,input$c_total_coating/2,input$c_total_ligand/2), positive=TRUE )
          fred <- multiroot( gfun, start=c(input$c_total_receptor/2,0,input$c_total_ligand/2), positive=TRUE )
          c_free_receptor <- fred$root[1]
          c_free_coat <- fred$root[2]
          c_free_ligand <- fred$root[3]
          c_rlcomplex <- c_free_receptor*c_free_ligand/kdreclig
          pc_occ_ligand <- 100*(1-c_free_ligand/input$c_total_ligand)
          #paste("Free ",input$receptor_name," is ",signif(c_free_receptor,digits=3)," pM.",sep="")
          paste(format(signif(c_rlcomplex,digits=3),decimal.mark=",")," (pM)", input$unit,sep="")
        })
      })
    }
  })
  
  # Fractional Occupancy of partner 1 bound to partner 2 - no competitor (Binding tab)
  observeEvent(input$myconfirmation_submit_binding, {
    if (!is.null (input$myconfirmation_submit_binding) ){
      output$fractionPartner1 <- renderText({
        isolate ({
          volume <- 1
          kdreclig <- input$KD #input$koff/input$kon*1e12
          kdcoating <- 10^(12-0)
          
          
          #gfun <- function(x) {c(input$c_total_receptor - x[1] - x[2]*x[1]/kdcoating - x[1]*x[3]/kdreclig, input$c_total_coating-x[2]-x[2]*x[1]/kdcoating, input$c_total_ligand-x[3]-x[1]*x[3]/kdreclig)}
          gfun <- function(x) {c(input$c_total_receptor - x[1] - x[2]*x[1]/kdcoating - x[1]*x[3]/kdreclig, 0-x[2]-x[2]*x[1]/kdcoating, input$c_total_ligand-x[3]-x[1]*x[3]/kdreclig)}
          #fred <- multiroot( gfun, start=c(input$c_total_receptor/2,input$c_total_coating/2,input$c_total_ligand/2), positive=TRUE )
          fred <- multiroot( gfun, start=c(input$c_total_receptor/2,0,input$c_total_ligand/2), positive=TRUE )
          c_free_receptor <- fred$root[1]
          c_free_coat <- fred$root[2]
          c_free_ligand <- fred$root[3]
          pc_occ_ligand <- 100*(1-c_free_ligand/input$c_total_ligand)
          pc_occ_receptor <- 100*(1-c_free_receptor/input$c_total_receptor)
          paste(format(signif(pc_occ_receptor, digits=3),decimal.mark=","),"% ",sep="")
        })
      })
    }
  })
  
  # Fractional Occupancy of partner 2 bound to partner 1 - no competitor (Binding tab)
  observeEvent(input$myconfirmation_submit_binding, {
    if (!is.null (input$myconfirmation_submit_binding) ){
      output$fractionPartner2 <- renderText({
        isolate({
          volume <- 1
          kdreclig <- input$KD #input$koff/input$kon*1e12
          kdcoating <- 10^(12-0)
          
          
          #gfun <- function(x) {c(input$c_total_receptor - x[1] - x[2]*x[1]/kdcoating - x[1]*x[3]/kdreclig, input$c_total_coating-x[2]-x[2]*x[1]/kdcoating, input$c_total_ligand-x[3]-x[1]*x[3]/kdreclig)}
          gfun <- function(x) {c(input$c_total_receptor - x[1] - x[2]*x[1]/kdcoating - x[1]*x[3]/kdreclig, 0-x[2]-x[2]*x[1]/kdcoating, input$c_total_ligand-x[3]-x[1]*x[3]/kdreclig)}
          #fred <- multiroot( gfun, start=c(input$c_total_receptor/2,input$c_total_coating/2,input$c_total_ligand/2), positive=TRUE )
          fred <- multiroot( gfun, start=c(input$c_total_receptor/2,0,input$c_total_ligand/2), positive=TRUE )
          c_free_receptor <- fred$root[1]
          c_free_coat <- fred$root[2]
          c_free_ligand <- fred$root[3]
          pc_occ_ligand <- 100*(1-c_free_ligand/input$c_total_ligand)
          pc_occ_receptor <- 100*(1-c_free_receptor/input$c_total_receptor)
          paste(format(signif(pc_occ_ligand, digits=3), decimal.mark=","),"%",sep="")
        })
      })
    }
  })
  
  
  # Time to equilibrium (Binding tab)
  output$TimeToEql_old <- renderUI({
    volume <- 1
    atotalreceptor <- input$c_total_receptor*volume
    atotalligand <- input$c_total_ligand*volume
    kd <- input$KD #input$koff/input$kon*1e12
    koffpers <- kon_pM()*1e-12*kd
    time_to_hours1 <- (4*0.693*1/koffpers)/3600
    time_to_hours2 <- (4*0.693*1/(koffpers*input$c_total_receptor/kd))/3600
    time_to_hours3 <- (4*0.693*1/(koffpers*input$c_total_ligand/kd))/3600
    time_to_hours <- min(time_to_hours1,time_to_hours2,time_to_hours3)
    minutes <- time_to_hours * 60
    hours <- minutes %/% 60
    hours_minutes <- minutes %% 60
    paste(signif(hours)," hours ", signif(hours_minutes), " minutes", sep="")
    
    
  })
  
  observeEvent(input$myconfirmation_submit_binding, {
    if (!is.null (input$myconfirmation_submit_binding) ){
      # observeEvent(c(input$constants), {
      output$TimeToEql <- renderUI({
        isolate({
          if (input$constants == "only KD"){
            span ("Insufficent inputs",style = "display:inline-block;font-size: 42px; font-weight:bold")
          } else {   
            # print(get_TimeToEql())
            print ("in else")
            temp = get_TimeToEql()
            
            paste(floor(temp)," hour(s) ", 
                  (temp - floor(temp)) * 60, "min", sep="")
            # # h3(floor(get_TimeToEql()), style = "display:inline-block; font-size: 42px; font-weight:bold"),
            # h3 ("hour(s)", style = "display:inline-block; font-size: 18px; font-weight:bold"),
            # h3 ((get_TimeToEql() - floor(get_TimeToEql())) * 60, style = "display:inline-block;font-size: 42px; font-weight:bold"),
            h3 (textOutput("TimeToEql"), style = "display:inline-block;font-size: 42px; font-weight:bold")
            # h3 ("hr", style = "display:inline-block; font-size: 18px; font-weight:bold")
            
            text1 <- floor(temp)
            
            span(floor(temp), style = "align:left;  margin:0px;font-size: 42px;font-weight: bold; padding:0px", 
                 span ("hr", 
                       style = "color: black; font-size: 18px; "),
                 span( (temp - floor(temp)) * 60, style = "font-size: 42px;font-weight: bold;"),
                 span ("min", 
                       style = "color: black; font-size: 18px; ")
            )
          }
        })
      }) 
    }
  })
  
  # output$TimeToEql <- renderText({
  get_TimeToEql <- reactive({
    
    # if (input$kon == 0 & input$koff== 0)
    # validate(
    #   need(input$kon != 0 & input$koff != 0, paste("Time to equilibrium cannot be plotted when only KD option is selected"))
    # )else {
    
    if(input$constants == "only KD"){
      # if (input$kon == 0 & input$koff== 0) {
      temp = 0
    }else {
      
      if (input$kon == 0 & input$koff == 0){
        updateNumericInput(session, "koff", value = format(1e8, scientific = T))
        updateNumericInput(session, "kon",  value = format(1e6, scientific = T))
        updateNumericInput(session, "kd",  value = format(100, scientific = T))
      }
      volume <- 1
      atotalreceptor <- input$c_total_receptor*volume
      atotalligand <- input$c_total_ligand*volume
      kdreclig <- input$KD #input$koff/input$kon*1e12
      kdcoating <- 10^(12-0) #input$pKdcoat[1]
      gfun <- function(x) {c(input$c_total_receptor - x[1] - x[2]*x[1]/kdcoating - x[1]*x[3]/kdreclig, 0-x[2]-x[2]*x[1]/kdcoating, input$c_total_ligand-x[3]-x[1]*x[3]/kdreclig)}
      fred <- multiroot( gfun, start=c(input$c_total_receptor/2,0/2,input$c_total_ligand/2), positive=TRUE )
      c_free_receptor <- fred$root[1]
      c_free_coat <- fred$root[2]
      c_free_ligand <- fred$root[3]
      pc_occ_ligand <- 100*(1-c_free_ligand/input$c_total_ligand)
      kd <- input$KD #input$koff/input$kon*1e12
      koffpers <- input$kon*1e-12*kd
      time_to_hours1 <- (4*0.693*1/koffpers)/3600
      time_to_hours2 <- (4*0.693*1/(koffpers*input$c_total_receptor/kd))/3600
      time_to_hours3 <- (4*0.693*1/(koffpers*input$c_total_ligand/kd))/3600
      time_to_hours <- min(time_to_hours1,time_to_hours2,time_to_hours3)
      # now the ode bit
      binding_model <- function( Time, State, pars ) {
        with(as.list(c(State,pars)), {
          R <- State[1]  # receptor
          L <- State[2]
          X <- State[3]
          R_ode <- -konperpms*R*L + koffpers*X
          L_ode <- -konperpms*R*L + koffpers*X
          X_ode <- konperpms*R*L - koffpers*X
          return(list(c(R_ode,L_ode,X_ode)))
        })
      }
      inits <- c(R=input$c_total_receptor,L=input$c_total_ligand, X=0)
      pars <- c(konperpms=input$kon*1e-12,koff=koffpers)
      print ("time to hours = ")
      print (time_to_hours)
      seq.times <- seq(from=0, to=2*time_to_hours*3600, by = 2*time_to_hours*36)   # calculate for each minute
      out <- data.frame(ode(y=inits, times=seq.times, func=binding_model, parms=pars, atol=1e-4,rtol=1e-4))
      time.points <- out$time/3600  # convert to hours
      yaxis.points <- 100*out$L/input$c_total_ligand
      ## calculate the time taken to get to 90% of the way there or more
      target_level_90 <- 100-0.9*pc_occ_ligand
      target_level_95 <- 100-0.95*pc_occ_ligand
      target_level_99 <- 100-0.99*pc_occ_ligand
      time_to_90 <- max( out$time[ 100*out$L/input$c_total_ligand>=target_level_90 ]  ) /3600
      time_to_95 <- max( out$time[ 100*out$L/input$c_total_ligand>=target_level_95 ]  ) /3600
      time_to_99 <- max( out$time[ 100*out$L/input$c_total_ligand>=target_level_99 ]  ) /3600
      
      
      temp <- signif(time_to_99, digits=2)
      
      
    }
    # }
    # temp = 1
    return (temp)
  })
  
  convert_units_back_p1 <- function (my.value){
    
    if(input$p1concunit == "molar units"){
      
      if (input$p1unit_molar == "pM"){
        
        fixed_receptor = my.value
        print (paste ("fixed rec:", fixed_receptor))
        print(fixed_receptor)
        
      } else  if (input$p1unit_molar == "uM"){
        
        fixed_receptor = my.value *  1e-6 
        
      } else if (input$p1unit_molar == "fM"){
        
        fixed_receptor = my.value * 1e3 
        
      } else if (input$p1unit_molar == "nM"){
        
        fixed_receptor = my.value * 1e-3
      } else if (input$p1unit_molar == "mM"){
        
        fixed_receptor = my.value * 1e-9
        
      }
      
      
    } else if (input$p1concunit == "metric units"){
      
      #convert receptor back to original units
      if (input$p1unit_metric == "ug/ml"){
        
        fixed_receptor <- my.value*(1/10^12) *(1/1000) * input$mol_weight_p1 * 10^6
        print(fixed_receptor)
        
        # temp = format  (input$c_total_receptor / 1e6 / input$mol_weight_p1 * 10^12 * 1000, scientific = T)
      }else if (input$p1unit_metric == "mg/ml"){
        
        fixed_receptor <- my.value *(1/10^12) *(1/1000) * input$mol_weight_p1 * 10^3
        
        # temp = format  (input$c_total_receptor / 1e3 / input$mol_weight_p1 * 10^12 * 1000, scientific = T)
      }else if (input$p1unit_metric == "ng/ml"){
        
        fixed_receptor <- my.value *(1/10^12) *(1/1000) * input$mol_weight_p1 * 10^9
        
        # temp = format  (input$c_total_receptor / 1e9 / input$mol_weight_p1 * 10^12 * 1000, scientific = T)
        
      }else if (input$p1unit_metric == "pg/ml"){
        
        # print("hello")
        fixed_receptor <- my.value *(1/10^12) *(1/1000) * input$mol_weight_p1 * 10^12
      }
    }
    return (fixed_receptor)
  }
  
  convert_units_back_p2 <- function (my.value){
    
    #convert ligand back to original units
    if(input$p2concunit == "molar units"){
      
      if (input$p2unit_molar == "pM"){
        
        fixed_ligand <- my.value
        print (paste("fix lig", fixed_ligand))
        
      } else  if (input$p2unit_molar == "uM"){
        fixed_ligand = my.value * 1e-6 
        
      } else if (input$p2unit_molar == "fM"){
        
        fixed_ligand = my.value * 1e3 
        
      } else if (input$p2unit_molar == "nM"){
        
        fixed_ligand = my.value * 1e-3
        
      } else if (input$p2unit_molar == "mM"){
        
        fixed_ligand = my.value * 1e-9
        
      }
      
    } else if (input$p2concunit == "metric units"){
      
      # print (input$p2concunit)
      
      
      if (input$p2unit_metric == "ug/ml"){
        
        fixed_ligand <- my.value *(1/10^12) *(1/1000) * input$mol_weight_p2 * 10^6
        
      }else if (input$p2unit_metric == "mg/ml"){
        
        fixed_ligand <- my.value *(1/10^12) *(1/1000) * input$mol_weight_p2 * 10^3
        
        
      }else if (input$p2unit_metric == "ng/ml"){
        
        fixed_ligand <- my.value *(1/10^12) *(1/1000) * input$mol_weight_p2 * 10^9
        
      }else if (input$p2unit_metric == "pg/ml"){
        
        fixed_ligand <- my.value *(1/10^12) *(1/1000) * input$mol_weight_p2 * 10^12
        
      }
    }
    
    return (fixed_ligand)
  }
  
  cxbyligrangerecdata <- reactive({ 
    
    
    # input <- NULL
    # input$KD <- 100
    # input$c_total_receptor <- 100
    # input$c_total_ligand <- 100
    # input$lig_range[1] <- -4
    # input$lig_range[2] <- 2
    # input$red_range[1] <- -2
    # input$red_range[2] <- 2
    # input$kd_range[1] <- -4 
    # input$kd_range[2] <- 2
    # input$plot_interval_p2 <- 1
    # input$plot_interval_p2 <- 0.2
    # input$plot_interval_kd <- 1
    
    volume <- 1
    fixed_kd <- input$KD
    kdcoating <- 10^(12-0) #10^(12-input$pKdcoat[1])
    fixed_receptor <- input$c_total_receptor
    fixed_ligand <- input$c_total_ligand
    
    # seq.lig <- seq(from=input$lig_range[1],to=input$lig_range[2],by=1)
    seq.lig <- seq(from=input$lig_range[1],to=input$lig_range[2],by=input$plot_interval_p2)
    lig.max <- input$lig_range[2]
    seq.lig <- unique(append(seq.lig, lig.max))
    # seq.rec <- seq(from=input$rec_range[1],to=input$rec_range[2],by=0.2)
    seq.rec <- seq(from=input$rec_range[1],to=input$rec_range[2],by=input$plot_interval_p1)
    rec.max <- input$rec_range[2]
    seq.rec <- unique(append(seq.rec, rec.max))
    # print (seq.rec)
    # seq.kd <- seq(from=input$kd_range[1],to=input$kd_range[2],by=1)
    seq.kd <- seq(from=input$kd_range[1],to=input$kd_range[2],by=input$plot_interval_kd)
    kd.max <- input$kd_range[2]
    seq.kd <- unique(append(seq.kd, kd.max))
    
    
    # seq.lig <- seq(from=input$lig_range_CT[1],to=input$lig_range_CT[2],by=input$plot_interval_p2_CT)
    # lig.max <-input$lig_range_CT[2]
    # seq.lig <- unique(append(seq.lig, lig.max))
    # 
    # # seq.rec <- seq(from=input$rec_range_CT[1],to=input$rec_range_CT[2],by=0.2)
    # seq.rec <- seq(from=input$rec_range_CT[1],to=input$rec_range_CT[2],by=input$plot_interval_p1_CT)
    # rec.max <- input$rec_range_CT[2]
    # seq.rec <- unique(append(seq.rec, rec.max))
    # 
    # # seq.kd <- seq(from=input$kd_range_CT[1],to=input$kd_range_CT[2],by=1)
    # seq.kd <- seq(from=input$kd_range_CT[1],to=input$kd_range_CT[2],by=input$plot_interval_kd_CT)
    # kd.max <- input$kd_range_CT[2]
    # seq.kd <- unique(append(seq.kd, kd.max))
    
    
    min.lig <- 10^min(seq.lig)*fixed_ligand
    # print(min.lig)
    max.lig <- 10^max(seq.lig)*fixed_ligand
    # print(max.lig)
    # print(seq.lig)
    
    min.rec <- 10^min(seq.rec)*fixed_receptor
    max.rec <- 10^max(seq.rec)*fixed_receptor
    #plot( c(min.rec,max.rec), c(min(min.lig,min.rec),max(max.lig,max.rec)), log="xy", col=0, main=paste("Concentration ", input$receptor_name,"-",input$ligand_name, HTML(" complex for fixed KD of "), fixed_kd, " pM", sep=""), xlab=paste("Concentration of ", input$receptor_name," (", input$c_total_receptor," pM)",sep=""), ylab=paste(input$ligand_name,"-",input$receptor_name," complex",sep="") )
    
    #lines( c(min.lig,max.lig), c(min.rec,max.rec), col=1)
    rec.dots <- 10^seq.rec*fixed_receptor
    # print(rec.dots)
    leggy <- c()
    my.df <- NULL
    mylist <- NULL
    for (lig.cnt in seq.lig) {
      this.lig <- 10^lig.cnt*fixed_ligand
      ## now each line
      occ.dots <- c()
      for (rec.cnt in seq.rec) {
        this.rec <- 10^rec.cnt*fixed_receptor
        gfun <- function(x) {c(this.rec - x[1] - x[2]*x[1]/kdcoating - x[1]*x[3]/fixed_kd, 0-x[2]-x[2]*x[1]/kdcoating, 
                               this.lig-x[3]-x[1]*x[3]/fixed_kd)}
        fred <- multiroot( gfun, start=c(this.rec/2,0,this.lig/2), positive=TRUE )
        #gfun <- function(x) {c(this.rec - x[1] - x[2]*x[1]/kdcoating - x[1]*x[3]/fixed_kd, input$c_total_coating-x[2]-x[2]*x[1]/kdcoating, this.lig-x[3]-x[1]*x[3]/fixed_kd)}
        #fred <- multiroot( gfun, start=c(this.rec/2,input$c_total_coating/2,this.lig/2), positive=TRUE )
        c_free_receptor <- fred$root[1]
        c_free_coat <- fred$root[2]
        c_free_ligand <- fred$root[3]
        c_complex <- this.lig - c_free_ligand
        pc_occ = 100*(1-c_free_ligand/this.lig)
        occ.dots <- c(occ.dots, c_complex)
        
        # print (paste("lig:", this.lig))
        # this.lig.converted <- convert_units_back_p2 (this.lig)
        # print ( paste("conversion:", this.lig.converted))
        # this.lig <- this.lig
        
        
        my.df <-(cbind(rec.dots, this.lig, occ.dots))
        
      }
      #leggy <- c(leggy, paste(this.lig," pM ",input$ligand_name,sep="") )
      #lines( rec.dots, occ.dots, col=(1+lig.cnt-min(seq.lig)),lwd=2 )
      
      mylist <- data.frame(rbind(mylist, my.df))
      
    }
    
    # print(mylist)
    
    if (input$p1concunit == "metric units"){
      print("this is ridiculous")
      rec.dots <- 10^seq.rec*input$c_total_receptor_start
      this.lig <- 10^seq.lig*input$c_total_ligand_start
      occ.dots <- mylist[,3]
      
      temp <- NULL
      for (i in 1:length(this.lig)){
        temp[[i]] <- cbind(rec.dots, this.lig[i])
      }
      temp <- do.call(rbind.data.frame, temp)
      colnames(temp)[2] <- "this.lig"
      
      mylist <- cbind.data.frame(temp, occ.dots)
      str(mylist)
      print(mylist)
      
    }
    
    # for (i in 1:nrow(mylist)){
    #   
    #   mylist$rec.dots[i] <- round(convert_units_back_p1 (mylist$rec.dots[i]),2)
    #   mylist$this.lig[i] <- round(convert_units_back_p2 (mylist$this.lig[i]),2)
    # }
    
    # print(mylist)
    
    return(mylist)
    # mylist_spread <- spread(mylist, key= "this.lig", value =  "occ.dots")
    # thinned_spread = mylist_spread[seq(1, nrow(mylist_spread), 5), ]
    # return(thinned_spread)
    
  })
  
  # roccbyrecrangeligplot
  
  inputCxbyligrangerecplot <- reactive({ 
    
    mylist <- cxbyligrangerecdata ()
    mylist_spread <- spread(mylist, key= "this.lig", value =  "occ.dots")
    thinned_spread = mylist_spread[seq(1, nrow(mylist_spread), 1), ]
    thinned_gather <-  gather(thinned_spread[ ,2:ncol(thinned_spread)], key= "this.lig", value =  "occ.dots")
    thinned_gather_new <- cbind(thinned_spread[ ,1], thinned_gather)
    names(thinned_gather_new)[1] <- 'rec.dots'
    
    # print (thinned_gather_new)
    
    if (input$p1concunit == "metric units"){
      
      my.units <- input$p1unit_metric
      my.conc <- input$c_total_receptor_start
    }else if (input$p1concunit == "molar units"){
      my.units <- input$p1unit_molar
      my.conc <- input$c_total_receptor
    }
    
    
    if (input$p2concunit == "metric units"){
      
      my.units2 <- input$p2unit_metric
      my.conc2 <- input$c_total_ligand_start
    }else  if (input$p2concunit == "molar units"){
      my.units2 <- input$p2unit_molar
      my.conc2 <- input$c_total_ligand
    }
    
    my.breaks <- thinned_gather_new$rec.dots
    
    q <- plot_ly (data = mylist,
                  x = ~rec.dots) %>%
      add_trace (y = ~occ.dots, mode = 'lines', color = ~factor(this.lig)) %>%
      add_trace (data = thinned_gather_new,
                 y = ~ occ.dots, mode = 'markers', color = ~factor(this.lig),
                 showlegend = F, showticklabels = FALSE) %>%
      layout(title = paste("Concentration", em(input$receptor_name), "- ", em(input$ligand_name), "complex (pM) at a fixed K<sub>D</sub> of", em(input$KD), "pM"),
             font = list(size = 9),
             yaxis = list (title = paste(input$receptor_name,"-",input$ligand_name," complex (pM)",sep="")),  
             xaxis = list(type = "log", autotick = F,titlefont = list(size = 14),tickfont = list(size = 12),
                          title = paste("Concentration of ", input$receptor_name," (", my.units, " )",sep="") ))
    
    # q <- ggplot(mylist, aes(x=rec.dots, y=occ.dots, group=this.lig, colour = factor(this.lig)))+geom_line() +
    #   #geom_point(data= mylist[mylist$rec.dots %in% c(1, 10, 100, 1000, 10000), ]) +
    #   geom_point(data= thinned_gather_new, aes(x=rec.dots, y=occ.dots, group=this.lig, colour = factor(this.lig))) +
    #   scale_x_continuous(trans = "log10") +
    #   #scale_y_continuous(trans = "log10") +
    #   # ggtitle(paste("Concentration ", input$receptor_name,"-",input$ligand_name, HTML(" complex at fixed KD of "), fixed_kd, input$unit, sep="")) +
    #   ggtitle (HTML(paste("Concentration", em(input$receptor_name), "- ", em(input$ligand_name), "complex (pM) at a fixed K<sub>D</sub> of", em(input$KD), "pM"))) +
    #   #reactive plot units added
    #   
    #   xlab(paste("Concentration of ", input$receptor_name," (", my.units, " )",sep="")) +
    #   ylab(paste(input$ligand_name,"-",input$receptor_name," complex",sep="")) +
    #   scale_color_brewer(palette = "Paired") +
    #   theme(legend.position = "left",plot.title = element_text(size=10)) +
    #   labs(color = paste("Concentration of ", input$ligand_name, sep = "")) 
    
    
    
    if(input$scale == 1){
      
      
      q %>%
        config(displayModeBar = FALSE)%>%
        config(displaylogo = FALSE) %>%
        config(collaborate = FALSE) %>%
        config(
          edits = list(
            annotationPosition = TRUE,
            annotationTail = TRUE,
            annotationText = TRUE
          ))%>%
        config(
          toImageButtonOptions = list(
            format = "png",
            width = 1000,
            height = 1000
          ))%>%
        layout(
          
          legend = list(
            orientation = "h",
            x = 0.225, y = -0.3,
            font = list(size = 12)
          )
        )%>%
        add_annotations( text=paste("Concentration of <br>", input$ligand_name, " (", my.units2, ") ",sep = ""), xref="paper", yref="paper",
                         x=0, xanchor="left",
                         y=-0.435, yanchor="bottom",    # Same y as legend below
                         legendtitle=TRUE, showarrow=FALSE,
                         font=list(size=12,face="bold")) 
      
      
    } else if (input$scale == 2){
      # q <- q + scale_y_continuous(trans = "log10") 
      
      q <- q %>% layout(yaxis = list(type = "log"))
      
      q %>%
        config(displayModeBar = FALSE)%>%
        config(displaylogo = FALSE) %>%
        config(collaborate = FALSE) %>%
        layout(legend = list(
          orientation = "h",
          x = 0.225, y = -0.3,
          font = list(size = 12)
        )
        )%>% 
        add_annotations( text=paste("Concentration of <br>", input$ligand_name, " (", my.units2, ") ", sep = ""), xref="paper", yref="paper",
                         x=0, xanchor="left",
                         y=-0.435, yanchor="bottom",    # Same y as legend below
                         legendtitle=TRUE, showarrow=FALSE,
                         font=list(size=12,face="bold")) 
      
    }
    
  })
  
  # observeEvent(c(input$p1unit_metric, input$p1unit_molar, input$p2unit_metric, input$p2unit_molar, 
  #                input$scale,input$c_total_ligand, input$c_total_receptor, input$lig_range,input$rec_range,input$kd_range,
  #                input$submit_slider_range_p1, input$submit_slider_range_p2, input$submit_slider_range_kd), {
  observeEvent(input$myconfirmation_submit_binding, {
    if (!is.null (input$myconfirmation_submit_binding) ){
      
      
      output$cxbyligrangerecplot <- renderPlotly({
        
        isolate({
          print(inputCxbyligrangerecplot())
        })
      })
    }
  })
  
  
  
  
  inputTable1 <- reactive ({ 
    
    mylist <- cxbyligrangerecdata()
    mylist_spread <- spread(mylist, key= "this.lig", value =  "occ.dots")
    thinned_spread = mylist_spread[seq(1, nrow(mylist_spread), 1), ]
    
    names(thinned_spread)[1] <- ""
    thinned_spread <- round(thinned_spread, 2)
    
  })
  
  
  observeEvent(input$myconfirmation_submit_binding, {
    if (!is.null (input$myconfirmation_submit_binding) ){
      
      output$Table1 <- renderDataTable({
        
        isolate({
          if (input$adv_slider_p1 | input$adv_slider_p2 | input$adv_slider_kd ){
            # if (input$adv_options){    
            validate(
              need(input$submit_slider_range_p1 | input$submit_slider_range_p2| input$submit_slider_range_kd, 
                   "Please provide advanced plotting parameters")
            )
            
            if (input$submit_slider_range_p1 | input$submit_slider_range_p2 | input$submit_slider_range_kd){
              
              if (input$p1concunit == "metric units"){
                name_partner1 <- HTML('Concentration of ' ,input$receptor_name, "<br>", "(", input$p1unit_metric, ")")
                name_partner2 <- HTML('Concentration of ' ,input$ligand_name, "<br>", "(", input$p2unit_metric, ")")
              }else if (input$p1concunit == "molar units"){
                name_partner1 <- HTML('Concentration of ' ,input$receptor_name, "<br>", "(", input$p1unit_molar, ")")
                name_partner2 <- HTML('Concentration of ' ,input$ligand_name, "<br>", "(", input$p2unit_molar, ")")
              }
              
              # name_partner1 <- HTML('Concentration of ' ,input$receptor_name, "<br>", "(", input$p1unit, ")")
              # name_partner2 <- HTML('Concentration of ' ,input$ligand_name, "<br>", "(", input$p2unit, ")")
              
              # mylist <- cxbyligrangerecdata()
              # mylist_spread <- spread(mylist, key= "this.lig", value =  "occ.dots")
              # thinned_spread = mylist_spread[seq(1, nrow(mylist_spread), 5), ]
              # 
              # names(thinned_spread)[1] <- ""
              # thinned_spread <- round(thinned_spread, 2)
              
              thinned_spread <- inputTable1()
              
              sketch <- createContainer(name_partner1, name_partner2, thinned_spread)
              
              datatable(
                thinned_spread,
                extensions = 'Buttons',
                class = "cell-border stripe hover",
                container = sketch,
                rownames = FALSE, 
                
                options = list(
                  pageLength = 7,
                  dom = 'tprB',
                  scrollX = TRUE,
                  buttons = list(
                    c('copy', 'excel'))
                )
              )%>%
                formatStyle(thinned_spread [ ,1], fontWeight = "bold")
            }
            
            
          }else {
            
            if (input$p1concunit == "metric units"){
              name_partner1 <- HTML('Concentration of ' ,input$receptor_name, "<br>", "(", input$p1unit_metric, ")")
              name_partner2 <- HTML('Concentration of ' ,input$ligand_name, "<br>", "(", input$p2unit_metric, ")")
            }else if (input$p1concunit == "molar units"){
              name_partner1 <- HTML('Concentration of ' ,input$receptor_name, "<br>", "(", input$p1unit_molar, ")")
              name_partner2 <- HTML('Concentration of ' ,input$ligand_name, "<br>", "(", input$p2unit_molar, ")")
            }
            
            # name_partner1 <- HTML('Concentration of ' ,input$receptor_name, "<br>", "(", input$p1unit, ")")
            # name_partner2 <- HTML('Concentration of ' ,input$ligand_name, "<br>", "(", input$p2unit, ")")
            
            # mylist <- cxbyligrangerecdata()
            # mylist_spread <- spread(mylist, key= "this.lig", value =  "occ.dots")
            # thinned_spread = mylist_spread[seq(1, nrow(mylist_spread), 5), ]
            # 
            # names(thinned_spread)[1] <- ""
            # thinned_spread <- round(thinned_spread, 2)
            
            thinned_spread <- inputTable1()
            
            sketch <- createContainer(name_partner1, name_partner2, thinned_spread)
            
            datatable(
              thinned_spread,
              extensions = 'Buttons',
              class = "cell-border stripe hover",
              container = sketch,
              rownames = FALSE, 
              
              options = list(
                pageLength = 7,
                dom = 'tprB',
                scrollX = TRUE,
                buttons = list(
                  c('copy', 'excel'))
              )
            )%>%
              formatStyle(thinned_spread [ ,1], fontWeight = "bold")
            
            
            
          }
          
        }) 
      })
    }
  })
  
  
  
  data2 <- reactive({
    volume <- 1
    fixed_kd <- input$KD
    kdcoating <- 10^(12-0) #10^(12-input$pKdcoat[1])
    fixed_receptor <- input$c_total_receptor
    fixed_ligand <- input$c_total_ligand
    # my.interval <- input$plot_interval_p1
    
    # seq.lig <- seq(from=input$lig_range[1],to=input$lig_range[2],by=input$plot_interval_p2)
    # seq.rec <- seq(from=input$rec_range[1],to=input$rec_range[2],by=input$plot_interval_p1)
    # seq.kd <- seq(from=input$kd_range[1],to=input$kd_range[2],by=input$plot_interval_kd)
    seq.lig <- seq(from=input$lig_range[1],to=input$lig_range[2],by=input$plot_interval_p2)
    lig.max <- input$lig_range[2]
    seq.lig <- unique(append(seq.lig, lig.max))
    # seq.rec <- seq(from=input$rec_range[1],to=input$rec_range[2],by=0.2)
    seq.rec <- seq(from=input$rec_range[1],to=input$rec_range[2],by=input$plot_interval_p1)
    rec.max <- input$rec_range[2]
    seq.rec <- unique(append(seq.rec, rec.max))
    # print (seq.rec)
    # seq.kd <- seq(from=input$kd_range[1],to=input$kd_range[2],by=1)
    seq.kd <- seq(from=input$kd_range[1],to=input$kd_range[2],by=input$plot_interval_kd)
    kd.max <- input$kd_range[2]
    seq.kd <- unique(append(seq.kd, kd.max))
    
    # seq.lig <- seq(from=input$lig_range[1],to=input$lig_range[2],by=1)
    # seq.rec <- seq(from=input$rec_range[1],to=input$rec_range[2],by=my.interval)
    # seq.kd <- seq(from=input$kd_range[1],to=input$kd_range[2],by=1)
    
    
    min.lig <- 10^min(seq.lig)*fixed_ligand
    max.lig <- 10^max(seq.lig)*fixed_ligand
    min.rec <- 10^min(seq.rec)*fixed_receptor
    max.rec <- 10^max(seq.rec)*fixed_receptor
    
    rec.dots <- 10^seq.rec*fixed_receptor
    leggy <- c()
    my.df <- NULL
    mylist <- NULL
    for (lig.cnt in seq.lig) {
      this.lig <- 10^lig.cnt*fixed_ligand
      ## now each line
      occ.dots <- c()
      for (rec.cnt in seq.rec) {
        this.rec <- 10^rec.cnt*fixed_receptor
        gfun <- function(x) {c(this.rec - x[1] - x[2]*x[1]/kdcoating - x[1]*x[3]/fixed_kd, 0-x[2]-x[2]*x[1]/kdcoating, this.lig-x[3]-x[1]*x[3]/fixed_kd)}
        fred <- multiroot(gfun, start=c(this.rec/2,0,this.lig/2), positive=TRUE )
        #gfun <- function(x) {c(this.rec - x[1] - x[2]*x[1]/kdcoating - x[1]*x[3]/fixed_kd, input$c_total_coating-x[2]-x[2]*x[1]/kdcoating, this.lig-x[3]-x[1]*x[3]/fixed_kd)}
        #fred <- multiroot( gfun, start=c(this.rec/2,input$c_total_coating/2,this.lig/2), positive=TRUE )
        c_free_receptor <- fred$root[1]
        c_free_coat <- fred$root[2]
        c_free_ligand <- fred$root[3]
        c_complex <- this.lig - c_free_ligand
        pc_occ = 100*(1-c_free_ligand/this.lig)
        occ.dots <- c(occ.dots, pc_occ)
        my.df <- (cbind(rec.dots, this.lig, occ.dots))
      }
      mylist <- data.frame(rbind(mylist, my.df))
    }
    # for (i in 1:nrow(mylist)){
    #   
    #   mylist$rec.dots[i] <- round(convert_units_back_p1 (mylist$rec.dots[i]),2)
    #   mylist$this.lig[i] <- round(convert_units_back_p2 (mylist$this.lig[i]),2)
    # }]
    
    if (input$p1concunit == "metric units"){
      print("this is ridiculous")
      rec.dots <- 10^seq.rec*input$c_total_receptor_start
      this.lig <- 10^seq.lig*input$c_total_ligand_start
      occ.dots <- mylist[,3]
      
      temp <- NULL
      for (i in 1:length(this.lig)){
        temp[[i]] <- cbind(rec.dots, this.lig[i])
      }
      temp <- do.call(rbind.data.frame, temp)
      colnames(temp)[2] <- "this.lig"
      
      mylist <- cbind.data.frame(temp, occ.dots)
      # str(mylist)
      # print(mylist)
      
    }
    return(mylist)
  })
  
  
  
  
  inputRoccbyrecrangeligplot <- reactive({
    
    mylist <- data2()
    mylist_spread <- spread(mylist, key= "this.lig", value =  "occ.dots")
    thinned_spread = mylist_spread[seq(1, nrow(mylist_spread), 1), ]
    thinned_gather <-  gather(thinned_spread[ ,2:ncol(thinned_spread)], key= "this.lig", value =  "occ.dots")
    thinned_gather_new <- cbind(thinned_spread[ ,1], thinned_gather)
    names(thinned_gather_new)[1] <- 'rec.dots'
    
    
    if (input$p1concunit == "metric units"){
      
      my.units <- input$p1unit_metric
      my.conc <- input$c_total_receptor_start
    }else {
      my.units <- input$p1unit_molar
      my.conc <- input$c_total_receptor
    }
    
    if (input$p2concunit == "metric units"){
      
      my.units2 <- input$p2unit_metric
      my.conc2 <- input$c_total_ligand_start
    }else  if (input$p2concunit == "molar units"){
      my.units2 <- input$p2unit_molar
      my.conc2 <- input$c_total_ligand
    }
    
    q <- plot_ly (data = mylist,
                  x = ~rec.dots) %>%
      add_trace (y = ~occ.dots, mode = 'lines', color = ~factor(this.lig)) %>%
      add_trace (data = thinned_gather_new,
                 y = ~ occ.dots, mode = 'markers', color = ~factor(this.lig),
                 showlegend = F, showticklabels = FALSE) %>%
      layout(title = paste("Fractional occupancy of ", em(input$ligand_name), " at a fixed K<sub>D</sub> of ", em(input$KD), "pM"),
             font = list(size = 9),
             yaxis = list (title = paste("% ",input$ligand_name, " occupancy", sep="")),  
             xaxis = list(type = "log", autotick = F,titlefont = list(size = 14),tickfont = list(size = 12),
                          title = paste("Concentration of ", input$receptor_name," (", my.units, ")",sep="")))
    
    
    # q <- ggplot(mylist, aes(x=rec.dots, y=occ.dots, group=this.lig, colour = factor(this.lig)))+geom_line() +
    #   #ylim(0, 100) +
    #   #geom_point(data= mylist[mylist$rec.dots %in% c(1, 10, 100, 1000, 10000), ]) +
    #   geom_point(data= thinned_gather_new, aes(x=rec.dots, y=occ.dots, group=this.lig, colour = factor(this.lig))) +
    #   scale_x_continuous(trans = "log10") +
    #   #scale_y_continuous(trans = "log10") +
    #   #ggtitle(paste(input$receptor_name, " occupancy for fixed KD of ", fixed_kd," pM",sep="")) +
    #   ggtitle(HTML(paste("Fractional occupancy of ", em(input$ligand_name), " at a fixed K<sub>D</sub> of ", em(input$KD), "pM"))) +
    #   xlab(paste("Concentration of ", input$receptor_name," (", my.units, ")",sep="")) +
    #   # xlab(paste("Concentration of ", input$receptor_name," (", my.conc, " ", my.units, " )",sep="")) +
    #   ylab(paste("% ",input$ligand_name, " occupancy", sep="")) + ylim(0,100) +
    #   scale_color_brewer(palette = "Paired") +
    #   theme(legend.position = "bottom", plot.title = element_text(size=10)) +
    #   labs(color = paste("Concentration of ", input$ligand_name, sep = ""))
    
    
    if(input$scale == 1){
      
      q %>%
        config(displayModeBar = FALSE)%>%
        config(displaylogo = FALSE) %>%
        config(collaborate = FALSE) %>%
        layout(legend = list(
          orientation = "h",
          x = 0.225, y = -0.3,
          font = list(size = 12)
        )
        )%>% 
        add_annotations( text=paste("Concentration of <br>", input$ligand_name, " (", my.units2, ") ",sep = ""), xref="paper", yref="paper",
                         x=0, xanchor="left",
                         y=-0.435, yanchor="bottom",    # Same y as legend below
                         legendtitle=TRUE, showarrow=FALSE,
                         font=list(size=12,face="bold")) 
      
      
    } else if (input$scale == 2){
      q <- q %>% layout(yaxis = list(type = "log"))
      
      q %>%
        config(displayModeBar = FALSE)%>%
        config(displaylogo = FALSE) %>%
        config(collaborate = FALSE) %>%
        layout(legend = list(
          orientation = "h",
          x = 0.225, y = -0.3,
          font = list(size = 12)
        ),
        yaxis = list (autotick = F)
        )%>% 
        add_annotations( text=paste("Concentration of <br>", input$ligand_name, " (", my.units2, ") ",sep = ""), xref="paper", yref="paper",
                         x=0, xanchor="left",
                         y=-0.435, yanchor="bottom",    # Same y as legend below
                         legendtitle=TRUE, showarrow=FALSE,
                         font=list(size=12,face="bold")) 
      
    }
    
  })
  
  observeEvent(input$myconfirmation_submit_binding, {
    if (!is.null (input$myconfirmation_submit_binding) ){
      # observeEvent(c(input$p1unit_metric, input$p2unit_metric, input$scale,input$c_total_ligand, input$c_total_receptor, input$lig_range,input$rec_range,input$kd_range,
      #                input$submit_slider_range_p1, input$submit_slider_range_p2, input$submit_slider_range_kd), { 
      
      output$roccbyrecrangeligplot <- renderPlotly({
        isolate({ 
          print(inputRoccbyrecrangeligplot())
        })
      })
    }
  })
  
  inputTable2 <- reactive ({ 
    
    mylist <- data2()
    mylist_spread <- spread(mylist, key= "this.lig", value =  "occ.dots")
    thinned_spread = mylist_spread[seq(1, nrow(mylist_spread), 1), ]
    names(thinned_spread)[1] <- ""
    thinned_spread <- round(thinned_spread, 2)
    
  })
  
  observeEvent(input$myconfirmation_submit_binding, {
    if (!is.null (input$myconfirmation_submit_binding) ){
      
      output$Table2 <- renderDataTable({
        isolate({
          
          if (input$adv_slider_p1 | input$adv_slider_p2| input$adv_slider_kd){
            # if (input$adv_options){    
            validate(
              need(input$submit_slider_range_p2 | input$submit_slider_range_p1 | input$submit_slider_range_kd, 
                   
                   "Please provide advanced plotting parameters")
            )
            
            if (input$submit_slider_range_p1 | input$submit_slider_range_p2 | input$submit_slider_range_kd){
              
              if (input$p1concunit == "metric units"){
                
                name_partner1 <- HTML('Concentration of ' ,input$receptor_name, "<br>", "(", input$p1unit_metric, ")")
                name_partner2 <- HTML('Concentration of ' ,input$ligand_name, "<br>", "(", input$p2unit_metric, ")")
              } else {
                name_partner1 <- HTML('Concentration of ' ,input$receptor_name, "<br>", "(", input$p1unit_molar, ")")
                name_partner2 <- HTML('Concentration of ' ,input$ligand_name, "<br>", "(", input$p2unit_molar, ")")
              }
              
              # mylist <- data2()
              # mylist_spread <- spread(mylist, key= "this.lig", value =  "occ.dots")
              # thinned_spread = mylist_spread[seq(1, nrow(mylist_spread), 5), ]
              # names(thinned_spread)[1] <- ""
              # thinned_spread <- round(thinned_spread, 2)
              thinned_spread <- inputTable2()
              sketch <- createContainer(name_partner1, name_partner2, thinned_spread)
              
              datatable(
                thinned_spread,
                extensions = 'Buttons',
                class = "cell-border stripe hover",
                container = sketch,
                rownames = FALSE, 
                
                options = list(
                  pageLength = 7,
                  dom = 'tprB',
                  scrollX = TRUE,
                  buttons = list(
                    c('copy', 'excel'))
                )
              )%>%
                formatStyle(thinned_spread [ ,1], fontWeight = "bold")
              
            }
            
          } else {
            
            if (input$p1concunit == "metric units"){
              
              name_partner1 <- HTML('Concentration of ' ,input$receptor_name, "<br>", "(", input$p1unit_metric, ")")
              name_partner2 <- HTML('Concentration of ' ,input$ligand_name, "<br>", "(", input$p2unit_metric, ")")
            } else {
              name_partner1 <- HTML('Concentration of ' ,input$receptor_name, "<br>", "(", input$p1unit_molar, ")")
              name_partner2 <- HTML('Concentration of ' ,input$ligand_name, "<br>", "(", input$p2unit_molar, ")")
            }
            # name_partner1 <- HTML('Concentration of ' ,input$receptor_name, "<br>", "(", input$p1unit, ")")
            # name_partner2 <- HTML('Concentration of ' ,input$ligand_name, "<br>", "(", input$p2unit, ")")
            
            # mylist <- data2()
            # mylist_spread <- spread(mylist, key= "this.lig", value =  "occ.dots")
            # thinned_spread = mylist_spread[seq(1, nrow(mylist_spread), 5), ]
            # names(thinned_spread)[1] <- ""
            # thinned_spread <- round(thinned_spread, 2)
            
            thinned_spread <- inputTable2()
            sketch <- createContainer(name_partner1, name_partner2, thinned_spread)
            
            datatable(
              thinned_spread,
              extensions = 'Buttons',
              class = "cell-border stripe hover",
              container = sketch,
              rownames = FALSE, 
              
              options = list(
                pageLength = 7,
                dom = 'tprB',
                scrollX = TRUE,
                buttons = list(
                  c('copy', 'excel'))
              )
            )%>%
              formatStyle(thinned_spread [ ,1], fontWeight = "bold")
            
          }
        })
      })
    }
  })
  
  
  cxbyligrangekddata <- reactive({
    volume <- 1
    fixed_kd <- input$KD
    kdcoating <-  10^(12-0)
    fixed_receptor <- input$c_total_receptor
    fixed_ligand <- input$c_total_ligand
    # my.interval <- input$plot_interval_p1
    
    # seq.lig <- seq(from=input$lig_range[1],to=input$lig_range[2],by=input$plot_interval_p2)
    # seq.rec <- seq(from=input$rec_range[1],to=input$rec_range[2],by=input$plot_interval_p1)
    # seq.kd <- seq(from=input$kd_range[1],to=input$kd_range[2],by=input$plot_interval_kd)
    seq.lig <- seq(from=input$lig_range[1],to=input$lig_range[2],by=input$plot_interval_p2)
    lig.max <- input$lig_range[2]
    seq.lig <- unique(append(seq.lig, lig.max))
    # seq.rec <- seq(from=input$rec_range[1],to=input$rec_range[2],by=0.2)
    seq.rec <- seq(from=input$rec_range[1],to=input$rec_range[2],by=input$plot_interval_p1)
    rec.max <- input$rec_range[2]
    seq.rec <- unique(append(seq.rec, rec.max))
    # print (seq.rec)
    # seq.kd <- seq(from=input$kd_range[1],to=input$kd_range[2],by=1)
    seq.kd <- seq(from=input$kd_range[1],to=input$kd_range[2],by=input$plot_interval_kd)
    kd.max <- input$kd_range[2]
    seq.kd <- unique(append(seq.kd, kd.max))
    
    
    min.lig <- 10^min(seq.lig)*fixed_ligand
    max.lig <- 10^max(seq.lig)*fixed_ligand
    min.rec <- 10^min(seq.rec)*fixed_receptor
    max.rec <- 10^max(seq.rec)*fixed_receptor
    #plot( c(min.rec,max.rec), c(min.rec,min(fixed_receptor,max.lig)), log="xy", col=0, main=paste("Concentration ",input$receptor_name,"-",input$ligand_name," complex (pM) for fixed ",input$ligand_name," of ", fixed_ligand," pM",sep=""), xlab=paste("Concentration of ", input$receptor_name," (pM)",sep=""), ylab=paste("Concentration ",input$receptor_name,"-",input$ligand_name," complex (pM)",sep="") )
    #lines( c(min.lig,max.lig), c(min.rec,max.rec), col=1)
    rec.dots <- 10^seq.rec*fixed_receptor
    leggy <- c()
    my.df <- NULL
    mylist <- NULL
    for (kd.cnt in seq.kd) {
      this.kd <- 10^kd.cnt*fixed_kd
      ## now each line
      cx.dots <- c()
      for (rec.cnt in seq.rec) {
        this.rec <- 10^rec.cnt*fixed_receptor
        gfun <- function(x) {c(this.rec - x[1] - x[2]*x[1]/kdcoating - x[1]*x[3]/this.kd, 0-x[2]-x[2]*x[1]/kdcoating, fixed_ligand-x[3]-x[1]*x[3]/this.kd)}
        fred <- multiroot(gfun, start=c(this.rec/2,0/2,fixed_ligand/2), positive=TRUE )
        #gfun <- function(x) {c(this.rec - x[1] - x[2]*x[1]/kdcoating - x[1]*x[3]/this.kd, input$c_total_coating-x[2]-x[2]*x[1]/kdcoating, fixed_ligand-x[3]-x[1]*x[3]/this.kd)}
        #fred <- multiroot( gfun, start=c(this.rec/2,input$c_total_coating/2,fixed_ligand/2), positive=TRUE )
        c_free_receptor <- fred$root[1]
        c_free_coat <- fred$root[2]
        c_free_ligand <- fred$root[3]
        #c_complex <- this.kd - c_free_ligand
        c_complex <- this.rec - c_free_receptor
        pc_occ = 100*(1-c_free_receptor/fixed_receptor)
        #occ.dots <- c(occ.dots, pc_occ)
        cx.dots <- c(cx.dots, c_complex)
        my.df <- (cbind(rec.dots, this.kd, cx.dots))
      }
      mylist <- data.frame(rbind(mylist, my.df))
      
    }
    
    mylist_spread <- spread(mylist, key= "this.kd", value =  "cx.dots")
    thinned_spread = mylist_spread[seq(1, nrow(mylist_spread), 5), ]
    
    if (input$p1concunit == "metric units"){
      print("this is ridiculous")
      rec.dots <- 10^seq.rec*input$c_total_receptor_start
      this.kd <- 10^seq.kd*input$KD
      cx.dots <- mylist[,3]
      
      temp <- NULL
      for (i in 1:length(this.kd)){
        temp[[i]] <- cbind(rec.dots, this.kd[i])
      }
      temp <- do.call(rbind.data.frame, temp)
      colnames(temp)[2] <- "this.kd"
      
      mylist <- cbind.data.frame(temp, cx.dots)
      # str(mylist)
      # print(mylist)
      
    }
    # for (i in 1:nrow(mylist)){
    #   
    #   mylist$rec.dots[i] <- round(convert_units_back_p1 (mylist$rec.dots[i]),2)
    #   # mylist$this.lig[i] <- round(convert_units_back_p2 (mylist$this.lig[i]),2)
    # }
    return(mylist)
  })  
  
  
  
  observeEvent(input$myconfirmation_submit_binding, {
    if (!is.null (input$myconfirmation_submit_binding) ){
      # Concentration Partner 1 - Partner 2 complex (pM) for a fixed partner 2 concentration
      # observeEvent(c(input$p1unit_molar, input$p1unit_metric,
      #                input$p2unit_metric, input$p2unit_molar, input$scale, input$c_total_ligand, input$c_total_receptor, input$lig_range,input$rec_range,input$kd_range,
      #                input$submit_slider_range_p1, input$submit_slider_range_p2, input$submit_slider_range_kd), {  
      
      output$cxbyligrangekdplot <- renderPlotly({
        isolate({
          print(inputCxbyligrangekdplot())
        })
      })    
    }               
  }) 
  
  inputCxbyligrangekdplot <- reactive ({
    
    mylist <- cxbyligrangekddata()
    mylist_spread <- spread(mylist, key= "this.kd", value =  "cx.dots")
    thinned_spread = mylist_spread[seq(1, nrow(mylist_spread), 1), ]
    thinned_gather <-  gather(thinned_spread[ ,2:ncol(thinned_spread)], key= "this.kd", value =  "cx.dots")
    thinned_gather_new <- cbind(thinned_spread[ ,1], thinned_gather)
    names(thinned_gather_new)[1] <- 'rec.dots'
    
    if (input$p1concunit == "metric units"){
      
      my.units <- input$p1unit_metric
      my.conc <- input$c_total_receptor_start
    }else {
      my.units <- input$p1unit_molar
      my.conc <- input$c_total_receptor
    }   
    
    if (input$p2concunit == "metric units"){
      
      my.unitsp2 <- input$p2unit_metric
      
    }else {
      my.unitsp2 <- input$p2unit_molar
    }
    
    q <- plot_ly (data = mylist,
                  x = ~rec.dots) %>%
      add_trace (y = ~cx.dots, mode = 'lines', color = ~as.factor(this.kd)) %>%
      add_trace (data = thinned_gather_new,
                 y = ~ cx.dots, mode = 'markers', color = ~as.factor(this.kd),
                 showlegend = F, showticklabels = FALSE) %>%
      layout(title = paste("Concentration", em(input$receptor_name), " - ", em(input$ligand_name),  "complex (pM) at a fixed", em(input$ligand_name),  "concentration of", 
                           em(input$c_total_ligand_start) ,my.unitsp2),
             font = list(size = 9),
             yaxis = list (title = paste(input$receptor_name,"-",input$ligand_name," complex (pM)",sep="")),  
             xaxis = list(type = "log", autotick = F,titlefont = list(size = 14),tickfont = list(size = 12),
                          title = paste("Concentration of ", input$receptor_name," (", my.units, ")",sep="")))
    
    # q <- ggplot(mylist, aes(x=rec.dots, y=cx.dots, group=this.kd, colour = factor(this.kd)))+geom_line() +
    #   #geom_point(data= mylist[mylist$lig.dots %in% c(1, 10, 100, 1000, 10000), ]) +
    #   geom_point(data= thinned_gather_new, aes(x=rec.dots, y=cx.dots, group=this.kd, colour = factor(this.kd))) +
    #   scale_x_continuous(trans = "log10") +
    #   #scale_y_continuous(trans = "log10") +
    #   #ggtitle(paste("Concentration ",input$receptor_name,"-",input$ligand_name," complex (pM) for fixed ",input$ligand_name," of ", fixed_ligand," pM",sep="")) +
    #   ggtitle (HTML(paste("Concentration", em(input$receptor_name), " - ", em(input$ligand_name),  "complex (pM) at a fixed", em(input$ligand_name),  "concentration of", 
    #                       em(input$c_total_ligand_start) ,input$p2unit))) +
    #   xlab(paste("Concentration of ", input$receptor_name," (", my.units, ")",sep="")) +
    #   # xlab(paste("Concentration of ", input$receptor_name," (", my.conc, " ", my.units, " )",sep="")) +
    #   ylab(paste("Concentration ",input$receptor_name,"-",input$ligand_name," complex (", input$unit,  ")",sep="")) +
    #   scale_color_brewer(palette = "Paired") +
    #   theme(legend.position = "bottom", plot.title = element_text(size=10)) +
    #   labs(color = paste("KD", sep = ""))
    
    # output$cxbyligrangekdplot <- renderPlotly({
    
    if(input$scale == 1){
      
      q %>%
        config(displayModeBar = FALSE)%>%
        config(displaylogo = FALSE) %>%
        config(collaborate = FALSE) %>%
        layout(legend = list(
          orientation = "h",
          # x = 0.2, y = -0.3
          x = 0.225, y = -0.3,
          font = list(size = 12)
        )
        )%>% 
        add_annotations( text= paste("K<sub>D</sub>", " (", "pM", ") ",sep = ""), xref="paper", yref="paper",
                         x=0.125, xanchor="left",
                         y=-0.385, yanchor="bottom",    # Same y as legend below
                         legendtitle=TRUE, showarrow=FALSE,
                         font=list(size=12,face="bold")) 
      
    } else if (input$scale == 2){
      q <- q %>% layout(yaxis = list(type = "log"))
      
      q %>%
        config(displayModeBar = FALSE)%>%
        config(displaylogo = FALSE) %>%
        config(collaborate = FALSE) %>%
        layout(legend = list(
          orientation = "h",
          x = 0.2, y = -0.3,
          font = list(size = 12)
        ),
        yaxis = list(autotick = F)
        )%>% 
        add_annotations( text=paste("K<sub>D</sub>",  " (", "pM", ") ", sep = ""), xref="paper", yref="paper",
                         x=0.125, xanchor="left",
                         y=-0.385, yanchor="bottom",    # Same y as legend below
                         legendtitle=TRUE, showarrow=FALSE,
                         font=list(size=12,face="bold")) 
      
    }
  })
  
  inputTable3 <- reactive ({ 
    
    mylist <- cxbyligrangekddata()
    mylist_spread <- spread(mylist, key= "this.kd", value =  "cx.dots")
    thinned_spread = mylist_spread[seq(1, nrow(mylist_spread), 1), ]
    names(thinned_spread)[1] <- ""
    thinned_spread <- round(thinned_spread, 2)
    
  })
  
  
  observeEvent(input$myconfirmation_submit_binding, {
    if (!is.null (input$myconfirmation_submit_binding) ){
      
      output$Table3 <- renderDataTable({
        isolate({
          
          if (input$adv_slider_p1 | input$adv_slider_p2 | input$adv_slider_kd){
            # if (input$adv_options){  
            validate(
              need(input$submit_slider_range_p1 | input$submit_slider_range_p2| input$submit_slider_range_kd, 
                   "Please provide advanced plotting parameters")
            )
            
            if (input$submit_slider_range_p1 | input$submit_slider_range_p1 | input$submit_slider_range_p1){
              
              if (input$p1concunit == "metric units"){
                
                name_partner1 <- HTML('Concentration of ' ,input$receptor_name, "<br>", "(", input$p1unit_metric, ")")
                name_partner2 <- HTML('KD'," (pM)", "")
                
              }else {
                name_partner1 <- HTML('Concentration of ' ,input$receptor_name, "<br>", "(", input$p1unit_molar, ")")
                name_partner2 <- HTML('KD'," (pM)", "")
              }
              
              # mylist <- cxbyligrangekddata()
              # mylist_spread <- spread(mylist, key= "this.kd", value =  "cx.dots")
              # thinned_spread = mylist_spread[seq(1, nrow(mylist_spread), 5), ]
              # names(thinned_spread)[1] <- ""
              # thinned_spread <- round(thinned_spread, 2)
              
              thinned_spread <- inputTable3()
              
              sketch <- createContainer(name_partner1, name_partner2, thinned_spread)
              
              datatable(
                thinned_spread,
                extensions = 'Buttons',
                class = "cell-border stripe hover",
                container = sketch,
                rownames = FALSE, 
                
                options = list(
                  pageLength = 7,
                  dom = 'tprB',
                  scrollX = TRUE,
                  buttons = list(
                    c('copy', 'excel'))
                )
              )%>%
                formatStyle(thinned_spread [ ,1], fontWeight = "bold")
              
            }
            
          } else {
            
            if (input$p1concunit == "metric units"){
              name_partner1 <- HTML('Concentration of ' ,input$receptor_name, "<br>", "(", input$p1unit_metric, ")")
              name_partner2 <- HTML('KD'," (pM)", "")
            } else {
              name_partner1 <- HTML('Concentration of ' ,input$receptor_name, "<br>", "(", input$p1unit_molar, ")")
              name_partner2 <- HTML('KD'," (pM)", "")
            }
            
            # mylist <- cxbyligrangekddata()
            # mylist_spread <- spread(mylist, key= "this.kd", value =  "cx.dots")
            # thinned_spread = mylist_spread[seq(1, nrow(mylist_spread), 5), ]
            # names(thinned_spread)[1] <- ""
            # thinned_spread <- round(thinned_spread, 2)
            
            thinned_spread <- inputTable3()
            sketch <- createContainer(name_partner1, name_partner2, thinned_spread)
            
            datatable(
              thinned_spread,
              extensions = 'Buttons',
              class = "cell-border stripe hover",
              container = sketch,
              rownames = FALSE, 
              
              options = list(
                pageLength = 7,
                dom = 'tprB',
                scrollX = TRUE,
                buttons = list(
                  c('copy', 'excel'))
              )
            )%>%
              formatStyle(thinned_spread [ ,1], fontWeight = "bold")
          }
        })
      })
    }
  })
  
  roccbyrecrangeligdata <- reactive({
    volume <- 1
    fixed_kd <- input$KD
    kdcoating <-  10^(12-0) #10^(12-input$pKdcoat[1])
    fixed_receptor <- input$c_total_receptor
    fixed_ligand <- input$c_total_ligand
    # my.interval <- input$plot_interval_p1
    
    # seq.lig <- seq(from=input$lig_range[1],to=input$lig_range[2],by=input$plot_interval_p2)
    # seq.rec <- seq(from=input$rec_range[1],to=input$rec_range[2],by=input$plot_interval_p1)
    # seq.kd <- seq(from=input$kd_range[1],to=input$kd_range[2],by=input$plot_interval_kd)
    
    seq.lig <- seq(from=input$lig_range[1],to=input$lig_range[2],by=input$plot_interval_p2)
    lig.max <- input$lig_range[2]
    seq.lig <- unique(append(seq.lig, lig.max))
    # seq.rec <- seq(from=input$rec_range[1],to=input$rec_range[2],by=0.2)
    seq.rec <- seq(from=input$rec_range[1],to=input$rec_range[2],by=input$plot_interval_p1)
    rec.max <- input$rec_range[2]
    seq.rec <- unique(append(seq.rec, rec.max))
    # print (seq.rec)
    # seq.kd <- seq(from=input$kd_range[1],to=input$kd_range[2],by=1)
    seq.kd <- seq(from=input$kd_range[1],to=input$kd_range[2],by=input$plot_interval_kd)
    kd.max <- input$kd_range[2]
    seq.kd <- unique(append(seq.kd, kd.max))
    
    min.lig <- 10^min(seq.lig)*fixed_ligand
    max.lig <- 10^max(seq.lig)*fixed_ligand
    min.rec <- 10^min(seq.rec)*fixed_receptor
    max.rec <- 10^max(seq.rec)*fixed_receptor
    #plot( c(min.rec,max.rec), c(0.1,100), log="xy", col=0, main=paste(input$receptor_name," occupancy for fixed Kd of ", fixed_kd," pM",sep=""), xlab=paste("Concentration of ", input$receptor_name," (pM)",sep=""), ylab=paste("% ",input$receptor_name," occupancy",sep="") )
    
    #lines( c(min.lig,max.lig), c(min.rec,max.rec), col=1)
    rec.dots <- 10^seq.rec*fixed_receptor
    leggy <- c()
    my.df <- NULL
    mylist <- NULL
    for (kd.cnt in seq.kd) {
      this.kd <- 10^kd.cnt*fixed_kd
      ## now each line
      occ.dots <- c()
      for (rec.cnt in seq.rec) {
        this.rec <- 10^rec.cnt*fixed_receptor
        gfun <- function(x) {c(this.rec - x[1] - x[2]*x[1]/kdcoating - x[1]*x[3]/this.kd, 0-x[2]-x[2]*x[1]/kdcoating, fixed_ligand-x[3]-x[1]*x[3]/this.kd)}
        fred <- multiroot(gfun, start=c(this.rec/2,0/2,fixed_ligand/2), positive=TRUE )
        #gfun <- function(x) {c(this.rec - x[1] - x[2]*x[1]/kdcoating - x[1]*x[3]/this.kd, input$c_total_coating-x[2]-x[2]*x[1]/kdcoating, fixed_ligand-x[3]-x[1]*x[3]/this.kd)}
        #fred <- multiroot( gfun, start=c(this.rec/2,input$c_total_coating/2,fixed_ligand/2), positive=TRUE )
        c_free_receptor <- fred$root[1]
        c_free_coat <- fred$root[2]
        c_free_ligand <- fred$root[3]
        c_complex <- this.kd - c_free_ligand
        #pc_occ = 100*(1-c_free_receptor/fixed_receptor) # lig, not rec
        pc_occ = 100*(1-c_free_ligand/fixed_ligand) # lig, not rec
        occ.dots <- c(occ.dots, pc_occ)
        my.df <- (cbind(rec.dots, this.kd, occ.dots))
      }
      mylist <- data.frame(rbind(mylist, my.df))
    }
    
    if (input$p1concunit == "metric units"){
      print("this is ridiculous")
      rec.dots <- 10^seq.rec*input$c_total_receptor_start
      this.kd <- 10^seq.kd*input$KD
      occ.dots <- mylist[,3]
      
      temp <- NULL
      for (i in 1:length(this.kd)){
        temp[[i]] <- cbind(rec.dots, this.kd[i])
      }
      temp <- do.call(rbind.data.frame, temp)
      colnames(temp)[2] <- "this.kd"
      
      mylist <- cbind.data.frame(temp, occ.dots)
      # str(mylist)
      # print(mylist)
      
    }
    
    # for (i in 1:nrow(mylist)){
    #   
    #   mylist$rec.dots[i] <- round(convert_units_back_p1 (mylist$rec.dots[i]),2)
    #   # mylist$this.lig[i] <- round(convert_units_back_p2 (mylist$this.lig[i]),2)
    # }
    return(mylist)
  })
  
  
  
  
  
  observeEvent(input$myconfirmation_submit_binding, {
    if (!is.null (input$myconfirmation_submit_binding) ){
      # observeEvent(c(input$p1unit_metric, input$p2unit_molar, 
      #                input$p2unit_metric, input$p2unit_molar,input$scale, input$c_total_ligand, input$c_total_receptor, input$lig_range,input$rec_range,input$kd_range,
      #                input$submit_slider_range_p1, input$submit_slider_range_p2, input$submit_slider_range_kd), {    
      
      output$roccbyrecrangeligplot2 <- renderPlotly({
        isolate({
          print(inputRoccbyrecrangeligplot2())
        })
      }) 
    }              
  }) 
  
  inputRoccbyrecrangeligplot2 <- reactive({
    
    mylist <- roccbyrecrangeligdata()
    mylist_spread <- spread(mylist, key= "this.kd", value =  "occ.dots")
    thinned_spread = mylist_spread[seq(1, nrow(mylist_spread), 1), ]
    thinned_gather <-  gather(thinned_spread[ ,2:ncol(thinned_spread)], key= "this.kd", value =  "occ.dots")
    thinned_gather_new <- cbind(thinned_spread[ ,1], thinned_gather)
    names(thinned_gather_new)[1] <- 'rec.dots'
    
    if (input$p1concunit == "metric units"){
      
      my.units <- input$p1unit_metric
      my.conc <- input$c_total_receptor_start
    }else {
      my.units <- input$p1unit_molar
      my.conc <- input$c_total_receptor
    }
    
    if (input$p2concunit == "metric units"){
      
      my.unitsp2 <- input$p2unit_metric
      
    }else {
      my.unitsp2 <- input$p2unit_molar
    }
    
    q <- plot_ly (data = mylist,
                  x = ~rec.dots) %>%
      add_trace (y = ~occ.dots, mode = 'lines', color = ~factor(this.kd)) %>%
      add_trace (data = thinned_gather_new,
                 y = ~ occ.dots, mode = 'markers', color = ~factor(this.kd),
                 showlegend = F, showticklabels = FALSE) %>%
      layout(title = paste("Fractional occupancy of", em(input$ligand_name),"at a fixed",  em(input$ligand_name), "concentration of", 
                           em(input$c_total_ligand_start) ,my.unitsp2),
             font = list(size = 9),
             yaxis = list (title = paste("% ",input$ligand_name," occupancy",sep="")),  
             xaxis = list(type = "log", autotick = F,titlefont = list(size = 14),tickfont = list(size = 12),
                          title = paste("Concentration of ", input$receptor_name," (", my.units, ")",sep="")))
    
    # q <- ggplot(mylist, aes(x=rec.dots, y=occ.dots, group=this.kd, colour = factor(this.kd)))+geom_line() +
    #   #geom_point(data= mylist[mylist$rec.dots %in% c(1, 10, 100, 1000, 10000), ]) +
    #   geom_point(data= thinned_gather_new, aes(x=rec.dots, y=occ.dots, group=this.kd, colour = factor(this.kd))) +
    #   scale_x_continuous(trans = "log10") +
    #   #scale_y_continuous(trans = "log10") +
    #   #ggtitle(paste(input$receptor_name," occupancy for fixed Kd of ", fixed_kd," pM",sep="")) +
    #   ggtitle(HTML(paste("Fractional occupancy of", em(input$ligand_name),"at a fixed",  em(input$ligand_name), "concentration of", 
    #                      em(input$c_total_ligand_start) ,input$p2unit))) +
    #   xlab(paste("Concentration of ", input$receptor_name," (", my.units, ")",sep="")) +
    #   # xlab(paste("Concentration of ", input$receptor_name," (", my.conc, " ", my.units, " )",sep="")) +
    #   ylab(paste("% ",input$ligand_name," occupancy",sep="")) + ylim(0,100) +
    #   scale_color_brewer(palette = "Paired") +
    #   theme(legend.position = "bottom", plot.title = element_text(size=10)) +
    #   labs(color = paste("KD", sep = ""))
    
    # output$roccbyrecrangeligplot2 <- renderPlotly({ 
    if(input$scale == 1){
      
      q %>%
        config(displayModeBar = FALSE)%>%
        config(displaylogo = FALSE) %>%
        config(collaborate = FALSE) %>%
        layout(legend = list(
          orientation = "h",
          x = 0.225, y = -0.3,
          font = list(size = 12)
        )
        )%>% 
        add_annotations( text=paste("K<sub>D</sub>", " (", "pM", ") ", sep = ""), xref="paper", yref="paper",
                         x=0.125, xanchor="left",
                         y=-0.385, yanchor="bottom",    # Same y as legend below
                         legendtitle=TRUE, showarrow=FALSE,
                         font=list(size=12,face="bold")) 
      
    } else if (input$scale==2){
      q <- q %>% layout(yaxis = list(type = "log")) 
      
      q %>%
        config(displayModeBar = FALSE)%>%
        config(displaylogo = FALSE) %>%
        config(collaborate = FALSE) %>%
        layout(legend = list(
          orientation = "h",
          x = 0.225, y = -0.3,
          font = list(size = 12)
        ),
        yaxis = list(autotick = F)
        )%>% 
        add_annotations( text=paste("K<sub>D</sub>", " (", "pM", ") ", sep = ""), xref="paper", yref="paper",
                         x=0.125, xanchor="left",
                         y=-0.385, yanchor="bottom",    # Same y as legend below
                         legendtitle=TRUE, showarrow=FALSE,
                         font=list(size=12,face="bold")) 
      
      
    }
    
  })
  
  
  inputTable4 <- reactive ({ 
    
    mylist <- roccbyrecrangeligdata()
    mylist_spread <- spread(mylist, key= "this.kd", value =  "occ.dots")
    thinned_spread = mylist_spread[seq(1, nrow(mylist_spread), 1), ]
    names(thinned_spread)[1] <- ""
    thinned_spread <- round(thinned_spread, 2)
    
  })
  
  
  observeEvent(input$myconfirmation_submit_binding, {
    if (!is.null (input$myconfirmation_submit_binding) ){
      output$Table4 <- renderDataTable({
        
        isolate ({ 
          if (input$adv_slider_p1 | input$adv_slider_p2 | input$adv_slider_kd){
            # if (input$adv_options){    
            validate(
              need(input$submit_slider_range_p1 | input$submit_slider_range_p2 | input$submit_slider_range_kd, 
                   "Please provide advanced plotting parameters")
            )
            
            if (input$submit_slider_range_p1 | input$submit_slider_range_p1 | input$submit_slider_range_p1){
              
              if (input$p1concunit == "metric units"){
                name_partner1 <- HTML('Concentration of ' ,input$receptor_name, "<br>", "(", input$p1unit_metric, ")")
                
                name_partner1 <- HTML('Concentration of ' ,input$receptor_name, "<br>", "(", input$p1unit_metric, ")")
                name_partner2 <- HTML('KD'," (pM)", "")
              }else {
                name_partner1 <- HTML('Concentration of ' ,input$receptor_name, "<br>", "(", input$p1unit_molar, ")")
                
                name_partner1 <- HTML('Concentration of ' ,input$receptor_name, "<br>", "(", input$p1unit_molar, ")")
                name_partner2 <- HTML('KD'," (pM)", "")
              }
              
              # mylist <- roccbyrecrangeligdata()
              # mylist_spread <- spread(mylist, key= "this.kd", value =  "occ.dots")
              # thinned_spread = mylist_spread[seq(1, nrow(mylist_spread), 5), ]
              # names(thinned_spread)[1] <- ""
              # thinned_spread <- round(thinned_spread, 2)
              
              thinned_spread <- inputTable4()
              
              sketch <- createContainer(name_partner1, name_partner2, thinned_spread)
              
              datatable(
                thinned_spread,
                extensions = 'Buttons',
                class = "cell-border stripe hover",
                container = sketch,
                rownames = FALSE, 
                
                options = list(
                  pageLength = 7,
                  dom = 'tprB',
                  scrollX = TRUE,
                  buttons = list(
                    c('copy', 'excel'))
                )
              )%>%
                formatStyle(thinned_spread [ ,1], fontWeight = "bold")
              
              
            }
            
          }else {
            
            if (input$p1concunit == "metric units"){
              name_partner1 <- HTML('Concentration of ' ,input$receptor_name, "<br>", "(", input$p1unit_metric, ")")
              name_partner2 <- HTML('KD'," (pM)", "")
            }else{
              
              name_partner1 <- HTML('Concentration of ' ,input$receptor_name, "<br>", "(", input$p1unit_molar, ")")
              name_partner2 <- HTML('KD'," (pM)", "")
            }   
            # mylist <- roccbyrecrangeligdata()
            # mylist_spread <- spread(mylist, key= "this.kd", value =  "occ.dots")
            # thinned_spread = mylist_spread[seq(1, nrow(mylist_spread), 5), ]
            # names(thinned_spread)[1] <- ""
            # thinned_spread <- round(thinned_spread, 2)
            
            thinned_spread <- inputTable4()
            
            sketch <- createContainer(name_partner1, name_partner2, thinned_spread)
            
            datatable(
              thinned_spread,
              extensions = 'Buttons',
              class = "cell-border stripe hover",
              container = sketch,
              rownames = FALSE, 
              
              options = list(
                pageLength = 7,
                dom = 'tprB',
                scrollX = TRUE,
                buttons = list(
                  c('copy', 'excel'))
              )
            )%>%
              formatStyle(thinned_spread [ ,1], fontWeight = "bold")
            
          }
        })
      })
    }
  })
  
  
  observeEvent(input$myconfirmation_submit_binding, {
    if (!is.null (input$myconfirmation_submit_binding) ){
      output$time_to_equilibrium_plot <- renderPlotly({
        isolate ({
          print(inputTime_to_equilibrium_plot())
        })
        
      })
    }
  })
  
  
  inputTime_to_equilibrium_plot <- reactive({
    
    validate(
      # need(input$kon != 0 & input$koff != 0, "Time to equilibrium cannot be plotted when only KD option is selected")
      need(input$constants != "only KD", "Time to equilibrium cannot be plotted when only KD option is selected")
    )
    
    ## free ligand calculation
    volume <- 1
    kdreclig <- input$KD #input$koff/input$kon*1e12
    kdcoating <- 10^(12-0) #input$pKdcoat[1]
    
    
    
    gfun <- function(x) {c(input$c_total_receptor - x[1] - x[2]*x[1]/kdcoating - x[1]*x[3]/kdreclig, 0-x[2]-x[2]*x[1]/kdcoating, input$c_total_ligand-x[3]-x[1]*x[3]/kdreclig)}
    fred <- multiroot( gfun, start=c(input$c_total_receptor/2,0/2,input$c_total_ligand/2), positive=TRUE )
    
    c_free_receptor <- fred$root[1]
    c_free_coat <- fred$root[2]
    c_free_ligand <- fred$root[3]
    pc_occ_ligand <- 100*(1-c_free_ligand/input$c_total_ligand)
    ## tte
    fold.halflife <- 4
    kd <- input$KD
    koffpers <- input$kon*1e-12*kd
    time_to_hours1 <- (fold.halflife*0.693*1/koffpers)/3600
    time_to_hours2 <- (fold.halflife*0.693*1/(koffpers*input$c_total_receptor/kd))/3600
    time_to_hours3 <- (fold.halflife*0.693*1/(koffpers*input$c_total_ligand/kd))/3600
    time_to_hours <- min(time_to_hours1,time_to_hours2,time_to_hours3)
    ## now the ode bit
    binding_model <- function( Time, State, pars ) {
      with(as.list(c(State,pars)), {
        R <- State[1]  # receptor
        L <- State[2]
        X <- State[3]
        R_ode <- -konperpms*R*L + koffpers*X
        L_ode <- -konperpms*R*L + koffpers*X
        X_ode <- konperpms*R*L - koffpers*X
        return(list(c(R_ode,L_ode,X_ode)))
      })
    }
    inits <- c(R=input$c_total_receptor,L=input$c_total_ligand, X=0)
    pars <- c(konperpms=input$kon*1e-12,koff=koffpers)
    seq.times <- seq(from=0, to=2*time_to_hours*3600, by = 2*time_to_hours*36)   # plot 100 points-worth
    out <- data.frame(ode(y=inits, times=seq.times, func=binding_model, parms=pars, atol=1e-4,rtol=1e-4))
    time.points <- out$time/3600  # convert to hours
    yaxis.points <- 100*out$L/input$c_total_ligand
    plot( time.points, yaxis.points, type="l", main="Time to Equilibrium", xlab="Time (Hours)", ylab=paste("% ",input$ligand_name," free",sep=""), lwd=2, ylim=c(0,100))
    lines( c(0,max(time.points)), c(100-pc_occ_ligand, 100-pc_occ_ligand), lty=2 )
    
    # q <- ggplot(out , aes(x=time/3600, y=100*L/input$c_total_ligand))+
    #    geom_line() +
    #     xlab(paste("Time in hours",sep="")) +
    #     ylab(paste("% Free of ",input$ligand_name, sep="")) +
    #    ggtitle ("Time to equilibrium") +
    #    theme(plot.title = element_text(size=10))
    
    ###
    
    
    q <- plot_ly (data = out,
                  x = ~time/3600) %>%
      add_trace (y = ~100*L/input$c_total_ligand, mode = 'lines') %>%
      #add_markers() %>%
      layout(title = "Time to equilibrium",
             #annotations = time_list,
             yaxis = list (title = paste("% Free of ",input$ligand_name, sep="")),  
             xaxis = list(title = paste("Time in hours",sep="")))
    
    time_x = get_TimeToEql()
    time_x_in_data <- time_x * 3600 * 0.77
    time_y_in_data <- subset(out, round(time) == time_x_in_data)$L
    time_y <- (time_y_in_data * 100)/input$c_total_ligand
    
    
    
    
    q %>%
      config(displayModeBar = FALSE)%>%
      config(displaylogo = FALSE) %>%
      config(collaborate = FALSE) %>%
      layout(legend = list(
        orientation = "h",
        x = 0.15, y = -0.2,
        font = list(size = 12)
      )
      ) %>%
      add_annotations("Time to eq.", x = time_x, y = time_y)
    
    
  })
  
  output$cxdilutionplot <-renderPlotly({
    dilutions <- 2^(0:8)
    c_complex <- c()
    for (dily in dilutions) {
      volume <- 1
      kdreclig <- input$KD #input$koff/input$kon*1e12
      kdcoating <- 10^(12-0)
      gfun <- function(x) {c(input$c_total_receptor/dily - x[1] - x[2]*x[1]/kdcoating - x[1]*x[3]/kdreclig, 0/dily-x[2]-x[2]*x[1]/kdcoating, input$c_total_ligand/dily-x[3]-x[1]*x[3]/kdreclig)}
      fred <- multiroot( gfun, start=c(input$c_total_receptor/2/dily,0/2/dily,input$c_total_ligand/2/dily), positive=TRUE )
      c_free_receptor <- fred$root[1]
      c_free_comp <- fred$root[2]
      c_free_ligand <- fred$root[3]
      c_complex <- c(c_complex, input$c_total_ligand/dily - c_free_ligand)
    }
    temp <- data.frame(cbind(dilutions,c_complex))
    
    # q <- ggplot(temp , aes(x=dilutions, y=c_complex))+
    #  geom_line() +
    #  scale_x_continuous(trans = "log10") +
    #  xlab("1:n dilution") +
    #  ylab(paste(input$receptor_name,"-",input$ligand_name," complex (pM)",sep="")) +
    #  ggtitle (HTML(input$receptor_name, " - ", input$ligand_name, " in complex (pM) following sample dilution")) +
    #  theme(plot.title = element_text(size=10))
    
    q <- plot_ly (data = temp,
                  x = ~dilutions) %>%
      add_trace (y = ~c_complex, mode = 'lines') %>%
      layout(title = paste(input$receptor_name, " - ", input$ligand_name, " in complex (pM) following sample dilution"),
             yaxis = list (title = paste(input$receptor_name,"-",input$ligand_name," complex (pM)",sep="")),  
             xaxis = list(type = "log", autotick = F,
                          title = "1:n dilution"))
    
    q %>%
      config(displayModeBar = FALSE)%>%
      config(displaylogo = FALSE) %>%
      config(collaborate = FALSE) %>%
      layout(legend = list(
        orientation = "h",
        x = 0.15, y = -0.2,
        font = list(size = 12)
      )
      )
    
    
  })
  
  output$dilutionTable1 <- renderDataTable({
    dilutions <- 2^(0:8)
    c_complex <- c()
    for (dily in dilutions) {
      volume <- 1
      kdreclig <- input$KD #input$koff/input$kon*1e12
      kdcoating <- 10^(12-0)
      gfun <- function(x) {c(input$c_total_receptor/dily - x[1] - x[2]*x[1]/kdcoating - x[1]*x[3]/kdreclig, 0/dily-x[2]-x[2]*x[1]/kdcoating, input$c_total_ligand/dily-x[3]-x[1]*x[3]/kdreclig)}
      fred <- multiroot( gfun, start=c(input$c_total_receptor/2/dily,0/2/dily,input$c_total_ligand/2/dily), positive=TRUE )
      c_free_receptor <- fred$root[1]
      c_free_comp <- fred$root[2]
      c_free_ligand <- fred$root[3]
      c_complex <- c(c_complex, input$c_total_ligand/dily - c_free_ligand)
    }
    temp <- data.frame(cbind(dilutions,c_complex))
    
    datatable(round(t(temp), 2),
              #Concentration partner 1 – partner 2 complex (pM)
              colnames = paste("Concentration ",input$receptor_name, " - ", input$ligand_name," complex (pM)",sep=""), 
              class = "cell-border stripe hover",
              extensions = 'Buttons',
              options = list(dom = 'tprB', 
                             paging = FALSE,
                             scrollX = TRUE,
                             buttons = list(
                               c('copy', 'excel'))
              )
    )
    
    # datatable(
    #   thinned_spread,
    #   extensions = 'Buttons',
    #   class = "cell-border stripe hover",
    #   container = sketch,
    #   rownames = FALSE, 
    #   
    #   options = list(
    #     pageLength = 7,
    #     dom = 'tprB',
    #     scrollX = TRUE,
    #     buttons = list(
    #       c('copy', 'excel'))
    #   )
    # )%>%
    #   formatStyle(thinned_spread [ ,1], fontWeight = "bold")
  })
  
  output$occdilutionplot <-renderPlotly({
    dilutions <- 2^(0:8)
    pc_occ <- c()
    for (dily in dilutions) {
      volume <- 1
      kdreclig <- input$KD #input$koff/input$kon*1e12
      kdcoating <- 10^(12-0) #10^(12-input$pKdcoat[1])
      gfun <- function(x) {c(input$c_total_receptor/dily - x[1] - x[2]*x[1]/kdcoating - x[1]*x[3]/kdreclig, 0/dily-x[2]-x[2]*x[1]/kdcoating, input$c_total_ligand/dily-x[3]-x[1]*x[3]/kdreclig)}
      fred <- multiroot( gfun, start=c(input$c_total_receptor/2/dily,0/2/dily,input$c_total_ligand/2/dily), positive=TRUE )
      c_free_receptor <- fred$root[1]
      c_free_comp <- fred$root[2]
      c_free_ligand <- fred$root[3]
      pc_occ <- c( pc_occ, 100*(1-c_free_ligand*dily/input$c_total_ligand))
    }
    
    temp <- data.frame(cbind(dilutions,pc_occ))
    # q <- ggplot(temp , aes(x=dilutions, y=pc_occ))+
    #   geom_line() +
    #   scale_x_continuous(trans = "log10") +
    #   xlab("1:n dilution") +
    #   ylab(paste("% ",input$ligand_name," occupancy",sep="")) +
    #   theme(plot.title = element_text(size=10)) +
    #   ggtitle (HTML("Fractional occupancy of  ", input$ligand_name, " following sample dilution") )
    
    
    q <- plot_ly (data = temp,
                  x = ~dilutions) %>%
      add_trace (y = ~pc_occ, mode = 'lines') %>%
      layout(title = HTML("Fractional occupancy of  ", input$ligand_name, " following sample dilution"),
             yaxis = list (title = paste("% ",input$ligand_name," occupancy",sep="")),  
             xaxis = list(type = "log", autotick = F,
                          title = "1:n dilution"))
    
    q %>%
      config(displayModeBar = FALSE)%>%
      config(displaylogo = FALSE) %>%
      config(collaborate = FALSE) %>%
      layout(legend = list(
        orientation = "h",
        x = 0.15, y = -0.2,
        font = list(size = 12)
      )
      )
    
    
  })
  
  output$dilutionTable2 <- renderDataTable({
    dilutions <- 2^(0:8)
    pc_occ <- c()
    for (dily in dilutions) {
      volume <- 1
      kdreclig <- input$KD #input$koff/input$kon*1e12
      kdcoating <- 10^(12-0) #10^(12-input$pKdcoat[1])
      gfun <- function(x) {c(input$c_total_receptor/dily - x[1] - x[2]*x[1]/kdcoating - x[1]*x[3]/kdreclig, 0/dily-x[2]-x[2]*x[1]/kdcoating, input$c_total_ligand/dily-x[3]-x[1]*x[3]/kdreclig)}
      fred <- multiroot( gfun, start=c(input$c_total_receptor/2/dily,0/2/dily,input$c_total_ligand/2/dily), positive=TRUE )
      c_free_receptor <- fred$root[1]
      c_free_comp <- fred$root[2]
      c_free_ligand <- fred$root[3]
      pc_occ <- c( pc_occ, 100*(1-c_free_ligand*dily/input$c_total_ligand))
    }
    
    temp <- data.frame(cbind(dilutions,pc_occ))
    
    datatable(round(t(temp), 2), 
              colnames = paste("% ",input$ligand_name," occupancy",sep=""), 
              class = "cell-border stripe hover",
              extensions = 'Buttons',
              options = list(dom = 'tprB', 
                             paging = FALSE,
                             scrollX = TRUE,
                             buttons = list(
                               c('copy', 'excel'))
              )
    )
    
  })
  
  output$roccdilutionplot <-renderPlotly({
    dilutions <- 2^(0:8)
    pc_occ <- c()
    for (dily in dilutions) {
      volume <- 1
      kdreclig <- input$KD #input$koff/input$kon*1e12
      kdcoating <- 10^(12-0) #10^(12-input$pKdcoat[1])
      gfun <- function(x) {c(input$c_total_receptor/dily - x[1] - x[2]*x[1]/kdcoating - x[1]*x[3]/kdreclig, 0/dily-x[2]-x[2]*x[1]/kdcoating, input$c_total_ligand/dily-x[3]-x[1]*x[3]/kdreclig)}
      fred <- multiroot( gfun, start=c(input$c_total_receptor/2/dily,0/2/dily,input$c_total_ligand/2/dily), positive=TRUE )
      c_free_receptor <- fred$root[1]
      c_free_comp <- fred$root[2]
      c_free_ligand <- fred$root[3]
      pc_occ <- c( pc_occ, 100*(1-c_free_receptor*dily/input$c_total_receptor))
    }
    temp <- data.frame(cbind(dilutions,pc_occ))
    
    # q <- ggplot(temp , aes(x=dilutions, y=pc_occ))+
    #   geom_line() +
    #   scale_x_continuous(trans = "log10") +
    #   xlab("1:n dilution") +
    #   ylab(paste("% ",input$receptor_name," occupancy",sep="")) +
    #   ggtitle (HTML("Fractional occupancy of  ", input$receptor_name, " following sample dilution")) +
    #   theme(plot.title = element_text(size=10))
    
    q <- plot_ly (data = temp,
                  x = ~dilutions) %>%
      add_trace (y = ~pc_occ, mode = 'lines') %>%
      layout(title = HTML("Fractional occupancy of  ", input$receptor_name, " following sample dilution"),
             yaxis = list (title = paste("% ",input$receptor_name," occupancy",sep="")),  
             xaxis = list(type = "log", autotick = F,
                          title = "1:n dilution"))
    
    q %>%
      config(displayModeBar = FALSE)%>%
      config(displaylogo = FALSE) %>%
      config(collaborate = FALSE) %>%
      layout(legend = list(
        orientation = "h",
        x = 0.15, y = -0.2,
        font = list(size = 12)
      )
      )
    
    
    #plot( dilutions, pc_occ, type="l", log="x", col=1, main=paste("% ",input$receptor_name," occupancy following dilution",sep=""), xlab="1:n dilution",ylab=paste("% ",input$receptor_name," occupancy",sep=""),lwd=2)
  })
  
  output$dilutionTable3 <- renderDataTable({
    dilutions <- 2^(0:8)
    pc_occ <- c()
    for (dily in dilutions) {
      volume <- 1
      kdreclig <- input$KD #input$koff/input$kon*1e12
      kdcoating <- 10^(12-0) #10^(12-input$pKdcoat[1])
      gfun <- function(x) {c(input$c_total_receptor/dily - x[1] - x[2]*x[1]/kdcoating - x[1]*x[3]/kdreclig, 0/dily-x[2]-x[2]*x[1]/kdcoating, input$c_total_ligand/dily-x[3]-x[1]*x[3]/kdreclig)}
      fred <- multiroot( gfun, start=c(input$c_total_receptor/2/dily,0/2/dily,input$c_total_ligand/2/dily), positive=TRUE )
      c_free_receptor <- fred$root[1]
      c_free_comp <- fred$root[2]
      c_free_ligand <- fred$root[3]
      pc_occ <- c( pc_occ, 100*(1-c_free_receptor*dily/input$c_total_receptor))
    }
    temp <- data.frame(cbind(dilutions,pc_occ))
    
    datatable(round(t(temp), 2), 
              colnames = paste("% ",input$receptor_name," occupancy",sep=""), 
              class = "cell-border stripe hover",
              extensions = 'Buttons',
              options = list(dom = 'tprB', 
                             paging = FALSE,
                             scrollX = TRUE,
                             buttons = list(
                               c('copy', 'excel'))
              )
    )
  })
  
  
  
#competition tab
  
  #switch p1 and p2
  observeEvent(input$switch_p1p2_CT, {
    
    if (input$switch_p1p2_CT){
      
      #if metric units 
      if (input$p1concunit_CT == "metric units"){
        
        my.units <- input$p1unit_CT_metric
        my.conc <- input$c_total_receptor_CT_start
        
        temp_units <- my.units
        temp_conc <- my.conc
        temp_molw <- input$mol_weight_p1_CT
        
        
        #update p1
        updateSelectInput(session, "p1unit_CT_metric", selected = input$p2unit_CT_metric)
        updateNumericInput(session, "c_total_receptor_CT_start", value = input$c_total_ligand_CT_start)
        updateNumericInput(session,  "mol_weight_p1_CT", value = input$mol_weight_p2_CT)
        
        #update p2
        updateSelectInput(session, "p2unit_CT_metric", selected = temp_units)
        updateNumericInput(session, "c_total_ligand_CT_start", value = temp_conc)
        updateNumericInput(session,  "mol_weight_p2_CT", value = temp_molw)
        
      }else if (input$p1concunit_CT == "molar units"){

        my.units <- input$p1unit_CT_molar
        my.conc <- input$c_total_receptor_CT_start
        
        temp_units <- my.units
        temp_conc <- my.conc
        
        #update p1
        updateSelectInput(session, "p1unit_CT_molar", selected = input$p2unit_CT_molar)
        updateNumericInput(session, "c_total_receptor_CT_start", value = input$c_total_ligand_CT_start)
        
        #update p2
        updateSelectInput(session, "p2unit_CT_molar", selected = temp_units)
        updateNumericInput(session, "c_total_ligand_CT_start", value = temp_conc)
        
      }
      
    }
  })
  
  observeEvent(c(input$p1concunit_CT, input$c_total_receptor_CT_start, input$mol_weight_p1_CT,input$p1unit_CT_metric,
                 input$p1unit_CT_molar),  {
                   
                   if (input$p1concunit_CT == "metric units"){
                     
                     
                     shinyjs::show("mol_weight_p1_CT")
                     shinyjs::hide("p1unit_CT_molar")
                     shinyjs::show("p1unit_CT_metric")
                     updateRadioButtons(session, "p2concunit_CT", selected = "metric units")
                     updateRadioButtons(session, "coat_concunit_CT", selected = "metric units")
                     
                     
                     temp <- input$c_total_receptor_CT_start
                     if (input$p1unit_CT_metric == "ug/ml"){
                       
                       temp = format  (input$c_total_receptor_CT_start / 1e6 / input$mol_weight_p1_CT * 10^12 * 1000, scientific = T)
                     }else if (input$p1unit_CT_metric == "mg/ml"){
                       
                       temp = format  (input$c_total_receptor_CT_start / 1e3 / input$mol_weight_p1_CT * 10^12 * 1000, scientific = T)
                     }else if (input$p1unit_CT_metric == "ng/ml"){
                       
                       temp = format  (input$c_total_receptor_CT_start / 1e9 / input$mol_weight_p1_CT * 10^12 * 1000, scientific = T)
                     }else if (input$p1unit_CT_metric == "pg/ml"){
                       
                       temp = format  (input$c_total_receptor_CT_start / 1e12/ input$mol_weight_p1_CT * 10^12 * 1000, scientific = T)
                     }
                     
                     
                     updateNumericInput(session, "c_total_receptor_CT", label = ("Conversion of partner 1 "),
                                        value = temp)
                     
                     
                     # )
                     
                   } else if (input$p1concunit_CT == "molar units"){
                     
                     
                     
                     shinyjs::hide("mol_weight_p1_CT")
                     shinyjs::show("p1unit_CT_molar")
                     shinyjs::hide("p1unit_CT_metric")
                     updateRadioButtons(session, "p2concunit_CT", selected = "molar units")
                     updateRadioButtons(session, "coat_concunit_CT", selected = "molar units")
                     
                     temp <- input$c_total_receptor_CT_start
                     
                     if (input$p1unit_CT_molar == "pM"){
                       
                       temp = format  (input$c_total_receptor_CT_start, scientific = T)
                       
                     } else  if (input$p1unit_CT_molar == "uM"){
                       print ("uM")
                       
                       temp = format  (input$c_total_receptor_CT_start / 1e-6 , scientific = T)
                       
                     } else if (input$p1unit_CT_molar == "fM"){
                       
                       temp = format  (input$c_total_receptor_CT_start / 1e3 , scientific = T)
                       
                     } else if (input$p1unit_CT_molar == "nM"){
                       
                       temp = format  (input$c_total_receptor_CT_start / 1e-3, scientific = T)
                     } else if (input$p1unit_CT_molar == "mM"){
                       
                       temp = format  (input$c_total_receptor_CT_start / 1e-9, scientific = T)
                       print(temp)
                     }
                     
                     print(temp)
                     updateNumericInput(session, "c_total_receptor_CT", label = ("Conversion of partner 1  "),
                                        value = format(temp, scientific=TRUE))
                   }
                   
                 })
  
  
  
  observeEvent(c(input$coat_concunit_CT, input$c_total_coating_CT_start, input$mol_weight_coat, input$coat_unit_CT_metric, 
                 input$coat_unit_CT_molar),  {
                   
                   if (input$coat_concunit_CT == "metric units"){
                     
                     shinyjs::show("mol_weight_coat")
                     shinyjs::hide("coat_unit_CT_molar")
                     shinyjs::show("coat_unit_CT_metric")
                     updateRadioButtons(session, "p2concunit_CT", selected = "metric units")
                     updateRadioButtons(session, "p1concunit_CT", selected = "metric units")
                     
                     # output$converted_concentration <-  renderText(
                     
                     
                     temp <- input$c_total_coating_CT_start
                     if (input$coat_unit_CT_metric == "ug/ml"){
                       
                       temp = format  (input$c_total_coating_CT_start / 1e6 / input$mol_weight_coat * 10^12 * 1000, scientific = T)
                     }else if (input$coat_unit_CT_metric == "mg/ml"){
                       
                       temp = format  (input$c_total_coating_CT_start / 1e3 / input$mol_weight_coat * 10^12 * 1000, scientific = T)
                     }else if (input$coat_unit_CT_metric == "ng/ml"){
                       
                       temp = format  (input$c_total_coating_CT_start / 1e9 / input$mol_weight_coat * 10^12 * 1000, scientific = T)
                     }else if (input$coat_unit_CT_metric == "pg/ml"){
                       
                       temp = format  (input$c_total_coating_CT_start / 1e12/ input$mol_weight_coat * 10^12 * 1000, scientific = T)
                     }
                     
                     
                     updateNumericInput(session, "c_total_coating_CT", label = ("Conversion of competitor "),
                                        value = temp)
                     
                     
                     # )
                     
                   } else if (input$coat_concunit_CT == "molar units"){
                     
                     
                     shinyjs::hide("mol_weight_coat")
                     shinyjs::show("coat_unit_CT_molar")
                     shinyjs::hide("coat_unit_CT_metric")
                     updateRadioButtons(session, "p2concunit_CT", selected = "molar units")
                     updateRadioButtons(session, "p1concunit_CT", selected = "molar units")
                     
                     temp <- input$c_total_coating_CT_start
                     
                     if (input$coat_unit_CT_molar == "pM"){
                       
                       temp = format  (input$c_total_coating_CT_start, scientific = T)
                       
                     } else  if (input$coat_unit_CT_molar == "uM"){
                       print ("uM")
                       
                       temp = format  (input$c_total_coating_CT_start / 1e-6 , scientific = T)
                       
                     } else if (input$coat_unit_CT_molar == "fM"){
                       
                       temp = format  (input$c_total_coating_CT_start / 1e3 , scientific = T)
                       
                     } else if (input$coat_unit_CT_molar == "nM"){
                       
                       temp = format  (input$c_total_coating_CT_start / 1e-3, scientific = T)
                     } else if (input$coat_unit_CT_molar == "mM"){
                       
                       temp = format  (input$c_total_coating_CT_start / 1e-9, scientific = T)
                       print(temp)
                     }
                     
                     print(temp)
                     updateNumericInput(session, "c_total_coating_CT", label = ("Conversion of competitor  "),
                                        value = format(temp, scientific=TRUE))
                   }
                   
                 })
  
  observeEvent (input$adv_slider_p1_CT, { 
    
    if (input$adv_slider_p1_CT){
      
      shinyjs::show("plot_start_p1_CT")
      shinyjs::show("plot_end_p1_CT")
      shinyjs::show("plot_interval_p1_CT")
      shinyjs::show("submit_slider_range_p1_CT")
      
      updateNumericInput(session, "plot_start_p1_CT", value = input$rec_range_CT[1])
      updateNumericInput(session, "plot_end_p1_CT" , value = input$rec_range_CT[2])
      
      shinyjs::disable("rec_range_CT")
    }else {
      shinyjs::hide ("plot_start_p1_CT")
      shinyjs::hide ("plot_end_p1_CT")
      shinyjs::hide ("plot_interval_p1_CT")
      shinyjs::hide("submit_slider_range_p1_CT")
      shinyjs::enable("rec_range_CT")
    }
    
  })
  
  observeEvent (input$submit_slider_range_p1_CT, { 
    
    if (input$submit_slider_range_p1_CT){
      
      shinyjs::hide ("plot_start_p1_CT")
      shinyjs::hide ("plot_end_p1_CT")
      shinyjs::hide ("plot_interval_p1_CT")
      shinyjs::hide("submit_slider_range_p1_CT")
      shinyjs::enable("rec_range_CT")
      
      updateSliderInput(session, "rec_range_CT", label = "Concentration range to plot (log units)",
                        # min = input$plot_start_p1_CT, max = input$plot_end_p1_CT,
                        step = 0.1,
                        value = c(input$plot_start_p1_CT, input$plot_end_p1_CT))
      # sliderInput("rec_range", "Concentration range to plot (log units)", min=-6, max=6, value=c(-2,2), step=1 ), 
      
      updateMaterialSwitch(session, "adv_slider_p1_CT", value = FALSE)
      
    }else {
      
      
      shinyjs::show("plot_start_p1_CT")
      shinyjs::show("plot_end_p1_CT")
      shinyjs::show("plot_interval_p1_CT")
      shinyjs::show("submit_slider_range_p1_CT")
      shinyjs::disable("rec_range_CT")
    }
    
  })
  
  observeEvent (input$adv_slider_p2_CT, { 
    
    if (input$adv_slider_p2_CT){
      
      shinyjs::show("plot_start_p2_CT")
      shinyjs::show("plot_end_p2_CT")
      shinyjs::show("plot_interval_p2_CT")
      shinyjs::show("submit_slider_range_p2_CT")
      
      updateNumericInput(session, "plot_start_p2_CT", value = input$lig_range_CT[1])
      updateNumericInput(session, "plot_end_p2_CT" , value = input$lig_range_CT[2])
      # updateNumericInput(session, "plot_interval", value = )
      
      
      
      shinyjs::disable("lig_range_CT")
    }else {
      shinyjs::hide ("plot_start_p2_CT")
      shinyjs::hide ("plot_end_p2_CT")
      shinyjs::hide ("plot_interval_p2_CT")
      shinyjs::hide("submit_slider_range_p2_CT")
      shinyjs::enable("lig_range_CT")
    }
    
  })
  
  observeEvent (input$submit_slider_range_p2_CT, { 
    
    if (input$submit_slider_range_p2_CT){
      
      shinyjs::hide ("plot_start_p2_CT")
      shinyjs::hide ("plot_end_p2_CT")
      shinyjs::hide ("plot_interval_p2_CT")
      shinyjs::hide("submit_slider_range_p2_CT")
      shinyjs::enable("rec_range_CT")
      
      updateSliderInput(session, "lig_range_CT", label = "Concentration range to plot (log units)",
                        # min = input$plot_start_p2_CT, max = input$plot_end_p2_CT, 
                        step = 1,
                        value = c(input$plot_start_p2_CT, input$plot_end_p2_CT))
      # sliderInput("rec_range", "Concentration range to plot (log units)", min=-6, max=6, value=c(-2,2), step=1 ), 
      
      updateMaterialSwitch(session, "adv_slider_p2_CT", value = FALSE)
      
    }else {
      
      
      shinyjs::show("plot_start_p2_CT")
      shinyjs::show("plot_end_p2_CT")
      shinyjs::show("plot_interval_p2_CT")
      shinyjs::show("submit_slider_range_p2_CT")
      shinyjs::disable("rec_range_CT")
    }
    
  })
  
  observeEvent (input$adv_slider_kd_CT, { 
    
    if (input$adv_slider_kd_CT){
      
      shinyjs::show("plot_start_kd_CT")
      shinyjs::show("plot_end_kd_CT")
      shinyjs::show("plot_interval_kd_CT")
      shinyjs::show("submit_slider_range_kd_CT")
      
      updateNumericInput(session, "plot_start_kd_CT", value = input$kd_range_CT[1])
      updateNumericInput(session, "plot_end_kd_CT" , value = input$kd_range_CT[2])
      # updateNumericInput(session, "plot_interval", value = )
      
      # print (input$rec_range[1])
      # str (input$rec_range)
      
      shinyjs::disable("kd_range_CT")
    }else {
      shinyjs::hide ("plot_start_kd_CT")
      shinyjs::hide ("plot_end_kd_CT")
      shinyjs::hide ("plot_interval_kd_CT")
      shinyjs::hide("submit_slider_range_kd_CT")
      shinyjs::enable("kd_range_CT")
    }
    
  })
  
  observeEvent (input$submit_slider_range_kd_CT, { 
    
    if (input$submit_slider_range_kd_CT){
      
      shinyjs::hide ("plot_start_kd_CT")
      shinyjs::hide ("plot_end_kd_CT")
      shinyjs::hide ("plot_interval_kd_CT")
      shinyjs::hide("submit_slider_range_kd_CT")
      shinyjs::enable("kd_range_CT")
      
      updateSliderInput(session, "kd_range_CT", 
                        # min = input$plot_start_kd_CT, max = input$plot_end_kd_CT, 
                        step = 1,
                        value = c(input$plot_start_kd_CT, input$plot_end_kd_CT))
      
      
      updateMaterialSwitch(session, "adv_slider_kd_CT", value = FALSE)
      
    }else {
      
      
      # shinyjs::show("plot_start_kd_CT")
      # shinyjs::show("plot_end_kd_CT")
      # shinyjs::show("plot_interval_kd_CT")
      # shinyjs::show("submit_slider_range_kd_CT")
      # shinyjs::disable("kd_range_CT")
    }
    
  })
  
  observeEvent (input$adv_slider_ki, { 
    
    if (input$adv_slider_ki){
      
      shinyjs::show("plot_start_ki")
      shinyjs::show("plot_end_ki")
      shinyjs::show("plot_interval_ki")
      shinyjs::show("submit_slider_range_ki")
      
      updateNumericInput(session, "plot_start_ki", value = input$pKdcoat_CT[1])
      updateNumericInput(session, "plot_end_ki" , value = input$pKdcoat_CT[2])
      # updateNumericInput(session, "plot_interval", value = )
      
      # print (input$rec_range[1])
      # str (input$rec_range)
      
      shinyjs::disable("pKdcoat_CT")
    }else {
      shinyjs::hide ("plot_start_ki")
      shinyjs::hide ("plot_end_ki")
      shinyjs::hide ("plot_interval_ki")
      shinyjs::hide("submit_slider_range_ki")
      shinyjs::enable("pKdcoat_CT")
    }
    
  })
  
  observeEvent (input$submit_slider_range_ki, { 
    
    if (input$submit_slider_range_ki){
      
      shinyjs::hide ("plot_start_ki")
      shinyjs::hide ("plot_end_ki")
      shinyjs::hide ("plot_interval_ki")
      shinyjs::hide("submit_slider_range_ki")
      shinyjs::enable("rec_range_CT")
      
      updateSliderInput(session, "pKdcoat_CT",
                        # min = input$plot_start_ki, max = input$plot_end_ki, 
                        step = 1,
                        value = c(input$plot_start_ki, input$plot_end_ki))
      # sliderInput("rec_range", "Concentration range to plot (log units)", min=-6, max=6, value=c(-2,2), step=1 ), 
      
      updateMaterialSwitch(session, "adv_slider_ki", value = FALSE)
      
    }else {
      
      
      shinyjs::show("plot_start_ki")
      shinyjs::show("plot_end_ki")
      shinyjs::show("plot_interval_ki")
      shinyjs::show("submit_slider_range_ki")
      shinyjs::disable("rec_range_CT")
    }
    
  })
  
  observeEvent (input$adv_slider_comp, { 
    
    if (input$adv_slider_comp){
      
      shinyjs::show("plot_start_comp")
      shinyjs::show("plot_end_comp")
      shinyjs::show("plot_interval_comp")
      shinyjs::show("submit_slider_range_comp")
      
      updateNumericInput(session, "plot_start_comp", value = input$coat_range_CT[1])
      updateNumericInput(session, "plot_end_comp" , value = input$coat_range_CT[2])
      # updateNumericInput(session, "plot_interval", value = )
      
      # print (input$rec_range[1])
      # str (input$rec_range)
      
      shinyjs::disable("coat_range_CT")
    }else {
      shinyjs::hide ("plot_start_comp")
      shinyjs::hide ("plot_end_comp")
      shinyjs::hide ("plot_interval_comp")
      shinyjs::hide("submit_slider_range_comp")
      shinyjs::enable("coat_range_CT")
    }
    
  })
  
  observeEvent (input$submit_slider_range_comp, { 
    
    if (input$submit_slider_range_comp){
      
      shinyjs::hide ("plot_start_comp")
      shinyjs::hide ("plot_end_comp")
      shinyjs::hide ("plot_interval_comp")
      shinyjs::hide("submit_slider_range_comp")
      shinyjs::enable("rec_range_CT")
      
      updateSliderInput(session, "ki_range", label = "Concentration range to plot (log units)",
                        # min = input$plot_start_comp, max = input$plot_end_comp, 
                        step = 1,
                        value = c(input$plot_start_comp, input$plot_end_comp))
      # sliderInput("rec_range", "Concentration range to plot (log units)", min=-6, max=6, value=c(-2,2), step=1 ), 
      
      updateMaterialSwitch(session, "adv_slider_comp", value = FALSE)
      
    }else {
      
      
      shinyjs::show("plot_start_comp")
      shinyjs::show("plot_end_comp")
      shinyjs::show("plot_interval_comp")
      shinyjs::show("submit_slider_range_comp")
      shinyjs::disable("rec_range_CT")
    }
    
  })
  
  observeEvent(c(input$p2concunit_CT, input$c_total_ligand_CT_start, 
                 input$mol_weight_p2_CT, input$p2unit_CT_molar, input$p2unit_CT_metric),  {
                   
                   if (input$p2concunit_CT == "metric units"){
                     
                     
                     shinyjs::show("mol_weight_p2_CT")
                     shinyjs::hide("p2unit_CT_molar")
                     shinyjs::show("p2unit_CT_metric")
                     updateRadioButtons(session, "p1concunit_CT", selected = "metric units")
                     updateRadioButtons(session, "p1concunit_CT", selected = "metric units")
                     
                     # output$converted_concentration <-  renderText(
                     
                     
                     temp <- input$c_total_ligand_CT_start
                     
                     if (input$p2unit_CT_metric == "ug/ml"){
                       
                       temp = format  (input$c_total_ligand_CT_start / 1e6 / input$mol_weight_p2_CT * 10^12 * 1000, scientific = T)
                     }else if (input$p2unit_CT_metric == "mg/ml"){
                       
                       temp = format  (input$c_total_ligand_CT_start / 1e3 / input$mol_weight_p2_CT * 10^12 * 1000, scientific = T)
                     }else if (input$p2unit_CT_metric == "ng/ml"){
                       
                       # temp = format  (input$c_total_ligand_start / 1e9 / input$mol_weight_p2 * 10^12 * 1000, scientific = T)
                       temp = format  (input$c_total_ligand_CT_start / 1e9 / input$mol_weight_p2_CT * 10^12 * 1000, scientific = T)
                       
                     }else if (input$p2unit_CT_metric == "pg/ml"){
                       
                       temp = format  (input$c_total_ligand_CT_start / 1e12/ input$mol_weight_p2_CT * 10^12 * 1000, scientific = T)
                     }
                     
                     updateNumericInput(session, "c_total_ligand_CT", label = ("Conversion of partner 2"),
                                        value = format(temp, scientific=TRUE))
                     
                     
                     
                     # )
                   } else if (input$p2concunit_CT == "molar units"){
                     
                     
                     
                     shinyjs::hide("mol_weight_p2_CT")
                     shinyjs::show("p2unit_CT_molar")
                     shinyjs::hide("p2unit_CT_metric")
                     updateRadioButtons(session, "p1concunit_CT", selected = "molar units")
                     updateRadioButtons(session, "p1concunit_CT", selected = "molar units")
                     
                     temp <- input$c_total_ligand_CT_start
                     
                     if (input$p2unit_CT_molar == "pM"){
                       
                       temp = format  (input$c_total_ligand_CT_start, scientific = T)
                       
                     } else  if (input$p2unit_CT_molar == "uM"){
                       print ("uM")
                       
                       temp = format  (input$c_total_ligand_CT_start / 1e-6 , scientific = T)
                       
                     } else if (input$p2unit_CT_molar == "fM"){
                       
                       temp = format  (input$c_total_ligand_CT_start / 1e3 , scientific = T)
                       
                     } else if (input$p2unit_CT_molar == "nM"){
                       
                       temp = format  (input$c_total_ligand_CT_start / 1e-3, scientific = T)
                     } else if (input$p2unit_CT_molar == "mM"){
                       
                       temp = format  (input$c_total_ligand_CT_start / 1e-9, scientific = T)
                       print(temp)
                     }
                     
                     print(temp)
                     
                     updateNumericInput(session, "c_total_ligand_CT", label = ("Conversion of partner 2"),
                                        value = format(temp, scientific=TRUE))
                   }
                   
                 })
  
  # Free Partner 1 in presence of Competitor (Competition tab)
  observeEvent(input$myconfirmation_submit_competition, {
    if (!is.null (input$myconfirmation_submit_competition) ){
      
      output$freePartner1_CT <- renderText({
        
        isolate({
          volume <- 1
          kdreclig <- input$Kd_CT #input$koff/input$kon*1e12
          # kdcoating <-10^(12-input$pKdcokdcoating <-10^(12-input$pKdcoat_CT[1])at_CT[1])
          # kdcoating <-10^(12-input$ki_CT)
          kdcoating <- input$ki_CT
          
          if (input$ki_CT <= 0){    
            validate(
              need(input$ki_CT > 0, 
                   "KI must be greater than 0")
            )
          }
          
          
          gfun <- function(x) {c(input$c_total_receptor_CT - x[1] - x[2]*x[1]/kdcoating - x[1]*x[3]/kdreclig, input$c_total_coating_CT-x[2]-x[2]*x[1]/kdcoating, input$c_total_ligand_CT-x[3]-x[1]*x[3]/kdreclig)}
          #gfun <- function(x) {c(input$c_total_receptor - x[1] - x[2]*x[1]/kdcoating - x[1]*x[3]/kdreclig, 0-x[2]-x[2]*x[1]/kdcoating, input$c_total_ligand-x[3]-x[1]*x[3]/kdreclig)}
          #fred <- multiroot( gfun, start=c(input$c_total_receptor/2,input$c_total_coating/2,input$c_total_ligand/2), positive=TRUE )
          fred <- multiroot( gfun, start=c(input$c_total_receptor_CT/2,0,input$c_total_ligand_CT/2), positive=TRUE )
          c_free_receptor <- fred$root[1]
          c_free_coat <- fred$root[2]
          c_free_ligand <- fred$root[3]
          pc_occ_ligand <- 100*(1-c_free_ligand/input$c_total_ligand_CT)
          #paste("Free ",input$receptor_name," is ",signif(c_free_receptor,digits=3)," pM.",sep="")
          paste(signif(c_free_receptor,digits=3)," (pM)", input$unit,sep="")
        })
      })
    }
  })
  
  # Free Partner 2 - in presence of Competitor (Competition tab)
  observeEvent(input$myconfirmation_submit_competition, {
    if (!is.null (input$myconfirmation_submit_competition) ){
      output$freePartner2_CT <- renderText({
        
        isolate({
          volume <- 1
          kdreclig <- input$Kd_CT #input$koff/input$kon*1e12
          # kdcoating <-  10^(12-input$pKdcoat_CT[1])
          kdcoating <- input$ki_CT
          
          if (input$ki_CT <= 0){    
            validate(
              need(input$ki_CT > 0, 
                   "KI must be greater than 0")
            )
          }
          
          gfun <- function(x) {c(input$c_total_receptor_CT - x[1] - x[2]*x[1]/kdcoating - x[1]*x[3]/kdreclig, input$c_total_coating_CT-x[2]-x[2]*x[1]/kdcoating, input$c_total_ligand_CT-x[3]-x[1]*x[3]/kdreclig)}
          #gfun <- function(x) {c(input$c_total_receptor - x[1] - x[2]*x[1]/kdcoating - x[1]*x[3]/kdreclig, 0-x[2]-x[2]*x[1]/kdcoating, input$c_total_ligand-x[3]-x[1]*x[3]/kdreclig)}
          #fred <- multiroot( gfun, start=c(input$c_total_receptor/2,input$c_total_coating/2,input$c_total_ligand/2), positive=TRUE )
          fred <- multiroot( gfun, start=c(input$c_total_receptor_CT/2,0,input$c_total_ligand_CT/2), positive=TRUE )
          c_free_receptor <- fred$root[1]
          c_free_coat <- fred$root[2]
          c_free_ligand <- fred$root[3]
          pc_occ_ligand <- 100*(1-c_free_ligand/input$c_total_ligand_CT)
          #paste("Free ",input$receptor_name," is ",signif(c_free_receptor,digits=3)," pM.",sep="")
          paste(signif(c_free_ligand,digits=3)," (pM) ", input$unit,sep="")
        })
      })
    }
  })
  
  
  # Free Competitor (Competition tab)
  observeEvent(input$myconfirmation_submit_competition, {
    if (!is.null (input$myconfirmation_submit_competition) ){
      output$freeCompetior <- renderText({
        
        isolate({
          volume <- 1
          kdreclig <- input$Kd_CT #input$koff/input$kon*1e12
          # kdcoating <- 10^(12-input$pKdcoat_CT[1])
          kdcoating <- input$ki_CT
          
          if (input$ki_CT <= 0){    
            validate(
              need(input$ki_CT > 0, 
                   "KI must be greater than 0")
            )
          }
          
          gfun <- function(x) {c(input$c_total_receptor_CT - x[1] - x[2]*x[1]/kdcoating - x[1]*x[3]/kdreclig, input$c_total_coating_CT-x[2]-x[2]*x[1]/kdcoating, input$c_total_ligand_CT-x[3]-x[1]*x[3]/kdreclig)}
          fred <- multiroot( gfun, start=c(input$c_total_receptor_CT/2,input$c_total_coating_CT/2,input$c_total_ligand_CT/2), positive=TRUE )
          c_free_receptor <- fred$root[1]
          c_free_coat <- fred$root[2]
          c_free_ligand <- fred$root[3]
          pc_occ_ligand <- 100*(1-c_free_ligand/input$c_total_ligand_CT)
          paste(signif(c_free_coat,digits=3)," (pM)", input$unit,sep="")
        })
      })
    }
  })
  
  # Fractional Occupancy of partner 1 bound to partner 2 - in the presence competitor (Competition tab)
  observeEvent(input$myconfirmation_submit_competition, {
    if (!is.null (input$myconfirmation_submit_competition) ){
      output$fractionPartner1_CT <- renderText({
        
        isolate({
          volume <- 1
          kdreclig <- input$Kd_CT #input$koff/input$kon*1e12
          # kdcoating <- 10^(12-input$pKdcoat_CT[1])
          kdcoating <- input$ki_CT
          
          if (input$ki_CT <= 0){    
            validate(
              need(input$ki_CT > 0, 
                   "KI must be greater than 0")
            )
          }
          
          gfun <- function(x) {c(input$c_total_receptor_CT - x[1] - x[2]*x[1]/kdcoating - x[1]*x[3]/kdreclig, input$c_total_coating_CT-x[2]-x[2]*x[1]/kdcoating, input$c_total_ligand_CT-x[3]-x[1]*x[3]/kdreclig)}
          #gfun <- function(x) {c(input$c_total_receptor - x[1] - x[2]*x[1]/kdcoating - x[1]*x[3]/kdreclig, 0-x[2]-x[2]*x[1]/kdcoating, input$c_total_ligand-x[3]-x[1]*x[3]/kdreclig)}
          #fred <- multiroot( gfun, start=c(input$c_total_receptor/2,input$c_total_coating/2,input$c_total_ligand/2), positive=TRUE )
          fred <- multiroot( gfun, start=c(input$c_total_receptor_CT/2,0,input$c_total_ligand_CT/2), positive=TRUE )
          c_free_receptor <- fred$root[1]
          c_free_coat <- fred$root[2]
          c_free_ligand <- fred$root[3]
          c_rlcomplex <- c_free_receptor*c_free_ligand/kdreclig
          
          # gfun <- function(x) {c(input$c_total_receptor_CT - x[1] - x[2]*x[1]/kdcoating - x[1]*x[3]/kdreclig, 0-x[2]-x[2]*x[1]/kdcoating, input$c_total_ligand_CT-x[3]-x[1]*x[3]/kdreclig)}
          # # gfun <- function(x) {c(input$c_total_receptor_CT - x[1] - x[2]*x[1]/kdcoating - x[1]*x[3]/kdreclig, 0-x[2]-x[2]*x[1]/kdcoating, input$c_total_ligand_CT-x[3]-x[1]*x[3]/kdreclig)}
          # # gfun <- function(x) {c(input$c_total_receptor - x[1] - x[2]*x[1]/kdcoating - x[1]*x[3]/kdreclig, 0-x[2]-x[2]*x[1]/kdcoating, input$c_total_ligand-x[3]-x[1]*x[3]/kdreclig)}
          # #fred <- multiroot( gfun, start=c(input$c_total_receptor/2,input$c_total_coating/2,input$c_total_ligand/2), positive=TRUE )
          # fred <- multiroot( gfun, start=c(input$c_total_receptor_CT/2,0,input$c_total_ligand_CT/2), positive=TRUE )
          
          # 
          # c_free_receptor <- fred$root[1]
          # c_free_coat <- fred$root[2]
          # c_free_ligand <- fred$root[3]
          # pc_occ_ligand <- 100*(1-c_free_ligand/input$c_total_ligand_CT)
          # 
          # c_rlcomplex <- c_free_receptor*c_free_ligand/kdreclig
          # c_rlcomplex <- c_free_receptor*c_free_ligand/kdreclig
          print (c_rlcomplex)
          print(input$c_total_receptor_CT)
          pc_occ_receptor <- 100*(c_rlcomplex/input$c_total_receptor_CT)
          # pc_occ_receptor <- 100*(1-c_free_receptor/input$c_total_receptor_CT)
          
          print ("Debugging:")
          print (paste("output: ",pc_occ_receptor ))
          
          paste(signif(pc_occ_receptor, digits=3),"% ",sep="")
        })
      })
    }
  })
  
  # Fractional Occupancy of partner 2 bound to partner 1 - in presence of competitor (Competition tab)
  observeEvent(input$myconfirmation_submit_competition, {
    if (!is.null (input$myconfirmation_submit_competition) ){
      output$fractionPartner2_CT <- renderText({
        
        isolate({
          volume <- 1
          kdreclig <- input$Kd_CT #input$koff/input$kon*1e12
          # kdcoating <- 10^(12-input$pKdcoat_CT[1])
          kdcoating <- input$ki_CT
          
          if (input$ki_CT <= 0){    
            validate(
              need(input$ki_CT > 0, 
                   "KI must be greater than 0")
            )
          }
          
          gfun <- function(x) {c(input$c_total_receptor_CT - x[1] - x[2]*x[1]/kdcoating - x[1]*x[3]/kdreclig, input$c_total_coating_CT-x[2]-x[2]*x[1]/kdcoating, input$c_total_ligand_CT-x[3]-x[1]*x[3]/kdreclig)}
          #gfun <- function(x) {c(input$c_total_receptor - x[1] - x[2]*x[1]/kdcoating - x[1]*x[3]/kdreclig, 0-x[2]-x[2]*x[1]/kdcoating, input$c_total_ligand-x[3]-x[1]*x[3]/kdreclig)}
          #fred <- multiroot( gfun, start=c(input$c_total_receptor/2,input$c_total_coating/2,input$c_total_ligand/2), positive=TRUE )
          fred <- multiroot( gfun, start=c(input$c_total_receptor_CT/2,0,input$c_total_ligand_CT/2), positive=TRUE )
          c_free_receptor <- fred$root[1]
          c_free_coat <- fred$root[2]
          c_free_ligand <- fred$root[3]
          pc_occ_ligand <- 100*(1-c_free_ligand/input$c_total_ligand_CT)
          pc_occ_receptor <- 100*(1-c_free_receptor/input$c_total_receptor_CT)
          paste(signif(pc_occ_ligand, digits=3),"%",sep="")
        })
      })
    }
  })
  
  # Fractional Occupancy of partner 1 bound to partner 3 (Competition tab)
  observeEvent(input$myconfirmation_submit_competition, {
    if (!is.null (input$myconfirmation_submit_competition) ){
      output$fractionP1P3 <- renderText({
        
        isolate({
          
          volume <- 1
          kdreclig <- input$Kd_CT #input$koff/input$kon*1e12
          # kdcoating <- 10^(12-input$pKdcoat_CT[1])
          kdcoating <- input$ki_CT
          
          if (input$ki_CT <= 0){
            validate(
              need(input$ki_CT > 0,
                   "KI must be greater than 0")
            )
          }
          
          gfun <- function(x) {c(input$c_total_receptor_CT - x[1] - x[2]*x[1]/kdcoating - x[1]*x[3]/kdreclig, input$c_total_coating_CT-x[2]-x[2]*x[1]/kdcoating, input$c_total_ligand_CT-x[3]-x[1]*x[3]/kdreclig)}
          #gfun <- function(x) {c(input$c_total_receptor - x[1] - x[2]*x[1]/kdcoating - x[1]*x[3]/kdreclig, 0-x[2]-x[2]*x[1]/kdcoating, input$c_total_ligand-x[3]-x[1]*x[3]/kdreclig)}
          #fred <- multiroot( gfun, start=c(input$c_total_receptor/2,input$c_total_coating/2,input$c_total_ligand/2), positive=TRUE )
          fred <- multiroot( gfun, start=c(input$c_total_receptor_CT/2,0,input$c_total_ligand_CT/2), positive=TRUE )
          c_free_receptor <- fred$root[1]
          c_free_coat <- fred$root[2]
          c_free_ligand <- fred$root[3]
          # pc_occ_ligand <- 100*(1-c_free_ligand/input$c_total_ligand_CT)
          # pc_occ_receptor <- 100*(1-c_free_receptor/input$c_total_receptor_CT)
          # pc_occ_competitor <- 100*(1-c_free_receptor/input$c_total_receptor_CT)
          # paste(signif(pc_occ_receptor, digits=3),"% ",sep="")
          
          
          # c_rccomplex <- c_free_receptor*c_free_coat/kdreclig
          c_rccomplex <- input$c_total_coating_CT - c_free_coat
          pc_occ_comp <- 100*(c_rccomplex/input$c_total_receptor_CT)
          # print(c_free_coat)
          # print ()
          # print (c_rccomplex)
          # print (input$c_total_receptor_CT)
          
          # c_free_receptor <- fred$root[1]
          # c_free_coat <- fred$root[2]
          # c_free_ligand <- fred$root[3]
          # c_rlcomplex <- c_free_receptor*c_free_ligand/kdreclig
          # c_rccomplex <- c_free_receptor*c_free_coat/kdreclig
          # pc_occ_ligand <- 100*(1-c_free_ligand/input$c_total_ligand_CT)
          
          paste(signif(pc_occ_comp,digits=3),"% ",sep="")
          # paste(signif(pc_occ_comp,digits=3),"% ",sep="")
        })
      })
    }
  })
  
  # Fractional Occupancy of partner 3 bound to partner 1 (Competition tab)
  observeEvent(input$myconfirmation_submit_competition, {
    if (!is.null (input$myconfirmation_submit_competition) ){
      output$fractionP3P1 <- renderText({
        
        isolate({
          volume <- 1
          kdreclig <- input$Kd_CT #input$koff/input$kon*1e12
          # kdcoating <- 10^(12-input$pKdcoat_CT[1])
          kdcoating <- input$ki_CT
          
          if (input$ki_CT <= 0){    
            validate(
              need(input$ki_CT > 0, 
                   "KI must be greater than 0")
            )
          }
          
          gfun <- function(x) {c(input$c_total_receptor_CT - x[1] - x[2]*x[1]/kdcoating - x[1]*x[3]/kdreclig, input$c_total_coating_CT-x[2]-x[2]*x[1]/kdcoating, input$c_total_ligand_CT-x[3]-x[1]*x[3]/kdreclig)}
          #gfun <- function(x) {c(input$c_total_receptor - x[1] - x[2]*x[1]/kdcoating - x[1]*x[3]/kdreclig, 0-x[2]-x[2]*x[1]/kdcoating, input$c_total_ligand-x[3]-x[1]*x[3]/kdreclig)}
          #fred <- multiroot( gfun, start=c(input$c_total_receptor/2,input$c_total_coating/2,input$c_total_ligand/2), positive=TRUE )
          fred <- multiroot( gfun, start=c(input$c_total_receptor_CT/2,0,input$c_total_ligand_CT/2), positive=TRUE )
          c_free_receptor <- fred$root[1]
          c_free_coat <- fred$root[2]
          c_free_ligand <- fred$root[3]
          pc_occ_ligand <- 100*(1-c_free_ligand/input$c_total_ligand_CT)
          pc_occ_receptor <- 100*(1-c_free_receptor/input$c_total_receptor_CT)
          pc_occ_competitor <- 100*(1-c_free_coat/input$c_total_coating_CT)
          paste(signif(pc_occ_competitor, digits=3),"% ",sep="")
          
        })
      })
    }
  })
  
  # In Complex Concentration: Partner 1 - Partner 2 (Competition tab)
  observeEvent(input$myconfirmation_submit_competition, {
    if (!is.null (input$myconfirmation_submit_competition) ){
      output$InComplexConctrP1P2 <- renderText({
        
        isolate({
          volume <- 1
          kdreclig <- input$Kd_CT #input$koff/input$kon*1e12
          # kdcoating <- 10^(12-input$pKdcoat_CT[1])
          kdcoating <- input$ki_CT
          
          if (input$ki_CT <= 0){    
            validate(
              need(input$ki_CT > 0, 
                   "KI must be greater than 0")
            )
          }
          
          gfun <- function(x) {c(input$c_total_receptor_CT - x[1] - x[2]*x[1]/kdcoating - x[1]*x[3]/kdreclig, input$c_total_coating_CT-x[2]-x[2]*x[1]/kdcoating, input$c_total_ligand_CT-x[3]-x[1]*x[3]/kdreclig)}
          #gfun <- function(x) {c(input$c_total_receptor - x[1] - x[2]*x[1]/kdcoating - x[1]*x[3]/kdreclig, 0-x[2]-x[2]*x[1]/kdcoating, input$c_total_ligand-x[3]-x[1]*x[3]/kdreclig)}
          #fred <- multiroot( gfun, start=c(input$c_total_receptor/2,input$c_total_coating/2,input$c_total_ligand/2), positive=TRUE )
          fred <- multiroot( gfun, start=c(input$c_total_receptor_CT/2,0,input$c_total_ligand_CT/2), positive=TRUE )
          c_free_receptor <- fred$root[1]
          c_free_coat <- fred$root[2]
          c_free_ligand <- fred$root[3]
          c_rlcomplex <- c_free_receptor*c_free_ligand/kdreclig
          
          print(paste("in complex conc p1p2: ", c_rlcomplex))
          pc_occ_ligand <- 100*(1-c_free_ligand/input$c_total_ligand_CT)
          #paste("Free ",input$receptor_name," is ",signif(c_free_receptor,digits=3)," pM.",sep="")
          paste(signif(c_rlcomplex,digits=3)," (pM)", input$unit,sep="")
        })
      })
    }
  })
  
  # In Complex Concentration: Partner 1 - Partner 3 (Competition tab)
  observeEvent(input$myconfirmation_submit_competition, {
    if (!is.null (input$myconfirmation_submit_competition) ){
      output$InComplexConctrP1P3 <- renderText({
        
        isolate({
          volume <- 1
          kdreclig <- input$Kd_CT #input$koff/input$kon*1e12
          # kdcoating <-  10^(12-input$pKdcoat_CT[1])
          kdcoating <- input$ki_CT
          
          if (input$ki_CT <= 0){    
            validate(
              need(input$ki_CT > 0, 
                   "KI must be greater than 0")
            )
          }
          
          gfun <- function(x) {c(input$c_total_receptor_CT - x[1] - x[2]*x[1]/kdcoating - x[1]*x[3]/kdreclig, input$c_total_coating_CT-x[2]-x[2]*x[1]/kdcoating, input$c_total_ligand_CT-x[3]-x[1]*x[3]/kdreclig)}
          #gfun <- function(x) {c(input$c_total_receptor - x[1] - x[2]*x[1]/kdcoating - x[1]*x[3]/kdreclig, 0-x[2]-x[2]*x[1]/kdcoating, input$c_total_ligand-x[3]-x[1]*x[3]/kdreclig)}
          #fred <- multiroot( gfun, start=c(input$c_total_receptor/2,input$c_total_coating/2,input$c_total_ligand/2), positive=TRUE )
          fred <- multiroot( gfun, start=c(input$c_total_receptor_CT/2,0,input$c_total_ligand_CT/2), positive=TRUE )
          c_free_receptor <- fred$root[1]
          c_free_coat <- fred$root[2]
          c_free_ligand <- fred$root[3]
          c_rlcomplex <- c_free_receptor*c_free_ligand/kdreclig
          # c_rccomplex <- c_free_receptor*c_free_coat/kdreclig
          
          c_rccomplex <- input$c_total_coating_CT - c_free_coat
          pc_occ_ligand <- 100*(1-c_free_ligand/input$c_total_ligand_CT)
          #paste("Free ",input$receptor_name," is ",signif(c_free_receptor,digits=3)," pM.",sep="")
          paste(signif(c_rccomplex,digits=3)," (pM)", input$unit,sep="")
        })
      })
    }
  })
  
  output$competitionDashboard <- renderUI({
    
    fluidRow(
      width = 12,
      
      # div (style = "display:inline-block; vertical-align:top; padding-top: 0px;margin-bottom:35px; margin-left:20px; ",
      #      actionBttn('submit_query', 'Go', size = "xs",
      #                 style = "unite", color = "success",  icon = icon("thumbs-up")),
      #            actionBttn('clear_filters', 'Reset all', size = "xs", style = "jelly", color = "danger",
      #                       icon = icon("refresh"))
      # ),
      #
      
      div( style = "margin-right:15px; margin-top:-10px;margin-bottom:30px;float:right",
           downloadButton('downloadCompetitionPage', 'Download Page'),
           # ),
           # div( style = "margin-right:15px; margin-top:-10px;margin-bottom:30px;float:right",
           downloadButton('downloadCompetitionPageData', 'Download Data')
      ),
      
      box (
        width=12,
        style = " background-color: #FFFFFF; margin-top:-25px",
        
        fluidRow(
          column(width = 6,
                 
                 div( style = "margin:0px; padding:5px;background-color: #F7FAFC;height:240px;",
                      
                      div (
                        HTML("Values"), style = "color:dodgerblue;margin-top: 0px; font-size: 20px;
                         font-weight: bold;font-style: Lato;"
                      ),
                      
                      
                      if (input$p1concunit_CT == "molar units" | input$p2concunit_CT == "molar units" |
                          input$coat_concunit_CT == "molar units"){
                        
                        fluidRow(
                          
                          column(
                            width = 6,
                            
                            div(HTML(input$receptor_name_CT),
                                style = "margin-top: 5px; font-weight:bold;
                                       font-size: 14px;
                                       margin-bottom: 0px;
                                       color: #718096; font-style: Lato;"),
                            
                            div(input$ligand_name_CT,
                                style = "margin-top: 0px; font-size: 14px;font-weight:bold;
                                 margin-bottom: 0px;
                                 color: #718096; font-style: Lato;"),
                            
                            div(input$coating_name_CT,
                                style = "margin-top: 0px; font-size: 14px;font-weight:bold;
                                 margin-bottom: 0px;
                                 color: #718096; font-style: Lato;"),
                            
                            
                            div(
                              HTML("K<sub>D</sub>"), br(),
                              # HTML("k<sub>off</sub>" ), br(),
                              # HTML("k<sub>on</sub>"), br(),
                              HTML("KI"),br(),
                              style = "margin-top: 0px; font-size: 14px; margin-bottom: 0px;font-weight:bold;
                             color: #718096; font-style: Lato;"
                            )
                          ),
                          
                          column(
                            width = 6,
                            div(HTML(format(input$c_total_receptor_CT+0,scientific = TRUE, decimal.mark = ","), "(pM)"),
                                style = "margin-top: 5px; font-size: 14px;
                                   margin-bottom: 0px;
                                   color: #718096; font-style: Lato;"),
                            
                            div(format(input$c_total_ligand_CT+0, scientific = TRUE, decimal.mark = ","), "(pM)",
                                style = "margin-top: 0px; font-size: 14px;
                                 margin-bottom: 0px;
                                 color: #718096; font-style: Lato;"),
                            
                            div(format(input$c_total_coating_CT+0, scientific = TRUE, decimal.mark = ","), "(pM)",
                                style = "margin-top: 0px; font-size: 14px;
                                 margin-bottom: 0px;
                                 color: #718096; font-style: Lato;"),
                            
                            div(
                              HTML(input$Kd_CT, "(pM)"), br(),
                              # HTML(input$koff, "(s<sup>-1</sup>)"), br(),
                              # HTML( input$kon, "(M<sup>-1</sup> s <sup>-1</sup>)"), br(),
                              HTML(input$ki_CT, "(M<sup>-1</sup> s <sup>-1</sup>)"), br(),
                              style = "margin-top: 0px; font-size: 14px; margin-bottom: 0px;
                             color: #718096; font-style: Lato;"
                            )
                          )
                          
                        )
                        
                      } else {
                        
                        fluidRow(
                          column(
                            width = 6,
                            
                            div(HTML(input$receptor_name_CT),
                                style = "margin-top: 5px; font-weight:bold;
                                       font-size: 14px;
                                       margin-bottom: 0px;
                                       color: #718096; font-style: Lato;"),
                            
                            div(
                              HTML("Molecular weight of partner 1"),
                              style = "margin-top: 0px; font-size: 14px;font-weight:bold;
                            margin-bottom: 0px;
                           color: #718096; font-style: Lato;"
                            ),
                            
                            
                            div(input$ligand_name_CT,
                                style = "margin-top: 0px; font-size: 14px;font-weight:bold;
                                 margin-bottom: 0px;
                                 color: #718096; font-style: Lato;"),
                            
                            div(
                              HTML("Molecular weight of partner 2"),
                              style = "margin-top: 0px; font-size: 14px;font-weight:bold;
                            margin-bottom: 0px;
                           color: #718096; font-style: Lato;"
                            ),
                            
                            div(input$coating_name_CT,
                                style = "margin-top: 0px; font-size: 14px;font-weight:bold;
                                 margin-bottom: 0px;
                                 color: #718096; font-style: Lato;"),
                            div(
                              HTML("Molecular weight of the competing partner"),
                              style = "margin-top: 0px; font-size: 14px;font-weight:bold;
                            margin-bottom: 0px;
                           color: #718096; font-style: Lato;"
                            ),
                            
                            div(
                              HTML("K<sub>D</sub>"), br(),
                              # HTML("k<sub>off</sub>" ), br(),
                              # HTML("k<sub>on</sub>"), br(),
                              HTML("KI"),br(),
                              style = "margin-top: 0px; font-size: 14px; margin-bottom: 0px;font-weight:bold;
                             color: #718096; font-style: Lato;"
                            )
                          ),
                          
                          
                          column(
                            width = 6,
                            div(HTML(format(input$c_total_receptor_CT+0,scientific = TRUE, decimal.mark = ","), "(pM)"),
                                style = "margin-top: 5px; font-size: 14px;
                                   margin-bottom: 0px;
                                   color: #718096; font-style: Lato;"),
                            
                            div(format(input$mol_weight_p1_CT, decimal.mark = ","), "(dalton)",
                                style = "margin-top: 0px; font-size: 14px;
                            margin-bottom: 0px;
                           color: #718096; font-style: Lato;"
                            ),
                            
                            
                            div(format(input$c_total_ligand_CT+0, scientific = TRUE, decimal.mark = ","), "(pM)",
                                style = "margin-top: 0px; font-size: 14px;
                                 margin-bottom: 0px;
                                 color: #718096; font-style: Lato;"),
                            
                            div(format(input$mol_weight_p2_CT, decimal.mark = ","), "(dalton)",
                                style = "margin-top: 0px; font-size: 14px;
                            margin-bottom: 0px;
                           color: #718096; font-style: Lato;"
                            ),
                            
                            div(format(input$c_total_coating_CT+0, scientific = TRUE, decimal.mark = ","), "(pM)",
                                style = "margin-top: 0px; font-size: 14px;
                                 margin-bottom: 0px;
                                 color: #718096; font-style: Lato;"),
                            
                            div(format(input$mol_weight_coat, decimal.mark = ","), "(dalton)",
                                style = "margin-top: 0px; font-size: 14px;
                            margin-bottom: 0px;
                           color: #718096; font-style: Lato;"
                            ),
                            
                            div(
                              HTML(input$Kd_CT, "(pM)"), br(),
                              # HTML(input$koff, "(s<sup>-1</sup>)"), br(),
                              # HTML( input$kon, "(M<sup>-1</sup> s <sup>-1</sup>)"), br(),
                              HTML(input$ki_CT, "(M<sup>-1</sup> s <sup>-1</sup>)"), br(),
                              style = "margin-top: 0px; font-size: 14px; margin-bottom: 0px;
                             color: #718096; font-style: Lato;"
                            )
                          )
                        )
                        
                      }
                 )
          ), #end values container
          
          column(
            width = 6,
            
            div(
              style = "padding:5px;background-color: #F7FAFC;margin-top:0px; height:240px;
                   margin-left:-20px; margin-bottom:10px",
              
              div(
                HTML("Observations"), style = "color:dodgerblue;margin-left:0px; margin-top: 0px;
                   font-size: 20px; margin-bottom: 0px;
                         font-weight: bold; font-style: Lato;"),
              
              div(style = "margin-left:0px;margin-top: 0px;",
                  textAreaInput("Comments", label = (""), value = "Add comments or notes for other colleagues...", height='100px')
              )
              
            )
          )#end container for observations
        ),
        
        fluidRow(
          width = 12,
          
          column(
            width = 12,
            div(
              style = "padding:5px;background-color: #F7FAFC;margin:0px;height:150px;",
              
              div(
                HTML("At equilibrium"), style = "margin-top: 0px; font-size: 16px; margin-bottom: 15px;
                  font-style: Lato; font-weight:bold;"
              ),
              
              fluidRow(
                width = 12,
                
                column(width = 4,
                       align = "left",
                       div(HTML("FREE", toupper(input$receptor_name_CT)), style = "margin-top: 0px; font-size: 11px;
                        margin-bottom: 0px; color: #718096; font-style: Lato;"),
                       div(style = ";",
                           h3(strong(textOutput("freePartner1_CT"), style = "font-size: 42px; margin-top: 0px")))
                ),
                
                column(width = 4,
                       align = "left",
                       div(HTML("FREE", toupper(input$ligand_name_CT)), style = "margin-left:160px;margin-top: 0px; font-size: 11px;
                                      margin-bottom: 0px; color: #718096; font-style: Lato;"),
                       div(style = "margin-top: 0px;margin-left:160px;",
                           h3(strong(textOutput("freePartner2_CT"), style = "font-size: 42px;margin-top: 0px")))
                ),
                
                column(width = 4,
                       align = "left",
                       div(HTML("FREE", toupper(input$coating_name_CT)), style = "margin-left:165px;margin-top: 0px; font-size: 11px;
                                      margin-bottom: 0px; color: #718096; font-style: Lato;"),
                       div(style = "margin-top: 0px;margin-left:165px;",
                           h3(strong(textOutput("freeCompetior"), style = "font-size: 42px;margin-top: 0px")))
                )
              )
              
            ) #end container
          )#end column
          
          
        ),
        
        fluidRow(
          width = 12,
          
          column(
            width = 9,
            
            div(
              style = "padding:5px; background-color: #F7FAFC; margin-top:10px;  margin-right:-10px;height:150;margin-bottom:10px",
              
              div(HTML("Fractional Occupancy of ", input$receptor_name_CT, "and ",   input$ligand_name_CT, "in presence of ", input$coating_name_CT),
                  style = "margin-top: 0px; font-size: 16px; margin-bottom: 7px; font-style: Lato; font-weight:bold;"),
              #
              fluidRow(
                #
                column(width=1,
                       div(img(src = "FO1.png", height = "100%", style = "margin:5px;margin-left:5px;"), style="text-align: center; "
                       )
                ),
                column(width = 4,
                       
                       div(HTML("FRACTION", toupper(input$receptor_name_CT), "BOUND TO", toupper(input$ligand_name_CT)),
                           style = "margin-top: 0px; margin-left:35px;font-size: 11px;  margin-bottom: 20px; color: #718096; font-style: Lato;"
                       ),
                       div(style = "margin-top: 0px;margin-left:35px;",
                           h3(strong(textOutput("fractionPartner1_CT") , style = "font-size: 42px;"))
                       )
                ),
                column(width = 4,
                       
                       div(style = "width:300px;margin-top: 0px;margin-left:195px;font-size: 11px;margin-bottom: 20px;color: #718096; font-style: Lato;",
                           HTML("FRACTION", toupper(input$ligand_name_CT), "BOUND TO", toupper(input$receptor_name_CT))
                       ),
                       
                       div(style = "margin-top: 0px;margin-left:195px;",
                           h3(strong(textOutput("fractionPartner2_CT"), style = "font-size: 42px;margin-bottom: 0px;"))
                       )
                )
                
              )
              
            )
          ),
          
          column(
            width = 3,
            
            div(
              style = "padding:5px;background-color: #F7FAFC;margin-top:10px;margin-left:-10px;height:135px; margin-bottom:5px",
              
              div(HTML("In Complex Concentration"), style = "margin-left:10px; margin-top: 0px; font-size: 16px; margin-bottom: 7px;
                font-weight:bold; font-style: Lato;"),
              
              div(HTML(toupper(input$receptor_name_CT),"-", toupper(input$ligand_name_CT)), style = "margin-top: 0px; font-size: 11px;
                         margin-bottom: 0px; color: #718096; font-style: Lato;margin-left:10px;"
              ),
              div(style = "margin-top: 0px;margin-left:10px;",
                  h3(strong(textOutput("InComplexConctrP1P2"), style = "font-size: 42px; "))
              )
              
              # )
            )
            
          ) #end container
          
        ),
        
        fluidRow(
          width = 12,
          
          column(
            width = 9,
            
            div(
              style = "padding:5px; background-color: #F7FAFC; margin-top:10px;  margin-right:-10px;height:150;margin-bottom:10px",
              
              div(HTML("Fractional Occupancy of ", input$receptor_name_CT, "and ",   input$coating_name_CT, "in presence of ", input$ligand_name_CT),
                  style = "margin-top: 0px; font-size: 16px; margin-bottom: 7px; font-style: Lato; font-weight:bold;"),
              #
              fluidRow(
                #
                column(width=1,
                       div(img(src = "FO2.png", height = "100%", style = "margin:5px;margin-left:5px;"), style="text-align: center; "
                       )
                ),
                column(width = 4,
                       
                       div(HTML("FRACTION", toupper(input$receptor_name_CT), "BOUND TO", toupper(input$coating_name_CT)),
                           style = "margin-top: 0px; margin-left:35px;font-size: 11px;  margin-bottom: 20px; color: #718096; font-style: Lato;"
                       ),
                       div(style = "margin-top: 0px;margin-left:35px;",
                           h3(strong(textOutput("fractionP1P3") , style = "font-size: 42px;"))
                       )
                ),
                column(width = 4,
                       
                       div(style = "width:300px;margin-top: 0px;margin-left:195px;font-size: 11px;margin-bottom: 20px;color: #718096; font-style: Lato;",
                           HTML("FRACTION", toupper(input$coating_name_CT), "BOUND TO", toupper(input$receptor_name_CT))
                       ),
                       
                       div(style = "margin-top: 0px;margin-left:195px;",
                           h3(strong(textOutput("fractionP3P1"), style = "font-size: 42px;margin-bottom: 0px;"))
                       )
                )
                
              )
              
            )
          ),
          
          column(
            width = 3,
            
            div(
              style = "padding:5px;background-color: #F7FAFC;margin-top:10px;margin-left:-10px;height:135px; margin-bottom:5px",
              
              div(HTML("In Complex Concentration"), style = "margin-left:10px; margin-top: 0px; font-size: 16px; margin-bottom: 7px;
                font-weight:bold; font-style: Lato;"),
              
              div(HTML(toupper(input$receptor_name_CT),"-", toupper(input$coating_name_CT)), style = "margin-top: 0px; font-size: 11px;
                         margin-bottom: 0px; color: #718096; font-style: Lato;margin-left:10px;"
              ),
              div(style = "margin-top: 0px;margin-left:10px;",
                  h3(strong(textOutput("InComplexConctrP1P3"), style = "font-size: 42px; "))
              )
              
              # )
            )
            
          ) #end container
          
        )
 
        
      )
      
    )
  })

  #competition go and clear control\
  
  #go
  observeEvent(input$submit_query_competition, {
    confirmSweetAlert(
      session = session,
      inputId = "myconfirmation_submit_competition",
      type = "warning",
      title = "Submit with selected inputs?",
      danger_mode = TRUE
    )
  })
  
  observeEvent(input$submit_query_local_graph1, {
    confirmSweetAlert(
      session = session,
      inputId = "myconfirmation_submit_competition_graph1",
      type = "warning",
      title = "Update plots?",
      danger_mode = TRUE
    )
  })
  
  observeEvent(input$submit_query_local_graph2, {
    confirmSweetAlert(
      session = session,
      inputId = "myconfirmation_submit_competition_graph2",
      type = "warning",
      title = "Update plots?",
      danger_mode = TRUE
    )
  })
  
  output$downloadCompetitionPage <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "competition_dashboard.html",
    
    
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      
      shiny::withProgress(
        message = "Downloading page to html file", {
          # value = 0,
          
          # shiny::incProgress(1/10)
          # Sys.sleep(1)
          # shiny::incProgress(1/20)
          # for (i in 1:15) {
          #   incProgress(1/15)
          #   Sys.sleep(0.25)
          # }
          # tempReport <- file.path(tempdir(), "report_competition.Rmd")
          # file.copy("report_competition.Rmd", tempReport, overwrite = TRUE)
          
          
          #get units
          if (input$p1concunit_CT == "metric units"){
            
            my.units <- input$p1unit_CT_metric
            my.conc <- input$c_total_receptor_CT_start
            p1mol = input$mol_weight_p1_CT
          }else if (input$p1concunit == "molar units"){
            my.units <- input$p1unit_CT_molar
            my.conc <- input$c_total_receptor_CT
            p1mol = ""
          }
          if (input$p2concunit_CT == "metric units"){
            
            my.units2 <- input$p2unit_CT_metric
            my.conc2 <- input$c_total_ligand_CT_start
            p2mol = input$mol_weight_p2_CT
          }else  if (input$p2concunit == "molar units"){
            my.units2 <- input$p2unit_CT_molar
            my.conc2 <- input$c_total_ligand_CT
            p2mol = ""
          }
          
          if (input$coat_concunit_CT == "metric units"){
            
            my.units3 <- input$coat_unit_CT_metric
            my.conc3 <- input$c_total_coating_CT_start
            p3mol = input$mol_weight_coat
          }else  if (input$p2concunit == "molar units"){
            my.units3 <- input$coat_unit_CT_molar
            my.conc3 <- input$c_total_coating_CT
            p3mol = ""
          }
          
          #graph 1
          if (input$A_change_first_var == input$ligand_name_CT){
            
            name_partner2 <- HTML('Concentration of ' ,input$A_change_first_var, "\n", " (", get_units(input$A_change_first_var), ")")
            name_partner1 <- paste('Concentration of ', input$receptor_name_CT, "\n", " (", get_units(input$receptor_name_CT), ")")
            
            
          } else if( input$A_change_first_var == input$coating_name_CT){
            
            name_partner2 <- HTML('Concentration of ' ,input$A_change_first_var, "\n", " (", get_units(input$A_change_first_var), ")")
            name_partner1 <- paste('Concentration of ', input$receptor_name_CT, "\n", " (", get_units(input$receptor_name_CT), ")")
            
            
          }else if( input$A_change_first_var == "KD"){
            name_partner2 <- HTML(input$A_change_first_var, "\n", " (", get_units(input$A_change_first_var), ")")
            name_partner1 <- paste('Concentration of ', input$receptor_name_CT, "\n", " (", get_units(input$receptor_name_CT), ")")
            
            
          }else if( input$A_change_first_var == "KI"){
            name_partner2 <- HTML(input$A_change_first_var, "\n", " (", get_units(input$A_change_first_var), ")")
            name_partner1 <- paste('Concentration of ', input$receptor_name_CT, "\n", " (", get_units(input$receptor_name_CT), ")")
            
          }
          
          
          #graph 2
          if (input$A_change_first_var_graph2 == input$ligand_name_CT){
            
            name_partner2_2 <- HTML('Concentration of ' ,input$A_change_first_var_graph2, "\n", " (", get_units(input$A_change_first_var_graph2), ")")
            name_partner1_2 <- paste('Concentration of ', input$receptor_name_CT, "\n", " (", get_units(input$receptor_name_CT), ")")
            
          } else if( input$A_change_first_var_graph2 == input$coating_name_CT){
            
            name_partner2_2 <- HTML('Concentration of ' ,input$A_change_first_var_graph2, "\n", " (", get_units(input$A_change_first_var_graph2), ")")
            name_partner1_2 <- paste('Concentration of ', input$receptor_name_CT, "\n", " (", get_units(input$receptor_name_CT), ")")
            
            
          }else if( input$A_change_first_var_graph2 == "KD"){
            name_partner2_2 <- HTML(input$A_change_first_var_graph2, "\n", " (", get_units(input$A_change_first_var_graph2), ")")
            name_partner1_2 <- paste('Concentration of ', input$receptor_name_CT, "\n", " (", get_units(input$receptor_name_CT), ")")
            
            
          }else if( input$A_change_first_var_graph2 == "KI"){
            name_partner2_2 <- HTML(input$A_change_first_var_graph2, "\n", " (", get_units(input$A_change_first_var_graph2), ")")
            name_partner1_2 <- paste('Concentration of ', input$receptor_name_CT, "\n", " (", get_units(input$receptor_name_CT), ")")
            
          }
          
          # Set up parameters to pass to Rmd document
          # params <- list(my.table1 = inputTable2())
          params <- list(p1_name = input$receptor_name_CT,
                         p1_conc = my.conc,
                         p1_mol_weight = p1mol,
                         p1unit = my.units,
                         p2unit = my.units2,
                         p2_name = input$ligand_name_CT,
                         p2_conc = my.conc2,
                         p2_mol_weight = p2mol,
                         comp_name = input$coating_name_CT,
                         comp_mol_weight = p3mol,
                         comp_conc = my.conc3,
                         comp_unit = my.units3,
                         kd = input$KD,
                         ki = input$ki_CT,

                          plot1 = inputfirstPlot_A(),
                          table1 = inputTable1_graph1_CT(),
                          plot2 = inputfirstPlot_B(),
                          table2 = inputTable2_graph1_CT(),
                          plot3 = inputsecondPlot_A_graph2(),
                          table3 = inputTable1_graph2_CT(),
                          plot4 = inputsecondPlot_B_graph2(),
                          table4 = inputTable2_graph2_CT(),
                          
                          namep1 = name_partner1,
                          namep2 = name_partner2,
                          namep12 = name_partner1_2,
                          namep22 = name_partner2_2,
                          container = createContainer
          )
          
          # Knit the document, passing in the `params` list, and eval it in a
          # child of the global environment (this isolates the code in the document
          # from the code in this app).
          rmarkdown::render("report_competition.Rmd", output_file = file,
                            params = params,
                            envir = new.env(parent = globalenv())
          )
        })
    }
  )
  
  
  output$downloadCompetitionPageData <- downloadHandler(
    filename = function() {'competitionDashboard.xlsx'},
    content = function(file) {
      
      my_workbook <- createWorkbook()
      
      
      #graph 1
      if (input$A_change_first_var == input$ligand_name_CT){
        
        name_partner2 <- HTML('Concentration of ' ,input$A_change_first_var, "\n", " (", get_units(input$A_change_first_var), ")")
        name_partner1 <- paste('Concentration of ', input$receptor_name_CT, "\n", " (", get_units(input$receptor_name_CT), ")")
        
        
      } else if( input$A_change_first_var == input$coating_name_CT){
        
        name_partner2 <- HTML('Concentration of ' ,input$A_change_first_var, "\n", " (", get_units(input$A_change_first_var), ")")
        name_partner1 <- paste('Concentration of ', input$receptor_name_CT, "\n", " (", get_units(input$receptor_name_CT), ")")
        
        
      }else if( input$A_change_first_var == "KD"){
        name_partner2 <- HTML(input$A_change_first_var, "\n", " (", get_units(input$A_change_first_var), ")")
        name_partner1 <- paste('Concentration of ', input$receptor_name_CT, "\n", " (", get_units(input$receptor_name_CT), ")")
        
        
      }else if( input$A_change_first_var == "KI"){
        name_partner2 <- HTML(input$A_change_first_var, "\n", " (", get_units(input$A_change_first_var), ")")
        name_partner1 <- paste('Concentration of ', input$receptor_name_CT, "\n", " (", get_units(input$receptor_name_CT), ")")
        
      }
      
      
      #graph 2
      if (input$A_change_first_var_graph2 == input$ligand_name_CT){
        
        name_partner2_2 <- HTML('Concentration of ' ,input$A_change_first_var_graph2, "\n", " (", get_units(input$A_change_first_var_graph2), ")")
        name_partner1_2 <- paste('Concentration of ', input$receptor_name_CT, "\n", " (", get_units(input$receptor_name_CT), ")")
        
      } else if( input$A_change_first_var_graph2 == input$coating_name_CT){
        
        name_partner2_2 <- HTML('Concentration of ' ,input$A_change_first_var_graph2, "\n", " (", get_units(input$A_change_first_var_graph2), ")")
        name_partner1_2 <- paste('Concentration of ', input$receptor_name_CT, "\n", " (", get_units(input$receptor_name_CT), ")")
        
        
      }else if( input$A_change_first_var_graph2 == "KD"){
        name_partner2_2 <- HTML(input$A_change_first_var_graph2, "\n", " (", get_units(input$A_change_first_var_graph2), ")")
        name_partner1_2 <- paste('Concentration of ', input$receptor_name_CT, "\n", " (", get_units(input$receptor_name_CT), ")")
        
        
      }else if( input$A_change_first_var_graph2 == "KI"){
        name_partner2_2 <- HTML(input$A_change_first_var_graph2, "\n", " (", get_units(input$A_change_first_var_graph2), ")")
        name_partner1_2 <- paste('Concentration of ', input$receptor_name_CT, "\n", " (", get_units(input$receptor_name_CT), ")")
        
      }
      
      
      # sheet 1
      addWorksheet(
        wb = my_workbook,
        sheetName = "Concentration of Complex (1)"
      )
      writeData(
        my_workbook,
        sheet = 1,
        paste("1. Concentration of ",input$receptor_name_CT,"-", input$ligand_name_CT, 
              " in complex in function of concentration of", input$receptor_name_CT),
        startRow = 1,
        startCol = 1
      )
      writeData(
        my_workbook,
        sheet = 1,
        paste(name_partner1),
        startRow = 4,
        startCol = 1
      )
      writeData(
        my_workbook,
        sheet = 1,
        paste(name_partner2),
        startRow = 4,
        startCol = 2
      )
      writeData(
        my_workbook,
        sheet = 1,
        inputTable1_graph1_CT(),
        startRow = 5,
        startCol = 1
      )
      
      # #sheet 2
      addWorksheet(
        wb = my_workbook,
        sheetName = "Concentration of Complex (2)"
      )
      writeData(
        my_workbook,
        sheet = 2,
        paste("1. Concentration of ",input$receptor_name_CT,"-", input$ligand_name_CT, 
              " in complex in function of concentration of", input$receptor_name_CT),
        startRow = 1,
        startCol = 1
      )
      writeData(
        my_workbook,
        sheet = 2,
        paste(name_partner1),
        startRow = 4,
        startCol = 1
      )
      writeData(
        my_workbook,
        sheet = 2,
        paste(name_partner2),
        startRow = 4,
        startCol = 2
      )
      writeData(
        my_workbook,
        sheet = 2,
        inputTable2_graph1_CT(),
        startRow = 5,
        startCol = 1
      )

      # #sheet 3
      addWorksheet(
        wb = my_workbook,
        sheetName = "Fractional Occupancy (1)"
      )
      writeData(
        my_workbook,
        sheet = 3,
        paste("2. Fractional Occupancy of ",input$ligand_name_CT,
              " in function of concentration of", input$receptor_name_CT),
        startRow = 1,
        startCol = 1
      )
      writeData(
        my_workbook,
        sheet = 3,
        paste(name_partner1_2),
        startRow = 4,
        startCol = 1
      )
      writeData(
        my_workbook,
        sheet = 3,
        paste(name_partner2_2),
        startRow = 4,
        startCol = 2
      )
      writeData(
        my_workbook,
        sheet = 3,
        inputTable1_graph2_CT(),
        startRow = 5,
        startCol = 1
      )
      

      # #sheet 4
      addWorksheet(
        wb = my_workbook,
        sheetName = "Fractional Occupancy (2)"
      )
      writeData(
        my_workbook,
        sheet = 4,
        paste("2. Fractional Occupancy of ",input$ligand_name_CT,
              " in function of concentration of", input$receptor_name_CT),
        startRow = 1,
        startCol = 1
      )
      writeData(
        my_workbook,
        sheet = 4,
        paste(name_partner1_2),
        startRow = 4,
        startCol = 1
      )
      writeData(
        my_workbook,
        sheet = 4,
        paste(name_partner2_2),
        startRow = 4,
        startCol = 2
      )
      writeData(
        my_workbook,
        sheet = 4,
        inputTable2_graph2_CT(),
        startRow = 5,
        startCol = 1
      )
      
      saveWorkbook(my_workbook, file)
      
      # tbl1 <- inputTable1_graph1_CT()
      # tbl2 <- inputTable2_graph1_CT()
      # tbl3 <- inputTable1_graph2_CT()
      # tbl4 <- inputTable2_graph2_CT()
      # 
      # sheets <- mget(ls(pattern = "tbl"))
      # names(sheets) <- c ("Concentration of Complex - Fixed value 1", "Concentration of Complex - Fixed value 1",
      #                     "Fractional Occupancy of Partner 2 - Fixed value 1", 
      #                     "Fractional Occupancy of Partner 2 - Fixed value 2")
      # 
      # writexl::write_xlsx(sheets, path = file) # saving the file
      
    }
  )
  
  
  # filters
  observeEvent(input$clear_filters_competition, {
    confirmSweetAlert(
      session = session,
      inputId = "myconfirmation_filters_competition",
      type = "warning",
      title = "Clear current selections?",
      danger_mode = TRUE
    )
  })
  
  observeEvent (input$myconfirmation_filters_competition, {
    
    if (input$myconfirmation_filters_competition == TRUE) {
      shinyjs::reset ("submit_query_competition")
      
      updateTextInput (session, "receptor_name_CT", value = "Partner 1 name")
      updateRadioButtons(session,"p1concunit_CT", selected = "metric units")
      updateSelectInput(session,"p1unit_CT_metric", selected = "pg/ml")
      updateNumericInput(session, "c_total_receptor_CT_start", value = 100)
      updateNumericInput(session, "c_total_receptor_CT", value = 100)
      updateNumericInput(session, "mol_weight_p1_CT", value = 100)
      updateSliderInput(session, "rec_range_CT",value=c(-2,2))
      updateMaterialSwitch(session, "adv_slider_p1_CT", value = FALSE)
      
      updateTextInput (session, "ligand_name_CT", value = "Partner 2 name")
      updateRadioButtons(session,"p2concunit_CT", selected = "metric units")
      updateSelectInput(session,"p2unit_CT_metric", selected = "pg/ml")
      updateNumericInput(session, "c_total_ligand_CT_start", value = 100)
      updateNumericInput(session, "c_total_ligand_CT", value = 100)
      updateNumericInput(session, "mol_weight_p2_CT", value = 100)
      updateSliderInput(session, "lig_range_CT",value=c(-4, 2))
      updateMaterialSwitch(session, "adv_slider_p2_CT", value = FALSE)
      
      
      updateNumericInput(session, "Kd_CT", value = 100)
      updateSliderInput(session, "kd_range_CT",value = c(-4, 2))
      updateMaterialSwitch(session, "adv_slider_kd_CT", value = FALSE)
      
      
      updateTextInput (session, "coating_name_CT", value = "Competing partner name")
      updateRadioButtons(session,"coat_concunit_CT", selected = "metric units")
      updateSelectInput(session,"coat_unit_CT_metric", selected = "pg/ml")
      updateNumericInput(session, "c_total_coating_CT_start", value = 100)
      updateNumericInput(session, "c_total_coating_CT", value = 100)
      updateNumericInput(session, "mol_weight_coat", value = 100)
      updateSliderInput(session, "coat_range_CT",value=c(-4, 2))
      updateMaterialSwitch(session, "adv_slider_comp", value = FALSE)
      
      
      updateNumericInput(session, "ki_CT", value = 1000)
      updateSliderInput(session, "pKdcoat_CT",value = c(6,8))
      updateMaterialSwitch(session, "adv_slider_ki", value = FALSE)
    }
  })
  
  
  get_units <- function (dummy){
    
    if (input$p1concunit_CT == "metric units"){
      
      my.p1.units <- input$p1unit_CT_metric
      my.conc <- input$c_total_receptor_CT_start
    }else if (input$p1concunit_CT == "molar units"){
      my.p1.units <- input$p1unit_CT_molar
      my.conc <- input$c_total_receptor_CT
    }
    
    
    if (input$p2concunit_CT == "metric units"){
      
      my.p2.units <- input$p2unit_CT_metric
      # print ("testing")
      # print (my.p2.units)
      my.conc <- input$c_total_ligand_CT_start
    }else if (input$p2concunit_CT == "molar units"){
      my.p2.units <- input$p2unit_CT_molar
      my.conc <- input$c_total_ligand_CT
    }
    
    if (input$coat_concunit_CT == "metric units"){
      
      my.coat.units <- input$coat_unit_CT_metric
      my.conc <- input$c_total_coating_CT_start
    }else if (input$coat_concunit_CT == "molar units"){
      my.coat.units <- input$coat_unit_CT_molar
      my.conc <- input$c_total_coating_CT
    }
    
    if (dummy == "KI") {
      my.units <- "pM"
    }else if (dummy == "KD"){
      my.units <- " pM"
    } else if (dummy == input$ligand_name_CT){
      my.units <- my.p2.units
      
    } else if (dummy == input$receptor_name_CT){
      my.units <- my.p1.units
    } else if (dummy == input$coating_name_CT){
      my.units <- my.coat.units
    }else {
      my.units <- "Please choose a variable"
    }
    
    
    return (my.units)
    
  }
  
  
  #The competition tab code
  
  #refresh/go buttons control
  
  output$comp_instructions <- renderUI ({
    
    fluidRow(
      box(width=12,
          column(width=1,
                 div(img(src = "info_icon.png", height = "50%", style = "width:50px; margin:20px; margin-left:10px"), style="text-align: center; "
                 )
          ),
          column(width=11,
                 HTML("<p>For all graphs below, please select the variables that you want to see plotted from the dropdown menus below. 
      <br><ul><li>The first variable, i.e. ‘fixed variable’, will be plotted as two individual graphs, one for each of the values entered, i.e. value 1 in the graph on the left and value 2 in the graph on the right. 
      <li>The second variable, i.e. plotted variable’, will be represented by the colored curves in both graphs and the legends below the graphs, based on the concentration range entered in the input panel above. </ul>
      Note that the values from the two remaining variables that were not selected will be based on the fixed value the user entered in the input panel above.
                                "),
                 style = "margin-top: 20px; margin-left:-20px;font-size: 14px; margin-bottom: 20px; color: #718096; font-style: Lato;"
          ))
    )
    
  })
  
  #header for graph 1 in competition tab
  output$controls_header <- renderUI ({
    
    fluidRow(
      box(width=12,
          style = "background-color: #293887",
          div(
            # HTML(paste("1. Concentration",  em(input$receptor_name_CT), "- ", input$ligand_name_CT, "in complex (pM) in function of concentration of ",
            # em(input$receptor_name_CT))),
            
            
            
            HTML(paste("1. Concentration of ",em(input$receptor_name_CT),"-", em(input$ligand_name_CT), 
                       " in complex in function of concentration of", em(input$receptor_name_CT))),
            style = "margin-top: 20px; font-size: 14px; margin-bottom: 20px; color: #FFFFFF; font-style: Lato;")
      )
    )
    
  })
  #header for graph 2 in competition tab
  output$controls_header_graph2 <- renderUI ({
    
    fluidRow(
      box(width=12,
          style = "background-color: #293887",
          div(
            # HTML(paste("1. Concentration",  em(input$receptor_name_CT), "- ", input$ligand_name_CT, "in complex (pM) in function of concentration of ",
            # em(input$receptor_name_CT))),
            
            
            
            HTML(paste("2. Fractional Occupancy of ",em(input$ligand_name_CT),
                       " in function of concentration of", em(input$receptor_name_CT))),
            style = "margin-top: 20px; font-size: 14px; margin-bottom: 20px; color: #FFFFFF; font-style: Lato;")
      )
    )
    
  })
  
  #update user supplied names for variables
  observeEvent( c(input$receptor_name_CT, input$ligand_name_CT, input$coating_name_CT), {
    
    updateSelectInput(session, "A_selected_first_var", label = 'Change fixed variable',
                      choices = list("KD", input$coating_name_CT, input$ligand_name_CT,"KI"),
                      selected = "KD")
    
    updateSelectInput(session, "A_change_first_var", label = 'Change plotted variable',
                      choices = list("KD", input$coating_name_CT, input$ligand_name_CT,"KI"),
                      selected = input$ligand_name_CT)
    
    updateSelectInput(session, "A_selected_first_var_graph2", label = 'Change fixed variable',
                      choices = list("KD", input$coating_name_CT, input$ligand_name_CT,"KI"),
                      selected = "KD")
    
    updateSelectInput(session, "A_change_first_var_graph2", label = 'Change plotted variable',
                      choices = list("KD", input$coating_name_CT, input$ligand_name_CT,"KI"),
                      selected = input$ligand_name_CT)
    
  })
  
  
  #update drop down for graph 1
  observeEvent( c(input$A_selected_first_var, input$A_change_first_var), {
    
    current_var_change <- input$A_change_first_var
    current_var_select <- input$A_selected_first_var
    
    #watch the change var/plotted variable
    if (input$A_change_first_var == "KD"){
      updateSelectInput(session, "A_selected_first_var", 
                        choices = list(input$coating_name_CT, input$ligand_name_CT,"KI"),
                        selected = current_var_select)
      
      
    } else if (input$A_change_first_var == "KI"){
      updateSelectInput(session, "A_selected_first_var", label = 'Change fixed variable',
                        choices = list("KD", input$coating_name_CT, input$ligand_name_CT),
                        selected = current_var_select)
      
    } else if (input$A_change_first_var == input$coating_name_CT){
      updateSelectInput(session, "A_selected_first_var", label = 'Change fixed variable',
                        choices = list("KD", input$ligand_name_CT, "KI"),
                        selected = current_var_select)
    }else if (input$A_change_first_var == input$ligand_name_CT){
      
      updateSelectInput(session, "A_selected_first_var", label = 'Change fixed variable',
                        choices = list("KD", input$coating_name_CT, "KI"),
                        selected = current_var_select)
    }
    
    #watch the selected var
    if (input$A_selected_first_var == "KD"){
      updateSelectInput(session, "A_change_first_var", 
                        choices = list(input$coating_name_CT, input$ligand_name_CT,"KI"),
                        selected = current_var_change)
      
      
    } else if (input$A_selected_first_var == "KI"){
      updateSelectInput(session, "A_change_first_var", label = 'Change plotted variable',
                        choices = list("KD", input$coating_name_CT, input$ligand_name_CT),
                        selected = current_var_change)
      
    } else if (input$A_selected_first_var == input$coating_name_CT){
      updateSelectInput(session, "A_change_first_var", label = 'Change plotted variable',
                        choices = list("KD", input$ligand_name_CT, "KI"),
                        selected = current_var_change)
    }else if (input$A_selected_first_var == input$ligand_name_CT){
      
      
      updateSelectInput(session, "A_change_first_var", label = 'Change plotted variable',
                        choices = list("KD", input$coating_name_CT, "KI"),
                        selected = current_var_change)
    }
    
  })
  
  #update drop down for graph 2
  observeEvent( c(input$A_selected_first_var_graph2, input$A_change_first_var_graph2), {
    
    current_var_change <- input$A_change_first_var_graph2
    current_var_select <- input$A_selected_first_var_graph2
    
    #watch the change var/plotted variable
    if (input$A_change_first_var_graph2 == "KD"){
      updateSelectInput(session, "A_selected_first_var_graph2", 
                        choices = list(input$coating_name_CT, input$ligand_name_CT,"KI"),
                        selected = current_var_select)
      
      
    } else if (input$A_change_first_var_graph2 == "KI"){
      updateSelectInput(session, "A_selected_first_var_graph2", label = 'Change fixed variable',
                        choices = list("KD", input$coating_name_CT, input$ligand_name_CT),
                        selected = current_var_select)
      
    } else if (input$A_change_first_var_graph2 == input$coating_name_CT){
      updateSelectInput(session, "A_selected_first_var_graph2", label = 'Change fixed variable',
                        choices = list("KD", input$ligand_name_CT, "KI"),
                        selected = current_var_select)
    }else if (input$A_change_first_var_graph2 == input$ligand_name_CT){
      
      updateSelectInput(session, "A_selected_first_var_graph2", label = 'Change fixed variable',
                        choices = list("KD", input$coating_name_CT, "KI"),
                        selected = current_var_select)
    }
    
    #watch the selected var
    if (input$A_selected_first_var_graph2 == "KD"){
      updateSelectInput(session, "A_change_first_var_graph2", 
                        choices = list(input$coating_name_CT, input$ligand_name_CT,"KI"),
                        selected = current_var_change)
      
      
    } else if (input$A_selected_first_var_graph2 == "KI"){
      updateSelectInput(session, "A_change_first_var_graph2", label = 'Change plotted variable',
                        choices = list("KD", input$coating_name_CT, input$ligand_name_CT),
                        selected = current_var_change)
      
    } else if (input$A_selected_first_var_graph2 == input$coating_name_CT){
      updateSelectInput(session, "A_change_first_var_graph2", label = 'Change plotted variable',
                        choices = list("KD", input$ligand_name_CT, "KI"),
                        selected = current_var_change)
    }else if (input$A_selected_first_var_graph2 == input$ligand_name_CT){
      
      
      updateSelectInput(session, "A_change_first_var_graph2", label = 'Change plotted variable',
                        choices = list("KD", input$coating_name_CT, "KI"),
                        selected = current_var_change)
    }
    
  })
  
  #update value 1 for graph 1
  observeEvent(c(input$coat_concunit_CT, input$p1concunit_CT, input$p2concunit_CT,
                 input$c_total_receptor_CT_start, input$c_total_ligand_CT_start, input$c_total_coating_CT_start,
                 input$A_selected_first_var, input$A_change_first_var, input$p1unit_CT_metric, input$p1unit_CT_molar,
                 input$p2unit_CT_metric, input$p2unit_CT_molar, input$coat_unit_CT_metric, input$coat_unit_CT_molar),{
                   
                   if (!is.null (input$A_selected_first_var)){
                     
                     updateNumericInput(session, "A_first_var_value", label = paste("Value 1 for ", input$A_selected_first_var, " (",
                                                                                    HTML(get_units (input$A_selected_first_var)),
                                                                                    ")"),
                                        value = input$A_first_var_value)
                   }
                   
                 })
  
  #update value 2 for graph 1
  observeEvent(c(input$coat_concunit_CT, input$p1concunit_CT, input$p2concunit_CT, 
                 input$c_total_receptor_CT_start, input$c_total_ligand_CT_start, input$c_total_coating_CT_start,
                 input$A_selected_first_var, input$A_change_first_var, input$p1unit_CT_metric, input$p1unit_CT_molar,
                 input$p2unit_CT_metric, input$p2unit_CT_molar, input$coat_unit_CT_metric, input$coat_unit_CT_molar),{
                   
                   if (!is.null (input$A_selected_first_var)){
                     
                     updateNumericInput(session, "A_first_var_value2", label = paste("Value 2 for ", input$A_selected_first_var, " (",
                                                                                     HTML(get_units (input$A_selected_first_var)),
                                                                                     ")"),
                                        value = input$A_first_var_value2 )
                   }
                   
                 })
  
  #update value 1 for graph 2
  observeEvent(c(input$coat_concunit_CT, input$p1concunit_CT, input$p2concunit_CT,
                 input$c_total_receptor_CT_start, input$c_total_ligand_CT_start, input$c_total_coating_CT_start,
                 input$A_selected_first_var_graph2, input$A_change_first_var_graph2, input$p1unit_CT_metric, input$p1unit_CT_molar,
                 input$p2unit_CT_metric, input$p2unit_CT_molar, input$coat_unit_CT_metric, input$coat_unit_CT_molar),{
                   
                   if (!is.null (input$A_selected_first_var_graph2)){
                     
                     updateNumericInput(session, "A_first_var_value_graph2", label = paste("Value 1 for ", input$A_selected_first_var_graph2, " (",
                                                                                           HTML(get_units (input$A_selected_first_var_graph2)),
                                                                                           ")"),
                                        value = input$A_first_var_value_graph2)
                   }
                   
                 })
  
  #update value 2 for graph 2
  observeEvent(c(input$coat_concunit_CT, input$p1concunit_CT, input$p2concunit_CT, 
                 input$c_total_receptor_CT_start, input$c_total_ligand_CT_start, input$c_total_coating_CT_start,
                 input$A_selected_first_var_graph2, input$A_change_first_var_graph2, input$p1unit_CT_metric, input$p1unit_CT_molar,
                 input$p2unit_CT_metric, input$p2unit_CT_molar, input$coat_unit_CT_metric, input$coat_unit_CT_molar),{
                   
                   if (!is.null (input$A_selected_first_var_graph2)){
                     
                     updateNumericInput(session, "A_first_var_value2_graph2", label = paste("Value 2 for ", input$A_selected_first_var_graph2, " (",
                                                                                            HTML(get_units (input$A_selected_first_var_graph2)),
                                                                                            ")"),
                                        value = input$A_first_var_value2_ )
                   }
                   
                 })
  
  
  
  #helper functions for plots
  convert_units_p2 <- function(dummy){
    
    
    if (input$p2concunit_CT == "metric units"){
      
      
      temp <- input$c_total_ligand_CT_start
      
      if (input$p2unit_CT_metric == "ug/ml"){
        
        temp = dummy / 1e6 / input$mol_weight_p2_CT * 10^12 * 1000
      }else if (input$p2unit_CT_metric == "mg/ml"){
        
        temp = dummy / 1e3 / input$mol_weight_p2_CT * 10^12 * 1000
      }else if (input$p2unit_CT_metric == "ng/ml"){
        
        # temp = format  (input$c_total_ligand_start / 1e9 / input$mol_weight_p2 * 10^12 * 1000, scientific = T)
        temp = dummy / 1e9 / input$mol_weight_p2_CT * 10^12 * 1000
        
      }else if (input$p2unit_CT_metric == "pg/ml"){
        
        temp = dummy / 1e12/ input$mol_weight_p2_CT * 10^12 * 1000
      }
      
      
      # )
    } else if (input$p2concunit_CT == "molar units"){
      
      
      temp <- input$c_total_ligand_CT_start
      
      if (input$p2unit_CT_molar == "pM"){
        
        temp = dummy
        
      } else  if (input$p2unit_CT_molar == "uM"){
        print ("uM")
        
        temp = dummy / 1e-6 
        
      } else if (input$p2unit_CT_molar == "fM"){
        
        temp =  temp <- dummy / 1e3
        
      } else if (input$p2unit_CT_molar == "nM"){
        
        temp = dummy / 1e-3
      } else if (input$p2unit_CT_molar == "mM"){
        
        temp = dummy / 1e-9
        
      }
      
    }
    
    return (temp)
  }
  
  
  convert_units_coat<- function(dummy){
    
    if (input$coat_concunit_CT == "metric units"){
      
      # print (paste("my starting coat is:", dummy))
      
      if (input$coat_unit_CT_metric == "ug/ml"){
        
        temp = dummy / 1e6 / input$mol_weight_p2_CT * 10^12 * 1000
      }else if (input$coat_unit_CT_metric == "mg/ml"){
        
        temp = dummy / 1e3 / input$mol_weight_p2_CT * 10^12 * 1000
      }else if (input$coat_unit_CT_metric == "ng/ml"){
        
        # temp = format  (input$c_total_ligand_start / 1e9 / input$mol_weight_p2 * 10^12 * 1000, scientific = T)
        temp = dummy / 1e9 / input$mol_weight_p2_CT * 10^12 * 1000
        
      }else if (input$coat_unit_CT_metric == "pg/ml"){
        
        temp = dummy / 1e12/ input$mol_weight_p2_CT * 10^12 * 1000
      }
      
      
      # )
    } else if (input$coat_concunit_CT == "molar units"){
      
      if (input$coat_unit_CT_molar == "pM"){
        
        temp = dummy
        
      } else  if (input$coat_unit_CT_molar == "uM"){
        print ("uM")
        
        temp = dummy / 1e-6 
        
      } else if (input$coat_unit_CT_molar == "fM"){
        
        temp = dummy / 1e3
        
      } else if (input$coat_unit_CT_molar == "nM"){
        
        temp = dummy / 1e-3
      } else if (input$coat_unit_CT_molar == "mM"){
        
        temp = dummy / 1e-9
        
      }
      
    }
    
    # print (paste("converted coat:", temp))
    
    return (temp)
  }
  
  #convert back to input units for plots after performing calculations
  convert_units_back_p1_CT <- function (my.value){
    # print (paste("my value:", my.value))
    
    if(input$p1concunit_CT == "molar units"){
      
      if (input$p1unit_CT_molar == "pM"){
        
        fixed_receptor = my.value
        
        
      } else  if (input$p1unit_CT_molar == "uM"){
        
        fixed_receptor = my.value *  1e-6 
        
      } else if (input$p1unit_CT_molar == "fM"){
        
        fixed_receptor = my.value * 1e3 
        
      } else if (input$p1unit_CT_molar == "nM"){
        
        fixed_receptor = my.value * 1e-3
      } else if (input$p1unit_CT_molar == "mM"){
        
        fixed_receptor = my.value * 1e-9
        
      }
      
      
    } else if (input$p1concunit_CT == "metric units"){
      
      #convert receptor back to original units
      if (input$p1unit_CT_metric == "ug/ml"){
        
        fixed_receptor <- my.value*(1/10^12) *(1/1000) * input$mol_weight_p1_CT * 10^6
        
        
        # temp = format  (input$c_total_receptor / 1e6 / input$mol_weight_p1 * 10^12 * 1000, scientific = T)
      }else if (input$p1unit_CT_metric == "mg/ml"){
        
        fixed_receptor <- my.value *(1/10^12) *(1/1000) * input$mol_weight_p1_CT * 10^3
        
        # temp = format  (input$c_total_receptor / 1e3 / input$mol_weight_p1 * 10^12 * 1000, scientific = T)
      }else if (input$p1unit_CT_metric == "ng/ml"){
        
        fixed_receptor <- my.value *(1/10^12) *(1/1000) * input$mol_weight_p1_CT * 10^9
        
        # temp = format  (input$c_total_receptor / 1e9 / input$mol_weight_p1 * 10^12 * 1000, scientific = T)
        
      }else if (input$p1unit_CT_metric == "pg/ml"){
        
        # print("hello")
        fixed_receptor <- my.value *(1/10^12) *(1/1000) * input$mol_weight_p1_CT * 10^12
      }
    }
    
    return (fixed_receptor)
  }
  
  convert_units_back_p2_CT <- function (my.value){
    
    #convert ligand back to original units
    if(input$p2concunit_CT == "molar units"){
      
      if (input$p2unit_CT_molar == "pM"){
        
        fixed_ligand <- my.value
        print (paste("fix lig", fixed_ligand))
        
      } else  if (input$p2unit_CT_molar == "uM"){
        fixed_ligand = my.value * 1e-6 
        
      } else if (input$p2unit_CT_molar == "fM"){
        
        fixed_ligand = my.value * 1e3 
        
      } else if (input$p2unit_CT_molar == "nM"){
        
        fixed_ligand = my.value * 1e-3
        
      } else if (input$p2unit_CT_molar == "mM"){
        
        fixed_ligand = my.value * 1e-9
        
      }
      
    } else if (input$p2concunit_CT == "metric units"){
      
      # print (input$p2concunit)
      
      
      if (input$p2unit_CT_metric == "ug/ml"){
        
        fixed_ligand <- my.value *(1/10^12) *(1/1000) * input$mol_weight_p2_CT * 10^6
        
      }else if (input$p2unit_CT_metric == "mg/ml"){
        
        fixed_ligand <- my.value *(1/10^12) *(1/1000) * input$mol_weight_p2_CT * 10^3
        
        
      }else if (input$p2unit_CT_metric == "ng/ml"){
        
        fixed_ligand <- my.value *(1/10^12) *(1/1000) * input$mol_weight_p2_CT * 10^9
        
      }else if (input$p2unit_CT_metric == "pg/ml"){
        
        fixed_ligand <- my.value *(1/10^12) *(1/1000) * input$mol_weight_p2_CT * 10^12
        
      }
    }
    
    return (fixed_ligand)
  }
  
  
  convert_units_back_coat <- function (my.value){
    
    #convert ligand back to original units
    if(input$coat_concunit_CT == "molar units"){
      
      if (input$coat_unit_CT_molar == "pM"){
        
        fixed_coat <- my.value
        
        
      } else  if (input$coat_unit_CT_molar == "uM"){
        fixed_coat = my.value * 1e-6 
        
      } else if (input$coat_unit_CT_molar == "fM"){
        
        fixed_coat = my.value * 1e3 
        
      } else if (input$coat_unit_CT_molar == "nM"){
        
        fixed_coat = my.value * 1e-3
        
      } else if (input$coat_unit_CT_molar == "mM"){
        
        fixed_coat = my.value * 1e-9
        
      }
      
    } else if (input$coat_concunit_CT == "metric units"){
      
      
      if (input$coat_unit_CT_metric == "ug/ml"){
        
        fixed_coat <- my.value *(1/10^12) *(1/1000) * input$mol_weight_coat * 10^6
        
      }else if (input$coat_unit_CT_metric == "mg/ml"){
        
        fixed_coat <- my.value *(1/10^12) *(1/1000) * input$mol_weight_coat * 10^3
        
        
      }else if (input$coat_unit_CT_metric == "ng/ml"){
        
        fixed_coat <- my.value *(1/10^12) *(1/1000) * input$mol_weight_coat * 10^9
        
      }else if (input$coat_unit_CT_metric == "pg/ml"){
        
        fixed_coat <- my.value *(1/10^12) *(1/1000) * input$mol_weight_coat * 10^12
        
      }
    }
    
    return (fixed_coat)
  }
  ##############################PLOTS ####################
  #graph 1
  # complex by p1 over p2
  # #x-axis partner 1, each line partner 2
  cxbyrecrangeligdataone <- reactive({
    volume <- 1
    
    fixed_ligand <- input$c_total_ligand_CT
    fixed_receptor <- input$c_total_receptor_CT
    fixed_coat <- input$c_total_coating_CT
    # fixed_ligand <- ifelse (input$p2concunit_CT == "metric units", input$c_total_ligand_CT_start, input$c_total_ligand_CT)
    # fixed_receptor <- ifelse (input$p1concunit_CT == "metric units", input$c_total_receptor_CT_start, input$c_total_receptor_CT)
    # fixed_coat <- ifelse (input$coat_concunit_CT == "metric units", input$c_total_coating_CT_start, input$c_total_coating_CT)
    
    
    # print (paste("inside plot 1: ", fixed_receptor))
    # print (paste("inside plot 1: ", fixed_ligand))
    # print (paste("inside plot 1: ", fixed_coat))
    
    # print("fixing this:")
    # print (fixed_ligand)
    # print(fixed_receptor)
    
    
    if (input$A_selected_first_var == "KD"){
      fixed_kd <- input$A_first_var_value
    }else {
      fixed_kd <- input$Kd_CT
    }
    
    if (input$A_selected_first_var == input$ligand_name_CT){
      fixed_ligand <- convert_units_p2 (input$A_first_var_value)
      # print (fixed_ligand)
      # print (paste("testing", fixed_ligand))
      
    } 
    # else if(input$p2concunit_CT == "molar units"){
    #   
    #   fixed_ligand <- input$c_total_ligand_CT
    #   # print (fixed_coat)
    #   
    # } else if (input$p2concunit_CT == "metric units"){
    #   
    #   fixed_ligand <- input$c_total_ligand_CT_start
    #   
    #   # print (fixed_coat)
    # }
    
    if (input$A_selected_first_var == input$coating_name_CT){
      fixed_coat <- convert_units_coat(input$A_first_var_value)
    } 
    # else if(input$coat_concunit_CT == "molar units"){
    #   
    #   fixed_coat <- as.numeric (input$c_total_coating_CT)
    #   # print (fixed_coat)
    #   
    # } else if (input$coat_concunit_CT == "metric units"){
    #   
    #   fixed_coat <- input$c_total_coating_CT_start
    #   
    #   # print (fixed_coat)
    # }
    
    if (input$A_selected_first_var == "KI"){
      kdcoating <- input$A_first_var_value
    }else {
      kdcoating <- input$ki_CT
    }
    
    
    # if (fixed_coat==0) {fixed_coat <- 1000}
    if (fixed_coat<=5) {fixed_coat <- 1000}
    if (fixed_ligand<=5) {fixed_ligand <- 1000}
    # seq.lig <- seq(from=input$lig_range_CT[1],to=input$lig_range_CT[2],by=1)
    seq.lig <- seq(from=input$lig_range_CT[1],to=input$lig_range_CT[2],by=input$plot_interval_p2_CT)
    lig.max <-input$lig_range_CT[2]
    seq.lig <- unique(append(seq.lig, lig.max))
    
    # seq.rec <- seq(from=input$rec_range_CT[1],to=input$rec_range_CT[2],by=0.2)
    seq.rec <- seq(from=input$rec_range_CT[1],to=input$rec_range_CT[2],by=input$plot_interval_p1_CT)
    rec.max <- input$rec_range_CT[2]
    seq.rec <- unique(append(seq.rec, rec.max))
    
    # seq.kd <- seq(from=input$kd_range_CT[1],to=input$kd_range_CT[2],by=1)
    seq.kd <- seq(from=input$kd_range_CT[1],to=input$kd_range_CT[2],by=input$plot_interval_kd_CT)
    kd.max <- input$kd_range_CT[2]
    seq.kd <- unique(append(seq.kd, kd.max))
    
    # seq.coat <- seq(from=input$coat_range_CT[1],to=input$coat_range_CT[2],by=0.2)
    seq.coat <- seq(from=input$coat_range_CT[1],to=input$coat_range_CT[2],by=input$plot_interval_comp)
    coat.max <- input$coat_range_CT[2]
    seq.coat <- unique(append(seq.coat, coat.max))
    
    seq.ki <- seq(from=input$pKdcoat_CT[1],to=input$pKdcoat_CT[2],by=input$plot_interval_ki)
    ki.max <- input$pKdcoat_CT[2]
    seq.ki <- unique(append(seq.ki, ki.max))
    
    min.coat <- 10^min(seq.coat)*fixed_coat
    max.coat <- 10^max(seq.coat)*fixed_coat
    min.lig <- 10^min(seq.lig)*fixed_ligand
    max.lig <- 10^max(seq.lig)*fixed_ligand
    min.rec <- 10^min(seq.rec)*fixed_receptor
    max.rec <- 10^max(seq.rec)*fixed_receptor
    
    rec.dots <- 10^seq.rec*fixed_receptor
    leggy <- c()
    my.df <- NULL
    mylist <- NULL
    
    
    #if competitor or kd
    for (lig.cnt in seq.lig) {
      this.lig <- 10^lig.cnt*fixed_ligand
      ## now each line
      cx.dots <- c()
      for (rec.cnt in seq.rec) {
        this.rec <- 10^rec.cnt*fixed_receptor
        gfun <- function(x) {c(this.rec - x[1] - x[2]*x[1]/kdcoating - x[1]*x[3]/fixed_kd, fixed_coat-x[2]-x[2]*x[1]/kdcoating, this.lig-x[3]-x[1]*x[3]/fixed_kd)}
        fred <- multiroot( gfun, start=c(this.rec/2,fixed_coat/2,this.lig/2), positive=TRUE  )
        c_free_receptor <- fred$root[1]
        c_free_coat <- fred$root[2]
        c_free_ligand <- fred$root[3]
        c_complex <- this.lig - c_free_ligand
        cx.dots <- c(cx.dots, c_complex)
        my.df <- (cbind(rec.dots, this.lig, cx.dots))
      }
      mylist <- data.frame(rbind(mylist, my.df))
      
    }
    
    
    if (input$p1concunit_CT == "metric units"){
      print("yes it does")
      rec.dots <- 10^seq.rec*input$c_total_receptor_CT_start
      this.lig <- 10^seq.lig*input$c_total_ligand_CT_start
      cx.dots <- mylist[,3]
      
      temp <- NULL
      for (i in 1:length(this.lig)){
        temp[[i]] <- cbind(rec.dots, this.lig[i])
      }
      temp <- do.call(rbind.data.frame, temp)
      colnames(temp)[2] <- "this.lig"
      
      mylist <- cbind.data.frame(temp, cx.dots)
      str(mylist)
      print(mylist)
      
    }
    
    # for (i in 1:nrow(mylist)){
    # 
    #   mylist$rec.dots[i] <- round(convert_units_back_p1_CT (mylist$rec.dots[i]),1)
    #   mylist$this.lig[i] <- round(convert_units_back_p2_CT (mylist$this.lig[i]),2)
    # }
    
    return(mylist)
  })
  
  #plot 2
  cxbyrecrangeligdatatwo <- reactive({
    
    fixed_ligand <- input$c_total_ligand_CT
    fixed_receptor <- input$c_total_receptor_CT
    fixed_coat <- input$c_total_coating_CT
    
    volume <- 1
    
    if (input$A_selected_first_var == "KD"){
      fixed_kd <- input$A_first_var_value2
    }else {
      fixed_kd <- input$Kd_CT
    }
    
    if (input$A_selected_first_var == input$ligand_name_CT){
      # fixed_ligand <- input$A_first_var_value2
      fixed_ligand <- convert_units_p2 (input$A_first_var_value2)
      
    } 
    # else if(input$p2concunit_CT == "molar units"){
    #   
    #   fixed_ligand <- input$c_total_ligand_CT
    #   # print (fixed_coat)
    #   
    # } else if (input$p2concunit_CT == "metric units"){
    #   
    #   fixed_ligand <- input$c_total_ligand_CT_start
    #   
    #   # print (fixed_coat)
    # }
    
    if (input$A_selected_first_var == input$coating_name_CT){
      fixed_coat <- convert_units_coat(input$A_first_var_value2)
    } 
    # else if(input$coat_concunit_CT == "molar units"){
    #   
    #   fixed_coat <- as.numeric (input$c_total_coating_CT)
    #   # print (fixed_coat)
    #   
    # } else if (input$coat_concunit_CT == "metric units"){
    #   
    #   fixed_coat <- input$c_total_coating_CT_start
    #   
    #   # print (fixed_coat)
    # }
    
    
    if (input$A_selected_first_var == "KI"){
      kdcoating <- input$A_first_var_value2
    }else {
      kdcoating <- input$ki_CT
    }
    
    
    # fixed_kd <- input$Kd_CT
    # kdcoating <- 10^(12-input$pKdcoat_CT[1])
    # fixed_receptor <- input$c_total_receptor_CT
    # fixed_ligand <- input$c_total_ligand_CT
    # fixed_coat <- input$c_total_coating_CT
    
    #   if (fixed_coat==0) {fixed_coat <- 1000}
    #   if (fixed_coat==0) {fixed_coat <- 1000}
    if (fixed_coat<=5) {fixed_coat <- 1000}
    if (fixed_ligand<=5) {fixed_ligand <- 1000}
    
    # seq.lig <- seq(from=input$lig_range_CT[1],to=input$lig_range_CT[2],by=1)
    seq.lig <- seq(from=input$lig_range_CT[1],to=input$lig_range_CT[2],by=input$plot_interval_p2_CT)
    lig.max <-input$lig_range_CT[2]
    seq.lig <- unique(append(seq.lig, lig.max))
    
    # seq.rec <- seq(from=input$rec_range_CT[1],to=input$rec_range_CT[2],by=0.2)
    seq.rec <- seq(from=input$rec_range_CT[1],to=input$rec_range_CT[2],by=input$plot_interval_p1_CT)
    rec.max <- input$rec_range_CT[2]
    seq.rec <- unique(append(seq.rec, rec.max))
    
    # seq.kd <- seq(from=input$kd_range_CT[1],to=input$kd_range_CT[2],by=1)
    seq.kd <- seq(from=input$kd_range_CT[1],to=input$kd_range_CT[2],by=input$plot_interval_kd_CT)
    kd.max <- input$kd_range_CT[2]
    seq.kd <- unique(append(seq.kd, kd.max))
    
    # seq.coat <- seq(from=input$coat_range_CT[1],to=input$coat_range_CT[2],by=0.2)
    seq.coat <- seq(from=input$coat_range_CT[1],to=input$coat_range_CT[2],by=input$plot_interval_comp)
    coat.max <- input$coat_range_CT[2]
    seq.coat <- unique(append(seq.coat, coat.max))
    
    seq.ki <- seq(from=input$pKdcoat_CT[1],to=input$pKdcoat_CT[2],by=input$plot_interval_ki)
    ki.max <- input$pKdcoat_CT[2]
    seq.ki <- unique(append(seq.ki, ki.max))
    
    min.coat <- 10^min(seq.coat)*fixed_coat
    max.coat <- 10^max(seq.coat)*fixed_coat
    min.lig <- 10^min(seq.lig)*fixed_ligand
    max.lig <- 10^max(seq.lig)*fixed_ligand
    min.rec <- 10^min(seq.rec)*fixed_receptor
    max.rec <- 10^max(seq.rec)*fixed_receptor
    rec.dots <- 10^seq.rec*fixed_receptor
    leggy <- c()
    my.df <- NULL
    mylist <- NULL
    for (lig.cnt in seq.lig) {
      this.lig <- 10^lig.cnt*fixed_ligand
      ## now each line
      cx.dots <- c()
      for (rec.cnt in seq.rec) {
        this.rec <- 10^rec.cnt*fixed_receptor
        gfun <- function(x) {c(this.rec - x[1] - x[2]*x[1]/kdcoating - x[1]*x[3]/fixed_kd, fixed_coat-x[2]-x[2]*x[1]/kdcoating, this.lig-x[3]-x[1]*x[3]/fixed_kd)}
        fred <- multiroot( gfun, start=c(this.rec/2,fixed_coat/2,this.lig/2), positive=TRUE  )
        c_free_receptor <- fred$root[1]
        c_free_coat <- fred$root[2]
        c_free_ligand <- fred$root[3]
        c_complex <- this.lig - c_free_ligand
        cx.dots <- c(cx.dots, c_complex)
        my.df <- (cbind(rec.dots, this.lig, cx.dots))
      }
      mylist <- data.frame(rbind(mylist, my.df))
    }
    
    
    if (input$p1concunit_CT == "metric units"){
      print("yes it does")
      print(seq.rec)
      print(seq.lig)
      rec.dots <- 10^seq.rec*input$c_total_receptor_CT_start
      this.lig <- 10^seq.lig*input$c_total_ligand_CT_start
      cx.dots <- mylist[,3]
      print(rec.dots)
      print(this.lig)
      print(cx.dots)
      
      temp <- NULL
      for (i in 1:length(this.lig)){
        temp[[i]] <- cbind(rec.dots, this.lig[i])
      }
      temp <- do.call(rbind.data.frame, temp)
      colnames(temp)[2] <- "this.lig"
      
      mylist <- cbind.data.frame(temp, cx.dots)
      str(mylist)
      print(mylist)
      
    }
    
    # for (i in 1:nrow(mylist)){
    #   
    #   mylist$rec.dots[i] <- round(convert_units_back_p1_CT (mylist$rec.dots[i]),1)
    #   mylist$this.lig[i] <- round(convert_units_back_p2_CT (mylist$this.lig[i]),2)
    # }
    # 
    return(mylist)
  })
  
  
  #complex by p1 over competitor
  #
  cxbyrecrangecoatdataone <- reactive({
    volume <- 1
    
    fixed_ligand <- input$c_total_ligand_CT
    fixed_receptor <- input$c_total_receptor_CT
    fixed_coat <- input$c_total_coating_CT
    # fixed_ligand <- ifelse (input$p2concunit_CT == "metric units", input$c_total_ligand_CT_start, input$c_total_ligand_CT)
    # fixed_receptor <- ifelse (input$p1concunit_CT == "metric units", input$c_total_receptor_CT_start, input$c_total_receptor_CT)
    # fixed_coat <- ifelse (input$coat_concunit_CT == "metric units", input$c_total_coating_CT_start, input$c_total_coating_CT)
    
    print ("looping over competitor")
    # print("fixing this:")
    print (fixed_ligand)
    print (fixed_receptor)
    print (fixed_coat)
    
    
    if (input$A_selected_first_var == "KD"){
      fixed_kd <- input$A_first_var_value
    }else {
      fixed_kd <- input$Kd_CT
    }
    
    print (paste("fixed Kd:", fixed_kd))
    
    if (input$A_selected_first_var == input$ligand_name_CT){
      fixed_ligand <- convert_units_p2 (input$A_first_var_value)
      # print (fixed_ligand)
      # print (paste("testing", fixed_ligand))
      
    } 
    # else if(input$p2concunit_CT == "molar units"){
    #   
    #   fixed_ligand <- input$c_total_ligand_CT
    #   # print (fixed_coat)
    #   
    # } else if (input$p2concunit_CT == "metric units"){
    #   
    #   fixed_ligand <- input$c_total_ligand_CT_start
    #   
    #   # print (fixed_coat)
    # }
    
    if (input$A_selected_first_var == input$coating_name_CT){
      fixed_coat <- convert_units_coat(input$A_first_var_value)
    }
    # } else if(input$coat_concunit_CT == "molar units"){
    #   
    #   fixed_coat <- as.numeric (input$c_total_coating_CT)
    #   # print (fixed_coat)
    #   
    # } else if (input$coat_concunit_CT == "metric units"){
    #   
    #   fixed_coat <- input$c_total_coating_CT_start
    #   
    #   # print (fixed_coat)
    # }
    
    if (input$A_selected_first_var == "KI"){
      kdcoating <- input$A_first_var_value
    }else {
      kdcoating <- input$ki_CT
    }
    
    
    #convert receptor back to original units
    # if(input$p1concunit_CT == "molar units"){
    #   
    #   fixed_receptor <- input$c_total_receptor_CT
    #   
    #   
    # } else if (input$p1concunit_CT == "metric units"){
    #   
    #   fixed_receptor <- input$c_total_receptor_CT_start
    #   # print (fixed_receptor)
    # }
    
    # if (fixed_coat==0) {fixed_coat <- 1000}
    if (fixed_coat<=5) {fixed_coat <- 1000}
    if (fixed_ligand<=5) {fixed_ligand <- 1000}
    # seq.lig <- seq(from=input$lig_range_CT[1],to=input$lig_range_CT[2],by=1)
    seq.lig <- seq(from=input$lig_range_CT[1],to=input$lig_range_CT[2],by=input$plot_interval_p2_CT)
    lig.max <-input$lig_range_CT[2]
    seq.lig <- unique(append(seq.lig, lig.max))
    
    # seq.rec <- seq(from=input$rec_range_CT[1],to=input$rec_range_CT[2],by=0.2)
    seq.rec <- seq(from=input$rec_range_CT[1],to=input$rec_range_CT[2],by=input$plot_interval_p1_CT)
    rec.max <- input$rec_range_CT[2]
    seq.rec <- unique(append(seq.rec, rec.max))
    
    # seq.kd <- seq(from=input$kd_range_CT[1],to=input$kd_range_CT[2],by=1)
    seq.kd <- seq(from=input$kd_range_CT[1],to=input$kd_range_CT[2],by=input$plot_interval_kd_CT)
    kd.max <- input$kd_range_CT[2]
    seq.kd <- unique(append(seq.kd, kd.max))
    
    # seq.coat <- seq(from=input$coat_range_CT[1],to=input$coat_range_CT[2],by=0.2)
    seq.coat <- seq(from=input$coat_range_CT[1],to=input$coat_range_CT[2],by=input$plot_interval_comp)
    coat.max <- input$coat_range_CT[2]
    seq.coat <- unique(append(seq.coat, coat.max))
    
    seq.ki <- seq(from=input$pKdcoat_CT[1],to=input$pKdcoat_CT[2],by=input$plot_interval_ki)
    ki.max <- input$pKdcoat_CT[2]
    seq.ki <- unique(append(seq.ki, ki.max))
    
    min.coat <- 10^min(seq.coat)*fixed_coat
    max.coat <- 10^max(seq.coat)*fixed_coat
    min.lig <- 10^min(seq.lig)*fixed_ligand
    max.lig <- 10^max(seq.lig)*fixed_ligand
    min.rec <- 10^min(seq.rec)*fixed_receptor
    max.rec <- 10^max(seq.rec)*fixed_receptor
    rec.dots <- 10^seq.rec*fixed_receptor
    leggy <- c()
    my.df <- NULL
    mylist <- NULL
    
    #if competitor or kd
    print(paste("fixed coat: ", fixed_coat))
    for (coat.cnt in seq.coat) {
      this.coat <- 10^coat.cnt*fixed_coat
      ## now each line
      cx.dots <- c()
      for (rec.cnt in seq.rec) {
        this.rec <- 10^rec.cnt*fixed_receptor
        gfun <- function(x) {c(this.rec - x[1] - x[2]*x[1]/kdcoating - x[1]*x[3]/fixed_kd, this.coat-x[2]-x[2]*x[1]/kdcoating, fixed_ligand-x[3]-x[1]*x[3]/fixed_kd)}
        fred <- multiroot( gfun, start=c(this.rec/2,this.coat/2,fixed_ligand/2), positive=TRUE )
        c_free_receptor <- fred$root[1]
        c_free_coat <- fred$root[2]
        c_free_ligand <- fred$root[3]
        c_complex <- fixed_ligand - c_free_ligand
        cx.dots <- c(cx.dots, c_complex)
        my.df <- (cbind( rec.dots, this.coat, cx.dots))
      }
      mylist <- data.frame(rbind(mylist, my.df))
      
    }
    
    if (input$p1concunit_CT == "metric units"){
      print("yes it does")
      rec.dots <- 10^seq.rec*input$c_total_receptor_CT_start
      this.coat <- 10^seq.coat*input$c_total_coating_CT_start
      cx.dots <- mylist[,3]
      
      temp <- NULL
      for (i in 1:length(this.coat)){
        temp[[i]] <- cbind(rec.dots, this.coat[i])
      }
      temp <- do.call(rbind.data.frame, temp)
      colnames(temp)[2] <- "this.coat"
      
      mylist <- cbind.data.frame(temp, cx.dots)
      str(mylist)
      print(mylist)
      
    }
    
    # for (i in 1:nrow(mylist)){
    #   
    #   mylist$rec.dots[i] <- round(convert_units_back_p1_CT (mylist$rec.dots[i]),1)
    #   mylist$this.coat[i] <- round(convert_units_back_coat (mylist$this.coat[i]),2)
    # }
    
    return(mylist)
  })
  
  #plot 2
  cxbyrecrangecoatdatatwo <- reactive({
    
    fixed_ligand <- input$c_total_ligand_CT
    fixed_receptor <- input$c_total_receptor_CT
    fixed_coat <- input$c_total_coating_CT
    
    volume <- 1
    
    if (input$A_selected_first_var == "KD"){
      fixed_kd <- input$A_first_var_value2
    }else {
      fixed_kd <- input$Kd_CT
    }
    
    if (input$A_selected_first_var == input$ligand_name_CT){
      # fixed_ligand <- input$A_first_var_value2
      fixed_ligand <- convert_units_p2 (input$A_first_var_value2)
      
    } 
    # if(input$p2concunit_CT == "molar units"){
    #   
    #   fixed_ligand <- input$c_total_ligand_CT
    #   # print (fixed_coat)
    #   
    # } else if (input$p2concunit_CT == "metric units"){
    #   
    #   fixed_ligand <- input$c_total_ligand_CT_start
    #   
    #   # print (fixed_coat)
    # }
    
    if (input$A_selected_first_var == input$coating_name_CT){
      fixed_coat <- convert_units_coat(input$A_first_var_value)
    } 
    # else if(input$coat_concunit_CT == "molar units"){
    #   
    #   fixed_coat <- as.numeric (input$c_total_coating_CT)
    #   # print (fixed_coat)
    #   
    # } else if (input$coat_concunit_CT == "metric units"){
    #   
    #   fixed_coat <- input$c_total_coating_CT_start
    #   
    #   # print (fixed_coat)
    # }
    
    
    if (input$A_selected_first_var == "KI"){
      kdcoating <- input$A_first_var_value2
    }else {
      kdcoating <- input$ki_CT
    }
    
    # if (fixed_coat==0) {fixed_coat <- 1000}
    if (fixed_coat<=5) {fixed_coat <- 1000}
    if (fixed_ligand<=5) {fixed_ligand <- 1000}
    # seq.lig <- seq(from=input$lig_range_CT[1],to=input$lig_range_CT[2],by=1)
    seq.lig <- seq(from=input$lig_range_CT[1],to=input$lig_range_CT[2],by=input$plot_interval_p2_CT)
    lig.max <-input$lig_range_CT[2]
    seq.lig <- unique(append(seq.lig, lig.max))
    
    # seq.rec <- seq(from=input$rec_range_CT[1],to=input$rec_range_CT[2],by=0.2)
    seq.rec <- seq(from=input$rec_range_CT[1],to=input$rec_range_CT[2],by=input$plot_interval_p1_CT)
    rec.max <- input$rec_range_CT[2]
    seq.rec <- unique(append(seq.rec, rec.max))
    
    # seq.kd <- seq(from=input$kd_range_CT[1],to=input$kd_range_CT[2],by=1)
    seq.kd <- seq(from=input$kd_range_CT[1],to=input$kd_range_CT[2],by=input$plot_interval_kd_CT)
    kd.max <- input$kd_range_CT[2]
    seq.kd <- unique(append(seq.kd, kd.max))
    
    # seq.coat <- seq(from=input$coat_range_CT[1],to=input$coat_range_CT[2],by=0.2)
    seq.coat <- seq(from=input$coat_range_CT[1],to=input$coat_range_CT[2],by=input$plot_interval_comp)
    coat.max <- input$coat_range_CT[2]
    seq.coat <- unique(append(seq.coat, coat.max))
    
    seq.ki <- seq(from=input$pKdcoat_CT[1],to=input$pKdcoat_CT[2],by=input$plot_interval_ki)
    ki.max <- input$pKdcoat_CT[2]
    seq.ki <- unique(append(seq.ki, ki.max))
    
    min.coat <- 10^min(seq.coat)*fixed_coat
    max.coat <- 10^max(seq.coat)*fixed_coat
    min.lig <- 10^min(seq.lig)*fixed_ligand
    max.lig <- 10^max(seq.lig)*fixed_ligand
    min.rec <- 10^min(seq.rec)*fixed_receptor
    max.rec <- 10^max(seq.rec)*fixed_receptor
    rec.dots <- 10^seq.rec*fixed_receptor
    leggy <- c()
    my.df <- NULL
    mylist <- NULL
    
    #if competitor or kd
    for (coat.cnt in seq.coat) {
      this.coat <- 10^coat.cnt*fixed_coat
      ## now each line
      cx.dots <- c()
      for (rec.cnt in seq.rec) {
        this.rec <- 10^rec.cnt*fixed_receptor
        gfun <- function(x) {c(this.rec - x[1] - x[2]*x[1]/kdcoating - x[1]*x[3]/fixed_kd, this.coat-x[2]-x[2]*x[1]/kdcoating, fixed_ligand-x[3]-x[1]*x[3]/fixed_kd)}
        fred <- multiroot( gfun, start=c(this.rec/2,this.coat/2,fixed_ligand/2), positive=TRUE )
        c_free_receptor <- fred$root[1]
        c_free_coat <- fred$root[2]
        c_free_ligand <- fred$root[3]
        c_complex <- fixed_ligand - c_free_ligand
        cx.dots <- c(cx.dots, c_complex)
        my.df <- (cbind(rec.dots, this.coat, cx.dots))
      }
      mylist <- data.frame(rbind(mylist, my.df))
      
    }
    if (input$p1concunit_CT == "metric units"){
      print("yes it does")
      rec.dots <- 10^seq.rec*input$c_total_receptor_CT_start
      this.coat <- 10^seq.coat*input$c_total_coating_CT_start
      cx.dots <- mylist[,3]
      
      temp <- NULL
      for (i in 1:length(this.coat)){
        temp[[i]] <- cbind(rec.dots, this.coat[i])
      }
      temp <- do.call(rbind.data.frame, temp)
      colnames(temp)[2] <- "this.coat"
      
      mylist <- cbind.data.frame(temp, cx.dots)
      str(mylist)
      print(mylist)
      
    }
    
    # for (i in 1:nrow(mylist)){
    #   
    #   
    #   mylist$rec.dots[i] <- round(convert_units_back_p1_CT (mylist$rec.dots[i]),1)
    #   mylist$this.coat[i] <- round(convert_units_back_coat (mylist$this.coat[i]),2)
    # }
    
    return(mylist)
  })
  
  
  #complex by p1 over kd
  cxbyrecrangekddataone <- reactive({
    volume <- 1
    fixed_ligand <- input$c_total_ligand_CT
    fixed_receptor <- input$c_total_receptor_CT
    fixed_coat <- input$c_total_coating_CT
    # print("fixing this:")
    # print (fixed_ligand)
    # print(fixed_receptor)
    
    
    if (input$A_selected_first_var == "KD"){
      fixed_kd <- input$A_first_var_value
    }else {
      fixed_kd <- input$Kd_CT
    }
    
    if (input$A_selected_first_var == input$ligand_name_CT){
      fixed_ligand <- convert_units_p2 (input$A_first_var_value)
      # print (fixed_ligand)
      # print (paste("testing", fixed_ligand))
      
    } 
    # else if(input$p2concunit_CT == "molar units"){
    #   
    #   fixed_ligand <- input$c_total_ligand_CT
    #   # print (fixed_coat)
    #   
    # } else if (input$p2concunit_CT == "metric units"){
    #   
    #   fixed_ligand <- input$c_total_ligand_CT_start
    #   
    #   # print (fixed_coat)
    # }
    
    if (input$A_selected_first_var == input$coating_name_CT){
      fixed_coat <- convert_units_coat(input$A_first_var_value)
      
      print (paste ("plot 1:", fixed_coat))
      
    } 
    # else if(input$coat_concunit_CT == "molar units"){
    #   
    #   fixed_coat <- as.numeric (input$c_total_coating_CT)
    #   # print (fixed_coat)
    #   
    # } else if (input$coat_concunit_CT == "metric units"){
    #   
    #   fixed_coat <- input$c_total_coating_CT_start
    #   
    #   # print (fixed_coat)
    # }
    
    if (input$A_selected_first_var == "KI"){
      kdcoating <- input$A_first_var_value
    }else {
      kdcoating <- input$ki_CT
    }
    
    
    # if (fixed_coat==0) {fixed_coat <- 1000}
    if (fixed_coat<=5) {fixed_coat <- 1000}
    if (fixed_ligand<=5) {fixed_ligand <- 1000}
    # seq.lig <- seq(from=input$lig_range_CT[1],to=input$lig_range_CT[2],by=1)
    seq.lig <- seq(from=input$lig_range_CT[1],to=input$lig_range_CT[2],by=input$plot_interval_p2_CT)
    lig.max <-input$lig_range_CT[2]
    seq.lig <- unique(append(seq.lig, lig.max))
    
    # seq.rec <- seq(from=input$rec_range_CT[1],to=input$rec_range_CT[2],by=0.2)
    seq.rec <- seq(from=input$rec_range_CT[1],to=input$rec_range_CT[2],by=input$plot_interval_p1_CT)
    rec.max <- input$rec_range_CT[2]
    seq.rec <- unique(append(seq.rec, rec.max))
    
    # seq.kd <- seq(from=input$kd_range_CT[1],to=input$kd_range_CT[2],by=1)
    seq.kd <- seq(from=input$kd_range_CT[1],to=input$kd_range_CT[2],by=input$plot_interval_kd_CT)
    kd.max <- input$kd_range_CT[2]
    seq.kd <- unique(append(seq.kd, kd.max))
    
    # seq.coat <- seq(from=input$coat_range_CT[1],to=input$coat_range_CT[2],by=0.2)
    seq.coat <- seq(from=input$coat_range_CT[1],to=input$coat_range_CT[2],by=input$plot_interval_comp)
    coat.max <- input$coat_range_CT[2]
    seq.coat <- unique(append(seq.coat, coat.max))
    
    seq.ki <- seq(from=input$pKdcoat_CT[1],to=input$pKdcoat_CT[2],by=input$plot_interval_ki)
    ki.max <- input$pKdcoat_CT[2]
    seq.ki <- unique(append(seq.ki, ki.max))
    
    min.coat <- 10^min(seq.coat)*fixed_coat
    max.coat <- 10^max(seq.coat)*fixed_coat
    min.lig <- 10^min(seq.lig)*fixed_ligand
    max.lig <- 10^max(seq.lig)*fixed_ligand
    min.rec <- 10^min(seq.rec)*fixed_receptor
    max.rec <- 10^max(seq.rec)*fixed_receptor
    rec.dots <- 10^seq.rec*fixed_receptor
    leggy <- c()
    my.df <- NULL
    mylist <- NULL
    
    for (kd.cnt in seq.kd) {
      this.kd <- 10^kd.cnt*fixed_kd
      ## now each line
      cx.dots <- c()
      for (rec.cnt in seq.rec) {
        this.rec <- 10^rec.cnt*fixed_receptor
        gfun <- function(x) {c(this.rec - x[1] - x[2]*x[1]/kdcoating - x[1]*x[3]/this.kd, fixed_coat-x[2]-x[2]*x[1]/kdcoating, fixed_ligand-x[3]-x[1]*x[3]/this.kd)}
        fred <- multiroot( gfun, start=c(this.rec/2,fixed_coat/2,fixed_ligand/2), positive=TRUE  )
        c_free_receptor <- fred$root[1]
        c_free_coat <- fred$root[2]
        c_free_ligand <- fred$root[3]
        c_complex <- fixed_ligand - c_free_ligand
        cx.dots <- c(cx.dots, c_complex)
        my.df <- (cbind(rec.dots, this.kd, cx.dots))
      }
      
      mylist <- data.frame(rbind(mylist, my.df))
      
    }
    
    if (input$p1concunit_CT == "metric units"){
      print("yes it does")
      rec.dots <- 10^seq.rec*input$c_total_receptor_CT_start
      this.kd <- 10^seq.kd*input$Kd_CT
      cx.dots <- mylist[,3]
      
      temp <- NULL
      for (i in 1:length(this.kd)){
        temp[[i]] <- cbind(rec.dots, this.kd[i])
      }
      temp <- do.call(rbind.data.frame, temp)
      colnames(temp)[2] <- "this.kd"
      
      mylist <- cbind.data.frame(temp, cx.dots)
      str(mylist)
      print(mylist)
      
    }
    
    # for (i in 1:nrow(mylist)){
    #   
    #   mylist$rec.dots[i] <- round(convert_units_back_p1_CT (mylist$rec.dots[i]),1)
    #   # mylist$this.coat[i] <- round(convert_units_back_coat (mylist$this.coat[i]),2)
    # }
    # 
    
    return(mylist)
  })
  
  #plot 2
  cxbyrecrangekddatatwo <- reactive({
    
    fixed_ligand <- input$c_total_ligand_CT
    fixed_receptor <- input$c_total_receptor_CT
    fixed_coat <- input$c_total_coating_CT
    
    volume <- 1
    
    if (input$A_selected_first_var == "KD"){
      fixed_kd <- input$A_first_var_value2
    }else {
      fixed_kd <- input$Kd_CT
    }
    
    if (input$A_selected_first_var == input$ligand_name_CT){
      # fixed_ligand <- input$A_first_var_value2
      fixed_ligand <- convert_units_p2 (input$A_first_var_value2)
      
    } 
    # else if(input$p2concunit_CT == "molar units"){
    #   
    #   fixed_ligand <- input$c_total_ligand_CT
    #   # print (fixed_coat)
    #   
    # } else if (input$p2concunit_CT == "metric units"){
    #   
    #   fixed_ligand <- input$c_total_ligand_CT_start
    #   
    #   # print (fixed_coat)
    # }
    
    if (input$A_selected_first_var == input$coating_name_CT){
      fixed_coat <- convert_units_coat(input$A_first_var_value2)
      
      print (paste("plot 2:", fixed_coat))
    } 
    # else if(input$coat_concunit_CT == "molar units"){
    #   
    #   fixed_coat <- as.numeric (input$c_total_coating_CT)
    #   # print (fixed_coat)
    #   
    # } else if (input$coat_concunit_CT == "metric units"){
    #   
    #   fixed_coat <- input$c_total_coating_CT_start
    #   
    #   # print (fixed_coat)
    # }
    
    
    if (input$A_selected_first_var == "KI"){
      kdcoating <- input$A_first_var_value2
    }else {
      kdcoating <- input$ki_CT
    }
    
    
    
    # fixed_kd <- input$Kd_CT
    # kdcoating <- 10^(12-input$pKdcoat_CT[1])
    # fixed_receptor <- input$c_total_receptor_CT
    # fixed_ligand <- input$c_total_ligand_CT
    # fixed_coat <- input$c_total_coating_CT
    
    #   if (fixed_coat==0) {fixed_coat <- 1000}
    #   if (fixed_coat==0) {fixed_coat <- 1000}
    if (fixed_coat<=5) {fixed_coat <- 1000}
    if (fixed_ligand<=5) {fixed_ligand <- 1000}
    
    # seq.lig <- seq(from=input$lig_range_CT[1],to=input$lig_range_CT[2],by=1)
    seq.lig <- seq(from=input$lig_range_CT[1],to=input$lig_range_CT[2],by=input$plot_interval_p2_CT)
    lig.max <-input$lig_range_CT[2]
    seq.lig <- unique(append(seq.lig, lig.max))
    
    # seq.rec <- seq(from=input$rec_range_CT[1],to=input$rec_range_CT[2],by=0.2)
    seq.rec <- seq(from=input$rec_range_CT[1],to=input$rec_range_CT[2],by=input$plot_interval_p1_CT)
    rec.max <- input$rec_range_CT[2]
    seq.rec <- unique(append(seq.rec, rec.max))
    
    # seq.kd <- seq(from=input$kd_range_CT[1],to=input$kd_range_CT[2],by=1)
    seq.kd <- seq(from=input$kd_range_CT[1],to=input$kd_range_CT[2],by=input$plot_interval_kd_CT)
    kd.max <- input$kd_range_CT[2]
    seq.kd <- unique(append(seq.kd, kd.max))
    
    # seq.coat <- seq(from=input$coat_range_CT[1],to=input$coat_range_CT[2],by=0.2)
    seq.coat <- seq(from=input$coat_range_CT[1],to=input$coat_range_CT[2],by=input$plot_interval_comp)
    coat.max <- input$coat_range_CT[2]
    seq.coat <- unique(append(seq.coat, coat.max))
    
    seq.ki <- seq(from=input$pKdcoat_CT[1],to=input$pKdcoat_CT[2],by=input$plot_interval_ki)
    ki.max <- input$pKdcoat_CT[2]
    seq.ki <- unique(append(seq.ki, ki.max))
    
    min.coat <- 10^min(seq.coat)*fixed_coat
    max.coat <- 10^max(seq.coat)*fixed_coat
    min.lig <- 10^min(seq.lig)*fixed_ligand
    max.lig <- 10^max(seq.lig)*fixed_ligand
    min.rec <- 10^min(seq.rec)*fixed_receptor
    max.rec <- 10^max(seq.rec)*fixed_receptor
    rec.dots <- 10^seq.rec*fixed_receptor
    leggy <- c()
    my.df <- NULL
    mylist <- NULL
    
    for (kd.cnt in seq.kd) {
      this.kd <- 10^kd.cnt*fixed_kd
      ## now each line
      cx.dots <- c()
      for (rec.cnt in seq.rec) {
        this.rec <- 10^rec.cnt*fixed_receptor
        gfun <- function(x) {c(this.rec - x[1] - x[2]*x[1]/kdcoating - x[1]*x[3]/this.kd, fixed_coat-x[2]-x[2]*x[1]/kdcoating, fixed_ligand-x[3]-x[1]*x[3]/this.kd)}
        fred <- multiroot( gfun, start=c(this.rec/2,fixed_coat/2,fixed_ligand/2), positive=TRUE  )
        c_free_receptor <- fred$root[1]
        c_free_coat <- fred$root[2]
        c_free_ligand <- fred$root[3]
        c_complex <- fixed_ligand - c_free_ligand
        cx.dots <- c(cx.dots, c_complex)
        my.df <- (cbind(rec.dots, this.kd, cx.dots))
      }
      mylist <- data.frame(rbind(mylist, my.df))
    }
    
    if (input$p1concunit_CT == "metric units"){
      print("yes it does")
      rec.dots <- 10^seq.rec*input$c_total_receptor_CT_start
      this.kd <- 10^seq.kd*input$Kd_CT
      cx.dots <- mylist[,3]
      
      temp <- NULL
      for (i in 1:length(this.kd)){
        temp[[i]] <- cbind(rec.dots, this.kd[i])
      }
      temp <- do.call(rbind.data.frame, temp)
      colnames(temp)[2] <- "this.kd"
      
      mylist <- cbind.data.frame(temp, cx.dots)
      str(mylist)
      print(mylist)
      
    }
    # for (i in 1:nrow(mylist)){
    #   
    #   mylist$rec.dots[i] <- round(convert_units_back_p1_CT (mylist$rec.dots[i]),1)
    #   # mylist$this.coat[i] <- round(convert_units_back_coat (mylist$this.coat[i]),2)
    # }
    
    return(mylist)
  })
  
  #complex by p1 over ki
  #plot 1
  cxbyrecrangekidataone <- reactive({
    
    volume <- 1
    
    fixed_ligand <- input$c_total_ligand_CT
    fixed_receptor <- input$c_total_receptor_CT
    fixed_coat <- input$c_total_coating_CT
    # print("fixing this:")
    
    
    if (input$A_selected_first_var == "KD"){
      fixed_kd <- input$A_first_var_value
    }else {
      fixed_kd <- input$Kd_CT
    }
    
    if (input$A_selected_first_var == input$ligand_name_CT){
      fixed_ligand <- convert_units_p2 (input$A_first_var_value)
      # print (fixed_ligand)
      # print (paste("testing", fixed_ligand))
      
    } 
    # else if(input$p2concunit_CT == "molar units"){
    #   
    #   fixed_ligand <- input$c_total_ligand_CT
    #   # print (fixed_coat)
    #   
    # } else if (input$p2concunit_CT == "metric units"){
    #   
    #   fixed_ligand <- input$c_total_ligand_CT_start
    #   
    #   # print (fixed_coat)
    # }
    
    if (input$A_selected_first_var == input$coating_name_CT){
      fixed_coat <- convert_units_coat(input$A_first_var_value)
      
      print (paste ("plot 1:", fixed_coat))
      
    } 
    # else if(input$coat_concunit_CT == "molar units"){
    #   
    #   fixed_coat <- as.numeric (input$c_total_coating_CT)
    #   # print (fixed_coat)
    #   
    # } else if (input$coat_concunit_CT == "metric units"){
    #   
    #   fixed_coat <- input$c_total_coating_CT_start
    #   
    #   # print (fixed_coat)
    # }
    
    if (input$A_selected_first_var == "KI"){
      kdcoating <- input$A_first_var_value
    }else {
      kdcoating <- input$ki_CT
    }
    
    
    # if (fixed_coat==0) {fixed_coat <- 1000}
    if (fixed_coat<=5) {fixed_coat <- 1000}
    if (fixed_ligand<=5) {fixed_ligand <- 1000}
    # seq.lig <- seq(from=input$lig_range_CT[1],to=input$lig_range_CT[2],by=1)
    seq.lig <- seq(from=input$lig_range_CT[1],to=input$lig_range_CT[2],by=input$plot_interval_p2_CT)
    lig.max <-input$lig_range_CT[2]
    seq.lig <- unique(append(seq.lig, lig.max))
    
    # seq.rec <- seq(from=input$rec_range_CT[1],to=input$rec_range_CT[2],by=0.2)
    seq.rec <- seq(from=input$rec_range_CT[1],to=input$rec_range_CT[2],by=input$plot_interval_p1_CT)
    rec.max <- input$rec_range_CT[2]
    seq.rec <- unique(append(seq.rec, rec.max))
    
    # seq.kd <- seq(from=input$kd_range_CT[1],to=input$kd_range_CT[2],by=1)
    seq.kd <- seq(from=input$kd_range_CT[1],to=input$kd_range_CT[2],by=input$plot_interval_kd_CT)
    kd.max <- input$kd_range_CT[2]
    seq.kd <- unique(append(seq.kd, kd.max))
    
    # seq.coat <- seq(from=input$coat_range_CT[1],to=input$coat_range_CT[2],by=0.2)
    seq.coat <- seq(from=input$coat_range_CT[1],to=input$coat_range_CT[2],by=input$plot_interval_comp)
    coat.max <- input$coat_range_CT[2]
    seq.coat <- unique(append(seq.coat, coat.max))
    
    seq.ki <- seq(from=input$pKdcoat_CT[1],to=input$pKdcoat_CT[2],by=input$plot_interval_ki)
    ki.max <- input$pKdcoat_CT[2]
    seq.ki <- unique(append(seq.ki, ki.max))
    
    min.coat <- 10^min(seq.coat)*fixed_coat
    max.coat <- 10^max(seq.coat)*fixed_coat
    min.lig <- 10^min(seq.lig)*fixed_ligand
    max.lig <- 10^max(seq.lig)*fixed_ligand
    min.rec <- 10^min(seq.rec)*fixed_receptor
    max.rec <- 10^max(seq.rec)*fixed_receptor
    rec.dots <- 10^seq.rec*fixed_receptor
    leggy <- c()
    my.df <- NULL
    mylist <- NULL
    
    for (ki.cnt in seq.ki) {
      this.ki <- 10^ki.cnt*kdcoating
      ## now each line
      cx.dots <- c()
      for (rec.cnt in seq.rec) {
        this.rec <- 10^rec.cnt*fixed_receptor
        gfun <- function(x) {c(this.rec - x[1] - x[2]*x[1]/this.ki - x[1]*x[3]/fixed_kd, fixed_coat-x[2]-x[2]*x[1]/this.ki, fixed_ligand-x[3]-x[1]*x[3]/fixed_kd)}
        fred <- multiroot( gfun, start=c(this.rec/2,fixed_coat/2,fixed_ligand/2), positive=TRUE  )
        c_free_receptor <- fred$root[1]
        c_free_coat <- fred$root[2]
        c_free_ligand <- fred$root[3]
        c_complex <- fixed_ligand - c_free_ligand
        cx.dots <- c(cx.dots, c_complex)
        my.df <- (cbind(rec.dots, this.ki, cx.dots))
      }
      
      mylist <- data.frame(rbind(mylist, my.df))
      
    }
    
    if (input$p1concunit_CT == "metric units"){
      print("yes it does")
      rec.dots <- 10^seq.rec*input$c_total_receptor_CT_start
      this.ki <- 10^seq.ki*input$ki_CT
      cx.dots <- mylist[,3]
      
      temp <- NULL
      for (i in 1:length(this.ki)){
        temp[[i]] <- cbind(rec.dots, this.ki[i])
      }
      temp <- do.call(rbind.data.frame, temp)
      colnames(temp)[2] <- "this.ki"
      
      mylist <- cbind.data.frame(temp, cx.dots)
      str(mylist)
      print(mylist)
      
    }
    # for (i in 1:nrow(mylist)){
    #   mylist$rec.dots[i] <- round(convert_units_back_p1_CT (mylist$rec.dots[i]),1)
    #   # mylist$this.coat[i] <- round(convert_units_back_coat (mylist$this.coat[i]),2)
    # }
    
    return(mylist)
  })
  
  
  #plot 2
  cxbyrecrangekidatatwo <- reactive({
    
    fixed_ligand <- input$c_total_ligand_CT
    fixed_receptor <- input$c_total_receptor_CT
    fixed_coat <- input$c_total_coating_CT
    
    volume <- 1
    
    if (input$A_selected_first_var == "KD"){
      fixed_kd <- input$A_first_var_value2
    }else {
      fixed_kd <- input$Kd_CT
    }
    
    if (input$A_selected_first_var == input$ligand_name_CT){
      # fixed_ligand <- input$A_first_var_value2
      fixed_ligand <- convert_units_p2 (input$A_first_var_value2)
      
    } 
    # else if(input$p2concunit_CT == "molar units"){
    #   
    #   fixed_ligand <- input$c_total_ligand_CT
    #   # print (fixed_coat)
    #   
    # } else if (input$p2concunit_CT == "metric units"){
    #   
    #   fixed_ligand <- input$c_total_ligand_CT_start
    #   
    #   # print (fixed_coat)
    # }
    
    if (input$A_selected_first_var == input$coating_name_CT){
      fixed_coat <- convert_units_coat(input$A_first_var_value2)
      
      print (paste("plot 2:", fixed_coat))
    } 
    # else if(input$coat_concunit_CT == "molar units"){
    #   
    #   fixed_coat <- as.numeric (input$c_total_coating_CT)
    #   # print (fixed_coat)
    #   
    # } else if (input$coat_concunit_CT == "metric units"){
    #   
    #   fixed_coat <- input$c_total_coating_CT_start
    #   
    #   # print (fixed_coat)
    # }
    
    
    if (input$A_selected_first_var == "KI"){
      kdcoating <- input$A_first_var_value2
    }else {
      kdcoating <- input$ki_CT
    }
    
    
    
    # fixed_kd <- input$Kd_CT
    # kdcoating <- 10^(12-input$pKdcoat_CT[1])
    # fixed_receptor <- input$c_total_receptor_CT
    # fixed_ligand <- input$c_total_ligand_CT
    # fixed_coat <- input$c_total_coating_CT
    
    #   if (fixed_coat==0) {fixed_coat <- 1000}
    #   if (fixed_coat==0) {fixed_coat <- 1000}
    if (fixed_coat<=5) {fixed_coat <- 1000}
    if (fixed_ligand<=5) {fixed_ligand <- 1000}
    
    # seq.lig <- seq(from=input$lig_range_CT[1],to=input$lig_range_CT[2],by=1)
    seq.lig <- seq(from=input$lig_range_CT[1],to=input$lig_range_CT[2],by=input$plot_interval_p2_CT)
    lig.max <-input$lig_range_CT[2]
    seq.lig <- unique(append(seq.lig, lig.max))
    
    # seq.rec <- seq(from=input$rec_range_CT[1],to=input$rec_range_CT[2],by=0.2)
    seq.rec <- seq(from=input$rec_range_CT[1],to=input$rec_range_CT[2],by=input$plot_interval_p1_CT)
    rec.max <- input$rec_range_CT[2]
    seq.rec <- unique(append(seq.rec, rec.max))
    
    # seq.kd <- seq(from=input$kd_range_CT[1],to=input$kd_range_CT[2],by=1)
    seq.kd <- seq(from=input$kd_range_CT[1],to=input$kd_range_CT[2],by=input$plot_interval_kd_CT)
    kd.max <- input$kd_range_CT[2]
    seq.kd <- unique(append(seq.kd, kd.max))
    
    # seq.coat <- seq(from=input$coat_range_CT[1],to=input$coat_range_CT[2],by=0.2)
    seq.coat <- seq(from=input$coat_range_CT[1],to=input$coat_range_CT[2],by=input$plot_interval_comp)
    coat.max <- input$coat_range_CT[2]
    seq.coat <- unique(append(seq.coat, coat.max))
    
    seq.ki <- seq(from=input$pKdcoat_CT[1],to=input$pKdcoat_CT[2],by=input$plot_interval_ki)
    ki.max <- input$pKdcoat_CT[2]
    seq.ki <- unique(append(seq.ki, ki.max))
    
    min.coat <- 10^min(seq.coat)*fixed_coat
    max.coat <- 10^max(seq.coat)*fixed_coat
    min.lig <- 10^min(seq.lig)*fixed_ligand
    max.lig <- 10^max(seq.lig)*fixed_ligand
    min.rec <- 10^min(seq.rec)*fixed_receptor
    max.rec <- 10^max(seq.rec)*fixed_receptor
    rec.dots <- 10^seq.rec*fixed_receptor
    leggy <- c()
    my.df <- NULL
    mylist <- NULL
    
    for (ki.cnt in seq.ki) {
      this.ki <- 10^ki.cnt*kdcoating
      ## now each line
      cx.dots <- c()
      for (rec.cnt in seq.rec) {
        this.rec <- 10^rec.cnt*fixed_receptor
        gfun <- function(x) {c(this.rec - x[1] - x[2]*x[1]/this.ki - x[1]*x[3]/fixed_kd, fixed_coat-x[2]-x[2]*x[1]/this.ki, fixed_ligand-x[3]-x[1]*x[3]/fixed_kd)}
        fred <- multiroot( gfun, start=c(this.rec/2,fixed_coat/2,fixed_ligand/2), positive=TRUE  )
        c_free_receptor <- fred$root[1]
        c_free_coat <- fred$root[2]
        c_free_ligand <- fred$root[3]
        c_complex <- fixed_ligand - c_free_ligand
        cx.dots <- c(cx.dots, c_complex)
        my.df <- (cbind(rec.dots, this.ki, cx.dots))
      }
      
      mylist <- data.frame(rbind(mylist, my.df))
    }
    
    
    
    if (input$p1concunit_CT == "metric units"){
      print("yes it does")
      rec.dots <- 10^seq.rec*input$c_total_receptor_CT_start
      this.ki <- 10^seq.ki*input$ki_CT
      cx.dots <- mylist[,3]
      
      temp <- NULL
      for (i in 1:length(this.ki)){
        temp[[i]] <- cbind(rec.dots, this.ki[i])
      }
      temp <- do.call(rbind.data.frame, temp)
      colnames(temp)[2] <- "this.ki"
      
      mylist <- cbind.data.frame(temp, cx.dots)
      str(mylist)
      print(mylist)
      
    }    
    # for (i in 1:nrow(mylist)){
    #   mylist$rec.dots[i] <- round(convert_units_back_p1_CT (mylist$rec.dots[i]),1)
    #   # mylist$this.coat[i] <- round(convert_units_back_coat (mylist$this.coat[i]),2)
    # }
    
    return(mylist)
  })
  
  
  #First Plot graph 1
  
  observeEvent( c(input$myconfirmation_submit_competition, input$myconfirmation_submit_competition_graph1), {
    if (!is.null (input$myconfirmation_submit_competition) | !is.null(input$myconfirmation_submit_competition_graph1) ){
      
      output$firstPlot_A <- renderPlotly({ 
        isolate({ 
          # output$cxbyligrangerecplot <- renderPlotly({
          print(inputfirstPlot_A())
        })
      })
    }
  })
  
  inputfirstPlot_A <- reactive ({
    
    if (input$A_change_first_var == input$ligand_name_CT){
      # print("kd p2")
      #complex by rec over ligand
      # print("complex by receptor over ligand")
      mylist <- cxbyrecrangeligdataone()
      mylist$this.lig <- round(mylist$this.lig,2)
      mylist_spread <- tidyr::spread(mylist, key= "this.lig", value =  "cx.dots")
      thinned_spread = mylist_spread[seq(1, nrow(mylist_spread), 1), ]
      thinned_spread <- round(thinned_spread, 2)
      thinned_gather <-  tidyr::gather(thinned_spread[ ,2:ncol(thinned_spread)], key= "this.lig", value =  "cx.dots")
      thinned_gather_new <- cbind(thinned_spread[ ,1], thinned_gather)
      names(thinned_gather_new)[1] <- 'rec.dots'
      
      
      q <- plot_ly (data = mylist,
                    x = ~rec.dots) %>%
        add_trace (y = ~cx.dots, mode = 'lines', color = ~factor(this.lig)) %>%
        add_trace (data = thinned_gather_new,
                   y = ~ cx.dots, mode = 'markers', color = ~factor(this.lig),
                   showlegend = F, showticklabels = FALSE) %>%
        layout(title = paste("Concentration of ",input$receptor_name_CT,"-", input$ligand_name_CT, 
                             " Complex ",
                             "\n in function of ", em(input$receptor_name_CT),
                             " for a fixed ", em(input$A_selected_first_var), " of ", em(input$A_first_var_value),
                             " ", get_units(input$A_selected_first_var), sep=""),
               font = list(size = 9),
               # yaxis = list (title =paste("Concentration of ", input$coating_name_CT," (", 
               #                            get_units(input$coating_name_CT), ")", sep="")), 
               yaxis = list (title = paste(input$receptor_name_CT,"-",input$ligand_name_CT," complex ",get_units(input$A_selected_first_var),sep="")), 
               
               xaxis = list(type = "log", autotick = F,titlefont = list(size = 14),tickfont = list(size = 12),
                            title = paste("Concentration of ", input$receptor_name_CT,
                                          " (", get_units(input$receptor_name_CT), ")", sep="") ))
      
    } else if (input$A_change_first_var == input$coating_name_CT){
      
      #complex by recepter over coat
      # print("kd comp")
      mylist <- cxbyrecrangecoatdataone()
      mylist$this.coat <- round(mylist$this.coat,2)
      mylist_spread <- tidyr::spread(mylist, key= "this.coat", value =  "cx.dots")
      thinned_spread = mylist_spread[seq(1, nrow(mylist_spread), 1), ]
      thinned_spread <- round(thinned_spread, 2)
      thinned_gather <-  tidyr::gather(thinned_spread[ ,2:ncol(thinned_spread)], key= "this.coat", value =  "cx.dots")
      thinned_gather_new <- cbind(thinned_spread[ ,1], thinned_gather)
      names(thinned_gather_new)[1] <- 'rec.dots'
      
      
      q <- plot_ly (data = mylist,
                    x = ~rec.dots) %>%
        add_trace (y = ~cx.dots, mode = 'lines', color = ~factor(this.coat)) %>%
        add_trace (data = thinned_gather_new,
                   y = ~ cx.dots, mode = 'markers', color = ~factor(this.coat),
                   showlegend = F, showticklabels = FALSE) %>%
        layout(title = paste("Concentration of ",input$receptor_name_CT,"-", input$ligand_name_CT, 
                             " Complex ",
                             "\n in function of ", em(input$receptor_name_CT),
                             " for a fixed ", em(input$A_selected_first_var), " of ", em(input$A_first_var_value),
                             " ", get_units(input$A_selected_first_var), sep=""),
               font = list(size = 9),
               # yaxis = list (title =paste("Concentration of ", input$coating_name_CT," (", 
               #                            get_units(input$coating_name_CT), ")", sep="")), 
               
               yaxis = list (title = paste(input$receptor_name_CT,"-",input$ligand_name_CT," complex ", get_units(input$A_selected_first_var),sep="")),
               xaxis = list(type = "log", autotick = F,titlefont = list(size = 14),tickfont = list(size = 12),
                            title = paste("Concentration of ", input$receptor_name_CT,
                                          " (", get_units(input$receptor_name_CT), ")", sep="") ))
      
      
    } else if (input$A_change_first_var == "KD"){
      
      #complex by receptor over kd
      # print ("complex by receptor over kd")
      
      mylist <- cxbyrecrangekddataone()
      mylist$this.kd <- round(mylist$this.kd,2)
      mylist_spread <- tidyr::spread(mylist, key= "this.kd", value =  "cx.dots")
      thinned_spread = mylist_spread[seq(1, nrow(mylist_spread), 1), ]
      thinned_spread <- round(thinned_spread, 2)
      thinned_gather <-  tidyr::gather(thinned_spread[ ,2:ncol(thinned_spread)], key= "this.kd", value =  "cx.dots")
      thinned_gather_new <- cbind(thinned_spread[ ,1], thinned_gather)
      names(thinned_gather_new)[1] <- 'rec.dots'
      
      
      q <- plot_ly (data = mylist,
                    x = ~rec.dots) %>%
        add_trace (y = ~cx.dots, mode = 'lines', color = ~factor(this.kd)) %>%
        add_trace (data = thinned_gather_new,
                   y = ~ cx.dots, mode = 'markers', color = ~factor(this.kd),
                   showlegend = F, showticklabels = FALSE) %>%
        layout(title = paste("Concentration of ",input$receptor_name_CT,"-", input$ligand_name_CT,
                             " Complex ",
                             "\n in function of ", em(input$receptor_name_CT),
                             " for a fixed ", em(input$A_selected_first_var), " of ", em(input$A_first_var_value),
                             " ", get_units(input$A_selected_first_var), sep=""),
               font = list(size = 9),
               # yaxis = list (title =paste("Concentration of ", input$coating_name_CT," (",
               #                            get_units(input$coating_name_CT), ")", sep="")),
               
               yaxis = list (title = paste(input$receptor_name_CT,"-",input$ligand_name_CT," complex ", get_units(input$A_selected_first_var), sep="")),
               xaxis = list(type = "log", autotick = F,titlefont = list(size = 14),tickfont = list(size = 12),
                            title = paste("Concentration of ", input$receptor_name_CT,
                                          " (", get_units(input$receptor_name_CT), ")", sep="") ))
      
    }else if (input$A_change_first_var == "KI"){
      
      # #complex by receptor over kd
      # # print ("complex by receptor over ki")
      # 
      mylist <-  cxbyrecrangekidataone()
      mylist$this.ki <- round(mylist$this.ki,2)
      mylist_spread <- tidyr::spread(mylist, key= "this.ki", value =  "cx.dots")
      thinned_spread = mylist_spread[seq(1, nrow(mylist_spread), 1), ]
      thinned_spread <- round(thinned_spread, 2)
      thinned_gather <-  tidyr::gather(thinned_spread[ ,2:ncol(thinned_spread)], key= "this.ki", value =  "cx.dots")
      thinned_gather_new <- cbind(thinned_spread[ ,1], thinned_gather)
      names(thinned_gather_new)[1] <- 'rec.dots'
      
      
      q <- plot_ly (data = mylist,
                    x = ~rec.dots) %>%
        add_trace (y = ~cx.dots, mode = 'lines', color = ~factor(this.ki)) %>%
        add_trace (data = thinned_gather_new,
                   y = ~ cx.dots, mode = 'markers', color = ~factor(this.ki),
                   showlegend = F, showticklabels = FALSE) %>%
        layout(title = paste("Concentration of ",input$receptor_name_CT,"-", input$ligand_name_CT,
                             " Complex ",
                             "\n in function of ", em(input$receptor_name_CT),
                             " for a fixed ", em(input$A_selected_first_var), " of ", em(input$A_first_var_value),
                             " ", get_units(input$A_selected_first_var), sep=""),
               font = list(size = 9),
               # yaxis = list (title =paste("Concentration of ", input$coating_name_CT," (",
               #                            get_units(input$coating_name_CT), ")", sep="")),
               
               yaxis = list (title = paste(input$receptor_name_CT,"-",input$ligand_name_CT," complex ",get_units(input$A_selected_first_var), sep="")),
               xaxis = list(type = "log", autotick = F,titlefont = list(size = 14),tickfont = list(size = 12),
                            title = paste("Concentration of ", input$receptor_name_CT,
                                          " (", get_units(input$receptor_name_CT), ")", sep="") ))
      
    }
    
    if(input$A_change_first_var == 'KD' | input$A_change_first_var == 'KI') {
      my.annotation <- paste(input$A_change_first_var, " (",get_units(input$A_change_first_var), ")", sep = "")
    }else{
      my.annotation <- paste("Concentration of <br>", input$A_change_first_var, " (",get_units(input$A_change_first_var), ")", sep = "")
    }
    
    
    if(input$scale_CT == 1){
      
      
      q %>%
        config(displaylogo = FALSE) %>%
        config(collaborate = FALSE) %>%
        config(displayModeBar = F) %>%
        config(
          edits = list(
            # annotationPosition = TRUE,
            # annotationTail = TRUE,
            # annotationText = TRUE
          ))%>%
        config(
          toImageButtonOptions = list(
            format = "png",
            width = 1000,
            height = 1000
          ))%>%
        layout(
          
          legend = list(
            orientation = "h",
            x = 0.225, y = -0.3,
            font = list(size = 12)
          )
        )%>%
        add_annotations( text= my.annotation, 
                         xref="paper", yref="paper",
                         x=0, xanchor="left",
                         y=-0.435, yanchor="bottom",    # Same y as legend below
                         legendtitle=F, showarrow=FALSE,
                         font=list(size=12,face="bold"))
      
      
    } else if (input$scale_CT == 2){
      # q <- q + scale_y_continuous(trans = "log10")
      
      q <- q %>% layout(yaxis = list(type = "log"))
      
      q %>%
        config(displaylogo = FALSE) %>%
        config(collaborate = FALSE) %>%
        config(displayModeBar = F) %>%
        layout(legend = list(
          orientation = "h",
          x = 0.225, y = -0.3,
          font = list(size = 12)
        )
        )%>%
        add_annotations( text= my.annotation, 
                         xref="paper", yref="paper",
                         x=0, xanchor="left",
                         y=-0.435, yanchor="bottom",    # Same y as legend below
                         legendtitle=F, showarrow=FALSE,
                         font=list(size=12,face="bold"))
    }
  })
  
  
  #Second Plot graph 1
  
  observeEvent( c(input$myconfirmation_submit_competition, input$myconfirmation_submit_competition_graph1), {
    if (!is.null (input$myconfirmation_submit_competition) | !is.null(input$myconfirmation_submit_competition_graph1) ){
      
      output$firstPlot_B <- renderPlotly({
        isolate({ 
          # output$cxbyligrangerecplot <- renderPlotly({
          print(inputfirstPlot_B())
        })
      })
    }
  })
  
  inputfirstPlot_B <- reactive({ 
    
    if (input$A_change_first_var == input$ligand_name_CT){
      
      mylist <- cxbyrecrangeligdatatwo()
      mylist$this.lig <- round(mylist$this.lig,2)
      mylist_spread <- tidyr::spread(mylist, key= "this.lig", value =  "cx.dots")
      thinned_spread = mylist_spread[seq(1, nrow(mylist_spread), 1), ]
      thinned_spread <- round(thinned_spread, 2)
      thinned_gather <-  tidyr::gather(thinned_spread[ ,2:ncol(thinned_spread)], key= "this.lig", value =  "cx.dots")
      thinned_gather_new <- cbind(thinned_spread[ ,1], thinned_gather)
      names(thinned_gather_new)[1] <- 'rec.dots'
      
      
      q <- plot_ly (data = mylist,
                    x = ~rec.dots) %>%
        add_trace (y = ~cx.dots, mode = 'lines', color = ~factor(this.lig)) %>%
        add_trace (data = thinned_gather_new,
                   y = ~ cx.dots, mode = 'markers', color = ~factor(this.lig),
                   showlegend = F, showticklabels = FALSE) %>%
        layout(title = paste("Concentration of ",input$receptor_name_CT,"-", input$ligand_name_CT,
                             " Complex ",
                             "\n in function of ", em(input$receptor_name_CT),
                             " for a fixed ", em(input$A_selected_first_var), " of ", em(input$A_first_var_value2),
                             " ", get_units(input$A_selected_first_var), sep=""),
               font = list(size = 9),
               # yaxis = list (title =paste("Concentration of ", input$coating_name_CT," (",
               #                            get_units(input$coating_name_CT), ")", sep="")),
               
               yaxis = list (title = paste(input$receptor_name_CT,"-",input$ligand_name_CT," complex ", get_units(input$A_selected_first_var),sep="")),
               
               xaxis = list(type = "log", autotick = F,titlefont = list(size = 14),tickfont = list(size = 12),
                            title = paste("Concentration of ", input$receptor_name_CT,
                                          " (", get_units(input$receptor_name_CT), ")", sep="") ))
      
    } else if (input$A_change_first_var == input$coating_name_CT){
      
      mylist <- cxbyrecrangecoatdatatwo()
      mylist$this.coat <- round(mylist$this.coat,2)
      mylist_spread <- tidyr::spread(mylist, key= "this.coat", value =  "cx.dots")
      thinned_spread = mylist_spread[seq(1, nrow(mylist_spread), 1), ]
      thinned_spread <- round(thinned_spread, 2)
      thinned_gather <-  tidyr::gather(thinned_spread[ ,2:ncol(thinned_spread)], key= "this.coat", value =  "cx.dots")
      thinned_gather_new <- cbind(thinned_spread[ ,1], thinned_gather)
      names(thinned_gather_new)[1] <- 'rec.dots'
      
      
      q <- plot_ly (data = mylist,
                    x = ~rec.dots) %>%
        add_trace (y = ~cx.dots, mode = 'lines', color = ~factor(this.coat)) %>%
        add_trace (data = thinned_gather_new,
                   y = ~ cx.dots, mode = 'markers', color = ~factor(this.coat),
                   showlegend = F, showticklabels = FALSE) %>%
        layout(title = paste("Concentration of ",input$receptor_name_CT,"-", input$ligand_name_CT,
                             " Complex ",
                             "\n in function of ", em(input$receptor_name_CT),
                             " for a fixed ", em(input$A_selected_first_var), " of ", em(input$A_first_var_value2),
                             " ", get_units(input$A_selected_first_var), sep=""),
               font = list(size = 9),
               # yaxis = list (title =paste("Concentration of ", input$coating_name_CT," (",
               #                            get_units(input$coating_name_CT), ")", sep="")),
               
               yaxis = list (title = paste(input$receptor_name_CT,"-",input$ligand_name_CT," complex ", get_units(input$A_selected_first_var),sep="")),
               
               xaxis = list(type = "log", autotick = F,titlefont = list(size = 14),tickfont = list(size = 12),
                            title = paste("Concentration of ", input$receptor_name_CT,
                                          " (", get_units(input$receptor_name_CT), ")", sep="") ))
      
      
    } else if (input$A_change_first_var == "KD"){
      
      #complex by receptor over kd
      # print ("complex by receptor over kd")
      
      mylist <- cxbyrecrangekddatatwo()
      mylist$this.kd <- round(mylist$this.kd,2)
      mylist_spread <- tidyr::spread(mylist, key= "this.kd", value =  "cx.dots")
      thinned_spread = mylist_spread[seq(1, nrow(mylist_spread), 1), ]
      thinned_spread <- round(thinned_spread, 2)
      thinned_gather <-  tidyr::gather(thinned_spread[ ,2:ncol(thinned_spread)], key= "this.kd", value =  "cx.dots")
      thinned_gather_new <- cbind(thinned_spread[ ,1], thinned_gather)
      names(thinned_gather_new)[1] <- 'rec.dots'
      
      
      q <- plot_ly (data = mylist,
                    x = ~rec.dots) %>%
        add_trace (y = ~cx.dots, mode = 'lines', color = ~factor(this.kd)) %>%
        add_trace (data = thinned_gather_new,
                   y = ~ cx.dots, mode = 'markers', color = ~factor(this.kd),
                   showlegend = F, showticklabels = FALSE) %>%
        layout(title = paste("Concentration of ",input$receptor_name_CT,"-", input$ligand_name_CT,
                             " Complex ",
                             "\n in function of ", em(input$receptor_name_CT),
                             " for a fixed ", em(input$A_selected_first_var), " of ", em(input$A_first_var_value2),
                             " ", get_units(input$A_selected_first_var), sep=""),
               font = list(size = 9),
               # yaxis = list (title =paste("Concentration of ", input$coating_name_CT," (",
               #                            get_units(input$coating_name_CT), ")", sep="")),
               
               yaxis = list (title = paste(input$receptor_name_CT,"-",input$ligand_name_CT," complex ", get_units(input$A_selected_first_var),sep="")),
               
               xaxis = list(type = "log", autotick = F,titlefont = list(size = 14),tickfont = list(size = 12),
                            title = paste("Concentration of ", input$receptor_name_CT,
                                          " (", get_units(input$receptor_name_CT), ")", sep="") ))
      
    }else if (input$A_change_first_var == "KI"){
      
      #complex by receptor over kd
      # print ("complex by receptor over ki")
      
      mylist <-  cxbyrecrangekidatatwo()
      mylist$this.ki <- round(mylist$this.ki,2)
      mylist_spread <- tidyr::spread(mylist, key= "this.ki", value =  "cx.dots")
      thinned_spread = mylist_spread[seq(1, nrow(mylist_spread), 1), ]
      thinned_spread <- round(thinned_spread, 2)
      thinned_gather <-  tidyr::gather(thinned_spread[ ,2:ncol(thinned_spread)], key= "this.ki", value =  "cx.dots")
      thinned_gather_new <- cbind(thinned_spread[ ,1], thinned_gather)
      names(thinned_gather_new)[1] <- 'rec.dots'
      
      
      q <- plot_ly (data = mylist,
                    x = ~rec.dots) %>%
        add_trace (y = ~cx.dots, mode = 'lines', color = ~factor(this.ki)) %>%
        add_trace (data = thinned_gather_new,
                   y = ~ cx.dots, mode = 'markers', color = ~factor(this.ki),
                   showlegend = F, showticklabels = FALSE) %>%
        layout(title = paste("Concentration of ",input$receptor_name_CT,"-", input$ligand_name_CT,
                             " Complex ",
                             "\n in function of ", em(input$receptor_name_CT),
                             " for a fixed ", em(input$A_selected_first_var), " of ", em(input$A_first_var_value2),
                             " ", get_units(input$A_selected_first_var), sep=""),
               font = list(size = 9),
               # yaxis = list (title =paste("Concentration of ", input$coating_name_CT," (",
               #                            get_units(input$coating_name_CT), ")", sep="")),
               
               yaxis = list (title = paste(input$receptor_name_CT,"-",input$ligand_name_CT," complex ", get_units(input$A_selected_first_var),sep="")),
               
               xaxis = list(type = "log", autotick = F,titlefont = list(size = 14),tickfont = list(size = 12),
                            title = paste("Concentration of ", input$receptor_name_CT,
                                          " (", get_units(input$receptor_name_CT), ")", sep="") ))
      
    }
    
    if(input$A_change_first_var == 'KD' | input$A_change_first_var == 'KI' ){
      my.annotation <- paste(input$A_change_first_var, " (", get_units(input$A_change_first_var), ")", sep = "")
    }else{
      my.annotation <- paste("Concentration of <br>", input$A_change_first_var, " (",get_units(input$A_change_first_var), ")", sep = "")
    }
    
    if(input$scale_CT == 1){
      
      
      q %>%
        config(displaylogo = FALSE) %>%
        config(collaborate = FALSE) %>%
        config(displayModeBar = F) %>%
        config(
          edits = list(
            # annotationPosition = TRUE,
            # annotationTail = TRUE,
            # annotationText = TRUE
          ))%>%
        config(
          toImageButtonOptions = list(
            format = "png",
            width = 1000,
            height = 1000
          ))%>%
        layout(
          
          legend = list(
            orientation = "h",
            x = 0.225, y = -0.3,
            font = list(size = 12)
          )
        )%>%
        add_annotations( text= my.annotation, 
                         xref="paper", yref="paper",
                         x=0, xanchor="left",
                         y=-0.435, yanchor="bottom",    # Same y as legend below
                         legendtitle=F, showarrow=FALSE,
                         font=list(size=12,face="bold"))
      
      
    } else if (input$scale_CT == 2){
      # q <- q + scale_y_continuous(trans = "log10")
      
      q <- q %>% layout(yaxis = list(type = "log"))
      
      q %>%
        config(displaylogo = FALSE) %>%
        config(collaborate = FALSE) %>%
        config(displayModeBar = F) %>%
        layout(legend = list(
          orientation = "h",
          x = 0.225, y = -0.3,
          font = list(size = 12)
        )
        )%>%
        add_annotations( text= my.annotation, 
                         xref="paper", yref="paper",
                         x=0, xanchor="left",
                         y=-0.435, yanchor="bottom",    # Same y as legend below
                         legendtitle=F, showarrow=FALSE,
                         font=list(size=12,face="bold"))
    }
  })
  
  
  inputTable1_graph1_CT <- reactive({
    
    if (input$A_change_first_var == input$ligand_name_CT){
      
      # name_partner2 <- HTML('Concentration of ' ,input$A_change_first_var, "\n", " (", get_units(input$A_change_first_var), ")")
      # name_partner1 <- paste('Concentration of ', input$receptor_name_CT, "\n", " (", get_units(input$receptor_name_CT), ")")
      
      mylist <- cxbyrecrangeligdataone()
      mylist$this.lig <- round(mylist$this.lig,2)
      mylist_spread <- tidyr::spread(mylist, key= "this.lig", value =  "cx.dots")
      thinned_spread = mylist_spread[seq(1, nrow(mylist_spread), 1), ]
      names(thinned_spread)[1] <- ""
      thinned_spread <- round(thinned_spread, 2)
      
    } else if( input$A_change_first_var == input$coating_name_CT){
      
      # name_partner2 <- HTML('Concentration of ' ,input$A_change_first_var, "\n", " (", get_units(input$A_change_first_var), ")")
      # name_partner1 <- paste('Concentration of ', input$receptor_name_CT, "\n", " (", get_units(input$receptor_name_CT), ")")
      
      mylist <- cxbyrecrangecoatdataone() 
      mylist$this.coat <- round(mylist$this.coat,2)
      mylist_spread <- tidyr::spread(mylist, key= "this.coat", value =  "cx.dots")
      thinned_spread = mylist_spread[seq(1, nrow(mylist_spread), 1), ]
      names(thinned_spread)[1] <- ""
      thinned_spread <- round(thinned_spread, 2)
      
      
    }else if( input$A_change_first_var == "KD"){
      # name_partner2 <- HTML('Concentration of ' ,input$A_change_first_var, "\n", " (", get_units(input$A_change_first_var), ")")
      # name_partner1 <- paste('Concentration of ', input$receptor_name_CT, "\n", " (", get_units(input$receptor_name_CT), ")")
      
      mylist <- cxbyrecrangekddataone() 
      mylist$this.kd <- round(mylist$this.kd,2)
      mylist_spread <- tidyr::spread(mylist, key= "this.kd", value =  "cx.dots")
      thinned_spread = mylist_spread[seq(1, nrow(mylist_spread), 1), ]
      names(thinned_spread)[1] <- ""
      thinned_spread <- round(thinned_spread, 2)
      
    }else if( input$A_change_first_var == "KI"){
      # name_partner2 <- HTML('Concentration of ' ,input$A_change_first_var, "\n", " (", get_units(input$A_change_first_var), ")")
      # name_partner1 <- paste('Concentration of ', input$receptor_name_CT, "\n", " (", get_units(input$receptor_name_CT), ")")
      
      mylist <- cxbyrecrangekidataone() 
      mylist$this.ki <- round(mylist$this.ki,2)
      mylist_spread <- tidyr::spread(mylist, key= "this.ki", value =  "cx.dots")
      thinned_spread = mylist_spread[seq(1, nrow(mylist_spread), 1), ]
      names(thinned_spread)[1] <- ""
      thinned_spread <- round(thinned_spread, 2)
    }
    
  })
  
  #First Table graph 1
  observeEvent( c(input$myconfirmation_submit_competition, input$myconfirmation_submit_competition_graph1), {
    if (!is.null (input$myconfirmation_submit_competition) | !is.null(input$myconfirmation_submit_competition_graph1) ){
      
      output$firstTable_A <- renderDataTable({
        
        isolate({ 
          
          if (input$A_change_first_var == input$ligand_name_CT){
            
            name_partner2 <- HTML('Concentration of ' ,input$A_change_first_var, "\n", " (", get_units(input$A_change_first_var), ")")
            name_partner1 <- paste('Concentration of ', input$receptor_name_CT, "\n", " (", get_units(input$receptor_name_CT), ")")
            
            
          } else if( input$A_change_first_var == input$coating_name_CT){
            
            name_partner2 <- HTML('Concentration of ' ,input$A_change_first_var, "\n", " (", get_units(input$A_change_first_var), ")")
            name_partner1 <- paste('Concentration of ', input$receptor_name_CT, "\n", " (", get_units(input$receptor_name_CT), ")")
            
            
          }else if( input$A_change_first_var == "KD"){
            name_partner2 <- HTML(input$A_change_first_var, "\n", " (", get_units(input$A_change_first_var), ")")
            name_partner1 <- paste('Concentration of ', input$receptor_name_CT, "\n", " (", get_units(input$receptor_name_CT), ")")
            
            
          }else if( input$A_change_first_var == "KI"){
            name_partner2 <- HTML(input$A_change_first_var, "\n", " (", get_units(input$A_change_first_var), ")")
            name_partner1 <- paste('Concentration of ', input$receptor_name_CT, "\n", " (", get_units(input$receptor_name_CT), ")")
            
          }
          
          thinned_spread <- inputTable1_graph1_CT()
          sketch <- createContainer(name_partner1, name_partner2,  thinned_spread)
          
          datatable(
            thinned_spread,
            extensions = 'Buttons',
            class = "cell-border stripe hover",
            caption = paste(input$receptor_name_CT,"-",input$ligand_name_CT," complex ", get_units(input$A_selected_first_var),sep=""),
            container = sketch,
            rownames = FALSE,
            
            options = list(
              pageLength = 7,
              dom = 'tprB',
              scrollX = TRUE,
              buttons = list(
                c('copy', 'excel'))
            )
          )%>%
            formatStyle(thinned_spread [ ,1], fontWeight = "bold")
          
        })
      })
    } 
    
  })
  
  
  #Second Table graph 1
  
  inputTable2_graph1_CT  <- reactive ({ 
    
    if (input$A_change_first_var == input$ligand_name_CT){
      
      # name_partner2 <- HTML('Concentration of ' ,input$A_change_first_var, "\n", " (", get_units(input$A_change_first_var), ")")
      # name_partner1 <- paste('Concentration of ', input$receptor_name_CT, "\n", " (", get_units(input$receptor_name_CT), ")")
      # 
      mylist <- cxbyrecrangeligdatatwo()
      mylist$this.lig <- round(mylist$this.lig,2)
      mylist_spread <- tidyr::spread(mylist, key= "this.lig", value =  "cx.dots")
      thinned_spread = mylist_spread[seq(1, nrow(mylist_spread), 1), ]
      names(thinned_spread)[1] <- ""
      thinned_spread <- round(thinned_spread, 2)
      
    } else if( input$A_change_first_var == input$coating_name_CT){
      # 
      # name_partner2 <- HTML('Concentration of ' ,input$A_change_first_var, "\n", " (", get_units(input$A_change_first_var), ")")
      # name_partner1 <- paste('Concentration of ', input$receptor_name_CT, "\n", " (", get_units(input$receptor_name_CT), ")")
      
      mylist <- cxbyrecrangecoatdatatwo()
      mylist$this.coat <- round(mylist$this.coat,2)
      mylist_spread <- tidyr::spread(mylist, key= "this.coat", value =  "cx.dots")
      thinned_spread = mylist_spread[seq(1, nrow(mylist_spread), 1), ]
      names(thinned_spread)[1] <- ""
      thinned_spread <- round(thinned_spread, 2)
      
      
    }else if( input$A_change_first_var == "KD"){
      # name_partner2 <- HTML('Concentration of ' ,input$A_change_first_var, "\n", " (", get_units(input$A_change_first_var), ")")
      # name_partner1 <- paste('Concentration of ', input$receptor_name_CT, "\n", " (", get_units(input$receptor_name_CT), ")")
      
      mylist <- cxbyrecrangekddatatwo() 
      mylist$this.kd <- round(mylist$this.kd,2)
      mylist_spread <- tidyr::spread(mylist, key= "this.kd", value =  "cx.dots")
      thinned_spread = mylist_spread[seq(1, nrow(mylist_spread), 1), ]
      names(thinned_spread)[1] <- ""
      thinned_spread <- round(thinned_spread, 2)
      
    }else if( input$A_change_first_var == "KI"){
      # name_partner2 <- HTML('Concentration of ' ,input$A_change_first_var, "\n", " (", get_units(input$A_change_first_var), ")")
      # name_partner1 <- paste('Concentration of ', input$receptor_name_CT, "\n", " (", get_units(input$receptor_name_CT), ")")
      
      mylist <- cxbyrecrangekidatatwo() 
      mylist$this.ki <- round(mylist$this.ki,2)
      mylist_spread <- tidyr::spread(mylist, key= "this.ki", value =  "cx.dots")
      thinned_spread = mylist_spread[seq(1, nrow(mylist_spread), 1), ]
      names(thinned_spread)[1] <- ""
      thinned_spread <- round(thinned_spread, 2)
    }
    
  })
  
  observeEvent( c(input$myconfirmation_submit_competition, input$myconfirmation_submit_competition_graph1), {
    if (!is.null (input$myconfirmation_submit_competition) | !is.null(input$myconfirmation_submit_competition_graph1) ){
      
      output$firstTable_B <- renderDataTable({
        isolate({ 
          if (input$A_change_first_var == input$ligand_name_CT){
            
            name_partner2 <- HTML('Concentration of ' ,input$A_change_first_var, "\n", " (", get_units(input$A_change_first_var), ")")
            name_partner1 <- paste('Concentration of ', input$receptor_name_CT, "\n", " (", get_units(input$receptor_name_CT), ")")
            
            
          } else if( input$A_change_first_var == input$coating_name_CT){
            
            name_partner2 <- HTML('Concentration of ' ,input$A_change_first_var, "\n", " (", get_units(input$A_change_first_var), ")")
            name_partner1 <- paste('Concentration of ', input$receptor_name_CT, "\n", " (", get_units(input$receptor_name_CT), ")")
            
            
          }else if( input$A_change_first_var == "KD"){
            name_partner2 <- HTML(input$A_change_first_var, "\n", " (", get_units(input$A_change_first_var), ")")
            name_partner1 <- paste('Concentration of ', input$receptor_name_CT, "\n", " (", get_units(input$receptor_name_CT), ")")
            
            
          }else if( input$A_change_first_var == "KI"){
            name_partner2 <- HTML(input$A_change_first_var, "\n", " (", get_units(input$A_change_first_var), ")")
            name_partner1 <- paste('Concentration of ', input$receptor_name_CT, "\n", " (", get_units(input$receptor_name_CT), ")")
            
          }
          
          
          thinned_spread <- inputTable2_graph1_CT()
          
          sketch <- createContainer(name_partner1, name_partner2,  thinned_spread)
          
          datatable(
            thinned_spread,
            extensions = 'Buttons',
            class = "cell-border stripe hover",
            caption = paste(input$receptor_name_CT,"-",input$ligand_name_CT," complex ", get_units(input$A_selected_first_var),sep=""),
            container = sketch,
            rownames = FALSE,
            
            options = list(
              pageLength = 7,
              dom = 'tprB',
              scrollX = TRUE,
              buttons = list(
                c('copy', 'excel'))
            )
          )%>%
            formatStyle(thinned_spread [ ,1], fontWeight = "bold")
          
        })
      })
    }
  })
  
  #Graph 2
  
  #over partner 2
  roccbyrecrangeligdataone <- reactive({
    
    volume <- 1
    
    fixed_ligand <- input$c_total_ligand_CT
    fixed_receptor <- input$c_total_receptor_CT
    fixed_coat <- input$c_total_coating_CT
    
    if (input$A_selected_first_var_graph2 == "KD"){
      fixed_kd <- input$A_first_var_value_graph2
    }else {
      fixed_kd <- input$Kd_CT
    }
    
    if (input$A_selected_first_var_graph2 == input$ligand_name_CT){
      fixed_ligand <- convert_units_p2 (input$A_first_var_value_graph2)
    }
    
    if (input$A_selected_first_var_graph2 == input$coating_name_CT){
      fixed_coat <- convert_units_coat(input$A_first_var_value_graph2)
    }
    print (fixed_coat)
    
    if (input$A_selected_first_var == "KI"){
      kdcoating <- input$A_first_var_value_graph2
    }else {
      kdcoating <- input$ki_CT
    }
    
    # if (fixed_coat==0) {fixed_coat <- 1000}
    if (fixed_coat<=5) {fixed_coat <- 1000}
    if (fixed_ligand<=5) {fixed_ligand <- 1000}
    # seq.lig <- seq(from=input$lig_range_CT[1],to=input$lig_range_CT[2],by=1)
    seq.lig <- seq(from=input$lig_range_CT[1],to=input$lig_range_CT[2],by=input$plot_interval_p2_CT)
    lig.max <-input$lig_range_CT[2]
    seq.lig <- unique(append(seq.lig, lig.max))
    
    # seq.rec <- seq(from=input$rec_range_CT[1],to=input$rec_range_CT[2],by=0.2)
    seq.rec <- seq(from=input$rec_range_CT[1],to=input$rec_range_CT[2],by=input$plot_interval_p1_CT)
    rec.max <- input$rec_range_CT[2]
    seq.rec <- unique(append(seq.rec, rec.max))
    
    # seq.kd <- seq(from=input$kd_range_CT[1],to=input$kd_range_CT[2],by=1)
    seq.kd <- seq(from=input$kd_range_CT[1],to=input$kd_range_CT[2],by=input$plot_interval_kd_CT)
    kd.max <- input$kd_range_CT[2]
    seq.kd <- unique(append(seq.kd, kd.max))
    
    # seq.coat <- seq(from=input$coat_range_CT[1],to=input$coat_range_CT[2],by=0.2)
    seq.coat <- seq(from=input$coat_range_CT[1],to=input$coat_range_CT[2],by=input$plot_interval_comp)
    coat.max <- input$coat_range_CT[2]
    seq.coat <- unique(append(seq.coat, coat.max))
    
    seq.ki <- seq(from=input$pKdcoat_CT[1],to=input$pKdcoat_CT[2],by=input$plot_interval_ki)
    ki.max <- input$pKdcoat_CT[2]
    seq.ki <- unique(append(seq.ki, ki.max))
    
    min.coat <- 10^min(seq.coat)*fixed_coat
    max.coat <- 10^max(seq.coat)*fixed_coat
    min.lig <- 10^min(seq.lig)*fixed_ligand
    max.lig <- 10^max(seq.lig)*fixed_ligand
    min.rec <- 10^min(seq.rec)*fixed_receptor
    max.rec <- 10^max(seq.rec)*fixed_receptor
    rec.dots <- 10^seq.rec*fixed_receptor
    
    
    leggy <- c()
    my.df <- NULL
    mylist <- NULL
    
    for (lig.cnt in seq.lig) {
      this.lig <- 10^lig.cnt*fixed_ligand
      ## now each line
      occ.dots <- c()
      for (rec.cnt in seq.rec) {
        this.rec <- 10^rec.cnt*fixed_receptor
        gfun <- function(x) {c(this.rec - x[1] - x[2]*x[1]/kdcoating - x[1]*x[3]/fixed_kd, 0-x[2]-x[2]*x[1]/kdcoating, this.lig-x[3]-x[1]*x[3]/fixed_kd)}
        fred <- multiroot(gfun, start=c(this.rec/2,0,this.lig/2), positive=TRUE )
        c_free_receptor <- fred$root[1]
        c_free_coat <- fred$root[2]
        c_free_ligand <- fred$root[3]
        c_complex <- this.lig - c_free_ligand
        pc_occ = 100*(1-c_free_ligand/this.lig)
        occ.dots <- c(occ.dots, pc_occ)
        my.df <- (cbind(rec.dots, this.lig, occ.dots))
      }
      mylist <- data.frame(rbind(mylist, my.df))
    }
    
    if (input$p1concunit_CT == "metric units"){
      print("yes it does")
      rec.dots <- 10^seq.rec*input$c_total_receptor_CT_start
      this.lig <- 10^seq.lig*input$c_total_ligand_CT_start
      occ.dots <- mylist[,3]
      
      temp <- NULL
      for (i in 1:length(this.lig)){
        temp[[i]] <- cbind(rec.dots, this.lig[i])
      }
      temp <- do.call(rbind.data.frame, temp)
      colnames(temp)[2] <- "this.lig"
      
      mylist <- cbind.data.frame(temp, occ.dots)
      str(mylist)
      print(mylist)
      
    } 
    # for (i in 1:nrow(mylist)){
    # 
    #   mylist$rec.dots[i] <- round(convert_units_back_p1_CT (mylist$rec.dots[i]),2)
    #   mylist$this.lig[i] <- round(convert_units_back_p2_CT (mylist$this.lig[i]),2)
    #   
    # }
    
    
    return(mylist)
  })
  
  roccbyrecrangeligdatatwo <- reactive({
    
    volume <- 1
    
    fixed_ligand <- input$c_total_ligand_CT
    fixed_receptor <- input$c_total_receptor_CT
    fixed_coat <- input$c_total_coating_CT
    
    if (input$A_selected_first_var_graph2 == "KD"){
      fixed_kd <- input$A_first_var_value2_graph2
    }else {
      fixed_kd <- input$Kd_CT
    }
    
    if (input$A_selected_first_var_graph2 == input$ligand_name_CT){
      fixed_ligand <- convert_units_p2 (input$A_first_var_value2_graph2)
    }
    
    if (input$A_selected_first_var_graph2 == input$coating_name_CT){
      fixed_coat <- convert_units_coat(input$A_first_var_value2_graph2)
    }
    print (fixed_coat)
    
    if (input$A_selected_first_var_graph2 == "KI"){
      kdcoating <- input$A_first_var_value2_graph2
    }else {
      kdcoating <- input$ki_CT
    }
    
    # if (fixed_coat==0) {fixed_coat <- 1000}
    if (fixed_coat<=5) {fixed_coat <- 1000}
    if (fixed_ligand<=5) {fixed_ligand <- 1000}
    # seq.lig <- seq(from=input$lig_range_CT[1],to=input$lig_range_CT[2],by=1)
    seq.lig <- seq(from=input$lig_range_CT[1],to=input$lig_range_CT[2],by=input$plot_interval_p2_CT)
    lig.max <-input$lig_range_CT[2]
    seq.lig <- unique(append(seq.lig, lig.max))
    
    # seq.rec <- seq(from=input$rec_range_CT[1],to=input$rec_range_CT[2],by=0.2)
    seq.rec <- seq(from=input$rec_range_CT[1],to=input$rec_range_CT[2],by=input$plot_interval_p1_CT)
    rec.max <- input$rec_range_CT[2]
    seq.rec <- unique(append(seq.rec, rec.max))
    
    # seq.kd <- seq(from=input$kd_range_CT[1],to=input$kd_range_CT[2],by=1)
    seq.kd <- seq(from=input$kd_range_CT[1],to=input$kd_range_CT[2],by=input$plot_interval_kd_CT)
    kd.max <- input$kd_range_CT[2]
    seq.kd <- unique(append(seq.kd, kd.max))
    
    # seq.coat <- seq(from=input$coat_range_CT[1],to=input$coat_range_CT[2],by=0.2)
    seq.coat <- seq(from=input$coat_range_CT[1],to=input$coat_range_CT[2],by=input$plot_interval_comp)
    coat.max <- input$coat_range_CT[2]
    seq.coat <- unique(append(seq.coat, coat.max))
    
    seq.ki <- seq(from=input$pKdcoat_CT[1],to=input$pKdcoat_CT[2],by=input$plot_interval_ki)
    ki.max <- input$pKdcoat_CT[2]
    seq.ki <- unique(append(seq.ki, ki.max))
    
    min.coat <- 10^min(seq.coat)*fixed_coat
    max.coat <- 10^max(seq.coat)*fixed_coat
    min.lig <- 10^min(seq.lig)*fixed_ligand
    max.lig <- 10^max(seq.lig)*fixed_ligand
    min.rec <- 10^min(seq.rec)*fixed_receptor
    max.rec <- 10^max(seq.rec)*fixed_receptor
    rec.dots <- 10^seq.rec*fixed_receptor
    
    
    leggy <- c()
    my.df <- NULL
    mylist <- NULL
    
    for (lig.cnt in seq.lig) {
      this.lig <- 10^lig.cnt*fixed_ligand
      ## now each line
      occ.dots <- c()
      for (rec.cnt in seq.rec) {
        this.rec <- 10^rec.cnt*fixed_receptor
        gfun <- function(x) {c(this.rec - x[1] - x[2]*x[1]/kdcoating - x[1]*x[3]/fixed_kd, 0-x[2]-x[2]*x[1]/kdcoating, this.lig-x[3]-x[1]*x[3]/fixed_kd)}
        fred <- multiroot(gfun, start=c(this.rec/2,0,this.lig/2), positive=TRUE )
        c_free_receptor <- fred$root[1]
        c_free_coat <- fred$root[2]
        c_free_ligand <- fred$root[3]
        c_complex <- this.lig - c_free_ligand
        pc_occ = 100*(1-c_free_ligand/this.lig)
        occ.dots <- c(occ.dots, pc_occ)
        my.df <- (cbind(rec.dots, this.lig, occ.dots))
      }
      mylist <- data.frame(rbind(mylist, my.df))
    }
    
    if (input$p1concunit_CT == "metric units"){
      print("yes it does")
      rec.dots <- 10^seq.rec*input$c_total_receptor_CT_start
      this.lig <- 10^seq.lig*input$c_total_ligand_CT_start
      occ.dots <- mylist[,3]
      
      temp <- NULL
      for (i in 1:length(this.lig)){
        temp[[i]] <- cbind(rec.dots, this.lig[i])
      }
      temp <- do.call(rbind.data.frame, temp)
      colnames(temp)[2] <- "this.lig"
      
      mylist <- cbind.data.frame(temp, occ.dots)
      # str(mylist)
      # print(mylist)
      
    } 
    
    # for (i in 1:nrow(mylist)){
    #   
    #   mylist$rec.dots[i] <- round(convert_units_back_p1_CT (mylist$rec.dots[i]),1)
    #   mylist$this.lig[i] <- round(convert_units_back_p2_CT (mylist$this.lig[i]),2)
    # }
    
    return(mylist)
  })
  
  #OVER Competitor
  
  roccbyrecrangecoatdataone <- reactive({
    
    volume <- 1
    
    fixed_ligand <- input$c_total_ligand_CT
    fixed_receptor <- input$c_total_receptor_CT
    fixed_coat <- input$c_total_coating_CT
    
    if (input$A_selected_first_var_graph2 == "KD"){
      fixed_kd <- input$A_first_var_value_graph2
    }else {
      fixed_kd <- input$Kd_CT
    }
    
    if (input$A_selected_first_var_graph2 == input$ligand_name_CT){
      fixed_ligand <- convert_units_p2 (input$A_first_var_value_graph2)
    }
    
    if (input$A_selected_first_var_graph2 == input$coating_name_CT){
      fixed_coat <- convert_units_coat(input$A_first_var_value_graph2)
    }
    
    
    if (input$A_selected_first_var == "KI"){
      kdcoating <- input$A_first_var_value_graph2
    }else {
      kdcoating <- input$ki_CT
    }
    
    # if (fixed_coat==0) {fixed_coat <- 1000}
    if (fixed_coat<=5) {fixed_coat <- 1000}
    if (fixed_ligand<=5) {fixed_ligand <- 1000}
    # seq.lig <- seq(from=input$lig_range_CT[1],to=input$lig_range_CT[2],by=1)
    seq.lig <- seq(from=input$lig_range_CT[1],to=input$lig_range_CT[2],by=input$plot_interval_p2_CT)
    lig.max <-input$lig_range_CT[2]
    seq.lig <- unique(append(seq.lig, lig.max))
    
    # seq.rec <- seq(from=input$rec_range_CT[1],to=input$rec_range_CT[2],by=0.2)
    seq.rec <- seq(from=input$rec_range_CT[1],to=input$rec_range_CT[2],by=input$plot_interval_p1_CT)
    rec.max <- input$rec_range_CT[2]
    seq.rec <- unique(append(seq.rec, rec.max))
    
    # seq.kd <- seq(from=input$kd_range_CT[1],to=input$kd_range_CT[2],by=1)
    seq.kd <- seq(from=input$kd_range_CT[1],to=input$kd_range_CT[2],by=input$plot_interval_kd_CT)
    kd.max <- input$kd_range_CT[2]
    seq.kd <- unique(append(seq.kd, kd.max))
    
    # seq.coat <- seq(from=input$coat_range_CT[1],to=input$coat_range_CT[2],by=0.2)
    seq.coat <- seq(from=input$coat_range_CT[1],to=input$coat_range_CT[2],by=input$plot_interval_comp)
    coat.max <- input$coat_range_CT[2]
    seq.coat <- unique(append(seq.coat, coat.max))
    
    seq.ki <- seq(from=input$pKdcoat_CT[1],to=input$pKdcoat_CT[2],by=input$plot_interval_ki)
    ki.max <- input$pKdcoat_CT[2]
    seq.ki <- unique(append(seq.ki, ki.max))
    
    min.coat <- 10^min(seq.coat)*fixed_coat
    max.coat <- 10^max(seq.coat)*fixed_coat
    min.lig <- 10^min(seq.lig)*fixed_ligand
    max.lig <- 10^max(seq.lig)*fixed_ligand
    min.rec <- 10^min(seq.rec)*fixed_receptor
    max.rec <- 10^max(seq.rec)*fixed_receptor
    rec.dots <- 10^seq.rec*fixed_receptor
    
    
    leggy <- c()
    my.df <- NULL
    mylist <- NULL
    
    for (coat.cnt in seq.coat) {
      this.coat <- 10^coat.cnt*fixed_coat
      ## now each line
      occ.dots <- c()
      for (rec.cnt in seq.rec) {
        this.rec <- 10^rec.cnt*fixed_receptor
        # gfun <- function(x) {c(this.rec - x[1] - x[2]*x[1]/kdcoating - x[1]*x[3]/fixed_kd, 0-x[2]-x[2]*x[1]/kdcoating, this.coat-x[3]-x[1]*x[3]/fixed_kd)}
        # fred <- multiroot(gfun, start=c(this.rec/2,0,this.coat/2), positive=TRUE )
        
        gfun <- function(x) {c(this.rec - x[1] - x[2]*x[1]/kdcoating - x[1]*x[3]/fixed_kd, this.coat-x[2]-x[2]*x[1]/kdcoating, fixed_ligand-x[3]-x[1]*x[3]/fixed_kd)}
        fred <- multiroot( gfun, start=c(this.rec/2,this.coat/2,fixed_ligand/2), positive=TRUE )
        
        c_free_receptor <- fred$root[1]
        c_free_coat <- fred$root[2]
        c_free_ligand <- fred$root[3]
        c_complex <- fixed_ligand - c_free_ligand
        pc_occ = 100*(1-c_free_ligand/fixed_ligand)
        occ.dots <- c(occ.dots, pc_occ)
        my.df <- (cbind(rec.dots, this.coat, occ.dots))
      }
      mylist <- data.frame(rbind(mylist, my.df))
    }
    
    if (input$p1concunit_CT == "metric units"){
      print("yes it does")
      rec.dots <- 10^seq.rec*input$c_total_receptor_CT_start
      this.coat <- 10^seq.lig*input$c_total_coating_CT_start
      occ.dots <- mylist[,3]
      
      temp <- NULL
      for (i in 1:length(this.coat)){
        temp[[i]] <- cbind(rec.dots, this.coat[i])
      }
      temp <- do.call(rbind.data.frame, temp)
      colnames(temp)[2] <- "this.coat"
      
      mylist <- cbind.data.frame(temp, occ.dots)
      # str(mylist)
      # print(mylist)
      
    } 
    
    # for (i in 1:nrow(mylist)){
    #   
    #   mylist$rec.dots[i] <- round(convert_units_back_p1_CT (mylist$rec.dots[i]),1)
    #   mylist$this.coat[i] <- round(convert_units_back_coat (mylist$this.coat[i]),2)
    # }
    # 
    return(mylist)
  })
  
  roccbyrecrangecoatdatatwo <- reactive({
    
    volume <- 1
    
    fixed_ligand <- input$c_total_ligand_CT
    fixed_receptor <- input$c_total_receptor_CT
    fixed_coat <- input$c_total_coating_CT
    
    if (input$A_selected_first_var_graph2 == "KD"){
      fixed_kd <- input$A_first_var_value2_graph2
    }else {
      fixed_kd <- input$Kd_CT
    }
    
    if (input$A_selected_first_var_graph2 == input$ligand_name_CT){
      fixed_ligand <- convert_units_p2 (input$A_first_var_value2_graph2)
    }
    
    if (input$A_selected_first_var_graph2 == input$coating_name_CT){
      fixed_coat <- convert_units_coat(input$A_first_var_value2_graph2)
    }
    
    
    if (input$A_selected_first_var == "KI"){
      kdcoating <- input$A_first_var_value2_graph2
    }else {
      kdcoating <- input$ki_CT
    }
    
    # if (fixed_coat==0) {fixed_coat <- 1000}
    if (fixed_coat<=5) {fixed_coat <- 1000}
    if (fixed_ligand<=5) {fixed_ligand <- 1000}
    # seq.lig <- seq(from=input$lig_range_CT[1],to=input$lig_range_CT[2],by=1)
    seq.lig <- seq(from=input$lig_range_CT[1],to=input$lig_range_CT[2],by=input$plot_interval_p2_CT)
    lig.max <-input$lig_range_CT[2]
    seq.lig <- unique(append(seq.lig, lig.max))
    
    # seq.rec <- seq(from=input$rec_range_CT[1],to=input$rec_range_CT[2],by=0.2)
    seq.rec <- seq(from=input$rec_range_CT[1],to=input$rec_range_CT[2],by=input$plot_interval_p1_CT)
    rec.max <- input$rec_range_CT[2]
    seq.rec <- unique(append(seq.rec, rec.max))
    
    # seq.kd <- seq(from=input$kd_range_CT[1],to=input$kd_range_CT[2],by=1)
    seq.kd <- seq(from=input$kd_range_CT[1],to=input$kd_range_CT[2],by=input$plot_interval_kd_CT)
    kd.max <- input$kd_range_CT[2]
    seq.kd <- unique(append(seq.kd, kd.max))
    
    # seq.coat <- seq(from=input$coat_range_CT[1],to=input$coat_range_CT[2],by=0.2)
    seq.coat <- seq(from=input$coat_range_CT[1],to=input$coat_range_CT[2],by=input$plot_interval_comp)
    coat.max <- input$coat_range_CT[2]
    seq.coat <- unique(append(seq.coat, coat.max))
    
    seq.ki <- seq(from=input$pKdcoat_CT[1],to=input$pKdcoat_CT[2],by=input$plot_interval_ki)
    ki.max <- input$pKdcoat_CT[2]
    seq.ki <- unique(append(seq.ki, ki.max))
    
    min.coat <- 10^min(seq.coat)*fixed_coat
    max.coat <- 10^max(seq.coat)*fixed_coat
    min.lig <- 10^min(seq.lig)*fixed_ligand
    max.lig <- 10^max(seq.lig)*fixed_ligand
    min.rec <- 10^min(seq.rec)*fixed_receptor
    max.rec <- 10^max(seq.rec)*fixed_receptor
    rec.dots <- 10^seq.rec*fixed_receptor
    
    
    leggy <- c()
    my.df <- NULL
    mylist <- NULL
    
    for (coat.cnt in seq.coat) {
      this.coat <- 10^coat.cnt*fixed_coat
      ## now each line
      occ.dots <- c()
      for (rec.cnt in seq.rec) {
        this.rec <- 10^rec.cnt*fixed_receptor
        # gfun <- function(x) {c(this.rec - x[1] - x[2]*x[1]/kdcoating - x[1]*x[3]/fixed_kd, 0-x[2]-x[2]*x[1]/kdcoating, this.coat-x[3]-x[1]*x[3]/fixed_kd)}
        # fred <- multiroot(gfun, start=c(this.rec/2,0,this.coat/2), positive=TRUE )
        
        gfun <- function(x) {c(this.rec - x[1] - x[2]*x[1]/kdcoating - x[1]*x[3]/fixed_kd, this.coat-x[2]-x[2]*x[1]/kdcoating, fixed_ligand-x[3]-x[1]*x[3]/fixed_kd)}
        fred <- multiroot( gfun, start=c(this.rec/2,this.coat/2,fixed_ligand/2), positive=TRUE )
        
        c_free_receptor <- fred$root[1]
        c_free_coat <- fred$root[2]
        c_free_ligand <- fred$root[3]
        c_complex <- fixed_ligand - c_free_ligand
        pc_occ = 100*(1-c_free_ligand/fixed_ligand)
        occ.dots <- c(occ.dots, pc_occ)
        my.df <- (cbind(rec.dots, this.coat, occ.dots))
      }
      mylist <- data.frame(rbind(mylist, my.df))
    }
    
    if (input$p1concunit_CT == "metric units"){
      print("yes it does")
      rec.dots <- 10^seq.rec*input$c_total_receptor_CT_start
      this.coat <- 10^seq.lig*input$c_total_coating_CT_start
      occ.dots <- mylist[,3]
      
      temp <- NULL
      for (i in 1:length(this.coat)){
        temp[[i]] <- cbind(rec.dots, this.coat[i])
      }
      temp <- do.call(rbind.data.frame, temp)
      colnames(temp)[2] <- "this.coat"
      
      mylist <- cbind.data.frame(temp, occ.dots)
      # str(mylist)
      # print(mylist)
      
    } 
    
    # for (i in 1:nrow(mylist)){
    #   
    #   mylist$rec.dots[i] <- round(convert_units_back_p1_CT (mylist$rec.dots[i]),1)
    #   mylist$this.coat[i] <- round(convert_units_back_coat (mylist$this.coat[i]),2)
    # }
    
    return(mylist)
  })
  
  #over kd
  roccbyrecrangekddataone <- reactive({
    
    volume <- 1
    
    fixed_ligand <- input$c_total_ligand_CT
    fixed_receptor <- input$c_total_receptor_CT
    fixed_coat <- input$c_total_coating_CT
    
    if (input$A_selected_first_var_graph2 == "KD"){
      fixed_kd <- input$A_first_var_value_graph2
    }else {
      fixed_kd <- input$Kd_CT
    }
    
    if (input$A_selected_first_var_graph2 == input$ligand_name_CT){
      fixed_ligand <- convert_units_p2 (input$A_first_var_value_graph2)
    }
    
    if (input$A_selected_first_var_graph2 == input$coating_name_CT){
      fixed_coat <- convert_units_coat(input$A_first_var_value_graph2)
    }
    
    
    if (input$A_selected_first_var == "KI"){
      kdcoating <- input$A_first_var_value_graph2
    }else {
      kdcoating <- input$ki_CT
    }
    
    # if (fixed_coat==0) {fixed_coat <- 1000}
    if (fixed_coat<=5) {fixed_coat <- 1000}
    if (fixed_ligand<=5) {fixed_ligand <- 1000}
    # seq.lig <- seq(from=input$lig_range_CT[1],to=input$lig_range_CT[2],by=1)
    seq.lig <- seq(from=input$lig_range_CT[1],to=input$lig_range_CT[2],by=input$plot_interval_p2_CT)
    lig.max <-input$lig_range_CT[2]
    seq.lig <- unique(append(seq.lig, lig.max))
    
    # seq.rec <- seq(from=input$rec_range_CT[1],to=input$rec_range_CT[2],by=0.2)
    seq.rec <- seq(from=input$rec_range_CT[1],to=input$rec_range_CT[2],by=input$plot_interval_p1_CT)
    rec.max <- input$rec_range_CT[2]
    seq.rec <- unique(append(seq.rec, rec.max))
    
    # seq.kd <- seq(from=input$kd_range_CT[1],to=input$kd_range_CT[2],by=1)
    seq.kd <- seq(from=input$kd_range_CT[1],to=input$kd_range_CT[2],by=input$plot_interval_kd_CT)
    kd.max <- input$kd_range_CT[2]
    seq.kd <- unique(append(seq.kd, kd.max))
    
    # seq.coat <- seq(from=input$coat_range_CT[1],to=input$coat_range_CT[2],by=0.2)
    seq.coat <- seq(from=input$coat_range_CT[1],to=input$coat_range_CT[2],by=input$plot_interval_comp)
    coat.max <- input$coat_range_CT[2]
    seq.coat <- unique(append(seq.coat, coat.max))
    
    seq.ki <- seq(from=input$pKdcoat_CT[1],to=input$pKdcoat_CT[2],by=input$plot_interval_ki)
    ki.max <- input$pKdcoat_CT[2]
    seq.ki <- unique(append(seq.ki, ki.max))
    
    min.coat <- 10^min(seq.coat)*fixed_coat
    max.coat <- 10^max(seq.coat)*fixed_coat
    min.lig <- 10^min(seq.lig)*fixed_ligand
    max.lig <- 10^max(seq.lig)*fixed_ligand
    min.rec <- 10^min(seq.rec)*fixed_receptor
    max.rec <- 10^max(seq.rec)*fixed_receptor
    rec.dots <- 10^seq.rec*fixed_receptor
    
    
    leggy <- c()
    my.df <- NULL
    mylist <- NULL
    
    for (kd.cnt in seq.kd) {
      this.kd <- 10^kd.cnt*fixed_kd
      ## now each line
      occ.dots <- c()
      for (rec.cnt in seq.rec) {
        this.rec <- 10^rec.cnt*fixed_receptor
        gfun <- function(x) {c(this.rec - x[1] - x[2]*x[1]/kdcoating - x[1]*x[3]/this.kd, 0-x[2]-x[2]*x[1]/kdcoating, fixed_coat-x[3]-x[1]*x[3]/this.kd)}
        fred <- multiroot(gfun, start=c(this.rec/2,0,fixed_coat/2), positive=TRUE )
        
        c_free_receptor <- fred$root[1]
        c_free_coat <- fred$root[2]
        c_free_ligand <- fred$root[3]
        c_complex <- fixed_ligand - c_free_ligand
        pc_occ = 100*(1-c_free_ligand/fixed_ligand)
        occ.dots <- c(occ.dots, pc_occ)
        my.df <- (cbind(rec.dots, this.kd, occ.dots))
      }
      mylist <- data.frame(rbind(mylist, my.df))
    }
    
    if (input$p1concunit_CT == "metric units"){
      print("yes it does")
      rec.dots <- 10^seq.rec*input$c_total_receptor_CT_start
      this.kd <- 10^seq.lig*input$Kd_CT
      occ.dots <- mylist[,3]
      
      temp <- NULL
      for (i in 1:length(this.kd)){
        temp[[i]] <- cbind(rec.dots, this.kd[i])
      }
      temp <- do.call(rbind.data.frame, temp)
      colnames(temp)[2] <- "this.kd"
      
      mylist <- cbind.data.frame(temp, occ.dots)
      # str(mylist)
      # print(mylist)
      
    } 
    
    # for (i in 1:nrow(mylist)){
    #   
    #   mylist$rec.dots[i] <- round(convert_units_back_p1_CT (mylist$rec.dots[i]),1)
    #   # mylist$this.coat[i] <- round(convert_units_back_coat (mylist$this.coat[i]),2)
    # }
    
    return(mylist)
  })
  
  roccbyrecrangekddatatwo <- reactive({
    
    volume <- 1
    
    fixed_ligand <- input$c_total_ligand_CT
    fixed_receptor <- input$c_total_receptor_CT
    fixed_coat <- input$c_total_coating_CT
    
    if (input$A_selected_first_var_graph2 == "KD"){
      fixed_kd <- input$A_first_var_value2_graph2
    }else {
      fixed_kd <- input$Kd_CT
    }
    
    if (input$A_selected_first_var_graph2 == input$ligand_name_CT){
      fixed_ligand <- convert_units_p2 (input$A_first_var_value2_graph2)
    }
    
    if (input$A_selected_first_var_graph2 == input$coating_name_CT){
      fixed_coat <- convert_units_coat(input$A_first_var_value2_graph2)
    }
    
    
    if (input$A_selected_first_var == "KI"){
      kdcoating <- input$A_first_var_value2_graph2
    }else {
      kdcoating <- input$ki_CT
    }
    
    # if (fixed_coat==0) {fixed_coat <- 1000}
    if (fixed_coat<=5) {fixed_coat <- 1000}
    if (fixed_ligand<=5) {fixed_ligand <- 1000}
    # seq.lig <- seq(from=input$lig_range_CT[1],to=input$lig_range_CT[2],by=1)
    seq.lig <- seq(from=input$lig_range_CT[1],to=input$lig_range_CT[2],by=input$plot_interval_p2_CT)
    lig.max <-input$lig_range_CT[2]
    seq.lig <- unique(append(seq.lig, lig.max))
    
    # seq.rec <- seq(from=input$rec_range_CT[1],to=input$rec_range_CT[2],by=0.2)
    seq.rec <- seq(from=input$rec_range_CT[1],to=input$rec_range_CT[2],by=input$plot_interval_p1_CT)
    rec.max <- input$rec_range_CT[2]
    seq.rec <- unique(append(seq.rec, rec.max))
    
    # seq.kd <- seq(from=input$kd_range_CT[1],to=input$kd_range_CT[2],by=1)
    seq.kd <- seq(from=input$kd_range_CT[1],to=input$kd_range_CT[2],by=input$plot_interval_kd_CT)
    kd.max <- input$kd_range_CT[2]
    seq.kd <- unique(append(seq.kd, kd.max))
    
    # seq.coat <- seq(from=input$coat_range_CT[1],to=input$coat_range_CT[2],by=0.2)
    seq.coat <- seq(from=input$coat_range_CT[1],to=input$coat_range_CT[2],by=input$plot_interval_comp)
    coat.max <- input$coat_range_CT[2]
    seq.coat <- unique(append(seq.coat, coat.max))
    
    seq.ki <- seq(from=input$pKdcoat_CT[1],to=input$pKdcoat_CT[2],by=input$plot_interval_ki)
    ki.max <- input$pKdcoat_CT[2]
    seq.ki <- unique(append(seq.ki, ki.max))
    
    min.coat <- 10^min(seq.coat)*fixed_coat
    max.coat <- 10^max(seq.coat)*fixed_coat
    min.lig <- 10^min(seq.lig)*fixed_ligand
    max.lig <- 10^max(seq.lig)*fixed_ligand
    min.rec <- 10^min(seq.rec)*fixed_receptor
    max.rec <- 10^max(seq.rec)*fixed_receptor
    rec.dots <- 10^seq.rec*fixed_receptor
    
    
    leggy <- c()
    my.df <- NULL
    mylist <- NULL
    
    for (kd.cnt in seq.kd) {
      this.kd <- 10^kd.cnt*fixed_kd
      ## now each line
      occ.dots <- c()
      for (rec.cnt in seq.rec) {
        this.rec <- 10^rec.cnt*fixed_receptor
        gfun <- function(x) {c(this.rec - x[1] - x[2]*x[1]/kdcoating - x[1]*x[3]/this.kd, 0-x[2]-x[2]*x[1]/kdcoating, fixed_coat-x[3]-x[1]*x[3]/this.kd)}
        fred <- multiroot(gfun, start=c(this.rec/2,0,fixed_coat/2), positive=TRUE )
        
        c_free_receptor <- fred$root[1]
        c_free_coat <- fred$root[2]
        c_free_ligand <- fred$root[3]
        c_complex <- fixed_ligand - c_free_ligand
        pc_occ = 100*(1-c_free_ligand/fixed_ligand)
        occ.dots <- c(occ.dots, pc_occ)
        my.df <- (cbind(rec.dots, this.kd, occ.dots))
      }
      mylist <- data.frame(rbind(mylist, my.df))
    }
    
    if (input$p1concunit_CT == "metric units"){
      print("yes it does")
      rec.dots <- 10^seq.rec*input$c_total_receptor_CT_start
      this.kd <- 10^seq.lig*input$Kd_CT
      occ.dots <- mylist[,3]
      
      temp <- NULL
      for (i in 1:length(this.kd)){
        temp[[i]] <- cbind(rec.dots, this.kd[i])
      }
      temp <- do.call(rbind.data.frame, temp)
      colnames(temp)[2] <- "this.kd"
      
      mylist <- cbind.data.frame(temp, occ.dots)
      # str(mylist)
      # print(mylist)
      
    } 
    # for (i in 1:nrow(mylist)){
    #   
    #   mylist$rec.dots[i] <- round(convert_units_back_p1_CT (mylist$rec.dots[i]),1)
    #   # mylist$this.coat[i] <- round(convert_units_back_coat (mylist$this.coat[i]),2)
    # }
    
    return(mylist)
  })
  
  #over ki
  roccbyrecrangekidataone <- reactive({
    
    volume <- 1
    
    fixed_ligand <- input$c_total_ligand_CT
    fixed_receptor <- input$c_total_receptor_CT
    fixed_coat <- input$c_total_coating_CT
    
    if (input$A_selected_first_var_graph2 == "KD"){
      fixed_kd <- input$A_first_var_value_graph2
    }else {
      fixed_kd <- input$Kd_CT
    }
    
    if (input$A_selected_first_var_graph2 == input$ligand_name_CT){
      fixed_ligand <- convert_units_p2 (input$A_first_var_value_graph2)
    }
    
    if (input$A_selected_first_var_graph2 == input$coating_name_CT){
      fixed_coat <- convert_units_coat(input$A_first_var_value_graph2)
    }
    
    
    if (input$A_selected_first_var == "KI"){
      kdcoating <- input$A_first_var_value_graph2
    }else {
      kdcoating <- input$ki_CT
    }
    
    # if (fixed_coat==0) {fixed_coat <- 1000}
    if (fixed_coat<=5) {fixed_coat <- 1000}
    if (fixed_ligand<=5) {fixed_ligand <- 1000}
    # seq.lig <- seq(from=input$lig_range_CT[1],to=input$lig_range_CT[2],by=1)
    seq.lig <- seq(from=input$lig_range_CT[1],to=input$lig_range_CT[2],by=input$plot_interval_p2_CT)
    lig.max <-input$lig_range_CT[2]
    seq.lig <- unique(append(seq.lig, lig.max))
    
    # seq.rec <- seq(from=input$rec_range_CT[1],to=input$rec_range_CT[2],by=0.2)
    seq.rec <- seq(from=input$rec_range_CT[1],to=input$rec_range_CT[2],by=input$plot_interval_p1_CT)
    rec.max <- input$rec_range_CT[2]
    seq.rec <- unique(append(seq.rec, rec.max))
    
    # seq.kd <- seq(from=input$kd_range_CT[1],to=input$kd_range_CT[2],by=1)
    seq.kd <- seq(from=input$kd_range_CT[1],to=input$kd_range_CT[2],by=input$plot_interval_kd_CT)
    kd.max <- input$kd_range_CT[2]
    seq.kd <- unique(append(seq.kd, kd.max))
    
    # seq.coat <- seq(from=input$coat_range_CT[1],to=input$coat_range_CT[2],by=0.2)
    seq.coat <- seq(from=input$coat_range_CT[1],to=input$coat_range_CT[2],by=input$plot_interval_comp)
    coat.max <- input$coat_range_CT[2]
    seq.coat <- unique(append(seq.coat, coat.max))
    
    seq.ki <- seq(from=input$pKdcoat_CT[1],to=input$pKdcoat_CT[2],by=input$plot_interval_ki)
    ki.max <- input$pKdcoat_CT[2]
    seq.ki <- unique(append(seq.ki, ki.max))
    
    min.coat <- 10^min(seq.coat)*fixed_coat
    max.coat <- 10^max(seq.coat)*fixed_coat
    min.lig <- 10^min(seq.lig)*fixed_ligand
    max.lig <- 10^max(seq.lig)*fixed_ligand
    min.rec <- 10^min(seq.rec)*fixed_receptor
    max.rec <- 10^max(seq.rec)*fixed_receptor
    rec.dots <- 10^seq.rec*fixed_receptor
    
    
    leggy <- c()
    my.df <- NULL
    mylist <- NULL
    
    for (ki.cnt in seq.ki) {
      this.ki <- 10^ki.cnt*kdcoating
      ## now each line
      occ.dots <- c()
      for (rec.cnt in seq.rec) {
        this.rec <- 10^rec.cnt*fixed_receptor
        gfun <- function(x) {c(this.rec - x[1] - x[2]*x[1]/this.ki - x[1]*x[3]/fixed_kd, 0-x[2]-x[2]*x[1]/this.ki, fixed_coat-x[3]-x[1]*x[3]/fixed_kd)}
        fred <- multiroot(gfun, start=c(this.rec/2,0,fixed_coat/2), positive=TRUE )
        
        c_free_receptor <- fred$root[1]
        c_free_coat <- fred$root[2]
        c_free_ligand <- fred$root[3]
        c_complex <- fixed_ligand - c_free_ligand
        pc_occ = 100*(1-c_free_ligand/fixed_ligand)
        occ.dots <- c(occ.dots, pc_occ)
        my.df <- (cbind(rec.dots, this.ki, occ.dots))
      }
      mylist <- data.frame(rbind(mylist, my.df))
    }
    
    if (input$p1concunit_CT == "metric units"){
      print("yes it does")
      rec.dots <- 10^seq.rec*input$c_total_receptor_CT_start
      this.ki <- 10^seq.lig*input$ki_CT
      occ.dots <- mylist[,3]
      
      temp <- NULL
      for (i in 1:length(this.ki)){
        temp[[i]] <- cbind(rec.dots, this.ki[i])
      }
      temp <- do.call(rbind.data.frame, temp)
      colnames(temp)[2] <- "this.ki"
      
      mylist <- cbind.data.frame(temp, occ.dots)
      # str(mylist)
      # print(mylist)
      
    } 
    # for (i in 1:nrow(mylist)){
    #   
    #   mylist$rec.dots[i] <- round(convert_units_back_p1_CT (mylist$rec.dots[i]),1)
    #   # mylist$this.coat[i] <- round(convert_units_back_coat (mylist$this.coat[i]),2)
    # }
    
    return(mylist)
  })
  
  roccbyrecrangekidatatwo <- reactive({
    
    volume <- 1
    
    fixed_ligand <- input$c_total_ligand_CT
    fixed_receptor <- input$c_total_receptor_CT
    fixed_coat <- input$c_total_coating_CT
    
    if (input$A_selected_first_var_graph2 == "KD"){
      fixed_kd <- input$A_first_var_value2_graph2
    }else {
      fixed_kd <- input$Kd_CT
    }
    
    if (input$A_selected_first_var_graph2 == input$ligand_name_CT){
      fixed_ligand <- convert_units_p2 (input$A_first_var_value2_graph2)
    }
    
    if (input$A_selected_first_var_graph2 == input$coating_name_CT){
      fixed_coat <- convert_units_coat(input$A_first_var_value2_graph2)
    }
    
    
    if (input$A_selected_first_var == "KI"){
      kdcoating <- input$A_first_var_value2_graph2
    }else {
      kdcoating <- input$ki_CT
    }
    
    # if (fixed_coat==0) {fixed_coat <- 1000}
    if (fixed_coat<=5) {fixed_coat <- 1000}
    if (fixed_ligand<=5) {fixed_ligand <- 1000}
    # seq.lig <- seq(from=input$lig_range_CT[1],to=input$lig_range_CT[2],by=1)
    seq.lig <- seq(from=input$lig_range_CT[1],to=input$lig_range_CT[2],by=input$plot_interval_p2_CT)
    lig.max <-input$lig_range_CT[2]
    seq.lig <- unique(append(seq.lig, lig.max))
    
    # seq.rec <- seq(from=input$rec_range_CT[1],to=input$rec_range_CT[2],by=0.2)
    seq.rec <- seq(from=input$rec_range_CT[1],to=input$rec_range_CT[2],by=input$plot_interval_p1_CT)
    rec.max <- input$rec_range_CT[2]
    seq.rec <- unique(append(seq.rec, rec.max))
    
    # seq.kd <- seq(from=input$kd_range_CT[1],to=input$kd_range_CT[2],by=1)
    seq.kd <- seq(from=input$kd_range_CT[1],to=input$kd_range_CT[2],by=input$plot_interval_kd_CT)
    kd.max <- input$kd_range_CT[2]
    seq.kd <- unique(append(seq.kd, kd.max))
    
    # seq.coat <- seq(from=input$coat_range_CT[1],to=input$coat_range_CT[2],by=0.2)
    seq.coat <- seq(from=input$coat_range_CT[1],to=input$coat_range_CT[2],by=input$plot_interval_comp)
    coat.max <- input$coat_range_CT[2]
    seq.coat <- unique(append(seq.coat, coat.max))
    
    seq.ki <- seq(from=input$pKdcoat_CT[1],to=input$pKdcoat_CT[2],by=input$plot_interval_ki)
    ki.max <- input$pKdcoat_CT[2]
    seq.ki <- unique(append(seq.ki, ki.max))
    
    min.coat <- 10^min(seq.coat)*fixed_coat
    max.coat <- 10^max(seq.coat)*fixed_coat
    min.lig <- 10^min(seq.lig)*fixed_ligand
    max.lig <- 10^max(seq.lig)*fixed_ligand
    min.rec <- 10^min(seq.rec)*fixed_receptor
    max.rec <- 10^max(seq.rec)*fixed_receptor
    rec.dots <- 10^seq.rec*fixed_receptor
    
    
    leggy <- c()
    my.df <- NULL
    mylist <- NULL
    
    for (ki.cnt in seq.ki) {
      this.ki <- 10^ki.cnt*kdcoating
      ## now each line
      occ.dots <- c()
      for (rec.cnt in seq.rec) {
        this.rec <- 10^rec.cnt*fixed_receptor
        gfun <- function(x) {c(this.rec - x[1] - x[2]*x[1]/this.ki - x[1]*x[3]/fixed_kd, 0-x[2]-x[2]*x[1]/this.ki, fixed_coat-x[3]-x[1]*x[3]/fixed_kd)}
        fred <- multiroot(gfun, start=c(this.rec/2,0,fixed_coat/2), positive=TRUE )
        
        c_free_receptor <- fred$root[1]
        c_free_coat <- fred$root[2]
        c_free_ligand <- fred$root[3]
        c_complex <- fixed_ligand - c_free_ligand
        pc_occ = 100*(1-c_free_ligand/fixed_ligand)
        occ.dots <- c(occ.dots, pc_occ)
        my.df <- (cbind(rec.dots, this.ki, occ.dots))
      }
      mylist <- data.frame(rbind(mylist, my.df))
    }
    
    if (input$p1concunit_CT == "metric units"){
      print("yes it does")
      rec.dots <- 10^seq.rec*input$c_total_receptor_CT_start
      this.ki <- 10^seq.lig*input$ki_CT
      occ.dots <- mylist[,3]
      
      temp <- NULL
      for (i in 1:length(this.ki)){
        temp[[i]] <- cbind(rec.dots, this.ki[i])
      }
      temp <- do.call(rbind.data.frame, temp)
      colnames(temp)[2] <- "this.ki"
      
      mylist <- cbind.data.frame(temp, occ.dots)
      # str(mylist)
      # print(mylist)
      
    } 
    
    # for (i in 1:nrow(mylist)){
    #   
    #   mylist$rec.dots[i] <- round(convert_units_back_p1_CT (mylist$rec.dots[i]),1)
    #   # mylist$this.coat[i] <- round(convert_units_back_coat (mylist$this.coat[i]),2)
    # }
    
    return(mylist)
  })
  
  
  #First Plot graph 2
  
  observeEvent( c(input$myconfirmation_submit_competition, input$myconfirmation_submit_competition_graph2), {
    if (!is.null (input$myconfirmation_submit_competition) | !is.null(input$myconfirmation_submit_competition_graph2) ){
      
      output$secondPlot_A_graph2 <- renderPlotly({ 
        isolate({ 
          # output$cxbyligrangerecplot <- renderPlotly({
          print(inputsecondPlot_A_graph2())
        })
      })
    }
  })
  
  
  inputsecondPlot_A_graph2  <- reactive ({  
    
    if (input$A_change_first_var_graph2 == input$ligand_name_CT){
      # print("kd p2")
      #complex by rec over ligand
      # print("complex by receptor over ligand")
      mylist <- roccbyrecrangeligdataone()
      mylist$this.lig <- round(mylist$this.lig,2)
      mylist_spread <- tidyr::spread(mylist, key= "this.lig", value =  "occ.dots")
      thinned_spread = mylist_spread[seq(1, nrow(mylist_spread), 1), ]
      thinned_spread <- round(thinned_spread, 2)
      thinned_gather <-  tidyr::gather(thinned_spread[ ,2:ncol(thinned_spread)], key= "this.lig", value =  "occ.dots")
      thinned_gather_new <- cbind(thinned_spread[ ,1], thinned_gather)
      names(thinned_gather_new)[1] <- 'rec.dots'
      
      
      q <- plot_ly (data = mylist,
                    x = ~rec.dots) %>%
        add_trace (y = ~occ.dots, mode = 'lines', color = ~factor(this.lig)) %>%
        add_trace (data = thinned_gather_new,
                   y = ~ occ.dots, mode = 'markers', color = ~factor(this.lig),
                   showlegend = F, showticklabels = FALSE) %>%
        layout(title = paste("Fractional occupancy of ", input$ligand_name_CT, 
                             "\n in function of ", em(input$receptor_name_CT),
                             " for a fixed ", em(input$A_selected_first_var_graph2), " of ", em(input$A_first_var_value_graph2),
                             " ", get_units(input$A_selected_first_var_graph2), sep=""),
               font = list(size = 9),
               # yaxis = list (title =paste("Concentration of ", input$coating_name_CT," (", 
               #                            get_units(input$coating_name_CT), ")", sep="")), 
               yaxis = list (title = paste("Fractional occupancy of ", input$ligand_name_CT,sep="")), 
               xaxis = list(type = "log", autotick = F,titlefont = list(size = 14),tickfont = list(size = 12),
                            title = paste("Concentration of ", input$receptor_name_CT,
                                          " (", get_units(input$receptor_name_CT), ")", sep="") ))
      
    } else if (input$A_change_first_var_graph2 == input$coating_name_CT){
      
      #complex by recepter over coat
      # print("kd comp")
      mylist <- roccbyrecrangecoatdataone()
      mylist$this.coat <- round(mylist$this.coat,2)
      mylist_spread <- tidyr::spread(mylist, key= "this.coat", value =  "occ.dots")
      thinned_spread = mylist_spread[seq(1, nrow(mylist_spread), 1), ]
      thinned_spread <- round(thinned_spread, 2)
      thinned_gather <-  tidyr::gather(thinned_spread[ ,2:ncol(thinned_spread)], key= "this.coat", value =  "occ.dots")
      thinned_gather_new <- cbind(thinned_spread[ ,1], thinned_gather)
      names(thinned_gather_new)[1] <- 'rec.dots'
      
      
      q <- plot_ly (data = mylist,
                    x = ~rec.dots) %>%
        add_trace (y = ~occ.dots, mode = 'lines', color = ~factor(this.coat)) %>%
        add_trace (data = thinned_gather_new,
                   y = ~ occ.dots, mode = 'markers', color = ~factor(this.coat),
                   showlegend = F, showticklabels = FALSE) %>%
        layout(title = paste("Fractional occupancy of ", input$ligand_name_CT, 
                             "\n in function of ", em(input$receptor_name_CT),
                             " for a fixed ", em(input$A_selected_first_var_graph2), " of ", em(input$A_first_var_value_graph2),
                             " ", get_units(input$A_selected_first_var_graph2), sep=""),
               font = list(size = 9),
               # yaxis = list (title =paste("Concentration of ", input$coating_name_CT," (",
               #                            get_units(input$coating_name_CT), ")", sep="")),
               yaxis = list (title = paste("Fractional occupancy of ", input$ligand_name_CT,sep="")), 
               xaxis = list(type = "log", autotick = F,titlefont = list(size = 14),tickfont = list(size = 12),
                            title = paste("Concentration of ", input$receptor_name_CT,
                                          " (", get_units(input$receptor_name_CT), ")", sep="") ))
      
      
    } else if (input$A_change_first_var_graph2 == "KD"){
      
      #complex by receptor over kd
      # print ("complex by receptor over kd")
      
      mylist <- roccbyrecrangekddataone()
      mylist$this.kd <- round(mylist$this.kd,2)
      mylist_spread <- tidyr::spread(mylist, key= "this.kd", value =  "occ.dots")
      thinned_spread = mylist_spread[seq(1, nrow(mylist_spread), 1), ]
      thinned_spread <- round(thinned_spread, 2)
      thinned_gather <-  tidyr::gather(thinned_spread[ ,2:ncol(thinned_spread)], key= "this.kd", value =  "occ.dots")
      thinned_gather_new <- cbind(thinned_spread[ ,1], thinned_gather)
      names(thinned_gather_new)[1] <- 'rec.dots'
      
      
      q <- plot_ly (data = mylist,
                    x = ~rec.dots) %>%
        add_trace (y = ~occ.dots, mode = 'lines', color = ~factor(this.kd)) %>%
        add_trace (data = thinned_gather_new,
                   y = ~ occ.dots, mode = 'markers', color = ~factor(this.kd),
                   showlegend = F, showticklabels = FALSE) %>%
        layout(title = paste("Fractional occupancy of ", input$ligand_name_CT, 
                             "\n in function of ", em(input$receptor_name_CT),
                             " for a fixed ", em(input$A_selected_first_var_graph2), " of ", em(input$A_first_var_value_graph2),
                             " ", get_units(input$A_selected_first_var_graph2), sep=""),
               font = list(size = 9),
               # yaxis = list (title =paste("Concentration of ", input$coating_name_CT," (",
               #                            get_units(input$coating_name_CT), ")", sep="")),
               yaxis = list (title = paste("Fractional occupancy of ", input$ligand_name_CT,sep="")), 
               xaxis = list(type = "log", autotick = F,titlefont = list(size = 14),tickfont = list(size = 12),
                            title = paste("Concentration of ", input$receptor_name_CT,
                                          " (", get_units(input$receptor_name_CT), ")", sep="") ))
      
    }else if (input$A_change_first_var_graph2 == "KI"){
      
      # #complex by receptor over kd
      # # print ("complex by receptor over ki")
      #
      mylist <-  roccbyrecrangekidataone()
      mylist$this.ki <- round(mylist$this.ki,2)
      mylist_spread <- tidyr::spread(mylist, key= "this.ki", value =  "occ.dots")
      thinned_spread = mylist_spread[seq(1, nrow(mylist_spread), 1), ]
      thinned_spread <- round(thinned_spread, 2)
      thinned_gather <-  tidyr::gather(thinned_spread[ ,2:ncol(thinned_spread)], key= "this.ki", value =  "occ.dots")
      thinned_gather_new <- cbind(thinned_spread[ ,1], thinned_gather)
      names(thinned_gather_new)[1] <- 'rec.dots'
      
      
      q <- plot_ly (data = mylist,
                    x = ~rec.dots) %>%
        add_trace (y = ~occ.dots, mode = 'lines', color = ~factor(this.ki)) %>%
        add_trace (data = thinned_gather_new,
                   y = ~ occ.dots, mode = 'markers', color = ~factor(this.ki),
                   showlegend = F, showticklabels = FALSE) %>%
        layout(title = paste("Fractional occupancy of ", input$ligand_name_CT, 
                             "\n in function of ", em(input$receptor_name_CT),
                             " for a fixed ", em(input$A_selected_first_var_graph2), " of ", em(input$A_first_var_value_graph2),
                             " ", get_units(input$A_selected_first_var_graph2), sep=""),
               font = list(size = 9),
               # yaxis = list (title =paste("Concentration of ", input$coating_name_CT," (",
               #                            get_units(input$coating_name_CT), ")", sep="")),
               yaxis = list (title = paste("Fractional occupancy of ", input$ligand_name_CT,sep="")), 
               xaxis = list(type = "log", autotick = F,titlefont = list(size = 14),tickfont = list(size = 12),
                            title = paste("Concentration of ", input$receptor_name_CT,
                                          " (", get_units(input$receptor_name_CT), ")", sep="") ))
      
    }
    
    if(input$A_change_first_var_graph2 == 'KD' | input$A_change_first_var_graph2 == 'KI' ){
      my.annotation <- paste(input$A_change_first_var_graph2, " (", get_units(input$A_change_first_var_graph2), ")", sep = "")
    }else{
      my.annotation <- paste("Concentration of <br>", input$A_change_first_var_graph2, " (",get_units(input$A_change_first_var_graph2), ")", sep = "")
    }
    
    
    # output$firstPlot_A_graph2 <- renderPlotly({ 
    
    if(input$scale_CT == 1){
      
      
      q %>%
        config(displaylogo = FALSE) %>%
        config(collaborate = FALSE) %>%
        config(displayModeBar = F) %>%
        config(
          edits = list(
            # annotationPosition = TRUE,
            # annotationTail = TRUE,
            # annotationText = TRUE
          ))%>%
        config(
          toImageButtonOptions = list(
            format = "png",
            width = 1000,
            height = 1000
          ))%>%
        layout(
          
          legend = list(
            orientation = "h",
            x = 0.225, y = -0.3,
            font = list(size = 12)
          )
        )%>%
        add_annotations( text= my.annotation, 
                         xref="paper", yref="paper",
                         x=0, xanchor="left",
                         y=-0.435, yanchor="bottom",    # Same y as legend below
                         legendtitle=F, showarrow=FALSE,
                         font=list(size=12,face="bold"))
      
      
    } else if (input$scale_CT == 2){
      # q <- q + scale_y_continuous(trans = "log10")
      
      q <- q %>% layout(yaxis = list(type = "log"))
      
      q %>%
        config(displaylogo = FALSE) %>%
        config(collaborate = FALSE) %>%
        config(displayModeBar = F) %>%
        layout(legend = list(
          orientation = "h",
          x = 0.225, y = -0.3,
          font = list(size = 12)
        )
        )%>%
        add_annotations( text= my.annotation, 
                         xref="paper", yref="paper",
                         x=0, xanchor="left",
                         y=-0.435, yanchor="bottom",    # Same y as legend below
                         legendtitle=TRUE, showarrow=FALSE,
                         font=list(size=12,face="bold"))
    }
    
    # })
    # }
  })
  
  #second plot graph 2
  
  observeEvent( c(input$myconfirmation_submit_competition, input$myconfirmation_submit_competition_graph2), {
    if (!is.null (input$myconfirmation_submit_competition) | !is.null(input$myconfirmation_submit_competition_graph2) ){
      
      output$secondPlot_B_graph2 <- renderPlotly({ 
        isolate({ 
          # output$cxbyligrangerecplot <- renderPlotly({
          print(inputsecondPlot_B_graph2())
        })
      })
    }
  })
  
  
  inputsecondPlot_B_graph2 <- reactive({ 
    
    if (input$A_change_first_var_graph2 == input$ligand_name_CT){
      # print("kd p2")
      #complex by rec over ligand
      # print("complex by receptor over ligand")
      mylist <- roccbyrecrangeligdatatwo ()
      mylist$this.lig <- round(mylist$this.lig,2)
      mylist_spread <- tidyr::spread(mylist, key= "this.lig", value =  "occ.dots")
      thinned_spread = mylist_spread[seq(1, nrow(mylist_spread), 1), ]
      thinned_spread <- round(thinned_spread, 2)
      thinned_gather <-  tidyr::gather(thinned_spread[ ,2:ncol(thinned_spread)], key= "this.lig", value =  "occ.dots")
      thinned_gather_new <- cbind(thinned_spread[ ,1], thinned_gather)
      names(thinned_gather_new)[1] <- 'rec.dots'
      
      
      q <- plot_ly (data = mylist,
                    x = ~rec.dots) %>%
        add_trace (y = ~occ.dots, mode = 'lines', color = ~factor(this.lig)) %>%
        add_trace (data = thinned_gather_new,
                   y = ~ occ.dots, mode = 'markers', color = ~factor(this.lig),
                   showlegend = F, showticklabels = FALSE) %>%
        layout(title = paste("Fractional occupancy of ", input$ligand_name_CT, 
                             "\n in function of ", em(input$receptor_name_CT),
                             " for a fixed ", em(input$A_selected_first_var_graph2), " of ", em(input$A_first_var_value2_graph2),
                             " ", get_units(input$A_selected_first_var_graph2), sep=""),
               font = list(size = 9),
               # yaxis = list (title =paste("Concentration of ", input$coating_name_CT," (", 
               #                            get_units(input$coating_name_CT), ")", sep="")), 
               yaxis = list (title = paste("Fractional occupancy of ", input$ligand_name_CT,sep="")), 
               xaxis = list(type = "log", autotick = F,titlefont = list(size = 14),tickfont = list(size = 12),
                            title = paste("Concentration of ", input$receptor_name_CT,
                                          " (", get_units(input$receptor_name_CT), ")", sep="") ))
      
    } else if (input$A_change_first_var_graph2 == input$coating_name_CT){
      
      #complex by recepter over coat
      # print("kd comp")
      mylist <- roccbyrecrangecoatdatatwo()
      mylist$this.coat <- round(mylist$this.coat,2)
      mylist_spread <- tidyr::spread(mylist, key= "this.coat", value =  "occ.dots")
      thinned_spread = mylist_spread[seq(1, nrow(mylist_spread), 1), ]
      thinned_spread <- round(thinned_spread, 2)
      thinned_gather <-  tidyr::gather(thinned_spread[ ,2:ncol(thinned_spread)], key= "this.coat", value =  "occ.dots")
      thinned_gather_new <- cbind(thinned_spread[ ,1], thinned_gather)
      names(thinned_gather_new)[1] <- 'rec.dots'
      
      
      q <- plot_ly (data = mylist,
                    x = ~rec.dots) %>%
        add_trace (y = ~occ.dots, mode = 'lines', color = ~factor(this.coat)) %>%
        add_trace (data = thinned_gather_new,
                   y = ~ occ.dots, mode = 'markers', color = ~factor(this.coat),
                   showlegend = F, showticklabels = FALSE) %>%
        layout(title = paste("Fractional occupancy of ", input$ligand_name_CT, 
                             "\n in function of ", em(input$receptor_name_CT),
                             " for a fixed ", em(input$A_selected_first_var_graph2), " of ", em(input$A_first_var_value2_graph2),
                             " ", get_units(input$A_selected_first_var_graph2), sep=""),
               font = list(size = 9),
               # yaxis = list (title =paste("Concentration of ", input$coating_name_CT," (",
               #                            get_units(input$coating_name_CT), ")", sep="")),
               yaxis = list (title = paste("Fractional occupancy of ", input$ligand_name_CT,sep="")), 
               xaxis = list(type = "log", autotick = F,titlefont = list(size = 14),tickfont = list(size = 12),
                            title = paste("Concentration of ", input$receptor_name_CT,
                                          " (", get_units(input$receptor_name_CT), ")", sep="") ))
      
    }else if (input$A_change_first_var_graph2 == "KD"){
      
      #complex by receptor over kd
      # print ("complex by receptor over kd")
      
      mylist <- roccbyrecrangekddatatwo()
      mylist$this.kd <- round(mylist$this.kd,2)
      mylist_spread <- tidyr::spread(mylist, key= "this.kd", value =  "occ.dots")
      thinned_spread = mylist_spread[seq(1, nrow(mylist_spread), 1), ]
      thinned_spread <- round(thinned_spread, 2)
      thinned_gather <-  tidyr::gather(thinned_spread[ ,2:ncol(thinned_spread)], key= "this.kd", value =  "occ.dots")
      thinned_gather_new <- cbind(thinned_spread[ ,1], thinned_gather)
      names(thinned_gather_new)[1] <- 'rec.dots'
      
      
      q <- plot_ly (data = mylist,
                    x = ~rec.dots) %>%
        add_trace (y = ~occ.dots, mode = 'lines', color = ~factor(this.kd)) %>%
        add_trace (data = thinned_gather_new,
                   y = ~ occ.dots, mode = 'markers', color = ~factor(this.kd),
                   showlegend = F, showticklabels = FALSE) %>%
        layout(title = paste("Fractional occupancy of ", input$ligand_name_CT, 
                             "\n in function of ", em(input$receptor_name_CT),
                             " for a fixed ", em(input$A_selected_first_var_graph2), " of ", em(input$A_first_var_value2_graph2),
                             " ", get_units(input$A_selected_first_var_graph2), sep=""),
               font = list(size = 9),
               # yaxis = list (title =paste("Concentration of ", input$coating_name_CT," (",
               #                            get_units(input$coating_name_CT), ")", sep="")),
               yaxis = list (title = paste("Fractional occupancy of ", input$ligand_name_CT,sep="")), 
               xaxis = list(type = "log", autotick = F,titlefont = list(size = 14),tickfont = list(size = 12),
                            title = paste("Concentration of ", input$receptor_name_CT,
                                          " (", get_units(input$receptor_name_CT), ")", sep="") ))
      
    }else if (input$A_change_first_var_graph2 == "KI"){
      
      # #complex by receptor over kd
      # # print ("complex by receptor over ki")
      #
      mylist <-  roccbyrecrangekidatatwo()
      mylist$this.ki <- round(mylist$this.ki,2)
      mylist_spread <- tidyr::spread(mylist, key= "this.ki", value =  "occ.dots")
      thinned_spread = mylist_spread[seq(1, nrow(mylist_spread), 1), ]
      thinned_spread <- round(thinned_spread, 2)
      thinned_gather <-  tidyr::gather(thinned_spread[ ,2:ncol(thinned_spread)], key= "this.ki", value =  "occ.dots")
      thinned_gather_new <- cbind(thinned_spread[ ,1], thinned_gather)
      names(thinned_gather_new)[1] <- 'rec.dots'
      
      
      q <- plot_ly (data = mylist,
                    x = ~rec.dots) %>%
        add_trace (y = ~occ.dots, mode = 'lines', color = ~factor(this.ki)) %>%
        add_trace (data = thinned_gather_new,
                   y = ~ occ.dots, mode = 'markers', color = ~factor(this.ki),
                   showlegend = F, showticklabels = FALSE) %>%
        layout(title = paste("Fractional occupancy of ", input$ligand_name_CT, 
                             "\n in function of ", em(input$receptor_name_CT),
                             " for a fixed ", em(input$A_selected_first_var_graph2), " of ", em(input$A_first_var_value2_graph2),
                             " ", get_units(input$A_selected_first_var_graph2), sep=""),
               font = list(size = 9),
               # yaxis = list (title =paste("Concentration of ", input$coating_name_CT," (",
               #                            get_units(input$coating_name_CT), ")", sep="")),
               yaxis = list (title = paste("Fractional occupancy of ", input$ligand_name_CT,sep="")), 
               xaxis = list(type = "log", autotick = F,titlefont = list(size = 14),tickfont = list(size = 12),
                            title = paste("Concentration of ", input$receptor_name_CT,
                                          " (", get_units(input$receptor_name_CT), ")", sep="") ))
      
    }
    
    
    if(input$A_change_first_var_graph2 == 'KD' | input$A_change_first_var_graph2 == 'KI' ){
      my.annotation <- paste(input$A_change_first_var_graph2, " (", get_units(input$A_change_first_var_graph2), ")", sep = "")
    }else{
      my.annotation <- paste("Concentration of <br>", input$A_change_first_var_graph2, " (",get_units(input$A_change_first_var_graph2), ")", sep = "")
    }
    
    
    # output$firstPlot_B_graph2 <- renderPlotly({ 
    
    if(input$scale_CT == 1){
      
      
      q %>%
        config(displaylogo = FALSE) %>%
        config(collaborate = FALSE) %>%
        config(displayModeBar = F) %>%
        config(
          edits = list(
            # annotationPosition = TRUE,
            # annotationTail = TRUE,
            # annotationText = TRUE
          ))%>%
        config(
          toImageButtonOptions = list(
            format = "png",
            width = 1000,
            height = 1000
          ))%>%
        layout(
          
          legend = list(
            orientation = "h",
            x = 0.225, y = -0.3,
            font = list(size = 12)
          )
        )%>%
        add_annotations( text= my.annotation, 
                         xref="paper", yref="paper",
                         x=0, xanchor="left",
                         y=-0.435, yanchor="bottom",    # Same y as legend below
                         legendtitle=F, showarrow=FALSE,
                         font=list(size=12,face="bold"))
      
      
    } else if (input$scale_CT == 2){
      # q <- q + scale_y_continuous(trans = "log10")
      
      q <- q %>% layout(yaxis = list(type = "log"))
      
      q %>%
        config(displaylogo = FALSE) %>%
        config(collaborate = FALSE) %>%
        config(displayModeBar = F) %>%
        layout(legend = list(
          orientation = "h",
          x = 0.225, y = -0.3,
          font = list(size = 12)
        )
        )%>%
        add_annotations( text= my.annotation, 
                         xref="paper", yref="paper",
                         x=0, xanchor="left",
                         y=-0.435, yanchor="bottom",    # Same y as legend below
                         legendtitle=TRUE, showarrow=FALSE,
                         font=list(size=12,face="bold"))
    }
    
    # })
    # }
  })  
  
  
  #First Table graph2
  
  inputTable1_graph2_CT  <- reactive ({ 
    
    if (input$A_change_first_var_graph2 == input$ligand_name_CT){
      
      # name_partner2 <- HTML('Concentration of ' ,input$A_change_first_var_graph2, "\n", " (", get_units(input$A_change_first_var_graph2), ")")
      # name_partner1 <- paste('Concentration of ', input$receptor_name_CT, "\n", " (", get_units(input$receptor_name_CT), ")")
      
      mylist <- roccbyrecrangeligdataone ()
      mylist$this.lig <- round(mylist$this.lig,2)
      mylist_spread <- tidyr::spread(mylist, key= "this.lig", value =  "occ.dots")
      thinned_spread = mylist_spread[seq(1, nrow(mylist_spread), 1), ]
      names(thinned_spread)[1] <- ""
      thinned_spread <- round(thinned_spread, 2)
      
    } else if( input$A_change_first_var_graph2 == input$coating_name_CT){
      
      # name_partner2 <- HTML('Concentration of ' ,input$A_change_first_var_graph2, "\n", " (", get_units(input$A_change_first_var_graph2), ")")
      # name_partner1 <- paste('Concentration of ', input$receptor_name_CT, "\n", " (", get_units(input$receptor_name_CT), ")")
      
      mylist <- roccbyrecrangecoatdataone() 
      mylist$this.coat <- round(mylist$this.coat,2)
      mylist_spread <- tidyr::spread(mylist, key= "this.coat", value =  "occ.dots")
      thinned_spread = mylist_spread[seq(1, nrow(mylist_spread), 1), ]
      names(thinned_spread)[1] <- ""
      thinned_spread <- round(thinned_spread, 2)
      
      
    }else if( input$A_change_first_var_graph2 == "KD"){
      # name_partner2 <- HTML('Concentration of ' ,input$A_change_first_var_graph2, "\n", " (", get_units(input$A_change_first_var_graph2), ")")
      # name_partner1 <- paste('Concentration of ', input$receptor_name_CT, "\n", " (", get_units(input$receptor_name_CT), ")")
      
      mylist <- roccbyrecrangekddataone() 
      mylist$this.kd <- round(mylist$this.kd,2)
      mylist_spread <- tidyr::spread(mylist, key= "this.kd", value =  "occ.dots")
      thinned_spread = mylist_spread[seq(1, nrow(mylist_spread), 1), ]
      names(thinned_spread)[1] <- ""
      thinned_spread <- round(thinned_spread, 2)
      
    }else if( input$A_change_first_var_graph2 == "KI"){
      # name_partner2 <- HTML('Concentration of ' ,input$A_change_first_var_graph2, "\n", " (", get_units(input$A_change_first_var_graph2), ")")
      # name_partner1 <- paste('Concentration of ', input$receptor_name_CT, "\n", " (", get_units(input$receptor_name_CT), ")")
      
      mylist <- roccbyrecrangekidataone() 
      mylist$this.ki <- round(mylist$this.ki,2)
      mylist_spread <- tidyr::spread(mylist, key= "this.ki", value =  "occ.dots")
      thinned_spread = mylist_spread[seq(1, nrow(mylist_spread), 1), ]
      names(thinned_spread)[1] <- ""
      thinned_spread <- round(thinned_spread, 2)
    }
    
  })
  
  
  
  observeEvent( c(input$myconfirmation_submit_competition, input$myconfirmation_submit_competition_graph2), {
    if (!is.null (input$myconfirmation_submit_competition) | !is.null(input$myconfirmation_submit_competition_graph2) ){
      
      output$firstTable_A_graph2 <- renderDataTable({
        isolate({ 
          
          if (input$A_change_first_var_graph2 == input$ligand_name_CT){
            
            name_partner2 <- HTML('Concentration of ' ,input$A_change_first_var_graph2, "\n", " (", get_units(input$A_change_first_var_graph2), ")")
            name_partner1 <- paste('Concentration of ', input$receptor_name_CT, "\n", " (", get_units(input$receptor_name_CT), ")")
            
          } else if( input$A_change_first_var_graph2 == input$coating_name_CT){
            
            name_partner2 <- HTML('Concentration of ' ,input$A_change_first_var_graph2, "\n", " (", get_units(input$A_change_first_var_graph2), ")")
            name_partner1 <- paste('Concentration of ', input$receptor_name_CT, "\n", " (", get_units(input$receptor_name_CT), ")")
            
            
          }else if( input$A_change_first_var_graph2 == "KD"){
            name_partner2 <- HTML(input$A_change_first_var_graph2, "\n", " (", get_units(input$A_change_first_var_graph2), ")")
            name_partner1 <- paste('Concentration of ', input$receptor_name_CT, "\n", " (", get_units(input$receptor_name_CT), ")")
            
            
          }else if( input$A_change_first_var_graph2 == "KI"){
            name_partner2 <- HTML(input$A_change_first_var_graph2, "\n", " (", get_units(input$A_change_first_var_graph2), ")")
            name_partner1 <- paste('Concentration of ', input$receptor_name_CT, "\n", " (", get_units(input$receptor_name_CT), ")")
            
          }
          
          thinned_spread <- inputTable1_graph2_CT()
          
          sketch <- createContainer(name_partner1, name_partner2,  thinned_spread)
          
          datatable(
            thinned_spread,
            extensions = 'Buttons',
            class = "cell-border stripe hover",
            caption = paste("Fractional occupancy of ", input$ligand_name_CT," (%)",sep=""),
            container = sketch,
            rownames = FALSE,
            
            options = list(
              pageLength = 7,
              dom = 'tprB',
              scrollX = TRUE,
              buttons = list(
                c('copy', 'excel'))
            )
          )%>%
            formatStyle(thinned_spread [ ,1], fontWeight = "bold")
          
        })
      })
    } 
    
  })
  
  
  #Second Table Graph 2
  inputTable2_graph2_CT  <- reactive ({     
    
    if (input$A_change_first_var_graph2 == input$ligand_name_CT){
      
      # name_partner2 <- HTML('Concentration of ' ,input$A_change_first_var_graph2, "\n", " (", get_units(input$A_change_first_var_graph2), ")")
      # name_partner1 <- paste('Concentration of ', input$receptor_name_CT, "\n", " (", get_units(input$receptor_name_CT), ")")
      
      mylist <- roccbyrecrangeligdatatwo()
      mylist$this.lig <- round(mylist$this.lig,2)
      mylist_spread <- tidyr::spread(mylist, key= "this.lig", value =  "occ.dots")
      thinned_spread = mylist_spread[seq(1, nrow(mylist_spread), 1), ]
      names(thinned_spread)[1] <- ""
      thinned_spread <- round(thinned_spread, 2)
      
    } else if( input$A_change_first_var_graph2 == input$coating_name_CT){
      
      # name_partner2 <- HTML('Concentration of ' ,input$A_change_first_var_graph2, "\n", " (", get_units(input$A_change_first_var_graph2), ")")
      # name_partner1 <- paste('Concentration of ', input$receptor_name_CT, "\n", " (", get_units(input$receptor_name_CT), ")")
      
      mylist <- roccbyrecrangecoatdatatwo()
      mylist$this.coat <- round(mylist$this.coat,2)
      mylist_spread <- tidyr::spread(mylist, key= "this.coat", value =  "occ.dots")
      thinned_spread = mylist_spread[seq(1, nrow(mylist_spread), 1), ]
      names(thinned_spread)[1] <- ""
      thinned_spread <- round(thinned_spread, 2)
      
      
    }else if( input$A_change_first_var_graph2 == "KD"){
      # name_partner2 <- HTML('Concentration of ' ,input$A_change_first_var_graph2, "\n", " (", get_units(input$A_change_first_var_graph2), ")")
      # name_partner1 <- paste('Concentration of ', input$receptor_name_CT, "\n", " (", get_units(input$receptor_name_CT), ")")
      
      mylist <- roccbyrecrangekddatatwo() 
      mylist$this.kd <- round(mylist$this.kd,2)
      mylist_spread <- tidyr::spread(mylist, key= "this.kd", value =  "occ.dots")
      thinned_spread = mylist_spread[seq(1, nrow(mylist_spread), 1), ]
      names(thinned_spread)[1] <- ""
      thinned_spread <- round(thinned_spread, 2)
      
    }else if( input$A_change_first_var_graph2 == "KI"){
      # name_partner2 <- HTML('Concentration of ' ,input$A_change_first_var_graph2, "\n", " (", get_units(input$A_change_first_var_graph2), ")")
      # name_partner1 <- paste('Concentration of ', input$receptor_name_CT, "\n", " (", get_units(input$receptor_name_CT), ")")
      
      mylist <- roccbyrecrangekidatatwo() 
      mylist$this.ki <- round(mylist$this.ki,2)
      mylist_spread <- tidyr::spread(mylist, key= "this.ki", value =  "occ.dots")
      thinned_spread = mylist_spread[seq(1, nrow(mylist_spread), 1), ]
      names(thinned_spread)[1] <- ""
      thinned_spread <- round(thinned_spread, 2)
    }
  })
  
  
  observeEvent( c(input$myconfirmation_submit_competition, input$myconfirmation_submit_competition_graph2), {
    if (!is.null (input$myconfirmation_submit_competition) | !is.null(input$myconfirmation_submit_competition_graph2) ){
      
      output$firstTable_B_graph2 <- renderDataTable({
        
        isolate({
          
          if (input$A_change_first_var_graph2 == input$ligand_name_CT){
            
            name_partner2 <- HTML('Concentration of ' ,input$A_change_first_var_graph2, "\n", " (", get_units(input$A_change_first_var_graph2), ")")
            name_partner1 <- paste('Concentration of ', input$receptor_name_CT, "\n", " (", get_units(input$receptor_name_CT), ")")
            
          } else if( input$A_change_first_var_graph2 == input$coating_name_CT){
            
            name_partner2 <- HTML('Concentration of ' ,input$A_change_first_var_graph2, "\n", " (", get_units(input$A_change_first_var_graph2), ")")
            name_partner1 <- paste('Concentration of ', input$receptor_name_CT, "\n", " (", get_units(input$receptor_name_CT), ")")
            
            
          }else if( input$A_change_first_var_graph2 == "KD"){
            name_partner2 <- HTML(input$A_change_first_var_graph2, "\n", " (", get_units(input$A_change_first_var_graph2), ")")
            name_partner1 <- paste('Concentration of ', input$receptor_name_CT, "\n", " (", get_units(input$receptor_name_CT), ")")
            
            
          }else if( input$A_change_first_var_graph2 == "KI"){
            name_partner2 <- HTML(input$A_change_first_var_graph2, "\n", " (", get_units(input$A_change_first_var_graph2), ")")
            name_partner1 <- paste('Concentration of ', input$receptor_name_CT, "\n", " (", get_units(input$receptor_name_CT), ")")
            
          }
          
          thinned_spread <- inputTable2_graph2_CT()
          
          sketch <- createContainer(name_partner1, name_partner2,  thinned_spread)
          
          datatable(
            thinned_spread,
            extensions = 'Buttons',
            class = "cell-border stripe hover",
            caption = paste("Fractional occupancy of ", input$ligand_name_CT," (%)",sep=""),
            container = sketch,
            rownames = FALSE,
            
            options = list(
              pageLength = 7,
              dom = 'tprB',
              scrollX = TRUE,
              buttons = list(
                c('copy', 'excel'))
            )
          )%>%
            formatStyle(thinned_spread [ ,1], fontWeight = "bold")
          
        })
      })
    }
  })
  

}#end server
