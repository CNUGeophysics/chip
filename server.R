server <- function(input, output, session){
  # load default DB ---------------------------
  load("RData/OrgRData/metCode.RData")
  load("RData/OrgRData/ref.RData")
  load("RData/OrgRData/spl.RData")
  load("RData/OrgRData/age.RData")  
  load("RData/OrgRData/value_ar.RData")
  load("RData/OrgRData/value_cosmo.RData")
  load("RData/OrgRData/value_kar.RData")
  load("RData/OrgRData/value_luhf.RData")
  load("RData/OrgRData/value_osl.RData")
  load("RData/OrgRData/value_smnd.RData")
  load("RData/OrgRData/value_upb.RData")
  # load custom DB ---------------------
  if(file.exists("RData/UserRData/ref_custom.RData")){
    load("RData/UserRData/ref_custom.RData")
    load("RData/UserRData/spl_custom.RData")
    load("RData/UserRData/age_custom.RData")
  }
  




  
  # ??
  decimal <- function(data){
    
    data <- na.omit(data)
    
    num <- data.frame()
    
    for(i in 1:nrow(data)){
      
      a <- as.character(data[i,1])
      a <- gsub("-", "", a)
      a <- as.numeric(a)
    
      decimalplaces <- function(a) {
        
        if ((a %% 1) != 0) {
          
          nchar(strsplit(sub("0+$", "", as.character(a)), ".", fixed=TRUE)[[1]][[2]])
          
        }else{
          
          return(0)
          
        }
      }
      
      num_temp <- decimalplaces(a)
      
      names(num_temp) <- c("NUM")
      
      num <- rbind(num, num_temp)
      
    }

    max(num)
  }
  
  # insert about html-------------------------------------------------------------------------
  
output$abt <- renderUI({
  
  includeHTML("www/About.html")
  
})


# Reference ---------------------------------------------------------------
  ## Reference Method -------------------------------------------------------------------------
  
  refmetdata <- reactive({
    
    load("RData/OrgRData/age.RData")  
    load("RData/OrgRData/ref.RData") 
    
    if(file.exists("RData/UserRData/ref_custom.RData")){
      load("RData/UserRData/ref_custom.RData")
      load("RData/UserRData/age_custom.RData")
      
      if(nrow(ref_custom) == 0){
        ref
      }else if(nrow(ref_custom) != 0){
        ref <- rbind(ref, ref_custom)
      }
      
      if(nrow(age_custom) == 0){
        age
      }else if(nrow(age_custom) != 0){
        age <- rbind(age, age_custom)
      }
    }

    age <- join(age, met_cd, by = "Method_cd")
    age <- age[,c(1,3,11)]
    
    age <- data.table(unique(age))
    
    ref <- join(ref, age, by = "RefID")
    
    ref <- ref[ref$Method %in% input$refmet, c(1:10)]
    
  })
  
  
  output$refmettbl <- DT::renderDataTable(
    refmetdata(),
    rownames = F,
    
    selection = "single",
    
    options = list(scrollX = T,
                   autoWidth = T,
                   columnDefs = list(list(width = '500px', targets = c(1)),
                                     list(width = '250px', targets = c(2)),
                                     list(width = '300px', targets = c(3:5)),
                                     list(visible = F, targets = c(0)))
    )
  )
  
  output$cntref_met_ref <- renderUI({
    paste("Reference:", nrow(refmetdata()))
  })
  
  subrefmetdata <- reactive({
    
    load("RData/OrgRData/spl.RData") 
    load("RData/OrgRData/metCode.RData")
    
    if(file.exists("RData/UserRData/spl_custom.RData")){
      load("RData/UserRData/spl_custom.RData")
      load("RData/UserRData/age_custom.RData")
      if(nrow(spl_custom) == 0){
        spl
      }else if(nrow(spl_custom) != 0){
        spl <- rbind(spl, spl_custom)
      } 
      
      if(nrow(age_custom) == 0){
        age
      }else if(nrow(age_custom) != 0){
        age <- rbind(age, age_custom)
      }
    }
    
    selected_row <- input$refmettbl_rows_selected
    selected_sample <- refmetdata()[as.integer(selected_row),]$RefID

    if(length(selected_sample)){
      spl$Longitude <- format(spl$Longitude, digits = 4, nsmall = 4)
      spl$Latitude <- format(spl$Latitude, digits = 4, nsmall = 4)
      
      spl <- join(spl, age, by = c("RefID", "Sample"))
      
      spl <- spl[,-c(8)]
      
      spl <- spl %>% 
        mutate(spl, ID = paste0(spl$RefID, spl$Sample_seq, spl$Method_cd))
      
      spl <- join(spl, met_cd, by = "Method_cd")

      spl <- spl[spl$Method %in% input$refmet,]
      
      spl <- spl[spl$RefID %in% selected_sample, ]
    }
    
  })
  
  output$subrefmettbl <- renderDataTable(
    subrefmetdata(),
    rownames = F,
    selection = "single",
    options = list(columnDefs = list(list(visible = F, targets = c(0,1,7:15))))
    )
  
  output$cntref_met_spl <- renderUI({
    if(length(subrefmetdata())){
      paste("Sample:", nrow(subrefmetdata()))  
    }
  })
  
  subrefmetval <- reactive({
    sub_selected_row <- input$subrefmettbl_rows_selected
    sub_selected_sample <- subrefmetdata()[as.integer(sub_selected_row),]$ID
  })
  
  subrefmetvaldata <- reactive({
    
    if(length(subrefmetval())){
      
      if(substr(subrefmetval(), nchar(subrefmetval())-2, nchar(subrefmetval())) == "001"){
        
        # method 001 --------------------------------------------------------------
        
        load("RData/OrgRData/value_ar.RData")
        if(file.exists("RData/UserRData/value_ar_custom.RData")){
          load("RData/UserRData/value_ar_custom.RData")
          value_ar <- c(value_ar, value_ar_custom)
        }
        
        df_temp<- as.data.frame(value_ar[[paste0(subrefmetval(), ".xlsx")]][-c(1)])
        
        if("Plateau.Age" %in% names(df_temp) ==T){
          df_temp$Plateau.Age <- df_temp$Plateau.Age*0.001 
          df_temp$Plateau.Age <- format(round(df_temp$Plateau.Age, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Plateau.Age"] = "Plateau.Age(Ka)"
        }
        
        if("Plateau.Age.SD" %in% names(df_temp) ==T){
          df_temp$Plateau.Age.SD <- df_temp$Plateau.Age.SD*0.001 
          df_temp$Plateau.Age.SD <- format(round(df_temp$Plateau.Age.SD, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Plateau.Age.SD"] = "Plateau.Age.SD(Ka)"
        }
        
        if("Intergrated.Age" %in% names(df_temp) ==T){
          df_temp$Intergrated.Age <- df_temp$Intergrated.Age*0.001 
          df_temp$Intergrated.Age <- format(round(df_temp$Intergrated.Age, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Intergrated.Age"] = "Intergrated.Age(Ka)"
        }
        
        if("Intergrated.Age.SD" %in% names(df_temp) ==T){
          df_temp$Intergrated.Age.SD <- df_temp$Intergrated.Age.SD*0.001 
          df_temp$Intergrated.Age.SD <- format(round(df_temp$Intergrated.Age.SD, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Intergrated.Age.SD"] = "Intergrated.Age.SD(Ka)"
        }
        
        df <- df_temp
        
      }else if(substr(subrefmetval(), nchar(subrefmetval())-2, nchar(subrefmetval())) == "002"){
        
        # method 002 --------------------------------------------------------------
        
        load("RData/OrgRData/value_cosmo.RData")
        if(file.exists("RData/UserRData/value_cosmo_custom.RData")){
          load("RData/UserRData/value_cosmo_custom.RData")
          value_cosmo <- c(value_cosmo, value_cosmo_custom)
        }
        
        df_temp<- as.data.frame(value_cosmo[[paste0(subrefmetval(), ".xlsx")]][-c(1)])
        
        if("Age" %in% names(df_temp) ==T){
          df_temp$Age <- df_temp$Age*0.001
          names(df_temp)[names(df_temp) == "Age"] = "Age(Ka)"
        }
        
        if("Age.PM" %in% names(df_temp) ==T){
          df_temp$Age.PM <- df_temp$Age.PM*0.001
          names(df_temp)[names(df_temp) == "Age.PM"] = "Age.PM(Ka)"
        }
        
        df <- df_temp
        
      }else if(substr(subrefmetval(), nchar(subrefmetval())-2, nchar(subrefmetval())) == "003"){
        
        # method 003 --------------------------------------------------------------
        
        load("RData/OrgRData/value_kar.RData")
        if(file.exists("RData/UserRData/value_kar_custom.RData")){
          load("RData/UserRData/value_kar_custom.RData")
          value_kar <- c(value_kar, value_kar_custom)
        }
        
        df_temp<- as.data.frame(value_kar[[paste0(subrefmetval(), ".xlsx")]][-c(1)])
        
        if("Delta.Radiogenic.Ar40" %in% names(df_temp) == T){
          df_temp$Delta.Radiogenic.Ar40 <- format(df_temp$Delta.Radiogenic.Ar40, scientific = F) 
        }
        
        if("Radiogenic.Ar36" %in% names(df_temp) == T){
          df_temp$Radiogenic.Ar36 <- format(df_temp$Radiogenic.Ar36, scientific = F)
        }
        
        if("Delta.Radiogenic.Ar36" %in% names(df_temp) == T){
          df_temp$Delta.Radiogenic.Ar36 <- format(df_temp$Delta.Radiogenic.Ar36, scientific = F)
        }
        
        if("Age" %in% names(df_temp) ==T){
          df_temp$Age <- df_temp$Age*0.000001
          names(df_temp)[names(df_temp) == "Age"] = "Age(Ma)"
        }
        
        if("Age.PM" %in% names(df_temp) ==T){
          df_temp$Age.PM <- df_temp$Age.PM*0.000001
          names(df_temp)[names(df_temp) == "Age.PM"] = "Age.PM(Ma)"
        }
        
        df <- df_temp
        
      }else if(substr(subrefmetval(), nchar(subrefmetval())-2, nchar(subrefmetval())) == "004"){
        
        # method 004 --------------------------------------------------------------
        
        load("RData/OrgRData/value_luhf.RData")
        if(file.exists("RData/UserRData/value_luhf_custom.RData")){
          load("RData/UserRData/value_luhf_custom.RData")
          value_luhf <- c(value_luhf, value_luhf_custom)
        }
        
        df_temp<- as.data.frame(value_luhf[[paste0(subrefmetval(), ".xlsx")]][-c(1)])
        
        if("Age" %in% names(df_temp) ==T){
          df_temp$Age <- df_temp$Age*0.000001
          names(df_temp)[names(df_temp) == "Age"] = "Age(Ma)"
        }
        
        if("TDM" %in% names(df_temp) ==T){
          df_temp$TDM <- df_temp$TDM*0.000001
          names(df_temp)[names(df_temp) == "TDM"] = "TDM(Ma)"
        }
        
        if("TDMc" %in% names(df_temp) ==T){
          df_temp$TDMc <- df_temp$TDMc*0.000001
          names(df_temp)[names(df_temp) == "TDMc"] = "TDMc(Ma)"
        }
        
        if("TDM2" %in% names(df_temp) ==T){
          df_temp$TDM2 <- df_temp$TDM2*0.000001
          names(df_temp)[names(df_temp) == "TDM2"] = "TDM2(Ma)"
        }
        
        df <- df_temp
        
      }else if(substr(subrefmetval(), nchar(subrefmetval())-2, nchar(subrefmetval())) == "005"){
        
        # method 005 --------------------------------------------------------------
        
        load("RData/OrgRData/value_osl.RData")
        if(file.exists("RData/UserRData/value_osl_custom.RData")){
          load("RData/UserRData/value_osl_custom.RData")
          value_osl <- c(value_osl, value_osl_custom)
        }
        
        df_temp<- as.data.frame(value_osl[[paste0(subrefmetval(), ".xlsx")]][-c(1)])
        
        if("Age" %in% names(df_temp) ==T){
          df_temp$Age <- df_temp$Age*0.001
          names(df_temp)[names(df_temp) == "Age"] = "Age(Ka)"
        }
        
        if("Age.PM" %in% names(df_temp) ==T){
          df_temp$Age.PM <- df_temp$Age.PM*0.001
          names(df_temp)[names(df_temp) == "Age.PM"] = "Age.PM(Ka)"
        }
        
        if("Age.SD" %in% names(df_temp) ==T){
          df_temp$Age.SD <- df_temp$Age.SD*0.001
          names(df_temp)[names(df_temp) == "Age.SD"] = "Age.SD(Ka)"
        }
        
        if("Age.SE" %in% names(df_temp) ==T){
          df_temp$Age.SE <- df_temp$Age.SE*0.001
          names(df_temp)[names(df_temp) == "Age.SE"] = "Age.SE(Ka)"
        }
        
        df <- df_temp
        
      }else if(substr(subrefmetval(), nchar(subrefmetval())-2, nchar(subrefmetval())) == "006"){
        
        # method 006 --------------------------------------------------------------
        
        load("RData/OrgRData/value_smnd.RData")
        if(file.exists("RData/UserRData/value_smnd_custom.RData")){
          load("RData/UserRData/value_smnd_custom.RData")
          value_smnd <- c(value_smnd, value_smnd_custom)
        }
        
        df_temp<- as.data.frame(value_smnd[[paste0(subrefmetval(), ".xlsx")]][-c(1)])
        
        if("TDM" %in% names(df_temp) ==T){
          df_temp$TDM <- df_temp$TDM*0.000001
          names(df_temp)[names(df_temp) == "TDM"] = "TDM(Ma)"
        }
        
        if("TDM2" %in% names(df_temp) ==T){
          df_temp$TDM2 <- df_temp$TDM2*0.000001
          names(df_temp)[names(df_temp) == "TDM2"] = "TDM2(Ma)"
        }
        
        df <- df_temp
        
      }else if(substr(subrefmetval(), nchar(subrefmetval())-2, nchar(subrefmetval())) == "007"){
        
        # method 007 --------------------------------------------------------------
        load("RData/OrgRData/value_upb.RData")
        if(file.exists("RData/UserRData/value_upb_custom.RData")){
          load("RData/UserRData/value_upb_custom.RData")  
          value_upb <- c(value_upb, value_upb_custom)
        }
        
        df_temp<- as.data.frame(value_upb[[paste0(subrefmetval(), ".xlsx")]][-c(1)])
        
        if("U" %in% names(df_temp) ==T){
          df_temp$U <- format(round(df_temp$U, digits = 0), nsmall = 0, scientific = F)
        }
        
        if("Th" %in% names(df_temp) ==T){
          df_temp$Th <- format(round(df_temp$Th, digits = 0), nsmall = 0, scientific = F)
        }
        
        if("Th_U" %in% names(df_temp) ==T){
          df_temp$Th_U <- format(round(df_temp$Th_U, digits = 2), nsmall = 2, scientific = F)
        }
        
        if("Th_U.Per" %in% names(df_temp) ==T){
          df_temp$Th_U.Per <- format(round(df_temp$Th_U.Per, digits = 2), nsmall = 2, scientific = F)
        }
        
        if("Th_U.PM.Per" %in% names(df_temp) ==T){
          df_temp$Th_U.PM.Per <- format(round(df_temp$Th_U.PM.Per, digits = 2), nsmall = 2, scientific = F)
        }
        
        ## pb series ---------------------------------------------------------------
        
        if("Pb" %in% names(df_temp) ==T){
          df_temp$Pb <- format(round(df_temp$Pb, digits = 2), nsmall = 2, scientific = F)
        }
        
        if("Pb206" %in% names(df_temp) ==T){
          df_temp$Pb206 <- format(round(df_temp$Pb206, digits = 2), nsmall = 2, scientific = F)
        }
        
        if("Pb206.PM.Per" %in% names(df_temp) ==T){
          df_temp$Pb206.PM.Per <- format(round(df_temp$Pb206.PM.Per, digits = 2), nsmall = 2, scientific = F)
        }
        
        if("Pbc" %in% names(df_temp) ==T){
          df_temp$Pbc <- format(round(df_temp$Pbc, digits = 2), nsmall = 2, scientific = F)
        }
        
        if("Pbr" %in% names(df_temp) ==T){
          df_temp$Pbr <- format(round(df_temp$Pbr, digits = 2), nsmall = 2, scientific = F)
        }
        
        
        # Pb204 -------------------------------------------------------------------
        
        if("Pb204_Pb206" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb204_Pb206)) >= 5){
            df_temp$Pb204_Pb206 <- format(round(df_temp$Pb204_Pb206, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb204_Pb206)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb204_Pb206)))
            df_temp$Pb204_Pb206 <- format(round(df_temp$Pb204_Pb206,  digits = num), nsmall = num)
          }}
        
        if("Pb204_Pb206.PM" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb204_Pb206.PM)) >= 5){
            df_temp$Pb204_Pb206.PM <- format(round(df_temp$Pb204_Pb206.PM, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb204_Pb206.PM)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb204_Pb206.PM)))
            df_temp$Pb204_Pb206.PM <- format(round(df_temp$Pb204_Pb206.PM,  digits = num), nsmall = num)
          }}
        
        if("Pb204_Pb206.PM.Per" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb204_Pb206.PM.Per)) >= 5){
            df_temp$Pb204_Pb206.PM.Per <- format(round(df_temp$Pb204_Pb206.PM.Per, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb204_Pb206.PM.Per)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb204_Pb206.PM.Per)))
            df_temp$Pb204_Pb206.PM.Per <- format(round(df_temp$Pb204_Pb206.PM.Per,  digits = num), nsmall = num)
          }}
        
        # Pb206 -------------------------------------------------------------------
        
        if("Pb206_Th232" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb206_Th232)) >= 5){
            df_temp$Pb206_Th232 <- format(round(df_temp$Pb206_Th232, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb206_Th232)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb206_Th232)))
            df_temp$Pb206_Th232 <- format(round(df_temp$Pb206_Th232,  digits = num), nsmall = num)
          }}
        
        
        if("Pb206_Th232.SD" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb206_Th232.SD)) >= 5){
            df_temp$Pb206_Th232.SD <- format(round(df_temp$Pb206_Th232.SD, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb206_Th232.SD)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb206_Th232.SD)))
            df_temp$Pb206_Th232.SD <- format(round(df_temp$Pb206_Th232.SD,  digits = num), nsmall = num)
          }}
        
        if("Pb206_U238" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb206_U238)) >= 5){
            df_temp$Pb206_U238 <- format(round(df_temp$Pb206_U238, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb206_U238)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb206_U238)))
            df_temp$Pb206_U238 <- format(round(df_temp$Pb206_U238,  digits = num), nsmall = num)
          }}
        
        if("Pb206_U238.PM" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb206_U238.PM)) >= 5){
            df_temp$Pb206_U238.PM <- format(round(df_temp$Pb206_U238.PM, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb206_U238.PM)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb206_U238.PM)))
            df_temp$Pb206_U238.PM <- format(round(df_temp$Pb206_U238.PM,  digits = num), nsmall = num)
          }}
        
        if("Pb206_U238.PM.Per" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb206_U238.PM.Per)) >= 5){
            df_temp$Pb206_U238.PM.Per <- format(round(df_temp$Pb206_U238.PM.Per, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb206_U238.PM.Per)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb206_U238.PM.Per)))
            df_temp$Pb206_U238.PM.Per <- format(round(df_temp$Pb206_U238.PM.Per,  digits = num), nsmall = num)
          }}
        
        if("Pb206_U238.SD" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb206_U238.SD)) >= 5){
            df_temp$Pb206_U238.SD <- format(round(df_temp$Pb206_U238.SD, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb206_U238.SD)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb206_U238.SD)))
            df_temp$Pb206_U238.SD <- format(round(df_temp$Pb206_U238.SD,  digits = num), nsmall = num)
          }}
        
        if("Pb206_U238.2SD" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb206_U238.2SD)) >= 5){
            df_temp$Pb206_U238.2SD <- format(round(df_temp$Pb206_U238.2SD, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb206_U238.2SD)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb206_U238.2SD)))
            df_temp$Pb206_U238.2SD <- format(round(df_temp$Pb206_U238.2SD,  digits = num), nsmall = num)
          }}
        
        if("Pb206_U238.2SE" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb206_U238.2SE)) >= 5){
            df_temp$Pb206_U238.2SE <- format(round(df_temp$Pb206_U238.2SE, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb206_U238.2SE)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb206_U238.2SE)))
            df_temp$Pb206_U238.2SE <- format(round(df_temp$Pb206_U238.2SE,  digits = num), nsmall = num)
          }}
        
        if("Pbr206_U238" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pbr206_U238)) >= 5){
            df_temp$Pbr206_U238 <- format(round(df_temp$Pbr206_U238, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pbr206_U238)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pbr206_U238)))
            df_temp$Pbr206_U238 <- format(round(df_temp$Pbr206_U238,  digits = num), nsmall = num)
          }}
        
        if("Pbr206_U238.PM" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pbr206_U238.PM)) >= 5){
            df_temp$Pbr206_U238.PM <- format(round(df_temp$Pbr206_U238.PM, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pbr206_U238.PM)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pbr206_U238.PM)))
            df_temp$Pbr206_U238.PM <- format(round(df_temp$Pbr206_U238.PM,  digits = num), nsmall = num)
          }}
        
        if("Pbr206_U238.PM.Per" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pbr206_U238.PM.Per)) >= 5){
            df_temp$Pbr206_U238.PM.Per <- format(round(df_temp$Pbr206_U238.PM.Per, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pbr206_U238.PM.Per)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pbr206_U238.PM.Per)))
            df_temp$Pbr206_U238.PM.Per <- format(round(df_temp$Pbr206_U238.PM.Per,  digits = num), nsmall = num)
          }}
        
        # U238 --------------------------------------------------------------------
        
        if("U238_Pb206" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$U238_Pb206)) >= 5){
            df_temp$U238_Pb206 <- format(round(df_temp$U238_Pb206, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$U238_Pb206)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$U238_Pb206)))
            df_temp$U238_Pb206 <- format(round(df_temp$U238_Pb206,  digits = num), nsmall = num)
          }}
        
        
        if("U238_Pb206.Per" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$U238_Pb206.Per)) >= 5){
            df_temp$U238_Pb206.Per <- format(round(df_temp$U238_Pb206.Per, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$U238_Pb206.Per)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$U238_Pb206.Per)))
            df_temp$U238_Pb206.Per <- format(round(df_temp$U238_Pb206.Per,  digits = num), nsmall = num)
          }}
        
        if("U238_Pb206.PM" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$U238_Pb206.PM)) >= 5){
            df_temp$U238_Pb206.PM <- format(round(df_temp$U238_Pb206.PM, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$U238_Pb206.PM)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$U238_Pb206.PM)))
            df_temp$U238_Pb206.PM <- format(round(df_temp$U238_Pb206.PM,  digits = num), nsmall = num)
          }}
        
        if("U238_Pb206.PM.Per" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$U238_Pb206.PM.Per)) >= 5){
            df_temp$U238_Pb206.PM.Per <- format(round(df_temp$U238_Pb206.PM.Per, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$U238_Pb206.PM.Per)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$U238_Pb206.PM.Per)))
            df_temp$U238_Pb206.PM.Per <- format(round(df_temp$U238_Pb206.PM.Per,  digits = num), nsmall = num)
          }}
        
        if("U238_Pb206.SD" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$U238_Pb206.SD)) >= 5){
            df_temp$U238_Pb206.SD <- format(round(df_temp$U238_Pb206.SD, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$U238_Pb206.SD)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$U238_Pb206.SD)))
            df_temp$U238_Pb206.SD <- format(round(df_temp$U238_Pb206.SD,  digits = num), nsmall = num)
          }}
        
        if("U238_Pbr206" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$U238_Pbr206)) >= 5){
            df_temp$U238_Pbr206 <- format(round(df_temp$U238_Pbr206, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$U238_Pbr206)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$U238_Pbr206)))
            df_temp$U238_Pbr206 <- format(round(df_temp$U238_Pbr206,  digits = num), nsmall = num)
          }}
        
        
        if("U238_Pbr206.PM" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$U238_Pbr206.PM)) >= 5){
            df_temp$U238_Pbr206.PM <- format(round(df_temp$U238_Pbr206.PM, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$U238_Pbr206.PM)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$U238_Pbr206.PM)))
            df_temp$U238_Pbr206.PM <- format(round(df_temp$U238_Pbr206.PM,  digits = num), nsmall = num)
          }}
        
        if("U238_Pbr206.PM.Per" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$U238_Pbr206.PM.Per)) >= 5){
            df_temp$U238_Pbr206.PM.Per <- format(round(df_temp$U238_Pbr206.PM.Per, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$U238_Pbr206.PM.Per)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$U238_Pbr206.PM.Per)))
            df_temp$U238_Pbr206.PM.Per <- format(round(df_temp$U238_Pbr206.PM.Per,  digits = num), nsmall = num)
          }}
        
        # Pb207 -------------------------------------------------------------------
        
        if("Pb207_Pb206" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb207_Pb206)) >= 5){
            df_temp$Pb207_Pb206 <- format(round(df_temp$Pb207_Pb206, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb207_Pb206)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb207_Pb206)))
            df_temp$Pb207_Pb206 <- format(round(df_temp$Pb207_Pb206,  digits = num), nsmall = num)
          }}
        
        if("Pb207_Pb206.Per" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb207_Pb206.Per)) >= 5){
            df_temp$Pb207_Pb206.Per <- format(round(df_temp$Pb207_Pb206.Per, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb207_Pb206.Per)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb207_Pb206.Per)))
            df_temp$Pb207_Pb206.Per <- format(round(df_temp$Pb207_Pb206.Per,  digits = num), nsmall = num)
          }}
        
        if("Pb207_Pb206.PM" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb207_Pb206.PM)) >= 5){
            df_temp$Pb207_Pb206.PM <- format(round(df_temp$Pb207_Pb206.PM, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb207_Pb206.PM)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb207_Pb206.PM)))
            df_temp$Pb207_Pb206.PM <- format(round(df_temp$Pb207_Pb206.PM,  digits = num), nsmall = num)
          }}
        
        if("Pb207_Pb206.PM.Per" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb207_Pb206.PM.Per)) >= 5){
            df_temp$Pb207_Pb206.PM.Per <- format(round(df_temp$Pb207_Pb206.PM.Per, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb207_Pb206.PM.Per)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb207_Pb206.PM.Per)))
            df_temp$Pb207_Pb206.PM.Per <- format(round(df_temp$Pb207_Pb206.PM.Per,  digits = num), nsmall = num)
          }}
        
        if("Pb207_Pb206.SD" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb207_Pb206.SD)) >= 5){
            df_temp$Pb207_Pb206.SD <- format(round(df_temp$Pb207_Pb206.SD, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb207_Pb206.SD)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb207_Pb206.SD)))
            df_temp$Pb207_Pb206.SD <- format(round(df_temp$Pb207_Pb206.SD,  digits = num), nsmall = num)
          }}
        
        if("Pb207_Pb206.2SD" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb207_Pb206.2SD)) >= 5){
            df_temp$Pb207_Pb206.2SD <- format(round(df_temp$Pb207_Pb206.2SD, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb207_Pb206.2SD)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb207_Pb206.2SD)))
            df_temp$Pb207_Pb206.2SD <- format(round(df_temp$Pb207_Pb206.2SD,  digits = num), nsmall = num)
          }}
        
        if("Pb207_Pb206.2SE" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb207_Pb206.2SE)) >= 5){
            df_temp$Pb207_Pb206.2SE <- format(round(df_temp$Pb207_Pb206.2SE, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb207_Pb206.2SE)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb207_Pb206.2SE)))
            df_temp$Pb207_Pb206.2SE <- format(round(df_temp$Pb207_Pb206.2SE,  digits = num), nsmall = num)
          }}
        
        if("Pbr207_Pb206" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pbr207_Pb206)) >= 5){
            df_temp$Pbr207_Pb206 <- format(round(df_temp$Pbr207_Pb206, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pbr207_Pb206)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pbr207_Pb206)))
            df_temp$Pbr207_Pb206 <- format(round(df_temp$Pbr207_Pb206,  digits = num), nsmall = num)
          }}
        
        if("Pbr207_Pb206.PM" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pbr207_Pb206.PM)) >= 5){
            df_temp$Pbr207_Pb206.PM <- format(round(df_temp$Pbr207_Pb206.PM, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pbr207_Pb206.PM)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pbr207_Pb206.PM)))
            df_temp$Pbr207_Pb206.PM <- format(round(df_temp$Pbr207_Pb206.PM,  digits = num), nsmall = num)
          }}
        
        if("Pbr207_Pb206.PM.Per" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pbr207_Pb206.PM.Per)) >= 5){
            df_temp$Pbr207_Pb206.PM.Per <- format(round(df_temp$Pbr207_Pb206.PM.Per, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pbr207_Pb206.PM.Per)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pbr207_Pb206.PM.Per)))
            df_temp$Pbr207_Pb206.PM.Per <- format(round(df_temp$Pbr207_Pb206.PM.Per,  digits = num), nsmall = num)
          }}
        
        if("Pbr207_Pbr206" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pbr207_Pbr206)) >= 5){
            df_temp$Pbr207_Pbr206 <- format(round(df_temp$Pbr207_Pbr206, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pbr207_Pbr206)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pbr207_Pbr206)))
            df_temp$Pbr207_Pbr206 <- format(round(df_temp$Pbr207_Pbr206,  digits = num), nsmall = num)
          }}
        
        if("Pbr207_Pbr206.PM.Per" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pbr207_Pbr206.PM.Per)) >= 5){
            df_temp$Pbr207_Pbr206.PM.Per <- format(round(df_temp$Pbr207_Pbr206.PM.Per, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pbr207_Pbr206.PM.Per)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pbr207_Pbr206.PM.Per)))
            df_temp$Pbr207_Pbr206.PM.Per <- format(round(df_temp$Pbr207_Pbr206.PM.Per,  digits = num), nsmall = num)
          }}
        
        if("Pb207_U235" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb207_U235)) >= 5){
            df_temp$Pb207_U235 <- format(round(df_temp$Pb207_U235, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb207_U235)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb207_U235)))
            df_temp$Pb207_U235 <- format(round(df_temp$Pb207_U235,  digits = num), nsmall = num)
          }}
        
        if("Pb207_U235.PM.Per" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb207_U235.PM.Per)) >= 5){
            df_temp$Pb207_U235.PM.Per <- format(round(df_temp$Pb207_U235.PM.Per, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb207_U235.PM.Per)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb207_U235.PM.Per)))
            df_temp$Pb207_U235.PM.Per <- format(round(df_temp$Pb207_U235.PM.Per,  digits = num), nsmall = num)
          }}
        
        if("Pb207_U235.SD" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb207_U235.SD)) >= 5){
            df_temp$Pb207_U235.SD <- format(round(df_temp$Pb207_U235.SD, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb207_U235.SD)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb207_U235.SD)))
            df_temp$Pb207_U235.SD <- format(round(df_temp$Pb207_U235.SD,  digits = num), nsmall = num)
          }}
        
        if("Pb207_U235.2SD" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb207_U235.2SD)) >= 5){
            df_temp$Pb207_U235.2SD <- format(round(df_temp$Pb207_U235.2SD, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb207_U235.2SD)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb207_U235.2SD)))
            df_temp$Pb207_U235.2SD <- format(round(df_temp$Pb207_U235.2SD,  digits = num), nsmall = num)
          }}
        
        if("Pb207_U235.2SE" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb207_U235.2SE)) >= 5){
            df_temp$Pb207_U235.2SE <- format(round(df_temp$Pb207_U235.2SE, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb207_U235.2SE)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb207_U235.2SE)))
            df_temp$Pb207_U235.2SE <- format(round(df_temp$Pb207_U235.2SE,  digits = num), nsmall = num)
          }}
        
        if("Pbr207_U235" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pbr207_U235)) >= 5){
            df_temp$Pbr207_U235 <- format(round(df_temp$Pbr207_U235, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pbr207_U235)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pbr207_U235)))
            df_temp$Pbr207_U235 <- format(round(df_temp$Pbr207_U235,  digits = num), nsmall = num)
          }}
        
        if("Pbr207_U235.PM" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pbr207_U235.PM)) >= 5){
            df_temp$Pbr207_U235.PM <- format(round(df_temp$Pbr207_U235.PM, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pbr207_U235.PM)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pbr207_U235.PM)))
            df_temp$Pbr207_U235.PM <- format(round(df_temp$Pbr207_U235.PM,  digits = num), nsmall = num)
          }}
        
        if("Pbr207_U235.PM.Per" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pbr207_U235.PM.Per)) >= 5){
            df_temp$Pbr207_U235.PM.Per <- format(round(df_temp$Pbr207_U235.PM.Per, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pbr207_U235.PM.Per)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pbr207_U235.PM.Per)))
            df_temp$Pbr207_U235.PM.Per <- format(round(df_temp$Pbr207_U235.PM.Per,  digits = num), nsmall = num)
          }}
        
        # Pb208 -------------------------------------------------------------------
        
        if("Pb208_Pb206" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb208_Pb206)) >= 5){
            df_temp$Pb208_Pb206 <- format(round(df_temp$Pb208_Pb206, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb208_Pb206)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb208_Pb206)))
            df_temp$Pb208_Pb206 <- format(round(df_temp$Pb208_Pb206,  digits = num), nsmall = num)
          }}
        
        if("Pb208_Pb206.PM.Per" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb208_Pb206.PM.Per)) >= 5){
            df_temp$Pb208_Pb206.PM.Per <- format(round(df_temp$Pb208_Pb206.PM.Per, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb208_Pb206.PM.Per)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb208_Pb206.PM.Per)))
            df_temp$Pb208_Pb206.PM.Per <- format(round(df_temp$Pb208_Pb206.PM.Per,  digits = num), nsmall = num)
          }}
        
        # Pb206.Age ---------------------------------------------------------------
        
        if("Pb206_U238.Age" %in% names(df_temp) ==T){
          df_temp$Pb206_U238.Age <- df_temp$Pb206_U238.Age*0.000001 
          df_temp$Pb206_U238.Age <- format(round(df_temp$Pb206_U238.Age, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb206_U238.Age"] = "Pb206_U238.Age(Ma)"
        }
        
        if("Pb206_U238.Age.PM" %in% names(df_temp) ==T){
          df_temp$Pb206_U238.Age.PM <- df_temp$Pb206_U238.Age.PM*0.000001 
          df_temp$Pb206_U238.Age.PM <- format(round(df_temp$Pb206_U238.Age.PM, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb206_U238.Age.PM"] = "Pb206_U238.Age.PM(Ma)"
        }
        
        if("Pb206_U238.Age.PM.Per" %in% names(df_temp) ==T){
          df_temp$Pb206_U238.Age.PM.Per <- df_temp$Pb206_U238.Age.PM.Per*0.000001 
          df_temp$Pb206_U238.Age.PM.Per <- format(round(df_temp$Pb206_U238.Age.PM.Per, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb206_U238.Age.PM.Per"] = "Pb206_U238.Age.PM.Per(Ma)"
        }
        
        if("Pb206_U238.Age.SD" %in% names(df_temp) ==T){
          df_temp$Pb206_U238.Age.SD <- df_temp$Pb206_U238.Age.SD*0.000001 
          df_temp$Pb206_U238.Age.SD <- format(round(df_temp$Pb206_U238.Age.SD, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb206_U238.Age.SD"] = "Pb206_U238.Age.SD(Ma)"
        }
        
        if("Pb206_U238.Age.2SD" %in% names(df_temp) ==T){
          df_temp$Pb206_U238.Age.2SD <- df_temp$Pb206_U238.Age.2SD*0.000001 
          df_temp$Pb206_U238.Age.2SD <- format(round(df_temp$Pb206_U238.Age.2SD, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb206_U238.Age.2SD"] = "Pb206_U238.Age.2SD(Ma)"
        }
        
        if("Pb206_U238.Age.2SE" %in% names(df_temp) ==T){
          df_temp$Pb206_U238.Age.2SE <- df_temp$Pb206_U238.Age.2SE*0.000001 
          df_temp$Pb206_U238.Age.2SE <- format(round(df_temp$Pb206_U238.Age.2SE, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb206_U238.Age.2SE"] = "Pb206_U238.Age.2SE(Ma)"
        }
        
        if("Pbr206_U238.Age" %in% names(df_temp) ==T){
          df_temp$Pbr206_U238.Age <- df_temp$Pbr206_U238.Age*0.000001 
          df_temp$Pbr206_U238.Age <- format(round(df_temp$Pbr206_U238.Age, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pbr206_U238.Age"] = "Pbr206_U238.Age(Ma)"
        }
        
        if("Pbr206_U238.Age.PM" %in% names(df_temp) ==T){
          df_temp$Pbr206_U238.Age.PM <- df_temp$Pbr206_U238.Age.PM*0.000001 
          df_temp$Pbr206_U238.Age.PM <- format(round(df_temp$Pbr206_U238.Age.PM, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pbr206_U238.Age.PM"] = "Pbr206_U238.Age.PM(Ma)"
        }
        
        if("Pbr206_U238.Age.PM.Per" %in% names(df_temp) ==T){
          df_temp$Pbr206_U238.Age.PM.Per <- df_temp$Pbr206_U238.Age.PM.Per*0.000001 
          df_temp$Pbr206_U238.Age.PM.Per <- format(round(df_temp$Pbr206_U238.Age.PM.Per, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pbr206_U238.Age.PM.Per"] = "Pbr206_U238.Age.PM.Per(Ma)"
        }
        
        # U238 Age--------------------------------------------------------------------
        
        if("U238_Pb206.Age" %in% names(df_temp) ==T){
          df_temp$U238_Pb206.Age <- df_temp$U238_Pb206.Age*0.000001 
          df_temp$U238_Pb206.Age <- format(round(df_temp$U238_Pb206.Age, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "U238_Pb206.Age"] = "U238_Pb206.Age(Ma)"
        }
        
        if("U238_Pb206.Age.PM.Per" %in% names(df_temp) ==T){
          df_temp$U238_Pb206.Age.PM.Per <- df_temp$U238_Pb206.Age.PM.Per*0.000001 
          df_temp$U238_Pb206.Age.PM.Per <- format(round(df_temp$U238_Pb206.Age.PM.Per, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "U238_Pb206.Age.PM.Per"] = "U238_Pb206.Age.PM.Per(Ma)"
        }
        
        # Pb207 Age ---------------------------------------------------------------
        
        if("Pb207_Pb206.Age" %in% names(df_temp) ==T){
          df_temp$Pb207_Pb206.Age <- df_temp$Pb207_Pb206.Age*0.000001 
          df_temp$Pb207_Pb206.Age <- format(round(df_temp$Pb207_Pb206.Age, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb207_Pb206.Age"] = "Pb207_Pb206.Age(Ma)"
        }
        
        if("Pb207_Pb206.Age.PM" %in% names(df_temp) ==T){
          df_temp$Pb207_Pb206.Age.PM <- df_temp$Pb207_Pb206.Age.PM*0.000001 
          df_temp$Pb207_Pb206.Age.PM <- format(round(df_temp$Pb207_Pb206.Age.PM, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb207_Pb206.Age.PM"] = "Pb207_Pb206.Age.PM(Ma)"
        }
        
        if("Pb207_Pb206.Age.PM.Per" %in% names(df_temp) ==T){
          df_temp$Pb207_Pb206.Age.PM.Per <- df_temp$Pb207_Pb206.Age.PM.Per*0.000001 
          df_temp$Pb207_Pb206.Age.PM.Per <- format(round(df_temp$Pb207_Pb206.Age.PM.Per, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb207_Pb206.Age.PM.Per"] = "Pb207_Pb206.Age.PM.Per(Ma)"
        }
        
        if("Pb207_Pb206.Age.SD" %in% names(df_temp) ==T){
          df_temp$Pb207_Pb206.Age.SD <- df_temp$Pb207_Pb206.Age.SD*0.000001 
          df_temp$Pb207_Pb206.Age.SD <- format(round(df_temp$Pb207_Pb206.Age.SD, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb207_Pb206.Age.SD"] = "Pb207_Pb206.Age.SD(Ma)"
        }
        
        if("Pb207_Pb206.Age.2SD" %in% names(df_temp) ==T){
          df_temp$Pb207_Pb206.Age.2SD <- df_temp$Pb207_Pb206.Age.2SD*0.000001 
          df_temp$Pb207_Pb206.Age.2SD <- format(round(df_temp$Pb207_Pb206.Age.2SD, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb207_Pb206.Age.2SD"] = "Pb207_Pb206.Age.2SD(Ma)"
        }
        
        if("Pb207_Pb206.Age.2SE" %in% names(df_temp) ==T){
          df_temp$Pb207_Pb206.Age.2SE <- df_temp$Pb207_Pb206.Age.2SE*0.000001 
          df_temp$Pb207_Pb206.Age.2SE <- format(round(df_temp$Pb207_Pb206.Age.2SE, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb207_Pb206.Age.2SE"] = "Pb207_Pb206.Age.2SE(Ma)"
        }
        
        if("Pbr207_Pbr206.Age" %in% names(df_temp) ==T){
          df_temp$Pbr207_Pbr206.Age <- df_temp$Pbr207_Pbr206.Age*0.000001 
          df_temp$Pbr207_Pbr206.Age <- format(round(df_temp$Pbr207_Pbr206.Age, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pbr207_Pbr206.Age"] = "Pbr207_Pbr206.Age(Ma)"
        }
        
        if("Pbr207_Pbr206.Age.PM" %in% names(df_temp) ==T){
          df_temp$Pbr207_Pbr206.Age.PM <- df_temp$Pbr207_Pbr206.Age.PM*0.000001 
          df_temp$Pbr207_Pbr206.Age.PM <- format(round(df_temp$Pbr207_Pbr206.Age.PM, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pbr207_Pbr206.Age.PM"] = "Pbr207_Pbr206.Age.PM(Ma)"
        }
        
        if("Pbr207_Pbr206.Age.PM.Per" %in% names(df_temp) ==T){
          df_temp$Pbr207_Pbr206.Age.PM.Per <- df_temp$Pbr207_Pbr206.Age.PM.Per*0.000001 
          df_temp$Pbr207_Pbr206.Age.PM.Per <- format(round(df_temp$Pbr207_Pbr206.Age.PM.Per, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pbr207_Pbr206.Age.PM.Per"] = "Pbr207_Pbr206.Age.PM.Per(Ma)"
        }
        
        if("Pb207_U235.Age" %in% names(df_temp) ==T){
          df_temp$Pb207_U235.Age <- df_temp$Pb207_U235.Age*0.000001 
          df_temp$Pb207_U235.Age <- format(round(df_temp$Pb207_U235.Age, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb207_U235.Age"] = "Pb207_U235.Age(Ma)"
        }
        
        if("Pb207_U235.Age.PM" %in% names(df_temp) ==T){
          df_temp$Pb207_U235.Age.PM <- df_temp$Pb207_U235.Age.PM*0.000001 
          df_temp$Pb207_U235.Age.PM <- format(round(df_temp$Pb207_U235.Age.PM, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb207_U235.Age.PM"] = "Pb207_U235.Age.PM(Ma)"
        }
        
        if("Pb207_U235.Age.SD" %in% names(df_temp) ==T){
          df_temp$Pb207_U235.Age.SD <- df_temp$Pb207_U235.Age.SD*0.000001 
          df_temp$Pb207_U235.Age.SD <- format(round(df_temp$Pb207_U235.Age.SD, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb207_U235.Age.SD"] = "Pb207_U235.Age.SD(Ma)"
        }
        
        if("Pb207_U235.Age.2SD" %in% names(df_temp) ==T){
          df_temp$Pb207_U235.Age.2SD <- df_temp$Pb207_U235.Age.2SD*0.000001 
          df_temp$Pb207_U235.Age.2SD <- format(round(df_temp$Pb207_U235.Age.2SD, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb207_U235.Age.2SD"] = "Pb207_U235.Age.2SD(Ma)"
        }
        
        if("Pb207_U235.Age.2SE" %in% names(df_temp) ==T){
          df_temp$Pb207_U235.Age.2SE <- df_temp$Pb207_U235.Age.2SE*0.000001 
          df_temp$Pb207_U235.Age.2SE <- format(round(df_temp$Pb207_U235.Age.2SE, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb207_U235.Age.2SE"] = "Pb207_U235.Age.2SE(Ma)"
        }
        
        # Pb208_Age ---------------------------------------------------------------
        
        if("Pb208_Th232.Age" %in% names(df_temp) ==T){
          df_temp$Pb208_Th232.Age <- df_temp$Pb208_Th232.Age*0.000001 
          df_temp$Pb208_Th232.Age <- format(round(df_temp$Pb208_Th232.Age, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb208_Th232.Age"] = "Pb208_Th232.Age(Ma)"
        }
        
        if("Pb208_Th232.Age.PM" %in% names(df_temp) ==T){
          df_temp$Pb208_Th232.Age.PM <- df_temp$Pb208_Th232.Age.PM*0.000001 
          df_temp$Pb208_Th232.Age.PM <- format(round(df_temp$Pb208_Th232.Age.PM, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb208_Th232.Age.PM"] = "Pb208_Th232.Age.PM(Ma)"
        }
        
        if("Age" %in% names(df_temp) ==T){
          df_temp$Age <- df_temp$Age*0.000001 
          df_temp$Age <- format(round(df_temp$Age, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Age"] = "Age(Ma)"
        }
        
        if("Age.PM" %in% names(df_temp) ==T){
          df_temp$Age.PM <- df_temp$Age.PM*0.000001 
          df_temp$Age.PM <- format(round(df_temp$Age.PM, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Age.PM"] = "Age.PM(Ma)"
        }
        
        
        if("Disc" %in% names(df_temp) == T){
          df_temp$Disc <- format(round(df_temp$Disc, digits = 0), nsmall = 0)
        }
        
        df <- df_temp
        
      }
    }
  })
  
  output$subrefmetvaltbl <- renderDataTable(
    subrefmetvaldata(),
    rownames = F,
    selection = "single")
  
  output$cntref_met_iso <- renderUI({
      paste("Isotope Data:", nrow(subrefmetvaldata()))  
  })
  
  output$DBrefmet <- downloadHandler(
    
    filename = function(){
      paste0("Reference_Method_Data.csv")    
    },
    content = function(file){
      write.csv(refmetdata()[,-c(1)], file, row.names = F)
    }
  )
  
  
  ## Reference Age ----------------------------------------------------------------------------
  
  refagedata <- reactive({
    
    load("RData/OrgRData/age.RData")  
    load("RData/OrgRData/ref.RData") 
    
    if(file.exists("RData/UserRData/ref_custom.RData")){
      load("RData/UserRData/ref_custom.RData")
      load("RData/UserRData/age_custom.RData")
      if(nrow(ref_custom) == 0){
        ref
      }else if(nrow(ref_custom) != 0){
        ref <- rbind(ref, ref_custom)
      }
      
      if(nrow(age_custom) == 0){
        age
      }else if(nrow(age_custom) != 0){
        age <- rbind(age, age_custom)
      }
    }
    
    age <- age[,c(1,8)]
    age <- data.table(unique(age))
    names(age) <- c("RefID", "Era")
    
    ref <- join(ref, age, by = "RefID")
    ref <- ref[ref$Era %in% input$refage, c(1:10)]    

  })
  
  output$refagetbl <-renderDataTable(
    
    refagedata(),
    rownames = F,
    selection = "single",
    
    options = list(scrollX = T,
                   autoWidth = T,
                   columnDefs = list(list(width = '500px', targets = c(1)),
                                     list(width = '250px', targets = c(2)),
                                     list(width = '300px', targets = c(3:5)),
                                     list(visible = F, targets = c(0)))
    ))
  
  output$cntref_age_ref <- renderUI({
    paste("Reference:", nrow(refagedata()))
  })
  
  
  subrefagedata <- reactive({
    
    load("RData/OrgRData/spl.RData")  
    if(file.exists("RData/UserRData/spl_custom.RData")){
      load("RData/UserRData/spl_custom.RData")
      load("RData/UserRData/age_custom.RData")
      if(nrow(spl_custom) == 0){
        spl
      }else if(nrow(spl_custom) != 0){
        spl <- rbind(spl, spl_custom)
      }
      
      if(nrow(age_custom) == 0){
        age
      }else if(nrow(age_custom) != 0){
        age <- rbind(age, age_custom)
      }
    }
    
    selected_row <- input$refagetbl_rows_selected
    selected_sample <- refagedata()[as.integer(selected_row),]$RefID
    
    if(length(selected_sample)){
      
      spl$Longitude <- format(spl$Longitude, digits = 4, nsmall = 4)
      spl$Latitude <- format(spl$Latitude, digits = 4, nsmall = 4)
      
      spl <- join(spl, age, by = c("RefID", "Sample_seq"))
      
      spl <- spl %>% 
        mutate(spl, ID = paste0(spl$RefID, spl$Sample_seq, spl$Method_cd))
      
      spl <- spl[,-c(9)]
      
      spl <- spl[spl$RefID %in% selected_sample, c(3:7,12,15)]
    } 
  })
  
  output$subrefagetbl <- renderDataTable(
    subrefagedata(),
    rownames = F,
    selection = "single",
    options = list(columnDefs = list(list(visible = F, targets = c(5,6))))
  )
  
  output$cntref_age_spl <- renderUI({
    if(length(subrefagedata())){
      paste("Sample:", nrow(subrefagedata()))  
    }
  })
  
  subrefageval <- reactive({
    sub_selected_row <- input$subrefagetbl_rows_selected
    sub_selected_sample <- subrefagedata()[as.integer(sub_selected_row),]$ID
  })
  
  
  
  subrefagevaldata <- reactive({
    
    if(length(subrefageval())){
      
      if(substr(subrefageval(), nchar(subrefageval())-2, nchar(subrefageval())) == "001"){
        
        # method 001 --------------------------------------------------------------
        
        load("RData/OrgRData/value_ar.RData")
        if(file.exists("RData/UserRData/value_ar_custom.RData")){
          load("RData/UserRData/value_ar_custom.RData")
          value_ar <- c(value_ar, value_ar_custom)
        }
        
        df_temp<- as.data.frame(value_ar[[paste0(subrefageval(), ".xlsx")]][-c(1)])
        
        if("Plateau.Age" %in% names(df_temp) ==T){
          df_temp$Plateau.Age <- df_temp$Plateau.Age*0.001 
          df_temp$Plateau.Age <- format(round(df_temp$Plateau.Age, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Plateau.Age"] = "Plateau.Age(Ka)"
        }
        
        if("Plateau.Age.SD" %in% names(df_temp) ==T){
          df_temp$Plateau.Age.SD <- df_temp$Plateau.Age.SD*0.001 
          df_temp$Plateau.Age.SD <- format(round(df_temp$Plateau.Age.SD, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Plateau.Age.SD"] = "Plateau.Age.SD(Ka)"
        }
        
        if("Intergrated.Age" %in% names(df_temp) ==T){
          df_temp$Intergrated.Age <- df_temp$Intergrated.Age*0.001 
          df_temp$Intergrated.Age <- format(round(df_temp$Intergrated.Age, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Intergrated.Age"] = "Intergrated.Age(Ka)"
        }
        
        if("Intergrated.Age.SD" %in% names(df_temp) ==T){
          df_temp$Intergrated.Age.SD <- df_temp$Intergrated.Age.SD*0.001 
          df_temp$Intergrated.Age.SD <- format(round(df_temp$Intergrated.Age.SD, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Intergrated.Age.SD"] = "Intergrated.Age.SD(Ka)"
        }
        
        df <- df_temp
        
      }else if(substr(subrefageval(), nchar(subrefageval())-2, nchar(subrefageval())) == "002"){
        
        # method 002 --------------------------------------------------------------
        
        load("RData/OrgRData/value_cosmo.RData")
        if(file.exists("RData/UserRData/value_cosmo_custom.RData")){
          load("RData/UserRData/value_cosmo_custom.RData")
          value_cosmo <- c(value_cosmo, value_cosmo_custom)
        }
        
        df_temp<- as.data.frame(value_cosmo[[paste0(subrefageval(), ".xlsx")]][-c(1)])
        
        if("Age" %in% names(df_temp) ==T){
          df_temp$Age <- df_temp$Age*0.001
          names(df_temp)[names(df_temp) == "Age"] = "Age(Ka)"
        }
        
        if("Age.PM" %in% names(df_temp) ==T){
          df_temp$Age.PM <- df_temp$Age.PM*0.001
          names(df_temp)[names(df_temp) == "Age.PM"] = "Age.PM(Ka)"
        }
        
        df <- df_temp
        
      }else if(substr(subrefageval(), nchar(subrefageval())-2, nchar(subrefageval())) == "003"){
        
        # method 003 --------------------------------------------------------------
        
        load("RData/OrgRData/value_kar.RData")
        if(file.exists("RData/UserRData/value_kar_custom.RData")){
          load("RData/UserRData/value_kar_custom.RData")
          value_kar <- c(value_kar, value_kar_custom)
        }
        
        df_temp<- as.data.frame(value_kar[[paste0(subrefageval(), ".xlsx")]][-c(1)])
        
        if("Delta.Radiogenic.Ar40" %in% names(df_temp) == T){
          df_temp$Delta.Radiogenic.Ar40 <- format(df_temp$Delta.Radiogenic.Ar40, scientific = F) 
        }
        
        if("Radiogenic.Ar36" %in% names(df_temp) == T){
          df_temp$Radiogenic.Ar36 <- format(df_temp$Radiogenic.Ar36, scientific = F)
        }
        
        if("Delta.Radiogenic.Ar36" %in% names(df_temp) == T){
          df_temp$Delta.Radiogenic.Ar36 <- format(df_temp$Delta.Radiogenic.Ar36, scientific = F)
        }
        
        if("Age" %in% names(df_temp) ==T){
          df_temp$Age <- df_temp$Age*0.000001
          names(df_temp)[names(df_temp) == "Age"] = "Age(Ma)"
        }
        
        if("Age.PM" %in% names(df_temp) ==T){
          df_temp$Age.PM <- df_temp$Age.PM*0.000001
          names(df_temp)[names(df_temp) == "Age.PM"] = "Age.PM(Ma)"
        }
        
        df <- df_temp
        
      }else if(substr(subrefageval(), nchar(subrefageval())-2, nchar(subrefageval())) == "004"){
        
        # method 004 --------------------------------------------------------------
        
        load("RData/OrgRData/value_luhf.RData")
        if(file.exists("RData/UserRData/value_luhf_custom.RData")){
          load("RData/UserRData/value_luhf_custom.RData")
          value_luhf <- c(value_luhf, value_luhf_custom)
        }
        
        df_temp<- as.data.frame(value_luhf[[paste0(subrefageval(), ".xlsx")]][-c(1)])
        
        if("Age" %in% names(df_temp) ==T){
          df_temp$Age <- df_temp$Age*0.000001
          names(df_temp)[names(df_temp) == "Age"] = "Age(Ma)"
        }
        
        if("TDM" %in% names(df_temp) ==T){
          df_temp$TDM <- df_temp$TDM*0.000001
          names(df_temp)[names(df_temp) == "TDM"] = "TDM(Ma)"
        }
        
        if("TDMc" %in% names(df_temp) ==T){
          df_temp$TDMc <- df_temp$TDMc*0.000001
          names(df_temp)[names(df_temp) == "TDMc"] = "TDMc(Ma)"
        }
        
        if("TDM2" %in% names(df_temp) ==T){
          df_temp$TDM2 <- df_temp$TDM2*0.000001
          names(df_temp)[names(df_temp) == "TDM2"] = "TDM2(Ma)"
        }
        
        df <- df_temp
        
      }else if(substr(subrefageval(), nchar(subrefageval())-2, nchar(subrefageval())) == "005"){
        
        # method 005 --------------------------------------------------------------
        
        load("RData/OrgRData/value_osl.RData")
        if(file.exists("RData/UserRData/value_osl_custom.RData")){
          load("RData/UserRData/value_osl_custom.RData")
          value_osl <- c(value_osl, value_osl_custom)
        }
        
        df_temp<- as.data.frame(value_osl[[paste0(subrefageval(), ".xlsx")]][-c(1)])
        
        if("Age" %in% names(df_temp) ==T){
          df_temp$Age <- df_temp$Age*0.001
          names(df_temp)[names(df_temp) == "Age"] = "Age(Ka)"
        }
        
        if("Age.PM" %in% names(df_temp) ==T){
          df_temp$Age.PM <- df_temp$Age.PM*0.001
          names(df_temp)[names(df_temp) == "Age.PM"] = "Age.PM(Ka)"
        }
        
        if("Age.SD" %in% names(df_temp) ==T){
          df_temp$Age.SD <- df_temp$Age.SD*0.001
          names(df_temp)[names(df_temp) == "Age.SD"] = "Age.SD(Ka)"
        }
        
        if("Age.SE" %in% names(df_temp) ==T){
          df_temp$Age.SE <- df_temp$Age.SE*0.001
          names(df_temp)[names(df_temp) == "Age.SE"] = "Age.SE(Ka)"
        }
        
        df <- df_temp
        
      }else if(substr(subrefageval(), nchar(subrefageval())-2, nchar(subrefageval())) == "006"){
        
        # method 006 --------------------------------------------------------------
        
        load("RData/OrgRData/value_smnd.RData")
        if(file.exists("RData/UserRData/value_smnd_custom.RData")){
          load("RData/UserRData/value_smnd_custom.RData")
          value_smnd <- c(value_smnd, value_smnd_custom)
        }
        
        df_temp<- as.data.frame(value_smnd[[paste0(subrefageval(), ".xlsx")]][-c(1)])
        
        if("TDM" %in% names(df_temp) ==T){
          df_temp$TDM <- df_temp$TDM*0.000001
          names(df_temp)[names(df_temp) == "TDM"] = "TDM(Ma)"
        }
        
        if("TDM2" %in% names(df_temp) ==T){
          df_temp$TDM2 <- df_temp$TDM2*0.000001
          names(df_temp)[names(df_temp) == "TDM2"] = "TDM2(Ma)"
        }
        
        df <- df_temp
        
        
      }else if(substr(subrefageval(), nchar(subrefageval())-2, nchar(subrefageval())) == "007"){
        
        # method 007 --------------------------------------------------------------
        load("RData/OrgRData/value_upb.RData")
        if(file.exists("RData/UserRData/value_upb_custom.RData")){
          load("RData/UserRData/value_upb_custom.RData")  
          value_upb <- c(value_upb, value_upb_custom)
        }
        
        df_temp<- as.data.frame(value_upb[[paste0(subrefageval(), ".xlsx")]][-c(1)])
        
        if("U" %in% names(df_temp) ==T){
          df_temp$U <- format(round(df_temp$U, digits = 0), nsmall = 0, scientific = F)
        }
        
        if("Th" %in% names(df_temp) ==T){
          df_temp$Th <- format(round(df_temp$Th, digits = 0), nsmall = 0, scientific = F)
        }
        
        if("Th_U" %in% names(df_temp) ==T){
          df_temp$Th_U <- format(round(df_temp$Th_U, digits = 2), nsmall = 2, scientific = F)
        }
        
        if("Th_U.Per" %in% names(df_temp) ==T){
          df_temp$Th_U.Per <- format(round(df_temp$Th_U.Per, digits = 2), nsmall = 2, scientific = F)
        }
        
        if("Th_U.PM.Per" %in% names(df_temp) ==T){
          df_temp$Th_U.PM.Per <- format(round(df_temp$Th_U.PM.Per, digits = 2), nsmall = 2, scientific = F)
        }
        
        ## pb series ---------------------------------------------------------------
        
        if("Pb" %in% names(df_temp) ==T){
          df_temp$Pb <- format(round(df_temp$Pb, digits = 2), nsmall = 2, scientific = F)
        }
        
        if("Pb206" %in% names(df_temp) ==T){
          df_temp$Pb206 <- format(round(df_temp$Pb206, digits = 2), nsmall = 2, scientific = F)
        }
        
        if("Pb206.PM.Per" %in% names(df_temp) ==T){
          df_temp$Pb206.PM.Per <- format(round(df_temp$Pb206.PM.Per, digits = 2), nsmall = 2, scientific = F)
        }
        
        if("Pbc" %in% names(df_temp) ==T){
          df_temp$Pbc <- format(round(df_temp$Pbc, digits = 2), nsmall = 2, scientific = F)
        }
        
        if("Pbr" %in% names(df_temp) ==T){
          df_temp$Pbr <- format(round(df_temp$Pbr, digits = 2), nsmall = 2, scientific = F)
        }
        
        
        # Pb204 -------------------------------------------------------------------
        
        if("Pb204_Pb206" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb204_Pb206)) >= 5){
            df_temp$Pb204_Pb206 <- format(round(df_temp$Pb204_Pb206, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb204_Pb206)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb204_Pb206)))
            df_temp$Pb204_Pb206 <- format(round(df_temp$Pb204_Pb206,  digits = num), nsmall = num)
          }}
        
        if("Pb204_Pb206.PM" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb204_Pb206.PM)) >= 5){
            df_temp$Pb204_Pb206.PM <- format(round(df_temp$Pb204_Pb206.PM, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb204_Pb206.PM)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb204_Pb206.PM)))
            df_temp$Pb204_Pb206.PM <- format(round(df_temp$Pb204_Pb206.PM,  digits = num), nsmall = num)
          }}
        
        if("Pb204_Pb206.PM.Per" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb204_Pb206.PM.Per)) >= 5){
            df_temp$Pb204_Pb206.PM.Per <- format(round(df_temp$Pb204_Pb206.PM.Per, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb204_Pb206.PM.Per)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb204_Pb206.PM.Per)))
            df_temp$Pb204_Pb206.PM.Per <- format(round(df_temp$Pb204_Pb206.PM.Per,  digits = num), nsmall = num)
          }}
        
        # Pb206 -------------------------------------------------------------------
        
        if("Pb206_Th232" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb206_Th232)) >= 5){
            df_temp$Pb206_Th232 <- format(round(df_temp$Pb206_Th232, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb206_Th232)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb206_Th232)))
            df_temp$Pb206_Th232 <- format(round(df_temp$Pb206_Th232,  digits = num), nsmall = num)
          }}
        
        
        if("Pb206_Th232.SD" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb206_Th232.SD)) >= 5){
            df_temp$Pb206_Th232.SD <- format(round(df_temp$Pb206_Th232.SD, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb206_Th232.SD)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb206_Th232.SD)))
            df_temp$Pb206_Th232.SD <- format(round(df_temp$Pb206_Th232.SD,  digits = num), nsmall = num)
          }}
        
        if("Pb206_U238" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb206_U238)) >= 5){
            df_temp$Pb206_U238 <- format(round(df_temp$Pb206_U238, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb206_U238)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb206_U238)))
            df_temp$Pb206_U238 <- format(round(df_temp$Pb206_U238,  digits = num), nsmall = num)
          }}
        
        if("Pb206_U238.PM" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb206_U238.PM)) >= 5){
            df_temp$Pb206_U238.PM <- format(round(df_temp$Pb206_U238.PM, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb206_U238.PM)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb206_U238.PM)))
            df_temp$Pb206_U238.PM <- format(round(df_temp$Pb206_U238.PM,  digits = num), nsmall = num)
          }}
        
        if("Pb206_U238.PM.Per" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb206_U238.PM.Per)) >= 5){
            df_temp$Pb206_U238.PM.Per <- format(round(df_temp$Pb206_U238.PM.Per, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb206_U238.PM.Per)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb206_U238.PM.Per)))
            df_temp$Pb206_U238.PM.Per <- format(round(df_temp$Pb206_U238.PM.Per,  digits = num), nsmall = num)
          }}
        
        if("Pb206_U238.SD" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb206_U238.SD)) >= 5){
            df_temp$Pb206_U238.SD <- format(round(df_temp$Pb206_U238.SD, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb206_U238.SD)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb206_U238.SD)))
            df_temp$Pb206_U238.SD <- format(round(df_temp$Pb206_U238.SD,  digits = num), nsmall = num)
          }}
        
        if("Pb206_U238.2SD" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb206_U238.2SD)) >= 5){
            df_temp$Pb206_U238.2SD <- format(round(df_temp$Pb206_U238.2SD, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb206_U238.2SD)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb206_U238.2SD)))
            df_temp$Pb206_U238.2SD <- format(round(df_temp$Pb206_U238.2SD,  digits = num), nsmall = num)
          }}
        
        if("Pb206_U238.2SE" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb206_U238.2SE)) >= 5){
            df_temp$Pb206_U238.2SE <- format(round(df_temp$Pb206_U238.2SE, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb206_U238.2SE)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb206_U238.2SE)))
            df_temp$Pb206_U238.2SE <- format(round(df_temp$Pb206_U238.2SE,  digits = num), nsmall = num)
          }}
        
        if("Pbr206_U238" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pbr206_U238)) >= 5){
            df_temp$Pbr206_U238 <- format(round(df_temp$Pbr206_U238, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pbr206_U238)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pbr206_U238)))
            df_temp$Pbr206_U238 <- format(round(df_temp$Pbr206_U238,  digits = num), nsmall = num)
          }}
        
        if("Pbr206_U238.PM" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pbr206_U238.PM)) >= 5){
            df_temp$Pbr206_U238.PM <- format(round(df_temp$Pbr206_U238.PM, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pbr206_U238.PM)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pbr206_U238.PM)))
            df_temp$Pbr206_U238.PM <- format(round(df_temp$Pbr206_U238.PM,  digits = num), nsmall = num)
          }}
        
        if("Pbr206_U238.PM.Per" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pbr206_U238.PM.Per)) >= 5){
            df_temp$Pbr206_U238.PM.Per <- format(round(df_temp$Pbr206_U238.PM.Per, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pbr206_U238.PM.Per)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pbr206_U238.PM.Per)))
            df_temp$Pbr206_U238.PM.Per <- format(round(df_temp$Pbr206_U238.PM.Per,  digits = num), nsmall = num)
          }}
        
        # U238 --------------------------------------------------------------------
        
        if("U238_Pb206" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$U238_Pb206)) >= 5){
            df_temp$U238_Pb206 <- format(round(df_temp$U238_Pb206, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$U238_Pb206)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$U238_Pb206)))
            df_temp$U238_Pb206 <- format(round(df_temp$U238_Pb206,  digits = num), nsmall = num)
          }}
        
        
        if("U238_Pb206.Per" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$U238_Pb206.Per)) >= 5){
            df_temp$U238_Pb206.Per <- format(round(df_temp$U238_Pb206.Per, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$U238_Pb206.Per)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$U238_Pb206.Per)))
            df_temp$U238_Pb206.Per <- format(round(df_temp$U238_Pb206.Per,  digits = num), nsmall = num)
          }}
        
        if("U238_Pb206.PM" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$U238_Pb206.PM)) >= 5){
            df_temp$U238_Pb206.PM <- format(round(df_temp$U238_Pb206.PM, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$U238_Pb206.PM)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$U238_Pb206.PM)))
            df_temp$U238_Pb206.PM <- format(round(df_temp$U238_Pb206.PM,  digits = num), nsmall = num)
          }}
        
        if("U238_Pb206.PM.Per" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$U238_Pb206.PM.Per)) >= 5){
            df_temp$U238_Pb206.PM.Per <- format(round(df_temp$U238_Pb206.PM.Per, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$U238_Pb206.PM.Per)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$U238_Pb206.PM.Per)))
            df_temp$U238_Pb206.PM.Per <- format(round(df_temp$U238_Pb206.PM.Per,  digits = num), nsmall = num)
          }}
        
        if("U238_Pb206.SD" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$U238_Pb206.SD)) >= 5){
            df_temp$U238_Pb206.SD <- format(round(df_temp$U238_Pb206.SD, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$U238_Pb206.SD)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$U238_Pb206.SD)))
            df_temp$U238_Pb206.SD <- format(round(df_temp$U238_Pb206.SD,  digits = num), nsmall = num)
          }}
        
        if("U238_Pbr206" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$U238_Pbr206)) >= 5){
            df_temp$U238_Pbr206 <- format(round(df_temp$U238_Pbr206, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$U238_Pbr206)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$U238_Pbr206)))
            df_temp$U238_Pbr206 <- format(round(df_temp$U238_Pbr206,  digits = num), nsmall = num)
          }}
        
        
        if("U238_Pbr206.PM" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$U238_Pbr206.PM)) >= 5){
            df_temp$U238_Pbr206.PM <- format(round(df_temp$U238_Pbr206.PM, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$U238_Pbr206.PM)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$U238_Pbr206.PM)))
            df_temp$U238_Pbr206.PM <- format(round(df_temp$U238_Pbr206.PM,  digits = num), nsmall = num)
          }}
        
        if("U238_Pbr206.PM.Per" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$U238_Pbr206.PM.Per)) >= 5){
            df_temp$U238_Pbr206.PM.Per <- format(round(df_temp$U238_Pbr206.PM.Per, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$U238_Pbr206.PM.Per)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$U238_Pbr206.PM.Per)))
            df_temp$U238_Pbr206.PM.Per <- format(round(df_temp$U238_Pbr206.PM.Per,  digits = num), nsmall = num)
          }}
        
        # Pb207 -------------------------------------------------------------------
        
        if("Pb207_Pb206" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb207_Pb206)) >= 5){
            df_temp$Pb207_Pb206 <- format(round(df_temp$Pb207_Pb206, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb207_Pb206)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb207_Pb206)))
            df_temp$Pb207_Pb206 <- format(round(df_temp$Pb207_Pb206,  digits = num), nsmall = num)
          }}
        
        if("Pb207_Pb206.Per" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb207_Pb206.Per)) >= 5){
            df_temp$Pb207_Pb206.Per <- format(round(df_temp$Pb207_Pb206.Per, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb207_Pb206.Per)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb207_Pb206.Per)))
            df_temp$Pb207_Pb206.Per <- format(round(df_temp$Pb207_Pb206.Per,  digits = num), nsmall = num)
          }}
        
        if("Pb207_Pb206.PM" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb207_Pb206.PM)) >= 5){
            df_temp$Pb207_Pb206.PM <- format(round(df_temp$Pb207_Pb206.PM, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb207_Pb206.PM)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb207_Pb206.PM)))
            df_temp$Pb207_Pb206.PM <- format(round(df_temp$Pb207_Pb206.PM,  digits = num), nsmall = num)
          }}
        
        if("Pb207_Pb206.PM.Per" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb207_Pb206.PM.Per)) >= 5){
            df_temp$Pb207_Pb206.PM.Per <- format(round(df_temp$Pb207_Pb206.PM.Per, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb207_Pb206.PM.Per)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb207_Pb206.PM.Per)))
            df_temp$Pb207_Pb206.PM.Per <- format(round(df_temp$Pb207_Pb206.PM.Per,  digits = num), nsmall = num)
          }}
        
        if("Pb207_Pb206.SD" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb207_Pb206.SD)) >= 5){
            df_temp$Pb207_Pb206.SD <- format(round(df_temp$Pb207_Pb206.SD, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb207_Pb206.SD)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb207_Pb206.SD)))
            df_temp$Pb207_Pb206.SD <- format(round(df_temp$Pb207_Pb206.SD,  digits = num), nsmall = num)
          }}
        
        if("Pb207_Pb206.2SD" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb207_Pb206.2SD)) >= 5){
            df_temp$Pb207_Pb206.2SD <- format(round(df_temp$Pb207_Pb206.2SD, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb207_Pb206.2SD)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb207_Pb206.2SD)))
            df_temp$Pb207_Pb206.2SD <- format(round(df_temp$Pb207_Pb206.2SD,  digits = num), nsmall = num)
          }}
        
        if("Pb207_Pb206.2SE" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb207_Pb206.2SE)) >= 5){
            df_temp$Pb207_Pb206.2SE <- format(round(df_temp$Pb207_Pb206.2SE, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb207_Pb206.2SE)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb207_Pb206.2SE)))
            df_temp$Pb207_Pb206.2SE <- format(round(df_temp$Pb207_Pb206.2SE,  digits = num), nsmall = num)
          }}
        
        if("Pbr207_Pb206" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pbr207_Pb206)) >= 5){
            df_temp$Pbr207_Pb206 <- format(round(df_temp$Pbr207_Pb206, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pbr207_Pb206)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pbr207_Pb206)))
            df_temp$Pbr207_Pb206 <- format(round(df_temp$Pbr207_Pb206,  digits = num), nsmall = num)
          }}
        
        if("Pbr207_Pb206.PM" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pbr207_Pb206.PM)) >= 5){
            df_temp$Pbr207_Pb206.PM <- format(round(df_temp$Pbr207_Pb206.PM, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pbr207_Pb206.PM)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pbr207_Pb206.PM)))
            df_temp$Pbr207_Pb206.PM <- format(round(df_temp$Pbr207_Pb206.PM,  digits = num), nsmall = num)
          }}
        
        if("Pbr207_Pb206.PM.Per" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pbr207_Pb206.PM.Per)) >= 5){
            df_temp$Pbr207_Pb206.PM.Per <- format(round(df_temp$Pbr207_Pb206.PM.Per, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pbr207_Pb206.PM.Per)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pbr207_Pb206.PM.Per)))
            df_temp$Pbr207_Pb206.PM.Per <- format(round(df_temp$Pbr207_Pb206.PM.Per,  digits = num), nsmall = num)
          }}
        
        if("Pbr207_Pbr206" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pbr207_Pbr206)) >= 5){
            df_temp$Pbr207_Pbr206 <- format(round(df_temp$Pbr207_Pbr206, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pbr207_Pbr206)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pbr207_Pbr206)))
            df_temp$Pbr207_Pbr206 <- format(round(df_temp$Pbr207_Pbr206,  digits = num), nsmall = num)
          }}
        
        if("Pbr207_Pbr206.PM.Per" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pbr207_Pbr206.PM.Per)) >= 5){
            df_temp$Pbr207_Pbr206.PM.Per <- format(round(df_temp$Pbr207_Pbr206.PM.Per, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pbr207_Pbr206.PM.Per)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pbr207_Pbr206.PM.Per)))
            df_temp$Pbr207_Pbr206.PM.Per <- format(round(df_temp$Pbr207_Pbr206.PM.Per,  digits = num), nsmall = num)
          }}
        
        if("Pb207_U235" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb207_U235)) >= 5){
            df_temp$Pb207_U235 <- format(round(df_temp$Pb207_U235, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb207_U235)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb207_U235)))
            df_temp$Pb207_U235 <- format(round(df_temp$Pb207_U235,  digits = num), nsmall = num)
          }}
        
        if("Pb207_U235.PM.Per" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb207_U235.PM.Per)) >= 5){
            df_temp$Pb207_U235.PM.Per <- format(round(df_temp$Pb207_U235.PM.Per, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb207_U235.PM.Per)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb207_U235.PM.Per)))
            df_temp$Pb207_U235.PM.Per <- format(round(df_temp$Pb207_U235.PM.Per,  digits = num), nsmall = num)
          }}
        
        if("Pb207_U235.SD" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb207_U235.SD)) >= 5){
            df_temp$Pb207_U235.SD <- format(round(df_temp$Pb207_U235.SD, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb207_U235.SD)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb207_U235.SD)))
            df_temp$Pb207_U235.SD <- format(round(df_temp$Pb207_U235.SD,  digits = num), nsmall = num)
          }}
        
        if("Pb207_U235.2SD" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb207_U235.2SD)) >= 5){
            df_temp$Pb207_U235.2SD <- format(round(df_temp$Pb207_U235.2SD, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb207_U235.2SD)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb207_U235.2SD)))
            df_temp$Pb207_U235.2SD <- format(round(df_temp$Pb207_U235.2SD,  digits = num), nsmall = num)
          }}
        
        if("Pb207_U235.2SE" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb207_U235.2SE)) >= 5){
            df_temp$Pb207_U235.2SE <- format(round(df_temp$Pb207_U235.2SE, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb207_U235.2SE)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb207_U235.2SE)))
            df_temp$Pb207_U235.2SE <- format(round(df_temp$Pb207_U235.2SE,  digits = num), nsmall = num)
          }}
        
        if("Pbr207_U235" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pbr207_U235)) >= 5){
            df_temp$Pbr207_U235 <- format(round(df_temp$Pbr207_U235, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pbr207_U235)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pbr207_U235)))
            df_temp$Pbr207_U235 <- format(round(df_temp$Pbr207_U235,  digits = num), nsmall = num)
          }}
        
        if("Pbr207_U235.PM" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pbr207_U235.PM)) >= 5){
            df_temp$Pbr207_U235.PM <- format(round(df_temp$Pbr207_U235.PM, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pbr207_U235.PM)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pbr207_U235.PM)))
            df_temp$Pbr207_U235.PM <- format(round(df_temp$Pbr207_U235.PM,  digits = num), nsmall = num)
          }}
        
        if("Pbr207_U235.PM.Per" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pbr207_U235.PM.Per)) >= 5){
            df_temp$Pbr207_U235.PM.Per <- format(round(df_temp$Pbr207_U235.PM.Per, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pbr207_U235.PM.Per)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pbr207_U235.PM.Per)))
            df_temp$Pbr207_U235.PM.Per <- format(round(df_temp$Pbr207_U235.PM.Per,  digits = num), nsmall = num)
          }}
        
        # Pb208 -------------------------------------------------------------------
        
        if("Pb208_Pb206" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb208_Pb206)) >= 5){
            df_temp$Pb208_Pb206 <- format(round(df_temp$Pb208_Pb206, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb208_Pb206)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb208_Pb206)))
            df_temp$Pb208_Pb206 <- format(round(df_temp$Pb208_Pb206,  digits = num), nsmall = num)
          }}
        
        if("Pb208_Pb206.PM.Per" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb208_Pb206.PM.Per)) >= 5){
            df_temp$Pb208_Pb206.PM.Per <- format(round(df_temp$Pb208_Pb206.PM.Per, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb208_Pb206.PM.Per)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb208_Pb206.PM.Per)))
            df_temp$Pb208_Pb206.PM.Per <- format(round(df_temp$Pb208_Pb206.PM.Per,  digits = num), nsmall = num)
          }}
        
        # Pb206.Age ---------------------------------------------------------------
        
        if("Pb206_U238.Age" %in% names(df_temp) ==T){
          df_temp$Pb206_U238.Age <- df_temp$Pb206_U238.Age*0.000001 
          df_temp$Pb206_U238.Age <- format(round(df_temp$Pb206_U238.Age, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb206_U238.Age"] = "Pb206_U238.Age(Ma)"
        }
        
        if("Pb206_U238.Age.PM" %in% names(df_temp) ==T){
          df_temp$Pb206_U238.Age.PM <- df_temp$Pb206_U238.Age.PM*0.000001 
          df_temp$Pb206_U238.Age.PM <- format(round(df_temp$Pb206_U238.Age.PM, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb206_U238.Age.PM"] = "Pb206_U238.Age.PM(Ma)"
        }
        
        if("Pb206_U238.Age.PM.Per" %in% names(df_temp) ==T){
          df_temp$Pb206_U238.Age.PM.Per <- df_temp$Pb206_U238.Age.PM.Per*0.000001 
          df_temp$Pb206_U238.Age.PM.Per <- format(round(df_temp$Pb206_U238.Age.PM.Per, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb206_U238.Age.PM.Per"] = "Pb206_U238.Age.PM.Per(Ma)"
        }
        
        if("Pb206_U238.Age.SD" %in% names(df_temp) ==T){
          df_temp$Pb206_U238.Age.SD <- df_temp$Pb206_U238.Age.SD*0.000001 
          df_temp$Pb206_U238.Age.SD <- format(round(df_temp$Pb206_U238.Age.SD, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb206_U238.Age.SD"] = "Pb206_U238.Age.SD(Ma)"
        }
        
        if("Pb206_U238.Age.2SD" %in% names(df_temp) ==T){
          df_temp$Pb206_U238.Age.2SD <- df_temp$Pb206_U238.Age.2SD*0.000001 
          df_temp$Pb206_U238.Age.2SD <- format(round(df_temp$Pb206_U238.Age.2SD, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb206_U238.Age.2SD"] = "Pb206_U238.Age.2SD(Ma)"
        }
        
        if("Pb206_U238.Age.2SE" %in% names(df_temp) ==T){
          df_temp$Pb206_U238.Age.2SE <- df_temp$Pb206_U238.Age.2SE*0.000001 
          df_temp$Pb206_U238.Age.2SE <- format(round(df_temp$Pb206_U238.Age.2SE, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb206_U238.Age.2SE"] = "Pb206_U238.Age.2SE(Ma)"
        }
        
        if("Pbr206_U238.Age" %in% names(df_temp) ==T){
          df_temp$Pbr206_U238.Age <- df_temp$Pbr206_U238.Age*0.000001 
          df_temp$Pbr206_U238.Age <- format(round(df_temp$Pbr206_U238.Age, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pbr206_U238.Age"] = "Pbr206_U238.Age(Ma)"
        }
        
        if("Pbr206_U238.Age.PM" %in% names(df_temp) ==T){
          df_temp$Pbr206_U238.Age.PM <- df_temp$Pbr206_U238.Age.PM*0.000001 
          df_temp$Pbr206_U238.Age.PM <- format(round(df_temp$Pbr206_U238.Age.PM, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pbr206_U238.Age.PM"] = "Pbr206_U238.Age.PM(Ma)"
        }
        
        if("Pbr206_U238.Age.PM.Per" %in% names(df_temp) ==T){
          df_temp$Pbr206_U238.Age.PM.Per <- df_temp$Pbr206_U238.Age.PM.Per*0.000001 
          df_temp$Pbr206_U238.Age.PM.Per <- format(round(df_temp$Pbr206_U238.Age.PM.Per, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pbr206_U238.Age.PM.Per"] = "Pbr206_U238.Age.PM.Per(Ma)"
        }
        
        # U238 Age--------------------------------------------------------------------
        
        if("U238_Pb206.Age" %in% names(df_temp) ==T){
          df_temp$U238_Pb206.Age <- df_temp$U238_Pb206.Age*0.000001 
          df_temp$U238_Pb206.Age <- format(round(df_temp$U238_Pb206.Age, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "U238_Pb206.Age"] = "U238_Pb206.Age(Ma)"
        }
        
        if("U238_Pb206.Age.PM.Per" %in% names(df_temp) ==T){
          df_temp$U238_Pb206.Age.PM.Per <- df_temp$U238_Pb206.Age.PM.Per*0.000001 
          df_temp$U238_Pb206.Age.PM.Per <- format(round(df_temp$U238_Pb206.Age.PM.Per, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "U238_Pb206.Age.PM.Per"] = "U238_Pb206.Age.PM.Per(Ma)"
        }
        
        # Pb207 Age ---------------------------------------------------------------
        
        if("Pb207_Pb206.Age" %in% names(df_temp) ==T){
          df_temp$Pb207_Pb206.Age <- df_temp$Pb207_Pb206.Age*0.000001 
          df_temp$Pb207_Pb206.Age <- format(round(df_temp$Pb207_Pb206.Age, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb207_Pb206.Age"] = "Pb207_Pb206.Age(Ma)"
        }
        
        if("Pb207_Pb206.Age.PM" %in% names(df_temp) ==T){
          df_temp$Pb207_Pb206.Age.PM <- df_temp$Pb207_Pb206.Age.PM*0.000001 
          df_temp$Pb207_Pb206.Age.PM <- format(round(df_temp$Pb207_Pb206.Age.PM, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb207_Pb206.Age.PM"] = "Pb207_Pb206.Age.PM(Ma)"
        }
        
        if("Pb207_Pb206.Age.PM.Per" %in% names(df_temp) ==T){
          df_temp$Pb207_Pb206.Age.PM.Per <- df_temp$Pb207_Pb206.Age.PM.Per*0.000001 
          df_temp$Pb207_Pb206.Age.PM.Per <- format(round(df_temp$Pb207_Pb206.Age.PM.Per, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb207_Pb206.Age.PM.Per"] = "Pb207_Pb206.Age.PM.Per(Ma)"
        }
        
        if("Pb207_Pb206.Age.SD" %in% names(df_temp) ==T){
          df_temp$Pb207_Pb206.Age.SD <- df_temp$Pb207_Pb206.Age.SD*0.000001 
          df_temp$Pb207_Pb206.Age.SD <- format(round(df_temp$Pb207_Pb206.Age.SD, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb207_Pb206.Age.SD"] = "Pb207_Pb206.Age.SD(Ma)"
        }
        
        if("Pb207_Pb206.Age.2SD" %in% names(df_temp) ==T){
          df_temp$Pb207_Pb206.Age.2SD <- df_temp$Pb207_Pb206.Age.2SD*0.000001 
          df_temp$Pb207_Pb206.Age.2SD <- format(round(df_temp$Pb207_Pb206.Age.2SD, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb207_Pb206.Age.2SD"] = "Pb207_Pb206.Age.2SD(Ma)"
        }
        
        if("Pb207_Pb206.Age.2SE" %in% names(df_temp) ==T){
          df_temp$Pb207_Pb206.Age.2SE <- df_temp$Pb207_Pb206.Age.2SE*0.000001 
          df_temp$Pb207_Pb206.Age.2SE <- format(round(df_temp$Pb207_Pb206.Age.2SE, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb207_Pb206.Age.2SE"] = "Pb207_Pb206.Age.2SE(Ma)"
        }
        
        if("Pbr207_Pbr206.Age" %in% names(df_temp) ==T){
          df_temp$Pbr207_Pbr206.Age <- df_temp$Pbr207_Pbr206.Age*0.000001 
          df_temp$Pbr207_Pbr206.Age <- format(round(df_temp$Pbr207_Pbr206.Age, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pbr207_Pbr206.Age"] = "Pbr207_Pbr206.Age(Ma)"
        }
        
        if("Pbr207_Pbr206.Age.PM" %in% names(df_temp) ==T){
          df_temp$Pbr207_Pbr206.Age.PM <- df_temp$Pbr207_Pbr206.Age.PM*0.000001 
          df_temp$Pbr207_Pbr206.Age.PM <- format(round(df_temp$Pbr207_Pbr206.Age.PM, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pbr207_Pbr206.Age.PM"] = "Pbr207_Pbr206.Age.PM(Ma)"
        }
        
        if("Pbr207_Pbr206.Age.PM.Per" %in% names(df_temp) ==T){
          df_temp$Pbr207_Pbr206.Age.PM.Per <- df_temp$Pbr207_Pbr206.Age.PM.Per*0.000001 
          df_temp$Pbr207_Pbr206.Age.PM.Per <- format(round(df_temp$Pbr207_Pbr206.Age.PM.Per, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pbr207_Pbr206.Age.PM.Per"] = "Pbr207_Pbr206.Age.PM.Per(Ma)"
        }
        
        if("Pb207_U235.Age" %in% names(df_temp) ==T){
          df_temp$Pb207_U235.Age <- df_temp$Pb207_U235.Age*0.000001 
          df_temp$Pb207_U235.Age <- format(round(df_temp$Pb207_U235.Age, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb207_U235.Age"] = "Pb207_U235.Age(Ma)"
        }
        
        if("Pb207_U235.Age.PM" %in% names(df_temp) ==T){
          df_temp$Pb207_U235.Age.PM <- df_temp$Pb207_U235.Age.PM*0.000001 
          df_temp$Pb207_U235.Age.PM <- format(round(df_temp$Pb207_U235.Age.PM, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb207_U235.Age.PM"] = "Pb207_U235.Age.PM(Ma)"
        }
        
        if("Pb207_U235.Age.SD" %in% names(df_temp) ==T){
          df_temp$Pb207_U235.Age.SD <- df_temp$Pb207_U235.Age.SD*0.000001 
          df_temp$Pb207_U235.Age.SD <- format(round(df_temp$Pb207_U235.Age.SD, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb207_U235.Age.SD"] = "Pb207_U235.Age.SD(Ma)"
        }
        
        if("Pb207_U235.Age.2SD" %in% names(df_temp) ==T){
          df_temp$Pb207_U235.Age.2SD <- df_temp$Pb207_U235.Age.2SD*0.000001 
          df_temp$Pb207_U235.Age.2SD <- format(round(df_temp$Pb207_U235.Age.2SD, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb207_U235.Age.2SD"] = "Pb207_U235.Age.2SD(Ma)"
        }
        
        if("Pb207_U235.Age.2SE" %in% names(df_temp) ==T){
          df_temp$Pb207_U235.Age.2SE <- df_temp$Pb207_U235.Age.2SE*0.000001 
          df_temp$Pb207_U235.Age.2SE <- format(round(df_temp$Pb207_U235.Age.2SE, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb207_U235.Age.2SE"] = "Pb207_U235.Age.2SE(Ma)"
        }
        
        # Pb208_Age ---------------------------------------------------------------
        
        if("Pb208_Th232.Age" %in% names(df_temp) ==T){
          df_temp$Pb208_Th232.Age <- df_temp$Pb208_Th232.Age*0.000001 
          df_temp$Pb208_Th232.Age <- format(round(df_temp$Pb208_Th232.Age, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb208_Th232.Age"] = "Pb208_Th232.Age(Ma)"
        }
        
        if("Pb208_Th232.Age.PM" %in% names(df_temp) ==T){
          df_temp$Pb208_Th232.Age.PM <- df_temp$Pb208_Th232.Age.PM*0.000001 
          df_temp$Pb208_Th232.Age.PM <- format(round(df_temp$Pb208_Th232.Age.PM, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb208_Th232.Age.PM"] = "Pb208_Th232.Age.PM(Ma)"
        }
        
        if("Age" %in% names(df_temp) ==T){
          df_temp$Age <- df_temp$Age*0.000001 
          df_temp$Age <- format(round(df_temp$Age, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Age"] = "Age(Ma)"
        }
        
        if("Age.PM" %in% names(df_temp) ==T){
          df_temp$Age.PM <- df_temp$Age.PM*0.000001 
          df_temp$Age.PM <- format(round(df_temp$Age.PM, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Age.PM"] = "Age.PM(Ma)"
        }
        
        
        if("Disc" %in% names(df_temp) == T){
          df_temp$Disc <- format(round(df_temp$Disc, digits = 0), nsmall = 0)
        }
        
        df <- df_temp
        
      }
    }
  })
  
  
  output$subrefagevaltbl <- renderDataTable(
    subrefagevaldata(),
    rownames = F,
    selection = "single")
  
  output$cntref_age_iso <- renderUI({
      paste("Isotope Data:", nrow(subrefagevaldata()))  
  })
  
  
  output$DBrefage <- downloadHandler(
    
    filename = function(){
      paste0("Reference_Age_Data.csv")
    },
    
    content = function(file){
      write.csv(refagedata()[,-c(1)], file, row.names = F)
    }
    
  )
  
  # Isotope Data ------------------------------------------------------------------
  
  ## Sample Method ----------------------------------------------------------------------
  
  splmetdata <- reactive({
    
    load("RData/OrgRData/age.RData")  
    load("RData/OrgRData/spl.RData") 
    
    if(file.exists("RData/UserRData/spl_custom.RData")){
      load("RData/UserRData/spl_custom.RData")
      load("RData/UserRData/age_custom.RData")
      if(nrow(spl_custom) == 0){
        spl
      }else if(nrow(spl_custom) != 0){
        spl <- rbind(spl, spl_custom)
      }
      if(nrow(age_custom) == 0){
        age
      }else if(nrow(age_custom) != 0){
        age <- rbind(age, age_custom)
      }
    }
    
    age <- age %>% 
      mutate(age, ID = paste0(age$RefID, age$Sample_seq, age$Method_cd))
    
    age <- age[,c(1,2,3,4,11)]
    age <- data.table(unique(age))
    names(age) <- c("RefID", "Sample_seq", "Method_cd", "Sample", "ID")
    
    age <- join(age, met_cd, by = "Method_cd")
    
    spl <- join(age, spl, by = c("RefID", "Sample_seq"))
    
    spl$Longitude <- format(spl$Longitude, digits = 4, nsmall = 4)
    spl$Latitude <- format(spl$Latitude, digits = 4, nsmall = 4)
    
    spl <- spl[spl$Method %in% input$sammet, c(5,7:11)]
    
  })
  
    output$splmettbl <- renderDataTable(
      splmetdata(),
      rownames = F,
      options = list(scrollX = T,
                     columnDefs = list(list(visible = F, targets = c(0)))),
      selection = "single" 
    )
    
    output$cntiso_met_spl <- renderUI({
      paste("Sample:", nrow(splmetdata()))
    })
  

  subsplmetdata <- reactive({
    
    selected_row <- input$splmettbl_rows_selected
    selected_sample <- splmetdata()[as.integer(selected_row),]$ID
    
  })
  
  
  ### Sample Method Value Option ---------------------------------------------------------------------------
  
  subsplmettbldata <- reactive({
    
    if(length(subsplmetdata())){
      
      if(substr(subsplmetdata(), nchar(subsplmetdata())-2, nchar(subsplmetdata())) == "001"){
        
        # method 001 --------------------------------------------------------------
        
        load("RData/OrgRData/value_ar.RData")
        if(file.exists("RData/UserRData/value_ar_custom.RData")){
          load("RData/UserRData/value_ar_custom.RData")
          value_ar <- c(value_ar, value_ar_custom)
        }
        
        df_temp<- as.data.frame(value_ar[[paste0(subsplmetdata(), ".xlsx")]][-c(1)])
        
        if("Plateau.Age" %in% names(df_temp) ==T){
          df_temp$Plateau.Age <- df_temp$Plateau.Age*0.001 
          df_temp$Plateau.Age <- format(round(df_temp$Plateau.Age, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Plateau.Age"] = "Plateau.Age(Ka)"
        }
        
        if("Plateau.Age.SD" %in% names(df_temp) ==T){
          df_temp$Plateau.Age.SD <- df_temp$Plateau.Age.SD*0.001 
          df_temp$Plateau.Age.SD <- format(round(df_temp$Plateau.Age.SD, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Plateau.Age.SD"] = "Plateau.Age.SD(Ka)"
        }
        
        if("Intergrated.Age" %in% names(df_temp) ==T){
          df_temp$Intergrated.Age <- df_temp$Intergrated.Age*0.001 
          df_temp$Intergrated.Age <- format(round(df_temp$Intergrated.Age, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Intergrated.Age"] = "Intergrated.Age(Ka)"
        }
        
        if("Intergrated.Age.SD" %in% names(df_temp) ==T){
          df_temp$Intergrated.Age.SD <- df_temp$Intergrated.Age.SD*0.001 
          df_temp$Intergrated.Age.SD <- format(round(df_temp$Intergrated.Age.SD, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Intergrated.Age.SD"] = "Intergrated.Age.SD(Ka)"
        }
        
        df <- df_temp
        
      }else if(substr(subsplmetdata(), nchar(subsplmetdata())-2, nchar(subsplmetdata())) == "002"){
        
        # method 002 --------------------------------------------------------------
        
        load("RData/OrgRData/value_cosmo.RData")
        if(file.exists("RData/UserRData/value_cosmo_custom.RData")){
          load("RData/UserRData/value_cosmo_custom.RData")
          value_cosmo <- c(value_cosmo, value_cosmo_custom)
        }
        
        df_temp<- as.data.frame(value_cosmo[[paste0(subsplmetdata(), ".xlsx")]][-c(1)])
        
        if("Age" %in% names(df_temp) ==T){
          df_temp$Age <- df_temp$Age*0.001
          names(df_temp)[names(df_temp) == "Age"] = "Age(Ka)"
        }
        
        if("Age.PM" %in% names(df_temp) ==T){
          df_temp$Age.PM <- df_temp$Age.PM*0.001
          names(df_temp)[names(df_temp) == "Age.PM"] = "Age.PM(Ka)"
        }
        
        df <- df_temp
        
      }else if(substr(subsplmetdata(), nchar(subsplmetdata())-2, nchar(subsplmetdata())) == "003"){
        
        # method 003 --------------------------------------------------------------
        
        load("RData/OrgRData/value_kar.RData")
        if(file.exists("RData/UserRData/value_kar_custom.RData")){
          load("RData/UserRData/value_kar_custom.RData")
          value_kar <- c(value_kar, value_kar_custom)
        }
        
        df_temp<- as.data.frame(value_kar[[paste0(subsplmetdata(), ".xlsx")]][-c(1)])
        
        if("Delta.Radiogenic.Ar40" %in% names(df_temp) == T){
          df_temp$Delta.Radiogenic.Ar40 <- format(df_temp$Delta.Radiogenic.Ar40, scientific = F) 
        }
        
        if("Radiogenic.Ar36" %in% names(df_temp) == T){
          df_temp$Radiogenic.Ar36 <- format(df_temp$Radiogenic.Ar36, scientific = F)
        }
        
        if("Delta.Radiogenic.Ar36" %in% names(df_temp) == T){
          df_temp$Delta.Radiogenic.Ar36 <- format(df_temp$Delta.Radiogenic.Ar36, scientific = F)
        }
        
        if("Age" %in% names(df_temp) ==T){
          df_temp$Age <- df_temp$Age*0.000001
          names(df_temp)[names(df_temp) == "Age"] = "Age(Ma)"
        }
        
        if("Age.PM" %in% names(df_temp) ==T){
          df_temp$Age.PM <- df_temp$Age.PM*0.000001
          names(df_temp)[names(df_temp) == "Age.PM"] = "Age.PM(Ma)"
        }
        
        df <- df_temp
        
      }else if(substr(subsplmetdata(), nchar(subsplmetdata())-2, nchar(subsplmetdata())) == "004"){
        
        # method 004 --------------------------------------------------------------
        
        load("RData/OrgRData/value_luhf.RData")
        if(file.exists("RData/UserRData/value_luhf_custom.RData")){
          load("RData/UserRData/value_luhf_custom.RData")
          value_luhf <- c(value_luhf, value_luhf_custom)
        }
        
        df_temp<- as.data.frame(value_luhf[[paste0(subsplmetdata(), ".xlsx")]][-c(1)])
        
        if("Age" %in% names(df_temp) ==T){
          df_temp$Age <- df_temp$Age*0.000001
          names(df_temp)[names(df_temp) == "Age"] = "Age(Ma)"
        }
        
        if("TDM" %in% names(df_temp) ==T){
          df_temp$TDM <- df_temp$TDM*0.000001
          names(df_temp)[names(df_temp) == "TDM"] = "TDM(Ma)"
        }
        
        if("TDMc" %in% names(df_temp) ==T){
          df_temp$TDMc <- df_temp$TDMc*0.000001
          names(df_temp)[names(df_temp) == "TDMc"] = "TDMc(Ma)"
        }
        
        if("TDM2" %in% names(df_temp) ==T){
          df_temp$TDM2 <- df_temp$TDM2*0.000001
          names(df_temp)[names(df_temp) == "TDM2"] = "TDM2(Ma)"
        }
        
        df <- df_temp
        
      }else if(substr(subsplmetdata(), nchar(subsplmetdata())-2, nchar(subsplmetdata())) == "005"){
        
        # method 005 --------------------------------------------------------------
        
        load("RData/OrgRData/value_osl.RData")
        if(file.exists("RData/UserRData/value_osl_custom.RData")){
          load("RData/UserRData/value_osl_custom.RData")
          value_osl <- c(value_osl, value_osl_custom)
        }
        
        df_temp<- as.data.frame(value_osl[[paste0(subsplmetdata(), ".xlsx")]][-c(1)])
        
        if("Age" %in% names(df_temp) ==T){
          df_temp$Age <- df_temp$Age*0.001
          names(df_temp)[names(df_temp) == "Age"] = "Age(Ka)"
        }
        
        if("Age.PM" %in% names(df_temp) ==T){
          df_temp$Age.PM <- df_temp$Age.PM*0.001
          names(df_temp)[names(df_temp) == "Age.PM"] = "Age.PM(Ka)"
        }
        
        if("Age.SD" %in% names(df_temp) ==T){
          df_temp$Age.SD <- df_temp$Age.SD*0.001
          names(df_temp)[names(df_temp) == "Age.SD"] = "Age.SD(Ka)"
        }
        
        if("Age.SE" %in% names(df_temp) ==T){
          df_temp$Age.SE <- df_temp$Age.SE*0.001
          names(df_temp)[names(df_temp) == "Age.SE"] = "Age.SE(Ka)"
        }
        
        df <- df_temp
        
      }else if(substr(subsplmetdata(), nchar(subsplmetdata())-2, nchar(subsplmetdata())) == "006"){
        
        # method 006 --------------------------------------------------------------
        
        load("RData/OrgRData/value_smnd.RData")
        if(file.exists("RData/UserRData/value_smnd_custom.RData")){
          load("RData/UserRData/value_smnd_custom.RData")
          value_smnd <- c(value_smnd, value_smnd_custom)
        }
        
        df_temp<- as.data.frame(value_smnd[[paste0(subsplmetdata(), ".xlsx")]][-c(1)])
        
        if("TDM" %in% names(df_temp) ==T){
          df_temp$TDM <- df_temp$TDM*0.000001
          names(df_temp)[names(df_temp) == "TDM"] = "TDM(Ma)"
        }
        
        if("TDM2" %in% names(df_temp) ==T){
          df_temp$TDM2 <- df_temp$TDM2*0.000001
          names(df_temp)[names(df_temp) == "TDM2"] = "TDM2(Ma)"
        }
        
        df <- df_temp
        
      }else if(substr(subsplmetdata(), nchar(subsplmetdata())-2, nchar(subsplmetdata())) == "007"){
        
        # method 007 --------------------------------------------------------------
        load("RData/OrgRData/value_upb.RData")
        if(file.exists("RData/UserRData/value_upb_custom.RData")){
          load("RData/UserRData/value_upb_custom.RData")  
          value_upb <- c(value_upb, value_upb_custom)
        }
        
        df_temp<- as.data.frame(value_upb[[paste0(subsplmetdata(), ".xlsx")]][-c(1)])
        
        if("U" %in% names(df_temp) ==T){
          df_temp$U <- format(round(df_temp$U, digits = 0), nsmall = 0, scientific = F)
        }
        
        if("Th" %in% names(df_temp) ==T){
          df_temp$Th <- format(round(df_temp$Th, digits = 0), nsmall = 0, scientific = F)
        }
        
        if("Th_U" %in% names(df_temp) ==T){
          df_temp$Th_U <- format(round(df_temp$Th_U, digits = 2), nsmall = 2, scientific = F)
        }
        
        if("Th_U.Per" %in% names(df_temp) ==T){
          df_temp$Th_U.Per <- format(round(df_temp$Th_U.Per, digits = 2), nsmall = 2, scientific = F)
        }
        
        if("Th_U.PM.Per" %in% names(df_temp) ==T){
          df_temp$Th_U.PM.Per <- format(round(df_temp$Th_U.PM.Per, digits = 2), nsmall = 2, scientific = F)
        }
        
        ## pb series ---------------------------------------------------------------
        
        if("Pb" %in% names(df_temp) ==T){
          df_temp$Pb <- format(round(df_temp$Pb, digits = 2), nsmall = 2, scientific = F)
        }
        
        if("Pb206" %in% names(df_temp) ==T){
          df_temp$Pb206 <- format(round(df_temp$Pb206, digits = 2), nsmall = 2, scientific = F)
        }
        
        if("Pb206.PM.Per" %in% names(df_temp) ==T){
          df_temp$Pb206.PM.Per <- format(round(df_temp$Pb206.PM.Per, digits = 2), nsmall = 2, scientific = F)
        }
        
        if("Pbc" %in% names(df_temp) ==T){
          df_temp$Pbc <- format(round(df_temp$Pbc, digits = 2), nsmall = 2, scientific = F)
        }
        
        if("Pbr" %in% names(df_temp) ==T){
          df_temp$Pbr <- format(round(df_temp$Pbr, digits = 2), nsmall = 2, scientific = F)
        }
        
        
        # Pb204 -------------------------------------------------------------------
        
        if("Pb204_Pb206" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb204_Pb206)) >= 5){
            df_temp$Pb204_Pb206 <- format(round(df_temp$Pb204_Pb206, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb204_Pb206)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb204_Pb206)))
            df_temp$Pb204_Pb206 <- format(round(df_temp$Pb204_Pb206,  digits = num), nsmall = num)
          }}
        
        if("Pb204_Pb206.PM" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb204_Pb206.PM)) >= 5){
            df_temp$Pb204_Pb206.PM <- format(round(df_temp$Pb204_Pb206.PM, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb204_Pb206.PM)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb204_Pb206.PM)))
            df_temp$Pb204_Pb206.PM <- format(round(df_temp$Pb204_Pb206.PM,  digits = num), nsmall = num)
          }}
        
        if("Pb204_Pb206.PM.Per" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb204_Pb206.PM.Per)) >= 5){
            df_temp$Pb204_Pb206.PM.Per <- format(round(df_temp$Pb204_Pb206.PM.Per, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb204_Pb206.PM.Per)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb204_Pb206.PM.Per)))
            df_temp$Pb204_Pb206.PM.Per <- format(round(df_temp$Pb204_Pb206.PM.Per,  digits = num), nsmall = num)
          }}
        
        # Pb206 -------------------------------------------------------------------
        
        if("Pb206_Th232" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb206_Th232)) >= 5){
            df_temp$Pb206_Th232 <- format(round(df_temp$Pb206_Th232, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb206_Th232)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb206_Th232)))
            df_temp$Pb206_Th232 <- format(round(df_temp$Pb206_Th232,  digits = num), nsmall = num)
          }}
        
        
        if("Pb206_Th232.SD" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb206_Th232.SD)) >= 5){
            df_temp$Pb206_Th232.SD <- format(round(df_temp$Pb206_Th232.SD, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb206_Th232.SD)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb206_Th232.SD)))
            df_temp$Pb206_Th232.SD <- format(round(df_temp$Pb206_Th232.SD,  digits = num), nsmall = num)
          }}
        
        if("Pb206_U238" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb206_U238)) >= 5){
            df_temp$Pb206_U238 <- format(round(df_temp$Pb206_U238, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb206_U238)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb206_U238)))
            df_temp$Pb206_U238 <- format(round(df_temp$Pb206_U238,  digits = num), nsmall = num)
          }}
        
        if("Pb206_U238.PM" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb206_U238.PM)) >= 5){
            df_temp$Pb206_U238.PM <- format(round(df_temp$Pb206_U238.PM, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb206_U238.PM)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb206_U238.PM)))
            df_temp$Pb206_U238.PM <- format(round(df_temp$Pb206_U238.PM,  digits = num), nsmall = num)
          }}
        
        if("Pb206_U238.PM.Per" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb206_U238.PM.Per)) >= 5){
            df_temp$Pb206_U238.PM.Per <- format(round(df_temp$Pb206_U238.PM.Per, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb206_U238.PM.Per)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb206_U238.PM.Per)))
            df_temp$Pb206_U238.PM.Per <- format(round(df_temp$Pb206_U238.PM.Per,  digits = num), nsmall = num)
          }}
        
        if("Pb206_U238.SD" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb206_U238.SD)) >= 5){
            df_temp$Pb206_U238.SD <- format(round(df_temp$Pb206_U238.SD, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb206_U238.SD)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb206_U238.SD)))
            df_temp$Pb206_U238.SD <- format(round(df_temp$Pb206_U238.SD,  digits = num), nsmall = num)
          }}
        
        if("Pb206_U238.2SD" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb206_U238.2SD)) >= 5){
            df_temp$Pb206_U238.2SD <- format(round(df_temp$Pb206_U238.2SD, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb206_U238.2SD)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb206_U238.2SD)))
            df_temp$Pb206_U238.2SD <- format(round(df_temp$Pb206_U238.2SD,  digits = num), nsmall = num)
          }}
        
        if("Pb206_U238.2SE" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb206_U238.2SE)) >= 5){
            df_temp$Pb206_U238.2SE <- format(round(df_temp$Pb206_U238.2SE, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb206_U238.2SE)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb206_U238.2SE)))
            df_temp$Pb206_U238.2SE <- format(round(df_temp$Pb206_U238.2SE,  digits = num), nsmall = num)
          }}
        
        if("Pbr206_U238" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pbr206_U238)) >= 5){
            df_temp$Pbr206_U238 <- format(round(df_temp$Pbr206_U238, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pbr206_U238)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pbr206_U238)))
            df_temp$Pbr206_U238 <- format(round(df_temp$Pbr206_U238,  digits = num), nsmall = num)
          }}
        
        if("Pbr206_U238.PM" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pbr206_U238.PM)) >= 5){
            df_temp$Pbr206_U238.PM <- format(round(df_temp$Pbr206_U238.PM, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pbr206_U238.PM)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pbr206_U238.PM)))
            df_temp$Pbr206_U238.PM <- format(round(df_temp$Pbr206_U238.PM,  digits = num), nsmall = num)
          }}
        
        if("Pbr206_U238.PM.Per" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pbr206_U238.PM.Per)) >= 5){
            df_temp$Pbr206_U238.PM.Per <- format(round(df_temp$Pbr206_U238.PM.Per, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pbr206_U238.PM.Per)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pbr206_U238.PM.Per)))
            df_temp$Pbr206_U238.PM.Per <- format(round(df_temp$Pbr206_U238.PM.Per,  digits = num), nsmall = num)
          }}
        
        # U238 --------------------------------------------------------------------
        
        if("U238_Pb206" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$U238_Pb206)) >= 5){
            df_temp$U238_Pb206 <- format(round(df_temp$U238_Pb206, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$U238_Pb206)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$U238_Pb206)))
            df_temp$U238_Pb206 <- format(round(df_temp$U238_Pb206,  digits = num), nsmall = num)
          }}
        
        
        if("U238_Pb206.Per" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$U238_Pb206.Per)) >= 5){
            df_temp$U238_Pb206.Per <- format(round(df_temp$U238_Pb206.Per, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$U238_Pb206.Per)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$U238_Pb206.Per)))
            df_temp$U238_Pb206.Per <- format(round(df_temp$U238_Pb206.Per,  digits = num), nsmall = num)
          }}
        
        if("U238_Pb206.PM" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$U238_Pb206.PM)) >= 5){
            df_temp$U238_Pb206.PM <- format(round(df_temp$U238_Pb206.PM, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$U238_Pb206.PM)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$U238_Pb206.PM)))
            df_temp$U238_Pb206.PM <- format(round(df_temp$U238_Pb206.PM,  digits = num), nsmall = num)
          }}
        
        if("U238_Pb206.PM.Per" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$U238_Pb206.PM.Per)) >= 5){
            df_temp$U238_Pb206.PM.Per <- format(round(df_temp$U238_Pb206.PM.Per, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$U238_Pb206.PM.Per)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$U238_Pb206.PM.Per)))
            df_temp$U238_Pb206.PM.Per <- format(round(df_temp$U238_Pb206.PM.Per,  digits = num), nsmall = num)
          }}
        
        if("U238_Pb206.SD" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$U238_Pb206.SD)) >= 5){
            df_temp$U238_Pb206.SD <- format(round(df_temp$U238_Pb206.SD, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$U238_Pb206.SD)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$U238_Pb206.SD)))
            df_temp$U238_Pb206.SD <- format(round(df_temp$U238_Pb206.SD,  digits = num), nsmall = num)
          }}
        
        if("U238_Pbr206" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$U238_Pbr206)) >= 5){
            df_temp$U238_Pbr206 <- format(round(df_temp$U238_Pbr206, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$U238_Pbr206)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$U238_Pbr206)))
            df_temp$U238_Pbr206 <- format(round(df_temp$U238_Pbr206,  digits = num), nsmall = num)
          }}
        
        
        if("U238_Pbr206.PM" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$U238_Pbr206.PM)) >= 5){
            df_temp$U238_Pbr206.PM <- format(round(df_temp$U238_Pbr206.PM, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$U238_Pbr206.PM)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$U238_Pbr206.PM)))
            df_temp$U238_Pbr206.PM <- format(round(df_temp$U238_Pbr206.PM,  digits = num), nsmall = num)
          }}
        
        if("U238_Pbr206.PM.Per" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$U238_Pbr206.PM.Per)) >= 5){
            df_temp$U238_Pbr206.PM.Per <- format(round(df_temp$U238_Pbr206.PM.Per, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$U238_Pbr206.PM.Per)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$U238_Pbr206.PM.Per)))
            df_temp$U238_Pbr206.PM.Per <- format(round(df_temp$U238_Pbr206.PM.Per,  digits = num), nsmall = num)
          }}
        
        # Pb207 -------------------------------------------------------------------
        
        if("Pb207_Pb206" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb207_Pb206)) >= 5){
            df_temp$Pb207_Pb206 <- format(round(df_temp$Pb207_Pb206, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb207_Pb206)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb207_Pb206)))
            df_temp$Pb207_Pb206 <- format(round(df_temp$Pb207_Pb206,  digits = num), nsmall = num)
          }}
        
        if("Pb207_Pb206.Per" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb207_Pb206.Per)) >= 5){
            df_temp$Pb207_Pb206.Per <- format(round(df_temp$Pb207_Pb206.Per, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb207_Pb206.Per)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb207_Pb206.Per)))
            df_temp$Pb207_Pb206.Per <- format(round(df_temp$Pb207_Pb206.Per,  digits = num), nsmall = num)
          }}
        
        if("Pb207_Pb206.PM" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb207_Pb206.PM)) >= 5){
            df_temp$Pb207_Pb206.PM <- format(round(df_temp$Pb207_Pb206.PM, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb207_Pb206.PM)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb207_Pb206.PM)))
            df_temp$Pb207_Pb206.PM <- format(round(df_temp$Pb207_Pb206.PM,  digits = num), nsmall = num)
          }}
        
        if("Pb207_Pb206.PM.Per" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb207_Pb206.PM.Per)) >= 5){
            df_temp$Pb207_Pb206.PM.Per <- format(round(df_temp$Pb207_Pb206.PM.Per, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb207_Pb206.PM.Per)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb207_Pb206.PM.Per)))
            df_temp$Pb207_Pb206.PM.Per <- format(round(df_temp$Pb207_Pb206.PM.Per,  digits = num), nsmall = num)
          }}
        
        if("Pb207_Pb206.SD" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb207_Pb206.SD)) >= 5){
            df_temp$Pb207_Pb206.SD <- format(round(df_temp$Pb207_Pb206.SD, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb207_Pb206.SD)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb207_Pb206.SD)))
            df_temp$Pb207_Pb206.SD <- format(round(df_temp$Pb207_Pb206.SD,  digits = num), nsmall = num)
          }}
        
        if("Pb207_Pb206.2SD" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb207_Pb206.2SD)) >= 5){
            df_temp$Pb207_Pb206.2SD <- format(round(df_temp$Pb207_Pb206.2SD, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb207_Pb206.2SD)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb207_Pb206.2SD)))
            df_temp$Pb207_Pb206.2SD <- format(round(df_temp$Pb207_Pb206.2SD,  digits = num), nsmall = num)
          }}
        
        if("Pb207_Pb206.2SE" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb207_Pb206.2SE)) >= 5){
            df_temp$Pb207_Pb206.2SE <- format(round(df_temp$Pb207_Pb206.2SE, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb207_Pb206.2SE)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb207_Pb206.2SE)))
            df_temp$Pb207_Pb206.2SE <- format(round(df_temp$Pb207_Pb206.2SE,  digits = num), nsmall = num)
          }}
        
        if("Pbr207_Pb206" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pbr207_Pb206)) >= 5){
            df_temp$Pbr207_Pb206 <- format(round(df_temp$Pbr207_Pb206, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pbr207_Pb206)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pbr207_Pb206)))
            df_temp$Pbr207_Pb206 <- format(round(df_temp$Pbr207_Pb206,  digits = num), nsmall = num)
          }}
        
        if("Pbr207_Pb206.PM" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pbr207_Pb206.PM)) >= 5){
            df_temp$Pbr207_Pb206.PM <- format(round(df_temp$Pbr207_Pb206.PM, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pbr207_Pb206.PM)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pbr207_Pb206.PM)))
            df_temp$Pbr207_Pb206.PM <- format(round(df_temp$Pbr207_Pb206.PM,  digits = num), nsmall = num)
          }}
        
        if("Pbr207_Pb206.PM.Per" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pbr207_Pb206.PM.Per)) >= 5){
            df_temp$Pbr207_Pb206.PM.Per <- format(round(df_temp$Pbr207_Pb206.PM.Per, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pbr207_Pb206.PM.Per)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pbr207_Pb206.PM.Per)))
            df_temp$Pbr207_Pb206.PM.Per <- format(round(df_temp$Pbr207_Pb206.PM.Per,  digits = num), nsmall = num)
          }}
        
        if("Pbr207_Pbr206" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pbr207_Pbr206)) >= 5){
            df_temp$Pbr207_Pbr206 <- format(round(df_temp$Pbr207_Pbr206, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pbr207_Pbr206)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pbr207_Pbr206)))
            df_temp$Pbr207_Pbr206 <- format(round(df_temp$Pbr207_Pbr206,  digits = num), nsmall = num)
          }}
        
        if("Pbr207_Pbr206.PM.Per" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pbr207_Pbr206.PM.Per)) >= 5){
            df_temp$Pbr207_Pbr206.PM.Per <- format(round(df_temp$Pbr207_Pbr206.PM.Per, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pbr207_Pbr206.PM.Per)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pbr207_Pbr206.PM.Per)))
            df_temp$Pbr207_Pbr206.PM.Per <- format(round(df_temp$Pbr207_Pbr206.PM.Per,  digits = num), nsmall = num)
          }}
        
        if("Pb207_U235" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb207_U235)) >= 5){
            df_temp$Pb207_U235 <- format(round(df_temp$Pb207_U235, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb207_U235)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb207_U235)))
            df_temp$Pb207_U235 <- format(round(df_temp$Pb207_U235,  digits = num), nsmall = num)
          }}
        
        if("Pb207_U235.PM.Per" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb207_U235.PM.Per)) >= 5){
            df_temp$Pb207_U235.PM.Per <- format(round(df_temp$Pb207_U235.PM.Per, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb207_U235.PM.Per)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb207_U235.PM.Per)))
            df_temp$Pb207_U235.PM.Per <- format(round(df_temp$Pb207_U235.PM.Per,  digits = num), nsmall = num)
          }}
        
        if("Pb207_U235.SD" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb207_U235.SD)) >= 5){
            df_temp$Pb207_U235.SD <- format(round(df_temp$Pb207_U235.SD, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb207_U235.SD)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb207_U235.SD)))
            df_temp$Pb207_U235.SD <- format(round(df_temp$Pb207_U235.SD,  digits = num), nsmall = num)
          }}
        
        if("Pb207_U235.2SD" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb207_U235.2SD)) >= 5){
            df_temp$Pb207_U235.2SD <- format(round(df_temp$Pb207_U235.2SD, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb207_U235.2SD)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb207_U235.2SD)))
            df_temp$Pb207_U235.2SD <- format(round(df_temp$Pb207_U235.2SD,  digits = num), nsmall = num)
          }}
        
        if("Pb207_U235.2SE" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb207_U235.2SE)) >= 5){
            df_temp$Pb207_U235.2SE <- format(round(df_temp$Pb207_U235.2SE, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb207_U235.2SE)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb207_U235.2SE)))
            df_temp$Pb207_U235.2SE <- format(round(df_temp$Pb207_U235.2SE,  digits = num), nsmall = num)
          }}
        
        if("Pbr207_U235" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pbr207_U235)) >= 5){
            df_temp$Pbr207_U235 <- format(round(df_temp$Pbr207_U235, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pbr207_U235)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pbr207_U235)))
            df_temp$Pbr207_U235 <- format(round(df_temp$Pbr207_U235,  digits = num), nsmall = num)
          }}
        
        if("Pbr207_U235.PM" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pbr207_U235.PM)) >= 5){
            df_temp$Pbr207_U235.PM <- format(round(df_temp$Pbr207_U235.PM, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pbr207_U235.PM)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pbr207_U235.PM)))
            df_temp$Pbr207_U235.PM <- format(round(df_temp$Pbr207_U235.PM,  digits = num), nsmall = num)
          }}
        
        if("Pbr207_U235.PM.Per" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pbr207_U235.PM.Per)) >= 5){
            df_temp$Pbr207_U235.PM.Per <- format(round(df_temp$Pbr207_U235.PM.Per, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pbr207_U235.PM.Per)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pbr207_U235.PM.Per)))
            df_temp$Pbr207_U235.PM.Per <- format(round(df_temp$Pbr207_U235.PM.Per,  digits = num), nsmall = num)
          }}
        
        # Pb208 -------------------------------------------------------------------
        
        if("Pb208_Pb206" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb208_Pb206)) >= 5){
            df_temp$Pb208_Pb206 <- format(round(df_temp$Pb208_Pb206, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb208_Pb206)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb208_Pb206)))
            df_temp$Pb208_Pb206 <- format(round(df_temp$Pb208_Pb206,  digits = num), nsmall = num)
          }}
        
        if("Pb208_Pb206.PM.Per" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb208_Pb206.PM.Per)) >= 5){
            df_temp$Pb208_Pb206.PM.Per <- format(round(df_temp$Pb208_Pb206.PM.Per, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb208_Pb206.PM.Per)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb208_Pb206.PM.Per)))
            df_temp$Pb208_Pb206.PM.Per <- format(round(df_temp$Pb208_Pb206.PM.Per,  digits = num), nsmall = num)
          }}
        
        # Pb206.Age ---------------------------------------------------------------
        
        if("Pb206_U238.Age" %in% names(df_temp) ==T){
          df_temp$Pb206_U238.Age <- df_temp$Pb206_U238.Age*0.000001 
          df_temp$Pb206_U238.Age <- format(round(df_temp$Pb206_U238.Age, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb206_U238.Age"] = "Pb206_U238.Age(Ma)"
        }
        
        if("Pb206_U238.Age.PM" %in% names(df_temp) ==T){
          df_temp$Pb206_U238.Age.PM <- df_temp$Pb206_U238.Age.PM*0.000001 
          df_temp$Pb206_U238.Age.PM <- format(round(df_temp$Pb206_U238.Age.PM, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb206_U238.Age.PM"] = "Pb206_U238.Age.PM(Ma)"
        }
        
        if("Pb206_U238.Age.PM.Per" %in% names(df_temp) ==T){
          df_temp$Pb206_U238.Age.PM.Per <- df_temp$Pb206_U238.Age.PM.Per*0.000001 
          df_temp$Pb206_U238.Age.PM.Per <- format(round(df_temp$Pb206_U238.Age.PM.Per, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb206_U238.Age.PM.Per"] = "Pb206_U238.Age.PM.Per(Ma)"
        }
        
        if("Pb206_U238.Age.SD" %in% names(df_temp) ==T){
          df_temp$Pb206_U238.Age.SD <- df_temp$Pb206_U238.Age.SD*0.000001 
          df_temp$Pb206_U238.Age.SD <- format(round(df_temp$Pb206_U238.Age.SD, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb206_U238.Age.SD"] = "Pb206_U238.Age.SD(Ma)"
        }
        
        if("Pb206_U238.Age.2SD" %in% names(df_temp) ==T){
          df_temp$Pb206_U238.Age.2SD <- df_temp$Pb206_U238.Age.2SD*0.000001 
          df_temp$Pb206_U238.Age.2SD <- format(round(df_temp$Pb206_U238.Age.2SD, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb206_U238.Age.2SD"] = "Pb206_U238.Age.2SD(Ma)"
        }
        
        if("Pb206_U238.Age.2SE" %in% names(df_temp) ==T){
          df_temp$Pb206_U238.Age.2SE <- df_temp$Pb206_U238.Age.2SE*0.000001 
          df_temp$Pb206_U238.Age.2SE <- format(round(df_temp$Pb206_U238.Age.2SE, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb206_U238.Age.2SE"] = "Pb206_U238.Age.2SE(Ma)"
        }
        
        if("Pbr206_U238.Age" %in% names(df_temp) ==T){
          df_temp$Pbr206_U238.Age <- df_temp$Pbr206_U238.Age*0.000001 
          df_temp$Pbr206_U238.Age <- format(round(df_temp$Pbr206_U238.Age, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pbr206_U238.Age"] = "Pbr206_U238.Age(Ma)"
        }
        
        if("Pbr206_U238.Age.PM" %in% names(df_temp) ==T){
          df_temp$Pbr206_U238.Age.PM <- df_temp$Pbr206_U238.Age.PM*0.000001 
          df_temp$Pbr206_U238.Age.PM <- format(round(df_temp$Pbr206_U238.Age.PM, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pbr206_U238.Age.PM"] = "Pbr206_U238.Age.PM(Ma)"
        }
        
        if("Pbr206_U238.Age.PM.Per" %in% names(df_temp) ==T){
          df_temp$Pbr206_U238.Age.PM.Per <- df_temp$Pbr206_U238.Age.PM.Per*0.000001 
          df_temp$Pbr206_U238.Age.PM.Per <- format(round(df_temp$Pbr206_U238.Age.PM.Per, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pbr206_U238.Age.PM.Per"] = "Pbr206_U238.Age.PM.Per(Ma)"
        }
        
        # U238 Age--------------------------------------------------------------------
        
        if("U238_Pb206.Age" %in% names(df_temp) ==T){
          df_temp$U238_Pb206.Age <- df_temp$U238_Pb206.Age*0.000001 
          df_temp$U238_Pb206.Age <- format(round(df_temp$U238_Pb206.Age, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "U238_Pb206.Age"] = "U238_Pb206.Age(Ma)"
        }
        
        if("U238_Pb206.Age.PM.Per" %in% names(df_temp) ==T){
          df_temp$U238_Pb206.Age.PM.Per <- df_temp$U238_Pb206.Age.PM.Per*0.000001 
          df_temp$U238_Pb206.Age.PM.Per <- format(round(df_temp$U238_Pb206.Age.PM.Per, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "U238_Pb206.Age.PM.Per"] = "U238_Pb206.Age.PM.Per(Ma)"
        }
        
        # Pb207 Age ---------------------------------------------------------------
        
        if("Pb207_Pb206.Age" %in% names(df_temp) ==T){
          df_temp$Pb207_Pb206.Age <- df_temp$Pb207_Pb206.Age*0.000001 
          df_temp$Pb207_Pb206.Age <- format(round(df_temp$Pb207_Pb206.Age, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb207_Pb206.Age"] = "Pb207_Pb206.Age(Ma)"
        }
        
        if("Pb207_Pb206.Age.PM" %in% names(df_temp) ==T){
          df_temp$Pb207_Pb206.Age.PM <- df_temp$Pb207_Pb206.Age.PM*0.000001 
          df_temp$Pb207_Pb206.Age.PM <- format(round(df_temp$Pb207_Pb206.Age.PM, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb207_Pb206.Age.PM"] = "Pb207_Pb206.Age.PM(Ma)"
        }
        
        if("Pb207_Pb206.Age.PM.Per" %in% names(df_temp) ==T){
          df_temp$Pb207_Pb206.Age.PM.Per <- df_temp$Pb207_Pb206.Age.PM.Per*0.000001 
          df_temp$Pb207_Pb206.Age.PM.Per <- format(round(df_temp$Pb207_Pb206.Age.PM.Per, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb207_Pb206.Age.PM.Per"] = "Pb207_Pb206.Age.PM.Per(Ma)"
        }
        
        if("Pb207_Pb206.Age.SD" %in% names(df_temp) ==T){
          df_temp$Pb207_Pb206.Age.SD <- df_temp$Pb207_Pb206.Age.SD*0.000001 
          df_temp$Pb207_Pb206.Age.SD <- format(round(df_temp$Pb207_Pb206.Age.SD, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb207_Pb206.Age.SD"] = "Pb207_Pb206.Age.SD(Ma)"
        }
        
        if("Pb207_Pb206.Age.2SD" %in% names(df_temp) ==T){
          df_temp$Pb207_Pb206.Age.2SD <- df_temp$Pb207_Pb206.Age.2SD*0.000001 
          df_temp$Pb207_Pb206.Age.2SD <- format(round(df_temp$Pb207_Pb206.Age.2SD, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb207_Pb206.Age.2SD"] = "Pb207_Pb206.Age.2SD(Ma)"
        }
        
        if("Pb207_Pb206.Age.2SE" %in% names(df_temp) ==T){
          df_temp$Pb207_Pb206.Age.2SE <- df_temp$Pb207_Pb206.Age.2SE*0.000001 
          df_temp$Pb207_Pb206.Age.2SE <- format(round(df_temp$Pb207_Pb206.Age.2SE, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb207_Pb206.Age.2SE"] = "Pb207_Pb206.Age.2SE(Ma)"
        }
        
        if("Pbr207_Pbr206.Age" %in% names(df_temp) ==T){
          df_temp$Pbr207_Pbr206.Age <- df_temp$Pbr207_Pbr206.Age*0.000001 
          df_temp$Pbr207_Pbr206.Age <- format(round(df_temp$Pbr207_Pbr206.Age, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pbr207_Pbr206.Age"] = "Pbr207_Pbr206.Age(Ma)"
        }
        
        if("Pbr207_Pbr206.Age.PM" %in% names(df_temp) ==T){
          df_temp$Pbr207_Pbr206.Age.PM <- df_temp$Pbr207_Pbr206.Age.PM*0.000001 
          df_temp$Pbr207_Pbr206.Age.PM <- format(round(df_temp$Pbr207_Pbr206.Age.PM, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pbr207_Pbr206.Age.PM"] = "Pbr207_Pbr206.Age.PM(Ma)"
        }
        
        if("Pbr207_Pbr206.Age.PM.Per" %in% names(df_temp) ==T){
          df_temp$Pbr207_Pbr206.Age.PM.Per <- df_temp$Pbr207_Pbr206.Age.PM.Per*0.000001 
          df_temp$Pbr207_Pbr206.Age.PM.Per <- format(round(df_temp$Pbr207_Pbr206.Age.PM.Per, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pbr207_Pbr206.Age.PM.Per"] = "Pbr207_Pbr206.Age.PM.Per(Ma)"
        }
        
        if("Pb207_U235.Age" %in% names(df_temp) ==T){
          df_temp$Pb207_U235.Age <- df_temp$Pb207_U235.Age*0.000001 
          df_temp$Pb207_U235.Age <- format(round(df_temp$Pb207_U235.Age, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb207_U235.Age"] = "Pb207_U235.Age(Ma)"
        }
        
        if("Pb207_U235.Age.PM" %in% names(df_temp) ==T){
          df_temp$Pb207_U235.Age.PM <- df_temp$Pb207_U235.Age.PM*0.000001 
          df_temp$Pb207_U235.Age.PM <- format(round(df_temp$Pb207_U235.Age.PM, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb207_U235.Age.PM"] = "Pb207_U235.Age.PM(Ma)"
        }
        
        if("Pb207_U235.Age.SD" %in% names(df_temp) ==T){
          df_temp$Pb207_U235.Age.SD <- df_temp$Pb207_U235.Age.SD*0.000001 
          df_temp$Pb207_U235.Age.SD <- format(round(df_temp$Pb207_U235.Age.SD, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb207_U235.Age.SD"] = "Pb207_U235.Age.SD(Ma)"
        }
        
        if("Pb207_U235.Age.2SD" %in% names(df_temp) ==T){
          df_temp$Pb207_U235.Age.2SD <- df_temp$Pb207_U235.Age.2SD*0.000001 
          df_temp$Pb207_U235.Age.2SD <- format(round(df_temp$Pb207_U235.Age.2SD, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb207_U235.Age.2SD"] = "Pb207_U235.Age.2SD(Ma)"
        }
        
        if("Pb207_U235.Age.2SE" %in% names(df_temp) ==T){
          df_temp$Pb207_U235.Age.2SE <- df_temp$Pb207_U235.Age.2SE*0.000001 
          df_temp$Pb207_U235.Age.2SE <- format(round(df_temp$Pb207_U235.Age.2SE, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb207_U235.Age.2SE"] = "Pb207_U235.Age.2SE(Ma)"
        }
        
        # Pb208_Age ---------------------------------------------------------------
        
        if("Pb208_Th232.Age" %in% names(df_temp) ==T){
          df_temp$Pb208_Th232.Age <- df_temp$Pb208_Th232.Age*0.000001 
          df_temp$Pb208_Th232.Age <- format(round(df_temp$Pb208_Th232.Age, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb208_Th232.Age"] = "Pb208_Th232.Age(Ma)"
        }
        
        if("Pb208_Th232.Age.PM" %in% names(df_temp) ==T){
          df_temp$Pb208_Th232.Age.PM <- df_temp$Pb208_Th232.Age.PM*0.000001 
          df_temp$Pb208_Th232.Age.PM <- format(round(df_temp$Pb208_Th232.Age.PM, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb208_Th232.Age.PM"] = "Pb208_Th232.Age.PM(Ma)"
        }
        
        if("Age" %in% names(df_temp) ==T){
          df_temp$Age <- df_temp$Age*0.000001 
          df_temp$Age <- format(round(df_temp$Age, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Age"] = "Age(Ma)"
        }
        
        if("Age.PM" %in% names(df_temp) ==T){
          df_temp$Age.PM <- df_temp$Age.PM*0.000001 
          df_temp$Age.PM <- format(round(df_temp$Age.PM, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Age.PM"] = "Age.PM(Ma)"
        }
        
        
        if("Disc" %in% names(df_temp) == T){
          df_temp$Disc <- format(round(df_temp$Disc, digits = 0), nsmall = 0)
        }
        
        df <- df_temp
        
      }
    }
    
    })
  
  output$subsplmettbl <- renderDataTable(
    subsplmettbldata(),
    rownames = F,
    selection = "single"
  )
  
  output$cntiso_met_iso <- renderUI({
    if(length(subsplmettbldata())){
      paste("Isotope Data:", nrow(subsplmettbldata()))  
    }
  })
  
  
  subsplmetrefdata <- reactive({

    refid <- as.character(substr(subsplmetdata(), 1, 9))
  
    load("RData/OrgRData/ref.RData") 
    
    if(file.exists("RData/UserRData/ref_custom.RData")){
      load("RData/UserRData/ref_custom.RData")
      if(nrow(ref_custom) == 0){
        ref
      }else if(nrow(ref_custom) != 0){
        ref <- rbind(ref, ref_custom)
      }
    }
    
    if(length(subsplmetdata())){
      ref <- ref[ref$RefID %in% refid,]
    }
  })
  
  output$subsplmetreftbl <- renderDataTable(
    subsplmetrefdata(),
    rownames = F,
    selection = "single",
    options = list(columnDefs = list(list(width = '500px', targets = c(1)),
                                     list(width = '250px', targets = c(2)),
                                     list(width = '300px', targets = c(3:5)),
                                     list(visible = F, targets = c(0))))
  )
  
  output$cntiso_met_ref <- renderUI({
    if(length(subsplmetrefdata())){
      paste("Reference:", nrow(subsplmetrefdata()))  
    }
  })
  
  savesplmetvalue <- reactive({
    value[[paste0(subsplmetdata(), ".xlsx")]][-c(1)]
  })
  
  
  output$DBsplmet <- downloadHandler(
    filename = function(){
      paste0("Sample_Method_Data.csv")},
    content = function(file){
      write.csv(splmetdata()[,-c(1)], file, row.names = F)
    })
  
  output$DBsplmetvalue <- downloadHandler(
    filename = function(){
      paste0("Sample_Method_Value_Data.csv")},
    content = function(file){
      write.csv(savesplmetvalue(), file, row.names = F)
    })
  

  ## Sample Age ------------------------------------------------------------------------   
  
  ## chrono() ------------------------------------------------------------------------   
  
  eradata <- reactive({
    
    load("RData/OrgRData/age.RData")  
    load("RData/OrgRData/spl.RData") 
    
    if(file.exists("RData/UserRData/spl_custom.RData")){
      load("RData/UserRData/spl_custom.RData")
      load("RData/UserRData/age_custom.RData")
      if(nrow(spl_custom) == 0){
        spl
      }else if(nrow(spl_custom) != 0){
        spl <- rbind(spl, spl_custom)
      }
      
      if(nrow(age_custom) == 0){
        age
      }else if(nrow(age_custom) != 0){
        age <- rbind(age, age_custom)
      }
    }
    
    age$Era <- gsub(" ", "", age$Era)
    age$Period <- gsub(" ", "", age$Period)
    age$Epoch <- gsub(" ", "", age$Epoch)
    
    age$Era <- gsub("[[:space:]]", "", age$Era)
    age$Period <- gsub("[[:space:]]", "", age$Period)
    age$Epoch <- gsub("[[:space:]]", "", age$Epoch)
    
    age <- age %>% 
      mutate(age, ID = paste0(age$RefID, age$Sample_seq, age$Method_cd))
    
    age <- data.table(unique(age))
    
    age <- age[,c(1:4, 8:11)]
    names(age) <- c("RefID", "Sample_seq", "Method_cd", "Sample", "Era","Period","Epoch","ID")
    
    age <- join(age, met_cd, by = "Method_cd")
    spl <- join(age, spl, by = c("RefID", "Sample_seq"))
    spl <- spl[,-c(10)]
    
    
    spl$Longitude <- format(spl$Longitude, digits = 4, nsmall = 4)
    spl$Latitude <- format(spl$Latitude, digits = 4, nsmall = 4)
    
    filter(spl, Era == input$splera)
    
  })
  
  
  observeEvent(eradata(),{
    
    choices <- unique(eradata()$Period)
    updateSelectInput(inputId = "splperi", choices = choices)
    
  })
  
  peridata <- reactive({
    
    req(input$splperi) 
    filter(eradata(), Period == input$splperi)
    
  })
  
  observeEvent(peridata(),{
    
    choices <- unique(peridata()$Epoch)
    updateSelectInput(inputId = "splepo", choices = unique(choices))
    
  })
  
  epodata <- reactive({
    
    req(input$splepo)
    filter(peridata(), Epoch == input$splepo)
    
  })
  
  chronodata <- reactive(
    
    if(!is.null(input$splera) & input$splperi == "NA"){
      eradata()
    }else if(!is.null(input$splperi) & input$splepo == "NA"){
      peridata()
    }else(
      epodata()
    )
  )
  
  
  output$splagetbl <- renderDataTable(
    chronodata(),
    rownames = F,
    options = list(scrollX = T,
                   columnDefs = list(list(visible = F, targets = c(0:2,4:7)))
    ),
    selection = "single"    
  )
  
  output$cntiso_age_spl <- renderUI({
    paste("Sample:", nrow(chronodata()))
  })

  
  ## Sample age subtbl 2 ---------------------------------------------------------------
  
  
  subagedata <- reactive({
    
    if(!is.null(input$splera) & input$splperi == "NA"){
      
      selected_row <- input$splagetbl_rows_selected
      selected_sample <- eradata()[as.integer(selected_row),]$ID
      
    }else if(!is.null(input$splperi) & input$splepo == "NA"){
      
      selected_row <- input$splagetbl_rows_selected
      selected_sample <- peridata()[as.integer(selected_row),]$ID
      
    }else({
      
      selected_row <- input$splagetbl_rows_selected
      selected_sample <- epodata()[as.integer(selected_row),]$ID
      
    })
    
  })
  
  
  ### Sample Age Value Option ----------------------------------------------------------------
  
  subagetbldata <- reactive({
    
    if(length(subagedata())){
      
      
      if(substr(subagedata(), nchar(subagedata())-2, nchar(subagedata())) == "001"){
        
        # method 001 --------------------------------------------------------------
        
        load("RData/OrgRData/value_ar.RData")
        if(file.exists("RData/UserRData/value_ar_custom.RData")){
          load("RData/UserRData/value_ar_custom.RData")
          value_ar <- c(value_ar, value_ar_custom)
        }
        
        df_temp<- as.data.frame(value_ar[[paste0(subagedata(), ".xlsx")]][-c(1)])
        
        if("Plateau.Age" %in% names(df_temp) ==T){
          df_temp$Plateau.Age <- df_temp$Plateau.Age*0.001 
          df_temp$Plateau.Age <- format(round(df_temp$Plateau.Age, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Plateau.Age"] = "Plateau.Age(Ka)"
        }
        
        if("Plateau.Age.SD" %in% names(df_temp) ==T){
          df_temp$Plateau.Age.SD <- df_temp$Plateau.Age.SD*0.001 
          df_temp$Plateau.Age.SD <- format(round(df_temp$Plateau.Age.SD, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Plateau.Age.SD"] = "Plateau.Age.SD(Ka)"
        }
        
        if("Intergrated.Age" %in% names(df_temp) ==T){
          df_temp$Intergrated.Age <- df_temp$Intergrated.Age*0.001 
          df_temp$Intergrated.Age <- format(round(df_temp$Intergrated.Age, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Intergrated.Age"] = "Intergrated.Age(Ka)"
        }
        
        if("Intergrated.Age.SD" %in% names(df_temp) ==T){
          df_temp$Intergrated.Age.SD <- df_temp$Intergrated.Age.SD*0.001 
          df_temp$Intergrated.Age.SD <- format(round(df_temp$Intergrated.Age.SD, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Intergrated.Age.SD"] = "Intergrated.Age.SD(Ka)"
        }
        
        df <- df_temp
        
      }else if(substr(subagedata(), nchar(subagedata())-2, nchar(subagedata())) == "002"){
        
        # method 002 --------------------------------------------------------------
        
        load("RData/OrgRData/value_cosmo.RData")
        if(file.exists("RData/UserRData/value_cosmo_custom.RData")){
          load("RData/UserRData/value_cosmo_custom.RData")
          value_cosmo <- c(value_cosmo, value_cosmo_custom)
        }
        
        df_temp<- as.data.frame(value_cosmo[[paste0(subagedata(), ".xlsx")]][-c(1)])
        
        if("Age" %in% names(df_temp) ==T){
          df_temp$Age <- df_temp$Age*0.001
          names(df_temp)[names(df_temp) == "Age"] = "Age(Ka)"
        }
        
        if("Age.PM" %in% names(df_temp) ==T){
          df_temp$Age.PM <- df_temp$Age.PM*0.001
          names(df_temp)[names(df_temp) == "Age.PM"] = "Age.PM(Ka)"
        }
        
        df <- df_temp
        
        
      }else if(substr(subagedata(), nchar(subagedata())-2, nchar(subagedata())) == "003"){
        
        # method 003 --------------------------------------------------------------
        
        load("RData/OrgRData/value_kar.RData")
        if(file.exists("RData/UserRData/value_kar_custom.RData")){
          load("RData/UserRData/value_kar_custom.RData")
          value_kar <- c(value_kar, value_kar_custom)
        }
        
        df_temp<- as.data.frame(value_kar[[paste0(subagedata(), ".xlsx")]][-c(1)])
        
        if("Delta.Radiogenic.Ar40" %in% names(df_temp) == T){
          df_temp$Delta.Radiogenic.Ar40 <- format(df_temp$Delta.Radiogenic.Ar40, scientific = F) 
        }
        
        if("Radiogenic.Ar36" %in% names(df_temp) == T){
          df_temp$Radiogenic.Ar36 <- format(df_temp$Radiogenic.Ar36, scientific = F)
        }
        
        if("Delta.Radiogenic.Ar36" %in% names(df_temp) == T){
          df_temp$Delta.Radiogenic.Ar36 <- format(df_temp$Delta.Radiogenic.Ar36, scientific = F)
        }
        
        if("Age" %in% names(df_temp) ==T){
          df_temp$Age <- df_temp$Age*0.000001
          names(df_temp)[names(df_temp) == "Age"] = "Age(Ma)"
        }
        
        if("Age.PM" %in% names(df_temp) ==T){
          df_temp$Age.PM <- df_temp$Age.PM*0.000001
          names(df_temp)[names(df_temp) == "Age.PM"] = "Age.PM(Ma)"
        }
        
        df <- df_temp
        
      }else if(substr(subagedata(), nchar(subagedata())-2, nchar(subagedata())) == "004"){
        
        # method 004 --------------------------------------------------------------
        
        load("RData/OrgRData/value_luhf.RData")
        if(file.exists("RData/UserRData/value_luhf_custom.RData")){
          load("RData/UserRData/value_luhf_custom.RData")
          value_luhf <- c(value_luhf, value_luhf_custom)
        }
        
        df_temp<- as.data.frame(value_luhf[[paste0(subagedata(), ".xlsx")]][-c(1)])
        
        if("Age" %in% names(df_temp) ==T){
          df_temp$Age <- df_temp$Age*0.000001
          names(df_temp)[names(df_temp) == "Age"] = "Age(Ma)"
        }
        
        if("TDM" %in% names(df_temp) ==T){
          df_temp$TDM <- df_temp$TDM*0.000001
          names(df_temp)[names(df_temp) == "TDM"] = "TDM(Ma)"
        }
        
        if("TDMc" %in% names(df_temp) ==T){
          df_temp$TDMc <- df_temp$TDMc*0.000001
          names(df_temp)[names(df_temp) == "TDMc"] = "TDMc(Ma)"
        }
        
        if("TDM2" %in% names(df_temp) ==T){
          df_temp$TDM2 <- df_temp$TDM2*0.000001
          names(df_temp)[names(df_temp) == "TDM2"] = "TDM2(Ma)"
        }
        
        df <- df_temp
        
      }else if(substr(subagedata(), nchar(subagedata())-2, nchar(subagedata())) == "005"){
        
        # method 005 --------------------------------------------------------------
        
        load("RData/OrgRData/value_osl.RData")
        if(file.exists("RData/UserRData/value_osl_custom.RData")){
          load("RData/UserRData/value_osl_custom.RData")
          value_osl <- c(value_osl, value_osl_custom)
        }
        
        df_temp<- as.data.frame(value_osl[[paste0(subagedata(), ".xlsx")]][-c(1)])
        
        if("Age" %in% names(df_temp) ==T){
          df_temp$Age <- df_temp$Age*0.001
          names(df_temp)[names(df_temp) == "Age"] = "Age(Ka)"
        }
        
        if("Age.PM" %in% names(df_temp) ==T){
          df_temp$Age.PM <- df_temp$Age.PM*0.001
          names(df_temp)[names(df_temp) == "Age.PM"] = "Age.PM(Ka)"
        }
        
        if("Age.SD" %in% names(df_temp) ==T){
          df_temp$Age.SD <- df_temp$Age.SD*0.001
          names(df_temp)[names(df_temp) == "Age.SD"] = "Age.SD(Ka)"
        }
        
        if("Age.SE" %in% names(df_temp) ==T){
          df_temp$Age.SE <- df_temp$Age.SE*0.001
          names(df_temp)[names(df_temp) == "Age.SE"] = "Age.SE(Ka)"
        }
        
        df <- df_temp
        
      }else if(substr(subagedata(), nchar(subagedata())-2, nchar(subagedata())) == "006"){
        
        # method 006 --------------------------------------------------------------
        
        load("RData/OrgRData/value_smnd.RData")
        if(file.exists("RData/UserRData/value_smnd_custom.RData")){
          load("RData/UserRData/value_smnd_custom.RData")
          value_smnd <- c(value_smnd, value_smnd_custom)
        }
        
        df_temp<- as.data.frame(value_smnd[[paste0(subagedata(), ".xlsx")]][-c(1)])
        
        if("TDM" %in% names(df_temp) ==T){
          df_temp$TDM <- df_temp$TDM*0.000001
          names(df_temp)[names(df_temp) == "TDM"] = "TDM(Ma)"
        }
        
        if("TDM2" %in% names(df_temp) ==T){
          df_temp$TDM2 <- df_temp$TDM2*0.000001
          names(df_temp)[names(df_temp) == "TDM2"] = "TDM2(Ma)"
        }
        
        df <- df_temp
        
      }else if(substr(subagedata(), nchar(subagedata())-2, nchar(subagedata())) == "007"){
        
        # method 007 --------------------------------------------------------------
        
        load("RData/OrgRData/value_upb.RData")
        if(file.exists("RData/UserRData/value_upb_custom.RData")){
          load("RData/UserRData/value_upb_custom.RData")  
          value_upb <- c(value_upb, value_upb_custom)
        }
        
        df_temp<- as.data.frame(value_upb[[paste0(subagedata(), ".xlsx")]][-c(1)])
        
        if("U" %in% names(df_temp) ==T){
          df_temp$U <- format(round(df_temp$U, digits = 0), nsmall = 0, scientific = F)
        }
        
        if("Th" %in% names(df_temp) ==T){
          df_temp$Th <- format(round(df_temp$Th, digits = 0), nsmall = 0, scientific = F)
        }
        
        if("Th_U" %in% names(df_temp) ==T){
          df_temp$Th_U <- format(round(df_temp$Th_U, digits = 2), nsmall = 2, scientific = F)
        }
        
        if("Th_U.Per" %in% names(df_temp) ==T){
          df_temp$Th_U.Per <- format(round(df_temp$Th_U.Per, digits = 2), nsmall = 2, scientific = F)
        }
        
        if("Th_U.PM.Per" %in% names(df_temp) ==T){
          df_temp$Th_U.PM.Per <- format(round(df_temp$Th_U.PM.Per, digits = 2), nsmall = 2, scientific = F)
        }
        
        
        ## pb series ---------------------------------------------------------------
        
        if("Pb" %in% names(df_temp) ==T){
          df_temp$Pb <- format(round(df_temp$Pb, digits = 2), nsmall = 2, scientific = F)
        }
        
        if("Pb206" %in% names(df_temp) ==T){
          df_temp$Pb206 <- format(round(df_temp$Pb206, digits = 2), nsmall = 2, scientific = F)
        }
        
        if("Pb206.PM.Per" %in% names(df_temp) ==T){
          df_temp$Pb206.PM.Per <- format(round(df_temp$Pb206.PM.Per, digits = 2), nsmall = 2, scientific = F)
        }
        
        if("Pbc" %in% names(df_temp) ==T){
          df_temp$Pbc <- format(round(df_temp$Pbc, digits = 2), nsmall = 2, scientific = F)
        }
        
        if("Pbr" %in% names(df_temp) ==T){
          df_temp$Pbr <- format(round(df_temp$Pbr, digits = 2), nsmall = 2, scientific = F)
        }
        
        
        # Pb204 -------------------------------------------------------------------
        
        if("Pb204_Pb206" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb204_Pb206)) >= 5){
            df_temp$Pb204_Pb206 <- format(round(df_temp$Pb204_Pb206, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb204_Pb206)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb204_Pb206)))
            df_temp$Pb204_Pb206 <- format(round(df_temp$Pb204_Pb206,  digits = num), nsmall = num)
          }}
        
        if("Pb204_Pb206.PM" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb204_Pb206.PM)) >= 5){
            df_temp$Pb204_Pb206.PM <- format(round(df_temp$Pb204_Pb206.PM, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb204_Pb206.PM)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb204_Pb206.PM)))
            df_temp$Pb204_Pb206.PM <- format(round(df_temp$Pb204_Pb206.PM,  digits = num), nsmall = num)
          }}
        
        if("Pb204_Pb206.PM.Per" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb204_Pb206.PM.Per)) >= 5){
            df_temp$Pb204_Pb206.PM.Per <- format(round(df_temp$Pb204_Pb206.PM.Per, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb204_Pb206.PM.Per)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb204_Pb206.PM.Per)))
            df_temp$Pb204_Pb206.PM.Per <- format(round(df_temp$Pb204_Pb206.PM.Per,  digits = num), nsmall = num)
          }}
        
        # Pb206 -------------------------------------------------------------------
        
        if("Pb206_Th232" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb206_Th232)) >= 5){
            df_temp$Pb206_Th232 <- format(round(df_temp$Pb206_Th232, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb206_Th232)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb206_Th232)))
            df_temp$Pb206_Th232 <- format(round(df_temp$Pb206_Th232,  digits = num), nsmall = num)
          }}
        
        if("Pb206_Th232.SD" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb206_Th232.SD)) >= 5){
            df_temp$Pb206_Th232.SD <- format(round(df_temp$Pb206_Th232.SD, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb206_Th232.SD)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb206_Th232.SD)))
            df_temp$Pb206_Th232.SD <- format(round(df_temp$Pb206_Th232.SD,  digits = num), nsmall = num)
          }}
        
        if("Pb206_U238" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb206_U238)) >= 5){
            df_temp$Pb206_U238 <- format(round(df_temp$Pb206_U238, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb206_U238)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb206_U238)))
            df_temp$Pb206_U238 <- format(round(df_temp$Pb206_U238,  digits = num), nsmall = num)
          }}
        
        if("Pb206_U238.PM" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb206_U238.PM)) >= 5){
            df_temp$Pb206_U238.PM <- format(round(df_temp$Pb206_U238.PM, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb206_U238.PM)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb206_U238.PM)))
            df_temp$Pb206_U238.PM <- format(round(df_temp$Pb206_U238.PM,  digits = num), nsmall = num)
          }}
        
        if("Pb206_U238.PM.Per" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb206_U238.PM.Per)) >= 5){
            df_temp$Pb206_U238.PM.Per <- format(round(df_temp$Pb206_U238.PM.Per, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb206_U238.PM.Per)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb206_U238.PM.Per)))
            df_temp$Pb206_U238.PM.Per <- format(round(df_temp$Pb206_U238.PM.Per,  digits = num), nsmall = num)
          }}
        
        if("Pb206_U238.SD" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb206_U238.SD)) >= 5){
            df_temp$Pb206_U238.SD <- format(round(df_temp$Pb206_U238.SD, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb206_U238.SD)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb206_U238.SD)))
            df_temp$Pb206_U238.SD <- format(round(df_temp$Pb206_U238.SD,  digits = num), nsmall = num)
          }}
        
        if("Pb206_U238.2SD" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb206_U238.2SD)) >= 5){
            df_temp$Pb206_U238.2SD <- format(round(df_temp$Pb206_U238.2SD, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb206_U238.2SD)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb206_U238.2SD)))
            df_temp$Pb206_U238.2SD <- format(round(df_temp$Pb206_U238.2SD,  digits = num), nsmall = num)
          }}
        
        if("Pb206_U238.2SE" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb206_U238.2SE)) >= 5){
            df_temp$Pb206_U238.2SE <- format(round(df_temp$Pb206_U238.2SE, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb206_U238.2SE)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb206_U238.2SE)))
            df_temp$Pb206_U238.2SE <- format(round(df_temp$Pb206_U238.2SE,  digits = num), nsmall = num)
          }}
        
        if("Pbr206_U238" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pbr206_U238)) >= 5){
            df_temp$Pbr206_U238 <- format(round(df_temp$Pbr206_U238, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pbr206_U238)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pbr206_U238)))
            df_temp$Pbr206_U238 <- format(round(df_temp$Pbr206_U238,  digits = num), nsmall = num)
          }}
        
        if("Pbr206_U238.PM" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pbr206_U238.PM)) >= 5){
            df_temp$Pbr206_U238.PM <- format(round(df_temp$Pbr206_U238.PM, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pbr206_U238.PM)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pbr206_U238.PM)))
            df_temp$Pbr206_U238.PM <- format(round(df_temp$Pbr206_U238.PM,  digits = num), nsmall = num)
          }}
        
        if("Pbr206_U238.PM.Per" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pbr206_U238.PM.Per)) >= 5){
            df_temp$Pbr206_U238.PM.Per <- format(round(df_temp$Pbr206_U238.PM.Per, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pbr206_U238.PM.Per)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pbr206_U238.PM.Per)))
            df_temp$Pbr206_U238.PM.Per <- format(round(df_temp$Pbr206_U238.PM.Per,  digits = num), nsmall = num)
          }}
        
        # U238 --------------------------------------------------------------------
        
        if("U238_Pb206" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$U238_Pb206)) >= 5){
            df_temp$U238_Pb206 <- format(round(df_temp$U238_Pb206, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$U238_Pb206)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$U238_Pb206)))
            df_temp$U238_Pb206 <- format(round(df_temp$U238_Pb206,  digits = num), nsmall = num)
          }}
        
        
        if("U238_Pb206.Per" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$U238_Pb206.Per)) >= 5){
            df_temp$U238_Pb206.Per <- format(round(df_temp$U238_Pb206.Per, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$U238_Pb206.Per)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$U238_Pb206.Per)))
            df_temp$U238_Pb206.Per <- format(round(df_temp$U238_Pb206.Per,  digits = num), nsmall = num)
          }}
        
        if("U238_Pb206.PM" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$U238_Pb206.PM)) >= 5){
            df_temp$U238_Pb206.PM <- format(round(df_temp$U238_Pb206.PM, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$U238_Pb206.PM)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$U238_Pb206.PM)))
            df_temp$U238_Pb206.PM <- format(round(df_temp$U238_Pb206.PM,  digits = num), nsmall = num)
          }}
        
        if("U238_Pb206.PM.Per" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$U238_Pb206.PM.Per)) >= 5){
            df_temp$U238_Pb206.PM.Per <- format(round(df_temp$U238_Pb206.PM.Per, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$U238_Pb206.PM.Per)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$U238_Pb206.PM.Per)))
            df_temp$U238_Pb206.PM.Per <- format(round(df_temp$U238_Pb206.PM.Per,  digits = num), nsmall = num)
          }}
        
        if("U238_Pb206.SD" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$U238_Pb206.SD)) >= 5){
            df_temp$U238_Pb206.SD <- format(round(df_temp$U238_Pb206.SD, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$U238_Pb206.SD)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$U238_Pb206.SD)))
            df_temp$U238_Pb206.SD <- format(round(df_temp$U238_Pb206.SD,  digits = num), nsmall = num)
          }}
        
        if("U238_Pbr206" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$U238_Pbr206)) >= 5){
            df_temp$U238_Pbr206 <- format(round(df_temp$U238_Pbr206, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$U238_Pbr206)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$U238_Pbr206)))
            df_temp$U238_Pbr206 <- format(round(df_temp$U238_Pbr206,  digits = num), nsmall = num)
          }}
        
        
        if("U238_Pbr206.PM" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$U238_Pbr206.PM)) >= 5){
            df_temp$U238_Pbr206.PM <- format(round(df_temp$U238_Pbr206.PM, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$U238_Pbr206.PM)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$U238_Pbr206.PM)))
            df_temp$U238_Pbr206.PM <- format(round(df_temp$U238_Pbr206.PM,  digits = num), nsmall = num)
          }}
        
        if("U238_Pbr206.PM.Per" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$U238_Pbr206.PM.Per)) >= 5){
            df_temp$U238_Pbr206.PM.Per <- format(round(df_temp$U238_Pbr206.PM.Per, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$U238_Pbr206.PM.Per)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$U238_Pbr206.PM.Per)))
            df_temp$U238_Pbr206.PM.Per <- format(round(df_temp$U238_Pbr206.PM.Per,  digits = num), nsmall = num)
          }}
        
        # Pb207 -------------------------------------------------------------------
        
        if("Pb207_Pb206" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb207_Pb206)) >= 5){
            df_temp$Pb207_Pb206 <- format(round(df_temp$Pb207_Pb206, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb207_Pb206)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb207_Pb206)))
            df_temp$Pb207_Pb206 <- format(round(df_temp$Pb207_Pb206,  digits = num), nsmall = num)
          }}
        
        if("Pb207_Pb206.Per" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb207_Pb206.Per)) >= 5){
            df_temp$Pb207_Pb206.Per <- format(round(df_temp$Pb207_Pb206.Per, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb207_Pb206.Per)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb207_Pb206.Per)))
            df_temp$Pb207_Pb206.Per <- format(round(df_temp$Pb207_Pb206.Per,  digits = num), nsmall = num)
          }}
        
        if("Pb207_Pb206.PM" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb207_Pb206.PM)) >= 5){
            df_temp$Pb207_Pb206.PM <- format(round(df_temp$Pb207_Pb206.PM, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb207_Pb206.PM)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb207_Pb206.PM)))
            df_temp$Pb207_Pb206.PM <- format(round(df_temp$Pb207_Pb206.PM,  digits = num), nsmall = num)
          }}
        
        if("Pb207_Pb206.PM.Per" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb207_Pb206.PM.Per)) >= 5){
            df_temp$Pb207_Pb206.PM.Per <- format(round(df_temp$Pb207_Pb206.PM.Per, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb207_Pb206.PM.Per)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb207_Pb206.PM.Per)))
            df_temp$Pb207_Pb206.PM.Per <- format(round(df_temp$Pb207_Pb206.PM.Per,  digits = num), nsmall = num)
          }}
        
        if("Pb207_Pb206.SD" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb207_Pb206.SD)) >= 5){
            df_temp$Pb207_Pb206.SD <- format(round(df_temp$Pb207_Pb206.SD, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb207_Pb206.SD)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb207_Pb206.SD)))
            df_temp$Pb207_Pb206.SD <- format(round(df_temp$Pb207_Pb206.SD,  digits = num), nsmall = num)
          }}
        
        if("Pb207_Pb206.2SD" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb207_Pb206.2SD)) >= 5){
            df_temp$Pb207_Pb206.2SD <- format(round(df_temp$Pb207_Pb206.2SD, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb207_Pb206.2SD)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb207_Pb206.2SD)))
            df_temp$Pb207_Pb206.2SD <- format(round(df_temp$Pb207_Pb206.2SD,  digits = num), nsmall = num)
          }}
        
        if("Pb207_Pb206.2SE" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb207_Pb206.2SE)) >= 5){
            df_temp$Pb207_Pb206.2SE <- format(round(df_temp$Pb207_Pb206.2SE, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb207_Pb206.2SE)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb207_Pb206.2SE)))
            df_temp$Pb207_Pb206.2SE <- format(round(df_temp$Pb207_Pb206.2SE,  digits = num), nsmall = num)
          }}
        
        if("Pbr207_Pb206" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pbr207_Pb206)) >= 5){
            df_temp$Pbr207_Pb206 <- format(round(df_temp$Pbr207_Pb206, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pbr207_Pb206)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pbr207_Pb206)))
            df_temp$Pbr207_Pb206 <- format(round(df_temp$Pbr207_Pb206,  digits = num), nsmall = num)
          }}
        
        if("Pbr207_Pb206.PM" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pbr207_Pb206.PM)) >= 5){
            df_temp$Pbr207_Pb206.PM <- format(round(df_temp$Pbr207_Pb206.PM, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pbr207_Pb206.PM)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pbr207_Pb206.PM)))
            df_temp$Pbr207_Pb206.PM <- format(round(df_temp$Pbr207_Pb206.PM,  digits = num), nsmall = num)
          }}
        
        if("Pbr207_Pb206.PM.Per" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pbr207_Pb206.PM.Per)) >= 5){
            df_temp$Pbr207_Pb206.PM.Per <- format(round(df_temp$Pbr207_Pb206.PM.Per, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pbr207_Pb206.PM.Per)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pbr207_Pb206.PM.Per)))
            df_temp$Pbr207_Pb206.PM.Per <- format(round(df_temp$Pbr207_Pb206.PM.Per,  digits = num), nsmall = num)
          }}
        
        if("Pbr207_Pbr206" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pbr207_Pbr206)) >= 5){
            df_temp$Pbr207_Pbr206 <- format(round(df_temp$Pbr207_Pbr206, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pbr207_Pbr206)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pbr207_Pbr206)))
            df_temp$Pbr207_Pbr206 <- format(round(df_temp$Pbr207_Pbr206,  digits = num), nsmall = num)
          }}
        
        if("Pbr207_Pbr206.PM.Per" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pbr207_Pbr206.PM.Per)) >= 5){
            df_temp$Pbr207_Pbr206.PM.Per <- format(round(df_temp$Pbr207_Pbr206.PM.Per, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pbr207_Pbr206.PM.Per)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pbr207_Pbr206.PM.Per)))
            df_temp$Pbr207_Pbr206.PM.Per <- format(round(df_temp$Pbr207_Pbr206.PM.Per,  digits = num), nsmall = num)
          }}
        
        
        if("Pb207_U235" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb207_U235)) >= 5){
            df_temp$Pb207_U235 <- format(round(df_temp$Pb207_U235, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb207_U235)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb207_U235)))
            df_temp$Pb207_U235 <- format(round(df_temp$Pb207_U235,  digits = num), nsmall = num)
          }}
        
        if("Pb207_U235.PM.Per" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb207_U235.PM.Per)) >= 5){
            df_temp$Pb207_U235.PM.Per <- format(round(df_temp$Pb207_U235.PM.Per, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb207_U235.PM.Per)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb207_U235.PM.Per)))
            df_temp$Pb207_U235.PM.Per <- format(round(df_temp$Pb207_U235.PM.Per,  digits = num), nsmall = num)
          }}
        
        if("Pb207_U235.SD" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb207_U235.SD)) >= 5){
            df_temp$Pb207_U235.SD <- format(round(df_temp$Pb207_U235.SD, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb207_U235.SD)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb207_U235.SD)))
            df_temp$Pb207_U235.SD <- format(round(df_temp$Pb207_U235.SD,  digits = num), nsmall = num)
          }}
        
        if("Pb207_U235.2SD" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb207_U235.2SD)) >= 5){
            df_temp$Pb207_U235.2SD <- format(round(df_temp$Pb207_U235.2SD, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb207_U235.2SD)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb207_U235.2SD)))
            df_temp$Pb207_U235.2SD <- format(round(df_temp$Pb207_U235.2SD,  digits = num), nsmall = num)
          }}
        
        if("Pb207_U235.2SE" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb207_U235.2SE)) >= 5){
            df_temp$Pb207_U235.2SE <- format(round(df_temp$Pb207_U235.2SE, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb207_U235.2SE)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb207_U235.2SE)))
            df_temp$Pb207_U235.2SE <- format(round(df_temp$Pb207_U235.2SE,  digits = num), nsmall = num)
          }}
        
        if("Pbr207_U235" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pbr207_U235)) >= 5){
            df_temp$Pbr207_U235 <- format(round(df_temp$Pbr207_U235, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pbr207_U235)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pbr207_U235)))
            df_temp$Pbr207_U235 <- format(round(df_temp$Pbr207_U235,  digits = num), nsmall = num)
          }}
        
        if("Pbr207_U235.PM" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pbr207_U235.PM)) >= 5){
            df_temp$Pbr207_U235.PM <- format(round(df_temp$Pbr207_U235.PM, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pbr207_U235.PM)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pbr207_U235.PM)))
            df_temp$Pbr207_U235.PM <- format(round(df_temp$Pbr207_U235.PM,  digits = num), nsmall = num)
          }}
        
        if("Pbr207_U235.PM.Per" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pbr207_U235.PM.Per)) >= 5){
            df_temp$Pbr207_U235.PM.Per <- format(round(df_temp$Pbr207_U235.PM.Per, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pbr207_U235.PM.Per)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pbr207_U235.PM.Per)))
            df_temp$Pbr207_U235.PM.Per <- format(round(df_temp$Pbr207_U235.PM.Per,  digits = num), nsmall = num)
          }}
        
        # Pb208 -------------------------------------------------------------------
        
        
        if("Pb208_Pb206" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb208_Pb206)) >= 5){
            df_temp$Pb208_Pb206 <- format(round(df_temp$Pb208_Pb206, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb208_Pb206)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb208_Pb206)))
            df_temp$Pb208_Pb206 <- format(round(df_temp$Pb208_Pb206,  digits = num), nsmall = num)
          }}
        
        
        if("Pb208_Pb206.PM.Per" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb208_Pb206.PM.Per)) >= 5){
            df_temp$Pb208_Pb206.PM.Per <- format(round(df_temp$Pb208_Pb206.PM.Per, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb208_Pb206.PM.Per)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb208_Pb206.PM.Per)))
            df_temp$Pb208_Pb206.PM.Per <- format(round(df_temp$Pb208_Pb206.PM.Per,  digits = num), nsmall = num)
          }}
        
        
        # Pb206.Age ---------------------------------------------------------------
        
        
        if("Pb206_U238.Age" %in% names(df_temp) ==T){
          df_temp$Pb206_U238.Age <- df_temp$Pb206_U238.Age*0.000001 
          df_temp$Pb206_U238.Age <- format(round(df_temp$Pb206_U238.Age, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb206_U238.Age"] = "Pb206_U238.Age(Ma)"
        }
        
        if("Pb206_U238.Age.PM" %in% names(df_temp) ==T){
          df_temp$Pb206_U238.Age.PM <- df_temp$Pb206_U238.Age.PM*0.000001 
          df_temp$Pb206_U238.Age.PM <- format(round(df_temp$Pb206_U238.Age.PM, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb206_U238.Age.PM"] = "Pb206_U238.Age.PM(Ma)"
        }
        
        if("Pb206_U238.Age.PM.Per" %in% names(df_temp) ==T){
          df_temp$Pb206_U238.Age.PM.Per <- df_temp$Pb206_U238.Age.PM.Per*0.000001 
          df_temp$Pb206_U238.Age.PM.Per <- format(round(df_temp$Pb206_U238.Age.PM.Per, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb206_U238.Age.PM.Per"] = "Pb206_U238.Age.PM.Per(Ma)"
        }
        
        
        if("Pb206_U238.Age.SD" %in% names(df_temp) ==T){
          df_temp$Pb206_U238.Age.SD <- df_temp$Pb206_U238.Age.SD*0.000001 
          df_temp$Pb206_U238.Age.SD <- format(round(df_temp$Pb206_U238.Age.SD, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb206_U238.Age.SD"] = "Pb206_U238.Age.SD(Ma)"
        }
        
        if("Pb206_U238.Age.2SD" %in% names(df_temp) ==T){
          df_temp$Pb206_U238.Age.2SD <- df_temp$Pb206_U238.Age.2SD*0.000001 
          df_temp$Pb206_U238.Age.2SD <- format(round(df_temp$Pb206_U238.Age.2SD, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb206_U238.Age.2SD"] = "Pb206_U238.Age.2SD(Ma)"
        }
        
        if("Pb206_U238.Age.2SE" %in% names(df_temp) ==T){
          df_temp$Pb206_U238.Age.2SE <- df_temp$Pb206_U238.Age.2SE*0.000001 
          df_temp$Pb206_U238.Age.2SE <- format(round(df_temp$Pb206_U238.Age.2SE, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb206_U238.Age.2SE"] = "Pb206_U238.Age.2SE(Ma)"
        }
        
        if("Pbr206_U238.Age" %in% names(df_temp) ==T){
          df_temp$Pbr206_U238.Age <- df_temp$Pbr206_U238.Age*0.000001 
          df_temp$Pbr206_U238.Age <- format(round(df_temp$Pbr206_U238.Age, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pbr206_U238.Age"] = "Pbr206_U238.Age(Ma)"
        }
        
        if("Pbr206_U238.Age.PM" %in% names(df_temp) ==T){
          df_temp$Pbr206_U238.Age.PM <- df_temp$Pbr206_U238.Age.PM*0.000001 
          df_temp$Pbr206_U238.Age.PM <- format(round(df_temp$Pbr206_U238.Age.PM, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pbr206_U238.Age.PM"] = "Pbr206_U238.Age.PM(Ma)"
        }
        
        if("Pbr206_U238.Age.PM.Per" %in% names(df_temp) ==T){
          df_temp$Pbr206_U238.Age.PM.Per <- df_temp$Pbr206_U238.Age.PM.Per*0.000001 
          df_temp$Pbr206_U238.Age.PM.Per <- format(round(df_temp$Pbr206_U238.Age.PM.Per, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pbr206_U238.Age.PM.Per"] = "Pbr206_U238.Age.PM.Per(Ma)"
        }
        
        
        # U238 Age--------------------------------------------------------------------
        
        
        if("U238_Pb206.Age" %in% names(df_temp) ==T){
          df_temp$U238_Pb206.Age <- df_temp$U238_Pb206.Age*0.000001 
          df_temp$U238_Pb206.Age <- format(round(df_temp$U238_Pb206.Age, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "U238_Pb206.Age"] = "U238_Pb206.Age(Ma)"
        }
        
        if("U238_Pb206.Age.PM.Per" %in% names(df_temp) ==T){
          df_temp$U238_Pb206.Age.PM.Per <- df_temp$U238_Pb206.Age.PM.Per*0.000001 
          df_temp$U238_Pb206.Age.PM.Per <- format(round(df_temp$U238_Pb206.Age.PM.Per, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "U238_Pb206.Age.PM.Per"] = "U238_Pb206.Age.PM.Per(Ma)"
        }
        
        
        # Pb207 Age ---------------------------------------------------------------
        
        if("Pb207_Pb206.Age" %in% names(df_temp) ==T){
          df_temp$Pb207_Pb206.Age <- df_temp$Pb207_Pb206.Age*0.000001 
          df_temp$Pb207_Pb206.Age <- format(round(df_temp$Pb207_Pb206.Age, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb207_Pb206.Age"] = "Pb207_Pb206.Age(Ma)"
        }
        
        if("Pb207_Pb206.Age.PM" %in% names(df_temp) ==T){
          df_temp$Pb207_Pb206.Age.PM <- df_temp$Pb207_Pb206.Age.PM*0.000001 
          df_temp$Pb207_Pb206.Age.PM <- format(round(df_temp$Pb207_Pb206.Age.PM, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb207_Pb206.Age.PM"] = "Pb207_Pb206.Age.PM(Ma)"
        }
        
        if("Pb207_Pb206.Age.PM.Per" %in% names(df_temp) ==T){
          df_temp$Pb207_Pb206.Age.PM.Per <- df_temp$Pb207_Pb206.Age.PM.Per*0.000001 
          df_temp$Pb207_Pb206.Age.PM.Per <- format(round(df_temp$Pb207_Pb206.Age.PM.Per, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb207_Pb206.Age.PM.Per"] = "Pb207_Pb206.Age.PM.Per(Ma)"
        }
        
        
        if("Pb207_Pb206.Age.SD" %in% names(df_temp) ==T){
          df_temp$Pb207_Pb206.Age.SD <- df_temp$Pb207_Pb206.Age.SD*0.000001 
          df_temp$Pb207_Pb206.Age.SD <- format(round(df_temp$Pb207_Pb206.Age.SD, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb207_Pb206.Age.SD"] = "Pb207_Pb206.Age.SD(Ma)"
        }
        
        
        if("Pb207_Pb206.Age.2SD" %in% names(df_temp) ==T){
          df_temp$Pb207_Pb206.Age.2SD <- df_temp$Pb207_Pb206.Age.2SD*0.000001 
          df_temp$Pb207_Pb206.Age.2SD <- format(round(df_temp$Pb207_Pb206.Age.2SD, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb207_Pb206.Age.2SD"] = "Pb207_Pb206.Age.2SD(Ma)"
        }
        
        if("Pb207_Pb206.Age.2SE" %in% names(df_temp) ==T){
          df_temp$Pb207_Pb206.Age.2SE <- df_temp$Pb207_Pb206.Age.2SE*0.000001 
          df_temp$Pb207_Pb206.Age.2SE <- format(round(df_temp$Pb207_Pb206.Age.2SE, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb207_Pb206.Age.2SE"] = "Pb207_Pb206.Age.2SE(Ma)"
        }
        
        if("Pbr207_Pbr206.Age" %in% names(df_temp) ==T){
          df_temp$Pbr207_Pbr206.Age <- df_temp$Pbr207_Pbr206.Age*0.000001 
          df_temp$Pbr207_Pbr206.Age <- format(round(df_temp$Pbr207_Pbr206.Age, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pbr207_Pbr206.Age"] = "Pbr207_Pbr206.Age(Ma)"
        }
        
        if("Pbr207_Pbr206.Age.PM" %in% names(df_temp) ==T){
          df_temp$Pbr207_Pbr206.Age.PM <- df_temp$Pbr207_Pbr206.Age.PM*0.000001 
          df_temp$Pbr207_Pbr206.Age.PM <- format(round(df_temp$Pbr207_Pbr206.Age.PM, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pbr207_Pbr206.Age.PM"] = "Pbr207_Pbr206.Age.PM(Ma)"
        }
        
        if("Pbr207_Pbr206.Age.PM.Per" %in% names(df_temp) ==T){
          df_temp$Pbr207_Pbr206.Age.PM.Per <- df_temp$Pbr207_Pbr206.Age.PM.Per*0.000001 
          df_temp$Pbr207_Pbr206.Age.PM.Per <- format(round(df_temp$Pbr207_Pbr206.Age.PM.Per, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pbr207_Pbr206.Age.PM.Per"] = "Pbr207_Pbr206.Age.PM.Per(Ma)"
        }
        
        if("Pb207_U235.Age" %in% names(df_temp) ==T){
          df_temp$Pb207_U235.Age <- df_temp$Pb207_U235.Age*0.000001 
          df_temp$Pb207_U235.Age <- format(round(df_temp$Pb207_U235.Age, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb207_U235.Age"] = "Pb207_U235.Age(Ma)"
        }
        
        if("Pb207_U235.Age.PM" %in% names(df_temp) ==T){
          df_temp$Pb207_U235.Age.PM <- df_temp$Pb207_U235.Age.PM*0.000001 
          df_temp$Pb207_U235.Age.PM <- format(round(df_temp$Pb207_U235.Age.PM, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb207_U235.Age.PM"] = "Pb207_U235.Age.PM(Ma)"
        }
        
        
        if("Pb207_U235.Age.SD" %in% names(df_temp) ==T){
          df_temp$Pb207_U235.Age.SD <- df_temp$Pb207_U235.Age.SD*0.000001 
          df_temp$Pb207_U235.Age.SD <- format(round(df_temp$Pb207_U235.Age.SD, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb207_U235.Age.SD"] = "Pb207_U235.Age.SD(Ma)"
        }
        
        if("Pb207_U235.Age.2SD" %in% names(df_temp) ==T){
          df_temp$Pb207_U235.Age.2SD <- df_temp$Pb207_U235.Age.2SD*0.000001 
          df_temp$Pb207_U235.Age.2SD <- format(round(df_temp$Pb207_U235.Age.2SD, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb207_U235.Age.2SD"] = "Pb207_U235.Age.2SD(Ma)"
        }
        
        if("Pb207_U235.Age.2SE" %in% names(df_temp) ==T){
          df_temp$Pb207_U235.Age.2SE <- df_temp$Pb207_U235.Age.2SE*0.000001 
          df_temp$Pb207_U235.Age.2SE <- format(round(df_temp$Pb207_U235.Age.2SE, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb207_U235.Age.2SE"] = "Pb207_U235.Age.2SE(Ma)"
        }
        
        # Pb208_Age ---------------------------------------------------------------
        
        
        if("Pb208_Th232.Age" %in% names(df_temp) ==T){
          df_temp$Pb208_Th232.Age <- df_temp$Pb208_Th232.Age*0.000001 
          df_temp$Pb208_Th232.Age <- format(round(df_temp$Pb208_Th232.Age, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb208_Th232.Age"] = "Pb208_Th232.Age(Ma)"
        }
        
        if("Pb208_Th232.Age.PM" %in% names(df_temp) ==T){
          df_temp$Pb208_Th232.Age.PM <- df_temp$Pb208_Th232.Age.PM*0.000001 
          df_temp$Pb208_Th232.Age.PM <- format(round(df_temp$Pb208_Th232.Age.PM, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb208_Th232.Age.PM"] = "Pb208_Th232.Age.PM(Ma)"
        }
        
        if("Age" %in% names(df_temp) ==T){
          df_temp$Age <- df_temp$Age*0.000001 
          df_temp$Age <- format(round(df_temp$Age, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Age"] = "Age(Ma)"
        }
        
        if("Age.PM" %in% names(df_temp) ==T){
          df_temp$Age.PM <- df_temp$Age.PM*0.000001 
          df_temp$Age.PM <- format(round(df_temp$Age.PM, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Age.PM"] = "Age.PM(Ma)"
        }
        
        
        if("Disc" %in% names(df_temp) == T){
          df_temp$Disc <- format(round(df_temp$Disc, digits = 0), nsmall = 0)
        }
        df <- df_temp
      }
    }
  })
  
  output$subsplagetbl <- renderDataTable(
    subagetbldata(),
    rownames = F,
    selection = "single"
  )
  
  output$cntiso_age_iso <- renderUI({
    paste("Isotope Data:", nrow(subagetbldata()))
  })

  
  
  subsplagerefdata <- reactive({
    
    refid <- as.character(substr(subagedata(), 1, 9))
    
    load("RData/OrgRData/ref.RData") 
    
    if(file.exists("RData/UserRData/ref_custom.RData")){
      load("RData/UserRData/ref_custom.RData")
      if(nrow(ref_custom) == 0){
        ref
      }else if(nrow(ref_custom) != 0){
        ref <- rbind(ref, ref_custom)
      }
    }
    
    if(length(subagedata())){
      ref <- ref[ref$RefID %in% refid,]
    }
  })
  
  output$subsplagereftbl <- renderDataTable(
    subsplagerefdata(),
    rownames = F,
    selection = "single",
    options = list(columnDefs = list(list(width = '500px', targets = c(1)),
                                     list(width = '250px', targets = c(2)),
                                     list(width = '300px', targets = c(3:5)),
                                     list(visible = F, targets = c(0))))
  )
  
  output$cntiso_age_ref <- renderUI({
    if(length(subsplagerefdata())){
      paste("Reference:", nrow(subsplagerefdata()))  
    }
  })
  
  savesplagevalue <- reactive({
    value[[paste0(subagetbldata(), ".xlsx")]][-c(1)]
  })
  
  output$DBsplage <- downloadHandler(
    filename = function(){
      paste0("Sample_Age_Data.csv")},
    content = function(file){
      write.csv(chronodata()[,-c(1:3,5:8)], file, row.names = F)
    })
  
  
  output$DBsplagevalue <- downloadHandler(
    filename = function(){
      paste0("Sample_Age_Value_Data.csv")},
    content = function(file){
      write.csv(subagetbldata(), file, row.names = F)
    })
  
  
  # Mapping ---------------------------------------------------------------------------
  
  mapdata <- reactive({
    
    load("RData/OrgRData/ref.RData")
    load("RData/OrgRData/age.RData")  
    load("RData/OrgRData/spl.RData") 
    load("RData/OrgRData/materialCode.RData")
    if(file.exists("RData/UserRData/spl_custom.RData")){
      load("RData/UserRData/spl_custom.RData")
      load("RData/UserRData/age_custom.RData")
      load("RData/UserRData/ref_custom.RData")
      if(nrow(age_custom) == 0){
        age
      }else if(nrow(age_custom) != 0){
        age <- rbind(age, age_custom)
      }
      
      if(nrow(spl_custom) == 0){
        spl <- spl
      }else if(nrow(spl_custom) != 0){
        spl <- rbind(spl, spl_custom)
      }
      
      if(nrow(ref_custom) == 0){
        ref <- ref
      }else if(nrow(ref_custom) != 0){
        ref <- rbind(ref, ref_custom)
      }
      
    }   
    
    spl <- join(spl, material_cd, by = "Material")
    
    for(i in 1:nrow(spl)){
      if(grepl("Ig", spl[i,]$Material) == T){
        spl[i,]$material_num <- c(1)
      }else if(grepl("Sedi", spl[i,]$Material) == T){
        spl[i,]$material_num <- c(2)
      }else if(grepl("Meta", spl[i,]$Material) == T){
        spl[i,]$material_num <- c(3)
      }else if(grepl("Depo", spl[i,]$Material) == T){
        spl[i,]$material_num <- c(4)
      }else if(grepl("Clay", spl[i,]$Material) == T){
        spl[i,]$material_num <- c(5)
      }
    }
    
    spl$Longitude <- format(spl$Longitude, digits = 4, nsmall = 4)
    spl$Latitude <- format(spl$Latitude, digits = 4, nsmall = 4)
    
    age <- join(age, met_cd, by = "Method_cd")
    age <- join(age, spl, by = c("RefID", "Sample_seq"))
    age <- join(age, ref, by = c("RefID"))
    age <- age[,-c(12)]
    
      age$Age <- as.numeric(age$Age)*0.000001
      names(age)[names(age) == "Age"] = "Age(Ma)"
      age$Age_min <- as.numeric(age$Age_min)*0.000001
      names(age)[names(age) == "Age_min"] = "Age_min(Ma)"
      age$Age_max <- as.numeric(age$Age_max)*0.000001
      names(age)[names(age) == "Age_max"] = "Age_max(Ma)"
    
    age <- age %>% 
      mutate(age, ID = paste0(age$RefID, age$Sample_seq, age$Method_cd))
    
    if(input$mapmet == "ALL" & input$mapage == "ALL"){
      age <- age
    }else if(input$mapmet == "ALL" & input$mapage != "ALL"){
      age  <- age[age$Era %in% input$mapage,]
    }else if(input$mapmet != "ALL" & input$mapage == "ALL"){
      age  <- age[age$Method %in% input$mapmet,]
    }else if(input$mapmet != "ALL" & input$mapage != "ALL"){
      age  <- age[age$Method %in% input$mapmet,]
      age <- age %>% 
        filter(Era == input$mapage)
    }
  })
  
  pal <- colorFactor(palette = c("red", "blue", "purple", "darkgreen", "yellow"),
                     levels = c(1:5))
  

  observeEvent(req(input$mapmet, input$mapage),{
    
    if(nrow(mapdata()) == 0){
      shinyalert(inputId = "map_alr", "No samples available!",
                 "Please, Select different Method and Era", type = "info",
                 confirmButtonText = "OK")
    }else{
      observeEvent(input$mapping_zoom, {
        # scaling for markers with zoom level
        new_zoom <- 7
        if(!is.null(input$mapping_zoom)) new_zoom <- input$mapping_zoom
        if (new_zoom < 5){
          kradius <- 1 / new_zoom
        }else{
          kradius <- new_zoom * 0.4
        }
        leafletProxy(
          mapId = "mapping",
          session = session
        ) %>% clearShapes() %>%
          addCircleMarkers(data = mapdata(),
                           lng = ~round(as.numeric(mapdata()$Longitude), 4),
                           lat = ~round(as.numeric(mapdata()$Latitude), 4),
                           radius = kradius,
                           stroke = F,
                           fillOpacity = 1,
                           color = ~pal(mapdata()$material_num),
                           popup = ~paste("Sample: ", mapdata()$Sample, "<br/>",
                                          "Era: ", mapdata()$Era, "<br/>",
                                          "Method: ", mapdata()$Method, "<br/>",
                                          "Taxon: ", mapdata()$Taxon, "<br/>",
                                          "Lon: ", mapdata()$Longitude, "<br/>",
                                          "Lat: ", mapdata()$Latitude))
        
      })
    }
  })
 

  output$mapping <- renderLeaflet({
    leaflet(mapdata()) %>%
      setView(lat = 36, lng = 128, zoom = 7) %>%
      # base groups
      addTiles(group="Default") %>%
      addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
      addProviderTiles(providers$Stamen.Terrain, group="Terrain") %>%
      addScaleBar(position = "topright") %>%
      addLegend(colors = c("red", "blue", "purple", "darkgreen", "yellow"),
                labels = c("Igneous Rock", "Sedimentary Rock", "Metamorphic Rock", "Deposit", "Clay"),
                position = "bottomleft") %>%
      # Layers control
      addLayersControl(
        baseGroups = c("Default", "Toner Lite", "Terrain"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  observeEvent(input$mapping_marker_click,{
  
    click <- input$mapping_marker_click
    if(is.null(click))
      return()
  
    long <- format(as.numeric(click$lng), digits = 4, nsmall = 4)
    lat <- format(as.numeric(click$lat), digits = 4, nsmall = 4)

    mapspltbldata <- reactive({
      if(!is.null(click)){
        filter(mapdata(), mapdata()$Longitude == long & mapdata()$Latitude == lat)  
      }
      
    })

    output$mapspltbl <- renderDataTable(
      mapspltbldata(),

      rownames = F,
      selection = "single",
      options = list(columnDefs = list(list(visible = F, targets = c(0:2, 10:25))))
    )
    
    output$cntmap_spl <- renderUI({
      paste("Sample information:", nrow(mapspltbldata()))
    })
    
    submapspldata <- reactive({
      selected_row <- input$mapspltbl_rows_selected
      selected_sample <- mapspltbldata()[as.integer(selected_row),]$ID
    })
    
    mapisodata <- reactive({
      
      if(substr(submapspldata(), nchar(submapspldata())-2, nchar(submapspldata())) == "001"){
        
        # method 001 --------------------------------------------------------------
        
        load("RData/OrgRData/value_ar.RData")
        if(file.exists("RData/UserRData/value_ar_custom.RData")){
          load("RData/UserRData/value_ar_custom.RData")
          value_ar <- c(value_ar, value_ar_custom)
        }
        
        df_temp<- as.data.frame(value_ar[[paste0(submapspldata(), ".xlsx")]][-c(1)])
        
        if("Plateau.Age" %in% names(df_temp) ==T){
          df_temp$Plateau.Age <- df_temp$Plateau.Age*0.001 
          df_temp$Plateau.Age <- format(round(df_temp$Plateau.Age, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Plateau.Age"] = "Plateau.Age(Ka)"
        }
        
        if("Plateau.Age.SD" %in% names(df_temp) ==T){
          df_temp$Plateau.Age.SD <- df_temp$Plateau.Age.SD*0.001 
          df_temp$Plateau.Age.SD <- format(round(df_temp$Plateau.Age.SD, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Plateau.Age.SD"] = "Plateau.Age.SD(Ka)"
        }
        
        if("Intergrated.Age" %in% names(df_temp) ==T){
          df_temp$Intergrated.Age <- df_temp$Intergrated.Age*0.001 
          df_temp$Intergrated.Age <- format(round(df_temp$Intergrated.Age, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Intergrated.Age"] = "Intergrated.Age(Ka)"
        }
        
        if("Intergrated.Age.SD" %in% names(df_temp) ==T){
          df_temp$Intergrated.Age.SD <- df_temp$Intergrated.Age.SD*0.001 
          df_temp$Intergrated.Age.SD <- format(round(df_temp$Intergrated.Age.SD, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Intergrated.Age.SD"] = "Intergrated.Age.SD(Ka)"
        }
        
        df <- df_temp
        
      }else if(substr(submapspldata(), nchar(submapspldata())-2, nchar(submapspldata())) == "002"){
        
        # method 002 --------------------------------------------------------------
        
        load("RData/OrgRData/value_cosmo.RData")
        if(file.exists("RData/UserRData/value_cosmo_custom.RData")){
          load("RData/UserRData/value_cosmo_custom.RData")
          value_cosmo <- c(value_cosmo, value_cosmo_custom)
        }
        value_cosmo[[paste0(submapspldata(), ".xlsx")]][-c(1)]
        
      }else if(substr(submapspldata(), nchar(submapspldata())-2, nchar(submapspldata())) == "003"){
        
        # method 003 --------------------------------------------------------------
        
        load("RData/OrgRData/value_kar.RData")
        if(file.exists("RData/UserRData/value_kar_custom.RData")){
          load("RData/UserRData/value_kar_custom.RData")
          value_kar <- c(value_kar, value_kar_custom)
        }
        
        df_temp<- as.data.frame(value_kar[[paste0(submapspldata(), ".xlsx")]][-c(1)])
        
        if("Delta.Radiogenic.Ar40" %in% names(df_temp) == T){
          df_temp$Delta.Radiogenic.Ar40 <- format(df_temp$Delta.Radiogenic.Ar40, scientific = F) 
        }
        
        if("Radiogenic.Ar36" %in% names(df_temp) == T){
          df_temp$Radiogenic.Ar36 <- format(df_temp$Radiogenic.Ar36, scientific = F)
        }
        
        if("Delta.Radiogenic.Ar36" %in% names(df_temp) == T){
          df_temp$Delta.Radiogenic.Ar36 <- format(df_temp$Delta.Radiogenic.Ar36, scientific = F)
        }
        
        df <- df_temp
        
      }else if(substr(submapspldata(), nchar(submapspldata())-2, nchar(submapspldata())) == "004"){
        
        # method 004 --------------------------------------------------------------
        
        load("RData/OrgRData/value_luhf.RData")
        if(file.exists("RData/UserRData/value_luhf_custom.RData")){
          load("RData/UserRData/value_luhf_custom.RData")
          value_luhf <- c(value_luhf, value_luhf_custom)
        }
        value_luhf[[paste0(submapspldata(), ".xlsx")]][-c(1)]
        
      }else if(substr(submapspldata(), nchar(submapspldata())-2, nchar(submapspldata())) == "005"){
        
        # method 005 --------------------------------------------------------------
        
        load("RData/OrgRData/value_osl.RData")
        if(file.exists("RData/UserRData/value_osl_custom.RData")){
          load("RData/UserRData/value_osl_custom.RData")
          value_osl <- c(value_osl, value_osl_custom)
        }
        value_osl[[paste0(submapspldata(), ".xlsx")]][-c(1)]
        
      }else if(substr(submapspldata(), nchar(submapspldata())-2, nchar(submapspldata())) == "006"){
        
        # method 006 --------------------------------------------------------------
        
        load("RData/OrgRData/value_smnd.RData")
        if(file.exists("RData/UserRData/value_smnd_custom.RData")){
          load("RData/UserRData/value_smnd_custom.RData")
          value_smnd <- c(value_smnd, value_smnd_custom)
        }
        value_smnd[[paste0(submapspldata(), ".xlsx")]][-c(1)]
        
      }else if(substr(submapspldata(), nchar(submapspldata())-2, nchar(submapspldata())) == "007"){
        
        # method 007 --------------------------------------------------------------
        load("RData/OrgRData/value_upb.RData")
        if(file.exists("RData/UserRData/value_upb_custom.RData")){
          load("RData/UserRData/value_upb_custom.RData")  
          value_upb <- c(value_upb, value_upb_custom)
        }
        
        df_temp<- as.data.frame(value_upb[[paste0(submapspldata(), ".xlsx")]][-c(1)])
        
        if("U" %in% names(df_temp) ==T){
          df_temp$U <- format(round(df_temp$U, digits = 0), nsmall = 0, scientific = F)
        }
        
        if("Th" %in% names(df_temp) ==T){
          df_temp$Th <- format(round(df_temp$Th, digits = 0), nsmall = 0, scientific = F)
        }
        
        if("Th_U" %in% names(df_temp) ==T){
          df_temp$Th_U <- format(round(df_temp$Th_U, digits = 2), nsmall = 2, scientific = F)
        }
        
        if("Th_U.Per" %in% names(df_temp) ==T){
          df_temp$Th_U.Per <- format(round(df_temp$Th_U.Per, digits = 2), nsmall = 2, scientific = F)
        }
        
        if("Th_U.PM.Per" %in% names(df_temp) ==T){
          df_temp$Th_U.PM.Per <- format(round(df_temp$Th_U.PM.Per, digits = 2), nsmall = 2, scientific = F)
        }
        
        ## pb series ---------------------------------------------------------------
        
        if("Pb" %in% names(df_temp) ==T){
          df_temp$Pb <- format(round(df_temp$Pb, digits = 2), nsmall = 2, scientific = F)
        }
        
        if("Pb206" %in% names(df_temp) ==T){
          df_temp$Pb206 <- format(round(df_temp$Pb206, digits = 2), nsmall = 2, scientific = F)
        }
        
        if("Pb206.PM.Per" %in% names(df_temp) ==T){
          df_temp$Pb206.PM.Per <- format(round(df_temp$Pb206.PM.Per, digits = 2), nsmall = 2, scientific = F)
        }
        
        if("Pbc" %in% names(df_temp) ==T){
          df_temp$Pbc <- format(round(df_temp$Pbc, digits = 2), nsmall = 2, scientific = F)
        }
        
        if("Pbr" %in% names(df_temp) ==T){
          df_temp$Pbr <- format(round(df_temp$Pbr, digits = 2), nsmall = 2, scientific = F)
        }
        
        
        # Pb204 -------------------------------------------------------------------
        
        if("Pb204_Pb206" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb204_Pb206)) >= 5){
            df_temp$Pb204_Pb206 <- format(round(df_temp$Pb204_Pb206, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb204_Pb206)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb204_Pb206)))
            df_temp$Pb204_Pb206 <- format(round(df_temp$Pb204_Pb206,  digits = num), nsmall = num)
          }}
        
        if("Pb204_Pb206.PM" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb204_Pb206.PM)) >= 5){
            df_temp$Pb204_Pb206.PM <- format(round(df_temp$Pb204_Pb206.PM, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb204_Pb206.PM)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb204_Pb206.PM)))
            df_temp$Pb204_Pb206.PM <- format(round(df_temp$Pb204_Pb206.PM,  digits = num), nsmall = num)
          }}
        
        if("Pb204_Pb206.PM.Per" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb204_Pb206.PM.Per)) >= 5){
            df_temp$Pb204_Pb206.PM.Per <- format(round(df_temp$Pb204_Pb206.PM.Per, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb204_Pb206.PM.Per)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb204_Pb206.PM.Per)))
            df_temp$Pb204_Pb206.PM.Per <- format(round(df_temp$Pb204_Pb206.PM.Per,  digits = num), nsmall = num)
          }}
        
        # Pb206 -------------------------------------------------------------------
        
        if("Pb206_Th232" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb206_Th232)) >= 5){
            df_temp$Pb206_Th232 <- format(round(df_temp$Pb206_Th232, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb206_Th232)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb206_Th232)))
            df_temp$Pb206_Th232 <- format(round(df_temp$Pb206_Th232,  digits = num), nsmall = num)
          }}
        
        
        if("Pb206_Th232.SD" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb206_Th232.SD)) >= 5){
            df_temp$Pb206_Th232.SD <- format(round(df_temp$Pb206_Th232.SD, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb206_Th232.SD)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb206_Th232.SD)))
            df_temp$Pb206_Th232.SD <- format(round(df_temp$Pb206_Th232.SD,  digits = num), nsmall = num)
          }}
        
        if("Pb206_U238" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb206_U238)) >= 5){
            df_temp$Pb206_U238 <- format(round(df_temp$Pb206_U238, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb206_U238)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb206_U238)))
            df_temp$Pb206_U238 <- format(round(df_temp$Pb206_U238,  digits = num), nsmall = num)
          }}
        
        if("Pb206_U238.PM" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb206_U238.PM)) >= 5){
            df_temp$Pb206_U238.PM <- format(round(df_temp$Pb206_U238.PM, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb206_U238.PM)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb206_U238.PM)))
            df_temp$Pb206_U238.PM <- format(round(df_temp$Pb206_U238.PM,  digits = num), nsmall = num)
          }}
        
        if("Pb206_U238.PM.Per" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb206_U238.PM.Per)) >= 5){
            df_temp$Pb206_U238.PM.Per <- format(round(df_temp$Pb206_U238.PM.Per, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb206_U238.PM.Per)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb206_U238.PM.Per)))
            df_temp$Pb206_U238.PM.Per <- format(round(df_temp$Pb206_U238.PM.Per,  digits = num), nsmall = num)
          }}
        
        if("Pb206_U238.SD" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb206_U238.SD)) >= 5){
            df_temp$Pb206_U238.SD <- format(round(df_temp$Pb206_U238.SD, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb206_U238.SD)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb206_U238.SD)))
            df_temp$Pb206_U238.SD <- format(round(df_temp$Pb206_U238.SD,  digits = num), nsmall = num)
          }}
        
        if("Pb206_U238.2SD" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb206_U238.2SD)) >= 5){
            df_temp$Pb206_U238.2SD <- format(round(df_temp$Pb206_U238.2SD, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb206_U238.2SD)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb206_U238.2SD)))
            df_temp$Pb206_U238.2SD <- format(round(df_temp$Pb206_U238.2SD,  digits = num), nsmall = num)
          }}
        
        if("Pb206_U238.2SE" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb206_U238.2SE)) >= 5){
            df_temp$Pb206_U238.2SE <- format(round(df_temp$Pb206_U238.2SE, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb206_U238.2SE)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb206_U238.2SE)))
            df_temp$Pb206_U238.2SE <- format(round(df_temp$Pb206_U238.2SE,  digits = num), nsmall = num)
          }}
        
        if("Pbr206_U238" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pbr206_U238)) >= 5){
            df_temp$Pbr206_U238 <- format(round(df_temp$Pbr206_U238, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pbr206_U238)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pbr206_U238)))
            df_temp$Pbr206_U238 <- format(round(df_temp$Pbr206_U238,  digits = num), nsmall = num)
          }}
        
        if("Pbr206_U238.PM" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pbr206_U238.PM)) >= 5){
            df_temp$Pbr206_U238.PM <- format(round(df_temp$Pbr206_U238.PM, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pbr206_U238.PM)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pbr206_U238.PM)))
            df_temp$Pbr206_U238.PM <- format(round(df_temp$Pbr206_U238.PM,  digits = num), nsmall = num)
          }}
        
        if("Pbr206_U238.PM.Per" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pbr206_U238.PM.Per)) >= 5){
            df_temp$Pbr206_U238.PM.Per <- format(round(df_temp$Pbr206_U238.PM.Per, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pbr206_U238.PM.Per)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pbr206_U238.PM.Per)))
            df_temp$Pbr206_U238.PM.Per <- format(round(df_temp$Pbr206_U238.PM.Per,  digits = num), nsmall = num)
          }}
        
        # U238 --------------------------------------------------------------------
        
        if("U238_Pb206" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$U238_Pb206)) >= 5){
            df_temp$U238_Pb206 <- format(round(df_temp$U238_Pb206, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$U238_Pb206)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$U238_Pb206)))
            df_temp$U238_Pb206 <- format(round(df_temp$U238_Pb206,  digits = num), nsmall = num)
          }}
        
        
        if("U238_Pb206.Per" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$U238_Pb206.Per)) >= 5){
            df_temp$U238_Pb206.Per <- format(round(df_temp$U238_Pb206.Per, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$U238_Pb206.Per)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$U238_Pb206.Per)))
            df_temp$U238_Pb206.Per <- format(round(df_temp$U238_Pb206.Per,  digits = num), nsmall = num)
          }}
        
        if("U238_Pb206.PM" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$U238_Pb206.PM)) >= 5){
            df_temp$U238_Pb206.PM <- format(round(df_temp$U238_Pb206.PM, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$U238_Pb206.PM)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$U238_Pb206.PM)))
            df_temp$U238_Pb206.PM <- format(round(df_temp$U238_Pb206.PM,  digits = num), nsmall = num)
          }}
        
        if("U238_Pb206.PM.Per" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$U238_Pb206.PM.Per)) >= 5){
            df_temp$U238_Pb206.PM.Per <- format(round(df_temp$U238_Pb206.PM.Per, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$U238_Pb206.PM.Per)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$U238_Pb206.PM.Per)))
            df_temp$U238_Pb206.PM.Per <- format(round(df_temp$U238_Pb206.PM.Per,  digits = num), nsmall = num)
          }}
        
        if("U238_Pb206.SD" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$U238_Pb206.SD)) >= 5){
            df_temp$U238_Pb206.SD <- format(round(df_temp$U238_Pb206.SD, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$U238_Pb206.SD)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$U238_Pb206.SD)))
            df_temp$U238_Pb206.SD <- format(round(df_temp$U238_Pb206.SD,  digits = num), nsmall = num)
          }}
        
        if("U238_Pbr206" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$U238_Pbr206)) >= 5){
            df_temp$U238_Pbr206 <- format(round(df_temp$U238_Pbr206, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$U238_Pbr206)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$U238_Pbr206)))
            df_temp$U238_Pbr206 <- format(round(df_temp$U238_Pbr206,  digits = num), nsmall = num)
          }}
        
        
        if("U238_Pbr206.PM" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$U238_Pbr206.PM)) >= 5){
            df_temp$U238_Pbr206.PM <- format(round(df_temp$U238_Pbr206.PM, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$U238_Pbr206.PM)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$U238_Pbr206.PM)))
            df_temp$U238_Pbr206.PM <- format(round(df_temp$U238_Pbr206.PM,  digits = num), nsmall = num)
          }}
        
        if("U238_Pbr206.PM.Per" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$U238_Pbr206.PM.Per)) >= 5){
            df_temp$U238_Pbr206.PM.Per <- format(round(df_temp$U238_Pbr206.PM.Per, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$U238_Pbr206.PM.Per)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$U238_Pbr206.PM.Per)))
            df_temp$U238_Pbr206.PM.Per <- format(round(df_temp$U238_Pbr206.PM.Per,  digits = num), nsmall = num)
          }}
        
        # Pb207 -------------------------------------------------------------------
        
        if("Pb207_Pb206" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb207_Pb206)) >= 5){
            df_temp$Pb207_Pb206 <- format(round(df_temp$Pb207_Pb206, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb207_Pb206)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb207_Pb206)))
            df_temp$Pb207_Pb206 <- format(round(df_temp$Pb207_Pb206,  digits = num), nsmall = num)
          }}
        
        if("Pb207_Pb206.Per" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb207_Pb206.Per)) >= 5){
            df_temp$Pb207_Pb206.Per <- format(round(df_temp$Pb207_Pb206.Per, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb207_Pb206.Per)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb207_Pb206.Per)))
            df_temp$Pb207_Pb206.Per <- format(round(df_temp$Pb207_Pb206.Per,  digits = num), nsmall = num)
          }}
        
        if("Pb207_Pb206.PM" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb207_Pb206.PM)) >= 5){
            df_temp$Pb207_Pb206.PM <- format(round(df_temp$Pb207_Pb206.PM, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb207_Pb206.PM)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb207_Pb206.PM)))
            df_temp$Pb207_Pb206.PM <- format(round(df_temp$Pb207_Pb206.PM,  digits = num), nsmall = num)
          }}
        
        if("Pb207_Pb206.PM.Per" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb207_Pb206.PM.Per)) >= 5){
            df_temp$Pb207_Pb206.PM.Per <- format(round(df_temp$Pb207_Pb206.PM.Per, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb207_Pb206.PM.Per)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb207_Pb206.PM.Per)))
            df_temp$Pb207_Pb206.PM.Per <- format(round(df_temp$Pb207_Pb206.PM.Per,  digits = num), nsmall = num)
          }}
        
        if("Pb207_Pb206.SD" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb207_Pb206.SD)) >= 5){
            df_temp$Pb207_Pb206.SD <- format(round(df_temp$Pb207_Pb206.SD, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb207_Pb206.SD)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb207_Pb206.SD)))
            df_temp$Pb207_Pb206.SD <- format(round(df_temp$Pb207_Pb206.SD,  digits = num), nsmall = num)
          }}
        
        if("Pb207_Pb206.2SD" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb207_Pb206.2SD)) >= 5){
            df_temp$Pb207_Pb206.2SD <- format(round(df_temp$Pb207_Pb206.2SD, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb207_Pb206.2SD)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb207_Pb206.2SD)))
            df_temp$Pb207_Pb206.2SD <- format(round(df_temp$Pb207_Pb206.2SD,  digits = num), nsmall = num)
          }}
        
        if("Pb207_Pb206.2SE" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb207_Pb206.2SE)) >= 5){
            df_temp$Pb207_Pb206.2SE <- format(round(df_temp$Pb207_Pb206.2SE, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb207_Pb206.2SE)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb207_Pb206.2SE)))
            df_temp$Pb207_Pb206.2SE <- format(round(df_temp$Pb207_Pb206.2SE,  digits = num), nsmall = num)
          }}
        
        if("Pbr207_Pb206" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pbr207_Pb206)) >= 5){
            df_temp$Pbr207_Pb206 <- format(round(df_temp$Pbr207_Pb206, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pbr207_Pb206)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pbr207_Pb206)))
            df_temp$Pbr207_Pb206 <- format(round(df_temp$Pbr207_Pb206,  digits = num), nsmall = num)
          }}
        
        if("Pbr207_Pb206.PM" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pbr207_Pb206.PM)) >= 5){
            df_temp$Pbr207_Pb206.PM <- format(round(df_temp$Pbr207_Pb206.PM, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pbr207_Pb206.PM)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pbr207_Pb206.PM)))
            df_temp$Pbr207_Pb206.PM <- format(round(df_temp$Pbr207_Pb206.PM,  digits = num), nsmall = num)
          }}
        
        if("Pbr207_Pb206.PM.Per" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pbr207_Pb206.PM.Per)) >= 5){
            df_temp$Pbr207_Pb206.PM.Per <- format(round(df_temp$Pbr207_Pb206.PM.Per, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pbr207_Pb206.PM.Per)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pbr207_Pb206.PM.Per)))
            df_temp$Pbr207_Pb206.PM.Per <- format(round(df_temp$Pbr207_Pb206.PM.Per,  digits = num), nsmall = num)
          }}
        
        if("Pbr207_Pbr206" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pbr207_Pbr206)) >= 5){
            df_temp$Pbr207_Pbr206 <- format(round(df_temp$Pbr207_Pbr206, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pbr207_Pbr206)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pbr207_Pbr206)))
            df_temp$Pbr207_Pbr206 <- format(round(df_temp$Pbr207_Pbr206,  digits = num), nsmall = num)
          }}
        
        if("Pbr207_Pbr206.PM.Per" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pbr207_Pbr206.PM.Per)) >= 5){
            df_temp$Pbr207_Pbr206.PM.Per <- format(round(df_temp$Pbr207_Pbr206.PM.Per, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pbr207_Pbr206.PM.Per)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pbr207_Pbr206.PM.Per)))
            df_temp$Pbr207_Pbr206.PM.Per <- format(round(df_temp$Pbr207_Pbr206.PM.Per,  digits = num), nsmall = num)
          }}
        
        if("Pb207_U235" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb207_U235)) >= 5){
            df_temp$Pb207_U235 <- format(round(df_temp$Pb207_U235, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb207_U235)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb207_U235)))
            df_temp$Pb207_U235 <- format(round(df_temp$Pb207_U235,  digits = num), nsmall = num)
          }}
        
        if("Pb207_U235.PM.Per" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb207_U235.PM.Per)) >= 5){
            df_temp$Pb207_U235.PM.Per <- format(round(df_temp$Pb207_U235.PM.Per, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb207_U235.PM.Per)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb207_U235.PM.Per)))
            df_temp$Pb207_U235.PM.Per <- format(round(df_temp$Pb207_U235.PM.Per,  digits = num), nsmall = num)
          }}
        
        if("Pb207_U235.SD" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb207_U235.SD)) >= 5){
            df_temp$Pb207_U235.SD <- format(round(df_temp$Pb207_U235.SD, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb207_U235.SD)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb207_U235.SD)))
            df_temp$Pb207_U235.SD <- format(round(df_temp$Pb207_U235.SD,  digits = num), nsmall = num)
          }}
        
        if("Pb207_U235.2SD" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb207_U235.2SD)) >= 5){
            df_temp$Pb207_U235.2SD <- format(round(df_temp$Pb207_U235.2SD, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb207_U235.2SD)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb207_U235.2SD)))
            df_temp$Pb207_U235.2SD <- format(round(df_temp$Pb207_U235.2SD,  digits = num), nsmall = num)
          }}
        
        if("Pb207_U235.2SE" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb207_U235.2SE)) >= 5){
            df_temp$Pb207_U235.2SE <- format(round(df_temp$Pb207_U235.2SE, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb207_U235.2SE)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb207_U235.2SE)))
            df_temp$Pb207_U235.2SE <- format(round(df_temp$Pb207_U235.2SE,  digits = num), nsmall = num)
          }}
        
        if("Pbr207_U235" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pbr207_U235)) >= 5){
            df_temp$Pbr207_U235 <- format(round(df_temp$Pbr207_U235, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pbr207_U235)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pbr207_U235)))
            df_temp$Pbr207_U235 <- format(round(df_temp$Pbr207_U235,  digits = num), nsmall = num)
          }}
        
        if("Pbr207_U235.PM" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pbr207_U235.PM)) >= 5){
            df_temp$Pbr207_U235.PM <- format(round(df_temp$Pbr207_U235.PM, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pbr207_U235.PM)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pbr207_U235.PM)))
            df_temp$Pbr207_U235.PM <- format(round(df_temp$Pbr207_U235.PM,  digits = num), nsmall = num)
          }}
        
        if("Pbr207_U235.PM.Per" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pbr207_U235.PM.Per)) >= 5){
            df_temp$Pbr207_U235.PM.Per <- format(round(df_temp$Pbr207_U235.PM.Per, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pbr207_U235.PM.Per)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pbr207_U235.PM.Per)))
            df_temp$Pbr207_U235.PM.Per <- format(round(df_temp$Pbr207_U235.PM.Per,  digits = num), nsmall = num)
          }}
        
        # Pb208 -------------------------------------------------------------------
        
        if("Pb208_Pb206" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb208_Pb206)) >= 5){
            df_temp$Pb208_Pb206 <- format(round(df_temp$Pb208_Pb206, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb208_Pb206)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb208_Pb206)))
            df_temp$Pb208_Pb206 <- format(round(df_temp$Pb208_Pb206,  digits = num), nsmall = num)
          }}
        
        if("Pb208_Pb206.PM.Per" %in% names(df_temp) ==T){
          if(decimal(data.frame(df_temp$Pb208_Pb206.PM.Per)) >= 5){
            df_temp$Pb208_Pb206.PM.Per <- format(round(df_temp$Pb208_Pb206.PM.Per, digits = 5), nsmall = 5)
          }else if(decimal(data.frame(df_temp$Pb208_Pb206.PM.Per)) < 5){
            num <- as.numeric(decimal(data.frame(df_temp$Pb208_Pb206.PM.Per)))
            df_temp$Pb208_Pb206.PM.Per <- format(round(df_temp$Pb208_Pb206.PM.Per,  digits = num), nsmall = num)
          }}
        
        # Pb206.Age ---------------------------------------------------------------
        
        if("Pb206_U238.Age" %in% names(df_temp) ==T){
          df_temp$Pb206_U238.Age <- df_temp$Pb206_U238.Age*0.000001 
          df_temp$Pb206_U238.Age <- format(round(df_temp$Pb206_U238.Age, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb206_U238.Age"] = "Pb206_U238.Age(Ma)"
        }
        
        if("Pb206_U238.Age.PM" %in% names(df_temp) ==T){
          df_temp$Pb206_U238.Age.PM <- df_temp$Pb206_U238.Age.PM*0.000001 
          df_temp$Pb206_U238.Age.PM <- format(round(df_temp$Pb206_U238.Age.PM, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb206_U238.Age.PM"] = "Pb206_U238.Age.PM(Ma)"
        }
        
        if("Pb206_U238.Age.PM.Per" %in% names(df_temp) ==T){
          df_temp$Pb206_U238.Age.PM.Per <- df_temp$Pb206_U238.Age.PM.Per*0.000001 
          df_temp$Pb206_U238.Age.PM.Per <- format(round(df_temp$Pb206_U238.Age.PM.Per, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb206_U238.Age.PM.Per"] = "Pb206_U238.Age.PM.Per(Ma)"
        }
        
        if("Pb206_U238.Age.SD" %in% names(df_temp) ==T){
          df_temp$Pb206_U238.Age.SD <- df_temp$Pb206_U238.Age.SD*0.000001 
          df_temp$Pb206_U238.Age.SD <- format(round(df_temp$Pb206_U238.Age.SD, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb206_U238.Age.SD"] = "Pb206_U238.Age.SD(Ma)"
        }
        
        if("Pb206_U238.Age.2SD" %in% names(df_temp) ==T){
          df_temp$Pb206_U238.Age.2SD <- df_temp$Pb206_U238.Age.2SD*0.000001 
          df_temp$Pb206_U238.Age.2SD <- format(round(df_temp$Pb206_U238.Age.2SD, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb206_U238.Age.2SD"] = "Pb206_U238.Age.2SD(Ma)"
        }
        
        if("Pb206_U238.Age.2SE" %in% names(df_temp) ==T){
          df_temp$Pb206_U238.Age.2SE <- df_temp$Pb206_U238.Age.2SE*0.000001 
          df_temp$Pb206_U238.Age.2SE <- format(round(df_temp$Pb206_U238.Age.2SE, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb206_U238.Age.2SE"] = "Pb206_U238.Age.2SE(Ma)"
        }
        
        if("Pbr206_U238.Age" %in% names(df_temp) ==T){
          df_temp$Pbr206_U238.Age <- df_temp$Pbr206_U238.Age*0.000001 
          df_temp$Pbr206_U238.Age <- format(round(df_temp$Pbr206_U238.Age, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pbr206_U238.Age"] = "Pbr206_U238.Age(Ma)"
        }
        
        if("Pbr206_U238.Age.PM" %in% names(df_temp) ==T){
          df_temp$Pbr206_U238.Age.PM <- df_temp$Pbr206_U238.Age.PM*0.000001 
          df_temp$Pbr206_U238.Age.PM <- format(round(df_temp$Pbr206_U238.Age.PM, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pbr206_U238.Age.PM"] = "Pbr206_U238.Age.PM(Ma)"
        }
        
        if("Pbr206_U238.Age.PM.Per" %in% names(df_temp) ==T){
          df_temp$Pbr206_U238.Age.PM.Per <- df_temp$Pbr206_U238.Age.PM.Per*0.000001 
          df_temp$Pbr206_U238.Age.PM.Per <- format(round(df_temp$Pbr206_U238.Age.PM.Per, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pbr206_U238.Age.PM.Per"] = "Pbr206_U238.Age.PM.Per(Ma)"
        }
        
        # U238 Age--------------------------------------------------------------------
        
        if("U238_Pb206.Age" %in% names(df_temp) ==T){
          df_temp$U238_Pb206.Age <- df_temp$U238_Pb206.Age*0.000001 
          df_temp$U238_Pb206.Age <- format(round(df_temp$U238_Pb206.Age, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "U238_Pb206.Age"] = "U238_Pb206.Age(Ma)"
        }
        
        if("U238_Pb206.Age.PM.Per" %in% names(df_temp) ==T){
          df_temp$U238_Pb206.Age.PM.Per <- df_temp$U238_Pb206.Age.PM.Per*0.000001 
          df_temp$U238_Pb206.Age.PM.Per <- format(round(df_temp$U238_Pb206.Age.PM.Per, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "U238_Pb206.Age.PM.Per"] = "U238_Pb206.Age.PM.Per(Ma)"
        }
        
        # Pb207 Age ---------------------------------------------------------------
        
        if("Pb207_Pb206.Age" %in% names(df_temp) ==T){
          df_temp$Pb207_Pb206.Age <- df_temp$Pb207_Pb206.Age*0.000001 
          df_temp$Pb207_Pb206.Age <- format(round(df_temp$Pb207_Pb206.Age, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb207_Pb206.Age"] = "Pb207_Pb206.Age(Ma)"
        }
        
        if("Pb207_Pb206.Age.PM" %in% names(df_temp) ==T){
          df_temp$Pb207_Pb206.Age.PM <- df_temp$Pb207_Pb206.Age.PM*0.000001 
          df_temp$Pb207_Pb206.Age.PM <- format(round(df_temp$Pb207_Pb206.Age.PM, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb207_Pb206.Age.PM"] = "Pb207_Pb206.Age.PM(Ma)"
        }
        
        if("Pb207_Pb206.Age.PM.Per" %in% names(df_temp) ==T){
          df_temp$Pb207_Pb206.Age.PM.Per <- df_temp$Pb207_Pb206.Age.PM.Per*0.000001 
          df_temp$Pb207_Pb206.Age.PM.Per <- format(round(df_temp$Pb207_Pb206.Age.PM.Per, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb207_Pb206.Age.PM.Per"] = "Pb207_Pb206.Age.PM.Per(Ma)"
        }
        
        if("Pb207_Pb206.Age.SD" %in% names(df_temp) ==T){
          df_temp$Pb207_Pb206.Age.SD <- df_temp$Pb207_Pb206.Age.SD*0.000001 
          df_temp$Pb207_Pb206.Age.SD <- format(round(df_temp$Pb207_Pb206.Age.SD, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb207_Pb206.Age.SD"] = "Pb207_Pb206.Age.SD(Ma)"
        }
        
        if("Pb207_Pb206.Age.2SD" %in% names(df_temp) ==T){
          df_temp$Pb207_Pb206.Age.2SD <- df_temp$Pb207_Pb206.Age.2SD*0.000001 
          df_temp$Pb207_Pb206.Age.2SD <- format(round(df_temp$Pb207_Pb206.Age.2SD, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb207_Pb206.Age.2SD"] = "Pb207_Pb206.Age.2SD(Ma)"
        }
        
        if("Pb207_Pb206.Age.2SE" %in% names(df_temp) ==T){
          df_temp$Pb207_Pb206.Age.2SE <- df_temp$Pb207_Pb206.Age.2SE*0.000001 
          df_temp$Pb207_Pb206.Age.2SE <- format(round(df_temp$Pb207_Pb206.Age.2SE, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb207_Pb206.Age.2SE"] = "Pb207_Pb206.Age.2SE(Ma)"
        }
        
        if("Pbr207_Pbr206.Age" %in% names(df_temp) ==T){
          df_temp$Pbr207_Pbr206.Age <- df_temp$Pbr207_Pbr206.Age*0.000001 
          df_temp$Pbr207_Pbr206.Age <- format(round(df_temp$Pbr207_Pbr206.Age, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pbr207_Pbr206.Age"] = "Pbr207_Pbr206.Age(Ma)"
        }
        
        if("Pbr207_Pbr206.Age.PM" %in% names(df_temp) ==T){
          df_temp$Pbr207_Pbr206.Age.PM <- df_temp$Pbr207_Pbr206.Age.PM*0.000001 
          df_temp$Pbr207_Pbr206.Age.PM <- format(round(df_temp$Pbr207_Pbr206.Age.PM, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pbr207_Pbr206.Age.PM"] = "Pbr207_Pbr206.Age.PM(Ma)"
        }
        
        if("Pbr207_Pbr206.Age.PM.Per" %in% names(df_temp) ==T){
          df_temp$Pbr207_Pbr206.Age.PM.Per <- df_temp$Pbr207_Pbr206.Age.PM.Per*0.000001 
          df_temp$Pbr207_Pbr206.Age.PM.Per <- format(round(df_temp$Pbr207_Pbr206.Age.PM.Per, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pbr207_Pbr206.Age.PM.Per"] = "Pbr207_Pbr206.Age.PM.Per(Ma)"
        }
        
        if("Pb207_U235.Age" %in% names(df_temp) ==T){
          df_temp$Pb207_U235.Age <- df_temp$Pb207_U235.Age*0.000001 
          df_temp$Pb207_U235.Age <- format(round(df_temp$Pb207_U235.Age, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb207_U235.Age"] = "Pb207_U235.Age(Ma)"
        }
        
        if("Pb207_U235.Age.PM" %in% names(df_temp) ==T){
          df_temp$Pb207_U235.Age.PM <- df_temp$Pb207_U235.Age.PM*0.000001 
          df_temp$Pb207_U235.Age.PM <- format(round(df_temp$Pb207_U235.Age.PM, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb207_U235.Age.PM"] = "Pb207_U235.Age.PM(Ma)"
        }
        
        if("Pb207_U235.Age.SD" %in% names(df_temp) ==T){
          df_temp$Pb207_U235.Age.SD <- df_temp$Pb207_U235.Age.SD*0.000001 
          df_temp$Pb207_U235.Age.SD <- format(round(df_temp$Pb207_U235.Age.SD, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb207_U235.Age.SD"] = "Pb207_U235.Age.SD(Ma)"
        }
        
        if("Pb207_U235.Age.2SD" %in% names(df_temp) ==T){
          df_temp$Pb207_U235.Age.2SD <- df_temp$Pb207_U235.Age.2SD*0.000001 
          df_temp$Pb207_U235.Age.2SD <- format(round(df_temp$Pb207_U235.Age.2SD, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb207_U235.Age.2SD"] = "Pb207_U235.Age.2SD(Ma)"
        }
        
        if("Pb207_U235.Age.2SE" %in% names(df_temp) ==T){
          df_temp$Pb207_U235.Age.2SE <- df_temp$Pb207_U235.Age.2SE*0.000001 
          df_temp$Pb207_U235.Age.2SE <- format(round(df_temp$Pb207_U235.Age.2SE, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb207_U235.Age.2SE"] = "Pb207_U235.Age.2SE(Ma)"
        }
        
        # Pb208_Age ---------------------------------------------------------------
        
        if("Pb208_Th232.Age" %in% names(df_temp) ==T){
          df_temp$Pb208_Th232.Age <- df_temp$Pb208_Th232.Age*0.000001 
          df_temp$Pb208_Th232.Age <- format(round(df_temp$Pb208_Th232.Age, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb208_Th232.Age"] = "Pb208_Th232.Age(Ma)"
        }
        
        if("Pb208_Th232.Age.PM" %in% names(df_temp) ==T){
          df_temp$Pb208_Th232.Age.PM <- df_temp$Pb208_Th232.Age.PM*0.000001 
          df_temp$Pb208_Th232.Age.PM <- format(round(df_temp$Pb208_Th232.Age.PM, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Pb208_Th232.Age.PM"] = "Pb208_Th232.Age.PM(Ma)"
        }
        
        if("Age" %in% names(df_temp) ==T){
          df_temp$Age <- df_temp$Age*0.000001 
          df_temp$Age <- format(round(df_temp$Age, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Age"] = "Age(Ma)"
        }
        
        if("Age.PM" %in% names(df_temp) ==T){
          df_temp$Age.PM <- df_temp$Age.PM*0.000001 
          df_temp$Age.PM <- format(round(df_temp$Age.PM, digits = 0), nsmall = 0)
          names(df_temp)[names(df_temp) == "Age.PM"] = "Age.PM(Ma)"
        }
        
        
        if("Disc" %in% names(df_temp) == T){
          df_temp$Disc <- format(round(df_temp$Disc, digits = 0), nsmall = 0)
        }
        
        df <- df_temp
        
      }
      
    })
    
    output$mapisotbl <- renderDataTable(
      mapisodata(),
      
      rownames = F,
      selection = "single")
    
    output$cntmap_iso <- renderUI({
      paste("Isotope Data:", nrow(mapisodata()))
    })
    
    
    submaprefdata <- reactive({
      
      if(length(submapspldata())){
        submapref <- mapspltbldata()
      }
      
      refid <- substr(as.character(submapspldata()),1,9)
      
      submapref <- submapref[,c(1, 17:25)]
      submapref <- unique(submapref)
      
      submapref <- submapref[submapref$RefID %in% refid,]
    })
    
    output$mapreftbl <- renderDataTable(
      submaprefdata(),
      
      rownames = F,
      selection = "single",
      options = list(columnDefs = list(list(visible = F, targets = c(0))))
    )
    
    output$cntmap_ref <- renderUI({
      paste("Reference Information:", nrow(submaprefdata()))
    })
  })
  


  
  ## download pdf -----------------------------------------------------------------------------------------
  
  
  output$template <- renderUI({
    
    tags$iframe(src = "manual.pdf", style = "height:800px; width:100%; scrolling = yes")
    
  })
  
  
  
  
  
  ## download template -----------------------------------------------------------------------------------------
  
  
  # Info -----------------------------------------------------------------------------------------
  
  output$DBinfo <- downloadHandler(
    
    filename = function(){
      
      paste("Info","xlsx",sep = ".")
      
    },
    
    content = function(file){
      
      file.copy(file.path(getwd(), "www", "Info.xlsx"), file, overwrite = T)
      
    },
    
    contentType = "text/xlsx"
  )
  
  
  
  
  # Ar-Ar ----------------------------------------------------------------------------------------
  
  output$DBar <- downloadHandler(
    
    filename = function(){
      
      paste("Ar-Ar","xlsx",sep = ".")
      
    },
    
    content = function(file){
      
      file.copy(file.path(getwd(), "www", "Ar-Ar.xlsx"), file, overwrite = T)
      
    },
    
    contentType = "text/xlsx"
  )
  
  
  # Cosmogenic ----------------------------------------------------------------------------------------
  
  output$DBcosma <- downloadHandler(
    
    filename = function(){
      
      paste("Cosmogenic","xlsx",sep = ".")
      
    },
    
    content = function(file){
      
      file.copy(file.path(getwd(), "www", "Cosmogenic.xlsx"), file, overwrite = T)
      
    },
    
    contentType = "text/xlsx"
  )
  
  
  # K-Ar ----------------------------------------------------------------------------------------
  
  output$DBkar <- downloadHandler(
    
    filename = function(){
      
      paste("K-Ar","xlsx",sep = ".")
      
    },
    
    content = function(file){
      
      file.copy(file.path(getwd(), "www", "K-Ar.xlsx"), file, overwrite = T)
      
    },
    
    contentType = "text/xlsx"
  )
  
  
  # Lu-Hf ----------------------------------------------------------------------------------------
  
  output$DBluhf <- downloadHandler(
    
    filename = function(){
      
      paste("Lu-Hf","xlsx",sep = ".")
      
    },
    
    content = function(file){
      
      file.copy(file.path(getwd(), "www", "Lu-Hf.xlsx"), file, overwrite = T)
      
    },
    
    contentType = "text/xlsx"
  )
  
  
  
  # OSL ----------------------------------------------------------------------------------------
  
  output$DBosl <- downloadHandler(
    
    filename = function(){
      
      paste("OSL","xlsx",sep = ".")
      
    },
    
    content = function(file){
      
      file.copy(file.path(getwd(), "www", "OSL.xlsx"), file, overwrite = T)
      
    },
    
    contentType = "text/xlsx"
  )
  
  
  
  # Sm-Nd ----------------------------------------------------------------------------------------
  
  output$DBsmnd <- downloadHandler(
    
    filename = function(){
      
      paste("Sm-Nd","xlsx",sep = ".")
      
    },
    
    content = function(file){
      
      file.copy(file.path(getwd(), "www", "Sm-Nd.xlsx"), file, overwrite = T)
      
    },
    
    contentType = "text/xlsx"
  )
  
  
  
  # U-Pb ----------------------------------------------------------------------------------------
  
  output$DBupb <- downloadHandler(
    
    filename = function(){
      
      paste("U-Pb","xlsx",sep = ".")
      
    },
    
    content = function(file){
      
      file.copy(file.path(getwd(), "www", "U-Pb.xlsx"), file, overwrite = T)
      
    },
    
    contentType = "text/xlsx"
  )
  
  
  # Submit Data -------------------------------------------------------------
  
  hideTab(inputId = "tabs", target = "Step 2")
  hideTab(inputId = "tabs", target = "Step 3")
  hideTab(inputId = "tabs", target = "Step 4")
  

 # Step 1 ------------------------------------------------------------------


  observeEvent(input$dupliinfo, {
    
    load("RData/OrgRData/ref.RData")
    load("RData/OrgRData/spl.RData")
    load("RData/OrgRData/age.RData")
    
    if(file.exists("RData/UserRData/ref_custom.RData")){
      load("RData/UserRData/ref_custom.RData")
      load("RData/UserRData/spl_custom.RData")
      load("RData/UserRData/age_custom.RData")  
      if(nrow(ref_custom) == 0){
        ref
      }else if(nrow(ref_custom) != 0){
        ref <- rbind(ref, ref_custom)
      }
      if(nrow(spl_custom) == 0){
        spl <- spl
      }else if(nrow(spl_custom) != 0){
        spl <- rbind(spl, spl_custom)
      }
      if(nrow(age_custom) == 0){
        age <- age
      }else if(nrow(age_custom) != 0){
        age <- rbind(age, age_custom)
      }  
    }

# file existence ----------------------------------------------------------

    sheet_nm <- excel_sheets(input$dupliinfo$datapath)
    
    for(i in 1:length(sheet_nm)){
      
      vname <- sheet_nm[i]
      
      if(vname == "Reference"){
        
        assign(vname, read_xlsx(input$dupliinfo$datapath, sheet = "Reference", 
                                col_types = c("text", "text", "text", "numeric", "text", "text", "text", "text", "text"),
                                na = "NA"))
        
        output$dupliref <- renderDataTable(
          
          Reference
          
        )
        
      }else if(vname == "Sample"){
        
        assign(vname, read_xlsx(input$dupliinfo$datapath, sheet = "Sample", 
                                col_types = c("text","text","numeric","numeric","text"),
                                na = "NA"))
        
        output$duplispl <- renderDataTable(
          Sample
        )
      }else if(vname == "Age"){
        
        assign(vname, read_xlsx(input$dupliinfo$datapath, sheet = "Age",
                                col_types = c("text", "text", "numeric","numeric","numeric", "text","text","text"),
                                na = "NA"))
        
        output$dupliage <- renderDataTable(
          
          Age
          
        )
        
      }
      
    }
    
    ## -- cheking duplicates --
    
    if(Reference[1,5] %in% ref$DOI){

      for(i in 1:nrow(Sample)){
        
        load_spl <- data.frame()
        
        if(Sample[i,1] %in% spl$Sample == T){
          
          load_spl_temp <- c("yes")
          
        }else if(tolower(Sample[i,1]) %in% tolower(spl$Sample) == F){
          
          load_spl_temp <- c("no")
          
        }
        load_spl <- rbind(load_spl, load_spl_temp)
      }
      
      names(load_spl) <- c("result")
        
      if(all(load_spl$result == "yes") == T){
        
        spl$Longitude <- round(spl$Longitude, digits = 2)
        spl$Latitude <- round(spl$Latitude, digits = 2)
        
    
        Sample$Longitude[i] <- round(as.numeric(Sample$Longitude[i]), digits = 2)
        Sample$Latitude[i] <- round(as.numeric(Sample$Latitude[i]), digits = 2)
         
        
        load_loc <- data.frame()
        
        if(Sample[i,3] %in% spl$Longitude){
          
          if(Sample[i,4] %in% spl$Latitude){
            
            load_loc_temp <- c("yes")
            
          }else{
            
            load_loc_temp <- c("no")
            
          }
          
        }
        
        load_loc <- rbind(load_loc, load_loc_temp)
        
        names(load_loc) <- c("result")
        
        if(all(load_loc$result == "yes") == T){
          
          load_mat <- data.frame()
          
          if(Sample[i,2] %in% spl$Material == T){
            
            load_mat_temp <- c("yes")
            
          }else if(Sample[i,2] %in% spl$Material == F){
            
            load_mat_temp <- c("no")
            
          }
          
          load_mat <- rbind(load_mat, load_mat_temp)
          
        }
        
        names(load_mat) <- c("result")
        
        if(all(load_mat$result == "yes") == T){
          
          load_age <- data.frame()
          
          for(j in 1:nrow(Age)){
            
            met_cd_temp <- as.numeric(Age[j,1])
            
            load_met_cd <- sprintf("%03d", met_cd_temp)
            
            
            if(load_met_cd %in% age$Method_cd == T){
              
              if(Age[j,2] %in% age$Sample == T){
                
                load_age_temp <- c("yes")
                
              }
              
            }else {
              
              load_age_temp <- c("no")
              
            }
          }
          load_age <- rbind(load_age, load_age_temp)
          
          names(load_age) <- c("result")
          
          if(all(load_age$result == "yes") == T){
            
            shinyalert(inputId = "dupli_alr", "Wait!", 
                       "Your entries seem to exist in the database. Would you like to continue ?", type = "warning",
                       confirmButtonText = "Proceed anyway",
                       cancelButtonText = "Cancel", showCancelButton = T,
                       callbackR = function(x) { if(x == T){
                         
                         hideTab("tabs", target = "Step 1")
                         showTab("tabs", target = "Step 2")
                       }else if(x == F){
                         reset("dupliinfo")
                         output$dupliref <- renderDataTable(NULL)
                         output$duplispl <- renderDataTable(NULL)
                         output$dupliage <- renderDataTable(NULL)
                       }
                         
                         })
          }
        }
      }
    }
      
      observeEvent(input$dupliNext, {
        
        showTab("tabs", target = "Step 2")
        hideTab("tabs", target = "Step 1")
        
      })
    
  })
  
  # Step 2 ------------------------------------------------------------------
  
  observeEvent(input$fileinfo,{
    
    load("RData/OrgRData/ref.RData")
    load("RData/OrgRData/spl.RData")
    load("RData/OrgRData/age.RData")
    
    if(file.exists("RData/UserRData/ref_custom.RData")){
      load("RData/UserRData/ref_custom.RData")
      load("RData/UserRData/spl_custom.RData")
      load("RData/UserRData/age_custom.RData")
      if(nrow(ref_custom) == 0){
        ref_all <- ref
      }else if(nrow(ref_custom) != 0){
        ref_all <- rbind(ref, ref_custom)
      }
      
      if(nrow(spl_custom) == 0){
        spl_all <- spl
      }else if(nrow(spl_custom) != 0){
        spl_all <- rbind(spl, spl_custom)
      }
      
      if(nrow(age_custom) == 0){
        age_all <- age
      }else if(nrow(age_custom) != 0){
        age_all <- rbind(age, age_custom)
      }
    }else{
      ref_all <- ref
      spl_all <- spl
      age_all <- age
    }
    
    reage <- reactive(
      age_all
    )
    
    respl <- reactive(
      spl_all
    )
    
    reref <- reactive(
      ref_all
    )
    
    ## --Assigning ID ---
    jour1 <- data.frame(table(reref()$Journal))
    
    names(jour1) <- c("jour", "N")
    
    jour2 <- data.frame(substr(as.character(reref()$RefID),2,5), reref()$Journal)
    jour2 <- data.frame(unique(jour2))
    names(jour2) <- c("seq", "jour")
    
    
    jour <- join(jour2, jour1, by = "jour")
    
    sheet_nm <- excel_sheets(input$fileinfo$datapath)
    
    for(i in 1:length(sheet_nm)){
      
      vname <- sheet_nm[i]
      
      
      if(vname == "Reference"){
        
        assign(vname, read_xlsx(input$fileinfo$datapath, sheet = "Reference", 
                                col_types = c("text", "text", "text", "numeric", "text", "text", "text", "text", "text"),
                                na = "NA"))
        
        jour_new <- as.character(Reference[1,2])
        jud <- ifelse(Reference[1,2] %in% jour$jour, "yes", "no")
        
        if(jud == "yes"){
          
          j = 1
          
          for(j in 1:nrow(jour)){
            
            jour_seq <- as.numeric(jour[j,1])
            jour_jour <- as.character(jour[j,2])
            jour_N <- as.numeric(jour[j,3])
            
            if(jour_jour == jour_new){
              
              seq <- jour_seq
              num <- jour_N + 1
              
            }}
          
        }else if(jud == "no"){
          
          j = 1
          
          for(j in 1:nrow(jour)){
            
            seq <- length(jour$jour) + 1
            num <- 1
            
          }}
        
        
        refID_seq <- sprintf("%04d", seq)
        refID_num <- sprintf("%04d", num)
        
        refID_temp <- data.frame(paste0("C", refID_seq, refID_num))
        
        names(refID_temp) <- c("RefID")
        
        ref_temp <- data.frame(refID_temp, Reference)
        
        names(ref_temp) <- c("RefID", "Title", "Journal", "Author", "Year", "DOI", "URL", "Vol", "Issue", "Page")
        
        temp_ref <- ref_temp
        
        output$refUpload <- renderDataTable(
          
          temp_ref <- temp_ref %>% 
            filter(RefID == refID_temp$RefID),
          
          options = list(
            columnDefs = list(list(visible = F, targets = c(1))))
          
        )
        
      }
      
      
      # upload sample -----------------------------------------------------------
      
      else if(vname == "Sample"){
        
        assign(vname, read_xlsx(input$fileinfo$datapath, sheet = "Sample", 
                                col_types = c("text","text","numeric","numeric","text"),
                                na = "NA"))
        
        Sample <- Sample %>% 
          mutate(Sample, RefID = ref_temp$RefID, 
                 Sample_seq = sprintf("%03d",c(1:length(Sample))))
        
        
        Sample <- Sample[,c(6,7,1:5)]
        names(Sample) <- c("RefID", "Sample_seq", "Sample", "Material", "Longitude", "Latitude", "Taxon")
        
        temp_spl <- Sample
        
        output$splUpload <- renderDataTable(
          
          temp_spl <- temp_spl %>% 
            filter(RefID == refID_temp$RefID),
          
          options = list(
            columnDefs = list(list(visible = F, targets = c(1,2))))
          
        )
        
      }
      
      
      # upload age --------------------------------------------------------------
      
      else if(vname == "Age"){
        
        assign(vname, read_xlsx(input$fileinfo$datapath, sheet = "Age",
                                col_types = c("text", "text", "numeric","numeric","numeric", "text","text","text"),
                                na = "NA"))
        
        Age$Method_cd <- sprintf("%03s", Age$Method_cd)
        
        if(nrow(Sample) == nrow(Age)){
          
          Age <- data.frame(Sample$RefID, Sample$Sample_seq, Age)
          
        }else if(nrow(Sample) != nrow(Age)){
          spl_temp <- data.frame(Sample$RefID, Sample$Sample_seq, Sample$Sample)
          names(spl_temp) <- c("RefID", "Sample_seq", "Sample")
          
          Age <- merge(spl_temp, Age, key = "Sample")
          
          Age <- Age[,c(2:4,1,5:10)]
          
        }
        
        
        names(Age) <- c("RefID", "Sample_seq", "Method_cd", "Sample", "Age", "Age_min", "Age_max", "Era", "Period", "Epoch")
        
        temp_age <- Age
        
        output$ageUpload <- renderDataTable(
          
          temp_age <- temp_age %>% 
            filter(RefID == refID_temp$RefID),
          
          options = list(
            columnDefs = list(list(visible = F, targets = c(1,2))))
          
        )
      }
    }
    
    
    
    observeEvent(input$info_temp,{
      
      file.copy(input$fileinfo$datapath, paste0("RData/UserFile/", refID_temp$RefID, "_info.xlsx"), overwrite = T)
      save(temp_ref, file = "RData/Temp/temp_ref.RData")
      save(temp_spl, file = "RData/Temp/temp_spl.RData")
      save(temp_age, file = "RData/Temp/temp_age.RData")
      
      hideTab(inputId = "tabs", target = "Step 2")
      showTab(inputId = "tabs", target = "Step 3")
      
      
    })
    
  })
  
  
  observeEvent(input$info_reset,{
    
    refresh()
    
  })
  
  
  # Step 3 ------------------------------------------------------------------
  
  
  # value file upload, ID assignment, Save Temp RData -------------------------------------------------------
  
  observeEvent(input$filevalue,{
    
    value_num <- data.frame()
    
    value_num <- data.frame(length(input$filevalue$datapath))
    names(value_num) <- c("N")
    
    save(value_num, file = "RData/Temp/value_num.RData")
    
    load("RData/Temp/temp_ref.RData")
    load("RData/Temp/temp_spl.RData")
    load("RData/Temp/temp_age.RData")
    
  
    eventLog <- data.frame()
    
    num <- as.numeric(value_num$N)
    
    if(num != nrow(temp_age)){
      
      shinyalert("Oops!", "Check Your Value files number.", type = "error")
      return()
      
    }
    
    
    U_Pb <- read_xlsx("RData/template/manual/U-Pb.xlsx")
    Sm_Nd <- read_xlsx("RData/template/manual/Sm-Nd.xlsx")
    Osl <- read_xlsx("RData/template/manual/OSL.xlsx")
    Lu_Hf <- read_xlsx("RData/template/manual/Lu-Hf.xlsx")
    K_Ar <- read_xlsx("RData/template/manual/K-Ar.xlsx")
    Cos <- read_xlsx("RData/template/manual/Cosmogenic.xlsx")
    Ar <- read_xlsx("RData/template/manual/Ar-Ar.xlsx")
    
    
    for(i in 1:length(input$filevalue$datapath)){
      
      temp_val <- read_xlsx(input$filevalue$datapath[i],
                            na = "NA")
      
      if(all(colnames(temp_val) %in% colnames(Ar)) == T){
        
        metcd <- sprintf("%03d", 1)
        
      }else if(all(colnames(temp_val) %in% colnames(Cos)) == T){
        
        metcd <- sprintf("%03d", 2)
        
      }else if(all(colnames(temp_val) %in% colnames(K_Ar)) == T){
        
        metcd <- sprintf("%03d", 3)
        
      }else if(all(colnames(temp_val) %in% colnames(Lu_Hf)) == T){
        
        metcd <- sprintf("%03d", 4)
        
      }else if(all(colnames(temp_val) %in% colnames(Osl)) == T){
        
        metcd <- sprintf("%03d", 5)
        
      }else if(all(colnames(temp_val) %in% colnames(Sm_Nd)) == T){
        
        metcd <- sprintf("%03d", 6)
        
      }else if(all(colnames(temp_val) %in% colnames(U_Pb)) == T){
        
        metcd <- sprintf("%03d", 7)
        
      }
      
      
      if(nrow(temp_age) == nrow(temp_spl)){
        
        for(j in 1:nrow(temp_age)){
          
          value_spl <- unique(temp_val$Sample)
          
          if(temp_age$Sample[j] == value_spl){
            
            ID <- paste0(temp_age$RefID[j], temp_age$Sample_seq[j], metcd)
            
            new_val_temp <- cbind(ID, temp_val)
            
            assign(paste0("temp_value", j), new_val_temp)
            
            get(paste0("temp_value",j)) %>% 
              saveRDS(file = paste0("RData/Temp/", "temp_value", j, ".RData"))
            
          }
          
        }
        
      }
      
      else if(nrow(temp_age) > nrow(temp_spl)){
        
        for(j in 1:nrow(temp_age)){
          
          each_age_spl <- temp_age$Sample[j]
          each_age_metcd <- temp_age$Method_cd[j]
          
          if(metcd == each_age_metcd & unique(temp_val$Sample) == each_age_spl){
            
            ID <- paste0(temp_age$RefID[j], temp_age$Sample_seq[j], metcd)
            
            new_val_temp <- cbind(ID, temp_val)
            
            assign(paste0("temp_value", j), new_val_temp)
            
            get(paste0("temp_value",j)) %>% 
              saveRDS(file = paste0("RData/Temp/", "temp_value", j, ".RData"))
            
          }
          
        }
      }
      
      
      temp_path <- input$filevalue$datapath[i]
      
      eventLog_temp <- cbind(ID, temp_path)
      
      eventLog <- rbind(eventLog, eventLog_temp)
      
      save(eventLog, file = "RData/Temp/eventLog.RData")
      
      
      
      # value option(fc,  fn) ---------------------------------------------------
      
      assign(paste0("temp_value",i), 
             readRDS(file = paste0("RData/Temp/", "temp_value", i, ".RData")))
      
      assign(paste0("ID_value",i), mget(ls(pattern = paste0("temp_value",i))))
      
      
      # value format 001 --------------------------------------------------------
      
      if(metcd == "001"){
        
        mkdf2 <- data.frame()
        
        for(m in 1:NROW(get(paste0("ID_value", i))[[1]])){
          
          n = 1
          
          repeat{
            
            if(is.na(get(paste0("ID_value", i))[[1]][[n]][m]) == T){
              
              assign(paste0("ID_value",i,n,m), NA)
              
            }else if(!is.na(get(paste0("ID_value", i))[[1]][[n]][m])){
              
              if(n %in% c(1,2,6)){
                
                fc <- function(i,x,y){as.character(get(paste0("ID_value",i))[[1]][[x]][y])}
                
                assign(paste0("ID_value",i,n,m), fc(i,n,m))
                
              }else if(n %in% c(3:5,7:9)){
                
                fn <- function(i,x,y){
                  
                  numeric_list <- get(paste0("ID_value",i))[[1]][[x]][y]
                  
                  as.numeric(numeric_list)
                  
                }
                
                assign(paste0("ID_value",i,n,m), fn(i,n,m))
                
              }
              
            }
            n = n + 1
            if(n > length(get(paste0("ID_value",  i))[[1]]))break
          }
          
          
          mkdf <- data.frame()
          
          l = 1
          
          while(l < n){
            
            temp_temp <- paste0("ID_value",i,l,m)
            
            mkdf_temp <- data.frame(get(temp_temp))
            
            if(nrow(mkdf) == 0){
              
              mkdf <- mkdf_temp
              
            }else if(nrow(mkdf) != 0){
              
              mkdf <- cbind(mkdf, mkdf_temp)
              
            }
            l = l + 1
          }
          
          mkdf2_temp <- mkdf
          mkdf2 <- rbind(mkdf2, mkdf2_temp)
          
        }
        
        assign(paste0("temp_value", i), mkdf2)
        
        newnames <- c("ID", "Sample", "Plateau.Age", "Plateau.Age.SD", "Plateau.Per", "Steps", "MSWD", "Intergrated.Age", "Intergrated.Age.SD")
        
        setnames(get(paste0("temp_value", i)), newnames)
        
      }
      
      
      
      
      
      # value format 002 --------------------------------------------------------
      
      if(metcd == "002"){
        
        mkdf2 <- data.frame()
        
        for(m in 1:NROW(get(paste0("ID_value", i))[[1]])){
          
          n = 1
          
          repeat{
            
            if(is.na(get(paste0("ID_value", i))[[1]][[n]][m]) == T){
              
              assign(paste0("ID_value",i,n,m), NA)
              
            }else if(!is.na(get(paste0("ID_value", i))[[1]][[n]][m])){
              
              if(n %in% c(1,2,5,7)){
                
                fc <- function(i,x,y){as.character(get(paste0("ID_value",i))[[1]][[x]][y])}
                
                assign(paste0("ID_value",i,n,m), fc(i,n,m))
                
              }else if(n %in% c(3,4,6,8:11)){
                
                fn <- function(i,x,y){
                  
                  numeric_list <- get(paste0("ID_value",i))[[1]][[x]][y]
                  
                  as.numeric(numeric_list)
                  
                }
                
                assign(paste0("ID_value",i,n,m), fn(i,n,m))
                
              }
              
            }
            n = n + 1
            if(n > length(get(paste0("ID_value",  i))[[1]]))break
          }
          
          
          mkdf <- data.frame()
          
          l = 1
          
          while(l < n){
            
            temp_temp <- paste0("ID_value",i,l,m)
            
            mkdf_temp <- data.frame(get(temp_temp))
            
            if(nrow(mkdf) == 0){
              
              mkdf <- mkdf_temp
              
            }else if(nrow(mkdf) != 0){
              
              mkdf <- cbind(mkdf, mkdf_temp)
              
            }
            l = l + 1
          }
          
          mkdf2_temp <- mkdf
          mkdf2 <- rbind(mkdf2, mkdf2_temp)
          
        }
        
        assign(paste0("temp_value", i), mkdf2)
        
        newnames <- c("ID", "Sample", "Concentration.Be10", "Concentration.Be10.PM", "Concentration.Be10.Unit", "Be.Carrier",
                      "Be.Carrier.Unit", "Be10_Be9", "Be10_Be9.PM", "Age", "Age.PM")
        
        setnames(get(paste0("temp_value", i)), newnames)
        
      }
      
      
      
      
      
      # value format 003 --------------------------------------------------------
      
      if(metcd == "003"){
        
        mkdf2 <- data.frame()
        
        for(m in 1:NROW(get(paste0("ID_value", i))[[1]])){
          
          n = 1
          
          repeat{
            
            if(is.na(get(paste0("ID_value", i))[[1]][[n]][m]) == T){
              
              assign(paste0("ID_value",i,n,m), NA)
              
            }else if(!is.na(get(paste0("ID_value", i))[[1]][[n]][m])){
              
              if(n %in% c(1,2,5,7,9,11,13,15,19,21)){
                
                fc <- function(i,x,y){as.character(get(paste0("ID_value",i))[[1]][[x]][y])}
                
                assign(paste0("ID_value",i,n,m), fc(i,n,m))
                
              }else if(n %in% c(3,4,6,8,10,12,14,16:18,20)){
                
                fn <- function(i,x,y){
                  
                  numeric_list <- get(paste0("ID_value",i))[[1]][[x]][y]
                  
                  as.numeric(numeric_list)
                  
                }
                
                assign(paste0("ID_value",i,n,m), fn(i,n,m))
                
              }
              
            }
            n = n + 1
            if(n > length(get(paste0("ID_value",  i))[[1]]))break
          }
          
          
          mkdf <- data.frame()
          
          l = 1
          
          while(l < n){
            
            temp_temp <- paste0("ID_value",i,l,m)
            
            mkdf_temp <- data.frame(get(temp_temp))
            
            if(nrow(mkdf) == 0){
              
              mkdf <- mkdf_temp
              
            }else if(nrow(mkdf) != 0){
              
              mkdf <- cbind(mkdf, mkdf_temp)
              
            }
            l = l + 1
          }
          
          mkdf2_temp <- mkdf
          mkdf2 <- rbind(mkdf2, mkdf2_temp)
          
        }
        
        assign(paste0("temp_value", i), mkdf2)
        
        newnames <- c("ID", "Sample", "K", "K.PM", "K.Unit", "Sample.Weight",
                      "Sample.Weight.Unit", "Radiogenic.Ar40", "Radiogenic.Ar40.Unit", "Delta.Radiogenic.Ar40", "Delta.Radiogenic.Ar40.Unit",
                      "Radiogenic.Ar36", "Radiogenic.Ar36.Unit", "Delta.Radiogenic.Ar36", "Delta.Radiogenic.Ar36.Unit", "Age", "Age.PM",
                      "Airfraction", "Airfraction.Unit", "Non.Radiogenic.Ar40", "Non.Radiogenic.Ar40.Unit")
        
        setnames(get(paste0("temp_value", i)), newnames)
        
      }
      
      
      
      
      
      # value format 004 --------------------------------------------------------
      
      if(metcd == "004"){
        
        mkdf2 <- data.frame()
        
        for(m in 1:NROW(get(paste0("ID_value", i))[[1]])){
          
          n = 1
          
          repeat{
            
            if(is.na(get(paste0("ID_value", i))[[1]][[n]][m]) == T){
              
              assign(paste0("ID_value",i,n,m), NA)
              
            }else if(!is.na(get(paste0("ID_value", i))[[1]][[n]][m])){
              
              if(n %in% c(1:3)){
                
                fc <- function(i,x,y){as.character(get(paste0("ID_value",i))[[1]][[x]][y])}
                
                assign(paste0("ID_value",i,n,m), fc(i,n,m))
                
              }else if(n %in% c(4:19)){
                
                fn <- function(i,x,y){
                  
                  numeric_list <- get(paste0("ID_value",i))[[1]][[x]][y]
                  
                  as.numeric(numeric_list)
                  
                }
                
                assign(paste0("ID_value",i,n,m), fn(i,n,m))
                
              }
              
            }
            n = n + 1
            if(n > length(get(paste0("ID_value",  i))[[1]]))break
          }
          
          
          mkdf <- data.frame()
          
          l = 1
          
          while(l < n){
            
            temp_temp <- paste0("ID_value",i,l,m)
            
            mkdf_temp <- data.frame(get(temp_temp))
            
            if(nrow(mkdf) == 0){
              
              mkdf <- mkdf_temp
              
            }else if(nrow(mkdf) != 0){
              
              mkdf <- cbind(mkdf, mkdf_temp)
              
            }
            l = l + 1
          }
          
          mkdf2_temp <- mkdf
          mkdf2 <- rbind(mkdf2, mkdf2_temp)
          
        }
        
        assign(paste0("temp_value", i), mkdf2)
        
        newnames <- c("ID", "Sample", "RunID", "Age", "Yb176_Hf177", "Yb176_Hf177.2SD",
                      "Lu176_Hf177", "Lu176_Hf177.2SD", "Hf176_Hf177", "Hf176_Hf177.2SD", "Hf176_Hf177.2SE",
                      "Hf176_Hf177_i", "Hf176_Hf177_t", "E_Hf_0", "E_Hf_t", "TDM", "TDMc",
                      "TDM2", "f_Lu_Hf")
        
        setnames(get(paste0("temp_value", i)), newnames)
        
      }
      
      
      
      
      
      # value format 005 --------------------------------------------------------
      
      if(metcd == "005"){
        
        mkdf2 <- data.frame()
        
        for(m in 1:NROW(get(paste0("ID_value", i))[[1]])){
          
          n = 1
          
          repeat{
            
            if(is.na(get(paste0("ID_value", i))[[1]][[n]][m]) == T){
              
              assign(paste0("ID_value",i,n,m), NA)
              
            }else if(!is.na(get(paste0("ID_value", i))[[1]][[n]][m])){
              
              if(n %in% c(1,2,4,6,9,12)){
                
                fc <- function(i,x,y){as.character(get(paste0("ID_value",i))[[1]][[x]][y])}
                
                assign(paste0("ID_value",i,n,m), fc(i,n,m))
                
              }else if(n %in% c(3,5,7,8,10,11,13:16)){
                
                fn <- function(i,x,y){
                  
                  numeric_list <- get(paste0("ID_value",i))[[1]][[x]][y]
                  
                  as.numeric(numeric_list)
                  
                }
                
                assign(paste0("ID_value",i,n,m), fn(i,n,m))
                
              }
              
            }
            n = n + 1
            if(n > length(get(paste0("ID_value",  i))[[1]]))break
          }
          
          
          mkdf <- data.frame()
          
          l = 1
          
          while(l < n){
            
            temp_temp <- paste0("ID_value",i,l,m)
            
            mkdf_temp <- data.frame(get(temp_temp))
            
            if(nrow(mkdf) == 0){
              
              mkdf <- mkdf_temp
              
            }else if(nrow(mkdf) != 0){
              
              mkdf <- cbind(mkdf, mkdf_temp)
              
            }
            l = l + 1
          }
          
          mkdf2_temp <- mkdf
          mkdf2 <- rbind(mkdf2, mkdf2_temp)
          
        }
        
        assign(paste0("temp_value", i), mkdf2)
        
        newnames <- c("ID", "Sample", "Depth", "Depth.Unit", "Water.Content", "Water.Content.Unit",
                      "Dose.Rate", "Dose.Rate.PM", "Dose.Rate.Unit", "Equivalent.Dose", "Equivalent.Dose.PM",
                      "Equivalent.Dose.Unit", "Age", "Age.PM", "Age.SE", "Age.SD")
        
        setnames(get(paste0("temp_value", i)), newnames)
        
      }
      
      
      
      
      
      # value format 006 --------------------------------------------------------
      
      if(metcd == "006"){
        
        mkdf2 <- data.frame()
        
        for(m in 1:NROW(get(paste0("ID_value", i))[[1]])){
          
          n = 1
          
          repeat{
            
            if(is.na(get(paste0("ID_value", i))[[1]][[n]][m]) == T){
              
              assign(paste0("ID_value",i,n,m), NA)
              
            }else if(!is.na(get(paste0("ID_value", i))[[1]][[n]][m])){
              
              if(n %in% c(1:3,5,7)){
                
                fc <- function(i,x,y){as.character(get(paste0("ID_value",i))[[1]][[x]][y])}
                
                assign(paste0("ID_value",i,n,m), fc(i,n,m))
                
              }else if(n %in% c(4,6,8:14)){
                
                fn <- function(i,x,y){
                  
                  numeric_list <- get(paste0("ID_value",i))[[1]][[x]][y]
                  
                  as.numeric(numeric_list)
                  
                }
                
                assign(paste0("ID_value",i,n,m), fn(i,n,m))
                
              }
              
            }
            n = n + 1
            if(n > length(get(paste0("ID_value",  i))[[1]]))break
          }
          
          
          mkdf <- data.frame()
          
          l = 1
          
          while(l < n){
            
            temp_temp <- paste0("ID_value",i,l,m)
            
            mkdf_temp <- data.frame(get(temp_temp))
            
            if(nrow(mkdf) == 0){
              
              mkdf <- mkdf_temp
              
            }else if(nrow(mkdf) != 0){
              
              mkdf <- cbind(mkdf, mkdf_temp)
              
            }
            l = l + 1
          }
          
          mkdf2_temp <- mkdf
          mkdf2 <- rbind(mkdf2, mkdf2_temp)
          
        }
        
        assign(paste0("temp_value", i), mkdf2)
        
        newnames <- c("ID", "Sample", "RunID", "Sm", "Sm.Unit", "Nd",
                      "Nd.Unit", "Sm147_Nd144", "Nd143_Nd144", "Nd143_Nd144.2SD", "E_Nd_0",
                      "E_Nd_t", "TDM", "TDM2")
        
        setnames(get(paste0("temp_value", i)), newnames)
        
      }
      
      
      
      
      # value format 007 --------------------------------------------------------
      
      if(metcd == "007"){
        
        mkdf2 <- data.frame()
        
        for(m in 1:NROW(get(paste0("ID_value", i))[[1]])){
          
          n = 1
          
          repeat{
            
            if(is.na(get(paste0("ID_value", i))[[1]][[n]][m]) == T){
              
              assign(paste0("ID_value",i,n,m), NA)
              
            }else if(!is.na(get(paste0("ID_value", i))[[1]][[n]][m])){
              
              if(n %in% c(1:3,5,8,10,12,14,16,20,95)){
                
                fc <- function(i,x,y){as.character(get(paste0("ID_value",i))[[1]][[x]][y])}
                
                assign(paste0("ID_value",i,n,m), fc(i,n,m))
                
              }else if(n %in% c(4,6,7,9,11,13,15,17:19,21:94)){
                
                fn <- function(i,x,y){
                  
                  numeric_list <- get(paste0("ID_value",i))[[1]][[x]][y]
                  
                  as.numeric(numeric_list)
                  
                }
                
                assign(paste0("ID_value",i,n,m), fn(i,n,m))
                
              }
              
            }
            n = n + 1
            if(n > length(get(paste0("ID_value",  i))[[1]]))break
          }
          
          
          mkdf <- data.frame()
          
          l = 1
          
          while(l < n){
            
            temp_temp <- paste0("ID_value",i,l,m)
            
            mkdf_temp <- data.frame(get(temp_temp))
            
            if(nrow(mkdf) == 0){
              
              mkdf <- mkdf_temp
              
            }else if(nrow(mkdf) != 0){
              
              mkdf <- cbind(mkdf, mkdf_temp)
              
            }
            l = l + 1
          }
          
          mkdf2_temp <- mkdf
          mkdf2 <- rbind(mkdf2, mkdf2_temp)
          
        }
        
        assign(paste0("temp_value", i), mkdf2)
        
        newnames <- c('ID','Sample','RunID','Pb','Pb.Unit','Pb206','Pb206.PM.Per','Pb206.Unit',
                      'Pbc','Pbc.Unit','Pbr','Pbr.Unit','U','U.Unit','Th','Th.Unit','Th_U','Th_U.Per','Th_U.PM.Per',
                      'Th_U.Unit','Pb204_Pb206','Pb204_Pb206.PM','Pb204_Pb206.PM.Per','Pb206_Th232','Pb206_Th232.SD',
                      'Pb206_U238','Pb206_U238.PM','Pb206_U238.PM.Per','Pb206_U238.SD','Pb206_U238.2SD','Pb206_U238.2SE',
                      'U238_Pb206','U238_Pb206.Per','U238_Pb206.PM','U238_Pb206.PM.Per','U238_Pb206.SD','Pbr206_U238','Pbr206_U238.PM',
                      'Pbr206_U238.PM.Per','U238_Pbr206','U238_Pbr206.PM','U238_Pbr206.PM.Per','Pb207_Pb206','Pb207_Pb206.Per','Pb207_Pb206.PM',
                      'Pb207_Pb206.PM.Per','Pb207_Pb206.SD','Pb207_Pb206.2SD','Pb207_Pb206.2SE','Pbr207_Pb206','Pbr207_Pb206.PM','Pbr207_Pb206.PM.Per',
                      'Pbr207_Pbr206','Pbr207_Pbr206.PM.Per','Pb207_U235','Pb207_U235.PM.Per','Pb207_U235.SD','Pb207_U235.2SD','Pb207_U235.2SE','Pbr207_U235',
                      'Pbr207_U235.PM','Pbr207_U235.PM.Per','Pb208_Pb206','Pb208_Pb206.PM.Per','Pb206_U238.Age','Pb206_U238.Age.PM','Pb206_U238.Age.PM.Per',
                      'Pb206_U238.Age.SD','Pb206_U238.Age.2SD','Pb206_U238.Age.2SE','Pbr206_U238.Age','Pbr206_U238.Age.PM','Pbr206_U238.Age.PM.Per','U238_Pb206.Age',
                      'U238_Pb206.Age.PM.Per','Pb207_Pb206.Age','Pb207_Pb206.Age.PM','Pb207_Pb206.Age.PM.Per','Pb207_Pb206.Age.SD','Pb207_Pb206.Age.2SD','Pb207_Pb206.Age.2SE',
                      'Pbr207_Pbr206.Age','Pbr207_Pbr206.Age.PM','Pbr207_Pbr206.Age.PM.Per','Pb207_U235.Age','Pb207_U235.Age.PM','Pb207_U235.Age.SD','Pb207_U235.Age.2SD',
                      'Pb207_U235.Age.2SE','Pb208_Th232.Age','Pb208_Th232.Age.PM','Age','Age.PM','Disc','Disc.Unit')
        
        setnames(get(paste0("temp_value", i)), newnames)
        
      }
      
      
      empty_col <- colSums(is.na(get(paste0("temp_value",i))) | get(paste0("temp_value",i)) == "") == nrow(get(paste0("temp_value",i)))
      
      assign(paste0("temp_value",i), get(paste0("temp_value",i))[,!empty_col])
      
      get(paste0("temp_value",i)) %>% 
        saveRDS(file = paste0("RData/Temp/", "temp_value", i, ".RData"))
    }
    
    value_tbl <- reactive({
      
      load("RData/Temp/value_num.RData")
      
      num <- length(input$filevalue$datapath)
      temp_num <- data.frame(num)
      names(temp_num) <- c("N")
      
      save(temp_num, file = "RData/Temp/value_num.RData")
      
      for(x in 1:num){
        
        assign(paste0("temp_value",x), readRDS(paste0("RData/Temp/temp_value",  x, ".RData")))  
        
        
      }
      
      tbl <- mget(ls(pattern = "temp_value"))
      
      
    })
    
    
    lapply(names(value_tbl()), function(x){
      
      
      output[[x]] = renderDataTable({value_tbl()[[x]]}, 
                                    
                                    options = list(
                                      columnDefs = list(list(visible = F, targets = c(1))))
                                    
      )
      
    })
    
    # view value tbl ----------------------------------------------------------
    
    output$valueUpload <- renderUI({
      
      lapply(names(value_tbl()), dataTableOutput)
      
    })
    
  })
  
  # Confirm and save org excel file  -------------------------------------------
  
  observeEvent(input$value_temp, {
    
    load("RData/Temp/eventLog.RData")
    
    for(i in 1:nrow(eventLog)){
      
      file.copy(eventLog$temp_path[i],
                paste0("RData/UserFile/", eventLog$ID[i], "_value.xlsx"))
      
    }
    
    hideTab(inputId = "tabs", target = "Step 3")
    showTab(inputId = "tabs", target = "Step 4")
    
  })
  
  observeEvent(input$value_reset, {
    refresh()
  })
  
  
  
  # Step 4 ------------------------------------------------------------------
  
  
  observeEvent(input$value_temp, {
    
    load("RData/Temp/temp_ref.RData")
    load("RData/Temp/temp_spl.RData")
    load("RData/Temp/temp_age.RData")
    
    load("RData/Temp/value_num.RData")
    
    output$AllrefUpload <- renderDataTable(
      
      temp_ref,
      
      options = list(
        columnDefs = list(list(visible = F, targets = c(1))))
      
    )
    
    output$AllsplUpload <- renderDataTable(
      
      temp_spl,
      
      options = list(
        columnDefs = list(list(visible = F, targets = c(1,2))))
      
    )
    
    output$AllageUpload <- renderDataTable(
      
      temp_age,
      
      options = list(
        columnDefs = list(list(visible = F, targets = c(1,2))))
    )
    
    value_all_tbl <- reactive({
      
      num <- as.numeric(temp_num$N) 
      
      for(i in 1:num){
        
        assign(paste0("temp_value",i),
               readRDS(paste0("RData/Temp/temp_value",i,".RData")))
      }
      
      tbl_all <- mget(ls(pattern = "temp_value"))
      
    })
    
    output$AllvalueUpload <- renderUI({
      
      tableList <- imap(value_all_tbl(), ~ {
        tagList(
          
          DTOutput(outputId = paste0("table_", .y))
          
        )
        
      })
      
      tagList(tableList)
      
    })
    
    iwalk(value_all_tbl(), ~{
      
      output_name <- paste0("table_", .y)
      output[[output_name]] <- renderDataTable(.x,
                                               options = list(
                                                 columnDefs = list(list(visible = F, targets = c(1)))))
      
    })
    
  })
  
  
  # All Submit --------------------------------------------------------------
  
  observeEvent(input$submit_all,{
    
    updateActionButton(session, "submit_all", label = "Done")
    
  })
  
  observeEvent(input$submit_all, {
    
    load("RData/Temp/temp_ref.RData")
    load("RData/Temp/temp_spl.RData")
    load("RData/Temp/temp_age.RData")
    
    if(file.exists("RData/UserRData/ref_custom.RData")){
      load("RData/UserRData/ref_custom.RData")
      load("RData/UserRData/spl_custom.RData")
      load("RData/UserRData/age_custom.RData")
      
      if(nrow(ref_custom) == 0){
        ref_custom <- temp_ref
      }else if(nrow(ref_custom) != 0){
        ref_custom <- rbind(ref_custom, temp_ref)
      }
      
      if(nrow(spl_custom) == 0){
        spl_custom <- temp_spl
      }else if(nrow(spl_custom) != 0){
        spl_custom <- rbind(spl_custom, temp_spl)
      }
      
      if(nrow(age_custom) == 0){
        age_custom <- temp_age
      }else if(nrow(age_custom) != 0){
        age_custom <- rbind(age_custom, temp_age)
      }
      
    }else{
      ref_custom <- temp_ref
      spl_custom <- temp_spl
      age_custom <- temp_age
    }
    
    # submit information ------------------------------------------------------
    
    save(ref_custom, file = "RData/UserRData/ref_custom.RData")
    save(spl_custom, file = "RData/UserRData/spl_custom.RData")
    save(age_custom, file = "RData/UserRData/age_custom.RData")
    
    # submit value ------------------------------------------------------------
    
    load("RData/Temp/value_num.RData")
    
    num <- as.numeric(temp_num$N) 
    
    for(i in 1:num){
      
      assign(paste0("temp_value",i),
             readRDS(paste0("RData/Temp/temp_value", i, ".RData")))
      
      assign("ID", unique(paste0(get(paste0("temp_value",i))$ID, ".xlsx")))
      
      assign("met_num", substr(as.character(ID), 13, 15))
      
      assign("ID", mget(ls(pattern = paste0("temp_value",i))))
      
      names(ID)[[1]] <- c(unique(paste0(get(paste0("temp_value",i))$ID, ".xlsx")))
      
      if(met_num == "001"){
        
        if(file.exists("RData/UserRData/value_ar_custom.RData")){
          load("RData/UserRData/value_ar_custom.RData")
          if(length(value_ar_custom) == 0){
            value_ar_custom <- ID
          }else if(length(value_ar_custom) != 0){
            value_ar_custom <- c(value_ar_custom, ID)
          }
        }else{
          value_ar_custom <- ID
        }
        save(value_ar_custom, file= "RData/UserRData/value_ar_custom.RData")
      }
      
      if(met_num == "002"){
        
        if(file.exists("RData/UserRData/value_cosmo_custom.RData")){
          load("RData/UserRData/value_cosmo_custom.RData")
          if(length(value_cosmo_custom) == 0){
            value_cosmo_custom <- ID
          }else if(length(value_cosmo_custom) != 0){
            value_cosmo_custom <- c(value_cosmo_custom, ID)
          }
        }else{
          value_cosmo_custom <- ID
        }
        
        save(value_cosmo_custom, file= "RData/UserRData/value_cosmo_custom.RData")
        
      }
      
      if(met_num == "003"){
        
        if(file.exists("RData/UserRData/value_kar_custom.RData")){
          load("RData/UserRData/value_kar_custom.RData")
          if(length(value_kar_custom) == 0){
            value_kar_custom <- ID
          }else if(length(value_kar_custom) != 0){
            value_kar_custom <- c(value_kar_custom, ID)
          }
        }else{
          value_kar_custom <- ID
        }
        save(value_kar_custom, file= "RData/UserRData/value_kar_custom.RData")
      }
      
      if(met_num == "004"){
        
        if(file.exists("RData/UserRData/value_luhf_custom.RData")){
          load("RData/UserRData/value_luhf_custom.RData")
          if(length(value_luhf_custom) == 0){
            value_luhf_custom <- ID
          }else if(length(value_luhf_custom) != 0){
            value_luhf_custom <- c(value_luhf_custom, ID)
          }
        }else{
          value_luhf_custom <- ID
        }
        save(value_luhf_custom, file= "RData/UserRData/value_luhf_custom.RData")
      }
      
      if(met_num == "005"){
        if(file.exists("RData/UserRData/value_osl_custom.RData")){
          load("RData/UserRData/value_osl_custom.RData")
          if(length(value_osl_custom) == 0){
            value_osl_custom <- ID
          }else if(length(value_osl_custom) != 0){
            value_osl_custom <- c(value_osl_custom, ID)
          }
        }else{
          value_osl_custom <- ID
        }
        save(value_osl_custom, file= "RData/UserRData/value_osl_custom.RData")
      }
      
      if(met_num == "006"){
        
        if(file.exists("RData/UserRData/value_smnd_custom.RData")){
          load("RData/UserRData/value_smnd_custom.RData")
          if(length(value_smnd_custom) == 0){
            value_smnd_custom <- ID
          }else if(length(value_smnd_custom) != 0){
            value_smnd_custom <- c(value_smnd_custom, ID)
          }
        }else{
          value_smnd_custom <- ID
        }
        save(value_smnd_custom, file= "RData/UserRData/value_smnd_custom.RData")
      }
      
      if(met_num == "007"){
        
        if(file.exists("RData/UserRData/value_upb_custom.RData")){
          load("RData/UserRData/value_upb_custom.RData")
          if(length(value_upb_custom) == 0){
            value_upb_custom <- ID
          }else if(length(value_upb_custom) != 0){
            value_upb_custom <- c(value_upb_custom, ID)
          }
        }else{
          value_upb_custom <- ID
        }
        
        save(value_upb_custom, file= "RData/UserRData/value_upb_custom.RData")
        
      }
    }
    
    shinyalert(inputId = "fn_alr", "Success!", 
               "Return to Home", type = "warning",
               confirmButtonText = "Okay",

               callbackR = function(x) { if(x == T){
                 
                 refresh()
 
               }})
               
    
  })
  
}