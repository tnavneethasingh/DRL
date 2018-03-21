NSEMarketWatch<-function()
{
  tryCatch(
    {
      starttime<-Sys.time()
      library(RSelenium)
      library(taskscheduleR)
      library(RODBC)
      library(audio)
      
      filename<-paste("data",".csv",sep="")
      path<-"C://Users//navneetha//Downloads/"
      if(file.exists(file.path(path,filename)))
      {
        file.remove(file.path(path,filename))
      }  
      
      
      homeurl<-"https://www.nseindia.com/index_nse.htm"
      nserD <- rsDriver(port=4446L)
      nseremDr <- nserD[["client"]]
      nseremDr$navigate(homeurl)
      
      nseMarketOCStatus<- nseremDr$findElement(using="id", value="status1")
      nsestatus<-strsplit(as.character(nseMarketOCStatus$getElementText())," ")[[1]]
      #if('Closed.' %in% nsestatus || 'closing' %in% nsestatus)
      if('Open' %in% nsestatus) 
      {
        ndatet<-Sys.time()
        time <- format(ndatet , "%H:%M")
        
        print(paste("Task Started at :",ndatet,sep=""))
        
        #print(paste("Task statrted time is :",time,sep=""))
        ########## Button click and download data ###
        url<-"https://www.nseindia.com/live_market/dynaContent/live_watch/equities_stock_watch.htm"
        nseremDr$navigate(url)
        nseremDr$setImplicitWaitTimeout(milliseconds = 600000)
        searchNFTYOP<-nseremDr$findElement(using = 'xpath', "//select[@name='bankNiftySelect']")
        opts <- searchNFTYOP$selectTag()
        
        ### download marketatch file ##
        DT<- nseremDr$findElement(using="id", value="time")
        #nseremDr$refresh()
        DateTime<-DT$getElementText()
        DateTime<-as.POSIXlt(trimws(gsub(",", " ", strsplit(as.character(DateTime)," on")[[1]][2], fixed = TRUE)),format="%b %d %Y %H:%M")
        
        
        for(i in 1:3)
        {
          print(sprintf(paste(" Stock Fetch Group ",i)))
          #nseremDr$refresh()
          optvalue<-opts$value[i]
          
          # change audi to whatever your option value is
          option <- nseremDr$findElement(using = 'xpath',paste("//*/option[@value = '",optvalue,"']",sep=""))
          option$clickElement()
          
          searchSymbols<-nseremDr$findElement(using="class name", value="download-data-link1")
          successHitOrError<-try(searchSymbols$clickElement())
          
          wait(2)
          if(file.exists(file.path(path,filename)))
          {
            
            
            print("file exisits")
            NSEdata<-read.csv(file.path(path,filename))
            names(NSEdata)<-c("Symbol","Open","High","Low","LTP","Change","PercentChange","TradedVolumeInlacs","TradedValueInCrs","High_52Week","Low_52Week","PercentChange_365Days","PercentChange_30Days")
            
            NSEdata$DateTime<-DateTime
            NSEdata$View<-NSEdata[1,c("Symbol")]
            NSEdata<-NSEdata[c("DateTime","View","Symbol","Open","High","Low","LTP","Change","PercentChange","TradedVolumeInlacs",
                               "TradedValueInCrs","High_52Week","Low_52Week","PercentChange_365Days","PercentChange_30Days")]
            
            NSEdata<-NSEdata[-1,]
            NSEdata$Open<-as.numeric(gsub(",","",NSEdata$Open))
            NSEdata$High<-as.numeric(gsub(",","",NSEdata$High))
            NSEdata$Low<-as.numeric(gsub(",","",NSEdata$Low))
            NSEdata$LTP<-as.numeric(gsub(",","",NSEdata$LTP))
            NSEdata$Change<-as.numeric(gsub(",","",NSEdata$Change))
            NSEdata$TradedVolumeInlacs<-as.numeric(gsub(",","",NSEdata$TradedVolumeInlacs))
            NSEdata$TradedValueInCrs<-as.numeric(gsub(",","",NSEdata$TradedValueInCrs))
            NSEdata$High_52Week<-as.numeric(gsub(",","",NSEdata$High_52Week))
            NSEdata$Low_52Week<-as.numeric(gsub(",","",NSEdata$Low_52Week))
            NSEdata$PercentChange_365Days<-as.numeric(NSEdata$PercentChange_365Days)
            NSEdata$PercentChange_30Days<-as.numeric(NSEdata$PercentChange_30Days)
            
            print(head(NSEdata))
            
            ## odbc connection to sql server
            # library(RODBC)
            ###Create DB connection
            connection<-odbcDriverConnect('driver={SQL Server};server=192.168.2.102;
                                          database=STAGING_HOURLY_STOCKS_DATA;uid=Navneetha;pwd=Nav@123')
            ####write data to table ####
            
            sqlSave(connection,NSEdata,tablename = "STAGING_HOURLY_NSE_DATA",rownames = FALSE,
                    varTypes=c(DateTime = "datetime not null", 
                               View="varchar(250)",
                               Symbol="varchar(250)",
                               Open="real",
                               High="real",
                               Low="real",
                               LTP="real",
                               Change="real",
                               PercentChange="real",
                               TradedVolumeInlacs="real",
                               TradedValueInCrs="real",
                               High_52Week="real",
                               Low_52Week="real",
                               PercentChange_365Days="real",
                               PercentChange_30Days="real"
                               
                    ),
                    append=TRUE
            )
            
            print("Data Written to Database")
            ###Closing Connection ########
            print("connection closed")
            odbcClose(connection)
            file.remove(file.path(path,filename))
            # nseremDr$refresh()
          }
          
        }
        
      }else
      {
        
        print(sprintf("Market is Close So Task is Schedule for Tomorrow"))
        
        #taskcheduler_stop("NSEMarketWatch")
        #taskscheduler_delete(taskname = "NSEMarketWatch")
        NSEMarketWatch <- system.file("extdata", "NSEMarketWatch.R", package = "taskscheduleR")
        # taskscheduler_create(taskname = "NSEMarketWatch", rscript = NSEMarketWatch, schedule = "DAILY", starttime = "09:36:00",startdate = format(Sys.Date()+1, "%m/%d/%Y"),
        #                     modifier = 1,schtasks_extra="/RI 30 /DU 24:00")
        
        tryCatch({
          deletensetask<- taskscheduler_delete(taskname = "NSEMarketWatch")
        }, warning = function(w) {
          print("#######")
          print(w)
          print(grepl("had status 1",w))
          if(grepl('had status 1',w))
          {
            #print("in warning create")
            taskscheduler_create(taskname = "NSEMarketWatch", 
                                 rscript = NSEMarketWatch,
                                 schedule = "DAILY", 
                                 starttime = "09:36:00",startdate = format(Sys.Date()+1, "%m/%d/%Y"),
                                 modifier = 1,
                                 schtasks_extra="/RI 30 /DU 24:00"
            )
            print("Task Created Successfully")
          }
          
        },error=function(e)
        {
          print('err')
          print(length(e))
          library(RDCOMClient)
          ## init com api
          OutApp <- COMCreate("Outlook.Application")
          ## create an email 
          outMail = OutApp$CreateItem(0)
          ## configure  email parameter 
          outMail[["SentOnBehalfOfName"]] = "tnavneethasingh@defteam.com"
          outMail[["To"]] = "tnavneethasingh@defteam.com"
          outMail[["subject"]] = "Error In : NSE MarketWatch Execution"
          #outMail[["body"]] =paste(plain.text, collapse = "")
          outMail[["body"]] = paste("Hi Team,\r\n\tNSEMarketWatch Execution Error Occurred.\r\n\tError Message is :",e,"\r\n\t",
                                    "\r\n\tFor referrence,Please find the attachment 'NSEMarketWatch.log.'\r\n\t\r\nEmail Triggered By,\r\nNSEMarkerWatch.R Script\t\r\n",collapse=" ")
          
          path_to_attach_file<-"C://Users//navneetha//Documents//R//win-library//3.3//taskscheduleR//extdata//NSEMarketWatch.log"
          outMail[["Attachments"]]$Add(path_to_attach_file)
          
          ## send it                     
          outMail$Send()
          
        },finally={
          if(grepl(0,deletensetask))
          {
            print("in final del")
            taskscheduler_create(taskname = "NSEMarketWatch", 
                                 rscript = NSEMarketWatch,
                                 schedule = "DAILY", 
                                 starttime = "09:36:00",startdate = format(Sys.Date()+1, "%m/%d/%Y"),
                                 modifier = 1,
                                 schtasks_extra="/RI 30 /DU 24:00"
            )
            print("Task Created Successfully")
          }
        }
        )
        
        
      }
      
      nseremDr$closeall()
      # # stop the selenium server
      nserD[["server"]]$stop()
      
    },error=function(e)
    {
      print(paste("Error Message:",e,collapse=" "))
      
      if(length(e)>1)
      {
        if(grepl('operation did not complete before its timeout expired.',e$message) | 
           grepl('Summary: Timeout',e$message) | grepl('Summary: NoSuchElement',e$message) |
           grepl('Summary: UnknownError',e$message)| 
           grepl('Couldnt connect to host on',e$message)|
           grepl('Selenium server signals port = 4446 is already in use',e$message)|
           grepl("Couldn't connect to server",e$message) | 
           grepl("Timeout was reached",e$message)|grepl('argument is of length zero',e$message)|
           grepl('SessionNotCreatedException',e$message) | grepl('SSL connect error',e$message)
        )
        {
          odbcCloseAll()
          nseremDr$closeall()
          # # stop the selenium server
          nserD[["server"]]$stop()
          ##function call again
          NSEMarketWatch()
          
        }else
        {
          library(RDCOMClient)
          ## init com api
          OutApp <- COMCreate("Outlook.Application")
          ## create an email 
          outMail = OutApp$CreateItem(0)
          ## configure  email parameter 
          outMail[["SentOnBehalfOfName"]] = "tnavneethasingh@defteam.com"
          outMail[["To"]] = "tnavneethasingh@defteam.com"
          outMail[["subject"]] = "Error In : NSE MarketWatch Execution"
          #outMail[["body"]] =paste(plain.text, collapse = "")
          outMail[["body"]] = paste("Hi Team,\r\n\tNSEMarketWatch Execution Error Occurred.\r\n\tError Message is :",e,"\r\n\t",
                                    "\r\n\tFor referrence,Please find the attachment 'NSEMarketWatch.log.'\r\n\t\r\nEmail Triggered By,\r\nNSEMarkerWatch.R Script\t\r\n",collapse=" ")
          
          path_to_attach_file<-"C://Users//navneetha//Documents//R//win-library//3.3//taskscheduleR//extdata//NSEMarketWatch.log"
          outMail[["Attachments"]]$Add(path_to_attach_file)
          
          ## send it                     
          outMail$Send()
        }
      }else
      {
        if(grepl("Summary: NoSuchElement",e) | grepl("operation did not complete before its timeout",e) | 
           grepl("Couldnt connect to host on",e) | grepl("Couldn't connect to server",e) | grepl("Timeout was reached",e)
        )
        {
          odbcCloseAll()
          nseremDr$closeall()
          # # stop the selenium server
          nserD[["server"]]$stop()
          ##function call again
          NSEMarketWatch()
        }
        else
        {
          library(RDCOMClient)
          ## init com api
          OutApp <- COMCreate("Outlook.Application")
          ## create an email 
          outMail = OutApp$CreateItem(0)
          ## configure  email parameter 
          outMail[["SentOnBehalfOfName"]] = "tnavneethasingh@defteam.com"
          outMail[["To"]] = "tnavneethasingh@defteam.com"
          outMail[["subject"]] = "Error In : NSE MarketWatch Execution"
          #outMail[["body"]] =paste(plain.text, collapse = "")
          outMail[["body"]] = paste("Hi Team,\r\n\tNSEMarketWatch Execution Error Occurred.\r\n\tError Message is :",e,"\r\n\t",
                                    "\r\n\tFor referrence,Please find the attachment 'NSEMarketWatch.log.'\r\n\t\r\nEmail Triggered By,\r\nNSEMarkerWatch.R Script\t\r\n",collapse=" ")
          
          path_to_attach_file<-"C://Users//navneetha//Documents//R//win-library//3.3//taskscheduleR//extdata//NSEMarketWatch.log"
          outMail[["Attachments"]]$Add(path_to_attach_file)
          
          ## send it                     
          outMail$Send()
        }
      }
      
      
      
    },finally={ 
      odbcCloseAll()
      # nseremDr$closeall()
      # # stop the selenium server
      # nserD[["server"]]$stop()
      Endtime<-Sys.time()
      functioncallduration<-Endtime-starttime
      print(sprintf(paste("Duration of function call is ",functioncallduration,sep="")))
      
    }
    )
  
  
}

## functionCall
NSEMarketWatch()