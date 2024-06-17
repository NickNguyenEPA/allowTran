# Define server ----------------------------------------------------------------

server <- function(input, output, session) {
  #Make an apiKEY variable as an reactiveValue
  apiKEY <- reactiveValues()
  #define the max size upload
  options(shiny.maxRequestSize=10000*1024^2)
  
  #load an apiKEY file widget
  observe({
    if (is.null(input$file1)) return()
    file.copy(input$file1$datapath, "./data/apiKey/api_key.txt", overwrite = TRUE)
    apiKEY <<- read_lines(input$file1$datapath)
    allowanceUrl <<- paste0(apiUrlBase,"/streaming-services/allowance-transactions?API_KEY=",apiKEY)
    
  })
  
  #make a chosenData reactive value
  chosenData <- reactiveValues()
  
  
  #Sorting chosenData by filter widgets
 observe({ chosenData$df <- subset(allowTrans, transactionDate >= as.Date(input$dateChoice[1]) & 
                                     transactionDate <= as.Date(input$dateChoice[2]) &
                                     programCodeInfo %in% input$checkGroups &
                                     transactionType %in% input$transTypes &
                                     vintageYear %in% input$vinYear &
                                     #sort by seller infos
                                     sellAccountNumber %in% input$sellNum &
                                     sellAccountName %in% input$sellName &
                                     sellAccountType %in% input$sellType &
                                     sellFacilityName %in% input$sellFacName &
                                     sellFacilityId %in% input$sellFacId &
                                     sellState %in% input$sellState &
                                     sellEpaRegion %in% input$sellEpaRegion &
                                     sellOwner %in% input$sellOwner &

                                    #sort buy buyer infos
                                     buyAccountNumber %in% input$buyNum &
                                     buyAccountName %in% input$buyName &
                                     buyAccountType %in% input$buyType &
                                     buyFacilityName %in% input$buyFacName &
                                     buyFacilityId %in% input$buyFacId &
                                     buyState %in% input$buyState &
                                     buyEpaRegion %in% input$buyEpaRegion &
                                     buyOwner %in% input$buyOwner
                                    
                                     )
 })


 # observe event when clicking Update data button
 observeEvent(input$callingAPI, {
   
   #api call data
   re <- GET(paste0(allowanceUrl, "&transactionBeginDate=", max_date, "&transactionEndDate=", current_date))
   
   #logical condition if res status return 200 and there value is content
   if(re$status_code == 200 & length(fromJSON(rawToChar(re$content))) > 0){
     # make a new update value data
     updateData <- fromJSON(rawToChar(re$content))
     updateData <- updateData %>% mutate_all(~replace(., is.na(.), "NULL"))
     updateData <- updateData %>% mutate(across(everything(), ~gsub("[[:punct:]]", " ", .x)))
     updateData$transactionDate <- as.Date(updateData$transactionDate, format = "%Y %m %d")
     updateData$totalBlock <- as.numeric(updateData$totalBlock)
     ma_date <- max(updateData$transactionDate, na.rm = TRUE)
     
        #another if-in-if to check that ma_date of update data > max_date of current data
       if(ma_date > max_date){
         #combine data
         allowTrans <<- rbind(allowTrans, updateData)
         #save data to allowTrans
         save(allowTrans, file="./data/allowanceTransaction/allowTrans.RData")
         
         
         chosenData$df <<- subset(allowTrans, transactionDate >= as.Date(input$dateChoice[1]) &
                                    transactionDate <= as.Date(input$dateChoice[2]) &
                                    programCodeInfo %in% input$checkGroups &
                                    transactionType %in% input$transTypes &
                                    vintageYear %in% input$vinYear &
                                    
                                    #sort by seller infos
                                    sellAccountNumber %in% input$sellNum &
                                    sellAccountName %in% input$sellName &
                                    sellAccountType %in% input$sellType &
                                    sellFacilityName %in% input$sellFacName &
                                    sellFacilityId %in% input$sellFacId &
                                    sellState %in% input$sellState &
                                    sellEpaRegion %in% input$sellEpaRegion &
                                    sellOwner %in% input$sellOwner &
                                    
                                    #sort buy buyer infos
                                    buyAccountNumber %in% input$buyNum &
                                    buyAccountName %in% input$buyName &
                                    buyAccountType %in% input$buyType &
                                    buyFacilityName %in% input$buyFacName &
                                    buyFacilityId %in% input$buyFacId &
                                    buyState %in% input$buyState &
                                    buyEpaRegion %in% input$buyEpaRegion &
                                    buyOwner %in% input$buyOwner
         )
   }
   
  #print message  
   output$mess <- renderText("your data up-to-date")
   #condition if there is no APIKey provide
   }else if(re$status_code == 403){
     output$mess <- renderText(fromJSON(rawToChar(re$content))$error$message)
     #other error condition
   }else{
     output$mess <- renderText((fromJSON(rawToChar(re$content)))$message)
   }
   
  })
  
  #line plot
  output$linePlot <- renderPlot({
    df <- chosenData$df %>%
      group_by(date = format(transactionDate, input$dateUnit), program = programCodeInfo) %>%
      summarise(total=sum(totalBlock))
    
    ggplot(df, aes(x = date, y = total)) +
      geom_line(aes(color = program, group=program), size = 1) +
      # scale_color_manual(values = c("#00AFBB", "#E7B800")) +
      guides(x =  guide_axis(angle = 90)) +
      scale_y_log10() +
      xlab("Time") +
      ylab("Total Transfer") +
      ggtitle("Allowance Transfer") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  #bar Plot

  output$barPlot <- renderPlot({
    df <- chosenData$df %>%
      group_by(program = programCodeInfo) %>%
      summarise(total=sum(totalBlock))

    ggplot(df, aes(x = program, y = total, fill=program)) +
      geom_bar(stat="identity", width=0.5) +
      xlab("Program") +
      ylab("Total Transfer") +
      ggtitle("Allowance Transactions Bar Chart") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  #general table
  output$allowTable <- renderDataTable({
    df <- chosenData$df %>%
      group_by(date = transactionDate, transaction_Type=transactionType, program = programCodeInfo, seller=sellAccountName, buyer=buyAccountName) %>%
      summarise(total=sum(totalBlock))
  })
  
  output$seller <- renderDataTable({
    df <- chosenData$df %>%
      group_by(date = format(transactionDate, input$dateUnit), program = programCodeInfo, seller=sellAccountName) %>%
      summarise(total=sum(totalBlock)) %>%
      arrange(desc(total)) %>% slice(1:3)
  })
    
    output$buyer <- renderDataTable({
      df <- chosenData$df %>%
        group_by(date = format(transactionDate, input$dateUnit), program = programCodeInfo, buyer=buyAccountName) %>%
        summarise(total=sum(totalBlock)) %>%
        arrange(desc(total)) %>% slice(1:3)
  })
    
    output$detail <- renderDataTable({
      chosenData$df
    })
    
    output$totalBlock <- renderText(sum(chosenData$df$totalBlock))
    
    output$summary <- DT::renderDataTable(
      df <- chosenData$df %>%
        group_by(program = programCodeInfo) %>%
        summarise(total=sum(totalBlock)),
      options = list(dom = 't', pagingType = "simple", ordering=F)

    )
    #download detail data
    output$downloadData <- downloadHandler(
      filename = function() {
        # Use the selected dataset as the suggested file name
        paste0("detail.csv")
      },
      content = function(file) {
        # Write the dataset to the `file` that will be downloaded
        write.csv(chosenData$df, file)
      }
    )
  
}
