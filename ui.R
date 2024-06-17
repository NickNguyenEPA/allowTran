
source('yknow.R')

# Define UI --------------------------------------------------------------------


ui <- fluidPage(
  
  sidebarLayout(
    
    sidebarPanel(
      
      #API file input
      fileInput(inputId = "file1", 
                label = "Upload API-Key File",
                buttonLabel = "Browse...",
                placeholder = "No file selected",
                
      ),
      
      #Pulling API request button
      actionButton(inputId = "callingAPI", label = "Update data"),
      #API pulling message
      verbatimTextOutput("mess"),
      
      br(),
      
      #Date range input
      dateRangeInput(inputId = "dateChoice",
                     label = "Select date range:",
                     # start = min_date, end = max_date,
                     min = "1994-03-15", max = current_date,
                     startview = "year"
      ),
      
      #Time unit input
      selectInput(inputId = "dateUnit", 
                  label = "Select Time unit", 
                  choices = c("Weekly" = "%Y-%U",
                              "Monthly" = "%Y-%m",
                              "Yearly" = "%Y"
                  )),
      
      #Sort by Program input
      virtualSelectInput(
        inputId = "checkGroups",
        label = "Select Program:", 
        choices = unique(allowTrans$programCodeInfo),
        selected = unique(allowTrans$programCodeInfo),
        search = TRUE,
        multiple = TRUE
      ),
      
      #Sort by transaction types
      virtualSelectInput(
        inputId = "transTypes",
        label = "Select Transaction Types:", 
        choices = unique(allowTrans$transactionType),
        selected = unique(allowTrans$transactionType),
        search = TRUE,
        multiple = TRUE
    ),
    
    #Sort by vintage year
    virtualSelectInput(
      inputId = "vinYear",
      label = "Select Vintage Year:", 
      choices = unique(allowTrans$vintageYear),
      selected = unique(allowTrans$vintageYear),
      search = TRUE,
      multiple = TRUE
    ),
    
    # Seller Info --------------------------------------------------------------------
    
    helpText("Select Seller Information:"),
    # Dropdown menu
    dropdown(
      label = "Seller Information ",
      icon = icon("sliders"),
      width = "100%",
    #Sort by sell number
    virtualSelectInput(
      inputId = "sellNum",
      label = "Sell Account Number:", 
      choices = unique(allowTrans$sellAccountNumber),
      selected = unique(allowTrans$sellAccountNumber),
      showValueAsTags = FALSE,
      search = TRUE,
      multiple = TRUE
    ),
    #Sort by sell name
    virtualSelectInput(
      inputId = "sellName",
      label = "Sell Name:", 
      choices = unique(allowTrans$sellAccountName),
      selected = unique(allowTrans$sellAccountName),
      showValueAsTags = FALSE,
      search = TRUE,
      multiple = TRUE
    ),
    #Sort by sell account type
    virtualSelectInput(
      inputId = "sellType",
      label = "Sell Account Type:", 
      choices = unique(allowTrans$sellAccountType),
      selected = unique(allowTrans$sellAccountType),
      showValueAsTags = FALSE,
      search = TRUE,
      multiple = TRUE
    ),
    #Sort by sell facility name
    virtualSelectInput(
      inputId = "sellFacName",
      label = "Sell Facility Name:", 
      choices = unique(allowTrans$sellFacilityName),
      selected = unique(allowTrans$sellFacilityName),
      showValueAsTags = FALSE,
      search = TRUE,
      multiple = TRUE
    ),
    #Sort by sell facility ID
    virtualSelectInput(
      inputId = "sellFacId",
      label = "Sell Facility ID:", 
      choices = unique(allowTrans$sellFacilityId),
      selected = unique(allowTrans$sellFacilityId),
      showValueAsTags = FALSE,
      search = TRUE,
      multiple = TRUE
    ),
    #Sort by sell State
    virtualSelectInput(
      inputId = "sellState",
      label = "Sell State:", 
      choices = unique(allowTrans$sellState),
      selected = unique(allowTrans$sellState),
      showValueAsTags = FALSE,
      search = TRUE,
      multiple = TRUE
    ),
    #sell EPA Region
    virtualSelectInput(
      inputId = "sellEpaRegion",
      label = "Sell EPA REgion:", 
      choices = unique(allowTrans$sellEpaRegion),
      selected = unique(allowTrans$sellEpaRegion),
      showValueAsTags = FALSE,
      search = TRUE,
      multiple = TRUE
    ),

    #Sort by sell owner
    virtualSelectInput(
      inputId = "sellOwner",
      label = "Sell Owner:", 
      choices = unique(allowTrans$sellOwner),
      selected = unique(allowTrans$sellOwner),
      showValueAsTags = FALSE,
      search = TRUE,
      multiple = TRUE
    )
    ),
    
    #Buyer Info --------------------------------------------------------------------
    
    helpText("Select Buyer Information"),
    #Buyer dropdown menu
    dropdown(
      label = " Buyer Information ", 
      icon = icon("sliders"),
      width = "100%",
    #Sort by buy number
    virtualSelectInput(
      inputId = "buyNum",
      label = "Buy Account Number:", 
      choices = unique(allowTrans$buyAccountNumber),
      selected = unique(allowTrans$buyAccountNumber),
      showValueAsTags = FALSE,
      search = TRUE,
      multiple = TRUE
    ),
      #Sort by buy name
      virtualSelectInput(
        inputId = "buyName",
        label = "Buy Name:", 
        choices = unique(allowTrans$buyAccountName),
        selected = unique(allowTrans$buyAccountName),
        showValueAsTags = FALSE,
        search = TRUE,
        multiple = TRUE
      ),
    #Sort by buy acccount type
    virtualSelectInput(
      inputId = "buyType",
      label = "Buy Account Type:", 
      choices = unique(allowTrans$buyAccountType),
      selected = unique(allowTrans$buyAccountType),
      showValueAsTags = FALSE,
      search = TRUE,
      multiple = TRUE
    ),
    #Sort by buy Facility Name
    virtualSelectInput(
      inputId = "buyFacName",
      label = "Buy Facility Name:", 
      choices = unique(allowTrans$buyFacilityName),
      selected = unique(allowTrans$buyFacilityName),
      showValueAsTags = FALSE,
      search = TRUE,
      multiple = TRUE
    ),
    #Sort by buy facility ID
    virtualSelectInput(
      inputId = "buyFacId",
      label = "Buy Facility ID:", 
      choices = unique(allowTrans$buyFacilityId),
      selected = unique(allowTrans$buyFacilityId),
      showValueAsTags = FALSE,
      search = TRUE,
      multiple = TRUE
    ),
    #Sort by buy State
    virtualSelectInput(
      inputId = "buyState",
      label = "Buy State:", 
      choices = unique(allowTrans$buyState),
      selected = unique(allowTrans$buyState),
      showValueAsTags = FALSE,
      search = TRUE,
      multiple = TRUE
    ),
    #sort by buy EPA Region
    virtualSelectInput(
      inputId = "buyEpaRegion",
      label = "Buy EPA Region:", 
      choices = unique(allowTrans$buyEpaRegion),
      selected = unique(allowTrans$buyEpaRegion),
      showValueAsTags = FALSE,
      search = TRUE,
      multiple = TRUE
    ),
    
    #sort by buy Owner
    virtualSelectInput(
      inputId = "buyOwner",
      label = "Buy Owner:", 
      choices = unique(allowTrans$buyOwner),
      selected = unique(allowTrans$buyOwner),
      showValueAsTags = FALSE,
      search = TRUE,
      multiple = TRUE
    )
    ),
    
    br(),
    
    #dowload button
    helpText("Download detail datatable"),
    downloadButton("downloadData", "Download")
    
    ),
    
    mainPanel(
      #Plots and Summary tabset panel
      tabsetPanel(
        type = "tabs",
        #Time Serires plot by program 
        tabPanel("Programs by Time Series", plotOutput(outputId = "linePlot")),
        #Bar Chart plot by program
        tabPanel("Programs by Bar Chart", plotOutput(outputId = "barPlot")),
        #Summary tab
        tabPanel("Summary",  p("The total amount of block is:", 
                               textOutput("totalBlock", inline = TRUE)),
                 DT::dataTableOutput('summary'))
      ),
      
      # Table tabset panel
      tabsetPanel(
        type = "tabs",
        #Daily General info
        tabPanel("Daily General", dataTableOutput(outputId = "allowTable")),
        #Top 3 seller table
        tabPanel("Top 3 Seller (by Time Unit)", dataTableOutput("seller")),
        #Top 3 buyer table
        tabPanel("Top 3 Buyer (by Time Unit)", dataTableOutput("buyer")),
        #Raw table
        tabPanel("Detail", dataTableOutput("detail"))
      )
    )
  )
)
