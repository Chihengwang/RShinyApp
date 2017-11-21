#===========================================================================
# Library
#===========================================================================
library(shiny)
library(dplyr)
library(data.table)
library(RCurl)
library(rjson)

#===========================================================================
# Server
#===========================================================================
function(input, output) {
  #==== Get UI.R's input ====
  UI_input <- reactive({  v_1 <- input$PassengerClass
  v_2 <- input$Gender 
  v_3 <- as.character(input$Age)  
  
  return(list( v_1,v_2,v_3 ))
  })
  
  #==== Output : Prediction ====   
  output$result_plot <- renderImage({
    #---- Connect to Azure ML workspace ----  
    options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
    # Accept SSL certificates issued by public Certificate Authorities
    h = basicTextGatherer()
    hdr = basicHeaderGatherer()
    input_data =  UI_input()
    #---- Put input_data to Azure ML workspace ----
    req = list(
      Inputs = list(
        "input1" = list(
          "ColumnNames" = list("Pclass", "Sex", "Age"),
          "Values" = list( input_data   ) 
          #Example: input_data = list("3", "male", "50", "0", "0", "0", "A")
        )
      ),
      GlobalParameters = setNames(fromJSON('{}'), character(0))
    )
    
    #---- Web service : API key ----
    body = enc2utf8(toJSON(req))
    api_key = "htGVAkrhVI9OcqgpkJY14Nw0EBX0HCNSt+tUVnf+OLoZfHdgrjdNQrV5SiYzP3AP3IsHY8Q41mCiaQgy71XheQ==" 
    authz_hdr = paste('Bearer', api_key, sep=' ')
    
    h$reset()
    curlPerform(url = "https://ussouthcentral.services.azureml.net/workspaces/ae5e6b1aa49847228498327c4960d383/services/976c45d08c57424db5b2ab78d466d9a0/execute?api-version=2.0&details=true",
                httpheader=c('Content-Type' = "application/json", 'Authorization' = authz_hdr),
                postfields=body,
                writefunction = h$update,
                headerfunction = hdr$update,
                verbose = TRUE
    )
    
    #---- Get Result  ----
    result = h$value()
    result= fromJSON(result)
    #================Check http status is 200=====================
    httpStatus = headers["status"]
    
    if (httpStatus >= 400)
    {
      print(paste("The request failed with status code:", httpStatus, sep=" "))
      
      # Print the headers - they include the requert ID and the timestamp, which are useful for debugging the failure
      print(headers)
    }
    if (as.numeric(result$Results$output1$value$Values)==1) {
      return( list(
        src = "www/survived.png",
        height = 480, width = 700,
        alt = "Survived"
      ))
    }else if (as.numeric(result$Results$output1$value$Values)==0) {
      return(list(
        src = "www/deceased.png",
        height = 480, width = 700,
        alt = "Deceased"
      ))
    }
  }, deleteFile = FALSE)
}
