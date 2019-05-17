rm(list=ls())
path <- paste0(.libPaths()[1],"/MicrosoftAdsR/")

####AUTHENTICATION#####

#' Microsoft Ads Authentication Function
#'
#' This function allows you to authenticate with Microsoft Ads
#' @param Credentials Pass the client id, account id, customer id and dev token
#' @keywords Authentication
#' @export
#' @examples
#' Authentication()


Authentication <- function(credentials)
{
  ## Define functions
  setAccessToken <- function(credentials, access_token, refresh_token, expires_in)
  {
    credentials <- list(
      client_id     = credentials$client_id,
      access_token  = access_token,
      refresh_token = refresh_token,
      expires_in    = expires_in,
      account_id    = credentials$account_id,
      download_location = credentials$download_location,
      filename      = credentials$filename,
      startDate     = credentials$startDate,
      endDate       = credentials$endDate,
      customer_id   = credentials$customer_id,
      developer_token = credentials$developer_token)

    print("Tokens Generated.")
    return(credentials)
  }

  authenticateFirstTimeUser <- function(credentials)
  {
    url <- sprintf('https://login.microsoftonline.com/common/oauth2/v2.0/authorize?client_id=%s&scope=openid profile https://ads.microsoft.com/ads.manage offline_access&response_type=code&redirect_uri=https://login.microsoftonline.com/common/oauth2/nativeclient&prompt=consent', credentials$client_id)
    browseURL(url)
    code <- readline("Grant consent in the browser and enter the code here: ")

    host <- ('https://login.microsoftonline.com/common/oauth2/v2.0/token')
    body <- sprintf('client_id=%s&scope=https://ads.microsoft.com/ads.manage offline_access&code=%s&grant_type=authorization_code&redirect_uri=https://login.microsoftonline.com/common/oauth2/nativeclient', credentials$client_id, code)
    header <- c("ContentType" =  "application/x-www-form-urlencoded")

    response <- POST(host, body = body, encode="form", add_headers(.headers = header))
    oauthtokens <- fromJSON(txt = as.character(rawToChar(response$content)))
    access_token <- oauthtokens$access_token
    expires_in <- oauthtokens$expires_in
    refresh_token <- oauthtokens$refresh_token
    credentials <- setAccessToken(credentials, access_token, refresh_token, expires_in)
    return(credentials)
  }

  # The access token will expire e.g., after one hour.
  # Use the refresh token to get a new access token.

  getAccessToken <- function(credentials)
  {
    host <- ('https://login.microsoftonline.com/common/oauth2/v2.0/token')
    header <- c("ContentType" =  "application/x-www-form-urlencoded")
    Body <- sprintf('client_id=%s&scope=https://ads.microsoft.com/ads.manage offline_access&grant_type=refresh_token&refresh_token=%s',credentials$client_id, bing_refresh_token)
    response = POST(host, body = Body, encode="form", add_headers(.headers = header))
    oauthtokens <- fromJSON(txt = as.character(rawToChar(response$content)))
    access_token <- oauthtokens$access_token
    expires_in <- oauthtokens$expires_in
    refresh_token <- oauthtokens$refresh_token
    write(refresh_token, "bing_refresh_token")
    credentials <- setAccessToken(credentials, access_token, refresh_token, expires_in)
    return(credentials)
  }

  ## Begin Authentication
  if(!file.exists("bing_refresh_token"))
  {
    print("Bing Refresh Token not present. Will Use code.")
    credentials <- authenticateFirstTimeUser(credentials)
  }
  else
  {
    bing_refresh_token <- readLines("bing_refresh_token")
    if(bing_refresh_token=="")
    {
      print("Refresh Token is empty. Will Use code.")
      credentials <- authenticateFirstTimeUser(credentials)
    }
    else
    {
      print("Getting Access Token using Refresh Token.")
      credentials <- getAccessToken(credentials)
    }
  }
  return(credentials)
}

##Report Any Type

#' Get Report ID Function
#'
#' This function allows you to request the report and get the required report id
#' @param Credentials Pass the client id, account id, customer id and dev token
#' @keywords getReportId
#' @export
#' @examples
#' getReportId()


getReportId <- function(credentials, report, columns, startDate, endDate){
  dateSplitter <- function(x){
    x <- as.Date(x, origin = "1970-01-01")
    tmp <- list()
    tmp$year <- as.integer(format(x, "%Y"))
    tmp$month <- as.integer(format(x, "%m"))
    tmp$day <- as.integer(format(x, "%d"))
    return(tmp)
  }

  getColumnsXML <- function(report, columms){
    columnsXML <- ""
    for(column in columns){
      columnsXML <- paste0(columnsXML, "<", report, "Column>", column, "</", report, "Column>")
    }
    return(columnsXML)
  }

  startDate <- dateSplitter(credentials$startDate)
  endDate <- dateSplitter(credentials$endDate)
  reportname <- gsub("Request","",report)
  url <- "https://reporting.api.bingads.microsoft.com/Api/Advertiser/Reporting/v13/ReportingService.svc"
  SOAPAction <- "SubmitGenerateReport"
  header <- paste(readLines(paste0(path,"reporting.header.xml")), collapse = "")
  bodyXML <- paste(readLines(paste0(path,"reporting.SubmitGenerateReportRequest.xml")), collapse = "")
  columnsXML <- getColumnsXML(reportname, columms)
  bodyXML <- sprintf(bodyXML, report, report, columnsXML, credentials$account_id, endDate$day, endDate$month, endDate$year, startDate$day, startDate$month, startDate$year)
  body <- sprintf(header, SOAPAction, credentials$access_token, credentials$account_id, credentials$customer_id, credentials$developer_token, bodyXML)
  h = basicTextGatherer()
  body
  h$reset()
  curlPerform(url=url,
              httpheader = c(Accept = "text/xml", Accept = "multipart/*",
                             SOAPAction = SOAPAction,
                             'Content-Type' = "text/xml;charset=utf-8"),
              postfields=body,
              writefunction = h$update,
              verbose = FALSE
              )

  body = h$value()
  reportId <- xmlToList(h$value())$Body$SubmitGenerateReportResponse$ReportRequestId
  if(is.null(reportId)|reportId==""){print(paste("Error - ", h$value))}
  return(reportId)
}

#' Download Report Function
#'
#' This function uses the report ID - polls to check if the report is ready then hits the download url
#' @param Credentials Pass the access token, developer token and account id along with the report id
#' @keywords getDownloadUrl
#' @export
#' @examples
#' getDownloadUrl()

getDownloadUrl <- function(credentials, reportId)
  {
    url <- "https://reporting.api.bingads.microsoft.com/Api/Advertiser/Reporting/v13/ReportingService.svc"
    SOAPAction <- "PollGenerateReport"
    report <- "PollGenerateReportRequest"
    header <- paste(readLines(paste0(path,"reporting.header.xml")), collapse = "")
    bodyXML <- '<PollGenerateReportRequest xmlns="https://bingads.microsoft.com/Reporting/v13"><ReportRequestId i:nil="false">%s</ReportRequestId></PollGenerateReportRequest>'
    bodyXML <- sprintf(bodyXML, reportId)
    body <- sprintf(header, SOAPAction, credentials$access_token,credentials$account_id,credentials$customer_id,  credentials$developer_token,bodyXML)
    h = basicTextGatherer()

    status <- "Generating"
    attempts <- 0
    print(sprintf("Report Status: %s",status))
    while (status != "Success" && attempts < 10)
    {
      attempts <- attempts + 1

      if (attempts > 9)
        {
        msg = ("Too many retries, job failed")
        stop(msg)
        } else if (status == "Error")
          {
          msg = ("Error in report request")
          stop(msg)
          } else {
            body <- sprintf(header, SOAPAction, credentials$access_token,credentials$account_id,credentials$customer_id,  credentials$developer_token,bodyXML)
            h = basicTextGatherer()
            h$reset()
            curlPerform(url = url,
                    httpheader = c(Accept = "text/xml", Accept = "multipart/*",
                                   SOAPAction = SOAPAction,
                                   'Content-Type' = "text/xml;charset=utf-8"),
                    postfields = body,
                    writefunction = h$update,
                    verbose = FALSE)
            body = h$value
            status <- xmlToList(h$value())$Body$PollGenerateReportResponse$ReportRequestStatus$Status
            print(sprintf("Attempt: %s",attempts))
            print(sprintf("Report Status: %s",status))
            msg = ("All good")
          }
    }

  downloadUrl <- xmlToList(h$value())$Body$PollGenerateReportResponse$ReportRequestStatus$ReportDownloadUrl
  destfilelocation <- sprintf(credentials$download_location, sprintf(credentials$filename,reportId,startDate,endDate))
  download.file(url = downloadUrl, destfile = destfilelocation, mode = 'wb', method ='auto', quiet=TRUE)
  print(sprintf("File downloaded Successfully here: %s",destfilelocation))
  }

