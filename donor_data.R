require(igraph)
require(packrat)
require(visNetwork)
require(shiny)
require(rvest)
require(stringr)
require(dplyr)

##################################################
# Helper function: obtains all industries in particular industry sector.
#
getIndustries <- function() {
  
  industries <- data.frame()
  
  for (i in 2:19) {
    
    url <- paste0("https://www.vpap.org/money/donors-industry-totals/", i, "/?recip_type=all&amp;year=all&year=all")
    webpage <- read_html(url)
    
    industries_table <- html_element(webpage, 'table')
    
    if (class(industries_table) == "xml_node") {
      
      newIndustries <- html_table(industries_table)
      
      sector <- webpage %>% 
        html_node(xpath = paste0('//*[@id="prime-container"]/div/div[1]/h3')) %>%
        html_text() %>%
        gsub("Donations from ", "", .)
      
      industries <- rbind(industries, cbind(newIndustries[,"Donor"], sector))
    }
    
  }
  
  colnames(industries) <- c("Industry", "Sector")
  return(industries)
}

##################################################
# Helper function: scrapes donor data from url
# 
# Pre:  requires key and ind
#
scrapeURL <- function(url, i) {
  
  webpage <- read_html(url)
  
  donors_table <- html_element(webpage, 'table')
  
  if (class(donors_table) == "xml_node") {
    
    donorData <- html_table(donors_table)
    
    donorData[,"Amount"] <- lapply(donorData[,"Amount"], function(x) {
      x <- gsub("\\$", "", x)
      x <- gsub(",", "", x)
      x <- as.numeric(x)
    })
    colnames(donorData)[1] <- "amount"
    
    industry <- webpage %>% 
      html_node(xpath = paste0("/html/body/div[5]/div[4]/div[2]/div[2]/h4/text()")) %>%
      html_text() %>%
      gsub("Donors by Occupation: ", "", .) %>%
      gsub("[[:cntrl:]]", "", .) %>%
      trimws()
    
    donorData$industry <- industry
    donorData$sector <- ind[ind$Industry == industry, "Sector"]
    donorData$key <- ifelse(i %in% key[,1], key[key[,1] == i, 2], "None")
    colnames(donorData)[2] <- "donor"
    
    return(donorData)
  }
  
  return(NULL)
}

##################################################
# Returns table of all individual donors
#
getDonors <- function(person, year, all = TRUE) {
  
  donors <- data.frame()
  
  ID <- donorIDs[donorIDs$Name == person, "ID"]
  
  for (i in valid) {
    
    if (all) {
      url <- paste0("https://www.vpap.org/candidates/", ID, "/donors_per_industry/", i, "?contrib_type=all&start_year=1900&end_year=", year, "&order=name")
      newDonors <- scrapeURL(url, i)
      if (!is.null(newDonors)) {
        newDonors$name <- person
        newDonors$year <- paste0("pre", year+1)
        donors <- rbind(donors, newDonors)
        print(as.data.frame(newDonors))
      }
    }
    else {
      url <- paste0("https://www.vpap.org/candidates/", ID, "/donors_per_industry/", i, "?contrib_type=all&start_year=", year, "&end_year=", year, "&order=name")
      newDonors <- scrapeURL(url, i)
      if (!is.null(newDonors)) {
        newDonors$name <- person
        newDonors$year <- year
        donors <- rbind(donors, newDonors)
        print(as.data.frame(newDonors))
      }
    }
  }

  return(donors)

}

##################################################
# Obtain summary of donation data
#
# type: can be either current or historical
# by: can be either key, donor, industry, or sector
#
sumDonors <- function(donors, person, year, type = "current", by = "key") {
  
  donors <- donors[donors$name == person,]
  
  if (type == "current") {
    donors <- donors[donors$year == year,]
  }
  else if (type == "historical") {
    donors <- donors[donors$year <= year | donors$year == paste0("pre", min(donors$year)),]
  }

  donors <- donors %>%
    group_by(donors[,by]) %>%
    summarise(amount = sum(amount))
  
  colnames(donors) = c(by, "amount")

  return(donors)

}

# Information on key bills
bills <- read.csv("C:/Users/k/Downloads/VT classes/Research/20220324_Bill_Mapping - List of Priority Bills.csv", header = TRUE)

# IDs for politicians on VPAP
donorIDs <- read.csv("C:/Users/k/Downloads/VT classes/Research/20220324_Bill_Mapping - Donor2.csv", header = TRUE)

# Valid pages to view on VPAP
valid <- c(2:141, 143:208, 210, 212:213, 215:217, 219:222, 226:232)

# Pages to view on VPAP for each key industry
agric <- cbind(c(2:7, 10, 11, 14), "Agriculture")
rlest <- cbind(c(15:33, 203, 227), "Real Estate")
fosch <- cbind(c(49:51, 58, 147, 149), "Fossil and Chemical")
utili <- cbind(c(52, 55, 202), "Utilities")
envir <- cbind(c(53, 54, 56, 59, 60), "Misc. Environmental")
powge <- cbind(c(57, 220), "Power Generation")
socgr <- cbind(c(165), "Social Greens")
trans <- cbind(c(175:187), "Transportation")
alten <- cbind(c(212), "Alternative Energy")
key <- rbind(agric, rlest, fosch, utili, envir, powge, socgr, trans, alten)

# Industries on VPAP for each industry sector
ind <- getIndustries()


# Donor data for Sam Rasoul, 2021
donors2 <- getDonors("Sam Rasoul", 2021, FALSE)

# Summary donor data
sumDonors(donors2, "Sam Rasoul", year = 2021, type = "current", by = "key")

##################################################
# Collecting all donor data and writing it to a csv file
#

#donors <- data.frame() # Note: only used when first collecting donor data

if (FALSE) {
  for (i in 1:nrow(donorIDs)) {
    
    # These do.call() functions are my way of saving my gathered data in chunks, this could be inefficient
    do.call("<-", list(paste("donors", gsub(" ", "_", donorIDs[i,1]), "pre2015", sep = "_"), getDonors(donorIDs[i,1], 2014)))
    donors <- rbind(donors, get(paste("donors", gsub(" ", "_", donorIDs[i,1]), "pre2015", sep = "_")))
    
    for (j in 2015:2022) {
      do.call("<-", list(paste("donors", gsub(" ", "_", donorIDs[i,1]), j, sep = "_"), getDonors(donorIDs[i,1], j, all = FALSE)))
      donors <- rbind(donors, get(paste("donors", gsub(" ", "_", donorIDs[i,1]), j, sep = "_")))
    }
    
  }
}

write.csv(donors, file = "C:/Users/k/Downloads/VT classes/Research/donorData.csv")

sumDonors(donors, "Sam Rasoul", 2021, type = "current", by = "key")


