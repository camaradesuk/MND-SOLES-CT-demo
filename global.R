library(googlesheets4)
library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(ggplot2)
library(plotly)
library(shinyWidgets)
library(viridis)
library(RColorBrewer)
library(data.table)
library(readr)
source("drugtochart.R")
library(knitr)
library(rmarkdown)
library(metaviz)
source("configure.R")

demoinvivoPublicationListName <- "demoinvivoPublicationList"
demoinvitroPublicationListName <- "demoinvitroPublicationList"
demodrugSummaryName <- "demodrugSummary"
demoprogressSummaryName <- "demoprogressSummary"
demopublicationListName <- "demopublicationList"

demodrugSummary <-
  googlesheets4::read_sheet(googleSheetId, sheet = demodrugSummaryName)

demoprogressSummary <-
  googlesheets4::read_sheet(googleSheetId, sheet = demoprogressSummaryName)
demopublicationList <-
  googlesheets4::read_sheet(googleSheetId, sheet = demopublicationListName)
demoinvivoPublicationList <-
  googlesheets4::read_sheet(googleSheetId, sheet = demoinvivoPublicationListName)
demoinvitroPublicationList <-
  googlesheets4::read_sheet(googleSheetId, sheet = demoinvitroPublicationListName)

democlinicalReviewSummary <- demoprogressSummary[1,]
democlinicalUpdateDate <- democlinicalReviewSummary$Date

demonClinicalUniquePubs <-
  democlinicalReviewSummary$nUniquePublications

demonClinicalIncludedPubs <-
  democlinicalReviewSummary$nIncludedPublications

demonClinicalDrugMeetLogic <-
  democlinicalReviewSummary$nDrugMeetLogic

demonClinicalPublicationsMeetLogic <-
  democlinicalReviewSummary$nPublicationsMeetLogic

demonClinicalCoreDrugs <- democlinicalReviewSummary$nCoreDrugs

demonClinicalCoreDrugsPubs <-
  democlinicalReviewSummary$nCoreDrugPublications

demonClinicalSingleAnnotated <-
  democlinicalReviewSummary$nSingleAnnotated

demopercentClinicalSingleAnnotated <-
  demonClinicalSingleAnnotated / demonClinicalCoreDrugsPubs * 100

demonClinicalDualAnnotated <-
  democlinicalReviewSummary$nDualAnnotated

demopercentClinicalDualAnnotated <-
  demonClinicalDualAnnotated / demonClinicalCoreDrugsPubs * 100

demonClinicalReconciled <- democlinicalReviewSummary$nReconciled

demopercentClinicalReconciled <-
  demonClinicalReconciled / demonClinicalCoreDrugsPubs * 100
######

demoInvivoReviewSummary <- demoprogressSummary[2,]
demoInvivoUpdateDate <- demoInvivoReviewSummary$Date

demonInvivoUniquePubs <- demoInvivoReviewSummary$nUniquePublications
demonInvivoIncludedPubs <-
  demoInvivoReviewSummary$nIncludedPublications

demonInvivoDrugMeetLogic <- demoInvivoReviewSummary$nDrugMeetLogic

demonInvivoPublicationsMeetLogic <-
  demoInvivoReviewSummary$nPublicationsMeetLogic

demonInvivoCoreDrugs <- demoInvivoReviewSummary$nCoreDrugs

demonInvivoCoreDrugsPubs <-
  demoInvivoReviewSummary$nCoreDrugPublications

demonInvivoSingleAnnotated <-
  demoInvivoReviewSummary$nSingleAnnotated

demopercentInvivoSingleAnnotated <-
  demonInvivoSingleAnnotated / demonInvivoCoreDrugsPubs * 100

demonInvivoDualAnnotated <- demoInvivoReviewSummary$nDualAnnotated

demopercentInvivoDualAnnotated <-
  demonInvivoDualAnnotated / demonInvivoCoreDrugsPubs * 100

demonInvivoReconciled <- demoInvivoReviewSummary$nReconciled

demopercentInvivoReconciled <-
  demonInvivoReconciled / demonInvivoCoreDrugsPubs * 100
#########

demoInvitroReviewSummary <- demoprogressSummary[3,]
demoInvitroUpdateDate <- demoInvitroReviewSummary$Date

demonInvitroUniquePubs <-
  demoInvitroReviewSummary$nUniquePublications
demonInvitroIncludedPubs <-
  demoInvitroReviewSummary$nIncludedPublications

demonInvitroDrugMeetLogic <- demoInvitroReviewSummary$nDrugMeetLogic

demonInvitroPublicationsMeetLogic <-
  demoInvitroReviewSummary$nPublicationsMeetLogic

demonInvitroCoreDrugs <- demoInvitroReviewSummary$nCoreDrugs

demonInvitroCoreDrugsPubs <-
  demoInvitroReviewSummary$nCoreDrugPublications

demonInvitroSingleAnnotated <-
  demoInvitroReviewSummary$nSingleAnnotated

demopercentInvitroSingleAnnotated <-
  demonInvitroSingleAnnotated / demonInvitroCoreDrugsPubs * 100

demonInvitroDualAnnotated <- demoInvitroReviewSummary$nDualAnnotated

demopercentInvitroDualAnnotated <-
  demonInvitroDualAnnotated / demonInvitroCoreDrugsPubs * 100

demonInvitroReconciled <- demoInvitroReviewSummary$nReconciled

demopercentInvitroReconciled <-
  demonInvitroReconciled / demonInvitroCoreDrugsPubs * 100

demodrugList <- sort(intersect(demodrugSummary$Drug, demopublicationList$Drug))
demodrugNumber <- gsub("drug", "", demodrugList)%>%as.numeric()%>% sort()
demodrugList <- paste0("drug", demodrugNumber)
#Progress invivo
demoinvivoReviewSummary <- demoprogressSummary[2,]

demoinvivoUpdateDate <- demoinvivoReviewSummary$Date

#Progress in vitro
demoinvitroUpdateDate <- demoprogressSummary$Date[3]