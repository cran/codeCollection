#'
#' Add label text to code
#'
#' @param x code value (character)
#' @param codebook data frame containing codes and text
#' @param lang language used, possible values "fi", "en", and "sv"
#' @param as.factor.B should value be converted to factor
#' @author Jari Haukka \email{jari.haukka@@helsinki.fi}
#' @return character vector of factor containing code value and text
#' @export
#' @examples
#' mkcodelabel("CJB10",TPKoodit,"en",FALSE)
#' mkcodelabel("CJB10",TPKoodit)
#' mkcodelabel("I351",ICD10Koodit,"en",FALSE)
mkcodelabel<-function(x,codebook,lang="fi",as.factor.B=TRUE){
  lv.tmp.ma<-match(x,codebook$Koodi)
  lv.1<-codebook[[lang]][lv.tmp.ma]
  lv.1<-ifelse(is.na(lv.1),x,lv.1)
  lv.1<-paste0(x," (",lv.1,")")
  if(as.factor.B)return(factor(lv.1))
  lv.1
}
#' @title Codebook for ICD10 codes
#'
#' @description A data set containing ICD10 codes (without dots) and labels
#' (\url{https://koodistopalvelu.kanta.fi/codeserver/pages/classification-list-page.xhtml}).
#'
#' @format A data frame:
#' \describe{
#'   \item{ Koodi}{ Code value }
#'   \item{ fi }{ Label in Finnish }
#'   \item{ la }{ Label in Latin }
#'   \item{ en }{ Name in English}
#'   \item{ sv }{ Name in Swedish}
#' }
#' @examples
#' # ICD10 codes starting with I67
#' ICD10Koodit[grepl("^I67",ICD10Koodit$Koodi),]
"ICD10Koodit"
#' @title Codebook for NOMESCO procedure codes
#'
#' @description A data set containing NOMESCO (\url{http://nowbase.org/publications/ncsp-classification-surgical-procedures}) procedure codes and labels
#'  (\url{https://koodistopalvelu.kanta.fi/codeserver/pages/classification-list-page.xhtml}).
#'
#' @format A data frame:
#' \describe{
#'   \item{ Koodi}{ Code value }
#'   \item{ fi }{ Label in Finnish }
#'   \item{ en }{ Name in English}
#'   \item{ sv }{ Name in Swedish}
#' }
#' @examples
#' # Procedure codes starting with CHD
#' TPKoodit[grepl("^CHD2",TPKoodit$Koodi),]
"TPKoodit"
#'
#' Groups rare levels of factor
#'
#' @param x  factor or other categorical variable
#' @param N.max maximum number of levels
#' @param lang language ("fi" or "en")
#' @return factor containing code value and text
#' @export
comb.levels<-function(x,N.max=10,lang="fi"){
  lv.1<-rev(sort(table(x,useNA = "always")))
  lv.1.N<-names(lv.1)[1:(min(N.max,length(lv.1)))]
  lv.p<-ifelse(lang=="fi","puuttuu","missing")
  lv.muu<-ifelse(lang=="fi","muu","other")
  lv.x<-factor(ifelse(is.na(x),lv.p,
                      ifelse(x%in%lv.1.N,as.character(x),lv.muu)))
  Epi::Relevel(lv.x,names(rev(sort(table(lv.x)))))
}
#' @title ATC codes
#'
#' @description A data set containing ATC codes
#' ”@note Only in English
#' @format A data frame:
#' \describe{
#'   \item{ Koodi}{ Code value }
#'   \item{ en }{ Label inEnglish}
#'   \item{ fi }{ Label in English}
#' }
"ATCKoodit"
#' @title ICPC codes
#'
#' @description A data set containing International Classification of Primary Care (ICPC) codes.
#' If English code is missing uses Finnish, and vice versa (\url{https://koodistopalvelu.kanta.fi/codeserver/pages/classification-list-page.xhtml}).
#' @format A data frame:
#' \describe{
#'   \item{ Koodi}{ Code value }
#'   \item{ en }{ Label inEnglish}
#'   \item{ fi }{ Label in English}
#' }
"ICPCKoodit"
#' @title Statistics Finland causes of death time series codes
#'
#' @description Statistics Finland causes of death time series codes
#' ( \url{https://tilastokeskus.fi/til/ksyyt/ksyyt_2018-11-12_luo_001.pdf} )
#' ”@note Only in English
#' @format A data frame:
#' \describe{
#'   \item{ Koodi}{ Code value }
#'   \item{ en }{ Label in English}
#'   \item{ fi }{ Label in Finnish}
#' }
"tpksaKoodit"
#' @title SPAT procedure codes for primary care
#'
#' @description SPAT procedure codes for primary care (\url{https://koodistopalvelu.kanta.fi/codeserver/pages/classification-list-page.xhtml}).
#' ”@note Only in Finnish and Swedish
#' @format A data frame:
#' \describe{
#'   \item{ Koodi}{ Code value }
#'   \item{ en }{ Label in Finnish}
#'   \item{ fi }{ Label in Finnish}
#'   \item{ se }{ Label in Swedish}
#' }
"SPATKoodit"
#' @title ICD-O-3 codes
#'
#' @description A data set containing ICD-O-3 codes and labels
#'
#' @format A data frame:
#' \describe{
#'   \item{ Koodi}{ Code value }
#'   \item{ fi }{ Label in English }
#'   \item{ en }{ Label in English }
#'   \item{ se }{ Label in English }
#' }
"ICDO3Koodit"
#' @title ICD-O-3 codes (no dot in codes)
#'
#' @description A data set containing ICD-O-3 codes (no dot in codes) and labels
#'
#' @format A data frame:
#' \describe{
#'   \item{ Koodi}{ Code value }
#'   \item{ fi }{ Label in English }
#'   \item{ en }{ Label in English }
#'   \item{ se }{ Label in English }
#' }
"ICDO3Koodit1"
