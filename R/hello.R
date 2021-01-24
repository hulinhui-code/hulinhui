# Hello, world!

#' @export
hello <- function() {
  print('Good luck')
}


updateR <- function() {
  lib <- (.packages())  # 已加载包列表
  if ('mypkg' %in% lib) {detach(package:mypkg)}  如果mypkg已加载，则卸载
  library(devtools) 
  install_github('hulinhui-code/hulinhui', lib = 'D:/R pkg lib')
  }


myInstall <- function(package_name) {
  install.packages(package_name, lib="D:/R pkg lib")
  }

myLibrary <- function(package_name) {
  library(package_name, lib="D:/R pkg lib")
  }


display_file <- function(pdf_path){
pdf_html <- paste('<iframe src="',pdf_path,
                  '" align="center" width="1111" height="900" frameBorder="0"></iframe>',
                  sep="")
IRdisplay::display_html(pdf_html)
#             return(pdf_html)
}


display_panel <- function(address1, address2){
pdf_html <- paste('<iframe src="',address1,
                  '" align="center" width="650" height="900" frameBorder="0"></iframe><iframe src="', address2,
                  '" align="center" width="650" height="900" frameBorder="0"></iframe>',
                  sep="")
IRdisplay::display_html(pdf_html)}


display_missing_data <- function(df){df[which(rowSums(is.na(df))>=1),]}


yt <- function(short_code_url){
    embed_html <- paste('<iframe src="https://www.youtube.com/embed/',short_code_url,
                                '" align="center" width="900" height="506" frameBorder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>',
                              sep="")
    IRdisplay::display_html(embed_html)
}


open_wd <- function(dir = getwd()){
    if (.Platform['OS.type'] == "windows"){
        shell.exec(dir)
    } else {
        system(paste(Sys.getenv("R_BROWSER"), dir))
    }
}


open_r <- function(){
    shell.exec("D:/HonorFiles/备份文件夹/代码集/Jupyter/胡林辉/R/display_file.R")
}


蓝灯<- function(){
    shell.exec("C:/Users/Jack/AppData/Roaming/Lantern/lantern.exe")
}


OCR_app<- function(){
    shell.exec("D:/Python App/图形化界面应用_OCR.exe")
}
