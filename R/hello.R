# Hello, world!

#' @export
hello <- function() {
  print('Good luck')
}


updateR <- function() {
  lib <- (.packages())  # 已加载包列表
  if ('mypkg' %in% lib) {detach(package:mypkg)}  #如果mypkg已加载，则卸载
  library(devtools) 
  install_github('hulinhui-code/hulinhui', lib = c('D:/R pkg lib', 'C:/Users/Jack/Documents/R/win-library/4.0'))  # 同时放在C盘（默认安装盘）
#   library(mypkg)   # 安装完成加载 ## 没用，更新完后还是要重新载入
  }

mygithub_install <- function(package_name, github_name){
  lib <- (.packages())  # 已加载包列表
  if (package_name %in% lib) {remove(package_name, lib="D:/R pkg lib")}  #如果已加载，则卸载
  library(devtools)
  install_github(github_name, lib = c('D:/R pkg lib'))
  }


myInstall <- function(package_name) {
  install.packages(package_name, lib="D:/R pkg lib")
  }

myLibrary <- function(package_name) {
  if (length(package_name) ==1) { 
  library(package_name, lib="D:/R pkg lib", character.only=TRUE)
  } else
    for (pkg in package_name){
        library(pkg, lib="D:/R pkg lib", character.only=TRUE)
    } 
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

cal_metrics <- function(label, pred){
    # 根据金标准和预测值的列表计算最佳cutoff及对应的sens和spec
    # label: 金标准，0 1 变量
    # pred: 模型预测值，连续变量
  roc.p=pROC::roc(label, pred)
  if (roc.p$auc>0.5){
    cutoff=roc.p$thresholds[which.max(roc.p$sensitivities+roc.p$specificities)]
    sensitivity=roc.p$sensitivities[which.max(roc.p$sensitivities+roc.p$specificities)]
    specificity=roc.p$specificities[which.max(roc.p$sensitivities+roc.p$specificities)]
    df=data.frame(type='positive classification',
                  auc=round(roc.p$auc,3),cutoff=cutoff,
                  sensitivity=sensitivity,specificity=specificity)
    return(df)
  }
  else{
    cutoff=roc.p$thresholds[which.min(roc.p$sensitivities+roc.p$specificities)]
    sensitivity=roc.p$sensitivities[which.min(roc.p$sensitivities+roc.p$specificities)]
    specificity=roc.p$specificities[which.min(roc.p$sensitivities+roc.p$specificities)]
    df=data.frame(type='negative classification',
                  auc=1-round(roc.p$auc,3),cutoff=cutoff,
                  sensitivity=1-sensitivity,specificity=1-specificity)
    return(df)
  }
}


cal_statistics_sspn<-function(fp, tp, tn, fn, decimal_digit=3){
  # 根据四格表计算sens,spec,ppv,npv, lr及其95%CI
  z=1.95996
  zsq = z**2
  a=fp
  b=tp
  c=tn
  d=fn
  ab=fp+tp
  cd=tn+fn
  ac=fp+tn
  bd=tp+fn
  nn=fp+tp+tn+fn
  
  # 计算prevanlence
  prev = bd/nn 
  ## lower limit of 95%CI of prev #
  p=prev
  n = nn
  q = 1 - p
  num = (2*n*p)+zsq-1-(z*sqrt(zsq-2-(1/n)+4*p*((n*q)+1)))
  denom = 2*(n+zsq)
  prev_l95b = num/denom
  
  
  if (p==0)
    {prev_l95b=0}
  else{p=prev}
  ## upper limit of 95%CI of prev #
  num = (2*n*p)+zsq+1+(z*sqrt(zsq+2-(1/n)+4*p*((n*q)-1)))
  denom = 2*(n+zsq)
  prev_u95b = num/denom
  if (p==1)
    {prev_u95b=1}
  else{p=prev}
  
  # 计算senstivity
  sens = tp/bd
  n = bd
  p = sens
  ## begin l95b #
  q = 1-p
  num = (2*n*p)+zsq-1-(z*sqrt(zsq-2-(1/n)+4*p*((n*q)+1)))
  denom = 2*(n+zsq)
  sens_l95b = num/denom
  if (p==0)
    {sens_l95b = 0}
  else{p = sens}
  ## begin u95b #
  num = (2*n*p)+zsq+1+(z*sqrt(zsq+2-(1/n)+4*p*((n*q)-1)));
  denom = 2*(n+zsq);
  sens_u95b = num/denom;
  if (p==1)
    {sens_u95b = 1}
  else{p = sens}
  #计算Specificity
  spec = tn/ac
  n = ac
  p = spec
  ## begin l95b #
  q = 1-p;
  num = (2*n*p)+zsq-1-(z*sqrt(zsq-2-(1/n)+4*p*((n*q)+1)))
  denom = 2*(n+zsq)
  spec_l95b = num/denom
  if (p==0)
    {spec_l95b = 0}
  else{p = spec}
  
  
  ## begin u95b #
  num = (2*n*p)+zsq+1+(z*sqrt(zsq+2-(1/n)+4*p*((n*q)-1)))
  denom = 2*(n+zsq)
  spec_u95b = num/denom
  if (p==1)
    {spec_u95b = 1}
  else{p = spec}
  #计算阳性率ppos
  ppos = ab/nn
  
  n = nn
  p = ppos
  
  ## begin l95b #
  q = 1-p
  num = (2*n*p)+zsq-1-(z*sqrt(zsq-2-(1/n)+4*p*((n*q)+1)))
  denom = 2*(n+zsq)
  ppos_l95b = num/denom
  if (p==0)
   { ppos_l95b = 0}
  else(p = ppos)
  
  ## begin u95b #
  num = (2*n*p)+zsq+1+(z*sqrt(zsq+2-(1/n)+4*p*((n*q)-1)))
  denom = 2*(n+zsq)
  ppos_u95b = num/denom
  if (p==1)
    {ppos_u95b = 1}
  else(p = ppos)
  #计算阴性率pneg 
  pneg = cd/nn
  n = nn
  p = pneg
  ## begin l95b #
  q = 1-p
  num = (2*n*p)+zsq-1-(z*sqrt(zsq-2-(1/n)+4*p*((n*q)+1)))
  denom = 2*(n+zsq)
  pneg_l95b = num/denom
  if (p==0)
    {pneg_l95b = 0}
  else{p = pneg}
  ## begin u95b #
  num = (2*n*p)+zsq+1+(z*sqrt(zsq+2-(1/n)+4*p*((n*q)-1)))
  denom = 2*(n+zsq)
  pneg_u95b = num/denom
  if (p==1)
    {pneg_u95b = 1}
  else{p = pneg}
  #计算阳性预测值PPV
  ppv = b/ab
  n = ab
  p = ppv
  ##begin l95b
  q = 1-p
  num = (2*n*p)+zsq-1-(z*sqrt(zsq-2-(1/n)+4*p*((n*q)+1)))
  denom = 2*(n+zsq)
  ppv_l95b = num/denom
  if (p==0)
    {ppv_l95b = 0}
  else{p = ppv}
  
  ##begin u95b#
  num = (2*n*p)+zsq+1+(z*sqrt(zsq+2-(1/n)+4*p*((n*q)-1)))
  denom = 2*(n+zsq)
  ppv_u95b = num/denom
  if (p==1)
    {ppv_u95b = 1}
  else{p = ppv}
  #计算阴性预测值NPV
  npv = c/cd
  n = cd
  p = npv
  ##begin l95b
  q = 1-p
  num = (2*n*p)+zsq-1-(z*sqrt(zsq-2-(1/n)+4*p*((n*q)+1)));
  denom = 2*(n+zsq);
  npv_l95b = num/denom;
  if (p==0)
    {npv_l95b = 0}
  else{p = npv}
  ## begin u95b
  num = (2*n*p)+zsq+1+(z*sqrt(zsq+2-(1/n)+4*p*((n*q)-1)));
  denom = 2*(n+zsq);
  npv_u95b = num/denom;
  if (p==1)
    {npv_u95b = 1}
  else{p = npv}
  # 计算阳性似然比  #计算阴性似然比
  pl = sens/(1-spec);
  nl = (1-sens)/spec;   
  xp = sqrt(((1-sens)/b)+(spec/a))
  xn = sqrt((sens)/d)+((1-spec)/c)
  lgpl = log(pl)
  lgnl = log(nl)     
  ## 95%CI
  pl_l95b = exp(lgpl-(1.95996*xp));
  pl_u95b = exp(lgpl+(1.95996*xp));
  
  
  ## 阴性似然比95%CI
  nl_l95b = exp(lgnl-(1.95996*xn));
  nl_u95b = exp(lgnl+(1.95996*xn));
  

  
  statistics_df = data.frame(Metric=c('Prevalance','Senstivity','Specificity','Positive','Negative',
                                        'Positive Predictive Value','Negative Predictive Value',
                                        'Positive likelihood Ratios','Negative likelihood Ratios'), 
    Estimated.value = c(prev,sens,spec,ppos,pneg,ppv,npv,pl,nl),
    Lower.95CI = c(prev_l95b,sens_l95b,spec_l95b,ppos_l95b,
                   pneg_l95b,ppv_l95b,npv_l95b,pl_l95b,nl_l95b),
    Upper.95CI=c(prev_u95b,sens_u95b,spec_u95b,ppos_u95b,
                 pneg_u95b,ppv_u95b,npv_u95b,pl_u95b,nl_u95b)
    )
  options(digits=decimal_digit)
  return(statistics_df)}
