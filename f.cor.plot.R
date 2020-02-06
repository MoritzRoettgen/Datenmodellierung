cor.plot<-function(m,method="pearson"){
  source("panelutils2.R")
  pairs(m, lower.panel=panel.smooth, upper.panel=panel.cor, diag.panel=panel.hist)
}
