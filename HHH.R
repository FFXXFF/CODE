library(meta)
library(metafor)
library(readxl) 

m1 <- read_excel("ORR.xlsx")

a <- metabin(
  event.exp, total.exp,
  event.con, total.con,
  data = m1,
  sm = "RR",                
  method = "MH",             
  studlab = study,           
  common = FALSE,            
  random = TRUE             
)

m1 <- read_excel("OS.xlsx")
m1 <- read_excel("PFS.xlsx")

a <- metacont(
  n.e = n.exp,       
  mean.e = mean.exp,     
  sd.e = sd.exp,          
  
  n.c = n.con,       
  mean.c = mean.con,      
  sd.c = sd.con,         
  
  data = m1,             
  studlab = study,       
  sm = "MD",           
  
  common = FALSE,        
  random = TRUE           
)

forest(a)

pdf("forestplot-PFS.pdf", width = 12, height =4)
forest(
  a,
  comb.fixed = FALSE,
  comb.random = TRUE,
  col.random = "#58A0CE",
  col.square = "#58A0CE",
  col.inside = "black",
  col.diamond = "maroon",
  print.tau2 = FALSE,
  print.I2 = TRUE,
  print.Q = FALSE,
  print.pval.Q = TRUE,
  smlab = "Risk Ratio (RR)",
  text.random = "Overall effect (random)",
  
  digits = 2,
  fontsize = 13,
  spacing = 1.1,
  leftlabs = c("Study", "RR [95% CI]"),
  
  label.e = "Synergistic therapy",        
  label.c = "Monotherapy" ,       
  digits.mean = 1,   
  digits.sd = 1,     
)
dev.off()

a$pval.random
summary(a)
