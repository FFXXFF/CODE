library(meta)
library(metafor)
library(readxl) 

m1 <- read_excel("ORR.xlsx")

a <- transform(m1, darcsin = 0.5 * (asin(sqrt(Case / (Sample.size + 1))) + 
                                     asin(sqrt((Case + 1) / (Sample.size + 1)))))

meta1 <- metaprop(Case, Sample.size,
                  data = a,
                  studlab = paste(Study),
                  sm = "PFT",
                  incr = 0.5,
                  method.incr = "all",
                  pscale = 100,
                  common = FALSE,
                  subgroup = Therapy)

pdf("forestplot.pdf", width = 16, height = 8)
forest(meta1,
       sortvar = Article.number,
       xlim = c(0, 100),pscale = 100,  
       col.square = "#58A0CE",
       col.diamond="maroon", col.diamond.lines="maroon",
       col.inside = "white",
       rightcols = c("effect", "ci"),
       rightlabs = c("PSA response (%)", "95% CI"),
       leftcols = c("studlab","Phase","Therapy","Sample.size"),
       leftlabs = c("Study","Phase","Therapy","Patients"),
       widths = c(4, 2, 4), 
       just.addcols.left = "left",
       digits.I2 = 1,
       digits.pval.Q = 2,
       print.tau2 = FALSE)
dev.off()

metainf_res <- metainf(meta1, pooled = "random")

pdf("Sensitivity.pdf", width = 16, height = 8)
forest(metainf_res,
       comb.random = TRUE,
       xlim = c(0, 70),
       col.square = "#58A0CE",
       col.diamond = "springgreen4",
       col.diamond.lines = "white",
       rightcols = c("effect", "ci"),
       rightlabs = c("Incidence (%)", "95% CI"))
dev.off()

funnel(meta1)
funnel(meta1,
       atransf = transf.iarcsin,
       yaxis = "sei",
       xlab = "Proportion",
       ylab = "Standard Error",
       digits = 4,
       level = c(90, 95, 99),
       shade = c("white", "grey", "lightblue3"),
       legend = FALSE,
       cex.lab = 1.5) 
dev.off()

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
