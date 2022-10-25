

library(rstatix) # rstatix version: 0.4.0


df=read.csv("mdd_dvg_123_ge3_d2.csv")
cat("----------Eye close by wilcox test----------------------\n")
df1=df[df$eye=='C',]
p_values=rep(0,19)
for (i in 3:21){
  myx=wilcox.test(df1[,i]~type,data=df1)
  p_values[i-2]=myx$p.value
  cat(i,"\t", p_values[i-2], "\n" )
}

cat("----------Eye Open by wilcox test----------------------\n")

df1=df[df$eye=='O',]
p_values=rep(0,19)
for (i in 3:21){
  myx=wilcox.test(df1[,i]~type,data=df1)
  p_values[i-2]=myx$p.value
  cat(i,"\t", p_values[i-2], "\n" )
}



cat("#--------------------Eye closed by p.adjust---------------------------\n")
df1=df[df$eye=='C',]
for (i in 3:21){
  #myx=t.test(df[,i]~df$c1.type)
  df1$mydata=df1[,i]
  myx <- df1 %>%
    wilcox_test(mydata ~ type) %>%
    adjust_pvalue(method = "BH") %>%
    add_significance("p.adj")
    p_values[i-2]=myx$p
  cat(i,"\t", p_values[i-2], "\n" )
}

cat("#--------------------Eye Open by p.adjust---------------------------\n")

df1=df[df$eye=='O',]
for (i in 3:21){
  #myx=t.test(df[,i]~df$c1.type)
  df1$mydata=df1[,i]
  myx <- df1 %>%
    wilcox_test(mydata ~ type) %>%
    adjust_pvalue(method = "BH") %>%
    add_significance("p.adj")
  p_values[i-2]=myx$p
  cat(i,"\t", p_values[i-2], "\n" )
}
