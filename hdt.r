################################################################################################################################
# Author: Naveenkumar Ramaraju                                                                                                 #
# Hidden Decision Trees                                                                                                        #
# Date: Feb-16-2017                                                                                                            #
# File version - 1                                                                                                             #
# R - version: 3.3.2                                                                                                           #
################################################################################################################################
ptm <- proc.time()
library(hash)
sum_pv = 0

List = hash()
list_pv = hash()
list_pv_max = hash()
list_pv_min = hash()
list_id = hash()
articleTitle = list()
articlepv = list()
con = file("HDT-data3.txt",open="r")
lines = readLines(con) 
for (line_num in 2:length(lines)){ # excluding first line as it is header
  line = lines[line_num] 
  line = tolower(line)
  aux = strsplit(line,'\t') # Indexes will have: 1 - Title, 2 - URL, 3 - data and 4 - page views 
  url = aux[[1]][2]
  pv = log(1 + as.numeric(aux[[1]][4])) 
  
  if (isTRUE(grep("/blogs/", url) == 1)) 
  {
    type = "BLOG"
  }
  else
  {
    type = "OTHER"
  }
  
  #--- clean article titles, remove stop words
  title = aux[[1]][1]
  title = paste("",title,"", sep=" ") # adding space at the ends
  title = gsub('["]', ' ', title) # replacing special characters with a space to avoid clbbing of words
  title = gsub('[?]', ' ? ', title)
  title = gsub('[:]', ' ', title)
  title = gsub('[.]', ' ', title)
  title = gsub('[(]', ' ', title)
  title = gsub('[)]', ' ', title)
  title = gsub('[,]', ' ', title)
  title = gsub(' a ', ' ', title)
  title = gsub(' the ', ' ', title)
  title = gsub(' for ', ' ', title)
  title = gsub(' in ', ' ', title)
  title = gsub(' and ', ' ', title)
  title = gsub(' or ', ' ', title)
  title = gsub(' is ', ' ', title)
  title = gsub(' in ', ' ', title)
  title = gsub(' are ', ' ', title)
  title = gsub(' of ', ' ', title)
  #title = gsub('  ', ' ', title) # replacing double spaces with single space
  title = trimws(title)
  
  #break down article title into keyword tokens
  aux2 = strsplit(title,' ')
  for (k in 1:length(aux2[[1]]))
  {
    aux2[[1]][k] = gsub(' ', '', aux2[[1]][k])
  }
  aux2 = aux2[[1]][aux2[[1]] != '']
  
  for (word in aux2) 
  {
    word = paste(word, "\t", "N/A", "\t", type)
    
    if(has.key(word, List))
    {
      List[[word]] = List[[word]] + 1
      list_pv[[word]] = list_pv[[word]] + pv
      
      if (pv > list_pv_max[[word]])
      {
        list_pv_max[[word]] = pv
      }
      
      if (pv < list_pv_min[[word]])
      {
        list_pv_min[[word]] = pv
      }
      
      list_id[[word]] = paste(list_id[[word]],'~',line_num-1,sep = "")
    }
    else
    {
      List[[word]] = 1
      list_pv[[word]] = pv
      list_pv_max[[word]] = pv
      list_pv_min[[word]] = pv
      list_id[[word]] = paste('~',line_num-1,sep = "")
    }
  }
  
  if (length(aux2) > 1)
  {
    for (k in 1:(length(aux2) - 1))
    {
      
      word1 = aux2[k]
      word2 = aux2[k+1]
      word = paste(word1, "\t", word2, "\t", type)
      
      if(has.key(word, List))
      {
        List[[word]] = List[[word]] + 1
        list_pv[[word]] = list_pv[[word]] + pv
        
        if (pv > list_pv_max[[word]])
        {
          list_pv_max[[word]] = pv
        }
        
        if (pv < list_pv_min[[word]])
        {
          list_pv_min[[word]] = pv
        }
        
        list_id[[word]] = paste(list_id[[word]],'~',line_num-1,sep = "")
      }
      else
      {
        List[[word]] = 1
        list_pv[[word]] = pv
        list_pv_max[[word]] = pv
        list_pv_min[[word]] = pv
        list_id[[word]] = paste('~',line_num-1,sep = "")
      }
    }
  }  
  
  articleTitle[[line_num-1]] = title
  articlepv[[line_num-1]] = pv
  sum_pv = sum_pv + pv
  
}

nArticles=length(lines) - 1 # -1 as first line is title
close(con)

avg_pv = sum_pv/nArticles
articleFlag = rep("BAD", nArticles)
nidx=0;
nidx_Good=0;
OUT = "hdt-out2.txt"
OUT2 = "hdt-reasons.txt"
f1 = file.create(OUT,overwrite=TRUE) # this is to truncate existing files and suppress warnings
f2 = file.create(OUT2,overwrite=TRUE)
for (idx in keys(List))
{
  n = List[[idx]]
  Avg = list_pv[[idx]]/n
  Min = list_pv_min[[idx]]
  Max = list_pv_max[[idx]]
  idlist = list_id[[idx]]
  nidx =  nidx + 1
  
  if ( ((n > 3) & (n < 8) & (Min > 6.9) & (Avg > 7.6)) | 
       ((n >= 8) & (n < 16) & (Min > 6.7) & (Avg > 7.4)) |
       ((n >= 16) & (n < 200) & (Min > 6.1) & (Avg > 7.2)) ) 
  {
    write(paste(idx, n, Avg, Min, Max, idlist, sep = "\t"), file = OUT,append=TRUE)
    nidx_Good = nidx_Good + 1
    aux = strsplit(idlist,'~')
    aux = aux[[1]][aux[[1]] != '']
    for (ID in aux)
    {
      ID = as.numeric(ID)
      title=articleTitle[[ID]]
      pv=articlepv[[ID]]
      #articleTitle[ID]=title;# this seems redundant
      write(paste(title, pv, idx, n, Avg, Min, Max, sep = "\t"), file = OUT2,append=TRUE)
      articleFlag[ID]="GOOD"; 
    }
  }
}


pv_threshold = 7.1
pv1 = 0
pv2 = 0
n1 = 0
n2 = 0
m1 = 0
m2 = 0
FalsePositive = 0
FalseNegative = 0
aggregationFactor = 0
for (ID in 1:nArticles)
{
  pv = articlepv[[ID]]
  if (articleFlag[ID] == "GOOD")
  {
    n1 = n1 + 1
    pv1 = pv1 + pv
    if (pv < pv_threshold)
    {
      FalsePositive = FalsePositive + 1
    }
  }
  else
  {
    n2 = n2 + 1
    pv2 = pv2 + pv
    if (pv > pv_threshold)
    {
      FalseNegative = FalseNegative + 1
    }
  }
  
  if (pv > pv_threshold)
  {
    m1 = m1 + 1
  }
  else
  {
    m2 = m2 + 1
  }
}



# Printing results

avg_pv1 = pv1/n1
avg_pv2 = pv2/n2
errorRate = (FalsePositive + FalseNegative)/nArticles
aggregationFactor = (nidx/nidx_Good)/(nArticles/n1)
print (paste("Average pv:", avg_pv))
print (paste("Number of articles marked as good: ", n1, " (real number is ", m1,")", sep = "") )
print (paste("Number of articles marked as bad: ", n2, " (real number is ", m2,")", sep = ""))
print (paste("Avg pv: articles marked as good:", avg_pv1))
print (paste("Avg pv: articles marked as bad:",avg_pv2))
print (paste("Number of false positive:",FalsePositive," (bad marked as good)"))
print (paste("Number of false negative:", FalseNegative, " (good marked as bad)"))
print (paste("Number of articles:", nArticles))
print (paste("Error Rate: ", errorRate))
print (paste("Number of feature values: ", nidx, " (marked as good: ", nidx_Good,")", sep = ""))
print (paste("Aggregation factor:", aggregationFactor))

proc.time() - ptm
