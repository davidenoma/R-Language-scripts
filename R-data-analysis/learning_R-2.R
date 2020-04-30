seq1 = seq(4,11,length.out = 10)

class(seq1)

index = seq1 > 9

#The code below prints objects that are true
#at equivalent indices and not the others. 

seq1[index]

#The one below prints only indexed which 
#satisfy the conditional.
which(seq1 > 9)

index2 <- seq1 > 5 | seq1 < 9

seq1_round = round(seq1)

char1 = as.character(seq1_round)

"12" %in% char1

#this finds the indexes that have a 6 char

index3 <- which("6" == char1)

