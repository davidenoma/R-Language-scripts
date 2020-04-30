library(datasets)
print(women)

write.table(x=women, file = 'women_comma.csv', sep=',')
csv(x=women, file = 'women_comma.csv', sep=',',row.names = F)
