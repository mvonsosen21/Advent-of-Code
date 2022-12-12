#Advent of Code: Day 1

data <- read.table("/Users/meganvonsosen/Documents/Advent of Code/input.txt", blank.lines.skip=FALSE)

#PROBLEM: Find the Elf carrying the most Calories. How many total Calories is that Elf carrying?

total_cals = c()
sum = 0

for(i in 1:nrow(data)){
  if(is.na(data[i,1]) == FALSE){
    sum = sum + data[i,1]
  }else if(is.na(data[i, 1]) == TRUE){
    total_cals = c(total_cals, sum)
    sum = 0
  }
}

max(total_cals) #68292 cals
which(total_cals == 68292) #61st elf

#Find the top three Elves carrying the most Calories. How many Calories are those Elves carrying in total?
total_cals = sort(total_cals, decreasing = TRUE)
sum(total_cals[1:3])
#203203