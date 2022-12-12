# Day 2

data <- read.table("/Users/meganvonsosen/Documents/Advent of Code/day2/input.txt")
colnames(data)[1] ="opponent"
colnames(data)[2] ="you"

#Opponent: A for rock, B for paper, and C for scissors
#You: X for rock, Y for paper, and Z for scissors

#The score for a single round is the score for the shape you selected (1 for Rock, 2 for Paper, 
#and 3 for Scissors) plus the score for the outcome of the round (0 if you lost, 3 if the round 
#was a draw, and 6 if you won).

#PROBLEM: What would your total score be if everything goes exactly according to your strategy guide?

#tie: a and x, b and y, c and z
#opponent wins: a and z, b and x, c and y
#you win: a and y, b and z, c and x

scores = c()
for(i in 1:nrow(data)){
  #reset the score to 0
  score = 0
  #check what points should be added according to shape
  if(data[i,2] == "X"){
    score = score + 1
  } else if(data[i,2] == "Y"){
    score = score + 2
  } else if(data[i,2] == "Z"){
    score = score + 3
  }
  #check the outcome
  if((data[i,1] == "A" & data[i,2] == "X") | (data[i,1] == "B" & data[i,2] == "Y") | (data[i,1] == "C" & data[i,2] == "Z")){
    #tie
    score = score + 3
    scores = c(scores, score)
  } else if((data[i,1] == "A" & data[i,2] == "Z") | (data[i,1] == "B" & data[i,2] == "X") | (data[i,1] == "C" & data[i,2] == "Y")){
    #opponent wins
    score = score + 0
    scores = c(scores, score)
  } else if((data[i,1] == "A" & data[i,2] == "Y") | (data[i,1] == "B" & data[i,2] == "Z") | (data[i,1] == "C" & data[i,2] == "X")){
    #you win
    score = score + 6
    scores = c(scores, score)
  }
}

#total score
sum(scores)
#13052

#Part 2

#PROBLEM: Following the Elf's instructions for the second column, what would your total score be if everything 
#goes exactly according to your strategy guide?

#Second column:
#X means you need to lose, Y means you need to end the round in a draw, and Z means you need to win

scores = c()
for( i in 1:nrow(data) ){
  
  #initialize score for the round
  score = 0
  
  if( data[i,2] == "X" ){ #Need to end in a loss
    #add 0 to score for a loss
    if ( data[i,1] == "A") { #need to play a Z to lose, so add 3
      score = score + 3
    } else if (data[i,1] == "B"){ #Need to play an X to lose, so add 1
      score = score + 1
    } else if (data[i,1] == "C"){ #Need to play a Y to lose, so add 2
      score = score + 2
    }
    scores = c(scores, score)
  } else if ( data[i,2] == "Y" ){ #Need to end in a draw
    #add 3 to score for a draw
    score = score + 3
    if ( data[i,1] == "A") { #need to play an X to tie, so add 1
      score = score + 1
    } else if (data[i,1] == "B"){ #Need to play a Y to tie, so add 2
      score = score + 2
    } else if (data[i,1] == "C"){ #Need to play a Z to tie, so add 3
      score = score + 3
    }
    scores = c(scores, score)
  } else if( data[i,2] == "Z" ){ #Need to end in a win
    #add 6 to score for a win
    score = score + 6
    if ( data[i,1] == "A") { #need to play an Y to win, so add 2
      score = score + 2
    } else if (data[i,1] == "B"){ #Need to play a Z to tie, so add 3
      score = score + 3
    } else if (data[i,1] == "C"){ #Need to play an X to tie, so add 1
      score = score + 1
    }
    scores = c(scores, score)
  }
}

sum(scores)
#13693



