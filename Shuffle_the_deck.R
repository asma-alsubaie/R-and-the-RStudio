#Create two functions with the following capabilities:
  
#Shuffle the deck (Function 1).
#Deal 5 cards to the players depending on how many players are in the game.
#Depending on the number of players if the number of cards needed is more than 52 cards, then your function should output that there are not enough cards for everyone before dealing. (Function 2).
#Save the cards each player gets to a variable designated for that player.
#Repeat tasks the 1st task but this time create a function that can deal 5, 7, or 10 cards for each player. 
#Note: The number of cards dealt could be an input for your function.
#Using your shuffle and deal function, create the following if-else statements (You may also need a for-loop to accomplish this task):
#Create a 2-player game that calculates the value of each hand using the "Value" column and create an if-else statement that says if player 1 or player 2 has more points.
#Note: The number of players is always 2 but the number of cards they receive depends on the user input.
#Your output should show if Player 1 or Player 2 has more points by saying, "Player 1 has more points than Player 2. Player 1: <point value here>, Player 2: <point value here>"


deck <- read.csv("https://s3.us-east-1.amazonaws.com/General_V88/boomyeah2015/codingdojo/curriculum/content/chapter/1618705129__deck.csv")
number_of_players <- 5



deal <- function(shuffled_deck, num_players){
  {if (num_players * 5 > nrow(shuffled_deck)) {print('There are not enough cards!')}
    #HERE IS WHERE I WRITE THE CODE TO DEAL THE CARDS!!!
    else {
      all_players <- list()
      for (i in 1:num_players){
        list_of_cards <- shuffled_deck[c(1:5),]
        shuffled_deck <- shuffled_deck[-c(1:5),]
        all_players <- c(all_players, list(list_of_cards))
        
        #(1) Get the top five cards of shuffled_deck and add them to an empty list / vector.
        #(2) Remove those same five cards from the beginning of the suffled_deck list / vector.
        #(3) Add the list / vector containing the five cards (the one from Step (1)) to the all_players list / vector.
      }
      return(all_players)}}
  #(4) Return the all_players list of lists.
}



all_players <- deal(deck, number_of_players)
all_players
values <- list()
for (i in 1:number_of_players){
  values <- c(values, sum(all_players[[i]][, 'value']))
  
  print(all_players[[i]])
  print(sum(all_players[[i]][, 'value']))
}
print(values)




#I'm going to choose to number the playing cards so that 1 is the Ace of Clubs, 13 is the King of Clubs, 14 is the Ace of Hearts, 26 is the King of Hearts, . . . .
get_points <- function(player){
  counter <- 0 
  for (i in player){
    {if (i %% 13 == 0) {counter <- counter + 13}
      else {counter <- counter + (i %% 13)}}
  }
  print(counter)
}


get_points(c(8,7,6,5,4))
#get_points(c(13,12,11,10,9))


player1 <- as.numeric(unlist(values[[1]][1]))
player1
typeof(player1)
player2 <- as.numeric(unlist(values[[2]][1]))
player2
typeof(player2)


#if statement to find who is the winner
if (player1>player2){
  paste("Player 1 has more points than Player 2. Player 1:",player1," Player 2:", player2 )
}


