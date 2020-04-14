prs <- function(input_list) {
 
if (!is.character(input_list)  || length(input_list) < 2) {
  stop('Only length > 1 character vectors permitted')
 
}

players <- character()
for (nm in input_list) {
  players <- toupper(c(players, nm))
}

game_choice <- sample(c('paper', 'rock', 'scissors'), length(input_list), replace = T)
contestant <- data.frame(players, game_choice )

attach(contestant)

if (game_choice[1] == 'paper' && game_choice[2] == 'paper' ||
    game_choice[1] == 'rock' && game_choice[2] == 'rock' ||
    game_choice[1] == 'scissors' && game_choice[2] == 'scissors')
 
{ decision <- 'Tie, play again.' }

else if (game_choice[1] == 'paper' && game_choice[2] == 'rock' ||
         game_choice[1] == 'rock' && game_choice[2] == 'scissors' ||
         game_choice[1] == 'scissors' && game_choice[2] == 'paper')
   
{ decision  <- paste0(players[1], ' is the winner!') }
         
else if (game_choice[2] == 'paper' && game_choice[1] == 'rock' ||
         game_choice[2] == 'rock' && game_choice[1] == 'scissors' ||
         game_choice[2] == 'scissors' && game_choice[1] == 'paper')
 
{ decision  <- paste0(players[2], ' is the winner!') }

cat('\n',
    players[1], 'has', game_choice[1], '\n',
    players[2], 'has', game_choice[2], '\n',
    '\n', decision)

detach(contestant)
         
}

player1 <- 'mario'
player2 <- 'luigi'

prs(c(player1,  player2))
