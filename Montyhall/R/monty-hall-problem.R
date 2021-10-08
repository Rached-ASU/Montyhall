#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies.
#'
#' @param ... no arguments are used by the function.
#'
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
}



#' @title
#'   Player selects a door.
#'
#' @description
#'   `select_door()` generates a random door.
#'
#' @details
#'   this function will represent the choice of the participant where he will have equal chances to select a door.
#'   
#'
#' @param ... no arguments are used by the function.
#'
#' @return  The function returns a length of 1 character vector
#'   indicating the selected door
#'
#' @examples
#'  select_door()
#'
#' @export
select_door <- function( )
{
  doors <- c(1,2,3)
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#' Host opens one goat door
#'
#' @description
#'    `open_goat_door()` This function will generate a number referring to a door with one goat behind
#'different from the one previously selected
#'
#' @details
#' The host will make the game with more suspense will choose a door with one goat behind it and
#' will leave the participant with two doors that one contains another goat and the other contain the car
#'  
#'  
#'  
#'  
#'
#' @param ... this function will take two parameters the first parameter game is a vector indicating the positions of goats and the car.
#'  ( game, a.pick )
#'
#' @return The function returns a  numeric vector of length one
#'   the second parameter will define the door previously selected from the participant indicating the opened door by host.
#'
#' @examples
#' open_goat_door(game, a.pick)
#'
#'
#' @export
open_goat_door <- function( game, a.pick )
{
   doors <- c(1,2,3)
   # if contestant selected car,
   # randomly select one of two goats
   if( game[ a.pick ] == "car" )
   {
     goat.doors <- doors[ game != "car" ]
     opened.door <- sample( goat.doors, size=1 )
   }
   if( game[ a.pick ] == "goat" )
   {
     opened.door <- doors[ game != "car" & doors != a.pick ]
   }
   return( opened.door ) # number between 1 and 3
}



#' @title
#' Chance to change doors
#'
#' @description
#'   Asks the user to change or keep the previously selected door
#'  
#'
#' @details
#'  The game will give a last chance to the user to either keep his door or change it and with
#'  this game we will try to get the results of the two scenarios
#'  
#'  
#' 
#'  
#'
#' @param ... a Boolean that can take wither true if the user will change the door or false when the user will keep the original door
#'
#' @return The function returns a length 1 numeric vector indicating the final pick of the participant
#'
#' @examples
#' change_door( stay=T, opened.door, a.pick )
#'
#'
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c(1,2,3)

   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     final.pick <- doors[ doors != opened.door & doors != a.pick ]
   }

   return( final.pick )  # number between 1 and 3
}



#' @title
#' Determine if the participant won
#'
#' @description
#'
#'  The function determines if contestant has won or lost depending on the situation of the final selection
#'   .
#'
#' @details
#' This function will show the user if he won or lost depending on his final pick.
#' If door behind the final pick was a car that the player win and if not the player will lose
#' 
#'
#' @param ... final pick a vector that will indicate the door that was last selected (final.pick,game)
#'
#' @return The function returns a WIN or LOSE
#'
#' @examples
#' determine_winner( final.pick, game )
#'
#'
#' @export
determine_winner <- function( final.pick, game )
{
   if( game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
   if( game[ final.pick ] == "goat" )
   {
      return( "LOSE" )
   }
}





#' @title
#' Play the game one time
#'
#' @description
#' Play_game is a general function that contains all the previous functions
#' of a single game in order and see which strategy wins/loses.
#'
#' @details
#'  The game consist of choosing one door from three doors, one of which has a car
#' behind it and two have goats The contestant selects a door, then the host
#' opens a door to reveal a goat, and then the contestant is
#' given an opportunity to stay with their original selection
#' or switch to the other unopened door. If the last pick was a car the participants
#' considered a winner otherwise a looser
#' 
#' @param ... no arguments are used by the function.
#'
#' @return
#' will return a small table that indicates the winning and losing strategy
#'
#' @examples
#' play_game()
#'
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )

  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}






#' @title
#' Monty Hall game simulation
#'
#' @description
#' Allows user to run the simulation for n times to know the WIN/LOSE probability.
#'
#' @details
#' The function will show the game outcomes if contestant stays or switches
#' and it takes into account the number of times the contestant plays.Results are saved in
#' a data frame showing probabilities of the two game strategies ( stay and switch)
#'
#' @param  ... n which indicates how many times the game is repeated
#'
#' @return
#'  number of times where stay and switch strategies show WIN and LOSE outcomes.
#'
#' @examples
#' play_n_games()
#'
#'
#' @export
play_n_games <- function( n=100 )
{

  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome
    loop.count <- loop.count + 1
  }

  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>%
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>%
  print()

  return( results.df )

}
