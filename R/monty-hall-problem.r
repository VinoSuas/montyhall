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
#'   Select Door
#'
#' @description
#'   `select_door()` represents the door that the contestant
#'   has chosen as their initial pick
#'
#' @details
#'   This returns the initial pick of contestant.
#'
#' @param ... no arguments are used by the function.
#'
#' @return
#'   The function returns a numeric value between 1 and 3,
#'   representing the initial pick.
#'
#' @examples
#'   select_door()
#'
#' @export
select_door <- function( )
{
  doors <- c(1,2,3)
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#'   Open Goat Door.
#'
#' @description
#'   `open_goat_door()` reveals a unopened door with a goat
#'   behind it.
#'
#' @details
#'   The removes the newly opened door, as one of the choices for the
#'   contestant.
#'
#' @param
#'   game is the 3 character vector from `create_game()`
#'   and a.pick is the single numeric output of `select_door()`
#'
#' @return
#'   The single numeric value is returned, which represents
#'   the opened door of the goat.
#'
#' @examples
#'   this.game <- create_game()
#'   this.game
#'   my.initial.pick <- select_door()
#'   my.initial.pick
#'   open_goat_door( this.game, my.initial.pick )
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
#'   Change Door
#'
#' @description
#'   `change_door()` allows the contestant to change their pick
#'   from their initial pick to a different unopened door if they
#'   choose.
#'
#' @details
#'   This finalizes the door choice of the contestant.
#'
#' @param
#'   Stay is a Boolean value for whether the contestant decides
#'   to stay or not. Open.door is a numeric value between 1 and 3,
#'   which represents which goat door was opened. a.pick is a single
#'   numeric value between 1 and 3 which represents the initial
#'   contestant pick.
#'
#' @return
#'  The function returns final.pick which is nua numeric value
#'  between 1 and 3 representing the final choice of the
#'  contestant.
#'
#' @examples
#'   opened.door <- open_goat_door( this.game, my.initial.pick )
#'   change_door(stay=T,opened.door=opened.door,a.pick=my.initial.pick )
#'   change_door(stay=F,opened.door=opened.door,a.pick=my.initial.pick)
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
#'   Decides whether the contestant has won or lost.
#'
#' @description
#'   `determine_winner()` checks if the final pick was a car
#'   or a goat.
#'
#' @details
#'   The contestant is given a "WIN" outcome if final pick is
#'   a car. The contestant is given a "LOSE" outcome if final pick is
#'   a goat.
#'
#' @param
#'   final.pick is the numeric value from the output of `change_door()`
#'   game is the 3 character vector from `create_game()`
#'
#' @return
#'  The function returns a string outcome "WIN" or "LOSE"
#'
#' @examples
#'   my.final.pick <- change_door(stay=F, opened.door=opened.door,
#'   a.pick=my.initial.pick )
#'   determine_winner( final.pick=my.final.pick, game=this.game )
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
#'    Monty Hall Game
#'
#' @description
#'   `play_game()` runs one full Monty Hall game.
#'
#' @details
#'   A game is created and the outcomes for staying and switching are
#'   determined for one initial pick.
#'
#' @param ... no arguments are used by the function.
#'
#' @return
#'   A table showing the return values, or outcomes, for staying and
#'   switching, given the inital pick.
#'
#' @examples
#'   play_game()
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
#'   Play Monty Hall n times.
#'
#' @description
#'   This runs the Monty Hall game simulation n times.
#'
#' @details
#'   The game results would be similar to the theoretical values
#'   if the number of games  increases. This functions allows for that.
#'
#' @param
#'   An integer value n represents the number of times the game will be
#'   played.
#'
#' @return
#'   A table of proportions would be displayed with a win rate of staying
#'   and switching.
#'
#' @examples
#'   play_n_games(100)
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
