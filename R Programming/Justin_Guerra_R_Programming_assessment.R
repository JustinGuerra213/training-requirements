# loading in libraries (please download from your end)
library(lava)
library(stringr)

# Randomizer for the computer thinking time
set.seed(1)


# starting_board function basically prints out a empty matrix in order to start
# the game!

starting_board <- function() {
  board <- matrix("", # Get board matrix
    nrow = 3,
    ncol = 3
  )

  return(board)
}

# Player X_O function allows the user to pick if they want to be "X" or "O".
# "X" goes first, "O" goes second.s

player_X_O <- function(response) {
  repeat{ # Repeat after computer's move

    if (interactive()) { # Establishing a connection with the terminal

      con <- stdin()
    } else {
      con <- "stdin"
    }

    cat("X or O? ") # Asking the user to be X or O
    response <- readLines(con = con, n = 1)

    if (response %in% c("X", "O")) {
      return(response)
    } else {
      cat("Invalid input. Please try again. \n") # Error message
    }
  }
}

# player_move function asks the user for the coordinate values to make a move in the game based on this format  e.g. "1,2".
# If the user inputs information not at this format, the console will ask the user to try again. Same goes for coordinates that
# are not availible in the board.

player_move <- function(board) {
  repeat{
    if (interactive()) { # Establishing a connection with the terminal

      con <- stdin()
    } else {
      con <- "stdin"
    }

    cat("Please enter coordinates for the board (row and column, comma separated) e.g 1,2: ")
    response <- readLines(con = con, n = 1) # Ask the user for the coordinates of the board

    if (str_detect(response, pattern = "[1-3],[1-3]") == FALSE) { # Detecting false input

      cat("Invalid input. Please try again.\n\n")
    } else {
      coordinates <- strsplit(response, split = ",") %>% # Split the response into two interger values
        unlist() %>%
        as.integer()

      if (board[coordinates[1], coordinates[2]] == "") { # Return coordinates

        return(coordinates)
      } else {
        cat("Invalid move. Please try again.\n\n") # repeat by asking the user again
      }
    }
  }
}

# computer_move function makes a random move to play tic-tac-toe. If there are no available spaces in the board
# the computer will stop making a move.

computer_move <- function(board) {
  repeat{ # Repeat after each move

    avail_coor <- which(board == "", arr.ind = TRUE) # getting the available coordinates in the current board

    if (avail_coor %>% nrow() >= 1) {
      random_coor_row <- sample(1:nrow(avail_coor), 1) # Random row with coordinates (row, column)

      computer_coor <- c(
        avail_coor[random_coor_row, ][1], # Row Values
        avail_coor[random_coor_row, ][2]
      ) # Column values


      return(computer_coor) # Return values
    } else {
      break # Stop after there are no available moves
    }
  }
}

# random_time_interval is the function to "mimic" the computer to be thinking.
random_time_interval <- function() {
  sec <- sample(1:10, 1) # Emulate thinking time for the computer

  return(sec)
}


# win_condition function are the different possibilities for the user or the
# computer to win in the tic-tac-toe game. Vertical, horizontal, and diagonals
# are the winning conditions.

win_condition <- function(board, player) {
  # Check for win condition for all rows
  for (i in 1:3) {
    if (all(board[i, ] == player)) {
      return(TRUE)
    }
  }




  # Check for win condition for all columns
  for (i in 1:3) {
    if (all(board[, i] == player)) {
      return(TRUE)
    }
  }


  # Check both diagonals
  if (all(diag(board) == player)) {
    return(TRUE)
  } # 1st diagonal

  if (all(revdiag(board) == player)) {
    return(TRUE)
  } # 2nd diagonal

  return(FALSE)
}

# play_game function starts the game of tic-tac-toe!

play_game <- function() {
  # Thinking time for the computer
  computer_time <- random_time_interval()

  # Initialize board and ask the user for input whether they want to be X or O
  board <- starting_board()
  player <- player_X_O()

  # Computer will take the other letter depending what the user inputs
  computer <- ifelse(test = player == "X", "O", "X")



  while (TRUE) {
    if (player == "O") { # User starts second for O

      Sys.sleep(time = computer_time) # emulate the computer thinking

      move <- computer_move(board = board) # computer moves first as X
      board[move[1], move[2]] <- computer # computer coordinates

      if (win_condition(board = board, player = computer) == TRUE) { # win condition

        print(board)
        cat(paste0("Computer (", player, ") Wins!"))
        break
      }


      if (!"" %in% board) { # draw condition if there are no moves for the user or computer to make

        print(board)
        cat("It's a draw!")
        break
      }


      print(board)
      move <- player_move(board = board)
      board[move[1], move[2]] <- player

      if (win_condition(board = board, player = player) == TRUE) {
        print(board)
        cat(paste0("User (", player, ") Wins!"))
        break
      }



      if (!"" %in% board) {
        print(board)
        cat("It's a draw!")
        break
      }
    } else { # User starts first for X


      print(board)
      move <- player_move(board = board)
      board[move[1], move[2]] <- player

      if (win_condition(board = board, player = player) == TRUE) {
        print(board)
        cat(paste0("User (", player, ") Wins!"))
        break
      }



      if (!"" %in% board) {
        print(board)
        cat("It's a draw!")
        break
      }

      Sys.sleep(time = computer_time) # emulate the computer thinking

      move <- computer_move(board = board)
      board[move[1], move[2]] <- computer

      if (win_condition(board = board, player = computer) == TRUE) {
        print(board)
        cat(paste0("Computer (", computer, ") Wins!"))
        break
      }


      if (!"" %in% board) {
        print(board)
        cat("It's a draw!")
        break
      }
    }
  }
}

play_game()
