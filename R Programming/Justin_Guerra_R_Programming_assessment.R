# loading in libraries (please download from your end)
library(magrittr)

coordinates = c()
total_row_col = c("1", "2", "3")

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
      cat("Invalid input. Please try again. \n\n") # Error message
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

    cat("Which row? ")
    n_row <- readLines(con = con, n = 1) # Ask the user for the row coordinate of the board
    
    cat("Which column? ")
    n_col <- readLines(con = con, n = 1) # Ask the user for the column coordinates of the board

    if (!(n_col %in% total_row_col) | !(n_row %in% total_row_col)) { # Detecting false input

      cat("Invalid input. Please try again.\n\n")
      
    } else {
      
      coordinates[1] <- n_row %>%
        as.integer()
      
      coordinates[2] <- n_col %>%
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

# Function for the antidiagonal
antidiag <- function(x, offset = 0L) {
  x[col(x) + row(x) - ncol(x) - 1L == offset]
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

  if (all(antidiag(board) == player)) {
    return(TRUE)
  } # 2nd diagonal

  return(FALSE)
}

# play_game function starts the game of tic-tac-toe!

play_game <- function() {
  
  cat("\nWelcome to the Tic-Tac-Toe Game!\n")
  
  # Initialize board and ask the user for input whether they want to be X or O
  board <- starting_board()
  player <- player_X_O()

  # Computer will take the other letter depending what the user inputs
  computer <- ifelse(test = player == "X", "O", "X")
  
  
  while (TRUE) {
    if (player == "O") { # User starts second for O

      move <- computer_move(board = board) # computer moves first as X
      board[move[1], move[2]] <- computer # computer coordinates

      if (win_condition(board = board, player = computer) == TRUE) { # win condition

        cat("\nCurrent Board:\n")
        cat("---------------------\n")
        print(board)
        cat("---------------------\n")
        cat(paste0("Computer Wins!\n"))
        break
      }


      if (!"" %in% board) { # draw condition if there are no moves for the user or computer to make

        cat("\nCurrent Board:\n")
        cat("---------------------\n")
        print(board)
        cat("---------------------\n")
        cat("It's a draw!\n")
        break
      }

      cat("\nCurrent Board:\n")
      cat("---------------------\n")
      print(board)
      cat("---------------------\n")
      move <- player_move(board = board)
      board[move[1], move[2]] <- player

      if (win_condition(board = board, player = player) == TRUE) {
        cat("\nCurrent Board:\n")
        cat("---------------------\n")
        print(board)
        cat("---------------------\n")
        cat(paste0("User Wins!\n"))
        break
      }



      if (!"" %in% board) {
        cat("\nCurrent Board:\n")
        cat("---------------------\n")
        print(board)
        cat("---------------------\n")
        cat("It's a draw!\n")
        break
      }
    } else { # User starts first for X

      cat("\nCurrent Board:\n")
      cat("---------------------\n")
      print(board)
      cat("---------------------\n")
      move <- player_move(board = board)
      board[move[1], move[2]] <- player

      if (win_condition(board = board, player = player) == TRUE) {
        cat("\nCurrent Board:\n")
        cat("---------------------\n")
        print(board)
        cat("---------------------\n")
        cat(paste0("User Wins!\n"))
        break
      }



      if (!"" %in% board) {
        cat("\nCurrent Board:\n")
        cat("---------------------\n")
        print(board)
        cat("---------------------\n")
        cat("It's a draw!\n")
        break
      }

      move <- computer_move(board = board)
      board[move[1], move[2]] <- computer

      if (win_condition(board = board, player = computer) == TRUE) {
        cat("\nCurrent Board:\n")
        cat("---------------------\n")
        print(board)
        cat("---------------------\n")
        cat(paste0("Computer Wins!\n"))
        break
      }


      if (!"" %in% board) {
        cat("\nCurrent Board:\n")
        cat("---------------------\n")
        print(board)
        cat("---------------------\n")
        cat("It's a draw!\n")
        break
      }
    }
  }
}

play_game()
