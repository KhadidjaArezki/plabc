# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here
  # class array holding all the pieces and their rotations
  All_My_Pieces = [[[[0, 0], [1, 0], [0, 1], [1, 1]]],  # square (only needs one)
               rotations([[0, 0], [-1, 0], [1, 0], [0, -1]]), # T
               [[[0, 0], [-1, 0], [1, 0], [2, 0]], # long (only needs two)
               [[0, 0], [0, -1], [0, 1], [0, 2]]],
               rotations([[0, 0], [0, -1], [0, 1], [1, 1]]), # L
               rotations([[0, 0], [0, -1], [0, 1], [-1, 1]]), # inverted L
               rotations([[0, 0], [-1, 0], [0, -1], [1, -1]]), # S
               rotations([[0, 0], [1, 0], [0, -1], [-1, -1]]), # Z
               [[[0, 0], [-1, 0], [1, 0], [2, 0], [-2, 0]], # longer (only needs two)
               [[0, 0], [0, -1], [0, 1], [0, 2], [0, -2]]],
               rotations([[0, 0], [1, 0], [0, 1]]), # square-1
               rotations([[0, 0], [1, 0], [0, 1], [1, 1], [-1, 0]])] # square+1

  # your enhancements here
  # class method to choose the next piece
  def self.next_piece (board)
    MyPiece.new(All_My_Pieces.sample, board)
  end

end

class MyBoard < Board
  # your enhancements here
  def initialize (game)
    super(game)
    @current_block = MyPiece.next_piece(self)
    @cheatting = false
  end
  
  def rotate_180_degrees
    if !game_over? and @game.is_running?
      @current_block.move(0, 0, 2)
    end
    draw
  end
  
  # gets the next piece
  def next_piece
    if @cheatting
      @current_block = MyPiece.new([[[0, 0]]], self)
      @cheatting = false
    else
      @current_block = MyPiece.next_piece(self)
    end
    @current_pos = nil
  end
  
  # gets the information from the current piece about where it is and uses this
  # to store the piece on the board itself. Then calls remove_filled.
  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    piece_blocks = locations.size
    # for each of the blocks in piece: get [x, y]coords of block, set on grid the new position of the block after move
    (0..(piece_blocks-1)).each{|index| 
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] = 
      @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end
  
  def cheat_helper
    if !game_over? and @game.is_running?
      if !stored?
        @game.update_score
        draw
        @game.timer.stop
        @game.timer.start(delay, (proc{cheat_helper}))
      else
        drop_cheat_piece
        @game.run_game
      end
    end
  end
  
  def drop_cheat_piece
    if !game_over?
      next_piece
    end
  end
    
  def stored?
    ran = @current_block.drop_by_one
    if !ran
      store_current
      @game.update_score
      draw
      return true
    else
      return false
    end
  end
    
  def cheat
    if !game_over? and @game.is_running?
      if !@cheatting
        if @score >= 100
          @score -= 100
          @cheatting = true
          cheat_helper
        end
      end
    end
  end
end

class MyTetris < Tetris
  # your enhancements here
  # creates a canvas and the board that interacts with it
  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end
  
  def timer
    @timer
  end

  def key_bindings  
    super
    @root.bind('u', proc {@board.rotate_180_degrees})
    @root.bind('c', proc {@board.cheat})
  end  

end


