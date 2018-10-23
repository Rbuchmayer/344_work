# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

# subclass of Piece containing three new pieces
class MyPiece < Piece

  # class array with the additional three pieces and their rotations
  All_My_Pieces = [[[[0, 0], [1, 0], [0, 1], [1, 1]]],  
                   rotations([[0, 0], [-1, 0], [1, 0], [0, -1]]), 
                   [[[0, 0], [-1, 0], [1, 0], [2, 0]],
                    [[0, 0], [0, -1], [0, 1], [0, 2]]],
                   rotations([[0, 0], [0, -1], [0, 1], [1, 1]]), 
                   rotations([[0, 0], [0, -1], [0, 1], [-1, 1]]),
                   rotations([[0, 0], [-1, 0], [0, -1], [1, -1]]), 
                   rotations([[0, 0], [1, 0], [0, -1], [-1, -1]]), 
                   rotations([[0, 0], [0, 1], [1, 1], [1, 0], [2, 0]]), 
                   rotations([[0, 0], [1, 0], [-1, 0], [2, 0], [-2, 0]]),
                   rotations([[0, 0], [0, 1], [1, 0]])]

  # class method that returns a new cheat piece
  def self.cheat_piece(board)
    MyPiece.new([[0, 0]], board)
  end

  # class method to choose the next piece, including the three additions 
  def self.next_piece(board)
    MyPiece.new(All_My_Pieces.sample, board)
  end
end

# subclass of Board that allows cheating and 180 degree rotation
class MyBoard < Board

  def initialize (game)
    super
    @cheating = false
    @current_block = MyPiece.next_piece(self)
  end

  # returns if the user is currently cheating
  def isCheating
    @cheating
  end

  # rotates current block 180 degrees
  def rotate_180
    if !game_over? and @game.is_running?
      @current_block.move(0, 0, 1)
      @current_block.move(0, 0, 1)
    end
    draw
  end

  # updates the score and cheating field if the user
  # is qualified to cheat
  def cheat
    if !isCheating and score >= 100
      @score -= 100
      @cheating = true
    end

    # choses the next piece depending on if the user is cheating
    def next_piece
      if isCheating
        @cheating = false
        @current_block = MyPiece.cheat_piece(self)
      elsif 
        @current_block = MyPiece.next_piece(self)
      end
      @current_pos = nil
    end
  end

  # stores the current piece of any length on the board
  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    high = locations.length-1
    (0..high).each{|index| 
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] = 
      @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end
end

# subclass of Tetris that sets up the game and adds
# the 'u' and 'c' keys for rotating 180 degrees and cheating
class MyTetris < Tetris

  # creates a canvas and the MyBoard that interacts with it
  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end

  # binds the 'u' and 'c' keys to rotate 180 and cheat
  def key_bindings
    super
    @root.bind('c', proc {@board.cheat}) 
    @root.bind('u', proc {@board.rotate_180})
  end
end

