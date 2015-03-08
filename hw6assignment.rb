# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece

  All_My_Pieces = All_Pieces + [rotations([[0,0],[1,0],[0,1],[1,1],[2,1]]),
  [[[0,0],[1,0],[2,0],[3,0],[4,0]],#|_|_|_|_|_| only needs two
  [[0,0],[0,1],[0,2],[0,3],[0,4]]],
  rotations([[0,0],[0,1],[1,1]])]

  # MyBoard uses MyPiece.next_piece(), so replacement of class name to "MyPiece" is
  # not enough. We should provide MyPiece's own next_piece function. otherwise it would
  # call Piece's next_piece() method.

   def self.next_piece(board)
      if board.cheatting
          MyPiece.new([[[0,0]]], board)
      else
          MyPiece.new(All_My_Pieces.sample, board)
      end
   end

end

class MyBoard < Board
    def initialize (game)
        @grid = Array.new(num_rows) {Array.new(num_columns)}
        @current_block = MyPiece.next_piece(self)
        @score = 0
        @game = game
        @delay = 500
        @isCheat = false
    end
    
    def next_piece
        @current_block = MyPiece.next_piece(self)
        @isCheat = false
        @current_pos = nil
    end
    
    def store_current
        locations = @current_block.current_rotation
        displacement = @current_block.position
        (0..locations.length-1).each{|index|
            current = locations[index];
            @grid[current[1]+displacement[1]][current[0]+displacement[0]] =
            @current_pos[index]
        }
        remove_filled
        @delay = [@delay - 2, 80].max
    end
    
    def cheatting
        @isCheat
    end

    def cheat
        if  !cheatting && (@score >= 100)
            @isCheat = true
            @score = @score - 100
        end
    end
    
end

class MyTetris < Tetris

  def set_board
      @canvas = TetrisCanvas.new
      @board = MyBoard.new(self)
      @canvas.place(@board.block_size * @board.num_rows + 3,
                    @board.block_size * @board.num_columns + 6, 24, 80)
      @board.draw
  end
  
   def key_bindings
       super()
       @root.bind('c', proc {@board.cheat})
       @root.bind('u', proc {@board.rotate_counter_clockwise; @board.rotate_counter_clockwise})
  end
end
