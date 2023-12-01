## Solution template for Guess The Word practice problem (section 7)

require_relative './section-7-provided'

class ExtendedGuessTheWordGame < GuessTheWordGame
  ## YOUR CODE HERE
  attr_accessor :guesses
  
  def initialize secret_word_class
    super(secret_word_class)
    self.guesses = []
  end
  
  def ask_for_guessed_letter
    puts "Secret word:"
    puts @secret_word.pattern
    puts @mistakes_allowed.to_s + " incorrect guess(es) left."
    puts "Enter the letter you want uncovered:"
    letter = gets.chomp
    if @secret_word.valid_guess? letter
      if self.guesses.include? letter
        puts "You have already made this guess, try again."
      # player guessed wrong
      elsif !@secret_word.guess_letter! letter
        @mistakes_allowed -= 1
        @game_over = @mistakes_allowed == 0
      # player guessed right
      else
        @game_over = @secret_word.is_solved?
      end
      self.guesses.push letter
    else
      puts "I'm sorry, but that's not a valid letter."
    end
  end
end

class ExtendedSecretWord < SecretWord
  ## YOUR CODE HERE
  attr_accessor :word, :pattern

  def initialize word
    @punctuation_regex = /[[:punct:]]/
    @spaces_regex = /\s/
    self.word = word
    self.pattern = make_pattern
  end
  
  def make_pattern
    pattern = ""
    self.word.each_char {|char|
      if @punctuation_regex.match(char) or @spaces_regex.match(char)
        pattern+=char
      else
        pattern+="-"
      end
    }
    return pattern
  end
  
  def valid_guess? guess
    guess.length == 1 and !(@punctuation_regex.match(guess) or @spaces_regex.match(guess))
  end
  
  # Returns the index of letter in self.word if found
  def guess_letter! letter
    found = self.word.downcase.index(letter.downcase)
    if found
      start = 0
      # Find all occurences of letter in self.word
      while ix = self.word.downcase.index(letter.downcase, start) # position of letter in self.word starting from a certain index
        self.pattern[ix] = self.word[ix] # replace dash in same position in pattern with letter
        start = ix + 1 # start search from after position of letter in next iteration
      end
    end
    found
  end
end


## Run game
## Change to `false` to run the original game
if true
  ExtendedGuessTheWordGame.new(ExtendedSecretWord).play
else
  GuessTheWordGame.new(SecretWord).play
end

