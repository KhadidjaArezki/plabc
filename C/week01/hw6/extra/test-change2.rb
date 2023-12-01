word = "The cow says MOO!"
letter = "T"
pattern = "---"

def guess_letter!(word, letter, pattern)
  found = word.downcase.index(letter.downcase)
  puts found
  if found
    puts ("found #{letter} at index:  #{found}")
    start = 0
    # Find all occurences of letter in self.word
    while ix = word.downcase.index(letter.downcase, start) # position of letter in self.word starting from a certain index
      puts ("found #{letter} again at index:  #{ix}")
      pattern[ix] = word[ix] # replace dash in same position in pattern with letter
      start = ix + 1 # start search from after position of letter in next iteration
    end
  end
end

guess_letter!(word,letter,pattern)
