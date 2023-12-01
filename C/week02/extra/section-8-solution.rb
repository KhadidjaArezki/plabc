require_relative './section-8-provided'

class Character
  def initialize hp
    @hp = hp
  end

  def resolve_encounter enc
    if !is_dead?
      play_out_encounter enc
    end
  end

  def is_dead?
    @hp <= 0
  end

  private

  def play_out_encounter enc
    ## YOUR CODE HERE
    self.play enc
  end
end

class Knight < Character
  attr_accessor :hp, :ap
  
  def initialize(hp, ap)
    super hp
    @ap = ap
  end

  def to_s
    "HP: " + hp.to_s + " AP: " + ap.to_s
  end

  ## YOUR CODE HERE
  # code damage_knight
  def damage_knight dam
    if @ap == 0
      hp = @hp - dam
      ap = 0
      return [hp, 0]
    else if dam > @ap
        self.damage_knight(dam - @ap)
        ap = 0
      else ap = @ap -dam
        return [hp, ap]
      end
    end
  end
  def play enc
    enc.playing_as_knight self
  end
end

class Wizard < Character
  attr_accessor :hp, :mp
  
  def initialize(hp, mp)
    super hp
    @mp = mp
  end

  def to_s
    "HP: " + hp.to_s + " MP: " + mp.to_s
  end

  ## YOUR CODE HERE
  # Override is_dead?
  def is_dead?
    hp <= 0 || mp < 0
  end
  def play enc
    enc.playing_as_wizard self
  end
end

class FloorTrap < Encounter
  attr_reader :dam

  def initialize dam
    @dam = dam
  end

  def to_s
    "A deadly floor trap dealing " + @dam.to_s + " point(s) of damage lies ahead!"
  end

  ## YOUR CODE HERE
  def playing_as_knight knight
    new_knight_state = knight.damage_knight @dam
    knight.hp = new_knight_state[0]
    knight.ap = new_knight_state[1]
  end
  def playing_as_wizard wizard
    if wizard.mp > 0
      wizard.mp = wizard.mp - 1
    else
      wizard.hp = wizard.hp - @dam
    end
  end
end

class Monster < Encounter
  attr_reader :dam, :hp

  def initialize(dam, hp)
    @dam = dam
    @hp = hp
  end

  def to_s
    "A horrible monster lurks in the shadows ahead. It can attack for " +
        @dam.to_s + " point(s) of damage and has " +
        @hp.to_s + " hitpoint(s)."
  end

  ## YOUR CODE HERE
  def playing_as_knight knight
    new_knight_state = knight.damage_knight @dam
    knight.hp = new_knight_state[0]
    knight.ap = new_knight_state[1]
  end
  def playing_as_wizard wizard
    wizard.mp = wizard.mp - @hp
  end
end

class Potion < Encounter
  attr_reader :hp, :mp

  def initialize(hp, mp)
    @hp = hp
    @mp = mp
  end

  def to_s
    "There is a potion here that can restore " + @hp.to_s +
        " hitpoint(s) and " + @mp.to_s + " mana point(s)."
  end

  ## YOUR CODE HERE
  def playing_as_knight knight
    knight.hp = knight.hp + @hp
  end
  def playing_as_wizard wizard
    wizard.hp = wizard.hp + @hp
    wizard.mp = wizard.mp + @mp
  end
end

class Armor < Encounter
  attr_reader :ap

  def initialize ap
    @ap = ap
  end

  def to_s
    "A shiny piece of armor, rated for " + @ap.to_s +
        " AP, is gathering dust in an alcove!"
  end

  ## YOUR CODE HERE
  def playing_as_knight knight
    night.ap = knight.ap + @ap
  end
  def playing_as_wizard wizard
    wizard
  end
end

if __FILE__ == $0
  Adventure.new(Stdout.new, Knight.new(15, 3),
    [Monster.new(1, 1),
    FloorTrap.new(3),
    Monster.new(5, 3),
    Potion.new(5, 5),
    Monster.new(1, 15),
    Armor.new(10),
    FloorTrap.new(5),
    Monster.new(10, 10)]).play_out
end
