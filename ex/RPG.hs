module RPG where

-- Improvise!  A Character could have, for example:
-- a Name, a Race, a Class, a Level, XP (experience points),
-- 6 attributes:
--   Strength Intelligence Wisdom Dexterity Constitution Charisma
-- current and total HP (hit points)
-- current and total MP (mana points)
-- current and total GP (gold pieces)
data CharacterRace = Human | Elf | Dwarf | Orc | Gnome | Halfling
data CharacterClass = Fighter | Mage | Cleric | Thief | Ranger | Paladin

type CharacterName =  String
type CharacterLevel =  Int
type CharacterXP =  Int

type CharacterStrength =  Int
type CharacterIntelligence =  Int
type CharacterWisdom =  Int
type CharacterDexterity =  Int
type CharacterConstitution =  Int
type CharacterCharisma =  Int

data CharacterHP = HP Int Int
data CharacterMP = MP Int Int
type CharacterGP = Int

data CharacterProfile = CharacterProfile
                            CharacterName
                            CharacterRace
                            CharacterClass

data CharacterAttributes = CharacterAttributes
                            CharacterStrength
                            CharacterIntelligence
                            CharacterWisdom
                            CharacterDexterity
                            CharacterConstitution
                            CharacterCharisma

data CharacterStats = CharacterStats
                        CharacterLevel
                        CharacterXP
                        CharacterHP
                        CharacterMP
                        CharacterGP


data Character =   Character CharacterProfile CharacterAttributes CharacterStats

-- does that make sense?
type Party = [Character]

-- gets a character and returns one that is the same but +1 level
gainLevel :: Character -> Character
gainLevel (Character profile attributes (CharacterStats level xp hp mp gp)) =
    Character profile attributes (CharacterStats (level+1) xp hp mp gp)


-- to be used when a character is hit
hitCharacter :: Character -> Int -> Character
hitCharacter (Character profile attributes (CharacterStats level xp (HP current total) mp gp)) damage =
    Character profile attributes (CharacterStats level xp (HP (current-damage) total) mp gp)

alive :: Character -> Bool
alive (Character profile attributes (CharacterStats level xp (HP current total) mp gp)) =
    current > 0


-- How would you implement skills and spells?

data Skill = Skill String Int

data Spell = Spell String Int


getSkillPerClass :: CharacterClass -> [Skill]
getSkillPerClass Fighter = [Skill "Attack" 1, Skill "Defend" 1]
getSkillPerClass Mage    = []
getSkillPerClass Cleric  = []
getSkillPerClass Thief   = [Skill "Steal" 1, Skill "Backstab" 1]
getSkillPerClass Ranger  = []
getSkillPerClass Paladin = []

getSpellPerClass :: CharacterClass -> [Spell]
getSpellPerClass Fighter = []
getSpellPerClass Mage    = [Spell "Fireball" 1, Spell "Icebolt" 1]
getSpellPerClass Cleric  = [Spell "Heal" 1, Spell "Bless" 1]
getSpellPerClass Thief   = []
getSpellPerClass Ranger  = []
getSpellPerClass Paladin = [Spell "Smite" 1, Spell "Bless" 1]




skills :: Character -> [Skill]
skills (Character (CharacterProfile _ _ cc ) att stats) = getSkillPerClass cc

spells :: Character -> [Spell]
spells (Character (CharacterProfile _ _ cc ) att stats) = getSpellPerClass cc

