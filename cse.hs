import System.Environment
import Data.List
import Data.Typeable
import System.IO
import Data.Char
import System.Random
import Control.Monad
import System.Exit (exitSuccess)


data Ninja = Ninja {name:: String, country:: Char, status:: String, exam1:: Float, exam2:: Float, ability1:: String, ability2:: String, r:: Int, score:: Float} deriving Show

fire :: [Ninja]
fire = []
lightning :: [Ninja]
lightning = []
water :: [Ninja]
water = []
wind :: [Ninja]
wind = []
earth :: [Ninja]
earth = []

-- findAbility: given a string, returns the value of that ability
findAbility :: String -> Float
findAbility a = case a of
        "Clone"     -> 20.0
        "Hit"       -> 10.0
        "Lightning" -> 50.0
        "Vision"    -> 30.0
        "Sand"      -> 50.0
        "Fire"      -> 40.0
        "Water"     -> 30.0
        "Blade"     -> 20.0
        "Summon"    -> 50.0
        "Storm"     -> 10.0
        "Rock"      -> 20.0
        
abilitySum :: (String -> Float) -> String -> String -> Float
abilitySum findAbility a b = findAbility a + findAbility b

-- calcScore: used to calculate a ninja's score. calls abilitySum function to get values of abilities
calcScore :: ((String -> Float) -> String -> String -> Float) -> String -> String -> Float -> Float -> Float
calcScore abilitySum a1 a2 e1 e2 = 0.5 * e1 + 0.3 * e2 + abilitySum findAbility a1 a2 

-- insertNinjas: send all lines to insertNinja function with map function. takes all lines of txt file as parameter
insertNinjas :: [[Char]] -> [Ninja]
insertNinjas lines = map insertNinja lines   

-- insertNinja: parameter is a line of the txt file which indicates a ninja, according to WORDS of that LINE, construct an instance of Ninja data type
insertNinja :: [Char] -> Ninja
insertNinja x = case (xWords !! 1) of
                  "Fire" -> (Ninja {name=(xWords !! 0), country = ((xWords !! 1) !! 0), status = "Junior", exam1 = read (xWords !! 2) :: Float, exam2 = read (xWords !! 3) :: Float, ability1 = (xWords !! 4), ability2 = (xWords !! 5), r = 0, score = calcScore abilitySum (xWords !! 4) (xWords !! 5) (read (xWords !! 2)::Float) (read (xWords !! 3)::Float)})
   
                  "Lightning" -> (Ninja {name=(xWords !! 0), country = ((xWords !! 1) !! 0), status = "Junior", exam1 = read (xWords !! 2) :: Float, exam2 = read (xWords !! 3) :: Float, ability1 = (xWords !! 4), ability2 = (xWords !! 5), r = 0, score = calcScore abilitySum (xWords !! 4) (xWords !! 5) (read (xWords !! 2)::Float) (read (xWords !! 3)::Float)})
                         
                  "Water" -> (Ninja {name=(xWords !! 0), country = ((xWords !! 1) !! 0), status = "Junior", exam1 = read (xWords !! 2) :: Float, exam2 = read (xWords !! 3) :: Float, ability1 = (xWords !! 4), ability2 = (xWords !! 5), r = 0, score = calcScore abilitySum (xWords !! 4) (xWords !! 5) (read (xWords !! 2)::Float) (read (xWords !! 3)::Float)})
                         
                  "Wind"  -> (Ninja {name=(xWords !! 0), country = toUpper ((xWords !! 1) !! 2), status = "Junior", exam1 = read (xWords !! 2) :: Float, exam2 = read (xWords !! 3) :: Float, ability1 = (xWords !! 4), ability2 = (xWords !! 5), r = 0, score = calcScore abilitySum (xWords !! 4) (xWords !! 5) (read (xWords !! 2)::Float) (read (xWords !! 3)::Float)})

                  "Earth" -> (Ninja {name=(xWords !! 0), country = ((xWords !! 1) !! 0), status = "Junior", exam1 = read (xWords !! 2) :: Float, exam2 = read (xWords !! 3) :: Float, ability1 = (xWords !! 4), ability2 = (xWords !! 5), r = 0, score = calcScore abilitySum (xWords !! 4) (xWords !! 5) (read (xWords !! 2)::Float) (read (xWords !! 3)::Float)})
                         
                where 
                   xWords = words (x)

-- ourFilter: this filter is used for creating seperate country lists from allLists which keep all ninja data
ourFilter f letter [] = [] 
ourFilter f letter (x:xs) 
    | f letter x        = x : ourFilter f letter xs
    | otherwise         = ourFilter f letter xs

-- parseNinjas: filter is applied with the filtering function of comparing the requested country and ninja's country
parseNinjas :: Char -> [Ninja] -> [Ninja] 
parseNinjas countryLetter allNinjas = ourFilter isSameCountry countryLetter allNinjas 

-- isSameCountry: checks whether given country letter is equal to that ninja's country
isSameCountry :: Char -> Ninja -> Bool 
isSameCountry countryLetter ninjaInstance
     | (country ninjaInstance) == countryLetter     = True
     | otherwise                                    = False

-- MENU OPERATIONS
printMenu allLists = do  
              putStrLn "a) View a Country's Ninja Information\nb) View All Countries' Ninja Information"
              putStrLn "c) Make a Round Between Ninjas\nd) Make a Round Between Countries\ne)Exit"
              putStrLn "Enter the choice:"
              choice <- getLine
              case head choice of
                     'a' -> do
                          putStrLn "Enter the country code: "
                          cCode <- getLine
                          if (toUpper (head cCode)) == 'F' || (toUpper (head cCode)) == 'E' || (toUpper (head cCode)) == 'N' || (toUpper (head cCode)) == 'W' || (toUpper (head cCode)) == 'L'
                          then putStr ""
                          else do
                            putStrLn "You have entered an invalid country code. Please choose again."
                            putStrLn "" >> printMenu allLists
                          let countryList = parseNinjas (toUpper (head cCode)) allLists
                          putStrLn ""
                          if length countryList > 0
                             then do printNinjas (nSort countryList)
                          else do
                              putStrLn "There is no suitable ninja in this country! Please choose again. (All ninjas in this country may be disqualified.)"
                          if checkIfPromoted countryList == True
                          then do 
                              putStrLn "This country has already one promoted ninja and can no longer take place in fights."
                              putStrLn "" >> printMenu allLists
                          else 
                              putStrLn "" >> printMenu allLists
                     'b' -> do 
                          --let allLists = insertNinjas input -- allLists type : [[Ninja]] 
                          let fire = parseNinjas 'F' allLists
                          let earth = parseNinjas 'E' allLists
                          let lightning = parseNinjas 'L' allLists
                          let water = parseNinjas 'W' allLists
                          let wind = parseNinjas 'n' allLists
                          putStrLn ""
                          printNinjas (mergeNinjas fire earth lightning water wind)
                          putStrLn "" >> printMenu allLists
                     'c' -> do
                          putStrLn "Enter the name of first ninja: "
                          name1 <- getLine
                          putStrLn "Enter the country code of first ninja: "
                          cCode1 <- getLine
                          if (toUpper (head cCode1)) == 'F' || (toUpper (head cCode1)) == 'E' || (toUpper (head cCode1)) == 'N' || (toUpper (head cCode1)) == 'W' || (toUpper (head cCode1)) == 'L'
                          then putStr ""
                          else do
                            putStrLn "You have entered an invalid country code. Please choose again."
                            putStrLn "" >> printMenu allLists
                          let countryList1 = nSort (parseNinjas (toUpper (head cCode1)) allLists)
                          if checkIfPromoted countryList1 == True
                          then do 
                              putStrLn "This country has already one promoted ninja and can no longer take place in fights. Please choose again."
                              putStrLn "" >> printMenu allLists
                          else do 
                            if (name (findNinja countryList1 name1)) == "Error"
                            then do 
                                putStrLn "There is no such available Ninja to fight for the first country."
                                putStrLn "" >> printMenu allLists
                            else putStr ""
                            putStrLn "Enter the name of second ninja: "
                            name2 <- getLine
                            putStrLn "Enter the country code of second ninja: "
                            cCode2 <- getLine
                            if (toUpper (head cCode2)) == 'F' || (toUpper (head cCode2)) == 'E' || (toUpper (head cCode2)) == 'N' || (toUpper (head cCode2)) == 'W' || (toUpper (head cCode2)) == 'L'
                            then putStr ""
                            else do
                              putStrLn "You have entered an invalid country code. Please choose again."
                              putStrLn "" >> printMenu allLists
                            let countryList2 = nSort (parseNinjas (toUpper (head cCode2)) allLists)
                            if (name (findNinja countryList2 name2)) == "Error"
                            then do 
                                putStrLn "There is no such available Ninja to fight for second country."
                                putStrLn "" >> printMenu allLists
                            else putStr ""
                            let ninjaLists = makeFight findNinja countryList1 countryList2 name1 name2
                            putStr "Winner: "
                            printNinja (head (fst ninjaLists))
                            putStrLn "" >> printMenu (findRemainingLists (toUpper (head cCode1)) (toUpper (head cCode2)) allLists (fst ninjaLists) (snd ninjaLists)) -- burada inputu güncellememiz gerekecek sanırım
                           
                     'd' -> do
                          putStrLn "Enter the country code of first ninja: "
                          cCode1 <- getLine
                          if (toUpper (head cCode1)) == 'F' || (toUpper (head cCode1)) == 'E' || (toUpper (head cCode1)) == 'N' || (toUpper (head cCode1)) == 'W' || (toUpper (head cCode1)) == 'L'
                          then putStr ""
                          else do
                            putStrLn "You have entered an invalid country code. Please choose again."
                            putStrLn "" >> printMenu allLists
                          let countryList1 = nSort (parseNinjas (toUpper (head cCode1)) allLists)
                          if checkIfPromoted countryList1 == True
                          then do 
                              putStrLn "This country has already one promoted ninja and can no longer take place in fights. Please choose again."
                              putStrLn "" >> printMenu allLists
                          else do 
                              if length countryList1 == 0 
                              then do
                                putStrLn "There is no such available Ninja to fight for first country."
                                putStrLn "" >> printMenu allLists
                              else putStr ""
                              putStrLn "Enter the country code of second ninja: "
                              cCode2 <- getLine
                              if (toUpper (head cCode1)) == 'F' || (toUpper (head cCode1)) == 'E' || (toUpper (head cCode1)) == 'N' || (toUpper (head cCode1)) == 'W' || (toUpper (head cCode1)) == 'L'
                              then putStr ""
                              else do
                                 putStrLn "You have entered an invalid country code. Please choose again."
                                 putStrLn "" >> printMenu allLists
                              let countryList2 = nSort (parseNinjas (toUpper (head cCode2)) allLists)
                              if checkIfPromoted countryList2 == True
                              then do 
                                 putStrLn "This country has already one promoted ninja and can no longer take place in fights. Please choose again."
                                 putStrLn "" >> printMenu allLists
                              else do 
                                 if length countryList2 == 0 
                                 then do
                                   putStrLn "There is no such available Ninja to fight for second country."
                                   putStrLn "" >> printMenu allLists
                                 else do                               
                                 let ninjaLists = makeFight findNinja countryList1 countryList2 (name (head countryList1)) (name (head countryList2))
                                 putStr "Winner: "
                                 printNinja2 (head (fst ninjaLists))
                                 putStrLn "" >> printMenu (findRemainingLists (toUpper (head cCode1)) (toUpper (head cCode2)) allLists (fst ninjaLists) (snd ninjaLists)) -- burada inputu güncellememiz gerekecek sanırım
                     'e' -> exitSuccess
                     _ -> putStrLn "Invalid choice, choose again." >> printMenu allLists

--- checkIfPromoted: checks whether a country has a promoted ninja, returns true if there's one
checkIfPromoted :: [Ninja] -> Bool
checkIfPromoted [] = False
checkIfPromoted list@(x:xs) 
  | status x == "JourneyMan"   = True
  | otherwise                  = checkIfPromoted xs

-- findRemainingLists: used in parts C and D to find which country lists were updated & which not so that we can merge all country lists back again, along with the updated ones
findRemainingLists :: Char -> Char -> [Ninja] -> [Ninja] -> [Ninja] -> [Ninja] 
findRemainingLists first second allLists list1 list2 = case (first,second) of
        ('F', 'F') ->  do --fire earth lightning water wind
                          let earth = parseNinjas 'E' allLists
                          let lightning = parseNinjas 'L' allLists
                          let water = parseNinjas 'W' allLists
                          let wind = parseNinjas 'N' allLists
                          mergeNinjas list2 earth lightning water wind
        ('E', 'E') ->  do --fire earth lightning water wind
                          let fire = parseNinjas 'F' allLists
                          let lightning = parseNinjas 'L' allLists
                          let water = parseNinjas 'W' allLists
                          let wind = parseNinjas 'N' allLists
                          mergeNinjas fire list2 lightning water wind
        ('W', 'W') ->  do --fire earth lightning water wind
                          let earth = parseNinjas 'E' allLists
                          let lightning = parseNinjas 'L' allLists
                          let fire = parseNinjas 'F' allLists
                          let wind = parseNinjas 'N' allLists
                          mergeNinjas fire earth lightning list2 wind
        ('L', 'L') ->  do --fire earth lightning water wind
                          let earth = parseNinjas 'E' allLists
                          let fire = parseNinjas 'F' allLists
                          let water = parseNinjas 'W' allLists
                          let wind = parseNinjas 'N' allLists
                          mergeNinjas fire earth list2 water wind
        ('N', 'N') ->  do --fire earth lightning water wind
                          let earth = parseNinjas 'E' allLists
                          let lightning = parseNinjas 'L' allLists
                          let water = parseNinjas 'W' allLists
                          let fire = parseNinjas 'F' allLists
                          mergeNinjas fire earth lightning water list2
        ('F', 'W') ->  do --fire earth lightning water wind
                          let earth = parseNinjas 'E' allLists
                          let lightning = parseNinjas 'L' allLists
                          let water = parseNinjas 'W' allLists
                          mergeNinjas list1 earth lightning list2 wind
        ('F', 'E') ->  do --fire earth lightning water wind
                          let wind = parseNinjas 'N' allLists
                          let lightning = parseNinjas 'L' allLists
                          let water = parseNinjas 'W' allLists
                          mergeNinjas list1 list2 lightning water wind
        ('F', 'L') ->  do --fire earth lightning water wind
                          let earth = parseNinjas 'E' allLists
                          let wind = parseNinjas 'N' allLists
                          let water = parseNinjas 'W' allLists
                          mergeNinjas list1 earth list2 water wind
        ('F', 'N') ->  do --fire earth lightning water wind
                          let earth = parseNinjas 'E' allLists
                          let lightning = parseNinjas 'L' allLists
                          let water = parseNinjas 'W' allLists
                          mergeNinjas list1 earth lightning water list2
        ('E', 'W') ->  do --fire earth lightning water wind
                          let fire = parseNinjas 'F' allLists
                          let lightning = parseNinjas 'L' allLists
                          let wind = parseNinjas 'N' allLists
                          mergeNinjas fire list1 lightning list2 wind
        ('E', 'L') ->  do --fire earth lightning water wind
                          let fire = parseNinjas 'F' allLists
                          let wind = parseNinjas 'N' allLists
                          let water = parseNinjas 'W' allLists
                          mergeNinjas fire list1 list2 water wind
        ('E', 'N') ->  do --fire earth lightning water wind
                          let fire = parseNinjas 'F' allLists
                          let lightning = parseNinjas 'L' allLists
                          let water = parseNinjas 'W' allLists
                          mergeNinjas fire list1 lightning water list2
        ('L', 'W') ->  do --fire earth lightning water wind
                          let fire = parseNinjas 'F' allLists
                          let earth = parseNinjas 'E' allLists
                          let wind = parseNinjas 'N' allLists
                          mergeNinjas fire earth list1 list2 wind
        ('L', 'N') ->  do --fire earth lightning water wind
                          let earth = parseNinjas 'E' allLists
                          let fire = parseNinjas 'F' allLists
                          let water = parseNinjas 'W' allLists
                          mergeNinjas list1 earth lightning water list2
        ('W', 'N') ->  do --fire earth lightning water wind
                          let earth = parseNinjas 'E' allLists
                          let lightning = parseNinjas 'L' allLists
                          let fire = parseNinjas 'F' allLists
                          mergeNinjas fire earth lightning list1 list2
        ('W', 'F') ->  do --fire earth lightning water wind
                          let earth = parseNinjas 'E' allLists
                          let lightning = parseNinjas 'L' allLists
                          let water = parseNinjas 'W' allLists
                          mergeNinjas list1 earth lightning list2 wind
        ('E', 'F') ->  do --fire earth lightning water wind
                          let wind = parseNinjas 'N' allLists
                          let lightning = parseNinjas 'L' allLists
                          let water = parseNinjas 'W' allLists
                          mergeNinjas list1 list2 lightning water wind
        ('L', 'F') ->  do --fire earth lightning water wind
                          let earth = parseNinjas 'E' allLists
                          let wind = parseNinjas 'N' allLists
                          let water = parseNinjas 'W' allLists
                          mergeNinjas list1 earth list2 water wind
        ('N', 'F') ->  do --fire earth lightning water wind
                          let earth = parseNinjas 'E' allLists
                          let lightning = parseNinjas 'L' allLists
                          let water = parseNinjas 'W' allLists
                          mergeNinjas list1 earth lightning water list2
        ('W', 'E') ->  do --fire earth lightning water wind
                          let fire = parseNinjas 'F' allLists
                          let lightning = parseNinjas 'L' allLists
                          let wind = parseNinjas 'N' allLists
                          mergeNinjas fire list1 lightning list2 wind
        ('L', 'E') ->  do --fire earth lightning water wind
                          let fire = parseNinjas 'F' allLists
                          let wind = parseNinjas 'N' allLists
                          let water = parseNinjas 'W' allLists
                          mergeNinjas fire list1 list2 water wind
        ('N', 'E') ->  do --fire earth lightning water wind
                          let fire = parseNinjas 'F' allLists
                          let lightning = parseNinjas 'L' allLists
                          let water = parseNinjas 'W' allLists
                          mergeNinjas fire list1 lightning water list2
        ('W', 'L') ->  do --fire earth lightning water wind
                          let fire = parseNinjas 'F' allLists
                          let earth = parseNinjas 'E' allLists
                          let wind = parseNinjas 'N' allLists
                          mergeNinjas fire earth list1 list2 wind
        ('N', 'L') ->  do --fire earth lightning water wind
                          let earth = parseNinjas 'E' allLists
                          let fire = parseNinjas 'F' allLists
                          let water = parseNinjas 'W' allLists
                          mergeNinjas list1 earth lightning water list2
        ('N', 'W') ->  do --fire earth lightning water wind
                          let earth = parseNinjas 'E' allLists
                          let lightning = parseNinjas 'L' allLists
                          let fire = parseNinjas 'F' allLists
                          mergeNinjas fire earth lightning list1 list2

-- findNinja: find a ninja by its name by searching recursively within a ninjaList
findNinja :: [Ninja] -> [Char] -> Ninja
findNinja [] _ = (Ninja {name="Error", country = '1', status = "Junior", exam1 = 1, exam2 = 1, ability1 = "", ability2 = "", r = 0, score = -1})  -- if input list is empty, or recursion ended and couldn't find such ninja, return a ninja with a name "Error"
findNinja ninjaList@(x:xs) nameWanted 
   | name x == nameWanted = x
   | otherwise            = findNinja xs nameWanted -- traverse ninjalists recursively until we obtain the requested ninja

-- makeFight: first find the requested ninja by calling findNinja function, then compare the 2 found ninjas based on their scores & availabilities
-- according to the result, send the winning info (1 for ninja1, 2 for ninja2) ninja names and lists to updateLists function which returns a tuple of 2 updated ninja lists
makeFight :: ([Ninja] -> [Char] -> Ninja) -> [Ninja] -> [Ninja] -> [Char] -> [Char] -> ([Ninja],[Ninja]) 
makeFight findNinja nList1 nList2 name1 name2
   | score (findNinja nList1 name1) > score (findNinja nList2 name2) = updateLists 1 name1 name2 nList1 nList2 -- 1st ninja wins by score
   | score (findNinja nList1 name1) < score (findNinja nList2 name2) = updateLists 2 name1 name2 nList1 nList2 -- 2nd ninja wins by score
   | (score (findNinja nList1 name1) == score (findNinja nList2 name2)) && compareAbilities (findNinja nList1 name1) (findNinja nList2 name2) = updateLists 1 name1 name2 nList1 nList2 -- 1st ninja by ability
   | (score (findNinja nList1 name1) == score (findNinja nList2 name2)) && compareAbilities (findNinja nList2 name2) (findNinja nList1 name1) = updateLists 2 name1 name2 nList1 nList2 -- 2nd ninja by ability
   | otherwise                                                                                                                                = updateLists 1 name1 name2 nList1 nList2 -- random 

-- compareAbilities: compares abilities of 2 ninjas based the result from findAbility function, returns true if ninja1 has more ability points, else false
compareAbilities :: Ninja -> Ninja -> Bool
compareAbilities ninja1 ninja2
   | (abilitySum findAbility (ability1 ninja1) (ability2 ninja1)) > (abilitySum findAbility (ability1 ninja2) (ability2 ninja2))  = True
   | otherwise                                                                                                                    = False

-- updateLists: takes 5 parameters: an Int indicating who won (1 if ninja1, 2 else), names of both ninjas and countryninjalists of 2 ninjas to update score or delete (disqualify) that ninja, returns a tuple of ninja lists as updated versions
updateLists :: Int -> [Char] -> [Char] -> [Ninja] -> [Ninja] -> ([Ninja],[Ninja]) 
updateLists winner name1 name2 nList1 nList2
   | winner == 1            = do                                                 -- find the winner ninja by its name and add 10 to score of winner ninja and 1 to his/her round amount
                                 let winner_ninja = findNinja nList1 name1        
                                 let new_score = score winner_ninja + 10
                                 let new_round = r winner_ninja + 1
                                 let loser_ninja = findNinja nList2 name2        -- find the loser ninja by its name
                                 if country winner_ninja == country loser_ninja
                                 then do 
                                           let new_ninja = (Ninja {name = (name winner_ninja), country = (country winner_ninja), status = "Junior", exam1 = (exam1 winner_ninja), exam2 = (exam2 winner_ninja), ability1 = (ability1 winner_ninja), ability2 = (ability2 winner_ninja), r = new_round, score = new_score})
                                           let nList1_temp = removeNinja winner_ninja nList2   -- first remove the winner ninja from list so we can push back the promoted version
                                           let nList1_temp2 = pushList new_ninja nList1_temp   -- push the promoted ninja to back its list
                                           let nList2_temp = removeNinja loser_ninja nList1_temp2    -- remove the disqualified ninja from country list
                                           (nList2_temp, nList2_temp) 
                                 else do 
                                   if new_round >= 3                               -- if the winner ninja has won more than or equal to 3 rounds, promote him/her to JourneyMan
                                   then do 
                                           let new_ninja = (Ninja {name = (name winner_ninja), country = (country winner_ninja), status = "JourneyMan", exam1 = (exam1 winner_ninja), exam2 = (exam2 winner_ninja), ability1 = (ability1 winner_ninja), ability2 = (ability2 winner_ninja), r = new_round, score = new_score})
                                           let nList1_temp = removeNinja winner_ninja nList1   -- first remove the winner ninja from list so we can push back the promoted version
                                           let nList1_temp2 = pushList new_ninja nList1_temp   -- push the promoted ninja to back its list
                                           let nList2_temp = removeNinja loser_ninja nList2    -- remove the disqualified ninja from country list
                                           (nList1_temp2, nList2_temp) 
                                   else do
                                              let new_ninja = (Ninja {name = (name winner_ninja), country = (country winner_ninja), status = "Junior", exam1 = (exam1 winner_ninja), exam2 = (exam2 winner_ninja), ability1 = (ability1 winner_ninja), ability2 = (ability2 winner_ninja), r = new_round, score = new_score})
                                              let nList1_temp = removeNinja winner_ninja nList1  -- first remove the winner ninja from list so we can push back the updated version
                                              let nList1_temp2 = pushList new_ninja nList1_temp  -- push the updated ninja back to its list
                                              let nList2_temp = removeNinja loser_ninja nList2   -- remove the disqualified ninja from country list
                                              (nList1_temp2, nList2_temp)

   
   | winner == 2            = do                                                 -- do same things as above conditions, but now winner ninja is ninja2 and ninja 1 is loser ninja
                                 let winner_ninja = findNinja nList2 name2
                                 let new_score = score winner_ninja + 10
                                 let new_round = r winner_ninja + 1
                                 let loser_ninja = findNinja nList1 name1
                                 if country winner_ninja == country loser_ninja
                                 then do 
                                           let new_ninja = (Ninja {name = (name winner_ninja), country = (country winner_ninja), status = "Junior", exam1 = (exam1 winner_ninja), exam2 = (exam2 winner_ninja), ability1 = (ability1 winner_ninja), ability2 = (ability2 winner_ninja), r = new_round, score = new_score})
                                           let nList1_temp = removeNinja winner_ninja nList2   -- first remove the winner ninja from list so we can push back the promoted version
                                           let nList1_temp2 = pushList new_ninja nList1_temp   -- push the promoted ninja to back its list
                                           let nList2_temp = removeNinja loser_ninja nList1_temp2    -- remove the disqualified ninja from country list
                                           (nList2_temp, nList2_temp) 
                                 else do 
                                   if new_round >= 3
                                     then do 
                                             let new_ninja = (Ninja {name = (name winner_ninja), country = (country winner_ninja), status = "JourneyMan", exam1 = (exam1 winner_ninja), exam2 = (exam2 winner_ninja), ability1 = (ability1 winner_ninja), ability2 = (ability2 winner_ninja), r = new_round, score = new_score})
                                             let nList2_temp = removeNinja winner_ninja nList2
                                             let nList2_temp2 = pushList new_ninja nList2_temp
                                             let nList1_temp = removeNinja loser_ninja nList1
                                             (nList2_temp2, nList1_temp)
                                    else do
                                             let new_ninja = (Ninja {name = (name winner_ninja), country = (country winner_ninja), status = "Junior", exam1 = (exam1 winner_ninja), exam2 = (exam2 winner_ninja), ability1 = (ability1 winner_ninja), ability2 = (ability2 winner_ninja), r = new_round, score = new_score})
                                             let nList2_temp = removeNinja winner_ninja nList2
                                             let nList2_temp2 = pushList new_ninja nList2_temp
                                             let nList1_temp = removeNinja loser_ninja nList1
                                             (nList2_temp2, nList1_temp)

-- removeNinja: to remove a disqualified / to-be-updated ninja from its list based on his/her name and returns the updated list
removeNinja :: Ninja -> [Ninja] -> [Ninja] 
removeNinja removedNinja [] = []
removeNinja removedNinja ninjalist@(x:xs) 
   | (name removedNinja) == name x  = removeNinja removedNinja xs
   | otherwise                      = x : removeNinja removedNinja xs

-- pushList: to add a ninja to a given list and returns the updated list
pushList :: Ninja -> [Ninja] -> [Ninja] 
pushList newNinja ninjaList = newNinja : ninjaList

-- compareScores: compares scores of two ninjas, returns true if first ninja's score is higher, else false
compareScores :: Ninja -> Ninja -> Bool
compareScores ninja1 ninja2 
   | score ninja1 > score ninja2  = True
   | otherwise                    = False

-- sortNinjas: sorts the ninjalist recursively based on comparing the rounds between ninjas first, if they are equal compares the scores. called by nSort function. uses compareScores to compare scores.
sortNinjas :: Ninja -> [Ninja] -> [Ninja]
sortNinjas ninja [] = [ninja]
sortNinjas ninja xs@(x':xs') 
   | r ninja < r x'                                = ninja : xs    
   | (r ninja == r x') && compareScores ninja x'   = ninja : xs
   | otherwise                                     = x' : sortNinjas ninja xs'

-- nSort: sorts all ninjas by sending the head and tail of the list to sortNinjas function
nSort :: [Ninja] -> [Ninja]
nSort [] = []
nSort (x:xs) = sortNinjas x (nSort xs)

-- mergeNinjas: we use this function especially in printMenu since we always do recursion over printMenu by using allLists, a merged version of all country lists. returns an overall list of all countries.
mergeNinjas :: [Ninja] -> [Ninja] -> [Ninja] -> [Ninja] -> [Ninja] -> [Ninja] 
mergeNinjas fire earth lightning water wind = nSort (fire ++ earth ++ lightning ++ water ++ wind)

-- printNinjas: takes a list of ninjas and maps the printNinja function to all elements of list so that all ninjas of a given list will be printed.
printNinjas :: [Ninja] -> IO ()
printNinjas ninjaList = mapM_ printNinja ninjaList

-- printNinja: called by printNinjas, used to print ninjas for parts a and b.
printNinja :: Ninja -> IO ()
printNinja ninja = do 
            putStr (show (name ninja))
            putStr (", Score: ")
            putStr (show (score ninja))
            putStr (", Status: ")
            putStr (show (status ninja))
            putStr (", Round: ")
            putStr (show (r ninja))
            putStrLn ""
            
-- printNinja2: called after 2 ninjas fight, shows the winner ninja's information
printNinja2 :: Ninja -> IO ()
printNinja2 ninja = do 
            putStr (show (name ninja))
            putStr (", Round: ")
            putStr (show (r ninja))
            putStr (", Status: ")
            putStr (show (status ninja))
            putStrLn "" 
 
main = do
    args <- getArgs -- IO [String]
    content <- readFile (args !! 0)
    let fileLines = lines content -- fileLines type: [[Char]]
    let allLists = insertNinjas fileLines
    printMenu allLists
