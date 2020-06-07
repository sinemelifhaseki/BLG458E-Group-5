import System.Environment
import Data.List
import Data.Typeable
import System.IO


data Ninja = Ninja {name:: String, country:: Char, status:: String, exam1:: Float, exam2:: Float, ability1:: String, ability2:: String, r:: Int, score:: Float} deriving Show
--status initialized as junior --> will be upgraded to journeyman
--r is number of rounds ninja took place, initialized as 0
--according to design we can add score 

--Lists: (they can be changed but we will mainly deal with 5 lists)
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

calcScore :: ((String -> Float) -> String -> String -> Float) -> String -> String -> Float -> Float -> Float -- call another function to get values of abilities
calcScore abilitySum a1 a2 e1 e2 = 0.5 * e1 + 0.3 * e2 + abilitySum findAbility a1 a2 

insertNinjas :: [[Char]] -> [Ninja]
insertNinjas lines = map insertNinja lines   --send all lines to insertNinja function


insertNinja :: [Char] -> Ninja
insertNinja x = case (xWords !! 1) of
                  "Fire" -> (Ninja {name=(xWords !! 0), country = ((xWords !! 1) !! 0), status = "Junior", exam1 = read (xWords !! 2) :: Float, exam2 = read (xWords !! 3) :: Float, ability1 = (xWords !! 4), ability2 = (xWords !! 5), r = 0, score = calcScore abilitySum (xWords !! 4) (xWords !! 5) (read (xWords !! 2)::Float) (read (xWords !! 3)::Float)})
   
                  "Lightning" -> (Ninja {name=(xWords !! 0), country = ((xWords !! 1) !! 0), status = "Junior", exam1 = read (xWords !! 2) :: Float, exam2 = read (xWords !! 3) :: Float, ability1 = (xWords !! 4), ability2 = (xWords !! 5), r = 0, score = calcScore abilitySum (xWords !! 4) (xWords !! 5) (read (xWords !! 2)::Float) (read (xWords !! 3)::Float)})
                         
                  "Water" -> (Ninja {name=(xWords !! 0), country = ((xWords !! 1) !! 0), status = "Junior", exam1 = read (xWords !! 2) :: Float, exam2 = read (xWords !! 3) :: Float, ability1 = (xWords !! 4), ability2 = (xWords !! 5), r = 0, score = calcScore abilitySum (xWords !! 4) (xWords !! 5) (read (xWords !! 2)::Float) (read (xWords !! 3)::Float)})
                         
                  "Wind"  -> (Ninja {name=(xWords !! 0), country = ((xWords !! 1) !! 2), status = "Junior", exam1 = read (xWords !! 2) :: Float, exam2 = read (xWords !! 3) :: Float, ability1 = (xWords !! 4), ability2 = (xWords !! 5), r = 0, score = calcScore abilitySum (xWords !! 4) (xWords !! 5) (read (xWords !! 2)::Float) (read (xWords !! 3)::Float)})

                  "Earth" -> (Ninja {name=(xWords !! 0), country = ((xWords !! 1) !! 0), status = "Junior", exam1 = read (xWords !! 2) :: Float, exam2 = read (xWords !! 3) :: Float, ability1 = (xWords !! 4), ability2 = (xWords !! 5), r = 0, score = calcScore abilitySum (xWords !! 4) (xWords !! 5) (read (xWords !! 2)::Float) (read (xWords !! 3)::Float)})
                         
                where 
                   xWords = words (x)


ourFilter f letter [] = []
ourFilter f letter (x:xs) 
    | f letter x        = x : ourFilter f letter xs
    | otherwise         = ourFilter f letter xs

parseNinjas :: Char -> [Ninja] -> [Ninja]
parseNinjas countryLetter allNinjas = ourFilter isSameCountry countryLetter allNinjas 

isSameCountry :: Char -> Ninja -> Bool
isSameCountry countryLetter ninjaInstance
     | (country ninjaInstance) == countryLetter     = True
     | otherwise                                  = False


printMenu input = do 
              putStrLn "a) View a Country's Ninja Information\nb) View All Countries' Ninja Information"
              putStrLn "c) Make a Round Between Ninjas\nd) Make a Round Between Countries\ne)Exit"
              putStrLn "Enter the choice:"
              choice <- getLine
              case head choice of
                     --'a' -> insertNinjas input >>= printMenu
                     'b' -> do 
                          let allLists = insertNinjas input -- allLists type : [[Ninja]] 
                          let fire = parseNinjas 'F' allLists
                          let earth = parseNinjas 'E' allLists
                          let lightning = parseNinjas 'L' allLists
                          let water = parseNinjas 'W' allLists
                          let wind = parseNinjas 'n' allLists
                          putStrLn ""
                          printNinjas (mergeNinjas fire earth lightning water wind)
                          putStrLn "" >> printMenu input
                     --'c' ->
                     --'d' ->
                     'e' -> return ()
                     _ -> putStrLn "Invalid choice, choose again." >> printMenu input

--compareAbilities :: Ninja -> Ninja -> Bool
--compareAbilities ninja1 ninja2
--   | (ability1 ninja1 + ability2 ninja1 ) > 

compareScores :: Ninja -> Ninja -> Bool
compareScores ninja1 ninja2 
   | score ninja1 > score ninja2  = True
   | otherwise                    = False

sortNinjas :: Ninja -> [Ninja] -> [Ninja]
sortNinjas ninja [] = [ninja]
sortNinjas ninja xs@(x':xs') 
   | r ninja < r x'                                = ninja : xs    
   | (r ninja == r x') && compareScores ninja x'   = ninja : xs
   | otherwise                                     = x' : sortNinjas ninja xs'

nSort :: [Ninja] -> [Ninja]
nSort [] = []
nSort (x:xs) = sortNinjas x (nSort xs)


mergeNinjas :: [Ninja] -> [Ninja] -> [Ninja] -> [Ninja] -> [Ninja] -> [Ninja] 
mergeNinjas fire earth lightning water wind = nSort (fire ++ earth ++ lightning ++ water ++ wind)

printNinjas :: [Ninja] -> IO ()
printNinjas ninjaList = mapM_ printNinja ninjaList

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
            
 
main = do
    args <- getArgs -- IO [String]
    content <- readFile (args !! 0)
    let fileLines = lines content -- fileLines type: [[Char]]
    --let lineWords = words (fileLines !! 1)
    --putStrLn (show (lineWords))
    let x = fileLines !! 1
    --putStrLn (show (typeOf (x)))
    --let naruto = (Ninja {name=(words (x) !! 0), country = ((words (x) !! 1) !! 0), status = "Junior", exam1 = read (words (x) !! 2) :: Float, exam2 = read (words (x) !! 3) :: Float, ability1 = (words (x) !! 4), ability2 = (words (x) !! 5), r = 0}) : fire
    --let allLists = insertNinjas fileLines -- allLists type : [[Ninja]] 
    --let fire = parseNinjas 'F' allLists
    --let earth = parseNinjas 'E' allLists
    --let lightning = parseNinjas 'L' allLists
    --let water = parseNinjas 'W' allLists
    --let wind = parseNinjas 'n' allLists
    --putStrLn (show ("Beni gorun:"))
    --printNinjas (mergeNinjas fire earth lightning water wind)
    --putStrLn (show (name (head (mergeNinjas fire earth lightning water wind)))) -- sasuke
    --putStrLn (show (name (head (tail (mergeNinjas fire earth lightning water wind))))) -- gaara 
    --putStrLn (show (name (head (tail (tail (mergeNinjas fire earth lightning water wind)))))) -- sana
    --putStrLn (show (name (head (tail (tail (tail (mergeNinjas fire earth lightning water wind))))))) -- naruto
    --putStrLn (show (name (head (tail (tail (tail (tail (mergeNinjas fire earth lightning water wind)))))))) -- aimi 
    --putStrLn (show (name (head (tail (tail (tail (tail (tail (mergeNinjas fire earth lightning water wind))))))))) -- suiu
    --putStrLn (show (name (head (tail (tail (tail (tail (tail (tail (mergeNinjas fire earth lightning water wind)))))))))) -- haruki
    --putStrLn (show (name (head (tail (tail (tail (tail (tail (tail (tail (mergeNinjas fire earth lightning water wind))))))))))) -- neiji 
    --putStrLn (show (name (head (tail (tail (tail (tail (tail (tail (tail (tail (mergeNinjas fire earth lightning water wind)))))))))))) -- samidare
    --putStrLn (show (name (head (tail (tail (tail (tail (tail (tail (tail (tail (tail (mergeNinjas fire earth lightning water wind))))))))))))) --midare
    --putStrLn (show ("Beni gordunuz:"))
    --putStrLn (show (name (head (tail (wind)))))
    --putStrLn (show (name (head (tail (fire)))))
    --putStrLn (show (name (head (tail (lightning)))))
    --putStrLn (show (name (head (tail (earth)))))    
    --putStrLn (show (name (head (tail (water)))))
    --putStrLn (show (typeOf (lightning)))
    --let lightning = (Ninja {name=(words (x) !! 0), country = ((words (x) !! 1) !! 0), status = "Junior", exam1 = read (words (x) !! 2) :: Float, exam2 = read (words (x) !! 3) :: Float, ability1 = (words (x) !! 4), ability2 = (words (x) !! 5), r = 0}) : lightning
   -- putStrLn (show (name (head (insertNinjas fileLines)))) -- şu an sadece txtnin son satırını alıyor ama lightninge de ekememiş anlamadım
    --putStrLn (show (score  (head (tail (allLists))))) -- Sasuke, fire, 133
    --putStrLn (show (score  (head (tail (tail (tail (tail (tail (allLists))))))))) -- Kankuro, wind
    --putStrLn (show (score  (head (tail (tail (tail (tail (tail (tail (allLists)))))))))) -- Midare, water
    --putStrLn (show (length allLists))
    printMenu fileLines
    --putStr (name naruto)
    --putStrLn (show (length lise))
