import System.Environment
import Data.List
import Data.Typeable
import System.IO
import Data.Char

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
                         
                  "Wind"  -> (Ninja {name=(xWords !! 0), country = toUpper ((xWords !! 1) !! 2), status = "Junior", exam1 = read (xWords !! 2) :: Float, exam2 = read (xWords !! 3) :: Float, ability1 = (xWords !! 4), ability2 = (xWords !! 5), r = 0, score = calcScore abilitySum (xWords !! 4) (xWords !! 5) (read (xWords !! 2)::Float) (read (xWords !! 3)::Float)})

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
     | otherwise                                    = False

--updateLists :: Int -> [Char] -> [Char] -> [Ninja] -> [Ninja] -> ([Ninja],[Ninja]) 
--updateLists winner name1 name2 nList1 nList2
--   | winner == 1            = do
                                 --let winner_ninja = findNinja nList1 name1
                                 --let new_score = score winner_ninja + 10
                                 --let new_round = r winner_ninja + 1
                                 --let loser_ninja = findNinja nList2 name2
                                 --if new_round >= 3
                                 --then do 
                                           --let new_ninja = (Ninja {name = (name winner_ninja), country = (country winner_ninja), status = "JourneyMan", exam1 = (exam1 winner_ninja), exam2 = (exam2 winner_ninja), ability1 = (ability1 winner_ninja), ability2 = (ability2 winner_ninja), r = new_round, score = new_score})
                                           --let nList1_temp = removeNinja winner_ninja nList1
                                           --let nList1_temp2 = pushList new_ninja nList1_temp
                                           --let nList2_temp = removeNinja loser_ninja nList2
                                           --(nList1_temp2, nList2_temp) 
                                 --else do
                                              --let new_ninja = (Ninja {name = (name winner_ninja), country = (country winner_ninja), status = "Junior", exam1 = (exam1 winner_ninja), exam2 = (exam2 winner_ninja), ability1 = (ability1 winner_ninja), ability2 = (ability2 winner_ninja), r = new_round, score = new_score})
                                              --let nList1_temp = removeNinja winner_ninja nList1
                                              --let nList1_temp2 = pushList new_ninja nList1_temp
                                              --let nList2_temp = removeNinja loser_ninja nList2
                                              --(nList1_temp2, nList2_temp)

   
--   | winner == 2            = do
                                 --let winner_ninja = findNinja nList2 name2
                                 --let new_score = score winner_ninja + 10
                                 --let new_round = r winner_ninja + 1
                                 --let loser_ninja = findNinja nList1 name1
                                 --if new_round >= 3
                                     --then do 
                                             --let new_ninja = (Ninja {name = (name winner_ninja), country = (country winner_ninja), status = "JourneyMan", exam1 = (exam1 winner_ninja), exam2 = (exam2 winner_ninja), ability1 = (ability1 winner_ninja), ability2 = (ability2 winner_ninja), r = new_round, score = new_score})
                                             --let nList2_temp = removeNinja winner_ninja nList2
                                             --let nList2_temp2 = pushList new_ninja nList2_temp
                                             --let nList1_temp = removeNinja loser_ninja nList1
                                             --(nList1_temp, nList2_temp2)
                                 --else do
                                             --let new_ninja = (Ninja {name = (name winner_ninja), country = (country winner_ninja), status = "Junior", exam1 = (exam1 winner_ninja), exam2 = (exam2 winner_ninja), ability1 = (ability1 winner_ninja), ability2 = (ability2 winner_ninja), r = new_round, score = new_score})
                                             --let nList2_temp = removeNinja winner_ninja nList2
                                             --let nList2_temp2 = pushList new_ninja nList2_temp
                                             --let nList1_temp = removeNinja loser_ninja nList1
                                             --(nList1_temp, nList2_temp2)



printMenu input = do 
              putStrLn "a) View a Country's Ninja Information\nb) View All Countries' Ninja Information"
              putStrLn "c) Make a Round Between Ninjas\nd) Make a Round Between Countries\ne)Exit"
              putStrLn "Enter the choice:"
              choice <- getLine
              case head choice of
                     'a' -> do
                          putStrLn "Enter the country code: "
                          cCode <- getLine
                          let allLists = insertNinjas input -- allLists type : [[Ninja]] 
                          let countryList = parseNinjas (toUpper (head cCode)) allLists
                          putStrLn ""
                          printNinjas (nSort countryList)
                          putStrLn "" >> printMenu input
                          
                     --insertNinjas input >>= printMenu
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
                     'c' -> do
                          putStrLn "Enter the name of first ninja: "
                          name1 <- getLine
                          putStrLn "Enter the country code of first ninja: "
                          cCode1 <- getLine
                          putStrLn "Enter the name of second ninja: "
                          name2 <- getLine
                          putStrLn "Enter the country code of second ninja: "
                          cCode2 <- getLine
                          let allLists = insertNinjas input -- allLists type : [[Ninja]]
                          let countryList1 = parseNinjas (toUpper (head cCode1)) allLists
                          let countryList2 = parseNinjas (toUpper (head cCode2)) allLists
                          -- fonksiyon buraya gelcekti
                          let ninjaLists = makeFight findNinja countryList1 countryList2 name1 name2
                          printNinjas (snd ninjaLists)
                          
                          putStrLn "" >> printMenu input
                          -- burada bir hata veriyor sanırım anlamadım
                     --'d' ->
                     'e' -> return ()
                     _ -> putStrLn "Invalid choice, choose again." >> printMenu input

findNinja :: [Ninja] -> [Char] -> Ninja
findNinja ninjaList@(x:xs) nameWanted 
   | name x == nameWanted = x
   | otherwise            = findNinja xs nameWanted

makeFight :: ([Ninja] -> [Char] -> Ninja) -> [Ninja] -> [Ninja] -> [Char] -> [Char] -> ([Ninja],[Ninja])
makeFight findNinja nList1 nList2 name1 name2
   | score (findNinja nList1 name1) > score (findNinja nList2 name2) = updateLists 1 name1 name2 nList1 nList2
   | score (findNinja nList1 name1) < score (findNinja nList2 name2) = updateLists 2 name1 name2 nList1 nList2
   | (score (findNinja nList1 name1) == score (findNinja nList2 name2)) && compareAbilities (findNinja nList1 name1) (findNinja nList2 name2) = updateLists 1 name1 name2 nList1 nList2
   | (score (findNinja nList1 name1) == score (findNinja nList2 name2)) && compareAbilities (findNinja nList2 name2) (findNinja nList1 name1) = updateLists 2 name1 name2 nList1 nList2
   | otherwise                                                                                                                                = updateLists 1 name1 name2 nList1 nList2 -- random olacak burası

compareAbilities :: Ninja -> Ninja -> Bool
compareAbilities ninja1 ninja2
   | (abilitySum findAbility (ability1 ninja1) (ability2 ninja1)) > (abilitySum findAbility (ability1 ninja2) (ability2 ninja2))  = True
   | otherwise                                                                                                                    = False


---------UPDATE: round artır diskalifiye et skor ekle oluşan listeleri döndür---> updateLists fonksiyonunu yaz


--- ozgun ekledi -----------------------------------------------------------------------------------------------------------------------

updateLists :: Int -> [Char] -> [Char] -> [Ninja] -> [Ninja] -> ([Ninja],[Ninja]) 
updateLists winner name1 name2 nList1 nList2
   | winner == 1            = do
                                 let winner_ninja = findNinja nList1 name1
                                 let new_score = score winner_ninja + 10
                                 let new_round = r winner_ninja + 1
                                 let loser_ninja = findNinja nList2 name2
                                 if new_round >= 3
                                 then do 
                                           let new_ninja = (Ninja {name = (name winner_ninja), country = (country winner_ninja), status = "JourneyMan", exam1 = (exam1 winner_ninja), exam2 = (exam2 winner_ninja), ability1 = (ability1 winner_ninja), ability2 = (ability2 winner_ninja), r = new_round, score = new_score})
                                           let nList1_temp = removeNinja winner_ninja nList1
                                           let nList1_temp2 = pushList new_ninja nList1_temp
                                           let nList2_temp = removeNinja loser_ninja nList2
                                           (nList1_temp2, nList2_temp) 
                                 else do
                                              let new_ninja = (Ninja {name = (name winner_ninja), country = (country winner_ninja), status = "Junior", exam1 = (exam1 winner_ninja), exam2 = (exam2 winner_ninja), ability1 = (ability1 winner_ninja), ability2 = (ability2 winner_ninja), r = new_round, score = new_score})
                                              let nList1_temp = removeNinja winner_ninja nList1
                                              let nList1_temp2 = pushList new_ninja nList1_temp
                                              let nList2_temp = removeNinja loser_ninja nList2
                                              (nList1_temp2, nList2_temp)

   
   | winner == 2            = do
                                 let winner_ninja = findNinja nList2 name2
                                 let new_score = score winner_ninja + 10
                                 let new_round = r winner_ninja + 1
                                 let loser_ninja = findNinja nList1 name1
                                 if new_round >= 3
                                     then do 
                                             let new_ninja = (Ninja {name = (name winner_ninja), country = (country winner_ninja), status = "JourneyMan", exam1 = (exam1 winner_ninja), exam2 = (exam2 winner_ninja), ability1 = (ability1 winner_ninja), ability2 = (ability2 winner_ninja), r = new_round, score = new_score})
                                             let nList2_temp = removeNinja winner_ninja nList2
                                             let nList2_temp2 = pushList new_ninja nList2_temp
                                             let nList1_temp = removeNinja loser_ninja nList1
                                             (nList1_temp, nList2_temp2)
                                 else do
                                             let new_ninja = (Ninja {name = (name winner_ninja), country = (country winner_ninja), status = "Junior", exam1 = (exam1 winner_ninja), exam2 = (exam2 winner_ninja), ability1 = (ability1 winner_ninja), ability2 = (ability2 winner_ninja), r = new_round, score = new_score})
                                             let nList2_temp = removeNinja winner_ninja nList2
                                             let nList2_temp2 = pushList new_ninja nList2_temp
                                             let nList1_temp = removeNinja loser_ninja nList1
                                             (nList1_temp, nList2_temp2)

   
removeNinja :: Ninja -> [Ninja] -> [Ninja] -- sinem: runtimedaki hatayi duzelttim, ama sonuclari yanlis veriyor
removeNinja removedNinja [] = []
removeNinja removedNinja ninjalist@(x:xs) 
   | (name removedNinja) == name x  = removeNinja removedNinja xs
   | otherwise                      = x : removeNinja removedNinja xs

   
pushList :: Ninja -> [Ninja] -> [Ninja] -- ozgun ekledi
pushList newNinja ninjaList = newNinja : ninjaList

------------------------------------------------------------------------------------------------------------------------------------


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
    let x = fileLines !! 1
    printMenu fileLines
