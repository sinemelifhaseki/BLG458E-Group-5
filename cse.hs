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

insertNinjas :: [[Char]] -> [[Ninja]]
insertNinjas lines = map insertNinja lines   --send all lines to insertNinja function


insertNinja :: [Char] -> [Ninja]
insertNinja x = case (xWords !! 1) of
                  "Fire" -> do
                         let temp = (Ninja {name=(xWords !! 0), country = ((xWords !! 1) !! 0), status = "Junior", exam1 = read (xWords !! 2) :: Float, exam2 = read (xWords !! 3) :: Float, ability1 = (xWords !! 4), ability2 = (xWords !! 5), r = 0})
                         fire <- fireAdder temp
                         return fire
                  "Lightning" -> do
                         let temp = (Ninja {name=(xWords !! 0), country = ((xWords !! 1) !! 0), status = "Junior", exam1 = read (xWords !! 2) :: Float, exam2 = read (xWords !! 3) :: Float, ability1 = (xWords !! 4), ability2 = (xWords !! 5), r = 0})
                         lightning <- lightningAdder temp
                         return lightning
                  "Water" -> do
                         let temp = (Ninja {name=(xWords !! 0), country = ((xWords !! 1) !! 0), status = "Junior", exam1 = read (xWords !! 2) :: Float, exam2 = read (xWords !! 3) :: Float, ability1 = (xWords !! 4), ability2 = (xWords !! 5), r = 0})
                         water <- waterAdder temp
                         return water
                  "Wind"  -> do
                         let temp = (Ninja {name=(xWords !! 0), country = ((xWords !! 1) !! 2), status = "Junior", exam1 = read (xWords !! 2) :: Float, exam2 = read (xWords !! 3) :: Float, ability1 = (xWords !! 4), ability2 = (xWords !! 5), r = 0})
                         wind <- windAdder temp
                         return wind
                  "Earth" -> do
                         let temp = (Ninja {name=(xWords !! 0), country = ((xWords !! 1) !! 0), status = "Junior", exam1 = read (xWords !! 2) :: Float, exam2 = read (xWords !! 3) :: Float, ability1 = (xWords !! 4), ability2 = (xWords !! 5), r = 0})
                         earth <- earthAdder temp
                         return earth
                where 
                   xWords = words (x)
                   
fireAdder :: Ninja -> [Ninja]
fireAdder temp = temp : fire

windAdder :: Ninja -> [Ninja]
windAdder temp = temp : wind

waterAdder :: Ninja -> [Ninja]
waterAdder temp = temp : water

earthAdder :: Ninja -> [Ninja]
earthAdder temp = temp : earth

lightningAdder :: Ninja -> [Ninja]
lightningAdder temp = temp : lightning


printMenu input = do 
              putStrLn "a) View a Country's Ninja Information\nb) View All Countries' Ninja Information"
              putStrLn "c) Make a Round Between Ninjas\nd) Make a Round Between Countries\ne)Exit"
              putStrLn "Enter the choice:"
              choice <- getLine
              case head choice of
                     --'a' -> insertNinjas input >>= printMenu
                     --'b' ->
                     --'c' ->
                     --'d' ->
                     'e' -> return ()
                     _ -> putStrLn "Invalid choice, choose again." >> printMenu input

main = do
    args <- getArgs -- IO [String]
    content <- readFile (args !! 0)
    let fileLines = lines content -- fileLines type: [[Char]]
    --let lineWords = words (fileLines !! 1)
    --putStrLn (show (lineWords))
    let x = fileLines !! 1
    putStrLn (show (typeOf (x)))
    --let naruto = (Ninja {name=(words (x) !! 0), country = ((words (x) !! 1) !! 0), status = "Junior", exam1 = read (words (x) !! 2) :: Float, exam2 = read (words (x) !! 3) :: Float, ability1 = (words (x) !! 4), ability2 = (words (x) !! 5), r = 0}) : fire
    let allLists = insertNinjas fileLines -- allLists type : [[Ninja]]
    putStrLn (show (typeOf (lightning)))
    --let lightning = (Ninja {name=(words (x) !! 0), country = ((words (x) !! 1) !! 0), status = "Junior", exam1 = read (words (x) !! 2) :: Float, exam2 = read (words (x) !! 3) :: Float, ability1 = (words (x) !! 4), ability2 = (words (x) !! 5), r = 0}) : lightning
   -- putStrLn (show (name (head (insertNinjas fileLines)))) -- şu an sadece txtnin son satırını alıyor ama lightninge de ekememiş anlamadım
    putStrLn (show (name (head (head (tail (allLists)))))) -- Sasuke, fire
    putStrLn (show (name (head (head (tail (tail (tail (tail (tail (allLists)))))))))) -- Kankuro, wind
    putStrLn (show (name (head (head (tail (tail (tail (tail (tail (tail (allLists))))))))))) -- Midare, water
    putStrLn (show (length allLists))
    printMenu fileLines
    --putStr (name naruto)
    --putStrLn (show (length lise))
