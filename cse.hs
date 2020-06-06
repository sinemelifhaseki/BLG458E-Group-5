import System.Environment
import Data.List
import Data.Typeable
import System.IO


data Ninja = Ninja {name:: String, country:: Char, status:: String, exam1:: Float, exam2:: Float, ability1:: String, ability2:: String, r:: Int} deriving Show
--status initialized as junior
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

--insertNinjas :: [Char] -> [Ninja]
--insertNinjas contents@(x:xs) = case x of 

--insertNinjas :: [[Char]] -> Ninja
--insertNinjas lines@(x:xs) = case ((words (x)) !! 1) of -- x is a line of txt
--                           "Fire" -> Ninja {name=(words (x) !! 0), country = ((words (x) !! 1) !! 0), status = "Junior", exam1 = read (words (x) !! 2) :: Float, exam2 = read (words (x) !! 3) :: Float, ability1 = (words (x) !! 4), ability2 = (words (x) !! 5), r = 0} ++ fire

--insertNinjas :: [[Char]] -> [Ninja]
--insertNinjas lines@(x:xs) = case xs of 
--                             [x']        -> insertNinja x' 
--                             xs@(x':_)   -> insertNinjas xs

insertNinjas :: [[Char]] -> [[Ninja]]
insertNinjas lines = map insertNinja lines   --send all lines to insertNinja function


insertNinja :: [Char] -> [Ninja]
insertNinja x = case ((xWords !! 1) !! 0) of
                  'F' -> do
                         let temp = (Ninja {name=(xWords !! 0), country = ((xWords !! 1) !! 0), status = "Junior", exam1 = read (xWords !! 2) :: Float, exam2 = read (xWords !! 3) :: Float, ability1 = (xWords !! 4), ability2 = (xWords !! 5), r = 0})
                         fire <- fireAdder temp
                         return fire
                  --'L' -> (Ninja {name=(xWords !! 0), country = ((xWords !! 1) !! 0), status = "Junior", exam1 = read (xWords !! 2) :: Float, exam2 = read (xWords !! 3) :: Float, ability1 = (xWords !! 4), ability2 = (xWords !! 5), r = 0}) : lightning
                  --'N' -> (Ninja {name=(xWords !! 0), country = ((xWords !! 1) !! 0), status = "Junior", exam1 = read (xWords !! 2) :: Float, exam2 = read (xWords !! 3) :: Float, ability1 = (xWords !! 4), ability2 = (xWords !! 5), r = 0}) : wind
                  --'W' -> (Ninja {name=(xWords !! 0), country = ((xWords !! 1) !! 0), status = "Junior", exam1 = read (xWords !! 2) :: Float, exam2 = read (xWords !! 3) :: Float, ability1 = (xWords !! 4), ability2 = (xWords !! 5), r = 0}) : water
                  --'E' -> (Ninja {name=(xWords !! 0), country = ((xWords !! 1) !! 0), status = "Junior", exam1 = read (xWords !! 2) :: Float, exam2 = read (xWords !! 3) :: Float, ability1 = (xWords !! 4), ability2 = (xWords !! 5), r = 0}) : earth
                where 
                   xWords = words (x)
                   
fireAdder :: Ninja -> [Ninja]
fireAdder temp = temp : fire

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
    let naruto = (Ninja {name=(words (x) !! 0), country = ((words (x) !! 1) !! 0), status = "Junior", exam1 = read (words (x) !! 2) :: Float, exam2 = read (words (x) !! 3) :: Float, ability1 = (words (x) !! 4), ability2 = (words (x) !! 5), r = 0}) : fire
    let allLists = insertNinjas fileLines
    putStrLn (show (typeOf (lightning)))
    --let lightning = (Ninja {name=(words (x) !! 0), country = ((words (x) !! 1) !! 0), status = "Junior", exam1 = read (words (x) !! 2) :: Float, exam2 = read (words (x) !! 3) :: Float, ability1 = (words (x) !! 4), ability2 = (words (x) !! 5), r = 0}) : lightning
   -- putStrLn (show (name (head (insertNinjas fileLines)))) -- şu an sadece txtnin son satırını alıyor ama lightninge de ekememiş anlamadım
    putStrLn (show (name (head (head (allLists)))))
    putStrLn (show (length allLists))
    printMenu fileLines
    --putStr (name naruto)
    --putStrLn (show (length lise))
