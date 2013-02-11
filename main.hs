module Main where

import Data.List

data Report = Report {recipe :: Recipe,
                      legal  :: Bool,
                      popcnt :: Integer,
                      cost   :: Integer}
              deriving (Eq, Show)

type Recipe = String  -- seven bits to encode seven ingredients

-- recipes in Gray-code order

gray :: Integer -> [Recipe]
gray n
  | n == 0 = [""]
  | n >  0 = map (++"0") (gray (n-1)) ++
             map (++"1") (reverse (gray (n-1)))

recipes = gray 7

-- recipe analysis and report generation

reports = [Report r (legal' r) (popcnt' r) (cost' r) | r <- recipes]
  
legal_reports = [r | r <- reports, legal r]

legal' :: Recipe -> Bool
-- Use "and" in postfix notation to make sure that the resulting list contains
-- only True values
{-legal' r = take 3 r == "000"-}
legal' r = and (map ($ r) ruleset)

ruleset = [rule0, rule1, rule2, rule3, rule4, rule5]

-- Rule 0: If truffles, then precisely truffles.
rule0 r = truffles r && preciselyTruffles r

-- Rule 1: Either truffles or some meat.
rule1 r = truffles r || (bacon r && ham r && sausage r )

-- Rule 2: Not both peppers and onions.
rule2 r = not (peppers r && onions r)

-- Rule 3: If bacon, then peppers.
rule3 r = not (bacon r) || peppers r

-- Rule 4: If sausage, then onions.
rule4 r = not (sausage r) || onions r

-- Rule 5: If ham, then mushrooms.
rule5 r = not (ham r) || mushrooms r

popcnt' :: Recipe -> Integer
-- number of ingredients
popcnt' r = fromIntegral (length (filter (== '1') r))

type Bit  = Char

type Cost = Integer

cost' :: Recipe -> Cost
cost' r = sum (zipWith (includeCost) costs r)
-- cost' r = sum (map (uncurry includeCost) (zip costs r))

includeCost :: Cost -> Bit -> Cost
includeCost cost bit = if bit == '1' then cost else 0

costs = [1, 2, 4, 8, 16, 32, 64]

-- ingredient encoding

peppers   r = r!!0 == '1'

bacon     r = r!!1 == '1'

ham       r = r!!2 == '1'

sausage   r = r!!3 == '1'

onions    r = r!!4 == '1'

mushrooms r = r!!5 == '1'

truffles  r = r!!6 == '1'

preciselyTruffles = (== "0000001")

-- main processing

type Run  = [Report]  -- sequence of legal reports that satisfy the metarule

-- Approach A: Work directly with Grey-code order

runsA = extractRunsA reports

extractRunsA :: [Report] -> [Run]
extractRunsA as
  | clean == [] = []

  | otherwise   = r : extractRunsA as'
                    where (r, as') = break illegal clean
                          clean    = dropWhile illegal as
                          illegal  = not . legal

-- Approach B: Guess a starting seed of a long Hamming run

runsB = extractRunsB legal_reports

extractRunsB :: [Report] -> [Run]
extractRunsB [] = []
extractRunsB xs = run : extractRunsB xs'
                    where run = buildRun (head xs) (tail xs)
                          xs' = xs \\ run  -- list difference

buildRun :: Report -> [Report] -> Run
buildRun seed reports
  | endOfRun  = [seed]

  | otherwise = seed : buildRun seed' reports'
                  where endOfRun   = successors == []
                        successors = filter (.~. seed) reports
                        reports'   = reports \\ [seed]
                        seed'      = head successors

(.~.) :: Report -> Report -> Bool
-- test recipe pairs for Hamming distance 1
x .~. y = length (filter (== True) (zipWith (==) (recipe x) (recipe y))) == 1

-- print routines

printReports :: [Report] -> IO ()
printReports = putStrLn . unlines . map show

printRecipes :: [Report] -> IO ()
printRecipes = putStrLn . showRecipes

printRuns :: [Run] -> IO ()
printRuns = putStrLn . unlines . map showRecipes

printRuns' :: [Run] -> IO ()
printRuns' = putStrLn . unlines . map showRecipes'

showRecipes :: Run -> String
showRecipes = unlines . map showNames

showRecipes' :: Run -> String
showRecipes' run = (unlines . map showNames) run ++ "  total cost  = $" ++
                   show (sum (map popcnt run)) ++ "\n" ++
                   "  prize money = $" ++ show (length run) ++ " million\n"

type Name = String

showNames :: Report -> String
showNames r = (concat . addEggs . translate . recipe) r ++
              "  (" ++ show (popcnt r) ++ ")" ++
              "  $" ++ show (cost r)

translate :: Recipe -> [Name]
translate = zipWith includeName names

includeName :: Name -> Bit -> Name
includeName name bit = if bit == '1' then name else replicate (length name) ' '

addEggs :: [Name] -> [Name]
addEggs = id  -- fix!

names =
  ["peppers ", "bacon ", "ham ", "sausage ", "onions ", "mushrooms ", "truffles"]

blankline = putStrLn ""

render = putStrLn . (++ "\n")

main = do
  blankline
  -- render       "All reports:"
  -- printReports reports
  render       "First ten recipes with number of ingredients and cost:"
  printRecipes $ take 10 reports
  render       "All legal recipes with number of ingredients and cost:"
  printRecipes legal_reports
  render       "All Gray-code runs:"
  printRuns    runsA
  render       "All Gray-code runs with total cost and prize money:"
  printRuns'   runsA
  render       "All seed-generated runs:"
  printRuns    runsB
  render       "All seed-generated runs with total cost and prize money:"
  printRuns'   runsB
