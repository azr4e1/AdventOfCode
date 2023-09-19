import System.IO

data Action = TurnOn | TurnOff | Toggle deriving (Show, Eq)
data Instruction = Instruction Action (Int, Int) (Int, Int) deriving (Show)

parseInstruction :: String -> Instruction
parseInstruction s =
  let ws = words s
      action = case head ws of
                 "turn" -> if ws !! 1 == "on" then TurnOn else TurnOff
                 "toggle" -> Toggle
      coordPairs = map (\x -> read ( "("++x++")" ) :: (Int, Int)) $ filter (\x -> ',' `elem` x) ws
  in Instruction action (head coordPairs) (last coordPairs)

applyInstruction :: Instruction -> [[Bool]] -> [[Bool]]
applyInstruction (Instruction action (x1, y1) (x2, y2)) grid =
  [ [ updateLight action (x, y) light
    | (light, x) <- zip row [0..]]
  | (row, y) <- zip grid [0..]]
  where
    updateLight a (x, y) l
      | x >= x1 && x <= x2 && y >= y1 && y <= y2 = case a of
          TurnOn -> True
          TurnOff -> False
          Toggle -> not l
      | otherwise = l

countLitLights :: [[Bool]] -> Int
countLitLights = sum . map (length . filter id)

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let instructions = map parseInstruction $ lines contents
      initialGrid = replicate 1000 $ replicate 1000 False
      finalGrid = foldl (flip applyInstruction) initialGrid instructions
  print $ countLitLights finalGrid
