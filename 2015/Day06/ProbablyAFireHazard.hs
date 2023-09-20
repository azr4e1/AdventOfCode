-- Because your neighbors keep defeating you in the holiday house decorating contest year after year, you've decided to deploy one million lights in a 1000x1000 grid.
--
-- Furthermore, because you've been especially nice this year, Santa has mailed you instructions on how to display the ideal lighting configuration.
--
-- Lights in your grid are numbered from 0 to 999 in each direction; the lights at each corner are at 0,0, 0,999, 999,999, and 999,0. The instructions include whether to turn on, turn off, or toggle various inclusive ranges given as coordinate pairs. Each coordinate pair represents opposite corners of a rectangle, inclusive; a coordinate pair like 0,0 through 2,2 therefore refers to 9 lights in a 3x3 square. The lights all start turned off.
--
-- To defeat your neighbors this year, all you have to do is set up your lights by doing the instructions Santa sent you in order.
--
-- For example:
--
-- turn on 0,0 through 999,999 would turn on (or leave on) every light.
-- toggle 0,0 through 999,0 would toggle the first line of 1000 lights, turning off the ones that were on, and turning on the ones that were off.
-- turn off 499,499 through 500,500 would turn off (or leave off) the middle four lights.
-- After following the instructions, how many lights are lit?

-- **Part 1**
import qualified Data.Map as Map
import Data.List (foldl')
import qualified Data.Set as Set

data Range = Range { firstCorner :: Corner
                   , secondCorner ::Corner
                   } deriving Show
data Instruction = Instruction { instruction :: String
                               , range :: Range
                               } deriving Show
type Corner = (Int, Int)
type Delimiter = Char
type Context = String
type Parsed = [String]

internalLineParser :: Parsed -> Context -> String -> Delimiter -> [String]
internalLineParser parsed "" [] _ = parsed
internalLineParser parsed context [] _ = context:parsed
internalLineParser parsed context (x:string) delimiter =
    internalLineParser newParsed newContext string delimiter
    where
        newParsed = if x == delimiter then context:parsed else parsed
        newContext = if x == delimiter then "" else reverse (x:reverse context)

lineParser :: String -> Delimiter -> [String]
lineParser string delimiter = reverse (internalLineParser [] "" string delimiter)

instructionParser :: String -> Instruction
instructionParser "" = Instruction "" (Range (0,0) (0,0))
instructionParser string =
    case string of
        ('t':'o':'g':'g':'l':'e':' ':rest) ->
            Instruction "toggle" (cornerParser rest)
        ('t':'u':'r':'n':' ':'o':'n':' ':rest) ->
            Instruction "turn on" (cornerParser rest)
        ('t':'u':'r':'n':' ':'o':'f':'f':' ':rest) ->
            Instruction "turn off" (cornerParser rest)
        _ -> Instruction "" (Range (0,0) (0,0))
    where cornerParser :: String -> Range
          cornerParser string = 
              let [corner1, word, corner2] = words string
                  [x1, y1] = map read (lineParser corner1 ',')
                  [x2, y2] = map read (lineParser corner2 ',')
              in Range (x1, y1) (x2, y2)

completeRange :: Range -> [Corner]
completeRange (Range (x1, y1) (x2, y2)) =
    let firstSide = x2 - x1
        secondSide = y2 - y1
    in [(x1+a, y1+b) | a <- [0..firstSide], b <- [0..secondSide]]

instructionExecute :: String -> Int -> Int
instructionExecute string val =
    case string of
        "toggle" -> val + 2
        "turn off" -> if val == 0 then 0 else val - 1
        "turn on" -> val + 1
        _ -> 0

instructionExecuteMap :: Instruction -> Map.Map Corner Int -> Map.Map Corner Int
instructionExecuteMap (Instruction instr range) map =
    let listRange = completeRange range
        updateMap inst map el = Map.insert el
                                           (instructionExecute inst
                                           . maybe 0 id $ Map.lookup el map) map
    -- in Map.filter (>0) (foldl' (updateMap instr) map listRange)
    in Map.filter (>0) $ foldl' (updateMap instr) map listRange

-- instructionExecuteList :: Instruction -> 

instructionExecute' :: String -> Set.Set Corner -> Set.Set Corner -> Set.Set Corner
instructionExecute' instr lightsOn newRange =
    case instr of
        "toggle" -> Set.union (Set.difference lightsOn newRange)
                              (Set.difference newRange lightsOn)
        "turn off" -> Set.difference lightsOn newRange
        "turn on" -> Set.union lightsOn newRange

instructionExecuteMap' :: Instruction -> Set.Set Corner -> Set.Set Corner
instructionExecuteMap' (Instruction instr range) lightsOn =
    let newRange = Set.fromList . completeRange $ range
    in instructionExecute' instr lightsOn newRange

-- **Part 2**
-- instructionExecuteNew :: String -> [(Corner, Int)] -> Set.Set Corner -> [(Corner, Int)]
-- instructionExecuteNew instr lights newRange =
--     case instr of
--         "toggle" -> 

main :: IO ()
main = do
    content <- readFile "./input.txt"
    let instructions = lines content
        parsedInstructions = map instructionParser instructions
        -- initialLightsOn = Set.empty
        -- finalLightsOn = foldl (flip instructionExecuteMap') initialLightsOn parsedInstructions
        -- nrLightsOn = length finalLightsOn
        finalLightsOnNew = sum . Map.elems $ foldl' (flip instructionExecuteMap) Map.empty parsedInstructions
    -- putStrLn ("Number of lights on: " <> show nrLightsOn)
    putStrLn ("Number of lights on new: " <> show finalLightsOnNew)
