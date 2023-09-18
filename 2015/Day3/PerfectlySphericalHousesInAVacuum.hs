-- Santa is delivering presents to an infinite two-dimensional grid of houses.
--
-- He begins by delivering a present to the house at his starting location, and then an elf at the North Pole calls him via radio and tells him where to move next. Moves are always exactly one house to the north (^), south (v), east (>), or west (<). After each move, he delivers another present to the house at his new location.
--
-- However, the elf back at the north pole has had a little too much eggnog, and so his directions are a little off, and Santa ends up visiting some houses more than once. How many houses receive at least one present?
--
-- For example:
--
--     > delivers presents to 2 houses: one at the starting location, and one to the east.
--     ^>v< delivers presents to 4 houses in a square, including twice to the house at his starting/ending location.
--     ^v^v^v^v^v delivers a bunch of presents to some very lucky children at only 2 houses.
-- **Part 1**
import Data.List (nub)

houseVisited :: (Int, Int) -> Char -> (Int, Int)
houseVisited (x, y) nextMove = case nextMove of
    'v' -> (x, y-1)
    '^' -> (x, y+1)
    '<' -> (x-1, y)
    '>' -> (x+1, y)
    _ -> (x, y)

listHouseVisited :: String -> [(Int, Int)]
listHouseVisited = foldl (\acc@(x:rest) move -> houseVisited x move:acc) [(0, 0)]

-- **Part 2**

main :: IO ()
main = do
    directions <- readFile "./input.txt"
    let listVisitedUnique = nub (listHouseVisited directions)
    putStrLn ("Houses that received at least one present: " <> show (length listVisitedUnique))
    let directionsSanta = [directions !! el | el <- [0,2..length directions-1]]
        directionsRoboSanta = [directions !! el | el <- [1,3..length directions-1]]
        santaVisited = listHouseVisited directionsSanta
        roboSantaVisited = listHouseVisited directionsRoboSanta
        allVisited = santaVisited ++ roboSantaVisited
        listAllVisitedUnique = nub allVisited
    putStrLn ("All unique houses visited by both Santa and RoboSanta: " <> show (length listAllVisitedUnique))
