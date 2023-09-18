-- The elves are running low on wrapping paper, and so they need to submit an order for more. They have a list of the dimensions (length l, width w, and height h) of each present, and only want to order exactly as much as they need.
--
-- Fortunately, every present is a box (a perfect right rectangular prism), which makes calculating the required wrapping paper for each gift a little easier: find the surface area of the box, which is 2*l*w + 2*w*h + 2*h*l. The elves also need a little extra paper for each present: the area of the smallest side.
--
-- For example:
--
--     A present with dimensions 2x3x4 requires 2*6 + 2*12 + 2*8 = 52 square feet of wrapping paper plus 6 square feet of slack, for a total of 58 square feet.
--     A present with dimensions 1x1x10 requires 2*1 + 2*10 + 2*10 = 42 square feet of wrapping paper plus 1 square foot of slack, for a total of 43 square feet.
--
-- All numbers in the elves' list are in feet. How many total square feet of wrapping paper should they order?

-- **Part 1**
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

wrapCalculator :: [String] -> Int
wrapCalculator [] = 0
wrapCalculator dimensions =
    let dims :: [Int]
        dims = map read dimensions
        x:y:z:[] = dims
        areas = [x*y, x*z, y*z]
        minimumSurf = minimum areas
    in sum areas * 2 + minimumSurf

-- **Part 2**
calculateRibbonLength :: [String] -> Int
calculateRibbonLength [] = 0
calculateRibbonLength dimensions =
    let dims :: [Int]
        dims = map read dimensions
        x:y:z:[] = dims
        perimeters = [2*x + 2*y, 2*x + 2*z, 2*y + 2*z]
    in minimum perimeters + product dims

main :: IO ()
main = do
    content <- readFile "./input.txt"
    let presentDimensions = lines content
        presentDimParsed = [lineParser el 'x' | el <- presentDimensions]
        dimensions = foldl (\acc dims -> acc + wrapCalculator dims) 0 presentDimParsed
    putStrLn ("Dimensions for paper: " <> show dimensions)
    let ribbonLength = foldl (\acc dims -> acc + calculateRibbonLength dims) 0 presentDimParsed
    putStrLn ("Length ribbon: " <> show ribbonLength)
