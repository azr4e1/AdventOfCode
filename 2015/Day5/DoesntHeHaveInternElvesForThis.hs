-- Santa needs help figuring out which strings in his text file are naughty or nice.
--
-- A nice string is one with all of the following properties:
--
--     It contains at least three vowels (aeiou only), like aei, xazegov, or aeiouaeiouaeiou.
--     It contains at least one letter that appears twice in a row, like xx, abcdde (dd), or aabbccdd (aa, bb, cc, or dd).
--     It does not contain the strings ab, cd, pq, or xy, even if they are part of one of the other requirements.
--
-- For example:
--
--     ugknbfddgicrmopn is nice because it has at least three vowels (u...i...o...), a double letter (...dd...), and none of the disallowed substrings.
--     aaa is nice because it has at least three vowels and a double letter, even though the letters used by different rules overlap.
--     jchzalrnumimnmhp is naughty because it has no double letter.
--     haegwjzuvuyypxyu is naughty because it contains the string xy.
--     dvszwmarrgswjxmb is naughty because it contains only one vowel.
--
-- How many strings are nice?

-- **Part 1**

vowelCheck :: String -> Bool
vowelCheck string = vowelNr >= 3
    where vowels = ['a', 'e', 'i', 'o', 'u']
          check vowel str = if vowel `elem` str then 1 else 0
          vowelNr = foldl (\acc vowel -> acc + check vowel string) 0 vowels

vowelCheck' :: String -> Bool
vowelCheck' string = vowelCheckUtil 0 string >= 3

vowelCheckUtil :: Int -> String -> Int
vowelCheckUtil acc [] = acc
vowelCheckUtil acc (x:xs)
    | x `elem` vowels = vowelCheckUtil (acc+1) xs
    | otherwise = vowelCheckUtil acc xs
    where vowels = ['a', 'e', 'i', 'o', 'u']

doublesCheck :: String -> Bool
doublesCheck string = doublesNr >= 1
    where doubles = [[a, a] | a <- ['a'..'z']]
          allPairs = [[string!!i, string!!(i+1)] | i <- [0..length string - 2]]
          check double allDouble = if double `elem` allDouble then 1 else 0
          doublesNr = foldl (\acc double -> acc + check double allPairs) 0 doubles

forbiddenStringCheck :: String -> Bool
forbiddenStringCheck string = forbiddenNr == 0
    where forbidden = ["ab", "cd", "pq", "xy"]
          allPairs = [[string!!i, string!!(i+1)] | i <- [0..length string - 2]]
          check double allDouble = if double `elem` allDouble then 1 else 0
          forbiddenNr = foldl (\acc double -> acc + check double allPairs) 0 forbidden

stringCheck :: String -> Bool
stringCheck string = vowelCheck' string && doublesCheck string && forbiddenStringCheck string

-- **Part 2**

newCheck1 :: String -> Bool

main :: IO ()
main = do
    content <- readFile "./input.txt"
    let strings = lines content
        result = length $ filter stringCheck strings
    putStrLn ("Number of nice strings: " <> show result)
