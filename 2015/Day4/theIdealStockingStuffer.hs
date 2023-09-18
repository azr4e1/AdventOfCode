-- Santa needs help mining some AdventCoins (very similar to bitcoins) to use as gifts for all the economically forward-thinking little girls and boys.
--
-- To do this, he needs to find MD5 hashes which, in hexadecimal, start with at least five zeroes. The input to the MD5 hash is some secret key (your puzzle input, given below) followed by a number in decimal. To mine AdventCoins, you must find Santa the lowest positive number (no leading zeroes: 1, 2, 3, ...) that produces such a hash.
--
-- For example:
--
--     If your secret key is abcdef, the answer is 609043, because the MD5 hash of abcdef609043 starts with five zeroes (000001dbbfa...), and it is the lowest such number to do so.
--     If your secret key is pqrstuv, the lowest number it combines with to make an MD5 hash starting with five zeroes is 1048970; that is, the MD5 hash of pqrstuv1048970 looks like 000006136ef....
--
-- Your puzzle input is iwrupvqb.

-- **Part 1**
import Data.Digest.Pure.MD5
import qualified Data.ByteString.Lazy as LB
import Data.List (takeWhile)
import Data.Char

packStr :: String -> LB.LazyByteString
packStr = LB.pack . map (fromIntegral . ord)

checkValidityFive :: String -> Int -> Bool
checkValidityFive key coin =
    case show (md5 (packStr (key<>show coin))) of
        '0':'0':'0':'0':'0':_ -> False
        _ -> True
        
        
searchCoinFive :: String -> Int
searchCoinFive key = head coins
    where coins = dropWhile (checkValidityFive key) [1..]

-- **Part 2**
checkValiditySix :: String -> Int -> Bool
checkValiditySix key coin =
    case show (md5 (packStr (key<>show coin))) of
        '0':'0':'0':'0':'0':'0':_ -> False
        _ -> True
        
        
searchCoinSix :: String -> Int
searchCoinSix key = head coins
    where coins = dropWhile (checkValiditySix key) [1..]

main :: IO ()
main = do
    let key = "iwrupvqb"
        coinFive = searchCoinFive key
        coinSix = searchCoinSix key
    putStrLn ("The coin is: " <> show coinFive)
    putStrLn ("The coin is: " <> show coinSix)
