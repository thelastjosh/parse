import System.Environment
import System.Random
import Data.List
import Data.List.Split

-- parse collects words or "units" in a simple, sequential way based on frequency

-- Definition: a subsequence of [a] is a list of elements of [a] with the same relative order
-- Example: "ad" is a subsequence of "ab cd"
-- note that Data.List has a subsequences function that returns all subsequences of a given list

-- Definition: a substring of [a] is a contiguous subsequence of [a]
-- Example: "ab c" is a substring of "ab cd"

-- for testing, to be removed later
library :: String
library = "She woke with a gasp, not knowing who she was, or where. The smell of blood was heavy in her nostrils... or was that her nightmare, lingering? She had dreamed of wolves again, of running through some dark pine forest with a great pack at her hells, hard on the scent of prey. Half-light filled the room, grey and gloomy. Shivering, she sat up in bed and ran a hand across her scalp. Stubble bristled against her palm. I need to shave before Izembaro sees. Mercy, I’m Mercy, and tonight I’ll be raped and murdered. Her true name was Mercedene, but Mercy was all anyone ever called her... Except in dreams. She took a breath to quiet the howling in her heart, trying to remember more of what she’d dreamt, but most of it had gone already. There had been blood in it, though, and a full moon overhead, and a tree that watched her as she ran. She had fastened the shutters back so the morning sun might wake her. But there was no sun outside the window of Mercy’s little room, only a wall of shifting grey fog. The air had grown chilly... and a good thing, else she might have slept all day. It would be just like Mercy to sleep through her own rape. Gooseprickles covered her legs. Her coverlet had twisted around her like a snake. She unwound it, threw the blanket to the bare plank floor and padded naked to the window. Braavos was lost in fog. She could see the green water of the little canal below, the cobbled stone street that ran beneath her building, two arches of the mossy bridge… but the far end of the bridge vanished in greyness, and of the buildings across the canal only a few vague lights remained. She heard a soft splash as a serpent boat emerged beneath the bridge’s central arch. What hour? Mercy called down to the man who stood by the snake’s uplifted tail, pushing her onward with his pole."

substrings :: [a] -> [[a]]
substrings = concatMap (tail . inits) . tails
-- example: substrings "abc" returns ["a", "ab", "abc", "b", "bc", "c"], but not the empty string

removeSingletons :: [[a]] -> [[a]]
removeSingletons xs = [ x | x <- xs, length x > 1 ]

simpleParse :: [a] -> [[a]]
simpleParse [] = []
simpleParse xs = (removeSingletons . substrings . take 4) xs
-- take the first 4 elements of the string, obtain substrings with length > 1 and < 5

parse :: (Eq a) => [Int] -> [a] -> [[a]]
parse num string = [ x | x <- splitPlaces num string , count (splitPlaces num string) x >= 3 ]
-- example: parse [1,2,3] "abc ab dbc abdc" returns ["ab", "bc"]

count :: (Eq a) => [a] -> a -> Int
count xs x
      | (xs == []) = 0
      | (x == head xs) = 1 + count (drop 1 xs) x
      | otherwise = 0 + count (drop 1 xs) x
-- given an element x and a list xs, counts the number of instances of x in that list
-- example: count ["ab", "ab", "bec", "cd"] "bec" returns 1.

randomlist :: Int -> StdGen -> [Int]
randomlist n = take n . unfoldr (Just . randomR (2,4))

main :: IO ()
main = do
     --num <- randomIO :: IO Int
     seed  <- newStdGen
     let num = randomlist (length library) seed
     -- generate a sequence of random integers x1 x2 .. such that x1 + x2 + .. == length x
     putStrLn $ "These are the split pieces: " ++ show (splitPlaces num library)
     putStrLn $ "These are the parsed words: " ++ show (parse num library)
     putStrLn $ "Minus duplicates: " ++ show ((nub (parse num library)))

-- to add: iterate parse by using old units to build new units
-- preserve the old order of the sentence, somehow
