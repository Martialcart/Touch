module Touch where

import Data.Time
import Data.List
import Data.Function
import Numeric

type Letter = (Double, Char)

main :: IO ()
main = do   putStr "Touch: press the buttons you want to exercise\n:>"
            input <- getLine
            let letters = sort (uniq input [])
            putStrLn letters
            touch (ml letters) 0

touch :: [Letter] -> Int -> IO ()
touch ((_,l):ls) n = do
    putStr("type " ++ [l] ++ ": ")
    start <- getCurrentTime
    c <- getChar
    end <- getCurrentTime
    let time = conSec (diffUTCTime end start)
    if 190 < wpm ls then putStrLn "\nyou are average"
    else
        if c == l then do
            putStrLn ("   >   " ++ showFFloat (Just 2) (wpm ls) "" ++ "\tcpm  :: " ++ lts ls)
            if 4 < n then            
                rerun ((time ,l):ls) n
            else
                rerun2 ((time / 2,l):ls) n
        else do
            putStrLn ("\nWrong letter " ++ showFFloat (Just 2) (10 + time) "" ++ " seconds penalty")
            rerun2 ((10 + time, l):ls) n

rerun :: [Letter] -> Int -> IO () 
rerun (l:ls) _ = touch (reverse (l:sort ls)) 0

rerun2 :: [Letter] -> Int -> IO ()
rerun2 (l:ls) n = touch (ls ++ [l]) (n + 1)

conSec :: NominalDiffTime -> Double
conSec t = read (init (show t))::Double

ml :: String -> [Letter]
ml xs = [(9,l)| l <- xs]

wpm :: [Letter] -> Double
wpm ls = 60 / (sum [n|(n,_) <- ls] / fromIntegral (length ls))

lts :: [Letter] -> String 
lts ls =    let
                sls = sortBy (flip compare) ls
                letters = [s:showFFloat (Just 2) n "" | (n,s) <- sls]
                worst = unwords (take 9 letters)
            in  
                worst

uniq :: String -> String -> String
uniq [] us = us
uniq (x:xs) us  | x `elem` xs = uniq xs us
                | otherwise = uniq xs (x:us)