module Main where
import System.Posix.Internals (puts)

main :: IO ()
main = rec1


rec1 = putStrLn "hello, world" 

rec2 = putStrLn "this is great"
