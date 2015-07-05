module Main where

{-# LANGUAGE ScopedTypeVariables #-}

import Data.Maybe

import Graphics.UI.WX hiding (Event)
import Reactive.Banana
import Reactive.Banana.WX
 
import AI.HNN.FF.Network
import Numeric.LinearAlgebra

import Data.List.Split
import Data.List (replicate)

main :: IO ()
main = start gui
 
gui :: IO ()
gui = do
    f       <- frame [ text := "Neural Network" ]
    
    lEpocs          <- staticText f [ text := "Epocs"]
    lMiddleLayers   <- staticText f [ text := "Middle & output layers"]
    lLearningRate   <- staticText f [ text := "Learning rate"]

    iEpocs          <- textCtrlRich f [ text := "1000" ]
    iMiddleLayers   <- textCtrlRich f [ text := "2,2,1"]
    iLearningRate   <- textCtrlRich f [ text := "0.8"]

    bStart  <- button f [ text := "Start" ]
    bQuit   <- button f [ text := "Quit" ]

    set bStart [ on command := parseInputAndTrain f iEpocs iMiddleLayers iLearningRate]
    set bQuit  [ on command := close f]
                
    set f [ layout := column 25 [ widget lEpocs, widget iEpocs, 
                                  widget lMiddleLayers, widget iMiddleLayers, 
                                  widget lLearningRate, widget iLearningRate, 
                                  widget bStart, widget bQuit ] ]

parseInputAndTrain :: (Paint w, Textual w1, Textual w2, Textual w3) => w -> w1 -> w2 -> w3 -> IO ()
parseInputAndTrain f iEpocs iMiddleLayers iLearningRate = do
    epocsString         <- get iEpocs text
    middleLayersString  <- get iMiddleLayers text
    learningRateString  <- get iLearningRate text
    let epocs = read epocsString :: Int
    let middleLayersSplited = splitOn "," middleLayersString
    let middleLayer = map (\s -> read s :: Int) middleLayersSplited
    let learningRate = read learningRateString :: Double
    trainNeuralNetwork epocs 2 middleLayer 1 learningRate

trainNeuralNetwork :: Int -> Int -> [Int] -> Int -> Double -> IO()
trainNeuralNetwork epocs firstLayer middleLayers outputLayer learningRate =  do

    xys <- readFile "xy.txt"
    zs <- readFile "z.txt"

    let list1 = parse xys
    let list2 = parse zs
    let list11 = map fromList list1
    let list22 = map fromList list2
    let samples2 = zip list11 list22

    putStrLn "1------------------"
    n <- createNetwork firstLayer middleLayers outputLayer :: IO (Network Double)
    mapM_ (putStrLn . show) samples2
    mapM_ (putStrLn . show . output n tanh . fst) samples2
    putStrLn "2------------------"
    let n' = trainNTimes epocs learningRate tanh tanh' n samples2
    mapM_ (putStrLn . show . output n' tanh . fst) samples2

parse :: String -> [[Double]]
parse a = map (map read . words) (lines a)

oneList :: [a] -> [b] -> [(a, b)]
oneList []     _      = []
oneList (x:xs) (y:ys) = (x, y) : oneList xs ys

samples :: Samples Double
samples = [ (fromList [0, 0], fromList [0])
          , (fromList [0, 1], fromList [1])
          , (fromList [1, 0], fromList [1])
          , (fromList [1, 1], fromList [0])
          ]
    
