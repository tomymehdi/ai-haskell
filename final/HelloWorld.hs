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
    set f [ on paint := drawNetwork iMiddleLayers]

drawNetwork :: Textual w => w -> DC a -> t -> IO ()
drawNetwork iMiddleLayers dc area = do
    middleLayersString  <- get iMiddleLayers text
    let middleLayersSplited = splitOn "," middleLayersString
    let middleLayers = map (\s -> read s :: Int) middleLayersSplited

    --Draw neurons
    let indexedMiddleLayers = zip [1..] middleLayers
    drawLayers dc ([(0,2)] ++ indexedMiddleLayers)
    --Draw connections
    line dc (Point 150 30) (Point 150 80) []
    line dc (Point 200 30) (Point 150 80) []
    line dc (Point 150 30) (Point 200 80) []
    line dc (Point 200 30) (Point 200 80) []
    line dc (Point 150 80) (Point 175 130) []
    line dc (Point 200 80) (Point 175 130) []

drawLayers :: DC a -> [(Int, Int)] -> IO [[()]]
drawLayers dc layers = mapM (drawCirclesRow dc) layers

drawCirclesRow :: DC a -> (Int, Int) -> IO [()]
drawCirclesRow dc positionCount = do
    let xlist = [1..snd(positionCount)]
    let amount = snd(positionCount) :: Int
    let y = fst(positionCount) :: Int
    let ylist = replicate amount y
    let list = zip xlist ylist
    mapM (drawCircle dc) list

drawCircle :: DC a -> (Int, Int) -> IO ()
drawCircle dc pos = do
    let xPos = fst(pos)
    let yPos = snd(pos)
    circle dc (Point (150+50*xPos) (80+50*yPos)) 10 [brushKind := BrushSolid, brushColor := red]

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
    n <- createNetwork firstLayer middleLayers outputLayer
    mapM_ (putStrLn . show . output n tanh . fst) samples
    putStrLn "------------------"
    let n' = trainNTimes epocs learningRate tanh tanh' n samples
    mapM_ (putStrLn . show . output n' tanh . fst) samples

samples :: Samples Double
samples = [ (fromList [0, 0], fromList [0])
          , (fromList [0, 1], fromList [1])
          , (fromList [1, 0], fromList [1])
          , (fromList [1, 1], fromList [0])
          ]