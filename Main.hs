{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import System.Environment (getArgs)
import Data.Set  (Set)
import qualified Data.Set  as Set
import Data.List (maximumBy)
import Data.Ord  (comparing)
import Text.Read (readMaybe)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Time (getCurrentTime, diffUTCTime)

-- | Coordenada en la matriz (fila, columna)
type Coord  = (Int, Int)
-- | Energía del mago
type Energy = Int

-- | Todos los movimientos permitidos:
--   (deltaFila, deltaColumna, costeExtra)
moves :: [(Int,Int,Energy)]
moves =
  [ ( 0,  1, 0)  -- derecha
  , ( 1,  0, 0)  -- abajo
  , ( 1,  1, 2)  -- diagonal (coste +2)
  , ( 0, -1, 0)  -- izquierda
  , (-1,  0, 0)  -- arriba
  ]

stepEnergy :: Energy -> Energy -> Int -> Energy
stepEnergy e moveCost runeVal =
    let trapCost = if runeVal == 0 then 3 else 0
    in e - moveCost - trapCost + runeVal
