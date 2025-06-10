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
go
  :: [[Int]]        -- ^ matriz de runas
  -> Int            -- ^ número de filas
  -> Int            -- ^ número de columnas
  -> Coord          -- ^ posición actual
  -> Energy         -- ^ energía actual
  -> Set Coord      -- ^ celdas ya visitadas
  -> [Coord]        -- ^ camino hasta la posición actual
  -> [([Coord],Energy)]
go forest rows cols pos@(r,c) e visited path
  | pos == (rows-1, cols-1) = [(path, e)]
  | otherwise = concat
      [ go forest rows cols newPos e' (Set.insert newPos visited) (path ++ [newPos])
      | (dr,dc,cost) <- moves
      , let newPos@(r',c') = (r+dr, c+dc)
      , r' >= 0, r' < rows
      , c' >= 0, c' < cols
      , not (Set.member newPos visited)
      , let runeVal = (forest !! r') !! c'
      , let e' = stepEnergy e cost runeVal
      , e' >= 0
      ]
