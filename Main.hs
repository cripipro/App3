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
goOpt
  :: [[Int]]
  -> Int
  -> Int
  -> Coord
  -> Energy
  -> Set Coord
  -> [Coord]
  -> Map Coord Energy
  -> [([Coord],Energy)]
goOpt forest rows cols pos@(r,c) e visited path bestMap
  | pos == (rows-1, cols-1) = [(path, e)]
  | otherwise = concat
      [ goOpt forest rows cols newPos e' (Set.insert newPos visited) (path ++ [newPos]) bestMap'
      | (dr,dc,cost) <- moves
      , let newPos@(r',c') = (r+dr, c+dc)
      , r' >= 0, r' < rows
      , c' >= 0, c' < cols
      , not (Set.member newPos visited)
      , let runeVal = (forest !! r') !! c'
      , let e' = stepEnergy e cost runeVal
      , e' >= 0
      , let better = case Map.lookup newPos bestMap of
                        Nothing -> True
                        Just prevE -> e' > prevE
      , let bestMap' = if better then Map.insert newPos e' bestMap else bestMap
      , better
      ]
main :: IO ()
main = do
  startTime <- getCurrentTime
  args <- getArgs
  case args of

    [matrixStr, energyStr] ->
      case ( readMaybe matrixStr :: Maybe [[Int]]
           , readMaybe energyStr :: Maybe Int
           ) of
        (Just forest, Just initE) ->
          let rows = length forest
              cols = case forest of
                       (row:) -> length row
                       []      -> 0
              start = (0,0)

              initialRune = (forest !! 0) !! 0
              e0 = stepEnergy initE 0 initialRune
          in
          if e0 < 0
            then putStrLn "No hay camino válido: energía inicial insuficiente en la casilla inicial"
            else do
              let paths = goOpt forest rows cols start e0 (Set.singleton start) [start] Map.empty
              if null paths
                then putStrLn "No existe ningún camino válido que llegue al destino sin agotar la energía."
                else do
                  let (bestPath, bestE) = maximumBy (comparing snd) paths
                  putStrLn $ "Mejor camino: " ++ show bestPath
                  putStrLn $ "Energía final: " ++ show bestE
              endTime <- getCurrentTime
              putStrLn $ "Tiempo de ejecución: " ++ show (diffUTCTime endTime startTime)

         -> putStrLn "Error: no pude parsear la matriz o la energía. Ejemplo de uso:\n  App3 "[[2,-3,1,0],[4,0,-2,3]]" 12"

    _ -> putStrLn "Uso: App3 <matriz> <energiaInicial>\nEjemplo:\n  App3 "[[2,-3,1,0,2,3],[-5,4,-2,1,0,-4],[1,3,0,-3,2,2],[2,-1,4,0,-5,1],[0,2,-3,3,4,-1],[1,0,2,-2,1,5]]" 12"
