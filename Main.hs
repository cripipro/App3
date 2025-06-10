module Main where

-import System.Environment (getArgs)
+-- (imports vendrán en commits posteriores)

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

-- | Calcula la nueva energía después de entrar en una celda:
--   - resta el coste de movimiento
--   - resta 3 si la runa vale 0 (trampa)
--   - suma el valor de la runa
 stepEnergy :: Energy -> Energy -> Int -> Energy
 stepEnergy e moveCost runeVal =
     let trapCost = if runeVal == 0 then 3 else 0
     in e - moveCost - trapCost + runeVal

