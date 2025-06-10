# Informe de Diseño – App #3: El Bosque de las Runas Mágicas

## Objetivo General

Desarrollar un programa en Haskell que permita calcular el camino óptimo para un mago que debe atravesar una matriz de runas, gestionando la energía disponible y los costos de movimiento. Se aplicaron conceptos funcionales y estructuras de datos eficientes para explorar caminos, optimizar recursos y evitar revisitas, utilizando recursión, manejo de conjuntos y mapas para mejorar el rendimiento.

## Estructura del Proyecto

```bash
App3/
├── src/
│   └── Main.hs
```

El proyecto consta principalmente de un único archivo `Main.hs` que contiene toda la lógica del programa. Se implementan funciones puras para la exploración de caminos, cálculo de energía y validación de restricciones.

## Componentes Principales

- **Main.hs**
  - Definición de `Coord` (coordenadas) y `Energy` (energía del mago).
  - Lista de movimientos permitidos con sus costos.
  - Funciones de búsqueda de caminos: `go` y `goOpt`.

- **Funciones de recorrido (`go` y `goOpt`)**
  - Exploran recursivamente todos los caminos posibles hasta la celda final.
  - Descartan rutas que agotan la energía o repiten celdas.
  - `goOpt` memoriza la mejor energía alcanzada en cada celda para podar trayectorias peores.

- **stepEnergy**
  - Calcula la energía remanente tras cada movimiento.
  - Considera:
    - **Coste de movimiento** (0 ó 2 unidades).
    - **Penalizaciones**: −3 unidades si la runa es 0 (trampa).
    - **Incremento**: valor de la runa visitada.

- **Función `main`**
  - Lee argumentos (matriz de runas y energía inicial).
  - Aplica `goOpt` para hallar el camino óptimo.
  - Imprime el resultado y estadísticas de tiempo de ejecución.

## Diseño y Funcionalidades

### Movimientos Permitidos

| Dirección | Desplazamiento | Coste adicional |
| --------- | -------------- | --------------- |
| Derecha   | (0, +1)        | 0               |
| Abajo     | (+1, 0)        | 0               |
| Diagonal  | (+1, +1)       | 2               |
| Izquierda | (0, −1)        | 0               |
| Arriba    | (−1, 0)        | 0               |

### Gestión de Energía

- Cada movimiento descuenta energía igual al coste.
- Si la runa vale 0 (trampa), resta 3 unidades adicionales.
- Se suma el valor de la runa de la celda entrante.

### Exploración y Poda de Caminos

- **Set `Coord`**: evita revisitar celdas.
- **Map `Coord → Energy`**: registra la mejor energía lograda en cada posición para podar rutas subóptimas.
- Objetivo: maximizar la energía remanente al llegar a `(rows − 1, cols − 1)`.

### Estructuras de Datos Utilizadas

- **`Set Coord`**: rastrear celdas visitadas y evitar ciclos.
- **`Map Coord Energy`**: memorizar la mejor energía por celda.
- **Listas**: representar la matriz de runas y secuencias de movimientos.

## Reflexiones Finales

- **Poda inteligente**: sin ella, la búsqueda crece exponencialmente. Registrar la mejor energía por celda redujo drásticamente la complejidad.
- **Cálculo preciso de energía**: `stepEnergy` maneja penalizaciones (trampas), recompensas (runas) y diferentes costes de movimiento.
- **Validación de entrada**: uso de `readMaybe` para leer y verificar argumentos por línea de comandos.
- El proyecto reforzó conceptos de programación funcional (recursión, inmutabilidad, memoización) y permitió evaluar rendimiento con distintos escenarios.

## Uso de Herramientas de IA

Durante el desarrollo se utilizó ChatGPT para:

- Mejorar la estructura y legibilidad del código Haskell.
- Resolver dudas sobre optimización usando funciones y estructuras propias de Haskell.
- Consultar ejemplos de recursión y técnicas de poda.
  Todas las sugerencias fueron revisadas y adaptadas al estilo y requisitos del proyecto.

## Integrantes

- **Cristóbal Meza Palacios** — crmeza@alumnos.uai.cl
- **Vasco Vasquez Ramirez** — vasvasquez@alumnos.uai.cl
- **Daniela Cuminao Embry** — dcuminao@alumnos.uai.cl
- **Benjamin Garcia Muñoz** — benjamigarcia@alumnos.uai.cl
