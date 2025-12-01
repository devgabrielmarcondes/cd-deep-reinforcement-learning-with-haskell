module Main where

import System.Random
import Data.List (maximumBy, elemIndex, intercalate, transpose)
import Data.Ord (comparing)
import Text.Printf (printf)
import Control.Monad (when)

-- ==========================================
-- 1. Módulo de Álgebra Linear (Rede Neural Engine)
-- ==========================================

-- Define uma Matriz como lista de listas de Doubles
type Matrix = [[Double]]
type Vector = [Double]

-- Cria um vetor One-Hot (Ex: tamanho 5, índice 2 -> [0,0,1,0,0])
toOneHot :: Int -> Int -> Vector
toOneHot idx size = [if i == idx then 1.0 else 0.0 | i <- [0..size-1]]

-- Soma de Vetores: v1 + v2
vecAdd :: Vector -> Vector -> Vector
vecAdd = zipWith (+)

-- Multiplicação de Vetor por Escalar: k * v
vecScale :: Double -> Vector -> Vector
vecScale k = map (*k)

-- Produto Escalar (Dot Product): v1 . v2
dot :: Vector -> Vector -> Double
dot v1 v2 = sum (zipWith (*) v1 v2)

-- Forward Pass: Matriz * Vetor ( W . x )
-- A matriz W tem dimensões [NumActions x NumStates]
matVecMul :: Matrix -> Vector -> Vector
matVecMul matrix vector = map (`dot` vector) matrix

-- Atualização de Pesos (Outer Product para Gradiente)
-- ΔW = LearningRate * ErrorVector * InputVector^T
updateWeights :: Matrix -> Vector -> Vector -> Double -> Matrix
updateWeights w inputVec errorVec learningRate =
    zipWith updateRow w errorVec
  where
    -- Atualiza uma linha inteira de pesos baseada no erro daquela ação
    updateRow :: Vector -> Double -> Vector
    updateRow rowWeights err = 
        let gradient = vecScale err inputVec -- Input escalado pelo erro
            delta    = vecScale learningRate gradient
        in  vecAdd rowWeights delta

-- ==========================================
-- 2. Configurações
-- ==========================================

data Difficulty = Easy | Medium | Hard deriving (Show, Read, Eq)

data Config = Config {
    cRows :: Int,
    cCols :: Int,
    cWallProb :: Double,
    cEpochs :: Int
}

getConfig :: Difficulty -> Config
getConfig Easy   = Config 5 5 0.15 400
getConfig Medium = Config 7 7 0.25 1000
getConfig Hard   = Config 10 10 0.30 3000

type Position = (Int, Int)
type Maze = [String]

-- A Rede Neural agora é a Matriz de Pesos [4 Ações x N Estados]
type NeuralNetwork = Matrix

-- ==========================================
-- 3. Geração de Labirinto (BFS Validado)
-- ==========================================

isValidBounds :: Int -> Int -> Position -> Bool
isValidBounds r c (x, y) = x >= 0 && x < r && y >= 0 && y < c

hasPath :: Maze -> Position -> Position -> Bool
hasPath maze start end = bfs [start] []
  where
    rows = length maze
    cols = length (head maze)
    bfs [] _ = False
    bfs (curr:queue) visited
        | curr == end = True
        | curr `elem` visited = bfs queue visited
        | otherwise = bfs (queue ++ neighbors) (curr:visited)
      where
        neighbors = [ (r, c) | (dr, dc) <- [(0,1),(0,-1),(1,0),(-1,0)]
                    , let r = fst curr + dr, let c = snd curr + dc
                    , isValidBounds rows cols (r,c), (maze !! r) !! c /= '#' ]

generateRandomGrid :: Config -> IO Maze
generateRandomGrid conf = do
    let r = cRows conf
    let c = cCols conf
    cells <- mapM (\_ -> randomRIO (0.0, 1.0)) [1..(r*c)]
    let toChar prob = if prob < cWallProb conf then '#' else ' '
    let rowsStr = chunk c (map toChar cells)
    let safeMaze = replaceChar rowsStr (0,0) 'P'
    return $ replaceChar safeMaze (r-1, c-1) 'X'

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = take n xs : chunk n (drop n xs)

replaceChar :: [String] -> Position -> Char -> [String]
replaceChar m (r, c) char =
    take r m ++ [take c (m !! r) ++ [char] ++ drop (c+1) (m !! r)] ++ drop (r+1) m

generateSolvableMaze :: Config -> IO Maze
generateSolvableMaze conf = do
    putStrLn "Gerando labirinto..."
    candidate <- generateRandomGrid conf
    if hasPath candidate (0,0) (cRows conf - 1, cCols conf - 1)
        then return candidate
        else generateSolvableMaze conf

-- ==========================================
-- 4. Ambiente e Física
-- ==========================================

getCell :: Maze -> Position -> Char
getCell m (r, c) = (m !! r) !! c

posToIndex :: Int -> Position -> Int
posToIndex nCols (r, c) = r * nCols + c

step :: Maze -> Position -> Int -> (Position, Double, Bool)
step maze (r, c) action
    | currentCell == 'X' = ((r,c), 0, True)
    | otherwise          = (newPos, finalReward, finished)
  where
    currentCell = getCell maze (r, c)
    rows = length maze
    cols = length (head maze)
    (nr, nc) = case action of
        0 -> (r - 1, c) -- Cima
        1 -> (r + 1, c) -- Baixo
        2 -> (r, c - 1) -- Esquerda
        3 -> (r, c + 1) -- Direita
        _ -> (r, c)
        
    valid = isValidBounds rows cols (nr, nc) && (getCell maze (nr, nc) /= '#')
    newPos = if valid then (nr, nc) else (r, c)
    newCell = getCell maze newPos
    finished = newCell == 'X'
    
    finalReward 
        | newCell == 'X' = 100.0
        | not valid      = -5.0
        | otherwise      = -1.0

-- ==========================================
-- 5. Deep Learning (Function Approximation)
-- ==========================================

-- Forward Pass: Recebe vetor de estado, retorna Q-values
predict :: NeuralNetwork -> Vector -> Vector
predict nn inputVec = matVecMul nn inputVec

-- Training Step: Backpropagation Simplificado
trainStep :: NeuralNetwork -> Vector -> Int -> Double -> Vector -> NeuralNetwork
trainStep nn inputVec action reward nextInputVec = 
    let 
        -- 1. Forward Pass (Estado Atual)
        qValues = predict nn inputVec
        currentQ = qValues !! action
        
        -- 2. Forward Pass (Próximo Estado - para Target)
        nextQValues = predict nn nextInputVec
        maxNextQ = maximum nextQValues
        
        -- 3. Cálculo do Erro (TD Error)
        gamma = 0.9
        target = reward + gamma * maxNextQ
        tdError = target - currentQ
        
        -- 4. Vetor de Erro para Backprop
        -- Como é One-Hot na saída também (conceitualmente para Q-Learning),
        -- o erro é 0 para todas as ações exceto a que tomamos.
        errorVec = [if i == action then tdError else 0.0 | i <- [0..3]]
        
        -- 5. Atualização dos Pesos (Backpropagation)
        -- Gradient Descent: W = W + alpha * errorVec * inputVec^T
        learningRate = 0.1
        newNN = updateWeights nn inputVec errorVec learningRate
    in 
        newNN

chooseAction :: NeuralNetwork -> Vector -> Double -> IO Int
chooseAction nn inputVec epsilon = do
    rnd <- randomRIO (0.0, 1.0)
    if rnd < epsilon
        then randomRIO (0, 3) 
        else do
            -- Inferência pela Rede
            let qValues = predict nn inputVec
            return $ fst $ maximumBy (comparing snd) (zip [0..] qValues)

-- ==========================================
-- 6. Loop de Treinamento
-- ==========================================

runEpisode :: Maze -> NeuralNetwork -> Position -> Double -> Double -> Int -> IO (NeuralNetwork, Double)
runEpisode maze nn pos epsilon totalRw steps
    | steps > (length maze * length (head maze) * 3) = return (nn, totalRw - 50)
    | otherwise = do
        let cols = length (head maze)
        let numStates = length maze * cols
        
        -- CONVERSÃO IMPORTANTE: Estado -> Vetor One-Hot
        let sIdx = posToIndex cols pos
        let inputVec = toOneHot sIdx numStates
        
        action <- chooseAction nn inputVec epsilon
        let (nextPos, r, done) = step maze pos action
        
        -- Próximo Estado -> Vetor One-Hot
        let nextSIdx = posToIndex cols nextPos
        let nextInputVec = toOneHot nextSIdx numStates
        
        let newNN = trainStep nn inputVec action r nextInputVec
        
        if done 
            then return (newNN, totalRw + r)
            else runEpisode maze newNN nextPos epsilon (totalRw + r) (steps + 1)

trainLoop :: Maze -> Config -> Int -> NeuralNetwork -> IO NeuralNetwork
trainLoop maze conf epoch nn
    | epoch > cEpochs conf = return nn
    | otherwise = do
        let progress = fromIntegral epoch / fromIntegral (cEpochs conf)
        let epsilon = max 0.05 (1.0 - progress) 
        
        (newNN, reward) <- runEpisode maze nn (0,0) epsilon 0.0 0
        
        when (epoch `mod` (cEpochs conf `div` 10) == 0) $
            printf "Epoca %d/%d | Epsilon: %.2f | Reward: %.2f\n" epoch (cEpochs conf) epsilon reward
            
        trainLoop maze conf (epoch + 1) newNN

-- ==========================================
-- 7. Visualização e Exportação
-- ==========================================

getSolutionPath :: Maze -> NeuralNetwork -> Position -> Int -> IO [Position]
getSolutionPath maze nn pos steps
    | steps > 200 = return []
    | otherwise = do
        let cell = getCell maze pos
        if cell == 'X' then return [pos] else do
            let cols = length (head maze)
            let numStates = length maze * cols
            let inputVec = toOneHot (posToIndex cols pos) numStates
            
            action <- chooseAction nn inputVec 0.0
            let (nextPos, _, _) = step maze pos action
            
            if nextPos == pos 
                then return [pos]
                else do
                    rest <- getSolutionPath maze nn nextPos (steps + 1)
                    return (pos : rest)

exportToJSON :: Maze -> [Position] -> String
exportToJSON maze path = 
    let rows = length maze
        cols = length (head maze)
        mazeStr = "[" ++ intercalate "," (map (\r -> "\"" ++ r ++ "\"") maze) ++ "]"
        pathStr = "[" ++ intercalate "," (map (\(r,c) -> "{\"r\":" ++ show r ++ ",\"c\":" ++ show c ++ "}") path) ++ "]"
    in "{\"rows\": " ++ show rows ++ ", \"cols\": " ++ show cols ++ ", \"maze\": " ++ mazeStr ++ ", \"path\": " ++ pathStr ++ "}"

main :: IO ()
main = do
    putStrLn "=== DEEP RL REAL (Dense Layer) ==="
    putStrLn "Escolha: 1-Facil, 2-Medio, 3-Dificil"
    input <- getLine
    let diff = case input of "1"->Easy; "2"->Medium; "3"->Hard; _->Easy
    let conf = getConfig diff
    
    maze <- generateSolvableMaze conf
    
    -- Inicialização da Rede Neural (Pesos Aleatórios Pequenos)
    -- Dimensão: [4 Ações x N Estados]
    let numStates = cRows conf * cCols conf
    -- Inicializa com valores bem pequenos (0.01) para quebrar simetria
    rndGen <- newStdGen
    let randomWeights = take 4 $ chunk numStates $ randomRs (-0.01, 0.01) rndGen :: Matrix
    
    putStrLn "\nIniciando Treinamento com Rede Neural..."
    finalNN <- trainLoop maze conf 1 randomWeights
    
    putStrLn "\nTreino finalizado! Calculando rota..."
    path <- getSolutionPath maze finalNN (0,0) 0
    
    putStrLn $ "Solucao: " ++ show (length path) ++ " passos."
    writeFile "maze_data.json" (exportToJSON maze path)
    putStrLn "JSON exportado."