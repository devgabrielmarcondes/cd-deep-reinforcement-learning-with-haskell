module Main where

import System.Random
import Data.List (maximumBy, elemIndex, nub, intercalate)
import Data.Ord (comparing)
import Text.Printf (printf)
import Control.Monad (when)

-- ==========================================
-- 1. Definições de Tipos e Configurações
-- ==========================================

data Difficulty = Easy | Medium | Hard deriving (Show, Read, Eq)

data Config = Config {
    cRows :: Int,
    cCols :: Int,
    cWallProb :: Double, -- Probabilidade de ter parede
    cEpochs :: Int
}

-- Configurações para cada nível
getConfig :: Difficulty -> Config
getConfig Easy   = Config 5 5 0.15 300   -- 5x5, poucas paredes
getConfig Medium = Config 7 7 0.25 800   -- 7x7, paredes médias
getConfig Hard   = Config 10 10 0.30 3000 -- 10x10, muitas paredes, treino longo

type Position = (Int, Int)
type Maze = [String]
type Weights = [[Double]] -- [NumEstados x 4 Ações]

-- ==========================================
-- 2. Geração de Labirinto (Com Validação)
-- ==========================================

-- Verifica se uma posição está dentro do mapa
isValidBounds :: Int -> Int -> Position -> Bool
isValidBounds r c (x, y) = x >= 0 && x < r && y >= 0 && y < c

-- Algoritmo BFS simples para verificar se existe solução
hasPath :: Maze -> Position -> Position -> Bool
hasPath maze start end = bfs [start] []
  where
    rows = length maze
    cols = length (head maze)
    
    bfs [] _ = False -- Fila vazia, não achou
    bfs (curr:queue) visited
        | curr == end = True
        | curr `elem` visited = bfs queue visited
        | otherwise = bfs (queue ++ neighbors) (curr:visited)
      where
        neighbors = [ (r, c) 
                    | (dr, dc) <- [(0,1), (0,-1), (1,0), (-1,0)]
                    , let r = fst curr + dr
                    , let c = snd curr + dc
                    , isValidBounds rows cols (r,c)
                    , (maze !! r) !! c /= '#' -- Não pode ser parede
                    ]

-- Gera um labirinto aleatório candidato
generateRandomGrid :: Config -> IO Maze
generateRandomGrid conf = do
    let r = cRows conf
    let c = cCols conf
    
    -- Gera células aleatórias
    cells <- mapM (\_ -> randomRIO (0.0, 1.0)) [1..(r*c)]
    
    let toChar prob = if prob < cWallProb conf then '#' else ' '
    let rawStr = map toChar cells
    
    -- Quebra em linhas
    let rowsStr = chunk c rawStr
    
    -- Força Posição Inicial (0,0) e Final (r-1, c-1) serem vazias
    let safeMaze = replaceChar rowsStr (0,0) 'P'
    let finalMaze = replaceChar safeMaze (r-1, c-1) 'X'
    
    return finalMaze

-- Função Auxiliar: Divide lista em pedaços (chunks)
chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = take n xs : chunk n (drop n xs)

-- Função Auxiliar: Substitui char em coordenada
replaceChar :: [String] -> Position -> Char -> [String]
replaceChar m (r, c) char =
    take r m ++ 
    [take c (m !! r) ++ [char] ++ drop (c+1) (m !! r)] ++ 
    drop (r+1) m

-- Loop que tenta gerar até achar um solúvel
generateSolvableMaze :: Config -> IO Maze
generateSolvableMaze conf = do
    putStrLn "Gerando labirinto..."
    candidate <- generateRandomGrid conf
    let start = (0, 0)
    let end   = (cRows conf - 1, cCols conf - 1)
    
    if hasPath candidate start end
        then return candidate
        else generateSolvableMaze conf -- Tenta de novo (Recursão)

-- ==========================================
-- 3. Lógica do Jogo (Environment)
-- ==========================================

getCell :: Maze -> Position -> Char
getCell m (r, c) = (m !! r) !! c

-- Transforma Posição em Índice para a Rede Neural
posToIndex :: Int -> Position -> Int
posToIndex nCols (r, c) = r * nCols + c

-- CORREÇÃO APLICADA AQUI NA FUNÇÃO STEP
step :: Maze -> Position -> Int -> (Position, Double, Bool)
step maze (r, c) action
    | currentCell == 'X' = ((r,c), 0, True) -- Segurança extra: se já tá no X, para.
    | otherwise          = (newPos, finalReward, finished)
  where
    currentCell = getCell maze (r, c) -- Definindo a célula atual
    
    rows = length maze
    cols = length (head maze)
    
    -- Tenta mover
    (nr, nc) = case action of
        0 -> (r - 1, c) -- Cima
        1 -> (r + 1, c) -- Baixo
        2 -> (r, c - 1) -- Esquerda
        3 -> (r, c + 1) -- Direita
        _ -> (r, c)
        
    -- Verifica colisão e limites
    valid = isValidBounds rows cols (nr, nc) && (getCell maze (nr, nc) /= '#')
    newPos = if valid then (nr, nc) else (r, c)
    
    newCell = getCell maze newPos
    finished = newCell == 'X'
    
    -- Sistema de Recompensa
    finalReward 
        | newCell == 'X' = 100.0       -- GRANDE recompensa ao achar
        | not valid      = -5.0        -- Punição média por bater na parede
        | otherwise      = -1.0        -- Punição leve por andar

-- ==========================================
-- 4. Rede Neural & Treinamento
-- ==========================================

predict :: Weights -> Int -> [Double]
predict w idx = w !! idx

trainStep :: Weights -> Int -> Int -> Double -> Int -> Weights
trainStep w sIdx action reward nextSIdx = 
    let 
        qCurr = (w !! sIdx) !! action
        qNextMax = maximum (w !! nextSIdx)
        
        -- Hiperparâmetros
        gamma = 0.9
        alpha = 0.1
        
        target = reward + gamma * qNextMax
        diff = target - qCurr
        newVal = qCurr + alpha * diff
        
        -- Atualiza matriz
        oldRow = w !! sIdx
        newRow = take action oldRow ++ [newVal] ++ drop (action + 1) oldRow
    in 
        take sIdx w ++ [newRow] ++ drop (sIdx + 1) w

chooseAction :: Weights -> Int -> Double -> IO Int
chooseAction w idx epsilon = do
    rnd <- randomRIO (0.0, 1.0)
    if rnd < epsilon
        then randomRIO (0, 3) 
        else return $ fst $ maximumBy (comparing snd) (zip [0..] (predict w idx))

-- Loop de 1 Episódio
runEpisode :: Maze -> Weights -> Position -> Double -> Double -> Int -> IO (Weights, Double)
runEpisode maze w pos epsilon totalRw steps
    | steps > (length maze * length (head maze) * 3) = return (w, totalRw - 50) -- Aborta se demorar demais
    | otherwise = do
        let cols = length (head maze)
        let sIdx = posToIndex cols pos
        
        action <- chooseAction w sIdx epsilon
        let (nextPos, r, done) = step maze pos action
        let nextSIdx = posToIndex cols nextPos
        
        let newW = trainStep w sIdx action r nextSIdx
        
        if done 
            then return (newW, totalRw + r)
            else runEpisode maze newW nextPos epsilon (totalRw + r) (steps + 1)

-- Loop Principal de Treino
trainLoop :: Maze -> Config -> Int -> Weights -> IO Weights
trainLoop maze conf epoch w 
    | epoch > cEpochs conf = return w
    | otherwise = do
        -- Epsilon Decay: Começa alto, termina baixo
        let progress = fromIntegral epoch / fromIntegral (cEpochs conf)
        let epsilon = max 0.05 (1.0 - progress) 
        
        (newW, reward) <- runEpisode maze w (0,0) epsilon 0.0 0
        
        when (epoch `mod` (cEpochs conf `div` 10) == 0) $
            printf "Epoca %d/%d | Epsilon: %.2f | Reward: %.2f\n" epoch (cEpochs conf) epsilon reward
            
        trainLoop maze conf (epoch + 1) newW

-- ==========================================
-- 5. Visualização e Exportação
-- ==========================================

printMaze :: Maze -> Position -> IO ()
printMaze maze (pr, pc) = do
    putStrLn "\n--- Labirinto ---"
    let rows = length maze
    let cols = length (head maze)
    sequence_ [ putStrLn [ charAt r c | c <- [0..cols-1] ] | r <- [0..rows-1] ]
  where
    charAt r c
        | r == pr && c == pc = 'A' -- Agente
        | otherwise          = (maze !! r) !! c

getSolutionPath :: Maze -> Weights -> Position -> Int -> IO [Position]
getSolutionPath maze w pos steps
    | steps > 100 = return [] -- Evita loops infinitos
    | otherwise = do
        let cell = getCell maze pos
        if cell == 'X' 
            then return [pos]
            else do
                let cols = length (head maze)
                let sIdx = posToIndex cols pos
                
                -- Epsilon 0 para jogar sério
                action <- chooseAction w sIdx 0.0
                let (nextPos, _, _) = step maze pos action
                
                if nextPos == pos 
                    then return [pos] -- Travou
                    else do
                        rest <- getSolutionPath maze w nextPos (steps + 1)
                        return (pos : rest)

exportToJSON :: Maze -> [Position] -> String
exportToJSON maze path = 
    let 
        rows = length maze
        cols = length (head maze)
        mazeStr = "[" ++ intercalate "," (map (\r -> "\"" ++ r ++ "\"") maze) ++ "]"
        pathStr = "[" ++ intercalate "," (map (\(r,c) -> "{\"r\":" ++ show r ++ ",\"c\":" ++ show c ++ "}") path) ++ "]"
    in
        "{\"rows\": " ++ show rows ++ ", \"cols\": " ++ show cols ++ ", \"maze\": " ++ mazeStr ++ ", \"path\": " ++ pathStr ++ "}"

-- ==========================================
-- 6. Main
-- ==========================================

main :: IO ()
main = do
    putStrLn "=== DEEP RL MAZE SOLVER ==="
    putStrLn "Escolha a dificuldade (1-Facil, 2-Medio, 3-Dificil):"
    input <- getLine
    let diff = case input of
            "1" -> Easy
            "2" -> Medium
            "3" -> Hard
            _   -> Easy
    
    let conf = getConfig diff
    putStrLn $ "Gerando labirinto nível " ++ show diff ++ "..."
    
    maze <- generateSolvableMaze conf
    
    putStrLn "Labirinto Gerado:"
    mapM_ putStrLn maze
    
    -- Inicializa pesos
    let numStates = cRows conf * cCols conf
    let initWeights = replicate numStates (replicate 4 0.0)
        
    putStrLn "\nIniciando Treinamento..."
    finalWeights <- trainLoop maze conf 1 initWeights
    
    putStrLn "\nTreino finalizado! Gerando solucao..."
    path <- getSolutionPath maze finalWeights (0,0) 0
    
    putStrLn $ "Solucao encontrada com " ++ show (length path) ++ " passos."
    
    let jsonOutput = exportToJSON maze path
    let outputFile = "maze_data.json"
    writeFile outputFile jsonOutput
    putStrLn $ "Dados exportados para " ++ outputFile