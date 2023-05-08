import System.IO  
import System.Environment


--------------------------- a[x][y] = el ----------------------------
replaceNth :: Int -> Int -> Int-> [[Int]] -> [[Int]]
replaceNth x y el grid = take x grid ++ [take y (grid !! x) ++ [el] ++ drop (y + 1) (grid !! x)] ++ drop (x + 1) grid
---------------------------------------------------------------------



------------ Sprawdz pole,kolumne, wiersz, kwadrat ------------------
possible :: Int -> Int -> Int -> [[Int]] -> Bool -- param: x,y,val,grid
possible x y val grid = (grid !! x) !! y == 0 && rowPossible x val grid && columnPossible y val grid && squarePossible x y val grid

rowPossible :: Int -> Int ->[[Int]] -> Bool
rowPossible x val grid = null $ foldl (\acc i -> if i == val then acc ++ [True] else acc) [] (grid !! x)

columnPossible :: Int -> Int -> [[Int]] -> Bool -- param : val
columnPossible y val grid = null $ foldl (\acc i -> if (i !! y) == val then acc ++ [True] else acc) [] grid 

squarePossible :: Int -> Int -> Int -> [[Int]] -> Bool
squarePossible x y val grid = let startX = (x `div` 3) * 3
                                  startY = (y `div` 3) * 3
                                  in (grid !! startX) !! startY /= val && (grid !! startX) !! (startY + 1) /= val && (grid !! startX) !! (startY + 2) /= val &&
                                     (grid !! (startX + 1)) !! startY /= val && (grid !! (startX + 1)) !! (startY + 1) /= val && (grid !! (startX + 1)) !! (startY + 2) /= val &&
                                     (grid !! (startX + 2)) !! startY /= val && (grid !! (startX + 2)) !! (startY + 1) /= val && (grid !! (startX + 2)) !! (startY + 2) /= val
---------------------------------------------------------------------


----------------- Sprawdzenie poprawnosci planszy -------------------
checkerPossible :: Int -> Int -> Int -> [[Int]] -> Bool
checkerPossible x y val grid = ((grid !! x) !! y) == 0 || possible x y val (replaceNth x y 0 grid)

correctGridChecker :: [[Int]] -> Bool
correctGridChecker = correctGridChecker' 0 0

correctGridChecker' :: Int -> Int -> [[Int]] -> Bool
correctGridChecker' x y grid =  if x < 9 && y < 9 then checkerPossible x y ((grid !! x) !! y) grid && correctGridChecker' x (y + 1) grid
                                else (x >= 9 && y >= 9) || (x >= 9) || correctGridChecker' (x + 1) 0 grid 
---------------------------------------------------------------------



------------------Solver -------------------
isFilled :: [[Int]] -> Bool
isFilled  = foldl (\acc row -> if acc then foldl (\accy y -> accy && y /= 0) True row else acc) True 

possibleValueChecker :: Int -> Int -> Int -> [[Int]] -> [[Int]]
possibleValueChecker i j val grid 
                                | val < 10 = if possible i j val grid then 
                                              let tryVal = solver (replaceNth i j val grid) in
                                                  if isFilled tryVal then tryVal 
                                                  else possibleValueChecker i j (val + 1) grid 
                                             else possibleValueChecker i j (val + 1) grid 
                                | otherwise = grid

solver' :: Int -> Int -> [[Int]] -> [[Int]]
solver' i j grid  
    | i < 9 && j < 9 = if ((grid !! i) !! j) == 0 then 
                        let changed = possibleValueChecker i j 1 grid in
                            if changed == grid then grid
                            else solver' i (j + 1) changed
                        else solver' i (j + 1) grid
    | j >= 9 = solver' (i + 1) 0 grid
    | otherwise = grid

solver :: [[Int]] -> [[Int]]
solver = solver' 0 0
---------------------------------------------------------------------



-------------------Rozwiazanie dla kazdej planszy--------------------
solve :: String -> IO() 
solve x =
    let castedGrid = read x :: [[Int]]
    in if correctGridChecker castedGrid then printGrid $ solver castedGrid
       else putStrLn "Niepoprawne sudoku na wejsciu\n"
---------------------------------------------------------------------

wordsGrid :: String -> [String] -- words dla '\n', podziel gridy
wordsGrid [] = [[]]
wordsGrid xs = 
    let line = takeWhile (/='\n') xs
        next = drop (length line + 1) xs
    in if not . null $ next then line : wordsGrid next else [line]

printGrid :: [[Int]] -> IO()
printGrid [] = putStrLn "\n"
printGrid (x:xs) = do print x
                      printGrid xs

-- 0 na planszy = miejsca puste, el od 1 do 9
main = do 
    args <- getArgs
    line <- readFile $ head args  -- pierwszy argument: nazwa pliku
    putStrLn "Rozwiazania:"
    mapM_ solve $ wordsGrid line
-- ./sudoku test1.txt