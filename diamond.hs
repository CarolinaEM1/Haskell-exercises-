import Data.List (intersperse)

-- Genera la mitad superior del diamante
upperHalf :: Char -> [String]
upperHalf c = map (\x -> line x c) ['A'..c]

-- Genera la mitad inferior del diamante (sin la línea del medio)
lowerHalf :: Char -> [String]
lowerHalf c = reverse (init (upperHalf c))

-- Genera una línea específica del diamante
line :: Char -> Char -> String
line currentChar maxChar = intersperse ' ' $ outerSpaces ++ [currentChar] ++ middleSpaces ++ [currentChar] ++ outerSpaces
  where
    outerSpaces = replicate (ord maxChar - ord currentChar) ' '
    middleSpaces = if currentChar == 'A'
                   then ""
                   else replicate ((ord currentChar - ord 'A') * 2 - 1) ' '

-- Genera el diamante completo
diamond :: Char -> [String]
diamond c = upperHalf c ++ lowerHalf c

-- Imprime el diamante
printDiamond :: Char -> IO ()
printDiamond c = mapM_ putStrLn (diamond c)

-- Función main para probar con la letra 'C' por ejemplo
main :: IO ()
main = printDiamond 'C'
