import JsonParser

main :: IO ()
main = do
    json <- parseFile "C:\\Users\\rhala\\Desktop\\testh.json"
    putStr $ show json
