import Text.Regex.Posix
import Data.Char (isLetter, isSpace, isDigit, isAscii)
import Data.Bool (Bool)
import Data.List
import Data.Map (Map)
import System.Random (randomRIO)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import Data.List.Split (splitOneOf)

type Bigram = (String, String)
type Dictionary = Map.Map String [String]

--task 1 -------


-- Разделение текста на предложения
splitText :: String -> [[String]]
splitText text = [words  x | x <- splitOneOf ".!?;:()]*[.!?;:()" text]


-- Очистка строки от нежелательных символов
-- Фильтруем символы, оставляя только буквы (isLetter), исключая цифры (not isDigit), и проверяем, что символ ASCII (isAscii)
cleanString :: String -> String
cleanString = filter (\c -> isLetter c && not (isDigit c) && isAscii c)


-- Очистка предложения от пустых строк
-- Применяем cleanString ко всем элементам списка wordList, затем фильтруем пустые строки
cleanSentence :: [String] -> [String]
cleanSentence wordList = filter (\s -> s /= "") (map cleanString wordList)


-- Очистка текста от пустых предложений
cleanText :: [[String]] -> [[String]]
cleanText text = filter (\el -> el /= []) (map cleanSentence text)


-- задание 2 

getBigrams :: [[String]] -> [Bigram]
getBigrams sentences = concatMap extractBigrams sentences
		where 
					extractBigrams words = zip words (tail words)



-- создание словаря 
createDict :: [Bigram] -> Dictionary -> Dictionary
createDict [] dict = dict 
createDict ((word1, word2):rest) dict = createDict rest updateDict
	where updateDict = Map.insertWith (\new old -> old ++ filter (`notElem` old) new) word1 [word2] dict
		-- проверка на входимость  
		

-- запись словаря в файл 
writeDictionaryToFile :: FilePath -> Dictionary -> IO ()
writeDictionaryToFile filePath dict = writeFile filePath (dictionaryToString dict)
	where dictionaryToString dict =  unlines [key ++ ": " ++ valuesToString (dict Map.! key) | key <- Map.keys dict]
		where valuesToString = unwords


--чтение словаря из файла 
readDictFromFile :: FilePath -> IO Dictionary
readDictFromFile filePath = do 
	content <- readFile filePath
	return (stringToDictionary content)


-- Преобразование строки в словарь
stringToDictionary :: String -> Dictionary
stringToDictionary = Map.fromList . map parseKeyValuePair . lines
  where
    parseKeyValuePair :: String -> (String, [String])
    parseKeyValuePair line =
      let (key, valuesStr) = break (== ':') line
          values = words (drop 1 valuesStr)
      in (strip key, values)

--  очистка от пробелов по краям
strip :: String -> String 
strip  = f . f
  where
    f = reverse . dropWhile (== ' ')


-- задание 3 
-- генерация текста 

-- Функция для получения случайного слова по ключу из словаря

getRandomWord :: String -> Dictionary -> IO String
getRandomWord key dict =
  case Map.lookup key dict of
    Just words -> do
      randomIndex <- randomRIO (0, length words - 1)
      return (words !! randomIndex)
    Nothing -> return " "


generateText :: Dictionary -> String -> String -> Int -> IO String
generateText _ _ curText 0 = return curText
generateText dict curKey curText size = do
  newWord <- getRandomWord curKey dict
  if newWord == " "
    then return (curText)
    else generateText dict newWord (curText ++ newWord ++ " ") (size - 1)


loop :: Dictionary -> IO ()
loop dict = do
  putStrLn "enter  word: (ctrl+c for  finish )"
  word <- getLine
  randomSize <- randomRIO (2 :: Int, 20 :: Int)
  text <- generateText dict word (word++" ") randomSize
  putStrLn $ "Text: " ++ show text ++ "\nSize: " ++ show randomSize
  loop dict
  putStrLn $ "Text: " ++ show word ++ "\nSize: " ++ show randomSize



main :: IO ()
main = do
  let fileName = "tolkien_the_fellowship"
  let filePath = "C:\\Users\\alex\\Downloads\\05_pr_task\\" ++ "dictionary_" ++ fileName ++ ".txt"

  putStrLn $ "Your filename is: " ++ show fileName

  putStrLn "1 create a dictionary \n2 use a ready-made dictionary\n"
  choice <- getLine

  if choice == "1"  then do
    text <- readFile ("C:\\Users\\alex\\Downloads\\05_pr_task\\" ++ fileName ++ ".txt")
    print "text <- readFile"
    



    let sentences = cleanText $ splitText text
    



    
    
    let bigrams = getBigrams sentences
    let dictionary = createDict bigrams Map.empty
    
    putStrLn (take 100 (show dictionary))
    print $ "Make Dictionary"

    -- Запись словаря в файл
    print $ "Writing into file: " ++ show filePath ++ "..."
    writeDictionaryToFile filePath dictionary
    
    print $ "Done."
    -- putStrLn (show dictionary)
    -- loop dictionary
  else do
    print $ "reading a dictionary from a file"
    -- Чтение словаря из файла
  dict <- readDictFromFile filePath
  --putStrLn (show dict)
  loop dict
