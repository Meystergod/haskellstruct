-- Структура данных: учебная группа; количество студентов; количество девушек. Создать два запроса: 
-- 1. Подсчитать количество групп, состоящих только из юношей.
-- 2. Определить, имеются ли группы, где количества девушек и юношей равны.

import Data.IORef
import Data.Typeable
import System.IO

import Prelude hiding (Functor, fmap, (<$>), Applicative, pure, (<*>))
data Group =  Group {
    groupname :: String,
    students  :: Int,
    girls     :: Int
} deriving Show

-- Functor
class Functor f where
    fmap :: (a -> b) -> f a -> f b
    (<$>) :: (a -> b) -> f a -> f b
    (<$>) = fmap

instance Functor [] where
    fmap = map

instance Functor Maybe where
    fmap _ Nothing = Nothing
    fmap f (Just a) = Just (f a)

-- Applicative functor
class Functor f => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

instance Applicative [] where  
    pure x = [x]  
    fs <*> xs = [f x | f <- fs, x <- xs]  

instance Applicative Maybe where  
    pure = Just  
    Nothing <*> _ = Nothing  
    (Just f) <*> something = fmap f something

g1 = Group {
    groupname = "KI18-16B",
    students = 26,
    girls = 3
}

g2 = Group {
    groupname = "KI18-17/1B",
    students = 30,
    girls = 0
}

g3 = Group {
    groupname = "KI18-17/2B",
    students = 30,
    girls = 15
}

g4 = Group {
    groupname = "KI19-16B",
    students = 22,
    girls = 0
}

g5 = Group {
    groupname = "KI19-17B",
    students = 41,
    girls = 16
}

list :: [Group]
list = [g1, g2, g3, g4, g5]

-- Функция удаление элемента по индексу, где index - индекс элемента списка, начиная с 1, который необходимо удалить.
del :: [Group] -> Int -> [Group]
del list index
    | index < 1 || index > length list = list
    | index == (length list) = init list
    | index == 1 = tail list
    | otherwise = head list : del (tail list) (index - 1)

-- Функция добавление элемента, где index - индекс элемента списка, начиная с 1, куда следует добавить элемент.
-- Переменная i - название элемента (структуры), который необходимо добавить. 
add :: [Group] -> Int -> Group -> [Group]
add list index i
    | index < 1 || index > length list + 1 = list
    | index == (length list + 1) = list ++ [i]
    | index == 1 = [i] ++ list
    | otherwise = head list : add (tail list) (index - 1) i

-- Функция замены элемента, где index - индекс элемента списка, начиная с 1, который следует заменить.
-- Переменная i - название элемента (структуры), которой будем заменять index. 
modification :: [Group] -> Int -> Group -> [Group]
modification list index i
    | index < 1 || index > length list + 1 = list
    | index == length list = init list ++ [i]
    | index == 1 = [i] ++ tail list
    | otherwise = add (del list index) index i

-- Функция для проверки условия девушки = юноши.
-- Output 1 - условие выполнилось. 
-- Output 0 - условие не выполнилось.
-- Функция filter является функцией высшего порядка, потому что принимает лямбда-функцию.
equal :: [Group] -> Bool
equal list
    | length (filter (\x -> (students x) - (girls x) == (girls x)) list) /= 0 = True
    | otherwise = False

-- Функция для подсчёта количества групп, где девушки = 0.
-- Output n, где n - количество групп.
-- Функция filter является функцией высшего порядка, потому что принимает лямбда-функцию.
zero :: [Group] -> Int
zero list = length (filter (\x -> (girls x) == 0) list)

clear :: [Char]
clear = ""

-- Меню программы
main = do
    mainMenu list

mainMenu :: ([Group]) -> IO ()
mainMenu groups = do
    putStrLn "1.Add"
    putStrLn "2.Del"
    putStrLn "3.Edit"
    putStrLn "4.Show"
    putStrLn "5.Get the number of groups without girls"
    putStrLn "6.Check for groups where the number of girls and boys is equal"
    putStrLn "7.Save into file"
    putStrLn "8.Load from file"
    putStrLn "9.Exit"
    putStr "> "
    choice <- getLine
    case choice of
        "1" -> do
            groups <- menuAdd groups
            mainMenu groups
        "2" -> do
            if length groups > 0
                then do
                groups <- menuDelete groups
                mainMenu groups
                else do
                putStrLn "Mo elements."
                mainMenu groups
        "3" -> do
            if length groups > 0
                then do
                    groups <- menuEdit groups
                    mainMenu groups
                else do
                    putStrLn "Mo elements."
                    mainMenu groups
        "4" -> do
            if length groups > 0
                then do
                    showMenu groups
                    mainMenu groups
                else do 
                    putStrLn "Mo elements."
                    mainMenu groups
        "5" -> do
            let count = zero groups
            putStrLn $ "Total groups: " ++ show count
            putStrLn ""
            mainMenu groups
        "6" -> do
            let status = equal groups
            if status == True
                then putStrLn "Such groups exist"
                else putStrLn "Such groups do not exist"
            mainMenu groups
        "7" -> do
            if length groups > 0
                then do
                    saveFile groups
                    putStrLn "Save complete"
                    mainMenu groups
                else do
                    writeFile "groups.txt" clear
                    mainMenu groups
        "8" -> do
            filename <- openFile "groups.txt" ReadMode
            content <- hGetContents filename
            print $ length content
            if length content == 0
                then do
                    putStrLn "Load error, empty file"
                    mainMenu groups
                else do
                    let groups = loadFile content
                    putStrLn "Load complete"
                    mainMenu groups
            hClose filename
        "9" -> do
            putStrLn "End of programm. Bye."
        _ -> do
            putStrLn "Error. Incorrect point of menu. Try again: "
            mainMenu groups

-- Добавление элемента
menuAdd :: [Group] -> IO [Group]
menuAdd groups = do
    putStr "1.Name of group: "
    groupname <- getLine
    putStr "2.Students: "
    line <- getLine
    students <- checkToInt line
    putStr "3.Girls: "
    nGirls <- check students
    putStr "4.Number of position: "
    position <- getPosition $ length groups
    putStrLn ""
    return $ add groups position (Group groupname students nGirls)

-- Удаление элемента
menuDelete :: [Group] -> IO [Group]
menuDelete groups = do
    putStr "Index of element: "
    position <- getPosition $ length groups
    putStrLn ""
    return (del groups position)

-- Редактирование элемента
menuEdit :: [Group] -> IO [Group]
menuEdit groups = do
    putStrLn "For skip field: *skip"
    putStr "Index of element: "
    position <- getPosition $ length groups
    let groupList = groups !! (position - 1)

    putStr "Name of group: "
    line <- getLine
    let groupName = if line == "*skip"
        then (groupname groupList)
        else line

    putStr "Students: "
    newStud <- getLine
    groupStud <- if newStud == "*skip"
        then return $ students groupList
        else checkToInt newStud

    putStr "Girls: "
    newGirls <- getLine
    groupGirls <- if newGirls == "*skip"
        then return $ girls groupList
        else checkToInt newGirls

    if groupGirls > groupStud
        then putStrLn "The number of girls can't be more than the number of students. Try again" >> menuEdit groups
        else return $ modification groups position (Group groupName groupStud groupGirls)

-- Вывод данных
showMenu :: [Group] -> IO ()
showMenu [] = do
    putStr ""
showMenu (g:gs) = do
    putStrLn $ "Name of group: " ++ (groupname g)
    putStrLn $ "Students: " ++ show (students g)
    putStrLn $ "Girls: " ++ show (girls g)
    putStrLn ""
    showMenu (gs)

-- Проверка на введённые данные
checkToInt :: [Char] -> IO Int
checkToInt line = do
    let checker = readMaybe line :: Maybe Int
    case checker of
        Nothing -> do
            putStr "Incorrect number. Thy again: "
            line <- getLine
            checkToInt line
        Just n  -> return n

-- Ввод и проверка индекса
getPosition :: Int -> IO Int
getPosition size = do
    index <- getLine
    let getPos = do
        let pos = readMaybe index :: Maybe Int
        case pos of
            Nothing -> putStr "Incorrect index. Try again: " >> getPosition size
            Just n  -> if n > size || n < 1
                then putStr "Incorrect index. Try again: " >> getPosition size
                else return n
    getPos

-- Проверка на введённые данные при добавлении
check :: Int -> IO Int
check s = do
    line <- getLine
    girls <- checkToInt line
    if girls > s
        then putStrLn "The number of girls can't be more than the number of students. Try again" >> check s
        else return girls

-- Запись данных в файл
saveFile :: [Group] -> IO ()
saveFile groups = do
    str <- getString groups ""
    writeFile "groups.txt" str
    where
        getString :: [Group] -> [Char] -> IO [Char]
        getString list str = do
            let fieldDivider = ","
            let structDivider = ['\n']
            let gr = head list
            let newString = str ++ (groupname gr) ++ fieldDivider
                            ++ (show $ students gr) ++ fieldDivider
                            ++ (show $ girls gr) ++ structDivider
            if length list == 1
                then return newString
                else getString (tail list) newString

-- Загрузка данных из файла
loadFile :: [Char] -> [Group]
loadFile content = do
    let str = content
    let listStrings = listOfStrings str
    let listFields = listOfFields listStrings
    let listGroups = listOfGroups listFields
    result <- listGroups
    return result

-- Вспомогательные функции
listOfStrings :: [Char] -> [[Char]]
listOfStrings str = init $ split str '\n'

listOfFields :: [[Char]] -> [[[Char]]]
listOfFields [str] = [split str ',']
listOfFields listOfStrings = [split (head listOfStrings) ','] ++ listOfFields (tail listOfStrings)

listOfGroups :: [[[Char]]] -> [Group]
listOfGroups listOfFields
    | length listOfFields == 1 = [(Group gname gstud ggirls)]
    | otherwise = (Group gname gstud ggirls) : (listOfGroups $ tail listOfFields)
    where
        gr = head listOfFields
        gname = gr !! 0
        gstud = read $ gr !! 1
        ggirls = read $ gr !! 2

readMaybe :: Read a => [Char] -> Maybe a
readMaybe tmp = case reads tmp of
                    [(x, "")] -> Just x
                    _         -> Nothing

split :: [Char] -> Char -> [[Char]]
split [] delim = [""]
split (c:cs) delim
    | c == delim = "" : rest
    | otherwise = (c : head rest) : tail rest
    where
        rest = split cs delim