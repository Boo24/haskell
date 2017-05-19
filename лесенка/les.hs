import qualified Data.Map as Map
import qualified Data.Set as Set

check_same_len :: String -> String -> Bool
check_same_len w1 w2 = length w1 == length w2

is_one_diff :: Int -> String -> String -> Bool
is_one_diff n "" "" = if (n==1) then True else False
is_one_diff n w1 w2 = if (head w1 /= head w2) then is_one_diff (n+1) (tail w1) (tail w2) else is_one_diff n (tail w1) (tail w2)

get_words_by_len :: Int -> [String] -> [String]
get_words_by_len l word_dict = filter (\w -> length w == l) word_dict

get_neib :: String -> [String] -> [String]
get_neib word word_dict = filter (\w -> is_one_diff 0 w word) word_dict 

update_queue :: [String] -> [String] -> [String] -> [String]
update_queue queue word_dict visited = (tail queue) ++ (get_neib_with_check_visited visited queue word_dict)

update_visited :: [String] -> [String] -> [String] -> [String]
update_visited visited queue words_dict =  visited ++ (get_neib (head queue) words_dict)

get_neib_with_check_visited :: [String] -> [String] -> [String] -> [String]
get_neib_with_check_visited visited queue words_dict = filter (\ x -> not (elem x visited)) (get_neib (head queue) words_dict)

update_parents :: [String] -> String -> Map.Map String String -> Map.Map String String
update_parents neib node parents = if ((length neib)==0) then parents else
    update_parents (tail neib) node (Map.insert (head neib) node parents)


bfs :: [String] -> [String] -> String -> Map.Map String String -> [String] -> Map.Map String String
bfs visited queue finish_word parents word_dict = if finish_word== head queue then parents 
    else bfs (update_visited visited queue word_dict) (update_queue queue word_dict visited) finish_word 
    (update_parents (get_neib_with_check_visited visited queue word_dict) (head queue) parents) word_dict
--bfs visited [] first_word parents word_dict = Map.empty
    
get_trace :: [String] -> String -> String -> Map.Map String String -> [String]
get_trace trace start node parents = if node==start then trace else get_trace (node:trace) start (parents Map.! node) parents


main = do
    s <- readFile "runouns1.txt"
    let dict = lines s
    putStrLn "Введите первое слово: "
    first_word <- getLine
    putStrLn "Введите второе слово: "
    second_word <- getLine
    let good_words = get_words_by_len (length first_word) dict
    let parents = bfs [first_word] [first_word] second_word Map.empty good_words
    putStrLn "Результат: \n\r"
    let trace = ([first_word] ++ (get_trace [] first_word second_word parents))
    putStrLn (unlines(trace))
    