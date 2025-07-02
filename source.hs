data BTree a = Et | Bt a (BTree a) (BTree a)
  deriving (Show, Eq)

extend :: [a] -> Int -> a -> [a]
extend xs n c = xs ++ (replicate n c)

present_tree :: (Show a, Eq a) => BTree a -> Int -> Int -> [String]
present_tree Et _ _ = [""]
present_tree (Bt x lt rt) dx dy = first_line:(empty_lines ++ (combine max_left_len max_right_len left_res right_res))
  where
    show_x :: String
    show_x = show x

    max_left_len :: Int
    max_left_len = maximum (0:map length left_res)

    max_right_len :: Int
    max_right_len = maximum (0:map length right_res)

    empty_lines :: [String]
    empty_lines = if (lt == Et) && (rt == Et) then [] else replicate dy ""

    extend_left :: [String] -> [String]
    extend_left = map (\text -> (replicate dx ' ') ++ text)

    extend_right :: [String] -> [String]
    extend_right = map (\text -> text ++ (replicate dx ' '))

    left_res :: [String]
    left_res = extend_right (present_tree lt dx dy)

    right_res :: [String]
    right_res = extend_left (present_tree rt dx dy)

    first_line :: String
    first_line = (replicate max_left_len ' ') ++ show_x ++ (replicate max_right_len ' ')

    middle_section :: String
    middle_section = (replicate (length show_x) ' ')

    pad_both :: Int -> Int -> String -> String -> String
    pad_both l_len r_len l r = extend l l_len ' ' ++ middle_section ++ extend r r_len ' '

    combine :: Int -> Int -> [String] -> [String] -> [String]
    combine _ _ [] [] = []
    combine l_len r_len (l:ls) [] = (pad_both (l_len - length l) r_len l ""):combine l_len r_len ls []
    combine l_len r_len [] (r:rs) = (pad_both l_len (r_len - length r) "" r):combine l_len r_len [] rs
    combine l_len r_len (l:ls) (r:rs) = (pad_both (l_len - length l) (r_len - length r) l r):combine l_len r_len ls rs


example :: BTree Int
example = Bt 10 (Bt 20 (Bt 4 Et Et) (Bt 5 Et Et)) (Bt 30 Et (Bt 60 Et Et))
