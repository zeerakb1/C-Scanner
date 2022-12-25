import Distribution.Simple.Program.HcPkg (list)
import System.IO
import GHC.Unicode (isDigit)

main = do
  handle <- openFile "code.txt" ReadMode
  contents <- hGetContents handle
  writeFile "output.txt" (main_code (get_str contents [] []))
  hClose handle



drop_val _ [] = []
drop_val i (x:xs) 
  | i > 0 = drop_val (i - 1) xs
  | otherwise = x : drop_val i xs

get_str [] _ x2 = x2
get_str (x : xs) x1 x2 
  | x == '\n' = get_str xs "" (x2 ++ [x1])
  | otherwise = get_str xs (x1 ++ [x]) x2



main_code [] = []
-- case of only one char, could be operator, variable or delimiter
main_code [x] = 
  if check_var x
    then check_variable [x]
  else if check_delimiter (x !! 0)
    then "<delimiter," ++ [x !! 0] ++ "> " ++ "\n"
    else "<operator," ++ [x !! 0] ++ "> "  ++ "\n"

main_code (x : x2 : xs) =
  -- move to next line string
  if length x == 0
    then main_code (x2 : xs)
    -- case of only one char, could be operator, variable or delimiter
    else if length x < 2
        then 
          if check_var x 
            then check_variable (x : x2 : xs)
            else if check_delimiter (x !! 0)
              then "<delimiter," ++ x ++ "> "  ++ "\n" ++ main_code (x2 : xs)
              else "<operator," ++ x ++ "> "  ++ "\n" ++ main_code (x2 : xs)
        -- more than 1 values in string could be indentifier, comment, keyword, error, contant, multiline
        else if (x !! 0 == ' ')
          then main_code ((drop_val 1 x) : x2 : xs)
          else if (x !! 0 == '/' && x !! 1 == '*')
            then mult_cmnt (x : x2 : xs) False []
                else delimiter (x : x2 : xs)


check_delimiter x = (x == ';' || x == ':' || x == ',')

delimiter ((x1 : xs) : ys) =
  if check_delimiter x1
    then "<delimiter," ++ [x1] ++ "> " ++ main_code ((xs) : ys)
    else operator ((x1 : xs) : ys)



check_op1 x1 = (x1 == '*' || x1 == '%' || x1 == '[' || x1 == ']' || x1 == '{' || x1 == '}' || x1 == '(' || x1 == ')')

check_op2 x1 = (x1 == '+' || x1 == '-' || x1 == '<' || x1 == '>' || x1 == '=' || x1 == '!' || x1 == '&' || x1 == '|' || x1 == '/')

check_op3 x1 x2 = ((x1 == '+' && x2 == '+') || (x1 == '-' && x2 == '-') || (x1 == '<' && x2 == '<') || (x1 == '<' && x2 == '=') || (x1 == '>' && x2 == '>') || (x1 == '>' && x2 == '=') || (x1 == '=' && x2 == '=') || (x1 == '!' && x2 == '=') || (x1 == '&' && x2 == '&') || (x1 == '|' && x2 == '|'))

check_allOp x = x == '+' || x == '*' || x == '-' || x == '/' || x == '%' || x == '<' || x == '>' || x == '=' || x == '!' || x == '|' || x == '&' || x == '[' || x == ']' || x == '(' || x == ')' || x == '{' || x == '}'


operator ((x1 : x2 : xs) : ys) =
  if check_op1 x1
    then "<operator," ++ [x1] ++ "> " ++ main_code ((x2 : xs) : ys)
    else if check_op2 x1
        then
          if check_op3 x1 x2
            then "<operator," ++ [x1] ++ [x2] ++ "> " ++ main_code (xs : ys)
            else if (x1 == '/' && x2 == x1)
                then "<comment," ++ (x1 : x2 : xs) ++ "> " ++ "\n" ++ main_code (ys)
                else "<operator," ++ [x1] ++ "> " ++ main_code ((x2 : xs) : ys)
        else check_keyword ((x1 : x2 : xs) : ys)




mult_cmnt ((x1:x2:xs):ys) check list =
  if (x1 == '/' && x2 == '*')
    then mult_cmnt ((xs):ys) True ("<multiline-comment,/*")
    else if (x1 == '*' && x2 == '/' && check == True)
      then (list ++ "*/> " ++ "\n") ++ main_code ((xs):ys)
      else if (x1 == ' ')
        then mult_cmnt ((x2:xs):ys) check (list ++ " ")
        else mult_cmnt ((x2:xs):ys) check (list ++ [x1])
mult_cmnt (x:xs) check list = mult_cmnt xs check (list ++ x ++ "\n")


-- compare function
compare6 str = 
  ((str !! 0 == 's' && str !! 1 == 't' && str !! 2 == 'r' && str !! 3 == 'i' && str !! 4 == 'n' && str !! 5 == 'g') || 
  (str !! 0 == 'r' && str !! 1 == 'e' && str !! 2 == 't' && str !! 3 == 'u' && str !! 4 == 'r' && str !! 5 == 'n') || 
  (str !! 0 == 'd' && str !! 1 == 'o' && str !! 2 == 'u' && str !! 3 == 'b' && str !! 4 == 'l' && str !! 5 == 'e') || 
  (str !! 0 == 's' && str !! 1 == 't' && str !! 2 == 'r' && str !! 3 == 'u' && str !! 4 == 'c' && str !! 5 == 't'))

comapre5 str = 
  ((str !! 0 == 'f' && str !! 1 == 'l' && str !! 2 == 'o' && str !! 3 == 'a' && str !! 4 == 't') || 
  (str !! 0 == 'b' && str !! 1 == 'r' && str !! 2 == 'e' && str !! 3 == 'a' && str !! 4 == 'k') || 
  (str !! 0 == 'w' && str !! 1 == 'h' && str !! 2 == 'i' && str !! 3 == 'l' && str !! 4 == 'e') || 
  (str !! 0 == 'a' && str !! 1 == 'r' && str !! 2 == 'r' && str !! 3 == 'a' && str !! 4 == 'y') || 
  (str !! 0 == 'f' && str !! 1 == 'a' && str !! 2 == 'l' && str !! 3 == 's' && str !! 4 == 'e') || 
  (str !! 0 == 'c' && str !! 1 == 'l' && str !! 2 == 'a' && str !! 3 == 's' && str !! 4 == 's'))

compare4 str =
  ((str !! 0 == 'c' && str !! 1 == 'h' && str !! 2 == 'a' && str !! 3 == 'r') ||  
  (str !! 0 == 'c' && str !! 1 == 'a' && str !! 2 == 's' && str !! 3 == 'e') || 
  (str !! 0 == 'c' && str !! 1 == 'o' && str !! 2 == 'u' && str !! 3 == 't') || 
  (str !! 0 == 't' && str !! 1 == 'r' && str !! 2 == 'u' && str !! 3 == 'e') || 
  (str !! 0 == 'e' && str !! 1 == 'n' && str !! 2 == 'd' && str !! 3 == 'l') ||
  (str !! 0 == 'b' && str !! 1 == 'o' && str !! 2 == 'o' && str !! 3 == 'l') || 
  (str !! 0 == 'e' && str !! 1 == 'l' && str !! 2 == 's' && str !! 3 == 'e') || 
  (str !! 0 == 'v' && str !! 1 == 'o' && str !! 2 == 'i' && str !! 3 == 'd'))

compare3 str =
  ((str !! 0 == 'f' && str !! 1 == 'o' && str !! 2 == 'r') ||
  (str !! 0 == 'c' && str !! 1 == 'i' && str !! 2 == 'n') ||
  (str !! 0 == 'i' && str !! 1 == 'n' && str !! 2 == 't'))

compare2 str =
  (str !! 0 == 'i' && str !! 1 == 'f')



check_keyword [] = []
check_keyword (x : x2 : xs) =
  if (length x >= 6 && compare6 x)
    then
      if (length x == 6)
        then "<keyword," ++ ([x !! 0] ++ [x !! 1] ++ [x !! 2] ++ [x !! 3] ++ [x !! 4] ++ [x !! 5] ) ++ "> " ++ check_keyword (x2 : xs)
        else "<keyword," ++ ([x !! 0] ++ [x !! 1] ++ [x !! 2] ++ [x !! 3] ++ [x !! 4] ++ [x !! 5] ) ++ "> " ++ check_keyword (drop_val 6 x : x2 : xs)
    else if (length x >= 5 && comapre5 x)
      then
          if (length x == 5)
            then "<keyword," ++ ([x !! 0] ++ [x !! 1] ++ [x !! 2] ++ [x !! 3] ++ [x !! 4]) ++ "> " ++ check_keyword (x2 : xs)
            else "<keyword," ++ ([x !! 0] ++ [x !! 1] ++ [x !! 2] ++ [x !! 3] ++ [x !! 4]) ++ "> " ++ check_keyword (drop_val 5 x : x2 : xs)
        else if (length x >= 4 && compare4 x)
          then
              if (length x == 4)
                then "<keyword," ++ ([x !! 0] ++ [x !! 1] ++ [x !! 2] ++ [x !! 3]) ++ "> " ++ check_keyword (x2 : xs)
                else "<keyword," ++ ([x !! 0] ++ [x !! 1] ++ [x !! 2] ++ [x !! 3]) ++ "> " ++ check_keyword (drop_val 4 x : x2 : xs)
            else if (length x >= 3 && compare3 x)
              then
                  if (length x == 3)
                    then "<keyword," ++ ([x !! 0] ++ [x !! 1] ++ [x !! 2]) ++ "> " ++ check_keyword (x2 : xs)
                    else "<keyword," ++ ([x !! 0] ++ [x !! 1] ++ [x !! 2]) ++ "> " ++ check_keyword (drop_val 3 x : x2 : xs)
                else if (length x >= 2 && compare2 x)
                    then
                      if (length x == 2)
                        then "<keyword," ++ ([x !! 0] ++ [x !! 1]) ++ "> " ++ check_keyword (x2 : xs)
                        else "<keyword," ++ ([x !! 0] ++ [x !! 1]) ++ "> " ++ check_keyword (drop_val 2 x : x2 : xs)
                    else 
                      check_string (x:x2:xs) False "" 


check_string (x : xs) check list  =
  if x !! 0 == '"' && check == False
    then check_string ((drop_val 1 x) : xs) True ("<stringConstant,\"")
    else if x !! 0 == '"' && check == True
      then list ++ "\"> " ++ main_code ((drop_val 1 x) : xs)
      else if check == True 
        then check_string ((drop_val 1 x) : xs) check (list ++ [x !! 0])
        else check_char (x : xs) False ""


check_char (x : xs) check list =
  if x !! 0 == '\'' && check == False
    then check_char ((drop_val 1 x) : xs) True ("<charConstant,'")
    else if x !! 0 == '\'' && check == True
      then list ++ "'> " ++ main_code ((drop_val 1 x) : xs)
      else if check == True 
        then check_char ((drop_val 1 x) : xs) check (list ++ [x !! 0])
        else 
          check_variable (x : xs)


check_type list1 floatt err elements =
  if err == True 
    then "<error," ++ elements ++ "> " ++ main_code list1
    else if floatt == True 
      then "<floatConstant," ++ elements ++ "> " ++ main_code list1
      else "<intConstant," ++ elements ++ "> " ++ main_code list1


check_number (x : xs) elem_list error_check float_check =
  if (x !! 0 == '.')
    then check_number ((drop_val 1 x) : xs) (elem_list ++ [x !! 0]) error_check True
    else if ((x !! 0 >= '0' && x !! 0 <= '9') && (x !! 0 > '6'))
      then check_number ((drop_val 1 x) : xs) (elem_list ++ [x !! 0]) True float_check
      else if (x !! 0 >= '0' && x !! 0 <= '9') 
        then check_number ((drop_val 1 x) : xs) (elem_list ++ [x !! 0]) error_check float_check
        else check_type (x:xs) float_check error_check elem_list


check_var str = 
  ((str !! 0 >= 'A' && str !! 0 <= 'Z') || 
  (str !! 0 >= 'a' && str !! 0 <= 'z') || 
  isDigit (str !! 0))

var_check (x : xs) list check1 len1 =
    if (x !! 0 >= 'A' && x !! 0 <= 'Z')
      then var_check ((drop_val 1 x) : xs) (list ++ [x !! 0]) True len1
      else if check_var x
        then var_check ((drop_val 1 x) : xs) (list ++ [x !! 0]) check1 len1
        else if (check1 == False || length list <= 1)
          then "<error," ++ list ++ "> " ++ main_code (x : xs)
          else "<identifier," ++ list ++ "> " ++ main_code (x : xs)


check_variable (x : xs) =
  if (x !! 0 >= '0' && x !! 0 <= '9')
    then check_number (x : xs) [] False False
    else if x !! 0 == ' '
      then main_code (x : xs)
      else var_check (x : xs) [] False False
