 ---- Part 1a ----
 {- 
 a) Explain in your own words what a (mathematical) function is and discuss to what extent Haskell functions resemble mathematical functions (Use examples to support your answer);


 A mathematical function is a type of relation constructed from two domains, namely the domain - which colloquially are known as the inputs - and the co-domain which is also colloquially referred to as the range (of results or 
 outputs)
 More specifically a given function named 'j' in broader mathematics will always map exactly one element from the domain 'k' to another in its co-domain, for example, 'l' uniquely. 
 As for the link to Haskell or even computing for that matter, a function can interpreted as a 'special type of binary relation'. 
 Binary relationships are utilised routinely in computer science, primarily seeing application in function use, more so in functional programming.
 A mathematical function is very alike to that of the pure functions that may compose programs written in Haskell and other functional programming languages. 
 With any given function, the user will input data (which will act as the domain) and the function will uniquely (if pure, as this assumes no side-effects) associate an output with a given value contained in the co-domain
 as would happen with a mathematical function.

 An example of a mathematical function that would be seen as a pure function in both disciplines would be the mapping of y as a function of x, in other words y=x.
 In this function every single value of x has exactly one y value associated with it infinitely.
 Same goes for any function of x with any co-efficient.


 ---- Part 1b ----

 b) Explain map function and filter function in Haskell in your own words with at least two examples for each. Do not use the examples from lecture slides. 

 Map Function:

 Mapping in Haskell is the process of applying the map higher order function to a list/array in order to transform the contents of the list/array.

 For example, if somebody wished to print the negative reciprocal of a list of numbers, they could do this by inputting the map function followed by the reciprocal and negative keywords followed by the list they wish
 to perform the action on, as can be seen if the following line is input in WinHugs (or some other interpreter):

 map (recip . negate) [4, 7, 0.9, -3.4]

 which would return:

 [-0.25,-0.142857142857143,-1.11111111111111,0.294117647058824]
 
 as the map function applies a transformation to the list of numbers, in this case the negative reciprocal, and makes a new list to represent these transformations.

 Another example for the operation of the map function can be to add punctuation to some words in a list by inputting the ++ operation followed by the punction desired encapsulated by speech marks, as seen in the following line:

 map (++ "?") ["How", "When", "What", "Why", "Where"]

 which would return:

 ["How?","When?","What?","Why?","Where?"]
 
 as we are applying a transformation that adds a question mark to these question starters, the map function creates a new list showing these transformations.


 Filter Function:

 Akin to the map function, the filter operation operates on a list/array of values evaluating each value based upon a given criterion in order to omit a value or return the value as part of the final list/array
 Unlike the map function, however, a new list/array is not created when the data is returned as can be seen in the type signatures defining these two functions:

 map :: (a -> b) -> [a] -> [b]
 filter :: (a -> Bool) -> [a] -> [a]

 This means that the data set originally provided is overwritten when the result is printed and thus data can be lost in this function
 An example of its use can be seen in the following line:

 filter (\greeting -> length greeting > 4) ["Hi","Hi, how are you doing today?","Hey"]

 which would return:

 ["Hi, how are you doing today?"]
 
 as we are filtering out all strings of this list that are less than 4 characters in length replacing the original list with the new list in the process.

 Another example could be to find all even numbers within a certain limit longer than 4 digits (i.e. up to2000), which would be defined by:

 filter (\x -> even x && x > 999) [1..2000]

 which would return:

 [1000, 1002, 1004, 1006, ... , 1998, 2000]
 
 as the numbers that are odd are filtered out, then all numbers less than 1000 are then filtered out the filter function then replaces the original list with this new list.



 -}

 ---- Part 2a ---- 
 {- 
 (a) Define a function steps that takes a positive Int value n and returns a String value that can be displayed as n steps, of width three stars and height 2 stars, upside-down, and repeats this pattern in opposite way
 
 For this part I am to create a function that takes a single input that produces a shape akin to an inverted stepped pyramid where the input is the number of steps in the shape (constructed from asterisks).
 Thus the type signature for the main function would ideally look something like this:
	
	steps :: Int -> String

 -- each step is 3 stars thinner than the last
 -- maximum height = 3 * input
 -- 2 stars across for each steps i.e. 2 rows of stars
 -}

steps :: Int -> String
steps h
	| h <= h = blockCreate(h)
    | otherwise = blockCreate(h-1)
	
	
blockCreate :: Int -> [Char]
blockCreate current_line = unlines (replicate 2 (replicate (w) '*'))

	where w = (current_line)*3






 ---- Part 2b ---- 
 {-
 (b) Define a function flagpattern that takes a positive Int value greater than or equal to five and returns a String that can be displayed as the following `flag' pattern of dimension n
 
 For this part I am to create a function that takes a single input variable and outputs a dimensionally square flag patter constructed from asterisks (length and width are the same number of asterisks).
 
 Thus the type signature for the main function would ideally look something like this:
	
	flagpattern ::  Int -> String
  
 The star pattern is created at the top and the bottom as these are always full lines of '*' symbol equal to the variable inputted.
 We use the built in function 'unlines' to print multiple lines (with newlines) followed by a user defined function reflect (which constructs the reverse for the latter half of the flag pattern for the sake of simplicity)
 the function will print asterisks as the first line (before the colon) and move on to the rest of the function as it is called after the colon.
 The line variable is defined as such making use of current_line in order to determine where whitespaces are supposed to be to establish the cross pattern in the flag.
 The selection statement is to ensure the centre of the flag consists of one star - if odd and four stars if even; to keep the flag design consistent
 
 -}
 
flagpattern :: Int -> String
flagpattern size = unlines(reflect(replicate size '*' : map row[0..current_line]))
  where current_line = div (size-3) 2
        row x   = reflect(concat["*", replicate x ' ', "*", replicate (current_line-x) ' '])
        reflect flagpat = 
	
				   if odd size
				   
                   then flagpat ++ tail (reverse flagpat)
				   
                   else flagpat ++ reverse flagpat

 ---- Part 3 ---- 
 {- 
 Define a function swapwords that takes three String values w1 w2 s, and returns s with all occurrences of the String w1, in the String s, replaced by the String w2
 
 For this part I am to define a function that takes three inputs, w1 - the word being replaced, w2 - the word being swapped in, and s - the string which at least contains w1
 Thus the type signature for the main function would ideally look something like this:
	
	swapwords ::  String -> String -> String -> String
	
 The type signature says we are taking 3 strings to output 1 string, which would be the output with the swapped words w1 and w2
 We set out the variable names listed in the task w1 w2 and s, making s a list of strings so that we can separate each word, detect matching words and replace them accordingly
 We use these parameters to compare w1 with the words in the string list, then adding them to the list and recalling the function to check for any more matches so long as the otherwise is being reached.
	
 -}
 
swapwords :: String -> String -> String -> String 
swapwords word1 word2 [s] = [] 
swapwords word1 word2 (x : xs)
  | length (x:xs) < comp = x : xs  
  | otherwise =
  
      if word1 == take comp (x:xs)
	  
      then word2 ++ swapwords word1 word2 (drop comp (x:xs)) 
	  
      else x : swapwords word1 word2 xs 
	  
  where 
    comp = length word1 






 ---- Part 4 ---- 
 {- 
 Explain merge sort in your own words and define a recursive function msort that implements merge sort
 
 A merge sort is one of various sorting variations where the original list is split up potentially several times and each new list is compared separately so that it is in order after which, lists are combined one by one
 to create more ordered but larger 'chunks' of the original list, until you arrive at the full ordered list. 

 For this part I was to create a function that makes use of this sorting method to work on any given list of numbers
	
 Thus the type signature for the main function would ideally look something like this:
	
	msort ::  [Int] -> [Int]
	
 To do this we create two functions 'merge' and the function that is called 'msort'
 No sorting occurs in msort it is all contained in merge (given the recursion seen in the function) which is called in the msort function.
 The variables of the halves to be sorted are defined by the number of elements of the list over 2 which is then used with the drop and take functions to split the lists equally, the msort function calls merge on both variables.
 In the merge function the terms of each half term another term resulting in the shifting around of terms and the recalling of the merge function until an ordered list is eventually achieved
 and the list is reformed and then output.
	

 -}				
  
merge ::[Int] -> [Int] -> [Int]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) 
 | x < y = x : merge xs (y:ys)
 | otherwise =  y: merge (x:xs) ys 


msort ::[Int] -> [Int]
msort [] = []
msort [a] = [a]
msort xs = merge (msort firstHalf) (msort secondHalf)

 where
 n = (length xs)`div` 2
 firstHalf = take n xs
 secondHalf = drop n xs


 ---- Part 5 ----
 {-
 Define a polymorphic function split that is applied to two arguments of types [a] and a, where a is a type on which == is defined, and returns a list of lists that partitions the original list at
 occurrences of the second argument

 In this function I am to define a function which takes two inputs, a string (a) and a char ([a]) of which the char is used as a delimiter term used to split the string into lists containing strings of
 data which preceeded the occurance of the delimiter term and anything after it if there is anything in either of these locations.
	
 Thus the type signature for the main function would ideally look something like this:
	
	split ::  String -> Char -> [String]
	
 -}
 



