module homeworksPart1
import StdEnv

/////////////////////////////////////////////////// Homework 1 ///////////////////////////////////////////////////

/**
  * Write a function that tests the Collatz conjecture.
  * Given a number 'n', if it is even, divide it by 2.
  * If 'n' is odd, multiply by 3 and add 1.
  * Repeat until the number reaches 1.
  * This function should return the total stopping time,
  * which is the number of steps it took to reach 1.
  *
  * Total: 30pts
  */
collatz :: Int -> Int
collatz x = f1 x 0

f1 :: Int Int -> Int
f1 a b
| a <= 1 = b
| isOdd a = f1 (a*3+1) (b+1)
=f1 (a/2) (b+1)
//Start = collatz 34 //13
//Start = collatz(collatz 10000001) //33
//Start = collatz 1 //0
//Start = collatz (~56) //0

/**
  * Write a function that takes an integer 'x'
  * and returns a boolean corresponding to whether or not x is prime.
  * A prime number is a number that has only itself and 1 as divisors.
  * 1 is not prime.
  *
  * Total: 30pts
  */
isPrime :: Int -> Bool
isPrime x 
| x<=1 =False
= prime x (x-1)

prime :: Int Int -> Bool
prime a b
| b == 1 = True
| (b rem a == 0) = False
=prime a (b-1)

//Start = isPrime 5 //True
//Start = isPrime (~3) //False (Negative numbers don't count, only Natural numbers)
//Start = isPrime 0 //False
//Start = isPrime 1 //False
//Start = isPrime 28736 //True

/**
  * Write a function that takes an integer 'x'
  * and checks if this number is a palindrome.
  * A palindrome is a number or word that is identical 
  * when written forward or backwards.
  *
  * e.g. 1234 is not a palindrome. 145626541 is a palindrome.
  * Total: 40pts
  */
isPalindrome :: Int -> Bool
isPalindrome x
| x<0 = False
| x<10 = True
| x == (palindrome x 0 ) = True
= False

palindrome :: Int Int -> Int
palindrome a b
| a<=0 = b
= palindrome (a/10) (b*10+(a rem 10)) 
/*
isPalindrome :: Int -> Bool
isPalindrome n
| n < 0 = False
= theList == reverse theList
  where
    theList = numToList n

numToList :: Int -> [Int]
numToList n
| n < 10 = [n]
= numToList (n/10) ++[n rem 10]
*/
//Start = isPalindrome 0 //True
//Start = isPalindrome 55 //True
//Start = isPalindrome 49594 //True
//Start = isPalindrome 1337 //False
//Start = isPalindrome (~57975) //False


//////////////////////////////////////////////////////// Homework 2 /////////////////////////////////////////////////////////

/**
  * Write a function that takes a number 'n'
  * and returns a list of the first n Fibonacci numbers.
  * A Fibonacci number is a sequence where F_0 = 0, F_1 = 1, and F_n = F_(n-2) + F_(n-1)
  * For example, FibList 8 = [1,1,2,3,5,8,13,21]
  *
  * Note: You must use recursion.
  *
  * Total: 20pts
  */
FibList :: Int -> [Int]
FibList n = map Fib [1..n]

Fib :: Int -> Int
Fib 0 = 1
Fib 1 = 1
Fib n = fibaux n 0 1

fibaux :: Int Int Int -> Int
fibaux 1 _ c = c
fibaux a b c = fibaux (a-1) c (b+c)

//Start = FibList 8 //[1,1,2,3,5,8,13,21]
//Start = FibList -3 //[]
//Start = FibList 0 //[]


/**
  * Write a function that takes a list of coefficients for a polynomial
  * and evaluates it at an integer given as the second parameter.
  * A list such as [1,6,9] would represent the the polynomial x^2+6x+9.
  * Note: Exponentiation via (^) or a custom exponential function
  * is NOT allowed.
  * 
  * For example: Evaluate [1,6,9] 2 = 25
  * Hint: Use Horner's Method
  * e.g. 3x^2 + 2x -4 = -4 + 2x + 3x^2 = -4 + x(2 + x(3)))
  *
  * Total: 50pts
  */
Evaluate :: [Int] Int -> Int
Evaluate [] _ = 0
Evaluate x y = (last x) + y*(Evaluate (init x) y)
//Start = Evaluate [1,6,9] 2 //25
//Start = Evaluate [1337] 12345 //1337
//Start = Evaluate [] 9001 //0
//Start = Evaluate [243,810,1080,720,240,32] (~2) //-1024

/**
  * Write a function that takes two lists of Strings,
  * one containing First Names and the other containing
  * Family Names, and creates a list where each sublist
  * will contain the First Names matched with the 
  * Family Names.
  * In the case that the list of Family Names has only
  * one Family name, assign it to every Frist Name.
  */
MakeFamily :: [String] [String] -> [[String]]
MakeFamily x y
| x == [] || y == [] = []
| length y == 1 = [[a]++[b]\\a<-x , b<-y] 
=[[a]++[b]\\a<-x & b<-y] 
//Start = MakeFamily ["Hossameldin", "Zuka", "Nicola", "Tringa", "Ying", "Nghia", "Pedro", "Evan"] ["Abdin", "Tsinadze", "Cenic", "Sylaj", "Xue", "Le", "Figueiredo", "Sitt"] //[["Hossameldin","Abdin"],["Zuka","Tsinadze"],["Nicola","Cenic"],["Tringa","Sylaj"],["Ying","Xue"],["Nghia","Le"],["Pedro","Figueiredo"],["Evan","Sitt"]]
//Start = MakeFamily ["Hossameldin", "Zuka", "Nicola", "Tringa", "Ying", "Nghia", "Pedro", "Evan"] ["Zsok"] //[["Hossameldin","Zsok"],["Zuka","Zsok"],["Nicola","Zsok"],["Tringa","Zsok"],["Ying","Zsok"],["Nghia","Zsok"],["Pedro","Zsok"],["Evan","Zsok"]]
//Start = MakeFamily [] ["Abdin", "Tsinadze", "Cenic", "Sylaj", "Xue", "Le Minh", "Figueiredo", "Sitt"] //[]
//Start = MakeFamily ["Hossameldin", "Zuka", "Nicola", "Tringa", "Ying", "Nghia", "Pedro", "Evan"] [] //[]



///////////////////////////////////////////////////// Homework 3 //////////////////////////////////////////////////

/*
Write a function that takes a list of numbers and
breaks it into two lists with alternating members from
the original list.
For example: [3,5,6,8,7,9] -> [ [3,6,7], [5,8,9] ]
*/
splitList :: [Int] -> [[Int]]
splitList x = [[a\\a<-x & b<-[1..] | isOdd b],[a\\a<-x & b<-[1..] | isEven b]]
//Start = splitList [56,3,87,5,234,5,0,-4] //[[56,87,234,0],[3,5,5,-4]]
//Start = splitList [1,4..50] //[[1,7,13,19,25,31,37,43,49],[4,10,16,22,28,34,40,46]]
//Start = splitList [420] //[[420],[]]
//Start = splitList []//[[],[]]

/*
Write a function that takes a list of numbers and
adds the first element, subtracts the second element,
adds the third element, subtracts the fourth element,
in this alternating repetition.
For example: [2,3,4,5,6,7] -> 2-3+4-5+6-7 = -3
*/
alternatingSum :: [Int] -> Int
alternatingSum a
| a == [] = 0
| length a == 1 = hd a
= ((hd a) - (hd (tl a))) + (alternatingSum (tl (tl a)))
//Start = alternatingSum [2..7] //-3
//Start = alternatingSum [45,-5,63,46,-345,4321] //-4599
//Start = alternatingSum [] //0

/*
Write a function that converts binary numbers to decimal numbers.
For example: 10010 = 2^4 + 2^1 = 18
*/
binaryToDecimal :: Int -> Int
binaryToDecimal x = bin x 0

bin :: Int Int -> Int
bin a b
| a<=0 = 0
= (a rem 10)*2^b + bin (a/10) (b+1)
//Start = binaryToDecimal 10010 //18
//Start = binaryToDecimal 1010101010101 //5461


////////////////////////////////////////////////// Homework 4 /////////////////////////////////////////

/**
  * 30 pts
  * Write a function that takes a list of integers
  * and returns the variance of the list.
  * That is, the sum of the square differences from the mean divided by
  * number of elements - 1.
  * For example, variance of [1,2,3,4,5] is calculated by:
  * Mean = (1+2+3+4+5)/5 = 3
  * Sum of Square Differences = (1-3)^2 + (2-3)^2 + (3-3)^2 + (4-3)^2 + (5-3)^2 = 10
  * Variance = 10/(5-1) = 10/4 = 2.5
  *
  * Note: Your solution must use 'map' or a list comprehension.
  */
Variance :: [Int] -> Real
Variance a
| a == [] = 0.0
= (Square a)/ toReal ((length a)-1)
Mean a = toReal (sum a) / toReal (length a)
Square a = sum (map (\x=(toReal x- (Mean a))^2.0)a)
//Start = Variance [1..5] //2.5
//Start = Variance [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1] //0
//Start = Variance [-4,1,6,0,-2,6] //16.96666666666
//Start = Variance [] //0

/**
  * 30 pts
  * Write a function that takes a list of integers and reverses their digits and order.
  * 
  * For example: ReverseDig [123,456,789] = [987,654,321]
  */
ReverseDig :: [Int] -> [Int]
ReverseDig x = reverse[rev a 0\\a<-x]

rev :: Int Int -> Int
rev x y
| x<= 0 = y
= rev (x/10) (y*10+(x rem 10)) 
//Start = ReverseDig [123,456,789] //[987,654,321]
//Start = ReverseDig [] //[]
//Start = ReverseDig [1,23,456,7891,23456] //[65432,1987,654,32,1]

/**
  * 40 pts
  * Write a function that takes a predicate (a -> Bool function) and
  * a list of sublists of integers and returns the sum of all elements that
  * return True on both  or one of the two predicates depending on
  * the given parameter "or"/"and".
  */
FilterSumAux :: (Int -> Bool) ([Bool]->Bool) (Int -> Bool) [[Int]] -> Int
FilterSumAux predA op predB list =sum [ x\\x<-(flatten list)| op [predA x, predB x]]
//Start = FilterSum isEven "or" ((<) 3) [[1..5],[-2..10]] //60
//Start = FilterSum ((<)10) "and" (\x=isEmpty[div\\div<-[2..(x-1)]|x rem  div == 0]) [[1..20],[90..100],[1..10]] //157
//Start = FilterSum isOdd "or" isEven []//0
