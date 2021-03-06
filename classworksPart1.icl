module classworksPart1
import StdEnv
////////////////////////////////////////////// Classwork 1 group 4 /////////////////////////
// Determine the greatest common divisor of two
// positive integer numbers.
f1 :: Int Int -> Int
f1 x y
| x > y = f1aux x y y
=f1 y x

f1aux :: Int Int Int -> Int
f1aux x y z
| (y rem z == 0) && (x rem z == 0) = z
= f1aux x y (z-1) 

f11 :: Int Int -> Int
f11 x y = gcd x y

//Start = f1 30 15 // 5
//Start = f1 6 11 // 1
//Start = f1 6 15 // 3

 
// You are climbing a stair case. It takes n steps to reach to the top.
// Each time you can either climb 1 or 2 steps.
// In how many distinct ways can you climb to the top?

fac :: Int -> Int
fac x
| x == 0 = 1
| x == 1 = 1
= x * fac (x-1)

f2 :: Int -> Int
f2 y = (fac y) / (2 * fac (y-2))


//Start = f2 2 // 2 (1+1,2)
//Start = f2 3 // 3 (1+1+1,1+2,2+1)

// Given an integer, write a function that returns
// the largest digit in the integer.

f3 :: Int -> Int
f3 x
| x < 0 = f3 (abs x)
| x < 10 = x
= f3aux (x/10) (x rem 10)

f3aux :: Int Int -> Int
f3aux a maxdigit
| a <= 0 = maxdigit
| (a rem 10) > maxdigit = f3aux (a/10) (a rem 10)
| (a rem 10) < maxdigit = f3aux (a/10) maxdigit

//Start = f3 564 //6
//Start = f3 5 //5
//Start = f3 6793 //9



//////////////////////////////////////////////////////Classwork 1 group 2 ///////////////////////////////////////////////
// Given an integer, write a function
// that returns the sum of all
// odd numbers less than it.
f1 :: Int -> Int
f1 x
| x<=0 = 0
= f1aux (x-1) 0

f1aux :: Int Int -> Int
f1aux a sum
| a == 0 = sum
| (a rem 2) == 0 = f1aux (a-1) sum
| (a rem 2) == 1 = f1aux (a-1) (sum+a)
//Start = f1 5 //4
//Start = f1 12 //36
//Start = f1 -6 //0

// How many different ways are there
// to distribute 'n' homework problems
// amongst 'm' number of friends?

fac :: Int -> Int
fac x
| x == 0 =1
| x == 1 =1
= x * fac (x-1)

f2 :: Int Int -> Int
f2 a b 
| a<=0 || b<=0 = 0
= (fac a) / ((fac b) * (fac (a-b)))
//Start = f2 200 4 //64684950
//Start = f2 4 2 //6
//Start = f2 -4 4 //0
//Start = f2 9999999 0 //0

// Given an integer, write a function
// that will sum up its digits.
f3 :: Int -> Int
f3 x
| x == 0 = 0
| x < 0 = f3 (abs x)
= f3aux x 0

f3aux :: Int Int -> Int
f3aux x sum
| x <= 0 = sum
=f3aux (x/10) (sum+(x rem 10))
//Start = f3 1234 //10
//Start = f3 506 //11
//Start = f3 6 //6
//Start = f3 -91 //10


///////////////////////////////////////////////////////// Classwork 1 group 1 ///////////////////////////////////////////////////

// Given two integers, write a function
// that will give us their least common multiple.
f1 :: Int Int -> Int
f1 x y
| x == 0 || y == 0 = 0
| y > x = f1 y x
= f1aux x y x

f1aux :: Int Int Int -> Int
f1aux a b c
| (c rem a == 0) && (c rem b == 0) = c
= f1aux a b (c+1)
//Start = f1 3 4 //12
//Start = f1 0 5 //0
//Start = f1 -7 4 //28
//Start = f1 12 10 //60

// Given 'n' number of friends
// and 'm' pieces of cake, 
// how many different ways are there to
// distribute these pieces of cake?
fac :: Int -> Int
fac x
| x == 0 = 1
| x == 1 = 1
= x * fac(x-1)

f2 :: Int Int -> Int
f2 x y
| x <= 0 || y <= 0 = 0
| x > y = (fac x) / ((fac y) * (fac (x-y)))
=f2 y x
//Start = f2 5 2 //10
//Start = f2 5 8 //56
//Start = f2 5 13 //0
//Start = f2 -4 9999 //0

// Given an integer, write a function
// that will check if each digit is even.
f3 :: Int -> Bool
f3 x = f3aux x 0

f3aux :: Int Int -> Bool
f3aux a b
| (a<=0) && (b==1) = False
| (a<=0) && (b==0) = True
| isOdd (a rem 10) = f3aux (a/10) 1
= f3aux (a/10) 0

//Start = f3 1234 //False
//Start = f3 506 //True
//Start = f3 -846 //True


/////////////////////////////////////////////// Classwork 2 group 1 ///////////////////////////////////////////////////

// 1. Compute the sum of the list excluding the largest and the smallest number
f1 :: [Int] -> Int
f1 x = sum(init (tl (sort x)))
//Start = f1 [1..5] //  9
//Start = f1 [4,6,2,7,1,9] //  19
//Start = f1 [-4,6,2,7,1,9] // 16

// 2. Replace every second number from the list by 1,2,3...
f2 :: [Int] -> [Int]
f2 x
| x == [] = []
= f2aux x 1

f2aux :: [Int] Int -> [Int]
f2aux a b
| length a == 1 = [hd a]
| a == [] = []
= [hd a] ++ [b] ++ f2aux (drop 2 a) (b+1)

//Start = f2 [] //[]
//Start = f2 [4] //[4]
//Start = f2 [6,4] //[6,1]
//Start = f2 [6,6,6,6] //[6,1,6,2]
//Start =  f2 [1,9,0,2,4,7,0,5,1,8,3,0,1,2] // [1,1,0,2,4,3,0,4,1,5,3,6,1,7]

// 3. Divide a list of integer into two lists
// One containing even numbers and one containing odd numbers
//[1,2,3,4,5,6] -> [[2,4,6],[1,3,5]]
f3::[Int] -> [[Int]]
f3 x = [filter isEven x, filter isOdd x]
//Start = f3 [1..6] //[[2,4,6],[1,3,5]]
//Start = f3 [34..43]  //[[34,36,38,40,42],[35,37,39,41,43]]

/////////////////////////////////////////////// Classwork 2 group 2 ///////////////////////////////////////////////////

// 1. Compute the product of odd numbers from 1 to n using recursion
prodOdd :: Int -> Int
prodOdd x
| x<=0 =0
= f1aux x 1

f1aux :: Int Int -> Int
f1aux x y
| y>x = 1
= y * f1aux x (y+2)

//Start = prodOdd 7 // 105
//Start = prodOdd 6 // 15
//Start = prodOdd 1 // 1
//Start = prodOdd (~54) //0

// 2. Remove all '0' from the list
remZero :: [Int] -> [Int]
remZero x = [a\\a<-x | a<>0]
//Start = remZero [1,9,0,2,4,7,0,5,1,8,3,0,1,2] // [1,9,2,4,7,5,1,8,3,1,2]
//Start = remZero [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0] // []
//Start = remZero [1,0,(~1),0,0,2,0,0,0,(~2),0,0,0,0,3] // [1,-1,2,-2,3]

// 3. Compute the difference between the first and the last element of each sublist.
sums :: [[Int]] -> [Int]
sums x 
| x == [] =[]
| length (hd x) == 1 = [0] ++ sums (tl x)
| length (hd x) == 0 = [0] ++ sums (tl x)
= [last(hd x)- hd (hd x)] ++ sums (tl x)
//Start = sums [[1,2], [3,4,5], [6,5,9,7], [], [8]] // [1,2,1,0,0]

/////////////////////////////////////////////// Classwork 2 group 4 ///////////////////////////////////////////////////

// Reverse the list recurievely
f1 :: [Int] -> [Int]
f1 x = f1aux x []

f1aux :: [Int] [Int] -> [Int]
f1aux a b
| a==[] = b
= f1aux (init a) (b ++[last a])
//Start = f1 [] // []
//Start = f1 [1,2,3,4,5] // [5,4,3,2,1]

// Generate every odd number from 1 to n (n is function parameter)
f2 :: Int -> [Int]
f2 x = f2aux x 1

f2aux :: Int Int -> [Int]
f2aux a b
| a < b = []
= [b] ++ f2aux a (b+2)
//Start = f2 1  // [1]
//Start = f2 10 // [1,3,5,7,9]
//Start = f2 (~4) // []

// Replicate the elements of a list a given number of times. 
// If list is [1,2,3] and given number 2 -> [1,1,2,2,3,3]
f3 :: [Int] Int -> [Int]
f3 x y
| x == [] = []
| y <=0 = []
= f3aux x y 

f3aux :: [Int] Int -> [Int]
f3aux x y
| x == [] = []
= (repeatn y (hd x)) ++ (f3aux (tl x) y)
//Start = f3 [1,1,3,4] 3 // [1,1,1,1,1,1,3,3,3,4,4,4]
//Start = f3 [1,2,3] 2 // [1,1,2,2,3,3]
//Start = f3 [] 100 // []
//Start = f3 [1..] (~45) // []

///////////////////////////////////////////////////////// Classwork 3 group 1 /////////////////////////////////////////


/* 1. Eliminate consecutive duplicates of list elements.
 If a list contains repeated elements they should be 
 replaced with a single copy of the element
 AND a number given by parameter. 
 The order of the elements should not be changed.
 */
f1 :: [Int] Int-> [Int]
f1 [] n = []
f1 [a] n = [a]
f1 [a,x:xs] n
| a == x = [a,n : f1 (dropWhile ((==)x) xs) n]
= [a] ++ f1 [x:xs] n 
//Start = f1 [1] 8 // [1]
//Start = f1 [] 1// []
//Start = f1 [1,2,2,3,3,5] 0// [1,2,0,3,0,5]
//Start = f1 [1,1,1,4,4,5,6,7,7,7,7] 9// [1,9,4,9,5,6,7,9]
//Start = f1 [1,2,3,4,5,6] 0// [1,2,3,4,5,6]
//Start = f1 [1,1,1,1,1,1] 5// [1,5]
//Start = f1 [2,2,2,2,2,2,2] 666// [2,666]

/* 2. Given a list of sublists of Int, 
keep only the lists where all numbers
are prime numbers.
*/
f2::[[Int]]->[[Int]]
f2 x = [a\\a<-x | and[isPrime q \\q<-a]]

isPrime :: Int -> Bool
isPrime x 
| x <= 1 = False
= isEmpty[x\\ n<-[2..x-1] | x rem n == 0]

//Start = f2 [] //[]
//Start = f2 [[],[4,5,6],[7,11],[7..11]] //[[],[7,11]]
//Start = f2 [[1..10],[2,3,7,5],[1,3,5,7],[21]] //[[2,3,7,5]]


// 3.  Delete the n-th element of each sublist in the list.
f3 ::[[Int]] Int -> [[Int]]
f3 x y = [[q\\q<-a & n<-[1..] | n <> y]\\a<-x]
//Start = f3 [1,2,3,4,5] 1
//Start = f3 [[1,2,3],[3],[4,5,6,7],[],[0,1,6,3,5]] 3  //[[1,2],[3],[4,5,7],[],[0,1,3,5]]
//Start = f3 [[1,2,6,8,3],[9,3],[0,5,0,6,7],[],[0,1,6,3,5,8]] 3  //[[1,2,8,3],[9,3],[0,5,6,7],[],[0,1,3,5,8]]
//Start = f3 [[0],[3],[4,5,6],[],[0,1,6,9,7,3,5]] 3  //[[0],[3],[4,5],[],[0,1,9,7,3,5]]


///////////////////////////////////////////////////////// Classwork 3 group 2 /////////////////////////////////////////

///////////////////////////////////////////////////////// Classwork 3 group 4 /////////////////////////////////////////


// Eliminate consecutive duplicates of list elements.
// If a list contains repeated elements they should be 
// replaced with a single copy of the element. 
// The order of the elements should not be changed.
f1 :: [Int] -> [Int]
f1 [] = []
f1 [a] = [a]
f1 [a,x : xs]
| a == x = [a : f1 xs]
=[a : f1 [x:xs] ]
//Start = f1 [1] // [1]
//Start = f1 [] // []
//Start = f1 [1,2,2,3,3,5] // [1,2,3,5]
//Start = f1 [1,1,1,4,4,5,6,7,7,7,7] // [1,1,4,5,6,7,7]
//Start = f1 [1,2,3,4,5,6] // [1,2,3,4,5,6]
//Start = f1 [1,1,1,1,1,1] // [1,1,1]
//Start = f1 [2,2,2,2,2,2,2] // [2,2,2,2]


// Determine the prime factors of a given positive integer.
// Construct a flat list containing the prime factors in ascending order. 
primeFactors :: Int -> [Int]
primeFactors y = [a\\a<-[2..y] | isPrime a && (y rem a == 0)]

isPrime :: Int -> Bool
isPrime x 
| x <= 1 = False
= isEmpty[a\\a<-[2..x-1] | x rem a == 0]

//Start = primeFactors 0 // []
//Start = primeFactors -5 // []
//Start = primeFactors 1 // []
//Start = primeFactors 17 // [17]
//Start = primeFactors 614889782588491410 // [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47]


// Rotate a list N places to the left. 

f3 :: [Int] Int -> [Int]
f3 x y = (drop y x) ++ (take y x)

//Start = f3 [1,2,3] 2   // [3,1,2]
//Start = f3 [] 3 // []
//Start = f3 [6] 5 // [6]
//Start = f3 [1,2,3,4,5,6,7,8] 3 // [4,5,6,7,8,1,2,3]


////////////////////////////////////////////////////////// Classwork 4 group 4 /////////////////////////////////////////////

// Generate list of Fibonacci numbers which are less than given n and are even.
f1 :: Int -> [Int]
f1 n = [(fib 1 1 x) \\ x<-[1..n] | isEven (fib 1 1 x) && (fib 1 1 x)<n && (fib 1 1 x) >0] 

fib :: Int Int Int -> Int
fib a b c
| c == 0 = b
= fib b (a+b) (c-1)

//Start = f1 10 // [2,8]
//Start = f1 1000 // [2,8,34,144,610]
//Start = f1 100000 // [2,8,34,144,610,2584,10946,46368]
//Start = f1 1000000000 // [2,8,34,144,610,2584,10946,46368,196418,832040,3524578,14930352,63245986,267914296]
//Start = f1 10000000000 // [2,8,34,144,610,2584,10946,46368,196418,832040,3524578,14930352,63245986,267914296,1134903170,4807526976]

// Define function myLength, which returns length of a list
// You must use foldr
myLength :: [Int] -> Int
myLength q = foldr (+) 0 (map (\x=1) q)
//Start = myLength [] // 0
//Start = myLength [1,2,3] // 3
//Start = myLength (take 100 [1..]) // 100
//Start = myLength [1..] // Heap full

// Define function "reverse" using foldr
myReverse :: [Int] -> [Int]
myReverse x = foldr (\a b = b ++ [a]) [] x
//Start = myReverse [1,2,3,4,5,6,7,8] // [8,7,6,5,4,3,2,1]
//Start = myReverse [] // []
//Start = myReverse [1] // [1]

//////////////////////////////////////////////////// Classwork 4 group 1 //////////////////////////////////////////////

// 1. For every sublist, eliminates its elements
// Until the current element is a prime number
// Requirement: 
//  - Use list comrehension to determin the prime number!
//  - Use map instead of recursion
//  - Use dropWhile

isPrime :: Int -> Bool
isPrime x 
| x <= 1 = True
= not (isEmpty[a\\a<-[2..x-1] | x rem a ==0])

f1 :: [[Int]] -> [[Int]]
f1 x = map (dropWhile (isPrime)) x
//Start = f1 [[1, 2, 3, 4], [9, 7, 6, 5, 4, 3, 0], [3, 5, 7, 9], [], [128, 64, 32]] // [[2,3,4],[7,6,5,4,3,0],[3,5,7,9],[],[]]
//Start = f1 [[1], [4], [2]] // [[],[],[2]]
//Start = f1 [[5..10], map (\x = x + 5) [1..4], [], [4,12,8,5, 4]] // [[5,6,7,8,9,10],[7,8,9],[],[5,4]]

//2. A positive number in the form like: 10, 200, 8, 1000, 40, 1, 9, 7000, 30000000
// (which has only one non-zero digit at first place) is called a "clean number"(0 is not included)
// find all clean numbers in the list of lists and write to a list

f2::[[Int]]->[Int]
f2 x = flatten(map(filter fun)x)

fun::Int->Bool
fun z
|z>0&&z<=10= True
|z>10 && z rem 10==0=fun (z/10)
=False

//Start = f2 [[1,2,7,10,50,102,33],[],[0,9,90],[11,980,20]] //[1,2,7,10,50,9,90,20]
//Start = f2 [[1..20],[10,20..60],[30,20.. -10]]//[1,2,3,4,5,6,7,8,9,10,20,10,20,30,40,50,60,30,20,10]

//3. find the middle element of each sublists of list.(hint:use !!)
// list of even length like [0,1,2,3] choose 2
// add them together using foldr
// suggest using only one function

f3 :: [[Int]] -> Int
f3 x = foldr (+) 0 (map (\some = some!!((length some)/2)) x)
//Start =f3 [[1],[1,2],[1,2,3]] //5
//Start =f3 [[1],[1,2],[1,2,3],[3,3,0,8,9]] //5
//Start = f3 [[10,20,30],[1,3]] //23

///////////////////////////////////////////////////////////////Classwork 5 group 4 ///////////////////////////////////////


// Calculate Euler's totient function phi(m).
// Euler's so-called totient function phi(m) is defined as 
// the number of positive integers r (1 <= r < m) that are coprime to m.

// Use list compherension
euler :: Int -> Int
euler x = length [a\\a<-[1..x-1] | gcd a x == 1]

//Start = euler 10 // 4
//Start = euler 100 // 40
//Start = euler 2500 // 1000
//Start = euler 1181 // 1180
//Start = euler 1021904 // 443904

// Generate the list of all possible (Day, Month) tuples in a given year.
// Make sure to take care of different number of days in different months.
    // January - 31 days
    // February - 28 days in a common year and 29 days in leap years
    // March - 31 days
    // April - 30 days
    // May - 31 days
    // June - 30 days
    // July - 31 days
    // August - 31 days
    // September - 30 days
    // October - 31 days
    // November - 30 days
    // December - 31 days
// Make sure to take care of leap years. 
// leap year: 
// if (year is not divisible by 4) then (it is a common year)
// else if (year is not divisible by 100) then (it is a leap year)
// else if (year is not divisible by 400) then (it is a common year)
// else (it is a leap year) 

dayMonth daymonth = flatten [ [( a , x ) \\ x <- [1..b] ] \\ ( a , b ) <- leap daymonth]
leap year
|(year rem 4 <> 0) || ((year rem 100 == 0)  && (year rem 400 <> 0)) = [(1, 31),(2, 28),(3, 31),(4, 30),(5, 31),(6, 30),(7,31),(8,31),(9,30),(10,31),(11,30),(12, 31)]
=[(1, 31),(2, 29),(3, 31),(4, 30),(5, 31),(6, 30),(7,31),(8,31),(9,30),(10,31),(11,30),(12, 31)]

// Start = dayMonth 2016


// You are given record representing set Q (rational numbers)
// Write function simplifyRational that takes rational number and brings it to normal form. 
// So 15/20 should be 3/4, 2/4 should be 1/2, ...

:: Q = { num :: Int, denom :: Int }
simplifyRational :: Q -> Q
simplifyRational {num = a, denom = b}
| b == 0 = abort "denominator is 0"
| a > 0 && b > 0  = {num = a / (gcd a b), denom = b/(gcd a b)}
| a < 0 || b < 0 = {num = ~a / (gcd a b), denom = ~b/(gcd a b)}
//Start = simplifyRational { num = 15, denom = 20 } // (Q 3 4)
//Start = simplifyRational { num = 2, denom = 4 } // (Q 1 2)
//Start = simplifyRational { num = 1, denom = 3 } // (Q 1 3)
//Start = simplifyRational { num = 5, denom = 1 } // (Q 5 1)
//Start = simplifyRational { num = 15, denom = -20} // (Q -3 4)

//////////////////////////////////////////////////////////// Classwork 5 group 1 ////////////////////////////////////////////////

/* 1. Given a list of integers, decide if they are pairwisely relatively prime or not.
(Two integers are relatively prime (or coprime) 
if there is no integer greater than one that divides both of them)
*/
f1::[Int]->Bool
f1 x = isEmpty[a\\a<-x & q<-x | coprime a q]

coprime :: Int Int -> Bool
coprime a b = (gcd a b == 1)

//Start = f1 [] //True
//Start = f1 [1,3,7,9] //False (3,9) not
//Start = f1 [11,12,13] //True

// 2. Define a record type for rational numbers, and add two rational numbers.
:: Q = {nom :: Int , den :: Int}
f2 :: Q Q -> Q
f2 {nom=a , den =b} {nom = c, den = d} = simplifyRational {nom = a*d+b*c, den = b*d}
//Start=f2 {nom=2,den=4} {nom=2,den=3} //(Q 7 6) 
//Start=f2 {nom=2,den=4} {nom=2,den=4} //(Q 1 1)

simplifyRational :: Q -> Q
simplifyRational {nom = a, den = b}
| b == 0 = abort "denominator is 0"
| a > 0 && b > 0  = {nom = a / (gcd a b), den = b/(gcd a b)}
| a < 0 || b < 0 = {nom = ~a / (gcd a b), den = ~b/(gcd a b)}

/* 3.Given a integer, return a tuple containing
a list of its divisors as the first element of the tuple,
and the mean value of that list as the second element of the tuple.
*/
f3 :: Int -> ([Int],Real)
f3 x = ((divisors x),mean (divisors x))

divisors :: Int -> [Int]
divisors x = [a\\a<-[1..x] | x rem a == 0]

mean :: [Int] -> Real
mean x = (toReal (sum x)) / (toReal (length x))

//Start = f3 8 //([1,2,4,8],3.75)
//Start = f3 9 //([1,3,9],4.33333333333333)
//Start = f3 15 //([1,3,5,15],6)

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////