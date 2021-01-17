module classworksPart2
import StdEnv
----------------------------------------Classwork 5 group 1-----------------------------------------
/* 1. Given a list of integers, decide if they are pairwisely relatively prime or not.
(Two integers are relatively prime (or coprime) 
if there is no integer greater than one that divides both of them)
*/
f1::[Int]-> Bool
f1 x = and[(relPrim a b) \\a<-x , b <-x | a<>b]
relPrim :: Int Int -> Bool
relPrim x y
| (gcd x y == 1) && (lcm x y== x*y)= True
=False
//Start = f1 [] //True
//Start = f1 [1,3,7,9] //False (3,9) not
//Start = f1 [11,12,13] //True

// 2. Define a record type for rational numbers, and add two rational numbers.
::Q = {nom::Int , den :: Int}
f2 :: Q Q -> Q
f2 {nom=a,den=b} {nom=c,den=d} = simplify{nom=a*d+b*c , den=b*d}
simplify :: Q -> Q
simplify {nom=x,den=y}
| y == 0 = abort "AYE"
| x >  0 = {nom=x/(gcd x y) , den= y/(gcd x y)}
| x <  0 = {nom= ~x/(gcd x y) , den= ~y/(gcd x y)}
//Start=f2 {nom=2,den=4} {nom=2,den=3} //(Q 7 6) 
//Start=f2 {nom=2,den=4} {nom=2,den=4} //(Q 1 1)

/* 3.Given a integer, return a tuple containing
a list of its divisors as the first element of the tuple,
and the mean value of that list as the second element of the tuple.
*/
f3 :: Int -> ([Int],Real)
f3 num = (divisors num,(toReal (sum(divisors num)) /  toReal(length(divisors num))))
divisors :: Int -> [Int]
divisors x = [y\\y<-[1..x]|x rem y==0]
//Start=divisors 9
//Start = f3 8 //([1,2,4,8],3.75)
//Start = f3 9 //([1,3,9],4.33333333333333)
//Start = f3 15 //([1,3,5,15],6)
-----------------------------------------------classwork 5 group 4-----------------------------
//1.Calculate Euler's totient function phi(m).
// Euler's so-called totient function phi(m) is defined as 
// the number of positive integers r (1 <= r < m) that are coprime to m.
// Use list compherension
euler :: Int -> Int
euler x = length[y\\y<-[1..x]| (gcd y x) ==1]
//Start = euler 10 // 4
//Start = euler 100 // 40
//Start = euler 2500 // 1000
//Start = euler 1181 // 1180
//Start = euler 1021904 // 443904

//2. Generate the list of all possible (Day, Month) tuples in a given year.
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
//Start = dayMonth 2016

//3. You are given record representing set Q (rational numbers)
// Write function simplifyRational that takes rational number and brings it to normal form. 
// So 15/20 should be 3/4, 2/4 should be 1/2, ...
:: Q = { num :: Int, denom :: Int }
simplifyRational :: Q -> Q
simplifyRational {num=x,denom=y}
| y == 0 = abort "AYE"
| x >  0 = {num=x/(gcd x y) , denom= y/(gcd x y)}
| x <  0 = {num= ~x/(gcd x y) , denom= ~y/(gcd x y)}
//Start = simplifyRational { num = 15, denom = 20 } // (Q 3 4)
//Start = simplifyRational { num = 2, denom = 4 } // (Q 1 2)
//Start = simplifyRational { num = 1, denom = 3 } // (Q 1 3)
//Start = simplifyRational { num = 5, denom = 1 } // (Q 5 1)
//Start = simplifyRational { num = 15, denom = -20} // (Q -3 4)
------------------------------------------------------------------classwork 6 group 1 ------------------------------------------------------
//1.It is hypothesized that every even number greater than two can be expressed as the sum of two primes. 
//  For example, 4 = 2+2, 6 = 3+3, 8 = 3+5. 
//  Check if this is true for all even numbers in the range 4 to n? 
//  Hint/Requirement: Use list comprehension
check :: Int -> Bool
check q = not( isEmpty[(a,b)\\a<-(primeList q),b<-(primeList q)|a+b==q])
isPrime :: Int -> Bool
isPrime x
| x <= 1 = False
= isEmpty[n\\n<-[2..(x-1)]|x rem n == 0]
primeList :: Int ->[Int]
primeList num = [x\\x<-[1..num]|isPrime x]
//Start = check 1000 // True
//Start = check 100000 // True
//Start = check 500 // True
//Start = check 17 // True

//2.Given three Vectors in 2D, decide if their endpoint lie on a same line.
// Hint1: Points are on a same line, if area of triangle formed by these points is 0.
// Hint2: https://www.dummies.com/education/math/algebra/finding-the-area-of-a-triangle-using-its-coordinates/
:: Vector2 = {x :: Real, y :: Real}
collinear :: Vector2 Vector2 Vector2 -> Bool
collinear v1 v2 v3 = (((v1.x * v2.y + v2.x * v3.y + v3.x * v1.y - v1.x * v3.y - v2.x * v1.y - v3.x * v2.y)/2.0) == 0.0)
//Start = collinear {x = 0.0, y = 0.0} {x = 1.0, y = 0.0} {x = 3.0, y = 0.0} // True
//Start = collinear {x = 0.0, y = 0.0} {x = 1.0, y = 0.0} {x = 3.0, y = 1.0} // False
//Start = collinear {x = 0.0, y = -1.0} {x = 2.0, y = 0.0} {x = 3.0, y = 0.0} // False
----------------------------------------------------------classwork 6 group 4----------------------------------------------------------------
//1.A Pythagorean triad is a triple of integers (a,b,c) such that
// a^2 + b^2 == c^2
// Count how many triads are there with 1<=a<=b<=c<=n.
// n is given as parameter
// Hint/Requirement: Use list comprehension
// Hint: To avoid three loops, check if a*a + b*b is perfect square, if yes increase count. 
// Also, you have to check that sqrt(a * a + b * b) <= n. No need to check sqrt(a * a + b * b)>=a,b, it is automatic. Think why.
countAll :: Int -> Int
countAll number = length[(x,y,z)\\x<-[1..number] , y<-[1..number] , z<-[1..number] | checkValid (x,y,z) number && checkVali (x,y,z) number]
checkValid :: (Int,Int,Int) Int -> Bool
checkValid (x,y,z) number= (1<=x) && (x<=y) && (y<=z) && (z<=number) && (a^2 +b^2 == z^2) 
    where
    sorted = sort[x,y,z]
    a = sorted!!0
    b = sorted!!1
    c = sorted!!2
checkVali :: (Int,Int,Int) Int -> Bool
checkVali (x,y,z) number = (sqrt (toReal (a * a +b * b)) <= toReal number)
    where
    sorted = sort[x,y,z]
    a = sorted!!0
    b = sorted!!1
    c = sorted!!2
//Start = countAll 100 // 52
//Start = countAll 1442 // 1349
//Start = countAll 3134 // 3334

//2.You are given a point and triangle. Determine if point lies inside triangle.
// Hint: https://stackoverflow.com/questions/2049582/how-to-determine-if-a-point-is-in-a-2d-triangle
// You have to write records for Point and Triangle. Point should be represented as two Real coordinates x and y. 
// Triangle is represented as three Points a,b and c.
:: Point = { x :: Real , y :: Real } 
:: Triangle = { a :: Point , b :: Point , c :: Point }
isInside :: Point Triangle -> Bool
isInside p {a=p1 , b=p2 , c=p3}
| distance p p1 p2 < 0.0 || distance p p2 p3 < 0.0 || distance p p3 p1 < 0.0 = False
| distance p p1 p2 > 0.0 || distance p p2 p3 > 0.0 || distance p p3 p1 > 0.0 = False
=True
distance :: Point Point Point -> Real
distance a b c = (a.x-c.x) * (b.y - c.y) - (b.x-c.x)*(a.y-c.y)
//Start = isInside {x = 0.0, y = 0.0} {a = {x = 1.0, y = 1.0}, b = {x = -1.0, y = 1.0}, c = {x = 0.0, y = -1.0}} // True
//Start = isInside {x = 3.0, y = 4.0} {a = {x = 1.0, y = 1.0}, b = {x = -1.0, y = 1.0}, c = {x = 0.0, y = -1.0}} // False
//Start = isInside {x = 0.0, y = 0.0} {a = {x = 0.0, y = 0.0}, b = {x = 0.0, y = 0.0}, c = {x = 0.0, y = 0.0}} // True
//Start = isInside {x = 0.0, y = 1.0} {a = {x = -1.0, y = 1.0}, b = {x = 1.0, y = 1.0}, c = {x = 1.0, y = 1.0}} // True
-------------------------------------------------------------classwork 7 group 1 -----------------------------------------------------
// 1. Given three points, write a function which decides if a they form a
// Right Triangle (triangle which has three equal sides).
:: Point = {x :: Real , y :: Real }
f1::Point Point Point -> Bool
f1 a b c 
| distance a b + distance b c == distance a c = True
| distance a c + distance b c == distance a b = True
| distance a b + distance a c  == distance b c = True
=False 
//Start = f1 p1 p2 p3 
//Start = f1 {x=(-4.0) , y=(-2.0)} {x=(-3.0) , y=(7.0)} {x=(4.0) , y=(-2.0)} //False
//Start = f1 {x=(0.0) , y=(3.0)} {x=(4.0) , y=(0.0)} {x=(0.0) , y=(0.0)} //True
distance :: Point Point -> Real
distance a b = ((a.x-b.x)^2.0) + ((a.y - b.y)^2.0)

//2. Give a list of people of record type Person.
// find how many girls like dog but dislike cat.
//The gender in record Person must be Algebraic type: Gender
:: Gender = Male | Female | Trans
:: Person = {name :: String , gender :: Gender , likedog :: Bool , likecat :: Bool}
f2 :: [Person] ->Int
f2 p = length[x \\ x<-p | x.likedog==True && x.likecat == False && isFemale x.gender]
isFemale :: Gender -> Bool
isFemale Female = True
isFemale _ = False
//Start =f2 [{name = "Alice", gender = Female, likedog = True, likecat = False },{name = "Alice2", gender = Female, likedog = True, likecat = True },{name = "Alice3", gender = Male, likedog = True, likecat = False }] //1
//Start =f2 [{name = "Alice", gender = Female, likedog = True, likecat = False },{name = "Alice2", gender = Female, likedog = True, likecat = False },{name = "Alice3", gender = Male, likedog = True, likecat = False }] //2

//3. find the student with the highest grade
:: Student = {name :: String , grade :: Real}
f3 :: [Student]->Real
f3 x = (last(sort x)).grade
instance < Student 
where
 < a b = a.grade < b.grade
//Start = f3 [{name = "Alice", grade = 22.1},{name = "Ban", grade = 58.0},{name = "kiki", grade = 94.2}] //94.2
------------------------------------------------------------classwork 7 group 4----------------------------------------------------------
:: Gender = Male | Female | Trans | PreferNotToTell
:: Survey = {hateFP :: Bool, gender :: Gender}
// Given a list of filled survey, return the number of people who did not tell their gender and hate FP :(
CountAnonymousHater :: [Survey] -> Int
CountAnonymousHater x = length[y\\y<-x | PreferNotToTell==y.gender && y.hateFP==True]
/*isPreferNotToTell :: Gender -> Bool
isPreferNotToTell PreferNotToTell = True
isPreferNotToTell _ =False*/
//Start = CountAnonymousHater [{hateFP = True, gender = Male}, {hateFP = True, gender = PreferNotToTell}, {hateFP = False, gender = PreferNotToTell}, {hateFP = False, gender = PreferNotToTell}] // 1
instance == Gender
where
 == Male Male = True
 == Female Female = True
 == Trans Trans = True
 == PreferNotToTell PreferNotToTell = True
 == _ _ =False
 
//2.Given a list of distinct name and a list of grade. 2 lists have the same length
// Generate a list of Person corresponding to the name list
// the grades of all Person should be the average of the 2nd list.
// Hint: The record Person should contain at least `name` and `average grade`
:: Person = {name :: String , averagegrade :: Real}
Generator :: [String] [Int] -> [Person]
Generator x y = [{name=a ,averagegrade=b}\\a<-x,b<-[(toReal(sum y) / toReal (length y))]]
//Start = Generator ["Evan", "Tringa"] [1, 4] // [(Person "Evan" 2.5), (Person "Tringa" 2.5)]
//Start = Generator ["Evan", "Tringa", "Viktoria"] [1, 4, 7] // [(Person "Evan" 4.0), (Person "Tringa" 4.0), (Person "Viktoria", 4.0)]

//3.Define records Point with real coordinates
// Given 3 Points A B C, decide whether B lies on the segment AC or not.
// Hint: AB + BC = AC
:: Point = {x:: Real , y::Real}
OnSegment :: Point Point Point -> Bool
OnSegment v1 v2 v3 = sqrt ((v1.x-v3.x)^2.0+(v1.y-v3.y)^2.0) == sqrt ((v1.x-v2.x)^2.0 + (v1.y-v2.y)^2.0) + sqrt ((v2.x-v3.x)^2.0 + (v2.y-v3.y)^2.0)
//Start = OnSegment {x = 0.0, y = 0.0} {x = 1.0, y = 1.0} {x = 2.0, y = 2.0}
----------------------------------------------------------classwork 8 group 1-----------------------------------------------------------------
/*1.
Given an array of Int and a single Int, use array
comprehension to double each element of the array,
keeping only the multiples of the second Int argument.
*/
f1 :: {Int} Int -> {Int}
f1 x y = {2*a\\a<-:x | (2*a) rem y == 0 }
//Start = f1 {1,2,3,4} 4 //{4,8}
//Start = f1 {3,4,5,7,2,9} 3 //{6,18}

/*2.
Implement a function that acts as 'foldr' for
arrays.
*/
arrFold :: (a -> b -> b) b {a} -> b
arrFold q w e = foldr q w (arrayToList e)
arrayToList :: {a} -> [a]
arrayToList x = [y\\y<-:x]
//Start = arrFold (+) 0 {1,2,3,4,5} // 15
//Start = arrFold (++) [] {[1],[2],[3],[4]} // [1,2,3,4]

/*3.
Given a Tree with nodes of type Person,
return the number of people who are older than 18.
That is, people born on or before 2001.11.22
*/
::Person = { name::String
			,birthday::(Int,Int,Int)
	}
::Tree a = Node a (Tree a) (Tree a)
	|Leaf
t1::Tree Person
t1 = Node {name = "hh", birthday = (2001,11,22)} Leaf Leaf
t2::Tree Person
t2 = Node {name = "hh", birthday = (2001,11,22)} (Node {name = "hr", birthday = (2001,11,21)} Leaf Leaf)(Node {name = "ht", birthday = (2001,11,23)} Leaf Leaf)
t3::Tree Person
t3 = Node {name = "hh", birthday = (2001,11,22)} (Node {name = "hr", birthday = (2001,11,21)} (Node {name = "hh", birthday = (2002,11,22)} Leaf Leaf) (Node {name = "hh", birthday = (1998,11,22)} Leaf Leaf))(Node {name = "ht", birthday = (2001,11,23)} Leaf Leaf)
f2 :: (Tree Person) -> Int
f2 a = length[x\\x<-(treeToList a) | x.birthday <=(2001,11,22)]
extractNode :: (Tree a) -> a
extractNode (Node x l r) = x
goL :: (Tree a) -> (Tree a)
goL (Node x l r) = l
goR :: (Tree a) -> (Tree a)
goR (Node x l r) = r
treeToList :: (Tree a) -> [a]
treeToList Leaf = []
treeToList tree = treeToList(goL tree)++[extractNode tree]++treeToList(goR tree)
//Start = f2 t2 //2
//Start = f2 t3  //3
--------------------------------------------------------------------------classwork 8 group 4----------------------------------------------------------------
/*
1.Given an array of Int and a single Int, use array
comprehension to double each element of the array,
keeping only the multiples of the second Int argument.
*/
f1 :: {Int} Int -> {Int}
f1 x y = {2 * z \\ z<-:x | (2*z) rem y == 0 }
//Start = f1 {1,2,3,4} 4 //{4,8}
//Start = f1 {3,4,5,7,2,9} 3 //{6,18}

/*
2.Given a Tree with nodes of type Person,
return the number of people who are older than 18.
That is, people born on or before 2001.11.22
*/
::Person = { name::String
			,birthday::(Int,Int,Int)
	}
::Tree a = Node a (Tree a) (Tree a)
	|Leaf
t1::Tree Person
t1 = Node {name = "hh", birthday = (2001,11,22)} Leaf Leaf
t2::Tree Person
t2 = Node {name = "hh", birthday = (2001,11,22)} (Node {name = "hr", birthday = (2001,11,21)} Leaf Leaf)(Node {name = "ht", birthday = (2001,11,23)} Leaf Leaf)
t3::Tree Person
t3 = Node {name = "hh", birthday = (2001,11,22)} (Node {name = "hr", birthday = (2001,11,21)} (Node {name = "hh", birthday = (2002,11,22)} Leaf Leaf) (Node {name = "hh", birthday = (1998,11,22)} Leaf Leaf))(Node {name = "ht", birthday = (2001,11,23)} Leaf Leaf)
/*
extractNode :: (Tree a) -> a
extractNode (Node x l r) = x
goL :: (Tree a) -> (Tree a)
goL (Node x l r) = l
goR :: (Tree a) -> (Tree a)
goR (Node x l r) = r
treeToList :: (Tree a) -> [a]
treeToList Leaf = []
treeToList tree = treeToList(goL tree)++[extractNode tree]++treeToList(goR tree)
f2 :: (Tree Person) -> Int
f2 a = length[x\\x<-(treeToList a) | x.birthday <= (2001,11,22)]
*/
f21 :: (Tree Person) -> Int
f21 a = length[x\\x<- (treeToList a) | isodder x.birthday]
//Start = f21 t2 //2
//Start = f2 t3  //3

/*
3.Given a Tree of type Person, return the same tree, except
with "_qualify" attached to the end of the names of each person
who is over 18.
*/
isodder :: (Int,Int,Int) -> Bool
isodder n 
|fst3 n < 2001 = True
|fst3 n == 2001 && snd3 n < 11 = True
|fst3 n == 2001 && snd3 n == 11 && thd3 n <= 22 = True
=False
f3 :: (Tree Person) -> (Tree Person)
f3 Leaf = Leaf
f3 (Node x l r) 
|isodder x.birthday = Node {x & name = (x.name +++ "_qualify"),birthday = (3,4,5)} (f3 l) (f3 r)
=Node x (f3 l) (f3 r)
//Start = f3 t2 //(Node (Person "hh_qualify" (2001,11,22)) (Node (Person "hr_qualify" (2001,11,21)) Leaf Leaf) (Node (Person "ht" (2001,11,23)) Leaf Leaf))
//Start = f3 t3  //(Node (Person "hh_qualify" (2001,11,22)) (Node (Person "hr_qualify" (2001,11,21)) (Node (Person "hh" (2002,11,22)) Leaf Leaf) (Node (Person "hh_qualify" (1998,11,22)) Leaf Leaf)) (Node (Person "ht" (2001,11,23)) Leaf Leaf))
---------------------------------------------------------------classwork 9 group 1---------------------------------------------------------------
/*
1. Check if a binary tree is an ordered and balanced tree
(balanced, the difference between the depth of left and right trees is at most 1)
(ordered, nodes on the left subtree < node < nodes on the right subtree)
*/
:: Tree a = Node a (Tree a) (Tree a) | Leaf
extractNode :: (Tree a) -> a
extractNode (Node x l r) = x
goL :: (Tree a) -> (Tree a)
goL (Node x l r) = l
goR :: (Tree a) -> (Tree a)
goR (Node x l r) = r
treeToList :: (Tree a) -> [a]
treeToList Leaf = []
treeToList tree = treeToList(goL tree)++[extractNode tree]++treeToList(goR tree)
f1 :: (Tree a) -> Bool | Ord,Eq a
f1 t = l == sort l
where
    l = treeToList t
//Start = f1 (Node 26 (Node 24 Leaf Leaf) (Node 28 (Node 27 Leaf Leaf) Leaf)) //True
//Start = f1 (Node 26 (Node 24 Leaf Leaf) (Node 29 (Node 27 Leaf (Node 28 Leaf Leaf)) Leaf)) //False
//Start = f1 (Node 26 (Node 24 Leaf Leaf) (Node 29 (Node 27 Leaf Leaf) (Node 28 Leaf Leaf))) //False
//Start = f1 (Node 26 (Node 31 Leaf Leaf) (Node 28 (Node 27 Leaf Leaf) Leaf)) //False

//2. Define rational class Q for rational numbers. Define instances for addition and multiplication
:: Q = {nom :: Int , den :: Int}
simplify :: Q -> Q
simplify {nom=n,den=d}
| d == 0 = abort "denominator of Q is 0!"
| d < 0 = {nom = ~n / g, den = ~d / g}
| otherwise = {nom = n / g, den = d / g}
where
 g = gcd n d
instance + Q
where
 + {nom=a,den=b} {nom=c,den=d} = simplify{nom=a*d+b*c,den=b*d}
instance * Q
where
 * {nom=a,den=b} {nom=c,den=d} = simplify{nom=a*c,den=b*d}
//Start = {nom=2, den=4} + {nom=5, den=6} // (Q 4 3)
//Start = {nom=2, den=4} * {nom=5, den=6}  // (Q 5 12)

//3. Define an abstract type queue

:: Queue a :==[a]
newQueue :: (Queue a) // Creates empty queue 
newQueue = []
isempty :: (Queue a) -> Bool // Checks if a queue is empty 
isempty [] = True
isempty x = False
enqueue :: a (Queue a) -> Queue a // add an item to the queue 
enqueue a x = x ++ [a]
dequeue :: (Queue a) -> Queue a //Remove an item  from the queue 
dequeue x = [last x]
peek :: (Queue a) -> a //Gets the element at the front of the queue
peek [x:xs] = x

//Start = isempty newQueue //True
//Start =  enqueue (1,2) (enqueue (6,5) (enqueue (0,9) newQueue)) //[(0,9),(6,5),(1,2)]
//Start =  peek (enqueue (1,2) (enqueue (6,5) (enqueue (0,9) newQueue))) //(0,9)
//Start =  dequeue (enqueue (6,5) (enqueue (0,9) newQueue)) //[(6,5)]

---------------------------------------------------------------classwork 9 group 4---------------------------------------------------------------
/*
1. Check whether a binary search tree is a degenerate case
(
  Degenerate tree: All of its nodes (for simplicity, exclude leaves) formed a straight line
  Binary search tree: For every nodes, its left node value < its value < its right node value.
)
*/
:: Tree a = Node a (Tree a) (Tree a) | Leaf
extractNode :: (Tree a) -> [a]
extractNode Leaf = []
extractNode (Node x l r) = (extractNode l) ++ [x] ++ (extractNode r)
f1 :: (Tree a) -> Bool | Ord,Eq a
f1 x = extractNode x == sort(extractNode x)
//Start = f1 (Node 3 (Node 2 (Node 1 Leaf Leaf) Leaf) Leaf) // True
//Start = f1 (Node 1 Leaf (Node 2 Leaf (Node 3 Leaf Leaf))) // True
//Start = f1 (Node 1 Leaf (Node 2 Leaf (Node 1 Leaf Leaf))) // False
//Start = f1 (Node 1 (Node 2 Leaf Leaf) (Node 3 Leaf Leaf)) // False

//2. Define rational class Q for rational numbers. Define instances for addition and multiplication
:: Q = {num :: Int , den :: Int}
instance + Q
where 
 (+) x y = Q (x.num*y.den+y.num*x.den) (x.den*y.den)
 
instance * Q
where 
 (*) x y = Q (x.num*y.num) (x.den*y.den) 
 
mkQ :: x x -> Q | toInt x
mkQ n d = simplify {num = toInt n, den = toInt d}
simplify :: Q -> Q
simplify {num=n,den=d}
| d == 0 = abort "denominator of Q is 0!"
| d < 0 = {num = ~n / g, den = ~d / g}
| otherwise = {num = n / g, den = d / g}
where
 g = gcd n d
Q n d = simplify {num=n,den=d} 
//Start = Q 2 4 + Q 5 6 // (Q 4 3)
//Start = Q 2 4 * Q 5 6  // (Q 5 12)
//Start = {num=2, den=4} + {num=5, den=6} // (Q 4 3)
//Start = {num=2, den=4} * {num=5, den=6}  // (Q 5 12)

//3. Define abstract type PriorityQueue
:: Queue a :== [a]
newQueue :: (Queue a) // Creates empty queue 
newQueue = []
isempty :: (Queue a) -> Bool // Checks if a queue is empty 
isempty [] = True
isempty x = False
push :: a (Queue a) -> Queue a // add an item to the end of the queue 
push e s = s++[e]
pop :: (Queue a) -> (Queue a, a) | Ord a // Remove the biggest item from the queue 
pop s = (init (sort s),last(sort s))
PQ :: (Queue Int)
PQ = [1, 2, 3, 4, 5]
//Start = isempty newQueue //True
//Start =  push 1 PQ //[1,2,3,4,5,1]
//Start =  pop PQ //([1,2,3,4,1], 5)
//Start = isempty newQueue //True
---------------------------------------------------------classwork 10 group 1 -------------------------------------------------
Same here as above

---------------------------------------------------------classwork 10 group 4 -------------------------------------------------
:: Tree a = Node a (Tree a) (Tree a) | Leaf

ourTree :: (Tree Int)
ourTree = Node 15(Node 3(Node 1 Leaf Leaf)(Node 10(Node 7 Leaf (Node 8 Leaf Leaf))(Node 13 (Node 11 Leaf Leaf) Leaf)))(Node 20 (Node 18 Leaf (Node 19 Leaf Leaf)) (Node 21 Leaf (Node 26 (Node 24 Leaf Leaf) (Node 28 Leaf Leaf))))
tree1 = Node 10 (Node 7 (Node 3 Leaf Leaf) (Node 15 Leaf Leaf)) (Node 5 Leaf (Node 10 Leaf Leaf))
tree2 = Node 9 (Node 1 (Node 0 (Node 7 Leaf Leaf) Leaf) (Node 15 Leaf Leaf)) (Node 4 (Node 4561 Leaf Leaf) (Node 8 (Node 1663 Leaf Leaf) Leaf))
unitTree = Node 1337 Leaf Leaf
noTree = Leaf

/*
Write a function returning the depth
of the largest prime number in the tree.
*/
isPrime :: Int -> Bool
isPrime x = and[x rem n <> 0\\n <- [2..(x-1)]]

extractNode :: (Tree Int) -> [Int]
extractNode Leaf = []
extractNode (Node x l r) = (extractNode l) ++ [x] ++ (extractNode r)

treeToList :: (Tree Int) Int -> [(Int,Int)]
treeToList Leaf cnt = [(0,cnt)]
treeToList (Node x l r) cnt
| isPrime x = [(x,cnt)]
=(treeToList l (cnt+1)) ++(treeToList r (cnt+1))

deepPrime :: (Tree Int) -> Int
deepPrime Leaf =0
deepPrime tree = hd[y\\(x,y)<-(treeToList tree 0) | isPrime x && bigprime == x]
 where
 bigprime = last[z\\z<-extractNode tree | isPrime z]
//Start = deepPrime ourTree //3
//Start = deepPrime tree1 //1
//Start = deepPrime Leaf //0

/*
Write a class 'lol'
with instances for Int, Real, Bool, String, and [a].
For Int, return that number + 1.
For Real, return the square root of that number.
For Bool, return the opposite boolean.
For String, return the String in reverse.
For a list [a], return the list concatenated to itself.
*/
class lol a :: a -> a
instance lol Int
 where lol a = a + 1
instance lol Real
 where lol a = sqrt a
instance lol Bool
 where 
 lol True = False
 lol False = True
instance lol String
 where
 lol x = {b\\b<-reverse[a\\a<-:x]}
instance lol [a]
 where
 lol a = a ++ a
//Start = lol 41 //42
//Start = lol 176752.9764 //420.42
//Start = lol True //False
//Start = lol "partyboob" //"boobytrap"
//Start = lol [1,2,3,4] //[1,2,3,4,1,2,3,4]

/*
Given an array, extract the elements
located at the prime numbered index locations
and return them in a new array.
*/
primeIndex :: (a e) -> (a e) | Array a e
primeIndex x = {x.[a-1]\\a<-[1..size(x)] | isPrime a}
/*Start :: {Int}
Start = primeIndex {x\\x<-[1..100]}*/
//{1,2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97}
//Start = primeIndex "Helplooblo aWes omrporlyeet!d !"
