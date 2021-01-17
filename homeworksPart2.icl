module homeworksPart2
import StdEnv

------------------------------------------------------------------homework 6 ----------------------------------------------
:: Person = {name::String, age::Int}

Tringa :: Person
Tringa = {name = "Tringa", age = 42}
Hossam :: Person
Hossam = {name = "Hossam", age = 69}
Nicola :: Person
Nicola = {name = "Nicola", age = 9001}
Zuka :: Person
Zuka = {name = "Zuka", age = 20}

/*
Write a function that finds the older of two people.
*/
//older :: Person Person -> String
//Start = older Tringa Hossam //"Hossam"
//Start = older Zuka Nicola //"Nicola"

-----------------------------------------------------------------homework 7 -------------------------------------------------
:: Month = January | February | March | April | May | June | July | August | September | October | November | December
:: Gender = Male | Female | AttackHelicopter | Nghia
:: Date = {year::Int, month::Month, day::Int}
:: Person = {name::String, gender::Gender, age::Int, birthday::Date, isDead::Bool}
:: Account = {number::Int, owner::Person, balance::Real, dateCreated::Date}

Tringa :: Person
Tringa = {name = "Tringa", gender = Female, age = 42, birthday = {year = 1977, month = May, day = 12}, isDead = False}
Hossam :: Person
Hossam = {name = "Hossam", gender = AttackHelicopter, age = 69, birthday = {year = 1950, month = June, day = 27}, isDead = False}
Nicola :: Person
Nicola = {name = "Nicola", gender = Male, age = 9001, birthday = {year = -6982, month = January, day = 1}, isDead = True}
Zuka :: Person
Zuka = {name = "Zuka", gender = Male, age = 20, birthday = {year = 1999, month = February, day = 11}, isDead = False}
LeMinhNghia :: Person
LeMinhNghia = {name = "Nghia", gender = Nghia, age = 420, birthday = {year = 1599, month = February, day = 4}, isDead = True}

A00 :: Account
A00 = {number = 0, owner = Nicola, balance = 9000.01, dateCreated = {year = 1945, month = August, day = 6}}

A01 :: Account
A01 = {number = 1, owner = Hossam, balance = -1337.42, dateCreated = {year = 1900, month = December, day = 25}}

A02 :: Account
A02 = {number = 2, owner = Tringa, balance = 123.45, dateCreated = {year = 2010, month = May, day = 12}}

A03 :: Account
A03 = {number = 3, owner = Zuka, balance = 35.0, dateCreated = {year = 2019, month = November, day = 11}}

A420 :: Account
A420 = {number = 420, owner = LeMinhNghia, balance = 420.0, dateCreated = {year = 420, month = April, day = 20}}

SittBank :: {Account}
SittBank = {A00,A01,A02,A03}

/*
1.Write a function that takes a Real and an
array of Accounts updates each Account
in an array by adding the number to the balance.
*/
gimmeMoney :: {Account} Real-> {Account}
gimmeMoney account money = {{a & balance = a.balance + money}\\a<-:account}
//Start = gimmeMoney SittBank 1000000.0 //{(Account 0 (Person "Nicola" Male 9001 (Date -6982 January 1) True) 1009000.01 (Date 1945 August 6)),(Account 1 (Person "Hossam" AttackHelicopter 69 (Date 1950 June 27) False) 998662.58 (Date 1900 December 25)),(Account 2 (Person "Tringa" Female 42 (Date 1977 May 12) False) 1000123.45 (Date 2010 May 12)),(Account 3 (Person "Zuka" Male 20 (Date 1999 February 11) False) 1000035 (Date 2019 November 11))}
//Start = gimmeMoney SittBank -9999999999999999.9999999999
//{(Account 0 (Person "Nicola" Male 9001 (Date -6982 January 1) True) -9.999999999991e+15 (Date 1945 August 6)),(Account 1 (Person "Hossam" AttackHelicopter 69 (Date 1950 June 27) 
//False) -1.00000000000013e+16 (Date 1900 December 25)),(Account 2 (Person "Tringa" Female 42 (Date 1977 May 12) False) -9.99999999999988e+15 (Date 2010 May 12)),(Account 3 (Person "Zuka" Male 20 (Date 1999 February 11) False) -9.99999999999996e+15 (Date 2019 November 11))}

/*
2.Write a function that takes an array of Accounts
and returns the name of the owner with the
highest balance who is NOT dead.
*/
soRich :: {Account} -> String
soRich account = (last(sort[x\\x<-:account | x.owner.isDead==False])).owner.name
instance < Account
where
 < a b = a.balance < b.balance
//Start = soRich {A01, A02, A03} //"Tringa"
//Start = soRich SittBank //"Tringa"

/*
3.Write a function that takes a tuple containing a
condition and two numbers, and an array of Accounts,
and returns an array containing all accounts
that match the condition, and that were created between
the two years (numbers provided).
*/
query :: ((Account -> Bool),Int,Int) {Account} -> {Account}
query (a,b,c) account = {q\\q<-:account | and[q.dateCreated.year>=b , q.dateCreated.year<c, a q]}
instance == Gender
where
 == Male Male = True
 == Female Female = True
 == AttackHelicopter AttackHelicopter = True
 == Nghia Nghia = True
 == _ _ = False
//Start = query ((\x = x.owner.gender==Male),0,2000) SittBank //{(Account 0 (Person "Nicola" Male 9001 (Date -6982 January 1) True) 9000.01 (Date 1945 August 6))}
//Start = query ((\x = x.balance > 0.0),-9999,9999) SittBank //{(Account 0 (Person "Nicola" Male 9001 (Date -6982 January 1) True) 9000.01 (Date 1945 August 6)),(Account 2 (Person "Tringa" Female 42 (Date 1977 May 12) False) 123.45 (Date 2010 May 12)),(Account 3 (Person "Zuka" Male 20 (Date 1999 February 11) False) 35 (Date 2019 November 11))}
//Start = query ((\x = not x.owner.isDead && length[z\\z<-:x.owner.name]==6),0,2020) {A00,A01,A02,A03,A420} //{(Account 1 (Person "Hossam" AttackHelicopter 69 (Date 1950 June 27) False) -1337.42 (Date 1900 December 25)),(Account 2 (Person "Tringa" Female 42 (Date 1977 May 12) False) 123.45 (Date 2010 May 12))}
----------------------------------------------------------------homework 8 ---------------------------------------------------------------

:: Tree a = Node a (Tree a) (Tree a) | Leaf
:: Yggdrasil a b c = Core a (Tree b) {c}

ourTree :: (Tree Int)
ourTree = Node 15(Node 3(Node 1 Leaf Leaf)(Node 10(Node 7 Leaf (Node 8 Leaf Leaf))(Node 13 (Node 11 Leaf Leaf) Leaf)))(Node 20 (Node 18 Leaf (Node 19 Leaf Leaf)) (Node 21 Leaf (Node 26 (Node 24 Leaf Leaf) (Node 28 Leaf Leaf))))

oddTree :: (Tree Int)
oddTree = (Node 7 (Node 3 (Node 1 Leaf Leaf) (Node 5 Leaf Leaf)) (Node 11 (Node 9 Leaf Leaf) Leaf))

flow1 :: (Yggdrasil ([Bool] -> Bool) Int (Int -> Bool))
flow1 = Core and ourTree {isEven, ((<)10)}


flow2 :: (Yggdrasil ([Bool] -> Bool) Int (Int -> Bool))
flow2 = Core or oddTree {((<)10),((==)1),(\x = and[x rem n <> 0\\n<-[2..(x-1)]])}

/*
Write an instance of equality (==) between two Trees,
such that we can use calls such as treeA == treeB.
*/
instance == (Tree a) | Eq a
where
 == Leaf Leaf = True
 == (Node x l r) (Node y le ri) = (x==y) && (l==le) && (r==ri)
//Start = ourTree == ourTree //True
//Start = ourTree == oddTree //False
//Start = isMember ourTree [ourTree, oddTree, Leaf] //True

/*
Write a function that will take the data constructs
and apply the array of conditions in the array portion,
and apply it with the boolean aggregator function (first portion)
to every element of the tree (second portion),
returning a list of elements that return True after processing.
For example:
Given a data construct of Core or someTree {cond1,cond2,cond3,cond4},
then return a list of elements from someTree, given that they return
True for one or more (the 'or' function) of the conditions from the
array (cond1, cond2, etc...).
Hint: Arrays are easier handled as lists for some operations.
*/
flowApp :: (Yggdrasil ([Bool] -> Bool) Int (Int -> Bool)) -> [Int]
flowApp (Core a (Node x l r) c) = [num\\num<-treeToList (Node x l r) | a[q num \\ q <-:c]]

extractNode :: (Tree a) -> a
extractNode (Node x l r) = x 
goL :: (Tree a) -> (Tree a)
goL (Node x l r) = l
goR :: (Tree a) -> (Tree a)
goR (Node x l r) = r
treeToList :: (Tree a) -> [a]
treeToList Leaf = []
treeToList tree = treeToList(goL tree)++[extractNode tree]++treeToList(goR tree)

//Start = flowApp flow1 //[18,20,24,26,28]
//Start = flowApp flow2 //[1,3,5,7,11]

/*
Write a function that checks if a String is a palindrome or not.
Hint: Strings are just arrays. ;)
*/
palindrome :: String -> Bool
palindrome x = (palin x) == reverse (palin x)
palin :: String -> [Char]
palin x = [a\\a<-:x]
//Start = palindrome "racecar" //True
//Start = palindrome "amanaplanacanalpanama" //True
//Start = palindrome "boobytrap" //False

---------------------------------------------------------------Homework 9 ---------------------------------------------------------------------

:: Complex = { real :: Real, imaginary :: Real}

:: RoflCopter :== Complex

timesRofl :: RoflCopter Int -> RoflCopter
timesRofl x n = {real = x.real * toReal( n) ,imaginary =x.imaginary * toReal( n) } 

/*
Given the above defined abstract data type, record, and function:
you are given a task to expand functionality by making RoflCopter
a Complex.
Refactor the code, so we can multiply the new RoflCopter by an Int.
Definition: Refactor (verb)
"a disciplined technique for restructuring an existing body of code,
altering its internal structure without changing its external behavior.
Its heart is a series of small behavior preserving transformations."
That is, make the code work, without changing the arguments of timesRofl.
You will need to change RoflCopter's definition, and the internal workings
of timesRofl.
*/
//Start = timesRofl {real = 2.0, imaginary = 3.0} 5 //(Complex 10 15)
//Start = timesRofl {real = (acos -1.0), imaginary = (exp 1.0)} 100 //(Complex 314.159265358979 271.828182845905)
//Start = timesRofl {real = 0.0, imaginary = 0.0} 420 //(Complex 0 0)

/*
Implement the Dictionary data type.
A dictionary is a list containing
pairings of two entries.
The first entry is the "key".
The second entry is the "value".
Every key has an value associated with it.
Keys are unique, there can not be duplicates.
Values can be duplicated between keys.
*/

//Type definition.
:: Dictionary :== [(String,Int)]
//Convenient toString instance:
instance toString Dictionary
where
    toString [] = "End of Dictionary"
    toString [(key,value):rest] = "Key: "+++(toString key)+++", "+++"Value: "+++(toString value)+++"\n"+++(toString rest)
//Custom Dictionary for testing.

myDict:: Dictionary
myDict = addEntry (addEntry (addEntry (addEntry (addEntry (addEntry (addEntry newDict "Evan" 42) "Hossam" 69) "Nghia" 420) "Tringa" 1337) "Zuka" 13) "Nikola" 9001) "Ying" 420


//Create a new dictionary. Should be empty.
newDict :: Dictionary
newDict=[]
//Add a new entry. Be sure to check for duplicate keys!
addEntry :: Dictionary String Int -> Dictionary
addEntry x a b
| lookup1 x a = x ++ [(a,b)]
= x
//Lookup the value associated with a key.
//Return 0 if the key isn't found.
(lookup) :: Dictionary String -> Int
(lookup) [] str = 0
(lookup) [(key,value):rest] str
| key == str = value
= (lookup) rest str


lookup1 :: Dictionary String -> Bool
lookup1 [] str = True
lookup1 [(key,value):rest] str
| key == str = False
= lookup1 rest str
//Find all keys associated with a certain value.
//Return as a list of keys.
(findKeys) :: Dictionary Int -> [String]
(findKeys) [] num = []
(findKeys) [(key,value):rest] num
| value == num = [key] ++ (findKeys) rest num
= (findKeys) rest num

//Start = toString newDict //"End of Dictionary"
//Start = toString myDict
/*
"Key: Evan, Value: 42
Key: Hossam, Value: 69
Key: Nghia, Value: 420
Key: Tringa, Value: 1337
Key: Zuka, Value: 13
Key: Nikola, Value: 9001
Key: Ying, Value: 420
End of Dictionary"
*/
//Start = toString(addEntry myDict "Hossam" 0)
/*
"Key: Evan, Value: 42
Key: Hossam, Value: 69
Key: Nghia, Value: 420
Key: Tringa, Value: 1337
Key: Zuka, Value: 13
Key: Nikola, Value: 9001
Key: Ying, Value: 420
End of Dictionary"
*/
//Start = myDict lookup "Nikola" //9001
//Start = myDict lookup "Viktoria" //0
//Start = myDict findKeys 42 //["Evan"]
//Start = myDict findKeys 420 //["Nghia","Ying"]
//Start = myDict findKeys 99999999999 //[]

-----------------------------------------------------------Homework 10 ----------------------------------------------------------

//Given these Algebraic Data Types, Records, and Tree...
:: Gender = Male | Female | NonBinary | AttackHelicopter | Nghia | OOBLECK
:: LivingStatus = Alive | Deceased | Undead
:: MarriageStatus = Married | Divorced | Single | Tinder
:: Person = { name :: String, gender :: Gender, age :: Int, livingStatus :: LivingStatus, marriageStatus :: MarriageStatus}
:: FamilyTree a = Name a (FamilyTree a) (FamilyTree a) | End | Polygamy [[[[[[[[[[[[[FamilyTree a]]]]]]]]]]]]]

//And these people:
Olivia = {name = "Olivia", gender = Female, age = 19, livingStatus = Alive, marriageStatus = Single}
Amelia = {name = "Amelia", gender = Female, age = 83, livingStatus = Alive, marriageStatus = Married}
Isla = {name = "Isla", gender = Female, age = 40, livingStatus = Alive, marriageStatus = Married}
Emily = {name = "Emily", gender = Female, age = 73, livingStatus = Alive, marriageStatus = Divorced}
Ava = {name = "Ava", gender = Female, age = 18, livingStatus = Alive, marriageStatus = Single}
Lily = {name = "Lily", gender = Female, age = 50, livingStatus = Alive, marriageStatus = Divorced}
Oliver = {name = "Oliver", gender = Male, age = 56, livingStatus = Alive, marriageStatus = Married}
Harry = {name = "Harry", gender = Male, age = 45, livingStatus = Alive, marriageStatus = Married}
Jack = {name = "Jack", gender = Male, age = 90, livingStatus = Deceased, marriageStatus = Married}
George = {name = "George", gender = Male, age = 43, livingStatus = Alive, marriageStatus = Married}
Noah = {name = "Noah", gender = Male, age = 74, livingStatus = Undead, marriageStatus = Divorced}
Freddie = {name = "Freddie", gender = Male, age = 24, livingStatus = Alive, marriageStatus = Single}
Ethan = {name = "Ethan", gender = Male, age = 20, livingStatus = Alive, marriageStatus = Single}

//And each person's immediate parents:
OliviaTree = Name Olivia OliverTree HarryTree
OliverTree = Name Oliver End End
HarryTree = Name Harry AmeliaTree JackTree
AmeliaTree = Name Amelia End End
JackTree = Name Jack End End
EthanTree = Name Ethan GeorgeTree IslaTree
GeorgeTree = Name George AmeliaTree JackTree
IslaTree = Name Isla NoahTree EmilyTree
NoahTree = Name Noah End End
EmilyTree = Name Emily End End
AvaTree = Name Ava LilyTree OliverTree
LilyTree = Name Lily End End
FreddieTree = Name Freddie End End

personsList = [Olivia, Amelia, Isla, Emily, Ava, Lily, Oliver, Harry, Jack, George, Noah, Freddie, Ethan]
familyList = [OliviaTree, OliverTree, HarryTree, AmeliaTree, JackTree, EthanTree, GeorgeTree, IslaTree, NoahTree, EmilyTree, AvaTree, LilyTree, FreddieTree]


/*
Write a function that tests if two persons are cousins
Condition: They share a grandparent.
*/

instance == (FamilyTree Person)
where
     == End End = True
     == _ _ = False


areCousins :: Person Person -> Bool
areCousins x y
| x.name == y.name = False 
=or(judge (f21 (f3 (f11 (Parents familyList  x)))) (f21 (f3 (f11 (Parents familyList  y)))))

Parents :: [FamilyTree Person] Person ->[FamilyTree Person]
Parents x b = [(Name m l r)\\(Name m l r)<-x | b.name == m.name ]

f11 :: [FamilyTree Person] -> [FamilyTree Person]
f11 x = [l\\ (Name m l r) <- x] ++ [r\\(Name m l r)<-x]

f3 :: [FamilyTree Person] -> [FamilyTree Person]
f3 x = [l\\ (Name m l r) <- x] ++ [r\\(Name m l r)<-x]

f21 :: [(FamilyTree Person)] -> [String]
f21 [] = []
f21 [x:xs]
| x==End = [] ++ f21 xs
=get x ++ f21 xs

where
 get (Name m l r) = [m.name]
judge :: [String] [String] -> [Bool]
judge [] _ = [False]
judge [x:xs] y
| isMember x y = [True] ++ judge xs y
= [False] ++ judge xs y 


//Start = f2 (f3 (f1 (Parents familyList  Ethan)))
//Start = areCousins Ethan Olivia //True
//Start = areCousins Ethan Ava //False
//Start = areCousins George Harry //False
//Start = areCousins George Isla //False
//Start = areCousins Ethan Ethan //False (same person)


:: IpV4Address :== (Int,Int,Int,Int)
:: Router = { nodeName :: String, ipAddress :: IpV4Address, activeStatus :: Bool}
:: Availability = NodeUp | NodeDown
:: Status = OK (Availability,Router) | NOK
instance == Availability
where
    == NodeUp NodeUp = True
    == NodeDown NodeDown = True
    == _ _ = False
instance toString Status
where
    toString NOK = "\nNo Router Match.\n"
    toString (OK (a,{nodeName = n, ipAddress = (i1,i2,i3,i4)}))
    |  a == NodeUp = stdOutput+++"Status: Available\n"
    = stdOutput +++ "Status: Unavailable\n"
    
    where
        tab = "    "
        stdOutput = "\nRouter Match:\n"+++tab+++"Router Name: "+++n+++"\n"+++tab+++"IpV4 Address: "+++toString i1+++"."+++toString i2+++"."+++toString i3+++"."+++toString i4+++"\n"+++tab

r1 :: Router
r1 = {nodeName = "PL1", ipAddress = (10,0,0,1), activeStatus = True}
r2 :: Router
r2 = {nodeName = "PL2", ipAddress = (10,0,0,2), activeStatus = True}
r3 :: Router
r3 = {nodeName = "PL3", ipAddress = (10,0,0,3), activeStatus = False}
r4 :: Router
r4 = {nodeName = "PL4", ipAddress = (10,0,0,4), activeStatus = True}
r5 :: Router
r5 = {nodeName = "PL5", ipAddress = (10,0,0,5), activeStatus = False}

CurrentRouters :: [Router]
CurrentRouters = [r1,r2,r3,r4,r5]

/*
Write a class of functions that will return a Router's status.
The function should be able to take a nodeName in the form
of a String, or an IpV4 address in the form of the predefined
type IpV4Address and return a Status, provided a list of existing routers.
The Status should be NOK if there is no matching Router.
The Status should be OK if there is a matching Router.
Availability will be NodeUp if the activeStatus is True.
Otherwise Availability will be NodeDown.

Note: You don't need to touch any of the definitions above, 
those have been written for you and will work provided that
your getStatus works properly.
*/

giveact :: Bool -> Availability
giveact True = NodeUp
giveact False = NodeDown

f1 :: String [Router] -> Status
f1 x y 
|isMember x [z.nodeName \\ z <- y] = OK (giveact (hd(filter (getname x) y)).activeStatus, (hd(filter (getname x) y))   )
= NOK
where getname x y 
             |x == y.nodeName = True
             |x <> y.nodeName = False 

f2::  IpV4Address [Router] -> Status
f2 x y 
|isMember x [z.ipAddress \\ z <- y] = OK (giveact (hd(filter (getip x) y)).activeStatus, (hd(filter (getip x) y))   )
= NOK

where getip x y
       |x == y.ipAddress = True
       |x<> y.ipAddress  = False


instance == IpV4Address 
where == (a,b,c,d) (q,w,e,r) 
          |(a == q && b == w && c == e && d == r) = True
          = False
         

 
class getStatus a b :: a b-> Status

instance getStatus String [Router]
where 
         getStatus  x y = f1 x y
instance getStatus IpV4Address [Router]
where      
         getStatus x y = f2 x y
//Start = toString(getStatus "PL1" CurrentRouters)
/*
"
Router Match:
    Router Name: PL1
    IpV4 Address: 10.0.0.1
    Status: Available
"
*/

//Start = toString(getStatus (10,0,0,5) CurrentRouters)
/*
"
Router Match:
    Router Name: PL5
    IpV4 Address: 10.0.0.5
    Status: Unavailable
"
*/

//Start = toString(getStatus "NULL_ROUTER" CurrentRouters)
/*
"
No Router Match.
"
*/
