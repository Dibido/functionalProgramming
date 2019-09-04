module Database
where

type Person  =  (Name, Age, FavouriteCourse)

type Name             =  String
type Age              =  Integer
type FavouriteCourse  =  String

frits, peter, ralf :: Person
frits  =  ("Frits",  33,  "Algorithms and Data Structures")
peter  =  ("Peter",  57,  "Imperative Programming")
ralf   =  ("Ralf",   33,  "Functional Programming")
ralph   =  ("Ralph",   23,  "Functional Programming")
dibran =  ("Dibran", 22,  "Functional Programming")

students   ::  [Person]
students   =  [frits, peter, ralf, ralph, dibran]

age :: Person -> Age
age (_n, a, _c)  =  a

name :: Person -> Name
name (n, _a, _c) = n

favouriteCourse  :: Person -> FavouriteCourse
favouriteCourse (_n, _a, c) = c

showPerson       :: Person -> String
showPerson (n, a, c) = n ++ " , " ++ show a ++ " , " ++ c ++ "."

twins            :: Person -> Person -> Bool
twins (_n1, a1, _c1) (_n2, a2, _c2)  = a1 == a2

increaseAge      :: Person -> Person
increaseAge (_n, a, _c) = (_n, a + 1, _c)

--Expressions
addTwoYears :: Person -> Person
addTwoYears(_n, a, _c) = (_n, a + 2, _c)
incrementByTwo = map addTwoYears students

promoteStudent :: Person -> Person
promoteStudent (n, _a, _c) = ("dr " ++ n, _a, _c)
promoteAllStudents = map promoteStudent students

findFrits = map (\ p -> (age p, name p)) (filter (\ p -> name p == "Frits") students)

funProg = "Functional Programming"
impProg = "Imperative Programming"

findFavFuncProg = map (\ p -> (age p, name p, favouriteCourse p)) (filter (\ p -> favouriteCourse p == funProg) students)

inTwenties (_n, a, _c) = a > 19 && a < 30
findInTwenties = map (\ p -> (age p, name p, favouriteCourse p)) (filter (\p -> inTwenties p) students)

findFavFuncProgAndTwenties = map (\ p -> (age p, name p, favouriteCourse p)) (filter (\p -> inTwenties p && favouriteCourse p == funProg) students)

findFavImpProgOrTwenties = map (\ p -> (age p, name p, favouriteCourse p)) (filter (\p -> inTwenties p || favouriteCourse p == impProg) students)
