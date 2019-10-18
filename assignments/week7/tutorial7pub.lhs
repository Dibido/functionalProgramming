Let's prepare you for 7.6:
For those who have read the assignment, you would have noticed that we wanted
you to use something called the foldr fusion law. But what is this thing?

Let me just write the law down:
f (g a b) = h a (f b) -> f (foldr g e xs) = foldr h (f e) xs

Let us consider an example where we want to prove that
1 + sum [1 .. 10] is the same as sum [1, 1, .. 10]
First express sum as foldr:
sum xs = foldr (+) 0 xs
So:
	1 + foldr (+) 0 [1 .. 10] = foldr (+) 1 [1 .. 10]
If we write it in the form of the fusion law:
	(+1) (foldr (+) 0 [1 .. 10]) = foldr (+) ((+1) 0) [1 .. 10]
This holds, if we prove the condition of the implication. Where:
	f = (+1)
	g = +
	h = +
Combined:
	(+ 1) (a + b) ?= a + ((+1) b)
Yes, by law of associativity and transitivity.
Therefore:
1 + foldr (+) 0 [1 .. 10] = foldr (+) 1 [1 .. 10]

Alright, let do a harder example:
To prove: reverse . reverse = id
To fuse folds, we need folds.
Let's write reverse and id as folds:
	reverse xs = foldr (\b c -> c ++ [b]) [] xs
	id xs = foldr (\b c -> b : c) [] xs
Now, let's write the original statement with the folds:
	reverse (foldr (\b c -> c ++ [b]) [] xs) ?= foldr (\d e -> d : e) (reverse []) xs
Identify the f g and h:
	f = reverse
	g = \b c -> c ++ [b]
	h = \d e -> d : e
Proof that:
	reverse (b ++ [a]) = a : (reverse b)
This, we can prove:
	reverse (b ++ [a])
	=> reverse [a] ++ reverse b -- This is true, we have seen this earlier.
	=> [] ++ [a] ++ reverse b -- 11
	=> [a] ++ reverse b -- 1
	=> (a:[]) ++ reverse b -- List notation
	=> a : ([] ++ reverse b) -- 2
	=> a : reverse b -- 1

In short, proving using the foldr fusion law will typically follow the
following steps:
1. Write your supposed equality in the form of the RHS
2. Identify f, g, h
3. Prove the LHS
