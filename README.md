# Unification-in-Haskell
Syntactic unification algorithm in Haskell.

Here I'm planning to describe in an algorithm a syntactic unification of two terms. 
In this first version I have only implemented the basic algorithm of a recursive descent based on an idea of a Rule-based approach. This algorithm might be eexponentially slow in case of nested recursive calls where we need a duplication of the same term, for example, if one want to unify f(x_1,...,x_n,g(y_0,y_0),...,g(y_(n-1),y_(n-1)),y_n) and f(g(x_0,x_0),...,g(x_(n-1),x_(n-1)),y_1,...,y_n,x_n).

Later, I'm planning 1) to implement a better representation of a problem to speed-up the algorithm, and 2) to add a special parser in order to more easily write inputs.
