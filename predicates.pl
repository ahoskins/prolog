%%%%%%%%%%%%%%%%%% Helpers %%%%%%%%%%%%%%%%%%%%%%

append([],L,L). % empty list and L is just L
append([A|L],L1,[A|L2]) :- % append
  append(L,L1,L2).

setMember(X,[X|_]).
setMember(X,[_|Y]) :-
    setMember(X,Y).




/*
xreverse/2

xreverse(L, R) will reverse a list, where L is a given list
and R is either a variable or another given list.

Examples:
xreverse([7,3,4],[4,3,7]) should return yes,
xreverse([7,3,4],[4,3,5]) should return no,
xreverse([7,3,4], R) should return R = [4,3,7].
*/

% public api
xreverse(List, Reverse) :-
  xreverse(List, [], Reverse).

xreverse([], Result, Result). % base case
xreverse([Head|Tail], Partial, Reverse) :- % worker
  xreverse(Tail, [Head|Partial], Reverse).





/*
xunique/2

xunique(L, Lu) where L is a given list of atoms and Lu is a copy of L where
all the duplicates have been removed. Lu can be either a variable, or a given list.
The elements of Lu should be in the order in which they first appear in L.

Examples:
xunique([a,c,a,d], L) should return L = [a,c,d],
xunique([a,c,a,d], [a,c,d]) should return yes,
xunique([a,c,a,d], [c,a,d]) should return no (because of wrong order),
xunique([a,a,a,a,a,b,b,b,b,b,c,c,c,c,b,a], L) should return L = [a,b,c],
xunique([], L) should return L = [].
*/

xunique([],[]). % base case
xunique([Head|Tail], [Head|TailUnique]) :-
  delete(Tail, Head, TailNew), % delete all Head from Tail, return TailNew
  xunique(TailNew, TailUnique).

% case when head is the target looking to delete
delete([Head|Tail],Head,TailNew) :-
  delete(Tail,Head,TailNew).

% case base
delete([],_,[]).

% case when head is not the same as targt looking to delete, keep in TailNew
delete([Head|Tail],E,[Head|TailNew]) :-
  Head \== E,
  delete(Tail,E,TailNew).





/*
xunion/3

xunion(L1, L2, L) where L1 and L2 are given lists of atoms, and L contains the
unique elements that are contained in both L1 and L2. L should contain the unique
elements of L1 (in the same order as in L1) followed by the unique elements of L2
that are not contained in L1 (in the same order as in L2). There should be no
redundancy in L. The predicate should work both if L is a variable and if L is a given list.

Examples:
xunion([a,c,a,d], [b,a,c], L) should return L = [a,c,d,b],
xunion([a,c,d], [b,a,c], [a,c,d,b]) should return yes,
xunion([a,c,d], [b,a,c], [a,c,d,b,a]) should return no.
*/

xunion(One, Two, Result) :-
  append(One, Two, AppendResult),
  xunique(AppendResult, Result).




/*
removeLast/3

removeLast(L, L1, Last) where L is a given nonempty list, L1 is the result of removing
the last element from L, and Last is that last element. L1 and Last can be either variables, or given values.

Examples:
removeLast([a,c,a,d], L1, Last) should return L1 = [a,c,a], Last = d,
removeLast([a,c,a,d], L1, d) should return L1 = [a,c,a],
removeLast([a,c,a,d], L1, [d]) should return no (why?),
removeLast([a], L1, Last) should return L1 = [], Last = a,
removeLast([[a,b,c]], L1, Last) should return L1 = [], Last = [a,b,c].
*/

% list has one element, result is empty
removeLast([Last], [], Last).

% list of two or more
removeLast([H|T], [H|WithoutLast], L) :-
  removeLast(T, WithoutLast, L).




/*
allConnected/L

determines if each node in L is connected to each other node in L.
*/

% checks if param one is a subset of param two
subset([], _).
subset([X|Xs], Set) :-
  append(_, [X|Set1], Set),
  subset(Xs, Set1).

clique(L) :-
  findall(X, node(X),Nodes), % nodes is a list of all the nodes
  subset(L, Nodes), % L must be a subset of all the nodes to be valid
  allConnected(L). % each in L must be connected to all others in L

allConnected([]).
allConnected([A|L]) :-
  connect(A, L), % A connected to all others in L
  allConnected(L).

% check if A is connected for every node in L
connect(_, []).
connect(A, [H|L]) :-
  (edge(A, H); edge(H, A)), % edge from either side
  connect(A, L). % and recure




/*
maxclique/2

compute all the maximal cliques of size N
*/

% add to result if is a clique
allCliques([], _).
allCliques([First|Rest], [First|Result]) :- % is a clique
  clique(First),
  allCliques(Rest, Result).
allCliques([_|Rest], Result) :- % is not a clique
  allCliques(Rest, Result).

% add to result if has no supersets
allMaximalCliques([], _, []).
allMaximalCliques([First|Rest], All, [First|Result]) :- % add, it's a maximal clique
  noSuperSets(First, All),
  allMaximalCliques(Rest, All, Result).
allMaximalCliques([_|Rest], All, Result) :- % don't add, not a maximal clique
  allMaximalCliques(Rest, All, Result).

% check E against all in list param two and make sure it has no supersets
noSuperSets(_, []).
noSuperSets(E, [First|Rest]) :-
  (E == First ; notSubset(E, First)),
  noSuperSets(E, Rest).

% check that X not container in list param two
notMember(_, []).
notMember(X, [Y|Rest]) :-
  X \== Y,
  notMember(X, Rest).

% check that param one is not a subset of param two
notSubset([], _) :- false. % empty is a subset of anything
notSubset([First|_], S) :-
  notMember(First, S).
notSubset([_|Rest], S) :-
  notSubset(Rest, S).

% filter list of lists by having length exactly N
ofLengthN([], _, []).
ofLengthN([First|Rest], N, [First|Result]) :- % add to list
  length(First, N),
  ofLengthN(Rest, N, Result).
ofLengthN([_|Rest], N, Result) :- % don't add to list
  ofLengthN(Rest, N, Result).

% will generate all the subsets of a list
allSubsets([], []).
allSubsets([E|Tail], [E|NTail]):-
  allSubsets(Tail, NTail).
allSubsets([_|Tail], NTail):-
  allSubsets(Tail, NTail).

maxclique(N, Cliques) :-
  findall(X, node(X), Nodes), % all the nodes
  findall(Y, allSubsets(Nodes, Y), AllCandidates), % # find all perms of the nodes
  allCliques(AllCandidates, AllCliques), % # find all cliques
  allMaximalCliques(AllCliques, AllCliques, AllMaximalCliques), % all cliques without subset cliques
  ofLengthN(AllMaximalCliques, N, Cliques). % find all lists of length N
