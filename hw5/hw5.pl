/*******************************************/
/**    Your solution goes in this file    **/ 
/*******************************************/

/* Part 1 */

/* List intersection helper function */
list_intersection([H|_], [H|_]) :- !.
list_intersection(L1, [_|T2]) :- list_intersection(L1, T2), !.
list_intersection([_|T1], L2) :- list_intersection(T1, L2), !.
    

/* Find all courses with 3 or 4 credits */
fc_course(X) :-
	course(X,_,N),
	N >= 3,
	N =< 4.

/* find all courses with immediate pre-req of ecs 110 */
prereq_110(X) :-
	course(X, Y, _),
	member(ecs110, Y).

/* Find names of students in ecs140a */
ecs140a_students(X) :-
	student(X,Y),
	member(ecs140a, Y).

/* names of all instructors who teach johns course */
instructor_names(N) :-
	student(john, SC),
	instructor(N, IC),
	list_intersection(SC, IC).

/* Find names of all students who are in Jims class */
students(S) :-
	instructor(jim, JC),
	student(S, SC),
	list_intersection(JC, SC).

allprereq([],[]) :- !.
allprereq([H | T], Output) :-
	course(H, HP, _),
	allprereq(HP, REST),
	append(REST, HP, HEAD),
	allprereq(T, TAIL),
	append(TAIL, HEAD, Output), !. 
 allprereq([H | T], Output) :-
    not(course(H, HP, _)),
    allprereq(HP, REST),
    append(REST, HP, HEAD),!. 
allprereq(Course, Output) :- allprereq([Course], Output).

/* Part 2 */

/* predicate all_length returns num atoms at all levels */

all_length([], 0).

all_length([H|T], Count) :-
    atomic(H),
    all_length(T, C1),
    Count is C1 + 1.
    
all_length([H|T], Count) :-
    all_length(H, C1),
    all_length(T, C2),
    Count is C1 + C2,
    !.
    
equal_a_b(L) :- 
    count_atoms(L, a, Ca),
    count_atoms(L, b, Cb),
    Ca = Cb.
   
count_atoms([], _, 0) :- !.
 
count_atoms([H|T], Atom, Count ) :-
    count_atoms(H, Atom, Cta),
    count_atoms(T, Atom, Ctb),
    Count is Cta + Ctb,
    !.
    
count_atoms(Atom, Atom, 1) :- !.

count_atoms(_, _, 0).

/* Swap prefix and suffix at pt */
swap_prefix_suffix(K, L, S, P) :-
    append(K, Suf, L),
    append(K, P, KandP),
    append(Suf, KandP, S).

swap_prefix_suffix(K, [H|T], S, Pac) :-
    append(Pac, [H], P),
    swap_prefix_suffix(K, T, S, P).
    
swap_prefix_suffix(K,L,S) :- swap_prefix_suffix(K,L,S,[]).


/* Is string a palindrome or not aka mirror image */
palin(A) :- reverse(A, A).

reverse(L1, L2) :- reverse(L1, [], L2). 

reverse([], L, L).

reverse([H|T], L2, L3) :- reverse(L, [H,L2],L3). 


/* Good function is either single number 0 or a 1 followed by two 0s */
good([0]).
good([1|A]) :-
    append(X, Y, A),
    good(X),
    good(Y).
    
    
/* Part 3 */


/* final move or intermediate move */
go(Curr, Next, Path) :-
    (safe(Curr),
    Edge = arc(_, Curr, Temp),
    mynot(member(Edge, Path)),
    Edge,
    go(Temp, Next, [Edge|Path])); /* final move */
    (safe(Curr),
    FinalEdge = arc(_, Curr, Next),
    FinalEdge,
    mynot(member(FinalEdge, Path)),
    print_moves([FinalEdge|Path]),
    !).

/* Track state of Farmer, Wolf, Goat, & Cabbage */
state(_, _, _, _).

opposite(right, left).

opposite(left, right).

/* take nothing accross */
arc(take(none, S, E), state(S, Wolf, Goat, Cabbage), state(E, Wolf, Goat, Cabbage)) :-
    take(none, S, E),
    safe(state(E, Wolf, Goat, Cabbage)).

/* take wolf accross */
arc(take(wolf, S, E), state(S, S, Goat, Cabbage), state(E, E, Goat, Cabbage)) :-
    take(wolf, S, E),
    safe(state(E, E, Goat, Cabbage)).

/* take goat accross */
arc(take(goat, S, E), state(S, Wolf, S, Cabbage), state(E, Wolf, E, Cabbage)) :-
    take(goat, S, E),
    safe(state(E, Wolf, E, Cabbage)).

/* take cabbage accross */
arc(take(cabbage, S, E), state(S, Wolf, Goat, S), state(E, Wolf, Goat, E)) :-
    take(cabbage, S, E),
    safe(state(E, Wolf, Goat, E)).

print_moves([]).

print_moves([arc(take(X, A, B),_, _)|T]) :-
    print_moves(T),
    print(take(X, A, B)),
    nl.

mynot(A) :- A, !, fail.

mynot(_).

unsafe(A) :- mynot(safe(A)).


/* wolf will not eat cabbage and vice versa, so we can leave them together */

safe(state(_, Wolf, Goat, Cabbage)) :- 
    opposite(Goat,Cabbage),
    opposite(Wolf,Goat).

safe(state(None, _, G, _)) :- 
    None = Goat, 
    !.

take(X, A, B) :- opposite(A, B).
    
/* solve puzzle */
solve :- 
    go(state(left, left, left, left), 
        state(right, right, right, right), []), !.