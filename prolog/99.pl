
% problem 1
% find last element of a list

my_last(X, [X]).
my_last(X, [_|L]) :-
    my_last(X, L).


% problem 2
% find the second to last element of a list

my_last_but1(X, [X|[_]]).
my_last_but1(X, [_|L]) :-
    my_last_but1(X, L).

% problem 3
% find the Kth element of a list (starting at 1)

my_nth_elem(X, [X|_], 1).
my_nth_elem(X, [_|L], N) :-
    N > 0,
    M is N - 1,
    my_nth_elem(X, L, M).

% problem 4
% find list length

my_length(0, []).
my_length(N, [_|Xs]) :-
    my_length(Nr, Xs),
    N is Nr + 1.

% problem 5
% Reverse a list

my_reverse([], []).
my_reverse([X], [X]).
my_reverse(R, [X|Xs]) :-
    my_reverse(Rs, Xs),
    append(Rs, [X], R).

% 6
% find out if a list is a palindrome

is_palindrome([]).
is_palindrome([X]).
is_palindrome(L) :-
    my_reverse(L, L).

% 7
% Flatten a nested list structure
my_flatten([], []).
my_flatten([X], [X]) :-
    not(is_list(X).
my_flatten([X|Xs], Flat) :-
    ((not(is_list(X)), Xf = X) ; my_flatten(X, Xf)),
    my_flatten(Xs, Xsf),
    append(Xf, Xsf, Flat).

    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 7 isn't finished.  It doesn't work.
    
    
