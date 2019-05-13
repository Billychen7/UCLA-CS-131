
basic_grid_restrictions(GridSize,Row) :-
    length(Row,GridSize), %each list in T is of length N
    fd_domain(Row,1,GridSize), %each list in T contains integers from 1 to N
    fd_all_different(Row), %each list in T contains distinct integers
    fd_labeling(Row). %find matching solutions (can backtrack to generate new solution)

% N is a nonnegative integer specifying the size of the square grid
% T, a list of N lists, represents each row of the square grid
% each row is a list of N distinct integers from 1 to N (same with columns)
% C is a structure with function symbol counts and arity 4
% C's arguments are lists of N integers, representing the
% tower counts for top, bottom, left, right respectively

test(N,T) :-
    N >= 0,
    length(T,N),
    maplist(basic_grid_restrictions(N),T).

/*
tower(N,T,C) :-
    N >= 0, %N is a nonnegative integer
    length(T,N), % T must contain N lists
    maplist(basic_grid_restrictions(N),T), % the lists of T must have N distinct integers from 1 to N

    C = counts(Top,Bottom,Left,Right).
*/
