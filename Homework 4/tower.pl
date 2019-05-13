
length_with_switched_parameters(Length,List) :-
    length(List,Length).

fd_domain_with_switched_parameters(LowerBound,UpperBound,List) :-
    fd_domain(List,LowerBound,UpperBound).

% N is a nonnegative integer specifying the size of the square grid
% T, a list of N lists, represents each row of the square grid
% each row is a list of N distinct integers from 1 to N (same with columns)
% C is a structure with function symbol counts and arity 4
% C's arguments are lists of N integers, representing the
% tower counts for top, bottom, left, right respectively


test(N,T) :-
    N >= 0,
    length(T,N),
    maplist(length_with_switched_parameters(N),T),

    maplist(fd_domain_with_switched_parameters(1,N),T),

    maplist(fd_all_different,T),

    maplist(fd_labeling,T).

%consider consolidating all of the maplists to a single rule

/*
tower(N,T,C) :-
    N >= 0, %N is a nonnegative integer
    length(T,N), % T must contain N lists
    maplist(length_with_switched_parameters(N),T), %each list in T is of length N
    maplist(fd_domain_with_switched_parameters(1,N),T), %each list in T contains integers from 1 to N
    maplist(fd_all_different,T), %each list in T contains distinct integers
    maplist(fd_labeling,T), %find matching solutions (can backtrack to generate new solution)

    C = counts(Top,Bottom,Left,Right).
*/
