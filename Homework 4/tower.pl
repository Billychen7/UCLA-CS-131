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

%think about optimizing this later - in terms of rows vs columns
test(N,T) :-
    N >= 0,
    length(T,N),
    maplist(basic_grid_restrictions(N),T), %impose basic restrictions upon the rows
    transpose(T,T_transpose),
    maplist(basic_grid_restrictions(N),T_transpose). %impose basic restrictions upon the columns


tower(N,T,C) :-
    N >= 0, %N is a nonnegative integer
    length(T,N), % T must contain N lists
    maplist(basic_grid_restrictions(N),T), %impose basic restrictions upon the rows
    transpose(T,T_transpose),
    maplist(basic_grid_restrictions(N),T_transpose), %impose basic restrictions upon the columns

    C = counts(Top,Bottom,Left,Right),
    Top = 0, Bottom = 0, Left = 0, Right = 0.


/* MATRIX TRANSFORMATION IMPLEMENTATION */

% true if the 2nd parameter contains the heads of all of the lists that the 1st parameter contains
get_all_list_heads([],[]).
get_all_list_heads([[FirstListHead|_]|OtherLists], [FirstHead|OtherHeads]) :-
    FirstListHead = FirstHead,
    get_all_list_heads(OtherLists,OtherHeads).

% true if the 2nd parameter contains the tails of all of the lists that the 1st parameter contains
get_all_list_tails([],[]).
get_all_list_tails([[_|FirstListTail]|OtherLists],[FirstTail|OtherTails]) :-
    FirstListTail = FirstTail,
    get_all_list_tails(OtherLists,OtherTails).

emptyList([]).

% this case is necessary because the last recursive call for transpose
% will look something like the following: transpose([[],[]],[])
transpose(X,[]) :-
    maplist(emptyList,X),!. % included a cut because it kept asking for more results

transpose(X,[Y_head|Y_tail]) :-
    get_all_list_heads(X,X_heads),
    X_heads = Y_head,
    get_all_list_tails(X,X_tails),
    transpose(X_tails,Y_tail).

/* END OF MATRIX TRANSFORMATION IMPLEMENTATION */
