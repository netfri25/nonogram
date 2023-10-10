:- module(solver, [nonogram/3, bench/2]).

:- use_module(library(clpfd)).

% 0 - empty cell
% 1 - filled cell
nonogram(RowsGroups, ColsGroups, Grid) :-
   Rows = Grid,
   same_length(Rows, RowsGroups),
   same_length(Cols, ColsGroups),
   maplist(same_length(ColsGroups), Rows),
   transpose(Rows, Cols),
   maplist(count_groups, RowsGroups, Rows),
   maplist(count_groups, ColsGroups, Cols).

count_groups(Gs, Line) :-
   sum(Gs, #=, LineSum),
   sum(Line, #=, LineSum),
   arcs(Gs, Arcs, start, Final),
   append(Line, [0], LineZ), % needed because the Line will always end with 0
   automaton(LineZ, [source(start), sink(Final)], [arc(start, 0, start) | Arcs]).

% if the group is 0 (no more filled cells in that group)
% arc(From, 0, From)   allows to repeatedly mark as empty 0 or more times
% arc(From, 0, To)     marks once an empty cell, because when the group reaches a count
%                      of 0 it means that it's in the end of the group so there can't be
%                      any more consecutive filled cells
% tldr: like 0+ in RegEx, one or more rempty cells
% if the group isn't 0, then add another filled cell
arcs([], [], Final, Final). % when there are no groups left, the last node is the final node
arcs([G|Gs], Arcs, From, Final) :-
   gensym(From, To),
   ( G == 0 ->
      Arcs = [arc(From, 0, From), arc(From, 0, To) | Rest],
      arcs(Gs, Rest, To, Final)
   ;
      Arcs = [arc(From, 1, To) | Rest],
      G1 is G-1,
      arcs([G1 | Gs], Rest, To, Final)
   ).
