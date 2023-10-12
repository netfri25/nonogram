:- module(dot, [dump_dot/3]).

:- use_module(solver, [line_arcs/5]).

dump_dot(RowsGroups, ColsGroups, Name) :-
   atom_concat(Name, "-rows.dot", FilenameRows),
   setup_call_cleanup(
      open(FilenameRows, write, OutStreamRows),
      write_groups(OutStreamRows, y, x, rows, "LR", RowsGroups),
      close(OutStreamRows)
   ),

   atom_concat(Name, "-cols.dot", FilenameCols),
   setup_call_cleanup(
      open(FilenameCols, write, OutStreamCols),
      write_groups(OutStreamCols, x, y, cols, "TB", ColsGroups),
      close(OutStreamCols)
   ), !.

write_groups(_, _, _, _, _, []).
write_groups(OutStream, LineAtom, CounterAtom, Name, RankDir, Gss) :-
   write(OutStream, "digraph "),
   write(OutStream, Name),
   writeln(OutStream, " {"),
   write(OutStream, "    rankdir="),
   writeln(OutStream, RankDir),
   write_groups_rec(OutStream, LineAtom, CounterAtom, Gss),
   writeln(OutStream, "}").

write_groups_rec(_, _, _, []).
write_groups_rec(OutStream, LineAtom, CounterAtom, [Gs|Gss]) :-
   gensym(LineAtom, NewLineAtom),
   write_line(OutStream, NewLineAtom, CounterAtom, Gs),
   write_groups_rec(OutStream, LineAtom, CounterAtom, Gss).

write_line(OutStream, LineAtom, CounterAtom, Gs) :-
   line_arcs(Gs, LineAtom, CounterAtom, Arcs),
   atom_concat(LineAtom, CounterAtom, FullAtom),
   write_node(OutStream, arc(FullAtom, 0, FullAtom)),
   maplist(write_node(OutStream), Arcs).

line_arcs(Gs, LineAtom, CounterAtom, Arcs) :-
   atom_concat(LineAtom, CounterAtom, FullAtom),
   line_arcs(Gs, Arcs, FullAtom, FullAtom, _).

write_node(Out, arc(From, Value, To)) :-
   write(Out, "    "),
   write(Out, From),
   write(Out, " -> "),
   write(Out, To),
   write(Out, " [label=\""),
   write(Out, Value),
   write(Out, "\"]"),
   nl(Out).
