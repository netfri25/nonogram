:- module(server, [run_server/0]).

:- use_module(solver, [nonogram/3]).
:- use_module(library(clpfd)).
:- use_module(library(socket)).
:- use_module(library(readutil)).

run_server :-
   writeln('INFO: starting server'),
   create_server(6969),
   halt.

create_server(Port) :-
   tcp_socket(Socket),
   tcp_setopt(Socket, reuseaddr),
   tcp_bind(Socket, Port),
   tcp_listen(Socket, 5),
   tcp_open_socket(Socket, AcceptFd, _),
   writeln('INFO: server started successfully'),
   dispatch(AcceptFd).

dispatch(AcceptFd) :-
   tcp_accept(AcceptFd, Socket, _),
   writeln('INFO: connected'),
   process_client(Socket),
   writeln('INFO: disconnected'),
   dispatch(AcceptFd).

process_client(Socket) :-
   setup_call_cleanup(
      tcp_open_socket(Socket, StreamPair),
      (handle_service(StreamPair) ; writeln('Unexpected error occurred')),
      (flush_output(StreamPair), close(StreamPair))
   ).

handle_service(StreamPair) :-
   Opts = [syntax_errors(quiet)],

   read_line_to_string(StreamPair, RowsText),
   term_string(RowsGroups, RowsText, Opts),
   write('Rows: '), writeln(RowsGroups),

   read_line_to_string(StreamPair, ColsText),
   term_string(ColsGroups, ColsText, Opts),
   write('Cols: '), writeln(ColsGroups),

   writeln('Solution:'),
   bench(nonogram(RowsGroups, ColsGroups, Grid), Tms),
   maplist(label, Grid),
   maplist(writeln, Grid),
   append(Grid, Flat),
   maplist(pretty_cell, Flat, PrettyFlat),
   atomic_list_concat(PrettyFlat, Output),
   writeln(StreamPair, Output),
   flush_output(StreamPair),
   write('Written: '), writeln(Output),
   format('took ~4fms', Tms), nl.

pretty_cell(0, 'x') :- !.
pretty_cell(1, '@') :- !.

bench(G,Tms) :-
   T0 is cputime,
   G,
   T1 is cputime,
   T is T1 - T0,
   Tms is T * 1000.
