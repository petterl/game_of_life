%% Author: Petter Sandholdt
%% Created: 18 maj 2011
%% Description: Game Of Life
-module(gol).

-define(ALIVE, $X).
-define(DEAD, $ ).

-define(BOARD, ["     ",
			    "  XX ",
			    " XX  ",
			    "  X  ",
			    "     "]).
%% -define(BOARD, [" X      ",
%% 			    "  X     ",
%% 			    "XXX     ",
%% 			    "        ",
%% 			    "        ", 
%% 				"        "]).
%% -define(BOARD, ["      ",
%% 				"      ",
%% 				" XXX  ",
%% 				"      ",
%% 				"      "]).
-define(MAX_TICKS, 20).

%%
%% Exported Functions
%%
-export([start/0, stop/0]).

%% Start here
start() ->	
	MaxX = length(hd(?BOARD))-1,
	MaxY = length(?BOARD)-1,
	init_display(?BOARD),
	init_board(MaxX, MaxY, ?BOARD, 0, 0).

%% Stop system by calling display proc to stop
stop() ->
	Pid = whereis('gol_display'),
	if (Pid =/= undefined) ->
		Pid ! harakiri;
	   true -> ok
	end,
	ok.

init_board(_, _, [], _, _) -> ok;
init_board(MaxX, MaxY, [[] | Board], _, Y) ->
	init_board(MaxX, MaxY, Board, 0, Y+1);
init_board(MaxX, MaxY, [[State|Cols] | Board], X, Y) ->
	Pid = spawn(fun() -> new_cell(State, X, Y, MaxX, MaxY) end),
	register(cell_name(X,Y), Pid),
	%%io:format("Registering cell ~p as ~p ~n", [cell_name(X,Y), Pid]),
	init_board(MaxX, MaxY, [Cols|Board], X+1, Y).

new_cell(State, X, Y, W, H) ->
	%%io:format("Cell ~p:~p[~p] : ~p ~n", [X, Y, [State],neighbours(X,Y, W, H)]),
	'gol_display' ! {new_cell, cell_name(X, Y)},
	cell_loop(State, X, Y, neighbours(X,Y, W, H), 0, 0, 0).

cell_loop(_State, X, Y, _Neighbors, ?MAX_TICKS, _Alive, _Dead) -> 
	unregister(cell_name(X,Y));
cell_loop(State, X, Y, Neighbors, Tick, Alive, Dead) ->
	%%io:format("Cell ~p:~p tick ~p~n", [X,Y,Tick]),
	
	if ((Alive+Dead) == 0) ->
		   'gol_display' ! {cell_state, X, Y, State, Tick},
		   send_cell_state(cell_name(X,Y), State, Neighbors, Tick);
	   true -> ok
	end,
	
	if (Alive+Dead == length(Neighbors)) ->
		   %% Calulate new state
		   NewState = calc_state(State, Alive, Dead),
		   %%io:format("~p ~p ~p=>~p~n", [Tick, cell_name(X,Y), [State], [NewState]]),
		   cell_loop(NewState, X, Y, Neighbors, Tick+1, 0, 0);
	   true -> ok
	end,

	%% wait for all neightbors state
	receive
		{cell_state, _Cell, ?ALIVE, Tick} ->
			cell_loop(State, X, Y, Neighbors, Tick, Alive+1, Dead);
		{cell_state, _Cell, ?DEAD, Tick} ->
			cell_loop(State, X, Y, Neighbors, Tick, Alive, Dead+1);
		harakiri -> 
			io:format("Cell ~p went harakiri~n", [cell_name(X,Y)])
	end.		

%%
%% Display functionality
%%
init_display(Board) ->
	Cells = length(hd(Board)) * length(Board),
	%%display_board(Board, 0),
	Pid = spawn(fun() -> display_loop(Board, [], Cells, 0) end),
	register('gol_display', Pid).

display_loop(_Board, _Cells, _CellsReceived, ?MAX_TICKS) ->
	unregister('gol_display');
display_loop(Board, Cells, 0, Tick) ->
	display_board(Board, Tick),
	display_loop(Board, Cells, length(Cells), Tick+1);
display_loop(Board, Cells, CellsLeft, Tick) ->
	receive
		{new_cell, CellName} ->
			display_loop(Board, [CellName |Cells], CellsLeft, Tick);
		{cell_state, X, Y, State, Tick} ->
			NewBoard = update_board(Board, X, Y, State),
			display_loop(NewBoard, Cells, CellsLeft-1, Tick);
		harakiri -> 
			io:format("Display made harakiri~n"),
			[Cell ! harakiri || Cell <- Cells],
			ok
	end.

display_board(Board, Tick) ->
	Out = lists:flatten(["|"++X++"|~n" || X <- Board]),
	io:format("  Tick ~p~n"++Out++"~n", [Tick]).

update_board([Row|Board], X, 0, State) ->
	[update_row(Row, X, State)|Board];
update_board([Row|Board], X, Y, State) ->
	[Row|update_board(Board, X, Y-1, State)].
	
update_row([_C|R], 0, State) ->
	[State|R];
update_row([C|R], X, State) ->
	[C | update_row(R, X-1, State)].
	
%%
%% Local Functions
%%
calc_state(?ALIVE, Alive, _Dead) when Alive < 2 -> ?DEAD; 
calc_state(?ALIVE, Alive, _Dead) when Alive > 3 -> ?DEAD; 
calc_state(?DEAD, Alive, _Dead) when Alive == 3 -> ?ALIVE; 
calc_state(State, _Alive, _Dead) -> State.

send_cell_state(_Name, _State, [], _Tick) -> ok;
send_cell_state(Name, State, [Cell|Rest], Tick) ->
	%%io:format("Cell ~p: send state to ~p~n", [Name, Cell]),
	Cell ! {cell_state, Name, State, Tick},
	send_cell_state(Name, State, Rest, Tick).
	
neighbours(X=0,Y=0,_W,_H) -> %% TL
	[cell_name(X+1, Y), cell_name(X, Y+1), cell_name(X+1, Y+1)];
neighbours(X,Y=0,X,_H) ->    %% TH
	[cell_name(X-1, Y), cell_name(X-1, Y+1), cell_name(X, Y+1)];
neighbours(X,Y=0,_W,_H) ->   %% TM
	[cell_name(X-1, Y), cell_name(X+1, Y),
	 cell_name(X-1, Y+1), cell_name(X, Y+1), cell_name(X+1, Y+1)];
neighbours(X=0,Y,_W,Y) ->    %% BL
	[cell_name(X, Y-1), cell_name(X+1, Y-1), cell_name(X+1, Y)];
neighbours(X=0,Y,_W,_H) ->   %% ML
	[cell_name(X, Y-1), cell_name(X+1, Y-1),
	 cell_name(X+1, Y),     
	 cell_name(X, Y+1), cell_name(X+1, Y+1)];
neighbours(X,Y,X,Y) ->       %% BR
	[cell_name(X-1, Y-1), cell_name(X, Y-1), cell_name(X-1, Y)];
neighbours(X,Y,X,_H) ->      %% MR
	[cell_name(X-1, Y-1), cell_name(X, Y-1),
	 cell_name(X-1, Y), 
	 cell_name(X-1, Y+1), cell_name(X, Y+1)];
neighbours(X,Y,_W,Y) ->      %% BM
	[cell_name(X-1, Y-1), cell_name(X, Y-1), cell_name(X+1, Y-1),
	 cell_name(X-1, Y), cell_name(X+1, Y)];
neighbours(X,Y,_W,_H) ->     %% MM
	[cell_name(X-1, Y-1), cell_name(X, Y-1), cell_name(X+1, Y-1),
	 cell_name(X-1, Y), cell_name(X+1, Y),
	 cell_name(X-1, Y+1), cell_name(X, Y+1), cell_name(X+1, Y+1)].

cell_name(X,Y) ->
	list_to_atom("gol_"++integer_to_list(X)++"_"++integer_to_list(Y)).

