-module(gol).
-compile([export_all]).


% X - x world length
% Y - y world length
main(X, Y) ->
    World = [{XX, YY, random_cell()} || XX <- lists:seq(1, X), YY <- lists:seq(1, Y)],
    PIDs = start_processes(World),
    loop(World, X, Y, PIDs, 0).

% random world cell
random_cell() ->
    case rand:uniform(2) of
        1 -> dead;
        2 -> alive
    end.

% symulation loop
loop(World, World_X, World_Y, PIDs, Generation) ->
    true.

% start processes
start_processes(World) ->
    true.

