-module(test).
-compile([export_all]).

proces(Main) ->
    receive
        {name} ->
            io:fwrite(" -- jestem sobie proces o id ~p, rodzic: ~p ~n", [self(), Main]),
            proces(Main)
    end.

spawn_threads(N) -> 
    spawn_threads(N, []).
spawn_threads(0, Pids) -> 
    Pids;
spawn_threads(N, Pids) ->
    New = spawn(?MODULE, proces, [self()]),
    spawn_threads(N-1, Pids ++ [New]).

print_names([]) -> 
    io:fwrite("~n");
print_names([H|T]) ->
    H ! {name},
    print_names(T).

main(N) ->
    io:fwrite("funkcja main~ngenerowanie ~p procesow...~n", [N]),
    
    Pids = spawn_threads(N),
    io:fwrite("Pids:~n~p~n", [Pids]),

    print_names(Pids),
    print_names(Pids),
    print_names(Pids),

    io:fwrite("koniec funkcji main.~n").
