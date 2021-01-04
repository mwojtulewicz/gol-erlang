-module(gol).
-compile([export_all]).


% X - x world length
% Y - y world length
% one World's cell is a tuple of form {X, Y, State}, where X, Y are cells coordinates and State is its state (either dead or alive)
main(X, Y) ->
    World = [{XX, YY, random_state()} || XX <- lists:seq(1, X), YY <- lists:seq(1, Y)],
    PIDs = start_processes(World),
    loop(World, X, Y, PIDs, 0).

% random world cell
random_state() ->
    case rand:uniform(2) of
        1 -> dead;
        2 -> alive
    end.

% symulation loop
loop(World, World_X, World_Y, PIDs, Generation) ->
    print({clear}),
    Offset = print({header, Generation, World_X, World_Y}),
    print_world(World, World_Y, Offset),
    print({foot}),

    % loop function, interactive interface
    Input_loop = fun(Input_loop_fun) ->
        Input = io:get_line(">> "),
        case string:trim(Input) of
            "help" ->
                print({help}),
                Input_loop_fun(Input_loop_fun);
            "stop" ->
                stop_processes(World, PIDs),
                exit;
            "restart" ->
                New_World = [{XX, YY, random_state()} || XX <- lists:seq(1, World_X), YY <- lists:seq(1, World_Y)],
                loop(New_World, World_X, World_Y, PIDs, 0);
            [] ->
                New_World = next_generation(World, PIDs),
                loop(New_World, World_X, World_Y, PIDs, Generation+1);
            _ ->
                io:format("Unknown command\n"),
                Input_loop_fun(Input_loop_fun)
            end
        end,
    Input_loop(Input_loop).

% redistributing calculations and receiving results
next_generation(World, PIDs) ->
    send_jobs(World, PIDs),
    receive_cell_jobs(length(World), []).


% process management
% new cells' states are calculated in separated processes - one for each cell

% start processes
% sending main thread PID as an argument
start_processes(World) ->
    lists:foldl(
        fun(_, Workers) ->
            [spawn(?MODULE, cell_job, [self()]) | Workers]
        end, [], World).

% terminate processes by sending them {stop} message
stop_processes(World, PIDs) -> 
    Zipped = lists:zip(World, PIDs),
    lists:map(fun({_Elem, PID}) -> PID!{stop} end, Zipped).

% sending cells to processes 
send_jobs(World, PIDs) ->
    Zipped = lists:zip(World, PIDs),
    lists:map(fun({Elem, PID}) -> PID!{Elem, World} end, Zipped).

% collecting calculated cell states
% waiting for that many messages as there are cells in the World
receive_cell_jobs(Max_length, New_World) ->
    receive
        Field ->
            case length(New_World) + 1 of
                Max_length -> [Field | New_World];
                _ -> receive_cell_jobs(Max_length, [Field | New_World])
            end
    end.

% job that is calculating cells neighbourhood and new state
% those are the most calculationally demanding operations - thus we can compute them in parallel to save time
cell_job(PID) ->
    receive
        {{X, Y, State}, World} ->
            Nhood = nhood(X, Y, World),
            PID ! {X, Y, rules(State, Nhood)},
            cell_job(PID);
        stop -> true
    end.


% rules and logic
% 1. Any live cell with two or three live neighbours survives.
% 2. Any dead cell with three live neighbours becomes a live cell.
% 3. All other live cells die in the next generation. Similarly, all other dead cells stay dead.

% finding 8 neighbours for a cell
nhood(Xin, Yin, World) ->
    Indices = lists:delete({Xin,Yin}, [{Xin+I, Yin+J} || I <- [-1,0,1], J <- [-1,0,1]]),
    lists:filter(
        fun({X,Y,_State}) ->
            lists:any(fun(Cell) -> {X,Y} == Cell end, Indices)
        end,
        World).

% updating cell state based on rules given the cell's neighbourhood
rules(State, Nhood) ->
    Alive_neigh = length(lists:filter(fun({_,_,S}) -> S == alive end, Nhood)),
    case State of
        dead -> 
            if
                Alive_neigh == 3 -> 
                    alive;
                true -> 
                    dead
            end;
        alive ->
            if
                (Alive_neigh == 2) or (Alive_neigh == 3) -> 
                    alive;
                true -> 
                    dead
            end
    end.


% printing and formatting

print_world([Cell], World_Y, Offset) ->
    print({Cell, Offset}),
    io:format("\n"),
    io:format("\033[~p;~pH", [World_Y+Offset+2, 0]);

print_world([Cell | T], World_Y, Offset) ->
    print({Cell, Offset}),
    print_world(T, World_Y, Offset).

print({{X,Y,State}, Offset}) ->
    io:format("\033[~p;~pH", [Y+Offset, X*2]),
    case State of
        alive -> io:format("X");
        dead -> io:format("-")
    end;

print({clear}) ->
    io:format("\033[2J", []);

print({header, Generation, X, Y}) ->
    io:format(
"\033[~p;~pHConway\'s Game of Life

World size: (~p, ~p)
X -> alive cell
- -> dead cell

Rules:
    1. Any live cell with two or three live neighbours survives.
    2. Any dead cell with three live neighbours becomes a live cell.
    3. All other live cells die in the next generation. Similarly, all other dead cells stay dead.

Generation: ~p", [0, 0, X, Y, Generation]), 13;

print({foot}) ->
    io:format(
"hit Enter to see next Generation
type 'restart' to start a new symulation
type 'help' for help, 'stop' to exit\n");

print({help}) ->
    io:format(
"\\n - new generation (hit Enter)
restart - start a new symulation
stop - terminate
help - show this message\n", []).