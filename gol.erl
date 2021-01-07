% Gabriela Matuszewska, Mateusz Wojtulewicz
% Game Of Life w Erlangu
% pwir 2020/21

-module(gol).
-compile([export_all]).

% ----- OPIS -----
% swiat w symulacji ma dwie wielkosci: szerokosc (X), dlugosc (Y)
% swiat sklada sie z listy X*Y komorek
% komorka to krotka postaci {X, Y, State}, gdzie X,Y to jej wspolrzedne a State to jej aktualny stan
% sa dwa mozliwe stany komorki: zywy (alive), martwy (dead)
% nowa generacja swiata zalezy od poprzedniej - ewolucja przebiega wedlug okreslonych zasad
% nowy stan komorki zalezy od jej stanu i stanu jej sasiedztwa
% zrownoleglenie obliczen wystepuje przy okreslaniu nowej generacji swiata
% procesow rownoleglych jest tyle ile komorek - 1 proces jest odpowiedzialny za obliczenie nastepnego stanu 1 komorki w symulacji


% 2 mozliwe funkcje glowne

% generujaca losowy swiat o podanej wielkosci
main(X, Y) ->
    World = [{XX, YY, random_state()} || XX <- lists:seq(1, X), YY <- lists:seq(1, Y)],
    PIDs = start_processes(World),
    loop(World, X, Y, PIDs, 0).

% swiat generowany na podstawie odpowiednio przygotowanego pliku
main(Filename) ->
    case file:open(Filename, [read]) of
        {ok, File} -> 
            {X, Y, World} = parse_file(File),
            file:close(File),
            PIDs = start_processes(World),
            loop(World, X, Y, PIDs, 0);
        {error, _} -> io:format("file error")
    end.

% funkcja do losowego inicjalizowania stanu komorki z dwoch dostepnych (zywa, martwa)
random_state() ->
    case rand:uniform(2) of
        1 -> dead;
        2 -> alive
    end.

% glowna petla symulacji
% wprowadza interfejs pseudograficzny
loop(World, World_X, World_Y, PIDs, Generation) ->
    % wyswietlanie informacji o symulacji
    print({clear}),
    print({header, Generation, World_X, World_Y}),
    % wyswietlanie swiata w aktualnej generacji
    show_world(World, World_X),
    print({foot}),

    % funkcja tworzaca interfejs pseudograficzny
    % pozwala uzytkownikowi na wprowadzenie komendy, ktora przetwarza
    Input_loop = fun(Input_loop_fun) ->
        Input = io:get_line(">> "),
        case string:trim(Input) of
            "help" ->
                print({help}),
                Input_loop_fun(Input_loop_fun);
            "stop" ->
                stop_processes(PIDs),
                exit;
            "restart" ->
                main(World_X, World_Y);
            "load" ->
                Filename = io:get_line(">> File: "),
                main(string:trim(Filename));
            [] ->
                % wcisniecie Enter
                % obliczenie nowej generacji symulacji
                New_World = next_generation(World, PIDs),
                % petla
                loop(New_World, World_X, World_Y, PIDs, Generation+1);
            _ ->
                io:format("Unknown command\n"),
                Input_loop_fun(Input_loop_fun)
            end
        end,
    % wywolanie tejze fukcji -> pseudo-petla
    Input_loop(Input_loop).

% funkcja rozdzielajaca zadania na procesy i zbierajaca wyniki
next_generation(World, PIDs) ->
    send_jobs(World, PIDs),
    receive_cell_jobs(length(World), []).


% zarzadzanie procesami

% stworzenie po jednym procesie na kazda komorke
% PID procesow zwracane sa jako lista
start_processes(World) ->
    lists:foldl(
        fun(_, Workers) ->
            [spawn(?MODULE, cell_job, [self()]) | Workers]
        end, [], World).

% zatrzymywanie procesow poprzez wyslanie im odpowiedniej wiadomosci
stop_processes(PIDs) -> 
    lists:map(fun(PID) -> PID!{stop} end, PIDs).

% funkcja przesylajaca do procesow po jednej komorce oraz kopie swiata 
send_jobs(World, PIDs) ->
    Zipped = lists:zip(World, PIDs),
    lists:map(fun({Elem, PID}) -> PID!{Elem, World} end, Zipped).

% funkcja zbierajaca obliczone stany komorek
% uruchamiana w glownym procesie
% procesy obliczajace nowe stany komorek odsylaja wyniki na PID glownego procesu
% dzieki argumentowi Max_Length funckja zwraca swiat dopiero po dodaniu do niego wszystkich komorek otrzymanych w wiadomosciach
receive_cell_jobs(Max_length, New_World) ->
    receive
        Field ->
            case length(New_World) + 1 of
                Max_length -> [Field | New_World];
                _ -> receive_cell_jobs(Max_length, [Field | New_World])
            end
    end.

% funkcje zadania uruchomione w procesach, przechowuja PID glownego procesu
% otrzymuja wiadomosc zawierajaca komorke (krotka) i kopie swiata
% wykonuja najbardziej zlozone obliczeniowo operacje, czyli znajdywanie sasiedztwa 3x3 i obliczanie nowego stanu komorki
% wynik wysylaja z powrotem do glownego procesu gdzie sa zlaczane w nowy swiat
cell_job(PID) ->
    receive
        {{X, Y, State}, World} ->
            Nhood = nhood(X, Y, World),
            PID ! {X, Y, rules(State, Nhood)},
            cell_job(PID);
        stop -> true
    end.


% zasady i logika
% 1. kazda zywa komorka z 2 lub 3 zywymi sasiadami pozostaje zywa
% 2. kazda martwa komorka z dokladnie 3 zywymi sasiadami staje sie komorka zywa
% 3. wszystkie pozostale komorki nie zmieniaja swojego stanu

% fznajdywanie sasiedztwa komorki
% dzieki uzyciu funkcji lists:any dla komorek na krawedziach zostanie zwrocona mniejsza ilosc komorek 
nhood(Xin, Yin, World) ->
    Indices = lists:delete({Xin,Yin}, [{Xin+I, Yin+J} || I <- [-1,0,1], J <- [-1,0,1]]),
    lists:filter(
        fun({X,Y,_State}) ->
            lists:any(fun(Cell) -> {X,Y} == Cell end, Indices)
        end,
        World).

% aktualizowanie stanu komorki na podstawie jej siasiedztwa i wlasnego stanu
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


% wyswietlanie i wypisywanie

% funkcja do wyswietlania stanu swiata symulacji
% sortkuje swiat korzystajac z wspolrzednych komorek
% przesyla przesortowana liste do funkcji pomocniczej
show_world(World, World_X) ->
    Sort = fun({X1,Y1,_}, {X2,Y2,_}) ->
        if 
            X1 < X2 ->
                true;
            X1 == X2 ->
                if 
                    Y1 < Y2 ->
                        true;
                    Y1 > Y2 ->
                        false
                end;
            X1 > X2 ->
                false
        end
    end,
    S_World = lists:usort(Sort, World),
    print_world(S_World, World_X).

% funkcja rekurejcyjna wypisujaca komorki ulozone w posortowanej liscie
% oddziela jeden rzad i przesyla go do funkcji pomocniczej
print_world([], _) -> io:format("\n");
print_world(World, World_X) ->
    {Row, Rest} = lists:split(World_X, World),
    print_row(Row),
    print_world(Rest, World_X).

% funkcja rekurencyjna wypisujaca jeden wiersz swiata
print_row([]) -> io:format("\n");
print_row([{_,_,State} | Rest]) ->
    case State of 
        alive ->
            io:format("X ");
        dead ->
            io:format("- ")
    end,
    print_row(Rest).


% funkcje odpowiedzialne za wypisywanie potrzebnych informacji w pseudo gui

print({clear}) ->
    io:format("\n\n-------------------------------------------------------------------------\n\n");

print({header, Generation, X, Y}) ->
    io:format(
"Conway\'s Game of Life

World size: (~p, ~p)
X -> alive cell
- -> dead cell

Rules:
    1. Any live cell with two or three live neighbours survives.
    2. Any dead cell with three live neighbours becomes a live cell.
    3. All other live cells die in the next generation. Similarly, all other dead cells stay dead.

Generation: ~p \n\n", [X, Y, Generation]);

print({foot}) ->
    io:format(
"hit Enter to see the next Generation \n
type:
 'restart' to start a new symulation with random world state
 'load' to load a world state from file
 'help' for help
 'stop' to exit\n\n");

print({help}) ->
    io:format(
"\\n - next generation (hit Enter)
restart - start a new symulation 
load - start a new symulation from file (given next)
stop - terminate
help - show this message\n", []).


% odczytywanie i parsowanie pliku

% plik musi byc odpowiednio przygotowany
% pierwsze dwie linijki to odpowiednio szerokosc i dlugosc swiata
% kolejne linijki to oddzielone spacjami odpowiednia ilosc symboli, gdzie x oznacza komorke zywa
% przyklad pliku: plik glider

% glowna funkcja parsujaca
parse_file(File) ->
    X = read_size(File),
    Y = read_size(File),
    World = read_world(File, 1, []),
    {X, Y, World}.

% funkcja odpowiedzialna za wyczytanie z jednej linijki szerokosci lub dlugosci swiata, uzywana dwukrotnie
read_size(File) ->
    {ok, Line} = file:read_line(File),
    Number = string:trim(Line),
    case string:to_integer(Number) of 
        {error, _Error} ->
            throw(parsing_error);
        {Num, _Rest} ->
            Num
    end.

% funkcja rekurencyjna do parsowania przygotowanych linijek
% rozdziela jedna linijke (string) na liste obcinajac po spacjach
read_world(File, Curr_X, World) ->
    case file:read_line(File) of
        eof -> 
            World;
        {error, _} ->
            throw(reading_error);
        {ok, Line} ->
            Cells = string:tokens(Line, " "),
            Row = read_row(Cells, Curr_X, 1, []),
            read_world(File, Curr_X+1, World ++ Row)
    end.

% funkcja pomocnicza do parsowania jednej linijki
read_row([], _, _, Row) -> Row;
read_row([H | T], Curr_X, Curr_Y, Row) ->
    case string:trim(H) of 
        "x" -> read_row(T, Curr_X, Curr_Y+1, [{Curr_Y, Curr_X, alive} | Row]);
        _ -> read_row(T, Curr_X, Curr_Y+1, [{Curr_Y, Curr_X, dead} | Row])
    end.
