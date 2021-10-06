-module(serv). 
-compile(export_all).

start_server() ->
    {ok, Listen} = gen_tcp:listen(1232, [binary, {packet, 4}, {active, true}]), %Port#, packets size 4 byte
    spawn(fun() -> connect(self(), Listen) end).
    
connect (Pid, Listen) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    Cid = spawn(fun()-> connect(Pid, Listen) end), %Pid of spawned process
    State = [], %array where all sockets are stored
    loop(Cid, Pid, State, Socket).
    
loop (Cid, Pid, State, Socket) ->
    receive
        {add, Name, Nsocket} -> %message for apdating State by adding new socket
            Find = proplists:get_value(Name, State, 0),
            case Find of
                 0 -> %If State doesn't have that key already
                    Newstate = [{Name, Nsocket} | State],
                    Pid ! {add, Name, Nsocket}, %sending new socket to parent process
                    Cid ! {add, Name, Nsocket}; %sending new socket to child process
                 _ -> 
                     Newstate = State
            end,
            loop(Cid, Pid, Newstate, Socket); 
        {tcp, Socket, Msg} ->
            Msgtrs = binary_to_term(Msg), %traslation message to term 
            case Msgtrs of                
                {lego, Txt} -> %receiving message from Lego robot
                    case Txt of
                        "1" -> % 1 - connection with client is done, upd State
                            Newstate = [{lego, Socket} | State],
                            Pid ! {add, lego, Socket},
                            Cid ! {add, lego, Socket},
                            io:format("Lego has picked the item ~n"),
                            Ardsocket = proplists:get_value(arduino, State, "0"), %extraction Arduino socket from State
                            gen_tcp:send(Ardsocket, term_to_binary("1")), % sending data request to Arduino 
                            loop(Cid, Pid, Newstate, Socket);
                        "11" -> %Lego sends info that it has received drop-off point location
                            io:format("Lego has received drop-off coordinates ~n"),
                            loop(Cid, Pid, State, Socket)
                    end;
                {Name, Txt} ->
                    case Txt of
                        "1" -> % 1 - connection with client is done, upd State
                            Newstate = [{Name, Socket} | State],
                            Pid ! {add, Name, Socket},
                            Cid ! {add, Name, Socket},
                            loop(Cid, Pid, Newstate, Socket);
                          _  ->  % data from Arduino client is received, start function for dropoff point calculation
                            spawn(fun()-> dropoffpoint (Txt, State) end),
                            loop(Cid, Pid, State, Socket)
                    end
            end
    end.
    
          
dropoffpoint (Data, State) -> 
    %connection and send req to DB using Data
    %here dummy values are used
    %Dropcoord - dropoff location coordinates, will be send to Lego
    %Locationid - dropoff location id in DB, will be send to GUI
    
     
    Dropcoord = "qwrty",
    Locationid = "14",
    Legosocket = proplists:get_value(lego, State, "0"), %extraction Lego socket from State
    Guisocket = proplists:get_value(gui, State, "0"), %extraction GUI socket from State
    gen_tcp:send(Legosocket, term_to_binary(Data)),
    gen_tcp:send(Guisocket, term_to_binary(Locationid)).
            
%%%%%%%%%% Test functions for clients simulation %%%%%%%%%%%
            
tlegoclient() ->
    {ok, Socket} = gen_tcp:connect("localhost", 1232, [binary, {packet, 4}]),
    Trytxt = {lego, "1"},
    gen_tcp:send(Socket, term_to_binary(Trytxt)),
    receive
        {tcp, Socket, Msg} ->
            Msgc = binary_to_term(Msg),
            io:format("Lego has got: ~p~n", [Msgc]),
            Tryans = {lego, "11"},
            gen_tcp:send(Socket, term_to_binary(Tryans))
    end.

tarduclient() ->
    {ok, Socket} = gen_tcp:connect("localhost", 1232, [binary, {packet, 4}]),
    Arduinostart = {arduino, "1"},
    gen_tcp:send(Socket, term_to_binary(Arduinostart)),
    ardlisten (Socket). 

ardlisten(Socket) ->
    receive
        {tcp, Socket, Msg} ->
            Msgc = binary_to_term(Msg),
            io:format("Arduino has got: ~p~n", [Msgc]),
            Arduinodata = {arduino, "1232"},
            gen_tcp:send(Socket, term_to_binary(Arduinodata))
    end,
    ardlisten(Socket).
    
tguiclient() ->
    {ok, Socket} = gen_tcp:connect("localhost", 1232, [binary, {packet, 4}]),
    Guistart = {gui, "1"},
    gen_tcp:send(Socket, term_to_binary(Guistart)),
    guilisten(Socket).
    
guilisten(Socket) ->
    receive
        {tcp, Socket, Msg} ->
            Msgc = binary_to_term(Msg),
            io:format("Gui has got: ~p~n", [Msgc])            
    end,
    guilisten(Socket).