-module(node).

-behaviour(gen_server).

%% API
-export([
    start_link/0,
    start_link/1,
    send_echo/0,
    send_nodes_echo/1]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
    configured_nodes = [],
    connected_nodes = [],
    node_connect_retry_interval = 5000,
    echo_interval = 5000,
    timer = null
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    start_link(['na@HP01', 'nb@HP02', 'nc@HP03', 'nd@HP04']).

start_link(Nodes) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Nodes], []).


send_echo() ->
    gen_server:call(?SERVER, echo).

send_nodes_echo(Nodes) ->
    [rpc:call(N, node, send_echo, []) || N<-Nodes].



%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([CfgNodes]) ->
    {ok, Ref} = timer:send_after(5, connect_nodes),
    {ok, connect_to_nodes, #state{configured_nodes=CfgNodes,
                                  timer=Ref}}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(echo, _From, State) ->
    {reply, {ok, echo}, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(connect_nodes, S=#state{configured_nodes=CfgNodes,
                                    node_connect_retry_interval=NCRI,
                                    echo_interval=EI,
                                    timer=Ref}) ->
    timer:cancel(Ref),
    lists:map(fun(Node)-> net_adm:ping(Node) end, CfgNodes),
    NotConnectedNodes = CfgNodes -- [node()|nodes()],
    case NotConnectedNodes of
        [] ->
            {ok, EchoRef} = timer:send_after(EI, post_echo_request),
            {noreply, S#state{connected_nodes=nodes(),
                              timer=EchoRef}};
	_ ->
            {ok, RetryRef} = timer:send_after(NCRI, connect_nodes_retry),
            {noreply, S#state{timer=RetryRef}}
    end;
handle_info(connect_nodes_retry, S=#state{configured_nodes=CfgNodes,
                                          echo_interval=EI,
                                          timer=Ref}) ->
    timer:cancel(Ref),
    lists:map(fun(Node)-> net_adm:ping(Node) end, CfgNodes),
    {ok, EchoRef} = timer:send_after(EI, post_echo_request),
    {noreply, S#state{connected_nodes=nodes(), timer=EchoRef}};

handle_info(post_echo_request, S=#state{timer=Ref,
                                        echo_interval=EI,
                                        connected_nodes=CNodes}) ->
    timer:cancel(Ref),
    send_nodes_echo(CNodes),
    {ok, EchoRef} = timer:send_after(EI, post_echo_request),
    {noreply, S#state{connected_nodes=nodes(),
                      timer=EchoRef}};
handle_info(_Info, State) ->
    {noreply, State}.



%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
