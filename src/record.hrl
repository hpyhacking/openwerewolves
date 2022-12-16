-record(player, { uuid :: atom(),
                  nickname :: string() }).

-record(playing_player, { role :: undefined | 'folk' | 'spy' | 'fool',
                          word = undefined  :: undefined | string(),
                          is_died = false :: boolean(),
                          first_speech = false :: boolean(),
                          player :: #player{} }).

-record(waiting_player, { is_me = false :: boolean(),
                          is_ready = false :: boolean(),
                          player   :: #player{} }).

-record(game, { pin     :: atom(),
                state   :: waiting | playing,
                folks   :: 'undefined' | integer(),
                spies   :: 'undefined' | integer(),
                fools   :: 'undefined' | integer(),
                playing_players :: nonempty_list(playing_player),
                waiting_players :: nonempty_list(waiting_player) }).

-record(request, { action :: atom(),
                   data :: 'undefined' | tuple() }).

-record(response, { action :: atom(),
                    data :: any() }).

-define(RSP_JOIN_GAME_OK, #response{action = join, data = ok}).
-define(RSP_JOIN_GAME_ERROR, #response{action = join, data = error}).
-define(RSP_START_GAME_ERROR, #response{action = start, data = error}).

-define(ROLES, [[3,1,0],    %4
                [3,1,1],    %5
                [4,1,1],    %6
                [5,1,1],    %7
                [6,1,1],    %8
                [6,2,1],    %9
                [7,2,1],    %10
                [8,2,1],    %11
                [9,2,1],    %12
                [9,3,1],    %13
                [10,3,1],   %14
                [11,3,1],   %15
                [12,3,1]]). %16

