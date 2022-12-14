-record(player, { uuid :: atom(),
                  nickname :: string(),
                  role :: 'folk' | 'spy' | 'fool',
                  word :: string(),
                  died :: boolean
                }).
