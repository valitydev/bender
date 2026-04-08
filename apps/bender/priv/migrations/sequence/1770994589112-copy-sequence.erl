-module('1770994589112-copy-sequence').

-export([perform/2]).

-spec perform(_, _) -> _.
perform(Connection, MigrationOpts) ->
    BatchSize = proplists:get_value(batch_size, MigrationOpts, 5000),
    perform_batch(Connection, 0, BatchSize).

perform_batch(Connection, Offset, Limit) ->
    SQL = "SELECT process_id, aux_state FROM bender_sequence_processes ORDER BY created_at OFFSET $1 LIMIT $2",
    case epg_pool:query(Connection, SQL, [Offset, Limit]) of
        {ok, _, []} ->
            ok;
        {ok, _, Rows} ->
            Values = lists:foldl(
                fun
                    ({ID, AuxState}, "") ->
                        #{value := Value} = binary_to_term(AuxState),
                        " ('" ++ unicode:characters_to_list(ID) ++ "', " ++ integer_to_list(Value) ++ ") ";
                    ({ID, AuxState}, Acc) ->
                        #{value := Value} = binary_to_term(AuxState),
                        " ('" ++ unicode:characters_to_list(ID) ++ "', " ++ integer_to_list(Value) ++ "), " ++ Acc
                end,
                "",
                Rows
            ),
            {ok, _} = epg_pool:query(
                Connection,
                "INSERT INTO bender_sequence_values (id, value) VALUES " ++ Values
            ),
            perform_batch(Connection, Offset + erlang:length(Rows), Limit)
    end.
