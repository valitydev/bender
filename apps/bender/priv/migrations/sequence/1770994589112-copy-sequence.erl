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
                    ({ID, AuxState}, "" = Acc) ->
                        case value(ID, AuxState) of
                            {ok, Value} ->
                                " ('" ++ unicode:characters_to_list(ID) ++ "', " ++ integer_to_list(Value) ++ ") ";
                            {error, _} ->
                                Acc
                        end;
                    ({ID, AuxState}, Acc) ->
                        case value(ID, AuxState) of
                            {ok, Value} ->
                                " ('" ++ unicode:characters_to_list(ID) ++ "', " ++ integer_to_list(Value) ++ "), " ++ Acc;
                            {error, _} ->
                                Acc
                        end
                end,
                "",
                Rows
            ),
            {ok, _} = epg_pool:query(
                Connection,
                "INSERT INTO bender_sequence_values (id, value) VALUES " ++ Values ++
                "  ON CONFLICT (id) DO UPDATE "
                "  SET value = GREATEST(EXCLUDED.value, bender_sequence_values.value)"
            ),
            perform_batch(Connection, Offset + erlang:length(Rows), Limit)
    end.

value(ID, null) ->
    logger:warning("migrations. sequence ~p state is null", [ID]),
    {error, state_is_null};
value(ID, AuxState) when is_binary(AuxState) ->
    try binary_to_term(AuxState) of
        #{value := Value} ->
            {ok, Value};
        BadState ->
            logger:warning("migrations. sequence ~p bad state: ~p", [ID, BadState]),
            {error, bad_state}
    catch
        _Error:_Term:_Stack ->
            logger:warning("migrations. sequence ~p bad state: ~p", [ID, AuxState]),
            {error, bad_state}
    end.
