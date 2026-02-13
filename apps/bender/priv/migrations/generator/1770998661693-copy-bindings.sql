INSERT INTO bender_generator_states (id, state) SELECT process_id, aux_state FROM bender_generator_processes;
