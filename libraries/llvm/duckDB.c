#ifndef EFFEKT_DUCKDB_C
#define EFFEKT_DUCKDB_C

#include <iostream>
#include <duckdb.h>


int c_test_duckDB(Stack stack) {
    duckdb_database db;
    duckdb_connection con;
    duckdb_open(nullptr, &db);
    duckdb_connect(db, &con);

    duckdb_result res;
    duckdb_query(con, "CREATE TABLE integers (i INTEGER, j INTEGER);", NULL);
    duckdb_query(con, "INSERT INTO integers VALUES (3, 4), (5, 6), (7, NULL);", NULL);
    duckdb_query(con, "SELECT * FROM integers;", &res);

    struct Pos rows = c_list_make_empty(); // Effekt-kompatible leere Liste erstellen

    while (true) {
        duckdb_data_chunk result = duckdb_fetch_chunk(res);
        if (!result) {
            break;
        }

        idx_t row_count = duckdb_data_chunk_get_size(result);

        duckdb_vector col1 = duckdb_data_chunk_get_vector(result, 0);
        int32_t* col1_data = (int32_t*)duckdb_vector_get_data(col1);
        uint64_t* col1_validity = duckdb_vector_get_validity(col1);

        duckdb_vector col2 = duckdb_data_chunk_get_vector(result, 1);
        int32_t* col2_data = (int32_t*)duckdb_vector_get_data(col2);
        uint64_t* col2_validity = duckdb_vector_get_validity(col2);

        for (idx_t row = 0; row < row_count; row++) {
            struct Pos row_tuple = c_tuple_make(2); // Effekt-Tupel erstellen

            if (duckdb_validity_row_is_valid(col1_validity, row)) {
                c_tuple_set(row_tuple, 0, c_int_make(col1_data[row]));
            } else {
                c_tuple_set(row_tuple, 0, c_none_make());
            }

            if (duckdb_validity_row_is_valid(col2_validity, row)) {
                c_tuple_set(row_tuple, 1, c_int_make(col2_data[row]));
            } else {
                c_tuple_set(row_tuple, 1, c_none_make());
            }

            rows = c_list_cons(row_tuple, rows); // Zeile zur Liste hinzufügen
        }

        duckdb_destroy_data_chunk(&result);
    }

    // clean-up
    duckdb_destroy_result(&res);
    duckdb_disconnect(&con);
    duckdb_close(&db);

    resume_Pos(stack, rows); // Rückgabe der Liste an Effekt
    return 0;
}

#endif