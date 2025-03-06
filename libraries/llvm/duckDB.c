#ifndef EFFEKT_DUCKDB_C
#define EFFEKT_DUCKDB_C

#include <duckdb.h>

struct Pos c_get_Instance() {
    duckdb_database* db_ptr = (duckdb_database*)malloc(sizeof(duckdb_database));

	if (duckdb_open(NULL, db_ptr) == DuckDBError) {
		fprintf(stderr, "Failed to open database\n");
	}
	else {
		return (struct Pos) { .tag = (Int)db_ptr, .obj = NULL, };
	}
}

struct Pos c_get_Connection(const struct Pos db) {
	duckdb_database* db_ptr = (duckdb_database*)db.tag;
	duckdb_connection* con_ptr = (duckdb_connection*)malloc(sizeof(duckdb_connection));

	if (duckdb_connect(*db_ptr, con_ptr) == DuckDBError) {
		fprintf(stderr, "Failed to open connection\n");
	}
	else {
		return (struct Pos) { .tag = (Int)con_ptr, .obj = NULL, };
	}
}

void c_run_Query(const struct Pos con, String query) {
	printf("here");
    duckdb_connection* con_ptr = (duckdb_connection*)con.tag;
    duckdb_result result;
    
    // Convert Effekt string to C string
    char* q = c_bytearray_into_nullterminated_string(query);
    
    // Execute query - pass the result pointer to store the result
    if (duckdb_query(*con_ptr, q, &result) == DuckDBError) {
        fprintf(stderr, "Failed to query database:\n");
        // Free the query string before returning
        free(q);
        // Return an error code
        return;
    }
    
    // Query succeeded, handle the result
    idx_t row_count = duckdb_row_count(&result);
    idx_t column_count = duckdb_column_count(&result);
    
    printf("Query executed successfully. Results: %llu rows, %llu columns\n", 
           (unsigned long long)row_count, (unsigned long long)column_count);
    
    // Print column names
    for (size_t i = 0; i < column_count; i++) {
        printf("%s ", duckdb_column_name(&result, i));
    }
    printf("\n");
    
    // Print data
    for (size_t row_idx = 0; row_idx < row_count; row_idx++) {
        for (size_t col_idx = 0; col_idx < column_count; col_idx++) {
            char *val = duckdb_value_varchar(&result, col_idx, row_idx);
            printf("%s ", val);
            duckdb_free(val);
        }
        printf("\n");
    }
    
    // Clean up resources
    duckdb_destroy_result(&result);
    free(q);
    
    // Return success code
	return;
}



void c_test_duckDB(Stack stack) {
    duckdb_database db = NULL;
	duckdb_connection con = NULL;
	duckdb_result result;

	if (duckdb_open(NULL, &db) == DuckDBError) {
		fprintf(stderr, "Failed to open database\n");
		goto cleanup;
	}
	if (duckdb_connect(db, &con) == DuckDBError) {
		fprintf(stderr, "Failed to open connection\n");
		goto cleanup;
	}
	if (duckdb_query(con, "CREATE TABLE integers(i INTEGER, j INTEGER);", NULL) == DuckDBError) {
		fprintf(stderr, "Failed to query database\n");
		goto cleanup;
	}
	if (duckdb_query(con, "INSERT INTO integers VALUES (3, 4), (5, 6), (7, NULL);", NULL) == DuckDBError) {
		fprintf(stderr, "Failed to query database\n");
		goto cleanup;
	}
	if (duckdb_query(con, "SELECT * FROM integers", &result) == DuckDBError) {
		fprintf(stderr, "Failed to query database\n");
		goto cleanup;
	}
	// print the names of the result
	idx_t row_count = duckdb_row_count(&result);
	idx_t column_count = duckdb_column_count(&result);
	for (size_t i = 0; i < column_count; i++) {
		printf("%s ", duckdb_column_name(&result, i));
	}
	printf("\n");
	// print the data of the result
	for (size_t row_idx = 0; row_idx < row_count; row_idx++) {
		for (size_t col_idx = 0; col_idx < column_count; col_idx++) {
			char *val = duckdb_value_varchar(&result, col_idx, row_idx);
			printf("%s ", val);
			duckdb_free(val);
		}
		printf("\n");
	}
	// duckdb_print_result(result);
cleanup:
	duckdb_destroy_result(&result);
	duckdb_disconnect(&con);
	duckdb_close(&db);

	resume_Int(stack, 13); // Rückgabe der Liste an Effekt
	return;
}

#endif