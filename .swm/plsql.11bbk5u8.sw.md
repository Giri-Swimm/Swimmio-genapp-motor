---
title: plsql
---
# Introduction

This document explains the implementation of a PL/SQL procedure designed to manage batch job statuses in the database. The main points covered are:

1. How the procedure ensures safe concurrent access to batch records.
2. How it enforces valid state transitions to prevent inconsistent batch states.
3. How it updates batch status and related counters atomically.
4. Why transaction control is delegated to the caller.
5. How error handling is structured to propagate meaningful errors.

# Handling concurrency and locking

The procedure first attempts to select the current status of the batch record while locking it for update. This prevents race conditions where multiple processes might try to update the same batch simultaneously. If the batch does not exist, it raises a clear error. If the row is locked by another process, it raises a different error indicating the lock conflict.

<SwmSnippet path="/base/src/PLSQLSample.pl" line="1">

---

This approach guarantees that only one process can update a batch at a time, avoiding data corruption or lost updates.

```
CREATE OR REPLACE  PROCEDURE prc_manage_batch_status (
        p_batch_id       IN  batch_job_master.batch_id%TYPE,
        p_new_status     IN  batch_job_master.status%TYPE,
        p_error_msg      IN  batch_job_master.error_message%TYPE DEFAULT NULL,
        p_success_count  IN  batch_job_master.records_success%TYPE DEFAULT 0,
        p_failed_count   IN  batch_job_master.records_failed%TYPE DEFAULT 0
    ) IS
        v_current_status batch_job_master.status%TYPE;
    BEGIN
        -- 1. Verify batch exists and lock the row to prevent race conditions
        BEGIN
            SELECT status 
              INTO v_current_status
              FROM batch_job_master
             WHERE batch_id = p_batch_id
               FOR UPDATE NOWAIT;
        EXCEPTION
            WHEN NO_DATA_FOUND THEN
                raise_application_error(-20002, 'Error: Batch ID ' || p_batch_id || ' does not exist.');
            WHEN OTHERS THEN
                raise_application_error(-20003, 'Error: Batch record is currently locked by another process.');
        END;
```

---

</SwmSnippet>

# Enforcing valid state transitions

Before updating, the procedure checks if the batch is already in a terminal state like 'SUCCESS' or 'FAILED'. If so, it forbids moving back to 'PROCESSING'. This prevents re-processing of batches that have been finalized, which could cause inconsistent or duplicated processing.

<SwmSnippet path="/base/src/PLSQLSample.pl" line="24">

---

This validation enforces business rules about batch lifecycle and maintains data integrity.

```
        -- 2. Validate state transitions (e.g., prevent moving to PROCESSING if already SUCCESS)
        IF v_current_status IN ('SUCCESS', 'FAILED') AND p_new_status = 'PROCESSING' THEN
            RAISE_APPLICATION_ERROR(-20001, 'Invalid Transition: Cannot re-process a finalized batch.');
        END IF;
```

---

</SwmSnippet>

# Updating batch status and counters

The procedure updates the batch record by setting the new status (converted to uppercase for consistency), incrementing success and failure counters, and updating the error message only if a new one is provided. It also updates the timestamp to reflect the change time.

<SwmSnippet path="/base/src/PLSQLSample.pl" line="29">

---

All these updates happen in a single atomic statement, ensuring the batch record is consistent after the procedure completes.

```
        -- 3. Execute the status update
        UPDATE batch_job_master
           SET status          = UPPER(p_new_status),
               records_success = records_success + p_success_count,
               records_failed  = records_failed + p_failed_count,
               error_message   = NVL(p_error_msg, error_message),
               updated_at      = SYSTIMESTAMP
         WHERE batch_id        = p_batch_id;
```

---

</SwmSnippet>

# Transaction control responsibility

The procedure explicitly leaves transaction control (commit or rollback) to the caller. This design allows the caller to group multiple operations into a single transaction if needed, or handle errors and retries at a higher level.

<SwmSnippet path="/base/src/PLSQLSample.pl" line="38">

---

This separation of concerns avoids premature commits inside the procedure that could complicate error recovery or multi-step workflows.

```
        -- Note: Transaction control (COMMIT/ROLLBACK) is intentionally left 
        -- to the calling environment to ensure data consistency.
```

---

</SwmSnippet>

# Error handling and propagation

Any unexpected errors during execution are caught by a generic exception handler that re-raises them. This allows the calling environment to log or handle system errors as appropriate, without swallowing or masking them inside the procedure.

<SwmSnippet path="/base/src/PLSQLSample.pl" line="41">

---

This approach keeps the procedure focused on its core logic and delegates error management to the caller.

```
    EXCEPTION
        WHEN OTHERS THEN
            -- Log the system error internally if required, then re-raise
            RAISE;
    END prc_manage_batch_status;
/
```

---

</SwmSnippet>

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBU3dpbW1pby1nZW5hcHAtbW90b3IlM0ElM0FHaXJpLVN3aW1t" repo-name="Swimmio-genapp-motor"><sup>Powered by [Swimm](https://app.swimm.io/)</sup></SwmMeta>
