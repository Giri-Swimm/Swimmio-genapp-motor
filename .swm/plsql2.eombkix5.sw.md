---
title: plsql2
---
# Introduction

This document explains the implementation of a PL/SQL procedure designed to manage batch job statuses in the database. The main questions and design decisions covered are:

1. How does the procedure ensure safe concurrent updates to batch records?
2. What rules govern valid status transitions?
3. How are batch status and related counters updated atomically?
4. Why is transaction control left to the caller?
5. How are errors handled within the procedure?

# Handling concurrency and locking

The procedure first verifies that the batch record exists and locks it immediately to prevent concurrent modifications. This is done by selecting the current status with a <SwmToken path="/base/src/PLSQLSample" pos="16:1:5" line-data="               FOR UPDATE NOWAIT;">`FOR UPDATE NOWAIT`</SwmToken> clause, which locks the row and raises an error if another process already holds the lock. This approach avoids race conditions where multiple processes might try to update the same batch simultaneously.

<SwmSnippet path="/base/src/PLSQLSample" line="1">

---

If the batch ID does not exist, a specific error is raised. If the row is locked by another process, a different error is raised to inform the caller about the contention.

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

# Validating status transitions

<SwmSnippet path="/base/src/PLSQLSample" line="24">

---

Before updating, the procedure enforces business rules on allowed status changes. For example, it prevents moving a batch back to 'PROCESSING' if it is already marked as 'SUCCESS' or 'FAILED'. This ensures finalized batches are not reprocessed, preserving data integrity and workflow correctness.

```
        -- 2. Validate state transitions (e.g., prevent moving to PROCESSING if already SUCCESS)
        IF v_current_status IN ('SUCCESS', 'FAILED') AND p_new_status = 'PROCESSING' THEN
            RAISE_APPLICATION_ERROR(-20001, 'Invalid Transition: Cannot re-process a finalized batch.');
        END IF;
```

---

</SwmSnippet>

# Updating batch status and counters

The procedure updates the batch record by setting the new status (converted to uppercase), incrementing success and failure counters by the provided amounts, and updating the error message only if a new one is supplied. The timestamp of the update is also recorded.

<SwmSnippet path="/base/src/PLSQLSample" line="29">

---

This update is atomic within the transaction, ensuring all related fields reflect the current batch state consistently.

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

<SwmSnippet path="/base/src/PLSQLSample" line="38">

---

The procedure deliberately does not include explicit <SwmToken path="/base/src/PLSQLSample" pos="38:11:11" line-data="        -- Note: Transaction control (COMMIT/ROLLBACK) is intentionally left ">`COMMIT`</SwmToken> or <SwmToken path="/base/src/PLSQLSample" pos="38:13:13" line-data="        -- Note: Transaction control (COMMIT/ROLLBACK) is intentionally left ">`ROLLBACK`</SwmToken> statements. This design choice delegates transaction control to the calling environment, allowing it to group multiple operations into a single transaction if needed. It also prevents premature commits that could leave the system in an inconsistent state if subsequent operations fail.

```
        -- Note: Transaction control (COMMIT/ROLLBACK) is intentionally left 
        -- to the calling environment to ensure data consistency.
```

---

</SwmSnippet>

# Error handling strategy

<SwmSnippet path="/base/src/PLSQLSample" line="41">

---

Any unexpected errors during execution are caught by a generic exception handler that re-raises the error after optionally logging it internally. This ensures that errors propagate to the caller for appropriate handling, while still allowing for internal diagnostics if implemented.

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
