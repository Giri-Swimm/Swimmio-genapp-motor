---
title: plsql3
---
# Introduction

This document explains the implementation of a PL/SQL procedure designed to manage batch job statuses in the database. We will cover:

1. How the procedure ensures safe concurrent access to batch records.
2. The logic behind validating allowed state transitions.
3. How the procedure updates batch status and related counters.
4. The rationale for leaving transaction control to the caller.
5. Error handling strategy within the procedure.

# Handling concurrent access and batch existence

The procedure first attempts to select the current status of the batch record while locking it using <SwmToken path="/base/src/PLSQLSample" pos="16:1:5" line-data="               FOR UPDATE NOWAIT;">`FOR UPDATE NOWAIT`</SwmToken>. This prevents race conditions by ensuring that no two processes can update the same batch simultaneously. If the batch ID does not exist, it raises a specific error. If the row is locked by another process, it raises a different error indicating the lock conflict.

<SwmSnippet path="/base/src/PLSQLSample" line="1">

---

This approach guarantees that updates happen on a valid and exclusively locked batch record, avoiding inconsistent or conflicting updates.

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

# Validating state transitions

Before updating, the procedure checks if the requested new status is valid given the current status. Specifically, it prevents moving a batch from a terminal state like 'SUCCESS' or 'FAILED' back to 'PROCESSING'. This enforces business rules that finalized batches should not be reprocessed.

<SwmSnippet path="/base/src/PLSQLSample" line="24">

---

This validation protects the integrity of batch processing states and prevents unintended or erroneous status changes.

```
        -- 2. Validate state transitions (e.g., prevent moving to PROCESSING if already SUCCESS)
        IF v_current_status IN ('SUCCESS', 'FAILED') AND p_new_status = 'PROCESSING' THEN
            RAISE_APPLICATION_ERROR(-20001, 'Invalid Transition: Cannot re-process a finalized batch.');
        END IF;
```

---

</SwmSnippet>

# Updating batch status and counters

The procedure updates the batch record by setting the new status (converted to uppercase), incrementing success and failure counters by the provided amounts, and updating the error message only if a new one is supplied. It also updates the timestamp to the current system time.

<SwmSnippet path="/base/src/PLSQLSample" line="29">

---

This update consolidates all relevant batch progress information in one atomic operation, ensuring the batch record reflects the latest processing results.

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

The procedure explicitly does not perform any COMMIT or ROLLBACK operations. This design choice delegates transaction control to the calling environment, allowing the caller to decide when to finalize or revert changes.

<SwmSnippet path="/base/src/PLSQLSample" line="38">

---

This separation is important for maintaining consistency across multiple related operations that might be part of a larger transaction scope.

```
        -- Note: Transaction control (COMMIT/ROLLBACK) is intentionally left 
        -- to the calling environment to ensure data consistency.
```

---

</SwmSnippet>

# Error handling strategy

Any unexpected errors during execution are caught by a generic exception handler that re-raises the error after optionally logging it internally (logging is implied but not shown). This ensures that errors are propagated back to the caller for appropriate handling.

<SwmSnippet path="/base/src/PLSQLSample" line="41">

---

This approach avoids swallowing errors silently and keeps error management centralized at a higher level.

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
