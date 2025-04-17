

# Add harness size column to CAPTURES  2025-04-17 11:43:49

```sql
ALTER TABLE CAPTURES
ADD COLUMN harness DOUBLE COMMENT 'harness diameter (mm)' AFTER tag_action;
```
