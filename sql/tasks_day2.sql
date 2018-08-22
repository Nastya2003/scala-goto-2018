create table task1 (id INT, name VARCHAR(50), age INT, salary INT)
INSERT INTO task1 VALUES (1, 'John', 23, 400)
INSERT INTO task1 VALUES (2, 'Mary', 20, 450)
INSERT INTO task1 VALUES (3, 'Max', 29, 1200)
INSERT INTO task1 VALUES (4, 'Felix', 18, 350)
INSERT INTO task1 VALUES (5, 'Groover', 22, 570)
INSERT INTO task1 VALUES (6, 'Ben', 19, 620)

select * from task1 WHERE id = 6
select * from task1 WHERE salary >= 500
INSERT INTO task1 VALUES (7, 'Denis', 23, 600)
DELETE FROM task1 WHERE name = 'Max'
--select * from task1 
select * from task1 WHERE salary >= 500 AND salary <= 1000 

