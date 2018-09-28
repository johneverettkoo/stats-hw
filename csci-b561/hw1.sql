/* Problem 1 */
/* I had to remove empty lines from the txt files */
/* and the file paths are specific to my setup */

CREATE TABLE boat(
    bid integer PRIMARY KEY, 
    bname varchar(15), 
    color varchar(15)
);

COPY boat(bid, bname, color)
FROM '/home/johnkoo/Documents/B561/boat.txt' 
DELIMITERS ','
CSV QUOTE E'\'';

CREATE TABLE sailor(
    sid integer PRIMARY KEY, 
    sname varchar(20), 
    rating integer, 
    age integer
);

COPY sailor(sid, sname, rating, age)
FROM '/home/johnkoo/Documents/B561/sailor.txt'
DELIMITERS ','
CSV QUOTE E'\'';

CREATE TABLE reserves(
    sid integer REFERENCES sailor(sid), 
    bid integer REFERENCES boat(bid), 
    day varchar(10)
);

COPY reserves(sid, bid, day)
FROM '/home/johnkoo/Documents/B561/reserves.txt'
DELIMITERS ','
CSV QUOTE E'\'';

/* cleanup - remove tabs */
UPDATE boat SET bname = REGEXP_REPLACE(bname, '\t', '');
UPDATE boat SET color = REGEXP_REPLACE(color, '\t', '');
UPDATE sailor SET sname = REGEXP_REPLACE(sname, '\s+', '');
UPDATE reserves SET day = REGEXP_REPLACE(day, '\t', '');

/* Problem 2 */

/* inserting a NULL into a primary key doesn't work */
INSERT INTO boat VALUES (NULL, 'Boat', 'yellow');

/* inserting a new value into a foreign primary key doesn't work */
INSERT INTO reserves VALUES (1, 101, 'Monday');

/* once that value exists in the table it's a primary key for, it works */
INSERT INTO sailor VALUES (1, 'Mary', 1, 1);
INSERT INTO reserves VALUES (1, 101, 'Monday');

/* Problem 3 */

/* a */
SELECT DISTINCT sname, rating FROM sailor;

/* b */
SELECT DISTINCT bid, color FROM boat;

/* c */
SELECT DISTINCT sname
FROM sailor
WHERE (age >= 15) AND (age <= 30);

/* d */
SELECT DISTINCT boat.bname
FROM boat, reserves
WHERE (boat.bid = reserves.bid) AND (reserves.day IN ('Saturday', 'Sunday'));

/* e */
SELECT DISTINCT sailor.sname
FROM 
    sailor, reserves, 
    (SELECT bid FROM boat WHERE color IN ('red', 'green')) boat
WHERE 
    (boat.bid = reserves.bid) AND
    (sailor.sid = reserves.sid);

/* f */
(SELECT DISTINCT sailor.sname
FROM 
    sailor, reserves, 
    (SELECT bid FROM boat WHERE color = 'red') boat
WHERE 
    (boat.bid = reserves.bid) AND
    (sailor.sid = reserves.sid))
EXCEPT 
(SELECT DISTINCT sailor.sname
FROM 
    sailor, reserves, 
    (SELECT bid FROM boat WHERE color IN ('blue', 'green')) boat
WHERE 
    (boat.bid = reserves.bid) AND
    (sailor.sid = reserves.sid));

/* g */
SELECT DISTINCT sailor.sname
FROM 
    sailor, 
    (SELECT DISTINCT bid, sid FROM reserves) b1, 
    (SELECT DISTINCT bid, sid FROM reserves) b2
WHERE 
    (sailor.sid = b1.sid) AND 
    (sailor.sid = b2.sid) AND 
    (b1.bid <> b2.bid);

/* h */
(SELECT DISTINCT sailor.sname
FROM sailor) 
EXCEPT
(SELECT DISTINCT sailor.sname
FROM sailor, reserves
WHERE sailor.sid = reserves.sid);

/* i */
SELECT DISTINCT r1.sid, r2.sid
FROM reserves r1, reserves r2
WHERE 
    r1.day = 'Saturday' AND 
    r2.day = 'Saturday' AND 
    r1.sid < r2.sid;  /* inequality to avoid reverse ordering duplicates */

/* j */
(SELECT DISTINCT bid FROM reserves)
EXCEPT
(SELECT DISTINCT boat.bid
FROM reserves r1, reserves r2, boat
WHERE 
    boat.bid = r1.bid AND 
    boat.bid = r2.bid AND 
    r1.sid <> r2.sid);
