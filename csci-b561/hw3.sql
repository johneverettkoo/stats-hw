/* setup */
/* note: insert commands generated in a separate python script */
/* then copy-pasted into this file */

\echo setup
\echo

\echo creating and connecting to database "hw3"
CREATE DATABASE hw3;
\connect hw3;

\echo

/* problem 1 */
\echo problem 1
CREATE TABLE a (x integer);
INSERT INTO a(x) VALUES (1), (2), (3), (4), (5);
SELECT 
    x, 
    SQRT(x) AS square_root_x, 
    POWER(x, 2) AS x_squared, 
    POWER(2, x) AS two_to_the_power_x, 
    x! AS x_factorial, 
    LN(X) AS logarithm_x
FROM a;
DROP TABLE a;
\echo 

/* problem 2 */
\echo problem 2
CREATE TABLE a (x integer);
CREATE TABLE b (x integer);
INSERT INTO a(x) VALUES (1), (2), (3);
INSERT INTO b(x) VALUES (1), (3), (4), (5);
SELECT 
    COUNT(a_minus_b.x) = 0 AS empty_a_minus_b, 
    COUNT(symmetric_difference.x) > 0 AS not_empty_symmetric_difference, 
    COUNT(a_intersection_b.x) = 0 AS empty_a_intersection_b
FROM 
    (
        (SELECT x FROM a) EXCEPT (SELECT x FROM b)
    ) a_minus_b,
    (
        ((SELECT x FROM a) EXCEPT (SELECT x FROM b))
        UNION
        ((SELECT x FROM b) EXCEPT (SELECT x FROM a) )
    ) symmetric_difference, 
    (
        (SELECT x FROM a) INTERSECT (SELECT x FROM b)
    ) a_intersection_b
;
DROP TABLE a;
DROP TABLE b;
\echo

/* problem 3 */
\echo problem 3
CREATE TABLE pair (x integer, y integer);
INSERT INTO pair(x, y) VALUES (1, 1), (2, 2), (3, 3), (4, 4), (5, 5);
SELECT pair1.x, pair1.y, pair2.x, pair2.y
FROM pair pair1, pair pair2
WHERE (pair1.x + pair1.y = pair2.x + pair2.y) AND
    (pair1.x <> pair2.x OR pair1.y <> pair2.y);
DROP TABLE pair;
\echo

/* problem 4 */
\echo problem 4
CREATE TABLE p (p boolean);
CREATE TABLE q (q boolean);
CREATE TABLE r (r boolean);
INSERT INTO p(p) VALUES (TRUE), (FALSE), (NULL);
INSERT INTO q(q) VALUES (TRUE), (FALSE), (NULL);
INSERT INTO r(r) VALUES (TRUE), (FALSE), (NULL);
SELECT p.p, q.q, r.r, NOT(NOT(p.p) OR q.q) OR r.r
FROM p, q, r;
DROP TABLE p;
DROP TABLE q;
DROP TABLE r;
\echo

/* problem 5 */
\echo problem 5
\echo

CREATE TABLE a (x integer);
CREATE TABLE b (x integer);
CREATE TABLE c (x integer);
INSERT INTO a VALUES (1), (2);
INSERT INTO b VALUES (1), (4), (5);
/* INSERT INTO b VALUES (1), (2); */
INSERT INTO c VALUES (1), (2), (4);

/* part a */
\echo part a
SELECT EXISTS(
    SELECT a_intersect_b.x
    FROM (
        (
            SELECT x FROM a
        ) INTERSECT (
            SELECT x FROM b
        ) 
    ) a_intersect_b
);
SELECT EXISTS (
    SELECT a.x 
    FROM a, b
    WHERE a.x = b.x
);
\echo

/* part b */
\echo part b
SELECT NOT EXISTS ((SELECT x FROM a) EXCEPT (SELECT x FROM b));
SELECT NOT EXISTS (SELECT x FROM a WHERE x NOT IN (SELECT x FROM b));
\echo

/* part c */
\echo part c
/* A intersect B = B means B is a subset of A */
/* so just reverse the sets from part (b) */
SELECT NOT EXISTS ((SELECT x FROM b) EXCEPT (SELECT x FROM a));
SELECT NOT EXISTS (SELECT x FROM b WHERE x NOT IN (SELECT x FROM a));
\echo

/* part d */
\echo part d
SELECT EXISTS (
    SELECT x
    FROM (
        (
            ((SELECT x from a) UNION (SELECT x FROM b))
        ) EXCEPT (
            ((SELECT x FROM a) INTERSECT (SELECT x FROM B))
        )
    ) a_union_b_minus_a_intersct_b
);
/* a is not a subset of b or b is not a subset of a */
SELECT EXISTS (
    SELECT x
    FROM a
    WHERE x NOT IN (SELECT x FROM b)
) OR EXISTS (
    SELECT x
    FROM b
    WHERE x NOT IN (SELECT x FROM a)
);
\echo

/* part e */
\echo part e
SELECT NOT EXISTS (
    SELECT a1.x, a2.x, a3.x
    FROM 
        ((SELECT x FROM a) INTERSECT (SELECT x FROM b)) a1, 
        ((SELECT x FROM a) INTERSECT (SELECT x FROM b)) a2, 
        ((SELECT x FROM a) INTERSECT (SELECT x FROM b)) a3
    WHERE a1.x <> a2.x AND
        a1.x <> a3.x AND 
        a2.x <> a3.x
);
SELECT NOT EXISTS (
    SELECT s1.x, s2.x, s3.x
    FROM 
        (SELECT a1.x FROM a a1, b b1 WHERE a1.x = b1.x) s1,
        (SELECT a2.x FROM a a2, b b2 WHERE a2.x = b2.x) s2, 
        (SELECT a3.x FROM a a3, b b3 WHERE a3.x = b3.x) s3
    WHERE 
        s1.x <> s2.x AND
        s1.x <> s3.x AND 
        s2.x <> s3.x
);
\echo

/* part f */
\echo part f
SELECT NOT EXISTS (
    (
        (SELECT x FROM a) UNION (SELECT x FROM b)
    ) EXCEPT (
        SELECT x FROM c
    )
);
SELECT NOT EXISTS (
    SELECT a_union_b.x FROM (
        (SELECT x FROM a) UNION (SELECT x FROM b)
    ) a_union_b
    WHERE a_union_b.x NOT IN (SELECT x FROM c));
\echo

/* part g */
\echo part g
/* check that the size of the set is not two or larger */
/* and that it is not empty */
SELECT NOT EXISTS (
    SELECT s1.x, s2.x
    FROM 
        ((
            (SELECT x FROM a) EXCEPT (SELECT x FROM b)
        ) UNION (
            (SELECT x FROM b) EXCEPT (SELECT x FROM c)
        )) s1, 
        ((
            (SELECT x FROM a) EXCEPT (SELECT x FROM b)
        ) UNION (
            (SELECT x FROM b) EXCEPT (SELECT x FROM c)
        )) s2
    WHERE s1.x <> s2.x
) AND EXISTS (
    SELECT s3.x
    FROM 
        ((
            (SELECT x FROM a) EXCEPT (SELECT x FROM b)
        ) UNION (
            (SELECT x FROM b) EXCEPT (SELECT x FROM c)
        )) s3
);
SELECT NOT EXISTS (
    SELECT s1.x, s2.x
    FROM
        ((
            SELECT x FROM a WHERE x NOT IN (SELECT x FROM b)
        ) UNION (
            SELECT x FROM b WHERE x NOT IN (SELECT x FROM c)
        )) s1, 
        ((
            SELECT x FROM a WHERE x NOT IN (SELECT x FROM b)
        ) UNION (
            SELECT x FROM b WHERE x NOT IN (SELECT x FROM c)
        )) s2
    WHERE s1.x <> s2.x
) AND EXISTS (
    SELECT s3.x
    FROM 
        ((
            SELECT x FROM a WHERE x NOT IN (SELECT x FROM B)
        ) UNION (
            SELECT x FROM b WHERE x NOT IN (SELECT x FROM c)
        )) s3
);
\echo

/* problem 6 */
\echo problem 6
\echo

/* part a */
\echo part a
SELECT COUNT(a_intersect_b.x) <> 0
FROM ((SELECT a.x FROM a) INTERSECT (SELECT b.x FROM b)) a_intersect_b;
\echo

/* part b */
\echo part b
\echo
SELECT COUNT(a.x) = 0
FROM a
WHERE x NOT IN (SELECT x FROM b);

/* part c */
\echo part c
SELECT COUNT(s.x) = 0
FROM (
    (
        SELECT b.x FROM b
    ) EXCEPT (
        ((SELECT a.x FROM a) INTERSECT (SELECT b.x FROM b))
    )
) s;
\echo

/* part d */
\echo part d
SELECT COUNT(a.x) + COUNT(b.x) > 0
FROM a, b
WHERE a.x NOT IN (SELECT b2.x FROM b b2) AND
    b.x NOT IN (SELECT a2.x FROM a a2);
\echo

/* part e */
\echo part e
SELECT COUNT(a_intersect_b.x) <= 2
FROM (
    (
        SELECT x FROM a
    ) INTERSECT (
        SELECT x FROM b
    )
) a_intersect_b;
\echo

/* part f */
\echo part f
SELECT COUNT(a_union_b.x) = 0
FROM (
    (
        SELECT x FROM a
    ) UNION (
        SELECT x FROM b
    )
) a_union_b
WHERE a_union_b.x NOT IN (SELECT x FROM c);
\echo

/* part g */
\echo part g
SELECT COUNT(s.x) = 1
FROM (
    (
        (
            SELECT x FROM a
        ) EXCEPT (
            SELECT x FROM b
        )
    ) UNION (
        (
            SELECT x FROM b
        ) EXCEPT (
            SELECT x FROM c
        )
    )
) s;
\echo

DROP TABLE a;
DROP TABLE b;
DROP TABLE c;

/* problem 7 */
\echo problem 7
CREATE TABLE w (a integer, b varchar(5));
/* INSERT INTO w(a, b) VALUES
    (1, 'John'), 
    (2, 'Ellen'), 
    (3, 'Ann'); */
INSERT INTO w(a, b) VALUES 
    (1, 'John'), 
    (2, 'Ellen'), 
    (2, 'Linda'), 
    (3, 'Ann'), 
    (4, 'Ann'), 
    (4, 'Nick'), 
    (4, 'Vince'), 
    (4, 'Lisa');
SELECT freq_table.a 
FROM (
    SELECT a, COUNT(1) freq
    FROM w
    GROUP BY a
) freq_table
WHERE 1 = ALL (
    SELECT COUNT(1) 
    FROM w
    GROUP BY a
) OR freq_table.freq > 1;
DROP TABLE w;
\echo

/* problem 8 */
\echo problem 8
\echo

\echo creating table "student"
CREATE TABLE student (
    sid integer PRIMARY KEY,
    sname varchar(15)
);
INSERT INTO student(sid, sname) VALUES
    (1001, 'Jean'),
    (1002, 'Maria'),
    (1003, 'Anna'),
    (1004, 'Chin'),
    (1005, 'John'),
    (1006, 'Ryan'),
    (1007, 'Catherine'),
    (1008, 'Emma'),
    (1009, 'Jan'),
    (1010, 'Linda'),
    (1011, 'Nick'),
    (1012, 'Eric'),
    (1013, 'Lisa'),
    (1014, 'Filip'),
    (1015, 'Dirk'),
    (1016, 'Mary'),
    (1017, 'Ellen'),
    (1020, 'Greg'),
    (1022, 'Qin'),
    (1023, 'Melanie'),
    (1040, 'Pam');

\echo creating table "major"
CREATE TABLE major (
    sid integer REFERENCES student(sid), 
    major varchar(15)
);
INSERT INTO major(sid, major) VALUES 
    (1001, 'Math'),
    (1001, 'Physics'),
    (1002, 'CS'),
    (1002, 'Math'),
    (1003, 'Math'),
    (1004, 'CS'),
    (1006, 'CS'),
    (1007, 'CS'),
    (1007, 'Physics'),
    (1008, 'Physics'),
    (1009, 'Biology'),
    (1010, 'Biology'),
    (1011, 'CS'),
    (1011, 'Math'),
    (1012, 'CS'),
    (1013, 'CS'),
    (1013, 'Psychology'),
    (1014, 'Theater'),
    (1017, 'Anthropology'),
    (1022, 'CS'),
    (1015, 'Chemistry');

\echo creating table "book"
CREATE TABLE book (
    bookno integer PRIMARY KEY, 
    title varchar(30), 
    price integer
);
INSERT INTO book(bookno, title, price) VALUES 
    (2001, 'Databases', 40),
    (2002, 'OperatingSystems', 25),
    (2003, 'Networks', 20),
    (2004, 'AI', 45),
    (2005, 'DiscreteMathematics', 20),
    (2006, 'SQL', 25),
    (2007, 'ProgrammingLanguages', 15),
    (2008, 'DataScience', 50),
    (2009, 'Calculus', 10),
    (2010, 'Philosophy', 25),
    (2012, 'Geometry', 80),
    (2013, 'RealAnalysis', 35),
    (2011, 'Anthropology', 50),
    (2014, 'Topology', 70);

\echo creating table buys
CREATE TABLE buys (
    sid integer REFERENCES student(sid), 
    bookno integer REFERENCES book(bookno)
);
INSERT INTO buys(sid, bookno) VALUES 
    (1023, 2012),
    (1023, 2014),
    (1040, 2002),
    (1001, 2002),
    (1001, 2007),
    (1001, 2009),
    (1001, 2011),
    (1001, 2013),
    (1002, 2001),
    (1002, 2002),
    (1002, 2007),
    (1002, 2011),
    (1002, 2012),
    (1002, 2013),
    (1003, 2002),
    (1003, 2007),
    (1003, 2011),
    (1003, 2012),
    (1003, 2013),
    (1004, 2006),
    (1004, 2007),
    (1004, 2008),
    (1004, 2011),
    (1004, 2012),
    (1004, 2013),
    (1005, 2007),
    (1005, 2011),
    (1005, 2012),
    (1005, 2013),
    (1006, 2006),
    (1006, 2007),
    (1006, 2008),
    (1006, 2011),
    (1006, 2012),
    (1006, 2013),
    (1007, 2001),
    (1007, 2002),
    (1007, 2003),
    (1007, 2007),
    (1007, 2008),
    (1007, 2009),
    (1007, 2010),
    (1007, 2011),
    (1007, 2012),
    (1007, 2013),
    (1008, 2007),
    (1008, 2011),
    (1008, 2012),
    (1008, 2013),
    (1009, 2001),
    (1009, 2002),
    (1009, 2011),
    (1009, 2012),
    (1009, 2013),
    (1010, 2001),
    (1010, 2002),
    (1010, 2003),
    (1010, 2011),
    (1010, 2012),
    (1010, 2013),
    (1011, 2002),
    (1011, 2011),
    (1011, 2012),
    (1012, 2011),
    (1012, 2012),
    (1013, 2001),
    (1013, 2011),
    (1013, 2012),
    (1014, 2008),
    (1014, 2011),
    (1014, 2012),
    (1017, 2001),
    (1017, 2002),
    (1017, 2003),
    (1017, 2008),
    (1017, 2012),
    (1020, 2001),
    (1020, 2012),
    (1022, 2014);
\echo 

/* part a.i. */
\echo part a.i.
CREATE FUNCTION booksBoughtByStudent(s INTEGER) 
    RETURNS TABLE(bookno INTEGER, title VARCHAR(30), price INTEGER) AS
    $$
        SELECT DISTINCT book.bookno, book.title, book.price
        FROM book, buys
        WHERE book.bookno = buys.bookno AND
            buys.sid = s;
    $$ LANGUAGE SQL;
\echo

/* part a.ii. */
\echo part a.ii.
SELECT * FROM booksBoughtByStudent(1001);
SELECT * FROM booksBoughtByStudent(1015);
\echo

/* part a.iii. */
\echo part a.iii.
SELECT student.sid
FROM student
WHERE 1 = (
    SELECT COUNT(*) 
    FROM booksBoughtByStudent(student.sid) s
    WHERE s.price < 50
);
SELECT DISTINCT s1.sid, s2.sid
FROM student s1, student s2
WHERE s1.sid <> s2.sid AND NOT EXISTS (
    (
        (SELECT bookno FROM booksBoughtByStudent(s1.sid)) EXCEPT 
        (SELECT bookno FROM booksBoughtByStudent(s2.sid))
    ) UNION (
        (SELECT bookno FROM booksBoughtByStudent(s2.sid)) EXCEPT
        (SELECT bookno FROM booksBoughtByStudent(s1.sid))
    )
);
\echo

/* part b.i. */
\echo part b.i.
CREATE FUNCTION studentsWhoBoughtBook(b INTEGER)
    RETURNS TABLE(sid INTEGER, sname VARCHAR(15)) AS
    $$
        SELECT student.sid, student.sname
        FROM buys, student
        WHERE buys.sid = student.sid AND buys.bookno = b;
    $$ LANGUAGE SQL;
\echo

/* part b.ii. */
\echo part b.ii.
SELECT * FROM studentsWhoBoughtBook(2001);
SELECT * FROM studentsWhoBoughtBook(2010);
\echo

/* part b.iii. */
\echo part b.iii.
SELECT DISTINCT book.bookno
FROM book, student student1, student student2
WHERE student1.sid <> student2.sid AND 
    book.bookno IN (SELECT bookno FROM booksBoughtByStudent(student1.sid)) AND 
    book.bookno IN (SELECT bookno FROM booksBoughtByStudent(student2.sid)) AND
    student1.sid IN (
        SELECT buys1.sid 
        FROM book book1, buys buys1 
        WHERE book1.bookno = buys1.bookno AND book1.price >= 30
    ) AND 
    student2.sid IN (
        SELECT buys2.sid 
        FROM book book2, buys buys2
        WHERE book2.bookno = buys2.bookno AND book2.price >= 30
    );
\echo

/* part c.i. */
\echo part c.i.
SELECT subquery.sid, subquery.major
FROM (
    SELECT student.sid, major.major, COUNT(1) freq
    FROM student, major, buys, book
    WHERE student.sid = major.sid AND 
        student.sid = buys.sid AND 
        buys.bookno = book.bookno AND 
        book.price > 30
    GROUP BY(student.sid, major.major)
) subquery
WHERE subquery.freq >= 4
ORDER BY subquery.sid;
\echo

/* part c.ii. */
\echo part c.ii.
SELECT subquery1.sid, subquery2.sid 
FROM 
    (
        SELECT student.sid, SUM(book.price) total
        FROM student, buys, book
        WHERE student.sid = buys.sid AND 
            buys.bookno = book.bookno
        GROUP BY student.sid
    ) subquery1,
    (
        SELECT student.sid, SUM(book.price) total
        FROM student, buys, book
        WHERE student.sid = buys.sid AND 
            buys.bookno = book.bookno
        GROUP BY student.sid
    ) subquery2
WHERE subquery1.sid <> subquery2.sid AND 
    subquery1.total = subquery2.total
ORDER BY subquery1.sid;
\echo

/* part c.iii. */
\echo part c.iii.
SELECT subquery1.sname, subquery1.sid
FROM (
    SELECT student.sname, student.sid, SUM(book.price) total_purchase
    FROM student, buys, book
    WHERE student.sid = buys.sid AND
        buys.bookno = book.bookno
    GROUP BY(student.sname, student.sid)
) subquery1
WHERE subquery1.total_purchase > (
    SELECT AVG(subquery2.total_purchase)
    FROM (
        SELECT SUM(book.price) total_purchase
        FROM major, student, buys, book
        WHERE major.major = 'CS' AND
            major.sid = student.sid AND
            student.sid = buys.sid AND
            buys.bookno = book.bookno
    GROUP BY student.sid
    ) subquery2
)
ORDER BY subquery1.sid;
\echo

/* part c.iv. */
\echo part c.iv.
SELECT bookno, title
FROM (
    SELECT book.bookno, book.title, book.price, 
        RANK() OVER(ORDER BY -book.price) price_rank
    FROM book
) subquery
WHERE price_rank = 3;
\echo

/* part c.v. */
\echo part c.v.
(
    SELECT DISTINCT book.bookno, book.title
    FROM book
) EXCEPT (
    SELECT DISTINCT book.bookno, book.title
    FROM book, buys
    WHERE book.bookno = buys.bookno AND
        buys.sid IN (
            SELECT DISTINCT student.sid
            FROM student
            WHERE 'CS' NOT IN (
                SELECT major.major
                FROM major
                WHERE student.sid = major.sid
            )
        )
);
\echo

/* part c.vi. */
\echo part c.vi.
SELECT DISTINCT student.sid, student.sname
FROM student, buys buys1
WHERE student.sid = buys1.sid AND
    buys1.bookno NOT IN (
        SELECT subquery1.bookno
        FROM (
            SELECT buys.bookno, COUNT(buys.sid) freq
            FROM buys, major
            WHERE buys.sid = major.sid AND
                major.major = 'CS'
            GROUP BY buys.bookno
            ORDER BY buys.bookno
        ) subquery1
        WHERE subquery1.freq > 1
)
ORDER BY student.sid;
\echo

/* part c.vii. */
\echo part c.vii.
SELECT DISTINCT subquery.sid, subquery.bookno
FROM (
    SELECT DISTINCT 
        buys.sid, 
        book.price,
        book.bookno,
        AVG(book.price) OVER (PARTITION BY buys.sid) AS average_spent
    FROM buys, book
    WHERE buys.bookno = book.bookno
) subquery
WHERE subquery.price < subquery.average_spent
ORDER BY subquery.sid, subquery.bookno;
\echo

/* part c.viii. */
\echo part c.viii.
SELECT DISTINCT subquery1.sid, subquery2.sid
FROM 
    (
        SELECT DISTINCT 
            student.sid, 
            major.major, 
            COUNT(book.bookno) AS books_bought
        FROM major, student, buys, book
        WHERE major.sid = student.sid AND
            student.sid = buys.sid AND
            buys.bookno = book.bookno
        GROUP BY(student.sid, major.major)
        ORDER BY student.sid
    ) subquery1, 
    (
        SELECT DISTINCT 
            student.sid, 
            major.major, 
            COUNT(book.bookno) AS books_bought
        FROM major, student, buys, book
        WHERE major.sid = student.sid AND
            student.sid = buys.sid AND
            buys.bookno = book.bookno
        GROUP BY(student.sid, major.major)
        ORDER BY student.sid
    ) subquery2
WHERE subquery1.sid <> subquery2.sid AND
    subquery1.major = subquery2.major AND
    subquery1.books_bought = subquery2.books_bought;
\echo

/* part c.ix. */
\echo part c.ix.
SELECT COUNT(*) 
FROM (
    SELECT subquery3.sid1, subquery3.sid2, MAX(subquery3.n) AS n
    FROM (
        (
            SELECT subquery2.sid1, subquery2.sid2, COUNT(subquery2.bookno) AS n
            FROM (
                SELECT DISTINCT 
                    student1.sid AS sid1, 
                    student2.sid AS sid2, 
                    book.bookno
                FROM 
                    student student1, student student2, 
                    major major1, major major2, 
                    buys, 
                    book
                WHERE student1.sid <> student2.sid AND
                    student1.sid = major1.sid AND
                    student2.sid = major2.sid AND
                    major1.major = major2.major AND
                    student1.sid = buys.sid AND
                    buys.bookno = book.bookno AND
                    book.bookno NOT IN (
                        SELECT subquery1.bookno 
                        FROM booksBoughtByStudent(student2.sid) subquery1
                    )
                ORDER BY student1.sid, student2.sid, book.bookno
            ) subquery2
            GROUP BY (subquery2.sid1, subquery2.sid2)
        ) UNION (
            SELECT DISTINCT 
                student1.sid AS sid1, 
                student2.sid AS sid2, 
                0 AS n
            FROM 
                student student1, 
                student student2,
                major major1, major major2, 
                buys,
                book
            WHERE student1.sid <> student2.sid AND
                student1.sid = major1.sid AND
                student2.sid = major2.sid AND
                major1.major = major2.major
        )
    ) subquery3
    GROUP BY (subquery3.sid1, subquery3.sid2)
) x;
\echo

/* part c.x. */
\echo part c.x.
SELECT subquery1.bookno
FROM (
    SELECT buys.bookno, COUNT(*) AS freq
    FROM buys, major
    WHERE buys.sid = major.sid AND
        major.major = 'CS'
    GROUP BY buys.bookno
) subquery1
WHERE subquery1.freq = -1 + (
    SELECT COUNT(*)
    FROM major
    WHERE major.major = 'CS'
);
\echo

DROP TABLE buys;
DROP TABLE major;
DROP TABLE student;
DROP TABLE book;

/* problem 9 */
\echo problem 9

CREATE TABLE student(
    sid INTEGER PRIMARY KEY, 
    sname VARCHAR(20)
);
CREATE TABLE course(
    cno INTEGER PRIMARY KEY, 
    cname VARCHAR(20), 
    total INTEGER, 
    max INTEGER
);
CREATE TABLE prerequisite(
    cno INTEGER REFERENCES course(cno), 
    prereq INTEGER REFERENCES course(cno)
);
CREATE TABLE hastaken(
    sid INTEGER REFERENCES student(sid), 
    cno INTEGER REFERENCES course(cno)
);
CREATE TABLE enroll(
    sid INTEGER REFERENCES student(sid), 
    cno INTEGER REFERENCES course(cno)
);
CREATE TABLE waitlist(
    sid INTEGER REFERENCES student(sid),
    cno INTEGER REFERENCES course(cno),
    position INTEGER
);

INSERT INTO course(cno, cname, total, max) VALUES (201,'Programming',0,3);
INSERT INTO course(cno, cname, total, max) VALUES (202,'Calculus',0,3);
INSERT INTO course(cno, cname, total, max) VALUES (203,'Probability',0,3);
INSERT INTO course(cno, cname, total, max) VALUES (204,'AI',0,2);
INSERT INTO course(cno, cname, total, max) VALUES (301,'DiscreteMathematics',0,2);
INSERT INTO course(cno, cname, total, max) VALUES (302,'OS',0,2);
INSERT INTO course(cno, cname, total, max) VALUES (303,'Databases',0,2);
INSERT INTO course(cno, cname, total, max) VALUES (401,'DataScience',0,2);
INSERT INTO course(cno, cname, total, max) VALUES (402,'Networks',0,2);
INSERT INTO course(cno, cname, total, max) VALUES (403,'Philosophy',0,2);

INSERT INTO prerequisite(cno, prereq) VALUES (301,201);
INSERT INTO prerequisite(cno, prereq) VALUES (301,202);
INSERT INTO prerequisite(cno, prereq) VALUES (302,201);
INSERT INTO prerequisite(cno, prereq) VALUES (302,202);
INSERT INTO prerequisite(cno, prereq) VALUES (302,203);
INSERT INTO prerequisite(cno, prereq) VALUES (401,301);
INSERT INTO prerequisite(cno, prereq) VALUES (401,204);
INSERT INTO prerequisite(cno, prereq) VALUES (402,302);

INSERT INTO student(sid, sname) VALUES (1,'Jean');
INSERT INTO student(sid, sname) VALUES (2,'Eric');
INSERT INTO student(sid, sname) VALUES (3,'Ahmed');
INSERT INTO student(sid, sname) VALUES (4,'Qin');
INSERT INTO student(sid, sname) VALUES (5,'Filip');
INSERT INTO student(sid, sname) VALUES (6,'Pam');
INSERT INTO student(sid, sname) VALUES (7,'Lisa');

INSERT INTO hastaken(sid, cno) VALUES (1,201);
INSERT INTO hastaken(sid, cno) VALUES (1,202);
INSERT INTO hastaken(sid, cno) VALUES (1,301);
INSERT INTO hastaken(sid, cno) VALUES (2,201);
INSERT INTO hastaken(sid, cno) VALUES (2,202);
INSERT INTO hastaken(sid, cno) VALUES (3,201);
INSERT INTO hastaken(sid, cno) VALUES (4,201);
INSERT INTO hastaken(sid, cno) VALUES (4,202);
INSERT INTO hastaken(sid, cno) VALUES (4,203);
INSERT INTO hastaken(sid, cno) VALUES (4,204);
INSERT INTO hastaken(sid, cno) VALUES (5,201);
INSERT INTO hastaken(sid, cno) VALUES (5,202);
INSERT INTO hastaken(sid, cno) VALUES (5,301);
INSERT INTO hastaken(sid, cno) VALUES (5,204);

SELECT * FROM student;
SELECT * FROM course;
SELECT * FROM prerequisite;
SELECT * FROM hastaken;

/* check if prereqs are fulfilled */
CREATE OR REPLACE FUNCTION check_prereqs() RETURNS TRIGGER AS
$$
BEGIN
    IF EXISTS (
        (
            SELECT prerequisite.prereq
            FROM prerequisite
            WHERE prerequisite.cno = NEW.cno
        ) EXCEPT (
            SELECT hastaken.cno
            FROM hastaken
            WHERE hastaken.sid = NEW.sid
        )
    ) 
    THEN 
        RAISE EXCEPTION 'not all prerequisites fulfilled';
    END IF;
    RETURN NEW;
END;
$$ LANGUAGE 'plpgsql';
CREATE TRIGGER check_prereqs_trigger
    BEFORE INSERT ON enroll
    FOR EACH ROW
    EXECUTE PROCEDURE check_prereqs();

/* check if max class size is exceeded */
/* if full, don't add to enroll */
CREATE OR REPLACE FUNCTION check_class_size() RETURNS TRIGGER AS
$$
BEGIN
    UPDATE course SET total = total + 1 WHERE cno = NEW.cno;
    IF (SELECT course.total > course.max FROM course WHERE course.cno = NEW.cno)
    THEN
        RAISE NOTICE 'class is full, not enrolled';
        DELETE FROM enroll WHERE enroll.sid = NEW.sid AND enroll.cno = NEW.cno;
        RAISE NOTICE 'adding to waitlist';
        INSERT INTO waitlist (sid, cno, position) VALUES
            (NEW.sid, NEW.cno, (SELECT COUNT(position) FROM waitlist) + 1);
    END IF;
    RETURN NEW;
END;
$$ LANGUAGE 'plpgsql';
CREATE TRIGGER check_class_size_trigger
    AFTER INSERT ON enroll
    FOR EACH ROW
    EXECUTE PROCEDURE check_class_size();

/* update waitlist when someone removes themselves */
CREATE OR REPLACE FUNCTION update_waitlist() RETURNS TRIGGER AS
$$
BEGIN
    UPDATE waitlist 
    SET position = position - 1 
    WHERE position > OLD.position AND cno = OLD.cno;
    RETURN OLD;
END;
$$ LANGUAGE 'plpgsql';
CREATE TRIGGER update_waitlist_trigger
    AFTER DELETE ON waitlist
    FOR EACH ROW
    EXECUTE PROCEDURE update_waitlist();

/* update waitlist when someone drops course */
/* and update course total */
CREATE OR REPLACE FUNCTION update_waitlist_when_drop() RETURNS TRIGGER AS
$$
BEGIN
    IF (
        (EXISTS (SELECT * FROM waitlist WHERE waitlist.cno = OLD.cno)) AND
        (SELECT course.max >= course.total FROM course WHERE cno = OLD.cno)
    )
    THEN 
        UPDATE course SET total = total - 1 WHERE cno = OLD.cno;
        RAISE NOTICE 'moving waitlisted student to course';
        INSERT INTO enroll VALUES (
            (
                SELECT sid 
                FROM waitlist 
                WHERE waitlist.position = 1 AND waitlist.cno = OLD.cno
            ), 
            (
                SELECT cno 
                FROM waitlist 
                WHERE waitlist.position = 1 AND waitlist.cno = OLD.cno
            )
        ); 
        DELETE FROM waitlist WHERE position = 1 AND cno = OLD.cno;
    ELSE
        UPDATE course
        SET total = total - 1
        WHERE cno = OLD.cno;
    END IF;
    RETURN OLD;
END;
$$ LANGUAGE 'plpgsql';
CREATE TRIGGER update_waitlist_when_drop_trigger
    AFTER DELETE ON enroll
    FOR EACH ROW
    EXECUTE PROCEDURE update_waitlist_when_drop();

\echo

/* cleanup */

\echo cleaning up ...
\echo
\echo connecting to postgres
\connect postgres;
\echo
\echo dropping database "hw3"
DROP DATABASE hw3;
