/* setup */
/* note: insert commands generated in a separate python script */
/* then copy-pasted into this file */

\echo setup
\echo

\echo creating and connecting to database "hw2"
CREATE DATABASE hw2;
\connect hw2;

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

\echo creating table "cites"
CREATE TABLE cites (
    bookno integer REFERENCES book(bookno), 
    citedbookno integer REFERENCES book(bookno)
);
INSERT INTO cites(bookno, citedbookno) VALUES 
    (2012, 2001),
    (2008, 2011),
    (2008, 2012),
    (2001, 2002),
    (2001, 2007),
    (2002, 2003),
    (2003, 2001),
    (2003, 2004),
    (2003, 2002),
    (2010, 2001),
    (2010, 2002),
    (2010, 2003),
    (2010, 2004),
    (2010, 2005),
    (2010, 2006),
    (2010, 2007),
    (2010, 2008),
    (2010, 2009),
    (2010, 2011),
    (2010, 2013),
    (2010, 2014);

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

/* problem 1 */
\echo problem 1
SELECT DISTINCT student.sid, major.major
FROM student, major, book, buys
WHERE student.sid = major.sid AND
    student.sid = buys.sid AND
    book.bookno = buys.bookno AND 
    book.price < 20
ORDER BY student.sid;
\echo

/* problem 2 */
\echo problem 2
SELECT DISTINCT book.bookno, book.title
FROM book
WHERE book.price >= 20 AND book.price <= 40 AND
    book.bookno IN (SELECT citedbookno FROM cites);
\echo 

/* problem 3 */
\echo problem 3
SELECT DISTINCT student.sid, student.sname
FROM student, major, book, buys
WHERE student.sid = major.sid AND
    major.major = 'CS' AND 
    student.sid = buys.sid AND 
    book.bookno IN (
        SELECT cites.citedbookno 
        FROM cites, book book2
        WHERE book.bookno = cites.citedbookno AND 
            book2.price > book.price
    );
\echo

/* problem 4 */
\echo problem 4
SELECT DISTINCT book.bookno, book.title
FROM book, cites, cites cites2 
WHERE book.bookno = cites.citedbookno AND 
    cites.bookno = cites2.citedbookno
ORDER BY book.bookno;
\echo

/* problem 5 */
\echo problem 5
SELECT bookno 
FROM book
WHERE price <= ALL(SELECT price FROM book book2);
\echo 

/* problem 6 */
\echo problem 6
(
    SELECT bookno, title
    FROM book
) EXCEPT (
    SELECT DISTINCT b1.bookno, b1.title
    FROM book b1, book b2
    WHERE b1.price < b2.price
);
\echo

/* problem 7 */
\echo problem 7
SELECT b2.bookno, b2.title
FROM (
    (
        SELECT bookno, title, price
        FROM book
    ) EXCEPT (
        SELECT bookno, title, price
        FROM book
        WHERE price >= ALL(SELECT price FROM book book2)
    )
) AS b2
WHERE b2.price >= ALL(
    (
        SELECT price
        FROM book
    ) EXCEPT (
        SELECT price
        FROM book
        WHERE price >= ALL(SELECT price FROM book book2)
    )
);
\echo

/* problem 8 */
\echo problem 8
(
    SELECT DISTINCT book.bookno, book.price
    FROM book, cites
    WHERE book.bookno = cites.citedbookno AND 
            book.bookno IN (
            SELECT cites2.bookno
            FROM book book2, cites cites2
            WHERE book2.price > 20 AND
                book2.bookno = cites2.citedbookno
        )
) UNION (
    SELECT DISTINCT book.bookno, book.price
    FROM book
    WHERE book.bookno NOT IN (SELECT citedbookno FROM cites)
)
ORDER BY bookno;
\echo 

/* problem 9 */
\echo problem 9
SELECT DISTINCT book.bookno, book.title
FROM book, student, buys, major
WHERE book.bookno = buys.bookno AND 
    buys.sid = student.sid AND 
    student.sid = major.sid
    AND major.major IN ('Biology', 'Psychology')
ORDER BY book.bookno;
\echo

/* problem 10 */
\echo problem 10
SELECT DISTINCT book.bookno, book.title
FROM book, student
WHERE NOT EXISTS(
    SELECT major.major
    FROM major
    WHERE student.sid = major.sid AND
        major.major = 'CS'
)
ORDER BY book.bookno;
\echo

/* problem 11 */
\echo problem 11
(
    SELECT DISTINCT book.bookno
    FROM book
) EXCEPT (
    SELECT DISTINCT book.bookno
    FROM book, buys, student, major
    WHERE book.bookno = buys.bookno AND
        buys.sid = student.sid AND
        student.sid = major.sid AND
        major.major <> 'Biology'
);
\echo

/* problem 12 */
/* 
    books for which there does not exist a dual math/cs major
    who hasn't bought it
*/
\echo problem 12
SELECT DISTINCT book.bookno, book.title
FROM book
WHERE NOT EXISTS (
    SELECT student.sid
    FROM student, major major1, major major2
    WHERE student.sid = major1.sid AND student.sid = major2.sid AND 
        major1.major = 'Math' AND major2.major = 'CS' AND
        student.sid NOT IN (
            SELECT buys.sid
            FROM buys
            WHERE buys.bookno = book.bookno AND 
                buys.sid = student.sid
        )
)
ORDER BY book.bookno;
\echo

/* problem 13 */
\echo problem 13
SELECT DISTINCT student.sid, student.sname
FROM student, buys b, book bb
WHERE student.sid = b.sid AND
    bb.bookno = b.bookno AND
    bb.bookno IN (
        (
            SELECT DISTINCT book.bookno
            FROM book, buys
            WHERE book.bookno = buys.bookno
        ) EXCEPT (
            SELECT DISTINCT book.bookno
            FROM book, buys b1, buys b2
            WHERE b1.bookno = book.bookno AND 
                b2.bookno = book.bookno AND 
                EXISTS (
                    SELECT s1.sid, s2.sid
                    FROM student s1, student s2
                    WHERE s1.sid = b1.sid AND 
                        s2.sid = b2.sid AND
                        s1.sid <> s2.sid AND
                        s1.sid IN (
                            SELECT major.sid 
                            FROM major 
                            WHERE major.major = 'CS'
                        ) AND 
                        s2.sid IN (
                            SELECT major.sid
                            FROM major
                            WHERE major.major = 'CS'
                        )
                )
        )
    )
ORDER BY student.sid;
\echo

/* problem 14 */
\echo problem 14
(
    SELECT DISTINCT student.sid, student.sname
    FROM student
) EXCEPT (
    SELECT DISTINCT s.sid, s.sname
    FROM student s, buys b1, buys b2
    WHERE s.sid = b1.sid AND 
        s.sid = b2.sid AND EXISTS (
            SELECT book1.bookno, book2.bookno
            FROM book book1, book book2
            WHERE book1.bookno <> book2.bookno AND
                book1.bookno = b1.bookno AND
                book2.bookno = b2.bookno AND 
                book1.price > 20 AND
                book2.price > 20
        )
)
ORDER BY sid;
\echo

/* problem 15 */
\echo problem 15
SELECT DISTINCT student.sid, book.bookno
FROM student, buys, book
WHERE student.sid = buys.sid AND
    buys.bookno = book.bookno AND
    book.price <= ALL (
        SELECT book2.price
        FROM buys buys2, book book2
        WHERE book2.bookno = buys2.bookno AND
            buys2.sid = student.sid
    )
ORDER BY student.sid;
\echo

/* problem 16 */
\echo problem 16
SELECT COUNT(*) 
FROM (
    SELECT DISTINCT s1.sid, s2.sid
    FROM student s1, student s2, major m1, major m2
    WHERE s1.sid = m1.sid AND 
        s2.sid = m2.sid AND
        m1.major = m2.major AND 
        s1.sid <> s2.sid AND EXISTS (
            (
                (
                    SELECT book.bookno
                    FROM book, buys
                    WHERE book.bookno = buys.bookno AND 
                        buys.sid = s1.sid
                ) EXCEPT (
                    SELECT book.bookno 
                    FROM book, buys
                    WHERE book.bookno = buys.bookno AND
                        buys.sid = s2.sid
                )
            ) UNION (
                (
                    SELECT book.bookno
                    FROM book, buys
                    WHERE book.bookno = buys.bookno AND 
                        buys.sid = s2.sid
                ) EXCEPT (
                    SELECT book.bookno 
                    FROM book, buys
                    WHERE book.bookno = buys.bookno AND
                        buys.sid = s1.sid
                )
            )
        )
    ORDER BY s1.sid, s2.sid
) x;
\echo

/* problem 17 */ 
\echo problem 17
SELECT COUNT(*) 
FROM (
    SELECT DISTINCT s1.sid, s2.sid, b1.bookno
    FROM student s1, student s2, book b1, buys buy1
    WHERE s1.sid = buy1.sid AND buy1.bookno = b1.bookno AND
        s1.sid <> s2.sid AND 
        b1.bookno NOT IN (
            SELECT b2.bookno
            FROM book b2, buys   buy2
            WHERE s2.sid = buy2.sid AND buy2.bookno = b2.bookno
        )
) x;
\echo
    

/* problem 18 */
\echo problem 18
SELECT COUNT(*) FROM (
    (
        SELECT DISTINCT s1.sid, s2.sid
        FROM student s1, student s2, book b1, book b2, buys buys1, buys buys2
        WHERE s1.sid = buys1.sid AND buys1.bookno = b1.bookno AND
            s2.sid = buys2.sid AND buys2.bookno = b2.bookno AND
            b1.bookno = b2.bookno AND
            s1.sid <> s2.sid
    ) EXCEPT (
        SELECT a.sid1, a.sid2
        FROM (
            SELECT DISTINCT s1.sid AS sid1, s2.sid AS sid2, b1.bookno AS bookno
            FROM student s1, student s2, book b1, book b2, buys buys1, buys buys2
            WHERE s1.sid = buys1.sid AND buys1.bookno = b1.bookno AND
                s2.sid = buys2.sid AND buys2.bookno = b2.bookno AND
                b1.bookno = b2.bookno AND
                s1.sid <> s2.sid
        ) AS a, (
            SELECT DISTINCT s1.sid AS sid1, s2.sid AS sid2, b1.bookno AS bookno
            FROM student s1, student s2, book b1, book b2, buys buys1, buys buys2
            WHERE s1.sid = buys1.sid AND buys1.bookno = b1.bookno AND
                s2.sid = buys2.sid AND buys2.bookno = b2.bookno AND
                b1.bookno = b2.bookno AND
                s1.sid <> s2.sid
        ) AS b
        WHERE a.sid1 = b.sid1 AND
            a.sid2 = b.sid2 AND 
            a.bookno <> b.bookno
    )
) x;
\echo


/* problem 19 */
\echo problem 19
/* exists only one cs student who hasn't purchased this book */
/* 
(books purchased by at least one CS student) \
    (
        (books purchased by all CS students) OR 
        (books that at least two CS students did not purchase)
    ) 
*/
(
    /* books purchased by at least one CS student */
    SELECT DISTINCT book.bookno
    FROM book, buys, student, major
    WHERE book.bookno = buys.bookno AND
        buys.sid = student.sid AND
        student.sid = major.sid AND
        major.major = 'CS'
) EXCEPT (
    /* books that at least two CS students did not purchase */
    SELECT DISTINCT book.bookno
    FROM book
    WHERE EXISTS (
        (
            SELECT s1.sid, s2.sid
            FROM student s1, student s2
            WHERE s1.sid <> s2.sid AND
                s1.sid IN (
                    SELECT m.sid 
                    FROM major m 
                    WHERE m.major = 'CS'
                ) AND
                s2.sid IN (
                    SELECT m.sid 
                    FROM major m 
                    WHERE m.major = 'CS'
                )
        ) INTERSECT (
            SELECT s1.sid, s2.sid
            FROM student s1, student s2
            WHERE s1.sid <> s2.sid AND 
                s1.sid NOT IN (
                    SELECT b1.sid 
                    FROM buys b1 
                    WHERE b1.bookno = book.bookno
                ) AND s2.sid NOT IN (
                    SELECT b1.sid 
                    FROM buys b1 
                    WHERE b1.bookno = book.bookno
                )
        )
    )
) EXCEPT (
    /* books purchased by all CS students */
    SELECT DISTINCT book.bookno
    FROM book
    WHERE NOT EXISTS (
        (
            SELECT major.sid
            FROM major
            WHERE major.major = 'CS'
        ) EXCEPT (
            SELECT buys.sid
            FROM buys
                WHERE buys.bookno = book.bookno
        )
    )
);
\echo

/* cleanup */

\echo cleaning up ...

\echo connecting to postgres
\c postgres;

\echo dropping database "hw2"
DROP DATABASE hw2;
