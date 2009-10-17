
import Database.Abstract

import Data.Maybe


main =
    putStrLn $ selectSQL PgSQLStyle columns (Table "person") whereClause

    where
        columns = map q ["id", "forename", "surname", "age"]

        whereClause =
            Or [
                And [
                    In (q "forename") [Literal "john", Literal "jane"],
                    Compare Equal (q "surname") (Literal "doe"),
                    Not $ Or [
                        Compare Less (q "age") (Literal "50"),
                        Compare Greater (q "age") (Literal "100")
                    ],
                    Not $ Compare Like (Function "lower" [q "surname"]) (Literal "%something'with'ticks%")
                ],
                In (q "id") [Select [(q "id")] (Table "person") (Compare Equal (q "forename") (Literal "Horst"))]
            ]

        q = QualifiedColumn "person"


{-
$ ./test | fixsql

SELECT
    "person"."id",
    "person"."forename",
    "person"."surname",
    "person"."age"
FROM
    "person"
WHERE (
    (
        "person"."forename" IN (
            'john',
            'jane'
        )
        AND
        "person"."surname" = 'doe'
        AND
        NOT (
            (
                "person"."age" < '50'
                OR
                "person"."age" > '100'
            )
        )
        AND
        NOT (
            LOWER (
                "person"."surname"
            )
            LIKE '%something''with''ticks%'
        )
    )
    OR
    "person"."id" IN (
        (
            SELECT
                "person"."id"
            FROM
                "person"
            WHERE
                "person"."forename" = 'Horst'
        )
    )
    )
-}
