
module Database.Abstract.Types where


    data QuoteStyle = PgSQLStyle | MySQLStyle | SQLiteStyle


    type TableName = String


    type ColumnName = String


    data SQL =
        NULL | TRUE | FALSE

        | Literal String

        | Everything
        | Column ColumnName
        | Table TableName

        | QualifiedColumn TableName ColumnName
        | QualifiedEverything TableName

        | Function String [SQL]
        | Select (NEL SQL) (NEL SQL) Criterion
        deriving (Eq)


    data Criterion =
        NoWHERE
        | RawSQL String
        | Compare Comparator SQL SQL
        | Not Criterion
        | IsNull SQL
        | In SQL (NEL SQL)
        | And (NEL Criterion)
        | Or (NEL Criterion)
        deriving (Eq)


    data Comparator =
        Equal | Less | Greater | LessEqual | GreaterEqual | Like
        deriving (Eq)


    -- Type for non-empty lists.
    data NEL a = NEL (a, [a]) deriving (Eq)

    -- | Get the content of a 'NEL' as list.
    fromNEL :: NEL a -> [a]
    fromNEL (NEL (x, xs)) = x : xs

    -- | Create a non-empty list with one single element.
    singleNEL :: a -> NEL a
    singleNEL a = NEL (a, [])


    -- | Create a non-empty list from a list. Will crash if the list is empty.
    toNEL :: [a] -> NEL a
    toNEL s = NEL (head s, tail s)
