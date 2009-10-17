
module Database.Abstract.Types where


    data QuoteStyle = PgSQLStyle | MySQLStyle | SQLiteStyle


    type TableName = String


    type ColumnName = String


    -- SELECTs are selectable values too...
    data SQL =
        Literal String
        | NULL
        | Column ColumnName
        | Table TableName
        | QualifiedColumn TableName ColumnName
        | Function String [SQL]
        | Select [SQL] SQL Criterion
        deriving (Eq)


    data Criterion =
        RawSQL String
        | Compare Comparator SQL SQL
        | Not Criterion
        | IsNull SQL
        | In SQL [SQL]
        | And [Criterion]
        | Or [Criterion]
        deriving (Eq)


    data Comparator =
        Equal | Less | Greater | LessEqual | GreaterEqual | Like
        deriving (Eq)
