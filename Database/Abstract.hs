
module Database.Abstract (
        selectSQL,
        insertSQL,
        updateSQL,

        whereSQL,
        quoteSQL,

        module Database.Abstract.Types
    ) where


    import Database.Abstract.Types

    import Data.List
    import Data.Maybe
    import Data.Char


    comparatorSQL _ Equal = "="
    comparatorSQL _ Like = "LIKE"
    comparatorSQL _ Greater = ">"
    comparatorSQL _ Less = "<"
    comparatorSQL _ GreaterEqual = ">="
    comparatorSQL _ LessEqual = "<="


    whereSQL :: QuoteStyle -> Criterion -> Maybe String

    -- Insert a string as-is in the query.
    whereSQL _ (RawSQL sql) = Just sql


    -- Compare two things.
    whereSQL style (Compare comparator left right) =
        Just (
            quoteSQL style left
            ++ " " ++
            comparatorSQL style comparator
            ++ " " ++
            quoteSQL style right
        )


    -- IS NULL.
    whereSQL style (IsNull left) = Just $ quoteSQL style left ++ " IS NULL"


    -- IN (...).
    whereSQL style (In left values) =
        if values == []
            then Nothing
            else Just $ quoteSQL style left ++ " IN (" ++ list ++ ")"
        where
            list = intercalate ", " (map (quoteSQL style) values)


    -- Negate a criterion.
    whereSQL style (Not criterion) =
        case whereSQL style criterion of
            Nothing -> Nothing
            Just sql -> Just $ "NOT (" ++ sql ++ ")"


    -- Combine criteria using AND.
    whereSQL style (And criteria) = combineCriteria style " AND " criteria


    -- Combine criteria using OR.
    whereSQL style (Or criteria) = combineCriteria style " OR " criteria


    combineCriteria style s criteria =
        case (mapMaybe (whereSQL style) criteria) of
            [] -> Nothing
            criteria -> Just $ "(" ++ intercalate s criteria ++ ")"


    valueListSQL style values =
        if values == []
            then quoteSQL style Everything
            else intercalate ", " (map (quoteSQL style) values)


    -- | Generate a SELECT statement.
    -- Joins? Groups? Limits? Orders? Havings? ...
    selectSQL :: QuoteStyle -> [SQL] -> SQL -> Criterion -> String
    selectSQL style values source criterion =
        maybe select ((select ++ " WHERE ") ++) (whereSQL style criterion)
        where
            select =
                "SELECT " ++ intercalate ", " (map (quoteSQL style) values)
                ++
                " FROM " ++ quoteSQL style source


    -- | Generate an INSERT statement.
    insertSQL :: QuoteStyle -> TableName -> [(ColumnName, SQL)] -> String
    insertSQL style table values =
        "INSERT INTO " ++ quoteName style table
        ++
        " (" ++ intercalate ", " (map (quoteName style . fst) values) ++ ") "
        ++
        "VALUES (" ++ valueListSQL style (map (snd) values) ++ ")"


    -- | Generate an UPDATE statement.
    updateSQL :: QuoteStyle -> TableName -> [(ColumnName, SQL)] -> Maybe Criterion -> String
    updateSQL style table values criterion =
        "UPDATE " ++ quoteName style table
        ++
        " SET " ++ intercalate ", " (map (update) values)
        ++
        case criterion of
            Nothing -> ""
            Just criterion -> maybe "" ((++) " WHERE ") (whereSQL style criterion)
        where
            update (column, value) = quoteName style column ++ " = " ++ quoteSQL style value


    quoteLiteral MySQLStyle literal =
        "'" ++ (concatMap translate literal) ++ "'"
        where
            translate '\'' = "\\'"
            translate '\\' = "\\\\"
            translate ch = [ch]


    quoteLiteral _ literal =
        "'" ++ (concatMap translate literal) ++ "'"
        where
            translate '\'' = "''"
            translate ch = [ch]


    quoteName MySQLStyle name = "`" ++ name ++ "`"
    quoteName _ name = "\"" ++ name ++ "\""


    quoteSQL :: QuoteStyle -> SQL -> String

    quoteSQL style (Literal literal) = quoteLiteral style literal

    quoteSQL style (Column column) = quoteName style column

    quoteSQL style (QualifiedColumn table column) =
        quoteName style table ++ "." ++ quoteName style column

    quoteSQL style (Function name p) =
        (map toUpper name) ++ "(" ++ intercalate ", " (map (quoteSQL style) p) ++ ")"

    quoteSQL style (Table name) = quoteName style name

    quoteSQL style (Select values source criterion) =
        "(" ++ selectSQL style values source criterion ++ ")"

    quoteSQL _ TRUE = "TRUE"

    quoteSQL _ FALSE = "FALSE"

    quoteSQL _ NULL = "NULL"

    quoteSQL _ Everything = "*"

    quoteSQL style (QualifiedEverything table) = quoteName style table ++ ".*"
