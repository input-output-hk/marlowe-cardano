# Hasql Dynamic Syntax

AST-based toolkit for constructing Hasql statements dynamically.

## Description

This library is an alternative to https://hackage.haskell.org/package/hasql-dynamic-statements
with stronger type guarantees and better composability. Like
`hasql-dynamic-statements`, its purpose it to create dynamic prepared SQL queries
the structure of which depends on runtime values. `hasql-dynamic-syntax`
provides the following benefits compared to `hasql-dynamic-statements`:

- SQL statements are built using an abstract syntax tree instead of fragments of raw SQL text.
  - This guarantees SQL queries are syntactically valid.
  - It also makes it easier to define reusable query pieces.
- Statement result row types are tracked at the type level, and decoders are
  automatically chosen. This is similar to using `hasql-th`.
- Parameters are allocated in a monadic context, allowing them to be reused
  without sending redundant parameter data via the database connection.

## Usage

Here is the same task accomplished with `hasql-th`, `hasql-dynamic-statements`,
and `hasql-dynamic-syntax`:

```haskell
data Book = Book
  { bookAuthor :: Text
  , bookTitle :: Text
  }

-- With hasql-th:
--
-- Benefits:
--  - Query syntax checked at compile time
--  - Types of parameters and results inferred by quasi quoter.
--  - Very readable, it's pure parsable SQL.
--
-- Drawbacks:
--  - Not composable. Need to write full query twice to change sort order. This
--    gets worse by a factor of (2 ^ n) for n simultaneous query modifications.
--  - Requires the use of TemplateHaskell
getBooksByAuthor :: Bool -> Text -> Statement () [Book]
getBooksByAuthor isAscending author = if isAscending
  dimap (const author) (Vector.toList . fmap (uncurry Book))
    $ if isAscending
        then
          [vectorStatement|
            SELECT book.author :: text, book.title :: text
            FROM book
            WHERE book.author = $1 :: text
            ORDER BY book.title
          |]
        else
          [vectorStatement|
            SELECT book.author :: text, book.title :: text
            FROM book
            WHERE book.author = $1 :: text
            ORDER BY book.title DESC
          |]

-- with hasql-dynamic-snippet
--
-- Benefits:
--  - Composable - query is able to change structure based on runtime values.
--  - Relatively readable.
--  - More control over how results are decoded.
--  - Parameterization is handled implicitly and transparently, as if values
--    were being directly spliced into query.
--
-- Drawbacks:
--  - Query syntax is not checked at compile time.
--  - Still suffers from composability issues - string concatenation isn't generally easy to compose.
--  - Burden is on programmer to get the correct number of rows, their positions,
--    and their types correct when writing decoder.
getBooksByAuthor :: Bool -> Text -> Statement () [Book]
getBooksByAuthor isAscending author =
  dynamicallyParameterized snippet decoder True
  where
    snippet = fold
      [ "SELECT book.author, book.title"
      , "FROM book"
      , "WHERE book.author = "
      , param author
      , "ORDER BY book.title"
      , if isAscending
          then mempty
          else " DESC"
      ]

    decoder = Decoders.rowList $
      Book
        <$> Decoders.column (Decoders.nonNullable Decoders.text)
        <*> Decoders.column (Decoders.nonNullable Decoders.text)

-- with hasql-dynamic-syntax
--
-- Benefits:
--  - Very Composable - query is structurally specified and pieces can be
--    trivially refactored or reused.
--  - Parameterization is handled implicitly and transparently, as if values
--    were being directly spliced into query.
--  - Syntax is enforced at compile time due to the use of a syntax tree.
--
-- Drawbacks:
--  - Much more difficult to read.
--  - Use of data kinds, GATDs, type families makes API non-beginner friendly.
--  - Must be very explicit about the exact structure of the query, makes it
type BookRows =
  '[ '("author", SqlText, NotNull)
   , '("title", SqlText, NotNull)
   ]

bookTable :: Table BookRows
bookTable = singTable "book" Nothing

getBooksByAuthor :: Bool -> Text -> Statement () [Book]
getBooksByAuthor isAscending author =
  buildStatement Book Decoders.rowList do
    authorParam <- param author
    pure $
      simpleSelectPreparableStmt $
        NormalSimpleSelect -- SELECT
          ( NormalTargeting $
              tableColumn @"author" bookTable
                :. tableColumn @"title" bookTable
                :. TargetListNil
          )
          Nothing -- INTO
          (Just $ pure $ toTableRef bookTable) -- FROM
          Nothing -- WHERE
          Nothing -- GROUP BY
          Nothing -- HAVING
          Nothing -- WINDOW
```

Here, the relative benefits and drawbacks of the three approaches are
illustrated. Some factors to consider when choosing an approach are:

- Is the query relatively static? `hasql-th` is probably the better choice,
  even if the query needs to be duplicated as much as 4 times. Beyond that, it
  starts becoming a probable source of bugs as changes to parts of a query must
  be performed multiple times.
- Is the query relatively simple and are the pieces used only in a single
  location? `hasql-dynamic-statements` may prove more straightforward and be
  worth considering.
- Is there a need for reuse of small chunks of queries, or is the SQL codegen
  logic sufficiently complex? `hasql-dynamic-syntax` would be the most robust
  and maintainable choice. A good example of this would be implementing an
  efficient backend for a GraphQL schema.
