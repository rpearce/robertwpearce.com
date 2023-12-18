{-# LANGUAGE LambdaCase #-}

module Text.HTML.TagSoup.Compressor (compress) where

--------------------------------------------------------------------------------
import qualified Data.Set as Set
import qualified Text.HTML.TagSoup as TS

--------------------------------------------------------------------------------
{- | Compresses TagSoup HTML strings by removing excess, non-significant
     whitespace and HTML comments (optional).
__Examples:__
@
compress TODO
-- "<RESULT>"
compress TODO
-- "<RESULT>"
@
-}
compress :: [TS.Tag String] -> [TS.Tag String]
compress = go Set.empty
  where
    go :: Set.Set String -> [TS.Tag String] -> [TS.Tag String]
    go stack =
      \case [] -> []
            -- Removes comments by not prepending the tag
            -- and, instead, continuing on with the other tags
            ((TS.TagComment _str):rest) ->
              go stack rest

            -- When we find an open tag, like `<div>`, prepend it
            -- and continue through the rest of the tags while
            -- keeping a separate stack of what elements a given
            -- tag is currently "inside"
            (tag@(TS.TagOpen name _attrs):rest) ->
              tag : go (Set.insert name stack) rest

            -- When we find a closing tag, like `</div>`, prepend it
            -- it and continue through the rest of the tags, making
            -- sure to remove it from our stack of currently opened
            -- elements
            (tag@(TS.TagClose name):rest) ->
              tag : go (Set.delete name stack) rest

            -- When a text/string tag is encountered, if it has
            -- significant whitespace that should be preserved,
            -- then prepend it without change; otherwise, clean up
            -- the whitespace, and prepend it
            (tag@(TS.TagText _str):rest)
              | hasSignificantWhitespace stack -> tag : go stack rest
              | otherwise -> fmap cleanWhitespace tag : go stack rest

            -- If none of the above match, then this is unexpected,
            -- so we should prepend the tag without change
            (tag:rest) ->
              tag : go stack rest

    -- Whitespace-sensitive content that shouldn't be compressed
    hasSignificantWhitespace :: Set.Set String -> Bool
    hasSignificantWhitespace stack =
      any (`Set.member` stack) content
      where
        content = [ "pre", "textarea" ]
        --content = [ "pre", "script", "textarea" ] -- @TODO: make `script` optional

    cleanWhitespace :: String -> String
    cleanWhitespace " " = " "
    cleanWhitespace str = cleanSurroundingWhitespace str (cleanHtmlWhitespace str)
      where
        -- Tests for the following:
        --   ' '  (space)
        --   '\f' (form feed)
        --   '\n' (newline [line feed])
        --   '\r' (carriage return)
        --   '\v' (vertical tab)
        isSpaceOrNewLineIsh :: Char -> Bool
        isSpaceOrNewLineIsh = (`elem` (" \f\n\r\v" :: String))

        -- Strips out newlines, spaces, etc
        cleanHtmlWhitespace :: String -> String
        cleanHtmlWhitespace = unwords . words'
          where
            -- Alternate `words` function that uses a different
            -- predicate than `isSpace` in order to avoid dropping
            -- certain types of spaces.
            -- https://hackage.haskell.org/package/base-4.17.0.0/docs/src/Data.OldList.html#words
            words' :: String -> [String]
            words' s = case dropWhile isSpaceOrNewLineIsh s of
              "" -> []
              s' -> w : words' s''
                    where (w, s'') =
                           break isSpaceOrNewLineIsh s'

        -- Clean the whitespace while preserving
        -- single leading and trailing whitespace
        -- characters when it makes sense
        cleanSurroundingWhitespace :: String -> String -> String
        cleanSurroundingWhitespace _originalStr "" = ""
        cleanSurroundingWhitespace originalStr trimmedStr =
          leadingStr ++ trimmedStr ++ trailingStr
          where
            leadingStr  = keepSpaceWhen head originalStr
            trailingStr = keepSpaceWhen last originalStr

        -- Determine when to keep a space based on a
        -- string and a function that returns a character
        -- within that string
        keepSpaceWhen :: ([Char] -> Char) -> String -> String
        keepSpaceWhen _fn ""  = ""
        keepSpaceWhen fn originalStr
          | (isSpaceOrNewLineIsh . fn) originalStr = " "
          | otherwise = ""
