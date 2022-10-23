module CommonWords where
import           Data.Char                      ( toLower )
import           Prelude                 hiding ( Word(..)
                                                , break
                                                , concat
                                                , lines
                                                , sort
                                                , span
                                                , unlines
                                                , unwods
                                                , words
                                                )

type Text = String
type Word = String

-- commonWords :: Int -> Text -> String
-- commonWords n =
--   concat
--     . map showRun
--     . take n
--     . sortRuns
--     . countRuns
--     . sortWords
--     . words
--     . map toLower
