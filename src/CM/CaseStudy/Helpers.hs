module CM.CaseStudy.Helpers where

data Gender = Male
            | Female
            deriving (Show, Read, Eq)

data Date = Date { dDay :: Word
                 , dMonth :: Word
                 , dYear :: Word
                 } deriving (Show, Read, Eq)

data DateTime = DateTime { dtDay    :: Word
                         , dtMonth  :: Word
                         , dtYear   :: Word
                         , dtHour   :: Word
                         , dtMinute :: Word
                         , dtSecond :: Word
                         } deriving (Show, Read, Eq)
