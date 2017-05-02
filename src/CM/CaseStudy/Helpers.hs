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

tupleToDate :: (Word, Word, Word) -> Date
tupleToDate (d, m, y) = Date { dDay = d, dMonth = m, dYear = y }

tupleToDateTime :: (Word, Word, Word, Word, Word, Word) -> DateTime
tupleToDateTime (d, m, y, h, i, s) = DateTime { dtDay = d, dtMonth = m, dtYear = y
                                              , dtHour = h, dtMinute = i, dtSecond = s }

data Address = Address { addressStreet   :: String
                       , addressCity     :: String
                       , addressPostcode :: String
                       , addressCountry  :: String
                       } deriving (Show, Read, Eq)

tupleToAddress :: (String, String, String, String) -> Address
tupleToAddress (s, c, p, n) = Address { addressStreet = s, addressCity = c, addressPostcode = p, addressCountry = n}
