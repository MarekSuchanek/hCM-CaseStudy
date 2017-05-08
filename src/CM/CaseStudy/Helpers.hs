{-# LANGUAGE RecordWildCards #-}
module CM.CaseStudy.Helpers where

import Data.List

data Gender = Male
            | Female
            deriving (Show, Read, Eq)

data Date = Date { dYear :: Word
                 , dMonth :: Word
                 , dDay :: Word
                 } deriving (Show, Read, Eq, Ord)

data DateTime = DateTime { dtYear   :: Word
                         , dtMonth  :: Word
                         , dtDay    :: Word
                         , dtHour   :: Word
                         , dtMinute :: Word
                         , dtSecond :: Word
                         } deriving (Show, Read, Eq, Ord)

dummyCurrentDate = tupleToDate(1, 5, 2017)

daysOfMonth :: Word -> Word
daysOfMonth m
      | m == 2 = 28
      | m `elem` [1,3,5,7,8,10,12] = 31
      | otherwise = 30

daysBetween :: Date -> Date -> Word
daysBetween d1 d2
   | d1 == d2  = 0
   | d1 <  d2  = -1 * daysBetween d2 d1
   | otherwise = 1 + daysBetween d1 (addDayTo d2)
     where addDayTo Date {..} = if dDay == daysOfMonth dMonth then
                                  if dMonth == 12 then tupleToDate(1, 1, dYear + 1)
                                  else tupleToDate(1, dMonth + 1, dYear)
                                else tupleToDate(dDay + 1, dMonth, dYear)

daysBetweenDt :: DateTime -> DateTime -> Word
daysBetweenDt dt1 dt2 = daysBetween (dateToDateTime dt1) (dateToDateTime dt2)

dateToDateTime :: DateTime -> Date
dateToDateTime DateTime {..} = Date { dYear = dtYear, dMonth = dtMonth, dDay = dtDay }

dateInPast :: Date -> Bool
dateInPast = (< dummyCurrentDate)

stdDateFormat :: Date -> String
stdDateFormat Date {..} = concat [d, "/", m, "/", y]
 where d = show dDay
       m = show dMonth
       y = show dYear

stdDateTimeFormat :: DateTime -> String
stdDateTimeFormat DateTime {..} = concat [d, "/", m, "/", y, " ", h, ":", i, ":", s]
  where d = show dtDay
        m = show dtMonth
        y = show dtYear
        h = show dtHour
        i = if dtMinute > 9 then show dtMinute else '0' : show dtMinute
        s = if dtSecond > 9 then show dtSecond else '0' : show dtSecond

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



stdEnveloperAddressFormat :: Address -> String
stdEnveloperAddressFormat Address {..} = intercalate "\n"
  [addressStreet, addressCity, addressPostcode, addressCountry]

tupleToAddress :: (String, String, String, String) -> Address
tupleToAddress (s, c, p, n) = Address { addressStreet = s, addressCity = c, addressPostcode = p, addressCountry = n}
