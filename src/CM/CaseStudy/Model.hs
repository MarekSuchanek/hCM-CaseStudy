module CM.CaseStudy.Model where

import Data.Dates

data Address = Address {
  addressStreet :: String,
  addressCity :: String,
  addressPostcode :: String,
  addressCountry :: String
}

data Subject = Company {
  companyName  :: String,
  companyId    :: String,
  companyScope :: String,
  companyForm  :: String,
  companyHome  :: Address
} | Person {
  personalId   :: String,
  personBirth  :: DateTime,
  personFirstname :: String,
  personLastname :: String,
  personHome :: Address
}

data CustomerAccount = CustomerAccount
data CustomerClass = CustomerClass {
  customerClassDiscount :: Int,
  customerClassName     :: String
}
data RentalContract = RentalContract {
  rentalContractSince :: DateTime,
  rentalContractUntil :: DateTime
}
data BranchOffice = BranchOffice {
  branchOfficeAddress :: Address
}
data Car = Car {
  carColor :: String,
  carVIN :: String,
  carNumberPlate :: String,
  carYearManufactured :: Int
}
data CarModel = CarModel {
  carModelType :: String,
  carModelName :: String,
  carModelMaxSpeed :: Int,
  carModelNumberOfDoors :: Int
}
data CarClass = CarClass {
  carClassDayPrice :: Double,
  carClassName :: String
}
