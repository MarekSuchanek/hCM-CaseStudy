module CM.CaseStudy.Instances.Generate where

import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Control.Monad
import CM.CaseStudy.Model
import CM.CaseStudy.Helpers
import CM.CaseStudy.Instances.Values as Values

--------------------------------------------------------------------------------
-- STEP 1a: Arbitrary for helper types
--------------------------------------------------------------------------------

instance Arbitrary Gender where
  arbitrary = do
    isMale <- arbitrary
    return $ if isMale then Male else Female

daysOfMonth m
          | m == 2 = 28
          | m `elem` [1,3,5,7,8,10,12] = 31
          | otherwise = 30

instance Arbitrary Date where
  arbitrary = do
    y <- choose (1950, 2020)
    m <- choose (1, 12)
    d <- choose (1, daysOfMonth m)
    return Date { dDay = d, dMonth = m, dYear = y }

instance Arbitrary DateTime where
  arbitrary = do
    y <- choose (1950, 2020)
    m <- choose (1, 12)
    d <- choose (1, daysOfMonth m)
    h <- choose (0, 23)
    i <- choose (0, 59)
    s <- choose (0, 59)
    return DateTime { dtDay = d, dtMonth = m, dtYear = y
                    , dtHour = h, dtMinute = i, dtSecond = s}

instance Arbitrary Address where
  arbitrary = do
    street <- elements Values.streets
    streetNum <- choose (1, 2000) :: Gen Word
    city <- elements Values.cities
    postcode <- choose (10000, 90000) :: Gen Word
    country <- elements Values.countries
    return $ tupleToAddress(street ++ " " ++ show streetNum, city, show postcode, country)

--------------------------------------------------------------------------------
-- STEP 1b: Arbitrary for entities
--------------------------------------------------------------------------------

idchars = elements (['A'..'Z'] ++ ['0'..'9'])

instance Arbitrary Person where
  arbitrary = do
    pid <- replicateM 12 idchars
    gender <- arbitrary
    birthdate <- arbitrary
    home <- arbitrary
    firstname <- case gender of
      Male -> elements Values.firstnamesMale
      _    -> elements Values.firstnamesFemale
    lastname <- elements Values.lastnames
    return Person { personalId = pid
                  , personBirth = birthdate
                  , personFirstname = firstname
                  , personLastname = lastname
                  , personHome = home
                  , personGender = gender
                  }

instance Arbitrary Company where
  arbitrary = do
    name <- elements Values.companies
    form <- elements Values.companyForms
    scope <- elements Values.companyScopes
    cid <- replicateM 14 idchars
    home <- arbitrary
    return Company { companyName = name
                   , companyId = cid
                   , companyScope = scope
                   , companyForm = form
                   , companyHome = home
                   }

instance Arbitrary Subject where
  arbitrary = do
    t <- elements ["person", "company"]
    case t of
      "company" -> do
        company <- arbitrary
        return $ SubjectCompany company
      _ -> do
        person <- arbitrary
        return $ SubjectPerson person

instance Arbitrary Employee where
  arbitrary = do
    person <- arbitrary
    return $ Employee person

instance Arbitrary CustomerAccount where
  arbitrary = do
    aid <- choose (1, 9999999)
    dtt <- arbitrary
    return CustomerAccount { accountId = aid
                           , accountFounded = dtt
                           }

instance Arbitrary CustomerClass where
 arbitrary = do
   discount <- choose (0, 50)
   name <- elements Values.cclassNames
   return CustomerClass { customerClassDiscount = discount
                        , customerClassName = name
                        }

instance Arbitrary RentalContract where
  arbitrary = do
    xsince <- arbitrary
    xuntil <- arbitrary
    return RentalContract { rentalContractSince = xsince
                          , rentalContractUntil = xuntil
                          }

instance Arbitrary BranchOffice where
  arbitrary = do
    addr <- arbitrary
    return BranchOffice { branchOfficeAddress = addr }

instance Arbitrary Car where
  arbitrary = do
    color <- elements Values.colors
    vin <- replicateM 17 idchars
    numberplate <- replicateM 7 idchars
    year <- choose (1950, 2020)
    return Car { carColor = color
               , carVIN = vin
               , carNumberPlate = numberplate
               , carYearManufactured = year
               }

instance Arbitrary CarModel where
  arbitrary = do
    cartype <- elements Values.carTypes
    carname <- elements Values.carNames
    speed <- choose (100, 350)
    doors <- choose (3, 10)
    return CarModel { carModelType = cartype
                    , carModelName = carname
                    , carModelMaxSpeed = speed
                    , carModelNumberOfDoors = doors
                    }

instance Arbitrary CarClass where
  arbitrary = do
    name <- elements Values.carClassNames
    price <- choose (10, 10000)
    return CarClass { carClassDayPrice = price
                    , carClassName = name
                    }

--------------------------------------------------------------------------------
-- STEP 2 -> arbitrary model & relationship forming
--           (specify sizing of model here)
--------------------------------------------------------------------------------
unifySubjects :: [Person] -> [Company] -> [Subject]
unifySubjects ps cs = (map SubjectPerson ps) ++ (map SubjectCompany cs)

instance Arbitrary CarRentalModel where
  arbitrary = do
    modelSize <- choose (2, 10)
    people <- replicateM modelSize arbitrary
    companies <- replicateM modelSize arbitrary
    nemployees <- choose (1, length people)
    employees <- replicateM nemployees (elements (map Employee people))
    accounts <- replicateM modelSize arbitrary
    custclasses <- replicateM modelSize arbitrary
    branches <- replicateM modelSize arbitrary
    contracts <- replicateM modelSize arbitrary
    cars <- replicateM modelSize arbitrary
    carmodels <- replicateM modelSize arbitrary
    carclasses <- replicateM modelSize arbitrary
    return CarRentalModel { crmEPeople = people
                          , crmECompanies = companies
                          , crmESubjects = unifySubjects people companies
                          , crmEEmployees = employees
                          , crmEAccounts = accounts
                          , crmECClasses = custclasses
                          , crmEContracts = contracts
                          , crmEBranches = branches
                          , crmECars = cars
                          , crmECarModels = carmodels
                          , crmECarClasses = carclasses
                          , crmRRental = []
                          , crmREmploying = []
                          , crmRAccMember = []
                          , crmRCarMember = []
                          , crmRCarModel = []
                          , crmRAccOwner = []
                          }

--------------------------------------------------------------------------------
-- STEP 3: Validation via generating instances - write some helper functions or
--         use GHCi, function "suchThat" is very helpful to get only valid and
--         only invalid instances of entities/models
--------------------------------------------------------------------------------
