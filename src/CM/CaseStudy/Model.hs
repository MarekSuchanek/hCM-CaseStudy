{-# LANGUAGE RecordWildCards #-}
module CM.CaseStudy.Model where

import CM.Metamodel
import CM.CaseStudy.Helpers

--------------------------------------------------------------------------------
-- hCM Case Study: Car rental company
-- Conceptual modeling for developing system of rental evidence
--------------------------------------------------------------------------------
-- STEP 1: Define entities in problem domain as record datatypes
--         with hierarchy/taxonomy as in real world
--------------------------------------------------------------------------------


data Address = Address { addressStreet   :: String
                       , addressCity     :: String
                       , addressPostcode :: String
                       , addressCountry  :: String
                       } deriving (Show, Read, Eq)

data Company = Company { companyName  :: String
                       , companyId    :: String
                       , companyScope :: String
                       , companyForm  :: String
                       , companyHome  :: Address
                       } deriving (Show, Read, Eq)

data Person = Person { personalId      :: String
                     , personBirth     :: Date
                     , personFirstname :: String
                     , personLastname  :: String
                     , personHome      :: Address
                     , personGender    :: Gender
                     } deriving (Show, Read, Eq)

data Subject = SubjectCompany Company
             | SubjectPerson Person
             deriving (Show, Read, Eq)

data Employee = Employee Person
              deriving (Show, Read, Eq)

data CustomerAccount = CustomerAccount { accountId      :: Int
                                       , accountFounded :: DateTime
                                       } deriving (Show, Read, Eq)

data CustomerClass = CustomerClass { customerClassDiscount :: Int
                                   , customerClassName     :: String
                                   } deriving (Show, Read, Eq)

data RentalContract = RentalContract { rentalContractSince :: DateTime
                                     , rentalContractUntil :: DateTime
                                     } deriving (Show, Read, Eq)

data BranchOffice = BranchOffice { branchOfficeAddress :: Address
                                 } deriving (Show, Read, Eq)

data Car = Car { carColor            :: String
               , carVIN              :: String
               , carNumberPlate      :: String
               , carYearManufactured :: Int
               } deriving (Show, Read, Eq)

data CarModel = CarModel { carModelType          :: String
                         , carModelName          :: String
                         , carModelMaxSpeed      :: Int
                         , carModelNumberOfDoors :: Int
                         } deriving (Show, Read, Eq)

data CarClass = CarClass { carClassDayPrice :: Double
                         , carClassName     :: String
                         } deriving (Show, Read, Eq)

--------------------------------------------------------------------------------
-- STEP 2: Define relationships between entities in problem domain as
--         record datatypes connecting participating entities
--------------------------------------------------------------------------------
data Rental = Rental { contract   :: RentalContract
                     , renter     :: CustomerAccount
                     , lessor     :: Employee
                     , rentedCar  :: Car
                     , rentedFrom :: BranchOffice
                     , rentedTo   :: BranchOffice
                     } deriving (Show, Read, Eq)

data IsEmployeeOf = IsEmployeeOf { employer :: Company
                                 , employee :: Person
                                 } deriving (Show, Read, Eq)

data BelongsToCustomerClass = BelongsToCustomerClass { accountMember    :: CustomerAccount
                                                     , itsCustomerClass :: CustomerClass
                                                     } deriving (Show, Read, Eq)

data BelongsToCarClass = BelongsToCarClass { carMember   :: CarModel
                                           , itsCarClass :: CarClass
                                           } deriving (Show, Read, Eq)

data CarInstanceOfModel = CarInstanceOfModel { carInstance :: Car
                                             , itsModel    :: CarModel
                                             } deriving (Show, Read, Eq)

data AccountOwnership = AccountOwnership { owner      :: Subject
                                         , itsAccount :: CustomerAccount
                                         } deriving (Show, Read, Eq)

--------------------------------------------------------------------------------
-- STEP 3: Define conceptual model data structure as instance of type
--         class ConceptualModel
--------------------------------------------------------------------------------

data CarRentalModel = CarRentalModel { crmEPeople     :: [Person]
                                     , crmECompanies  :: [Company]
                                     , crmEAccounts   :: [CustomerAccount]
                                     , crmECClasses   :: [CustomerClass]
                                     , crmEContracts  :: [RentalContract]
                                     , crmEBranches   :: [BranchOffice]
                                     , crmECars       :: [Car]
                                     , crmECarModels  :: [CarModel]
                                     , crmECarClasses :: [CarClass]
                                     , crmRRental     :: [Rental]
                                     , crmREmploying  :: [IsEmployeeOf]
                                     , crmRAccMember  :: [BelongsToCustomerClass]
                                     , crmRCarMember  :: [BelongsToCarClass]
                                     , crmRCarModel   :: [CarInstanceOfModel]
                                     , crmRAccOwner   :: [AccountOwnership]
                                     } deriving (Show, Read, Eq)

crmESubjects :: CarRentalModel -> [Subject]
crmESubjects CarRentalModel {..} = (map SubjectPerson crmEPeople)
                                ++ (map SubjectCompany crmECompanies)

instance ConceptualModel CarRentalModel where
  cmodelElements CarRentalModel {..} = (map toMeta crmEPeople)
                                    ++ (map toMeta crmECompanies)
                                    ++ (map toMeta crmEAccounts)
                                    ++ (map toMeta crmECClasses)
                                    ++ (map toMeta crmEContracts)
                                    ++ (map toMeta crmECars)
                                    ++ (map toMeta crmECarModels)
                                    ++ (map toMeta crmECarClasses)
                                    ++ (map toMeta crmRRental)
                                    ++ (map toMeta crmREmploying)
                                    ++ (map toMeta crmRAccMember)
                                    ++ (map toMeta crmRCarMember)
                                    ++ (map toMeta crmRCarModel)
                                    ++ (map toMeta crmRAccOwner)

--------------------------------------------------------------------------------
-- STEP 4: CDM = Compiler driven modeling (GHC guides thru the process)
--------------------------------------------------------------------------------

-- 4.1 Conceptual Model is also element
instance CMElement CarRentalModel

-- 4.2 Entites are elements and entities
instance CMElement Person
instance CMElement Company
instance CMElement CustomerAccount
instance CMElement CustomerClass
instance CMElement RentalContract
instance CMElement BranchOffice
instance CMElement Car
instance CMElement CarModel
instance CMElement CarClass
instance CMElement Rental
instance CMElement IsEmployeeOf
instance CMElement BelongsToCustomerClass
instance CMElement BelongsToCarClass
instance CMElement CarInstanceOfModel
instance CMElement AccountOwnership


instance Entity Person
instance Entity Company
instance Entity CustomerAccount
instance Entity CustomerClass
instance Entity RentalContract
instance Entity BranchOffice
instance Entity Car
instance Entity CarModel
instance Entity CarClass

instance Relationship Rental
instance Relationship IsEmployeeOf
instance Relationship BelongsToCustomerClass
instance Relationship BelongsToCarClass
instance Relationship CarInstanceOfModel
instance Relationship AccountOwnership

instance Identifiable Person where
  identifier = show . personalId
instance Identifiable Company where
  identifier = show . companyId
instance Identifiable CustomerAccount where
  identifier = show . accountId
instance Identifiable CustomerClass where
  identifier = customerClassName
instance Identifiable RentalContract
instance Identifiable BranchOffice where
  identifier = show . branchOfficeAddress
instance Identifiable Car where
  identifier = carVIN
instance Identifiable CarModel where
  identifier = carModelName
instance Identifiable CarClass where
  identifier = carClassName
instance Identifiable Rental
instance Identifiable IsEmployeeOf
instance Identifiable BelongsToCustomerClass
instance Identifiable BelongsToCarClass
instance Identifiable CarInstanceOfModel
instance Identifiable AccountOwnership
