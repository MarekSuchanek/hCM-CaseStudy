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
                                     , rentalContractUntil :: Maybe DateTime
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
                                     , crmESubjects   :: [Subject]
                                     , crmEEmployees  :: [Employee]
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

instance ConceptualModel CarRentalModel where
  cmodelElements model = (map (toMeta model) $ crmEPeople model)
                      ++ (map (toMeta model) $ crmECompanies model)
                      ++ (map (toMeta model) $ crmESubjects model)
                      ++ (map (toMeta model) $ crmEEmployees model)
                      ++ (map (toMeta model) $ crmEAccounts model)
                      ++ (map (toMeta model) $ crmECClasses model)
                      ++ (map (toMeta model) $ crmEContracts model)
                      ++ (map (toMeta model) $ crmEBranches model)
                      ++ (map (toMeta model) $ crmECars model)
                      ++ (map (toMeta model) $ crmECarModels model)
                      ++ (map (toMeta model) $ crmECarClasses model)
                      ++ (map (toMeta model) $ crmRRental model)
                      ++ (map (toMeta model) $ crmREmploying model)
                      ++ (map (toMeta model) $ crmRAccMember model)
                      ++ (map (toMeta model) $ crmRCarMember model)
                      ++ (map (toMeta model) $ crmRCarModel model)
                      ++ (map (toMeta model) $ crmRAccOwner model)

--------------------------------------------------------------------------------
-- STEP 4: CDM = Compiler driven modeling (GHC guides thru the process)
--------------------------------------------------------------------------------

-- 4.1 Conceptual Model is also element
instance CMElement CarRentalModel where
  toMeta = toMetaModel

-- 4.2 Entites are elements and entities
instance CMElement Person where
  toMeta = toMetaEntity
instance CMElement Company where
  toMeta = toMetaEntity
instance CMElement Subject where
  toMeta m (SubjectPerson  p) = toMetaEntity m p
  toMeta m (SubjectCompany c) = toMetaEntity m c
instance CMElement Employee where
  toMeta m (Employee p) = toMetaEntity m p
instance CMElement CustomerAccount where
  toMeta = toMetaEntity
instance CMElement CustomerClass where
  toMeta = toMetaEntity
instance CMElement RentalContract where
  toMeta = toMetaEntity
instance CMElement BranchOffice where
  toMeta = toMetaEntity
instance CMElement Car where
  toMeta = toMetaEntity
instance CMElement CarModel where
  toMeta = toMetaEntity
instance CMElement CarClass where
  toMeta = toMetaEntity
instance CMElement Rental where
  toMeta = toMetaRelationship
instance CMElement IsEmployeeOf where
  toMeta = toMetaRelationship
instance CMElement BelongsToCustomerClass where
  toMeta = toMetaRelationship
instance CMElement BelongsToCarClass where
  toMeta = toMetaRelationship
instance CMElement CarInstanceOfModel where
  toMeta = toMetaRelationship
instance CMElement AccountOwnership where
  toMeta = toMetaRelationship

instance Entity Subject where
  entityAttributes _ = []
instance Entity Person where
  entitySuperNames _ = ["Subject"]
  entityAttributes Person {..} = map tupleToAttribute
    [ ("Personal ID", "String", personalId)
    , ("Birthdate", "Date", stdDateFormat personBirth)
    , ("Firstname", "String", personFirstname)
    , ("Lastname", "String", personLastname)
    , ("Home", "Address", stdEnveloperAddressFormat $ personHome)
    , ("Gender", "Gender", show personGender)
    ]
instance Entity Company where
  entitySuperNames _ = ["Subject"]
  entityAttributes Company {..} = map tupleToAttribute
    [ ("Company ID", "String", companyId)
    , ("Name", "String", companyName)
    , ("Scope", "String", companyScope)
    , ("Form", "String", companyForm)
    , ("Home", "Address", stdEnveloperAddressFormat companyHome)
    ]
instance Entity Employee where
  entitySuperNames _ = ["Person"]
  entityAttributes _ = []
instance Entity CustomerAccount where
  entityAttributes CustomerAccount {..} = map tupleToAttribute
    [ ("Account ID", "Int", show accountId)
    , ("Founded", "DateTime", stdDateTimeFormat accountFounded)
    ]
instance Entity CustomerClass where
  entityAttributes CustomerClass {..} = map tupleToAttribute
    [ ("Name", "String", customerClassName)
    , ("Discount", "Int", show customerClassDiscount ++ "%")
    ]
instance Entity RentalContract where
  entityAttributes RentalContract {..} = map tupleToAttribute
    [ ("Since", "DateTime", stdDateTimeFormat rentalContractSince)
    , ("Until", "DateTime", maybe "N/A" stdDateTimeFormat rentalContractUntil)
    ]
instance Entity BranchOffice where
  entityAttributes BranchOffice {..} = map tupleToAttribute
    [ ("Address", "Address", stdEnveloperAddressFormat branchOfficeAddress)
    ]
instance Entity Car where
  entityAttributes Car {..} = map tupleToAttribute
    [ ("VIN", "String", carVIN)
    , ("Numberplate", "String", carNumberPlate)
    , ("Year (manuf.)", "Int", show carYearManufactured)
    , ("Color", "String", carColor)
    ]
instance Entity CarModel where
  entityAttributes CarModel {..} = map tupleToAttribute
    [ ("Name", "String", carModelName)
    , ("Type", "String", carModelType)
    , ("Max. speed", "Int", show carModelMaxSpeed)
    , ("#Doors", "Int", show carModelNumberOfDoors)
    ]
instance Entity CarClass where
  entityAttributes CarClass {..} = map tupleToAttribute
    [ ("Name", "String", carClassName)
    , ("Price (per day)", "Double", show carClassDayPrice)
    ]

instance Relationship Rental where
  relationshipParticipations Rental {..} = map tupleToParticipation
    [ ("contract", "RentalContract", identifier contract, Mandatory Unique)
    , ("renter", "CustomerAccount", identifier renter, Optional Unlimited)
    , ("lessor", "Employee", identifier lessor,  Optional Unlimited)
    , ("rentedCar", "Car", identifier rentedCar,  Optional Unlimited)
    , ("rentedFrom", "BranchOffice", identifier rentedFrom,  Optional Unlimited)
    , ("rentedTo", "BranchOffice", identifier rentedTo,  Optional Unlimited)
    ]
instance Relationship IsEmployeeOf where
  relationshipParticipations IsEmployeeOf {..} = map tupleToParticipation
    [ ("employer", "Company", identifier employer, Optional Unlimited)
    , ("employee", "Person", identifier employee, Optional Unlimited)
    ]
instance Relationship BelongsToCustomerClass where
  relationshipParticipations BelongsToCustomerClass {..} = map tupleToParticipation
    [ ("customer (account)", "CustomerAccount", identifier accountMember, Optional Unique)
    , ("class", "CustomerClass", identifier itsCustomerClass, Optional Unlimited)
    ]
instance Relationship BelongsToCarClass where
  relationshipParticipations BelongsToCarClass {..} = map tupleToParticipation
    [ ("car", "CarModel", identifier carMember, Mandatory Unique)
    , ("class", "CarClass", identifier itsCarClass, Optional Unlimited)
    ]
instance Relationship CarInstanceOfModel where
  relationshipParticipations CarInstanceOfModel {..} = map tupleToParticipation
    [ ("car", "Car", identifier carInstance, Mandatory Unique)
    , ("model", "CarModel", identifier itsModel, Optional Unlimited)
    ]
instance Relationship AccountOwnership where
  relationshipParticipations AccountOwnership {..} = map tupleToParticipation
    [ ("owner", "Subject", identifier owner, Optional Unique)
    , ("account", "CustomerAccount", identifier itsAccount, Mandatory Unique)
    ]

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

instance Identifiable Subject where
  identifier (SubjectPerson  p) = identifier p
  identifier (SubjectCompany c) = identifier c

instance Identifiable Employee where
  identifier (Employee p) = identifier p
