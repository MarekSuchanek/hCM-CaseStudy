module CM.CaseStudy.Instances.Manual where

import CM.CaseStudy.Model
import CM.CaseStudy.Helpers

personBob = Person { personalId = "X18541488"
                   , personBirth = tupleToDate (1, 9, 1985)
                   , personFirstname = "Bob"
                   , personLastname = "Tyler"
                   , personHome = tupleToAddress ("Silver rd. 10", "Moolywood", "77564", "United Testing States")
                   , personGender = Male
                 }
subjectBob = SubjectPerson personBob

personAlice = Person { personalId = "E181651238"
                     , personBirth = tupleToDate (24, 12, 1987)
                     , personFirstname = "Alice"
                     , personLastname = "Nelson"
                     , personHome = tupleToAddress ("Technical st. 7", "Dummypolis", "17486", "United Testing States")
                     , personGender = Female
                     }
employeeAlice = Employee personAlice

companyACME = Company { companyName = "ACME"
                      , companyId = "US1218416551"
                      , companyScope = "SW development"
                      , companyForm = "unltd."
                      , companyHome = tupleToAddress ("Lombard st. 654", "Dummypolis", "17466", "United Testing States")
                      }
subjectACME = SubjectCompany companyACME

bobsAccount = CustomerAccount { accountId = 2
                              , accountFounded = tupleToDateTime (1, 4, 2017, 12, 00, 50)
                              }
acmesAccount = CustomerAccount { accountId = 2
                               , accountFounded = tupleToDateTime (23, 2, 2017, 12, 07, 47)
                               }
bobHasAccount =  AccountOwnership { owner = subjectBob
                                  , itsAccount = bobsAccount
                                  }
acmeHasAccount =  AccountOwnership { owner = subjectACME
                                   , itsAccount = acmesAccount
                                   }

goldClass = CustomerClass { customerClassName = "Gold"
                          , customerClassDiscount = 25
                          }
silverClass = CustomerClass { customerClassName = "Silver"
                            , customerClassDiscount = 10
                            }
bronzeClass = CustomerClass { customerClassName = "Bronze"
                            , customerClassDiscount = 5
                            }
acmeIsGold = BelongsToCustomerClass { accountMember = acmesAccount
                                    , itsCustomerClass = goldClass
                                    }
bobIsBronze = BelongsToCustomerClass { accountMember = bobsAccount
                                     , itsCustomerClass = bronzeClass
                                     }

bobsEmployment = IsEmployeeOf { employer = companyACME
                              , employee = personBob
                              }

onlyBranch = BranchOffice { branchOfficeAddress = tupleToAddress ("Rental st. 10", "Dummypolis", "17476", "United Testing States") }

carBMW1 = Car { carColor = "red"
              , carVIN = "STXUFBAK654375424"
              , carNumberPlate = "ABC 5487"
              , carYearManufactured = 2009
              }
carBMW2 = Car { carColor = "white"
              , carVIN = "STWJKMNW22366624"
              , carNumberPlate = "CAD 7887"
              , carYearManufactured = 2008
              }
carCamaro = Car { carColor = "black"
                , carVIN = "STWJKMNW22366624"
                , carNumberPlate = "FTW 0000"
                , carYearManufactured = 1969
                }

modelBMW = CarModel { carModelType = "sedan"
                    , carModelName = "BMW 320 d"
                    , carModelMaxSpeed = 221
                    , carModelNumberOfDoors = 5
                    }
modelCamaro = CarModel { carModelType = "sedan"
                       , carModelName = "Chevrolet Camaro SS 396/375HP"
                       , carModelMaxSpeed = 250
                       , carModelNumberOfDoors = 3
                       }
bmw1IsBMW = CarInstanceOfModel { carInstance = carBMW1, itsModel = modelBMW }
bmw2IsBMW = CarInstanceOfModel { carInstance = carBMW2, itsModel = modelBMW }
camaroIsCamaro = CarInstanceOfModel { carInstance = carCamaro, itsModel = modelCamaro }

luxuryCars = CarClass { carClassDayPrice = 90.00
                      , carClassName = "Luxury"
                      }
oldiesCars = CarClass { carClassDayPrice = 175.00
                      , carClassName = "Oldies"
                      }
bmwIsLuxury = BelongsToCarClass { carMember = modelBMW, itsCarClass = luxuryCars }
camaroIsOld = BelongsToCarClass { carMember = modelCamaro, itsCarClass = oldiesCars }

bobsRental = RentalContract { rentalContractSince = tupleToDateTime (1, 5, 2017, 12, 00, 00)
                            , rentalContractUntil = Nothing
                            }
acmesRental = RentalContract { rentalContractSince = tupleToDateTime (17, 4, 2017, 12, 00, 00)
                             , rentalContractUntil = Just $ tupleToDateTime (21, 4, 2017, 18, 00, 00)
                             }
bobRentsACar = Rental { contract = bobsRental
                      , renter = bobsAccount
                      , lessor = employeeAlice
                      , rentedCar = carCamaro
                      , rentedFrom = onlyBranch
                      , rentedTo = onlyBranch
                      }
acmeRentsACar = Rental { contract = acmesRental
                       , renter = acmesAccount
                       , lessor = employeeAlice
                       , rentedCar = carBMW1
                       , rentedFrom = onlyBranch
                       , rentedTo = onlyBranch
                       }

model = CarRentalModel { crmEPeople = [personBob, personAlice]
                       , crmECompanies = [companyACME]
                       , crmESubjects = [subjectBob, subjectACME]
                       , crmEEmployees = [employeeAlice]
                       , crmEAccounts = [bobsAccount, acmesAccount]
                       , crmECClasses = [bronzeClass, silverClass, goldClass]
                       , crmEContracts = [bobsRental, acmesRental]
                       , crmEBranches = [onlyBranch]
                       , crmECars = [carBMW1, carBMW2, carCamaro]
                       , crmECarModels = [modelBMW, modelCamaro]
                       , crmECarClasses = [luxuryCars, oldiesCars]
                       , crmRRental = [acmeRentsACar, bobRentsACar]
                       , crmREmploying = [bobsEmployment]
                       , crmRAccMember = [acmeIsGold, bobIsBronze]
                       , crmRCarMember = [bmwIsLuxury, camaroIsOld]
                       , crmRCarModel = [bmw1IsBMW, bmw2IsBMW, camaroIsCamaro]
                       , crmRAccOwner = [bobHasAccount, acmeHasAccount]
                       }
