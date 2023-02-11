import Data.Maybe (fromMaybe)

-- Data types

data Status = Upcoming | Canceled | InProgress | Finished
    deriving (Show, Eq)

data Activity = Activity {
    activityName :: String,
    duration :: Int
}
    deriving (Show, Eq)

data CancellationPolicy = Flexible | Moderate | Strict
    deriving (Show, Eq)

data Trip = Trip {
        destination :: String,
        price :: Double,
        status :: Status,
        activities :: [Activity]
    }
    | TripWithCancellationPolicy {
        destination :: String,
        price :: Double,
        status :: Status,
        activities :: [Activity],
        cancellationPolicy :: CancellationPolicy
    }
    deriving Show

instance Eq Trip where
    a == b =
        destination a == destination b &&
        activities a == activities b &&
        price a == price b

data Customer = Customer {
    customerName :: String,
    trips :: [Trip],
    loyaltyPoints :: Int
}
    deriving Show

data Agency = Agency {
    customers :: [Customer]
}
    deriving Show

data Thresholds = Thresholds {
    moneyThreshold :: Double,
    durationThreshold :: Int,
    loyaltyPointsThreshold :: Int
}
    deriving Show

-- Problem a

freeTripEligibleCustomers :: Agency -> Thresholds -> [Customer]
freeTripEligibleCustomers (Agency []) _ = []
freeTripEligibleCustomers (Agency (customer:remainingCustomers)) thresholds =
    if isEligible customer thresholds then
        customer : freeTripEligibleCustomers (Agency remainingCustomers) thresholds
    else
        freeTripEligibleCustomers (Agency remainingCustomers) thresholds
    where
        isEligible :: Customer -> Thresholds -> Bool
        isEligible customer thresholds =
            spendMoney customer > moneyThreshold thresholds &&
            tripsDurationSum customer > durationThreshold thresholds &&
            loyaltyPoints customer > loyaltyPointsThreshold thresholds &&
            not (hasCancelledTrips customer) &&
            not (hasRepeatedDestination customer)

        spendMoney :: Customer -> Double
        spendMoney customer =
            tripsPricesSum (trips customer)
            where
                tripsPricesSum :: [Trip] -> Double
                tripsPricesSum [] = 0
                tripsPricesSum (trip:remainingTrips) =
                    price trip + tripsPricesSum remainingTrips

        tripsDurationSum :: Customer -> Int
        tripsDurationSum customer =
            tripsDurationSum (trips customer)
            where
                tripsDurationSum :: [Trip] -> Int
                tripsDurationSum [] = 0
                tripsDurationSum (trip:remainingTrips) =
                    (if status trip == Finished then
                        activitiesDurationSum (activities trip)
                    else
                        0)
                     + tripsDurationSum remainingTrips
                    where
                        activitiesDurationSum :: [Activity] -> Int
                        activitiesDurationSum [] = 0
                        activitiesDurationSum (activity:remainingActivities) =
                            duration activity + activitiesDurationSum remainingActivities

        hasCancelledTrips :: Customer -> Bool
        hasCancelledTrips customer =
            hasCancelledTrips (trips customer)
            where
                hasCancelledTrips :: [Trip] -> Bool
                hasCancelledTrips [] = False
                hasCancelledTrips (trip:remainingTrips) =
                    status trip == Canceled || hasCancelledTrips remainingTrips

        hasRepeatedDestination :: Customer -> Bool
        hasRepeatedDestination customer = let
                visitedDestinations :: [String]
                visitedDestinations =
                    findVisitedDestinations (trips customer)
                    where
                        findVisitedDestinations :: [Trip] -> [String]
                        findVisitedDestinations [] = []
                        findVisitedDestinations (trip:remainingTrips) =
                            ([destination trip | status trip == Finished || status trip == InProgress])
                            ++ findVisitedDestinations remainingTrips
            in
                hasRepeatedElements visitedDestinations
                where
                    hasRepeatedElements :: [String] -> Bool
                    hasRepeatedElements [] = False
                    hasRepeatedElements (element:remainingElements) =
                        element `elem` remainingElements || hasRepeatedElements remainingElements

-- Problem b

getRefund :: Agency -> String -> String -> Maybe Double
getRefund agency wantedCustomerName wantedDestination
    | status trip == Finished || status trip == Canceled = Nothing
    | status trip == InProgress = Just 0
    | otherwise = Just refund
    where
        tripAndCustomer :: (Trip, Customer)
        tripAndCustomer =
            findTripAndCustomer (customers agency) wantedCustomerName wantedDestination
            where
                findTripAndCustomer :: [Customer] -> String -> String -> (Trip, Customer)
                findTripAndCustomer [] wantedCustomerName wantedDestination =
                    error
                        ("The agency does not have a trip to " ++
                        wantedDestination ++ " for customer " ++ wantedCustomerName)
                findTripAndCustomer (customer:remainingCustomers) wantedCustomerName wantedDestination =
                    if customerName customer /= wantedCustomerName then
                        findTripAndCustomer remainingCustomers wantedCustomerName wantedDestination
                    else
                        fromMaybe
                            (findTripAndCustomer remainingCustomers wantedCustomerName wantedDestination)
                            (findTripTo customer wantedDestination)
                    where
                        findTripTo :: Customer -> String -> Maybe (Trip, Customer)
                        findTripTo customer wantedDestination =
                            findTripTo (trips customer) wantedDestination
                            where
                                findTripTo :: [Trip] -> String -> Maybe (Trip, Customer)
                                findTripTo [] _ = Nothing
                                findTripTo (trip:remainingTrips) wantedDestination =
                                    if destination trip == wantedDestination then
                                        Just (trip, customer)
                                    else
                                        findTripTo remainingTrips wantedDestination

        trip :: Trip
        trip = fst tripAndCustomer
        
        customer :: Customer
        customer = snd tripAndCustomer

        refund :: Double
        refund =  max (k customer * c trip * price trip - fee) 0
            where
                fee :: Double
                fee = quotient (customers agency) trip * canceledAvg (trips customer) trip

                k :: Customer -> Double
                k customer = fromIntegral (notCancelledTripsCount customer) / fromIntegral (allTripsCount customer)
                    where
                        notCancelledTripsCount :: Customer -> Int
                        notCancelledTripsCount customer =
                            notCancelledTripsCount (trips customer)
                            where
                                notCancelledTripsCount :: [Trip] -> Int
                                notCancelledTripsCount [] = 0
                                notCancelledTripsCount (trip:remainingTrips) =
                                    (if status trip == Canceled then 0 else 1)
                                    + notCancelledTripsCount remainingTrips

                        allTripsCount :: Customer -> Int
                        allTripsCount customer =
                            length $ trips customer

                c :: Trip -> Double
                c (TripWithCancellationPolicy _ _ _ _ Flexible) = 0.7
                c (TripWithCancellationPolicy _ _ _ _ Moderate) = 0.4
                c (TripWithCancellationPolicy _ _ _ _ Strict) = 0.1
                c _ = error "Trip was not in the correct format"

                canceledAvg :: [Trip] -> Trip -> Double
                canceledAvg trips currentTrip =
                    canceledAvg trips currentTrip 0 0
                    where
                        canceledAvg :: [Trip] -> Trip -> Double -> Double -> Double
                        canceledAvg [] _ _ 0 = 0
                        canceledAvg [] _ sum count = sum / count
                        canceledAvg (trip:remainingTrips) currentTrip sum tripsCount =
                            if trip /= currentTrip && status trip == Canceled then
                                canceledAvg remainingTrips currentTrip (sum + price trip) (tripsCount + 1)
                            else
                                canceledAvg remainingTrips currentTrip sum tripsCount

                quotient :: [Customer] -> Trip -> Double
                quotient customers trip =
                    cancelledCount customers trip / signedUpCount customers trip
                    where
                        cancelledCount :: [Customer] -> Trip -> Double
                        cancelledCount [] _ = 0
                        cancelledCount (customer:remainingCustomers) trip =
                            (if hasCancelledTrip customer trip then 1 else 0)
                            + cancelledCount remainingCustomers trip
                            where
                                hasCancelledTrip :: Customer -> Trip -> Bool
                                hasCancelledTrip customer trip =
                                    helper (trips customer) trip
                                    where
                                        helper :: [Trip] -> Trip -> Bool
                                        helper [] _ = False
                                        helper (trip:remainingTrips) wantedTrip =
                                            (trip == wantedTrip && status trip == Canceled) ||
                                            helper remainingTrips wantedTrip


                        signedUpCount :: [Customer] -> Trip -> Double
                        signedUpCount [] _ = 0
                        signedUpCount (customer:remainingCustomers) trip =
                            (if trip `elem` trips customer then 1 else 0)
                            + signedUpCount remainingCustomers trip