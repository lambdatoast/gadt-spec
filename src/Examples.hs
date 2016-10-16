{-# LANGUAGE GADTs #-}
module Examples where

import Prelude hiding (Read)

{- DB operations DSL -}

data Create
data Read
data Update
data Delete

data DBOp o r a where
  DBFind :: DBOp Read r a
  DBCreate :: DBOp Create r a
  Chain  :: (a -> DBOp o r b) -> DBOp o r a -> DBOp o r b
  Map    :: (a -> b) -> DBOp o r a -> DBOp o r b

{- Business-specific modeling -}

newtype Id = Id Int
newtype ErrorMsg = ErrorMsg String

data Tourist
data Agency
data Trip = Trip { tourists :: [Id] }

data RunArg r where
  TouristModel :: RunArg Tourist
  AgencyModel :: RunArg Agency
  TripModel :: RunArg Trip

{- Business-specific DB operations evaluation -}

run :: RunArg r -> DBOp o r a -> a
run = undefined

runM :: Monad m => RunArg r -> DBOp o r a -> m a
runM = undefined

class Monad p => Promise p where
  thenP :: (a -> b) -> p a -> p b
  chainP :: (a -> p b) -> p a -> p b

class Promise p => PromiseEither p where
  thenPE :: (a -> b) -> p (Either err a) -> p (Either err b)
  chainPE :: (a -> p (Either err b)) -> p (Either err a) -> p (Either err b)

runP :: (Monad m, Promise p) => RunArg r -> DBOp o r a -> p (m a)
runP = undefined

runPE :: PromiseEither p => RunArg r -> DBOp o r a -> p (Either err a)
runPE = undefined

{- Business-specific specs -}

_id :: a -> Id
_id = undefined

-- convenience
(.:.) = flip ($)

-- function getAgency(criteria) { return DB.find(criteria) }
getAgency :: Tourist -> DBOp Read Agency Id
getAgency t = DBFind .:. Map _id

createTourist :: Agency -> DBOp Create Tourist Id
createTourist c = DBCreate .:. Map _id

loadTrip :: Id -> DBOp Read Trip (Trip, [DBOp Read Tourist Tourist])
loadTrip i = DBFind .:. Map (\trip -> (trip, tourists trip .:. fmap (const DBFind)))

{-- Programs --}

-- var program = DB.find(x).map(getAgency)
program :: DBOp Read Tourist (DBOp Read Agency Id)
program = DBFind .:. Map getAgency

program2 :: DBOp Read Agency (DBOp Create Tourist Id)
program2 = DBFind .:. Map createTourist

{-- Evaluations --}

r :: Id
r = run TouristModel program .:. run AgencyModel

r2 :: Id
r2 = run AgencyModel program2 .:. run TouristModel

trip :: (Trip, [Tourist])
trip = let (t, tOps) = run TripModel (loadTrip (Id 42))
       in (t, tOps .:. fmap (run TouristModel))

-- For the case where evaluation yields an Either
r3 :: Either ErrorMsg Id
r3 = runM AgencyModel program2 >>= runM TouristModel

-- For the case where evaluation yields some Monad
r4 :: Promise p => p (Either ErrorMsg Id)
r4 = runP AgencyModel program2 .:. chainP (\opE -> opE >>= runP TouristModel)

-- For the case where evaluation yields specifically a Promise of an Either
r5 :: PromiseEither p => p (Either ErrorMsg Id)
r5 = runPE AgencyModel program2 .:. chainPE (runPE TouristModel)
