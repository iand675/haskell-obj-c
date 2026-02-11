{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INBoatTrip@.
module ObjC.Intents.INBoatTrip
  ( INBoatTrip
  , IsINBoatTrip(..)
  , init_
  , initWithProvider_boatName_boatNumber_tripDuration_departureBoatTerminalLocation_arrivalBoatTerminalLocation
  , provider
  , boatName
  , boatNumber
  , tripDuration
  , departureBoatTerminalLocation
  , arrivalBoatTerminalLocation
  , initSelector
  , initWithProvider_boatName_boatNumber_tripDuration_departureBoatTerminalLocation_arrivalBoatTerminalLocationSelector
  , providerSelector
  , boatNameSelector
  , boatNumberSelector
  , tripDurationSelector
  , departureBoatTerminalLocationSelector
  , arrivalBoatTerminalLocationSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.CoreLocation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsINBoatTrip inBoatTrip => inBoatTrip -> IO (Id INBoatTrip)
init_ inBoatTrip  =
  sendMsg inBoatTrip (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithProvider:boatName:boatNumber:tripDuration:departureBoatTerminalLocation:arrivalBoatTerminalLocation:@
initWithProvider_boatName_boatNumber_tripDuration_departureBoatTerminalLocation_arrivalBoatTerminalLocation :: (IsINBoatTrip inBoatTrip, IsNSString provider, IsNSString boatName, IsNSString boatNumber, IsINDateComponentsRange tripDuration, IsCLPlacemark departureBoatTerminalLocation, IsCLPlacemark arrivalBoatTerminalLocation) => inBoatTrip -> provider -> boatName -> boatNumber -> tripDuration -> departureBoatTerminalLocation -> arrivalBoatTerminalLocation -> IO (Id INBoatTrip)
initWithProvider_boatName_boatNumber_tripDuration_departureBoatTerminalLocation_arrivalBoatTerminalLocation inBoatTrip  provider boatName boatNumber tripDuration departureBoatTerminalLocation arrivalBoatTerminalLocation =
withObjCPtr provider $ \raw_provider ->
  withObjCPtr boatName $ \raw_boatName ->
    withObjCPtr boatNumber $ \raw_boatNumber ->
      withObjCPtr tripDuration $ \raw_tripDuration ->
        withObjCPtr departureBoatTerminalLocation $ \raw_departureBoatTerminalLocation ->
          withObjCPtr arrivalBoatTerminalLocation $ \raw_arrivalBoatTerminalLocation ->
              sendMsg inBoatTrip (mkSelector "initWithProvider:boatName:boatNumber:tripDuration:departureBoatTerminalLocation:arrivalBoatTerminalLocation:") (retPtr retVoid) [argPtr (castPtr raw_provider :: Ptr ()), argPtr (castPtr raw_boatName :: Ptr ()), argPtr (castPtr raw_boatNumber :: Ptr ()), argPtr (castPtr raw_tripDuration :: Ptr ()), argPtr (castPtr raw_departureBoatTerminalLocation :: Ptr ()), argPtr (castPtr raw_arrivalBoatTerminalLocation :: Ptr ())] >>= ownedObject . castPtr

-- | @- provider@
provider :: IsINBoatTrip inBoatTrip => inBoatTrip -> IO (Id NSString)
provider inBoatTrip  =
  sendMsg inBoatTrip (mkSelector "provider") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- boatName@
boatName :: IsINBoatTrip inBoatTrip => inBoatTrip -> IO (Id NSString)
boatName inBoatTrip  =
  sendMsg inBoatTrip (mkSelector "boatName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- boatNumber@
boatNumber :: IsINBoatTrip inBoatTrip => inBoatTrip -> IO (Id NSString)
boatNumber inBoatTrip  =
  sendMsg inBoatTrip (mkSelector "boatNumber") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- tripDuration@
tripDuration :: IsINBoatTrip inBoatTrip => inBoatTrip -> IO (Id INDateComponentsRange)
tripDuration inBoatTrip  =
  sendMsg inBoatTrip (mkSelector "tripDuration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- departureBoatTerminalLocation@
departureBoatTerminalLocation :: IsINBoatTrip inBoatTrip => inBoatTrip -> IO (Id CLPlacemark)
departureBoatTerminalLocation inBoatTrip  =
  sendMsg inBoatTrip (mkSelector "departureBoatTerminalLocation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- arrivalBoatTerminalLocation@
arrivalBoatTerminalLocation :: IsINBoatTrip inBoatTrip => inBoatTrip -> IO (Id CLPlacemark)
arrivalBoatTerminalLocation inBoatTrip  =
  sendMsg inBoatTrip (mkSelector "arrivalBoatTerminalLocation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithProvider:boatName:boatNumber:tripDuration:departureBoatTerminalLocation:arrivalBoatTerminalLocation:@
initWithProvider_boatName_boatNumber_tripDuration_departureBoatTerminalLocation_arrivalBoatTerminalLocationSelector :: Selector
initWithProvider_boatName_boatNumber_tripDuration_departureBoatTerminalLocation_arrivalBoatTerminalLocationSelector = mkSelector "initWithProvider:boatName:boatNumber:tripDuration:departureBoatTerminalLocation:arrivalBoatTerminalLocation:"

-- | @Selector@ for @provider@
providerSelector :: Selector
providerSelector = mkSelector "provider"

-- | @Selector@ for @boatName@
boatNameSelector :: Selector
boatNameSelector = mkSelector "boatName"

-- | @Selector@ for @boatNumber@
boatNumberSelector :: Selector
boatNumberSelector = mkSelector "boatNumber"

-- | @Selector@ for @tripDuration@
tripDurationSelector :: Selector
tripDurationSelector = mkSelector "tripDuration"

-- | @Selector@ for @departureBoatTerminalLocation@
departureBoatTerminalLocationSelector :: Selector
departureBoatTerminalLocationSelector = mkSelector "departureBoatTerminalLocation"

-- | @Selector@ for @arrivalBoatTerminalLocation@
arrivalBoatTerminalLocationSelector :: Selector
arrivalBoatTerminalLocationSelector = mkSelector "arrivalBoatTerminalLocation"

