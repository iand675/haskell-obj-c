{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INRidePartySizeOption@.
module ObjC.Intents.INRidePartySizeOption
  ( INRidePartySizeOption
  , IsINRidePartySizeOption(..)
  , initWithPartySizeRange_sizeDescription_priceRange
  , init_
  , partySizeRange
  , sizeDescription
  , priceRange
  , initSelector
  , initWithPartySizeRange_sizeDescription_priceRangeSelector
  , partySizeRangeSelector
  , priceRangeSelector
  , sizeDescriptionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | @- initWithPartySizeRange:sizeDescription:priceRange:@
initWithPartySizeRange_sizeDescription_priceRange :: (IsINRidePartySizeOption inRidePartySizeOption, IsNSString sizeDescription, IsINPriceRange priceRange) => inRidePartySizeOption -> NSRange -> sizeDescription -> priceRange -> IO (Id INRidePartySizeOption)
initWithPartySizeRange_sizeDescription_priceRange inRidePartySizeOption partySizeRange sizeDescription priceRange =
  sendOwnedMessage inRidePartySizeOption initWithPartySizeRange_sizeDescription_priceRangeSelector partySizeRange (toNSString sizeDescription) (toINPriceRange priceRange)

-- | @- init@
init_ :: IsINRidePartySizeOption inRidePartySizeOption => inRidePartySizeOption -> IO (Id INRidePartySizeOption)
init_ inRidePartySizeOption =
  sendOwnedMessage inRidePartySizeOption initSelector

-- | @- partySizeRange@
partySizeRange :: IsINRidePartySizeOption inRidePartySizeOption => inRidePartySizeOption -> IO NSRange
partySizeRange inRidePartySizeOption =
  sendMessage inRidePartySizeOption partySizeRangeSelector

-- | @- sizeDescription@
sizeDescription :: IsINRidePartySizeOption inRidePartySizeOption => inRidePartySizeOption -> IO (Id NSString)
sizeDescription inRidePartySizeOption =
  sendMessage inRidePartySizeOption sizeDescriptionSelector

-- | @- priceRange@
priceRange :: IsINRidePartySizeOption inRidePartySizeOption => inRidePartySizeOption -> IO (Id INPriceRange)
priceRange inRidePartySizeOption =
  sendMessage inRidePartySizeOption priceRangeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithPartySizeRange:sizeDescription:priceRange:@
initWithPartySizeRange_sizeDescription_priceRangeSelector :: Selector '[NSRange, Id NSString, Id INPriceRange] (Id INRidePartySizeOption)
initWithPartySizeRange_sizeDescription_priceRangeSelector = mkSelector "initWithPartySizeRange:sizeDescription:priceRange:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id INRidePartySizeOption)
initSelector = mkSelector "init"

-- | @Selector@ for @partySizeRange@
partySizeRangeSelector :: Selector '[] NSRange
partySizeRangeSelector = mkSelector "partySizeRange"

-- | @Selector@ for @sizeDescription@
sizeDescriptionSelector :: Selector '[] (Id NSString)
sizeDescriptionSelector = mkSelector "sizeDescription"

-- | @Selector@ for @priceRange@
priceRangeSelector :: Selector '[] (Id INPriceRange)
priceRangeSelector = mkSelector "priceRange"

