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
  , initWithPartySizeRange_sizeDescription_priceRangeSelector
  , initSelector
  , partySizeRangeSelector
  , sizeDescriptionSelector
  , priceRangeSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | @- initWithPartySizeRange:sizeDescription:priceRange:@
initWithPartySizeRange_sizeDescription_priceRange :: (IsINRidePartySizeOption inRidePartySizeOption, IsNSString sizeDescription, IsINPriceRange priceRange) => inRidePartySizeOption -> NSRange -> sizeDescription -> priceRange -> IO (Id INRidePartySizeOption)
initWithPartySizeRange_sizeDescription_priceRange inRidePartySizeOption  partySizeRange sizeDescription priceRange =
withObjCPtr sizeDescription $ \raw_sizeDescription ->
  withObjCPtr priceRange $ \raw_priceRange ->
      sendMsg inRidePartySizeOption (mkSelector "initWithPartySizeRange:sizeDescription:priceRange:") (retPtr retVoid) [argNSRange partySizeRange, argPtr (castPtr raw_sizeDescription :: Ptr ()), argPtr (castPtr raw_priceRange :: Ptr ())] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsINRidePartySizeOption inRidePartySizeOption => inRidePartySizeOption -> IO (Id INRidePartySizeOption)
init_ inRidePartySizeOption  =
  sendMsg inRidePartySizeOption (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- partySizeRange@
partySizeRange :: IsINRidePartySizeOption inRidePartySizeOption => inRidePartySizeOption -> IO NSRange
partySizeRange inRidePartySizeOption  =
  sendMsgStret inRidePartySizeOption (mkSelector "partySizeRange") retNSRange []

-- | @- sizeDescription@
sizeDescription :: IsINRidePartySizeOption inRidePartySizeOption => inRidePartySizeOption -> IO (Id NSString)
sizeDescription inRidePartySizeOption  =
  sendMsg inRidePartySizeOption (mkSelector "sizeDescription") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- priceRange@
priceRange :: IsINRidePartySizeOption inRidePartySizeOption => inRidePartySizeOption -> IO (Id INPriceRange)
priceRange inRidePartySizeOption  =
  sendMsg inRidePartySizeOption (mkSelector "priceRange") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithPartySizeRange:sizeDescription:priceRange:@
initWithPartySizeRange_sizeDescription_priceRangeSelector :: Selector
initWithPartySizeRange_sizeDescription_priceRangeSelector = mkSelector "initWithPartySizeRange:sizeDescription:priceRange:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @partySizeRange@
partySizeRangeSelector :: Selector
partySizeRangeSelector = mkSelector "partySizeRange"

-- | @Selector@ for @sizeDescription@
sizeDescriptionSelector :: Selector
sizeDescriptionSelector = mkSelector "sizeDescription"

-- | @Selector@ for @priceRange@
priceRangeSelector :: Selector
priceRangeSelector = mkSelector "priceRange"

