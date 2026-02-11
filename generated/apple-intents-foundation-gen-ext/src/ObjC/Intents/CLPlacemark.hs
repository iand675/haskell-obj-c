{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CLPlacemark@.
module ObjC.Intents.CLPlacemark
  ( CLPlacemark
  , IsCLPlacemark(..)
  , placemarkWithLocation_name_postalAddress
  , placemarkWithLocation_name_postalAddressSelector


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
import ObjC.Foundation.Internal.Classes

-- | @+ placemarkWithLocation:name:postalAddress:@
placemarkWithLocation_name_postalAddress :: IsNSString name => RawId -> name -> RawId -> IO (Id CLPlacemark)
placemarkWithLocation_name_postalAddress location name postalAddress =
  do
    cls' <- getRequiredClass "CLPlacemark"
    withObjCPtr name $ \raw_name ->
      sendClassMsg cls' (mkSelector "placemarkWithLocation:name:postalAddress:") (retPtr retVoid) [argPtr (castPtr (unRawId location) :: Ptr ()), argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr (unRawId postalAddress) :: Ptr ())] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @placemarkWithLocation:name:postalAddress:@
placemarkWithLocation_name_postalAddressSelector :: Selector
placemarkWithLocation_name_postalAddressSelector = mkSelector "placemarkWithLocation:name:postalAddress:"

