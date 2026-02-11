{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CSLocalizedString@.
module ObjC.CoreSpotlight.CSLocalizedString
  ( CSLocalizedString
  , IsCSLocalizedString(..)
  , initWithLocalizedStrings
  , localizedString
  , initWithLocalizedStringsSelector
  , localizedStringSelector


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

import ObjC.CoreSpotlight.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithLocalizedStrings:@
initWithLocalizedStrings :: (IsCSLocalizedString csLocalizedString, IsNSDictionary localizedStrings) => csLocalizedString -> localizedStrings -> IO (Id CSLocalizedString)
initWithLocalizedStrings csLocalizedString  localizedStrings =
withObjCPtr localizedStrings $ \raw_localizedStrings ->
    sendMsg csLocalizedString (mkSelector "initWithLocalizedStrings:") (retPtr retVoid) [argPtr (castPtr raw_localizedStrings :: Ptr ())] >>= ownedObject . castPtr

-- | @- localizedString@
localizedString :: IsCSLocalizedString csLocalizedString => csLocalizedString -> IO (Id NSString)
localizedString csLocalizedString  =
  sendMsg csLocalizedString (mkSelector "localizedString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithLocalizedStrings:@
initWithLocalizedStringsSelector :: Selector
initWithLocalizedStringsSelector = mkSelector "initWithLocalizedStrings:"

-- | @Selector@ for @localizedString@
localizedStringSelector :: Selector
localizedStringSelector = mkSelector "localizedString"

