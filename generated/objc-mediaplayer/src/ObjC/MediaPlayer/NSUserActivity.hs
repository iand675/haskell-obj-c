{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSUserActivity@.
module ObjC.MediaPlayer.NSUserActivity
  ( NSUserActivity
  , IsNSUserActivity(..)
  , externalMediaContentIdentifier
  , setExternalMediaContentIdentifier
  , externalMediaContentIdentifierSelector
  , setExternalMediaContentIdentifierSelector


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

import ObjC.MediaPlayer.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | A unique identifier relative to the app's media content catalog for the displayed media item.
--
-- ObjC selector: @- externalMediaContentIdentifier@
externalMediaContentIdentifier :: IsNSUserActivity nsUserActivity => nsUserActivity -> IO (Id NSString)
externalMediaContentIdentifier nsUserActivity  =
  sendMsg nsUserActivity (mkSelector "externalMediaContentIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A unique identifier relative to the app's media content catalog for the displayed media item.
--
-- ObjC selector: @- setExternalMediaContentIdentifier:@
setExternalMediaContentIdentifier :: (IsNSUserActivity nsUserActivity, IsNSString value) => nsUserActivity -> value -> IO ()
setExternalMediaContentIdentifier nsUserActivity  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsUserActivity (mkSelector "setExternalMediaContentIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @externalMediaContentIdentifier@
externalMediaContentIdentifierSelector :: Selector
externalMediaContentIdentifierSelector = mkSelector "externalMediaContentIdentifier"

-- | @Selector@ for @setExternalMediaContentIdentifier:@
setExternalMediaContentIdentifierSelector :: Selector
setExternalMediaContentIdentifierSelector = mkSelector "setExternalMediaContentIdentifier:"

