{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MediaPlayer.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | A unique identifier relative to the app's media content catalog for the displayed media item.
--
-- ObjC selector: @- externalMediaContentIdentifier@
externalMediaContentIdentifier :: IsNSUserActivity nsUserActivity => nsUserActivity -> IO (Id NSString)
externalMediaContentIdentifier nsUserActivity =
  sendMessage nsUserActivity externalMediaContentIdentifierSelector

-- | A unique identifier relative to the app's media content catalog for the displayed media item.
--
-- ObjC selector: @- setExternalMediaContentIdentifier:@
setExternalMediaContentIdentifier :: (IsNSUserActivity nsUserActivity, IsNSString value) => nsUserActivity -> value -> IO ()
setExternalMediaContentIdentifier nsUserActivity value =
  sendMessage nsUserActivity setExternalMediaContentIdentifierSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @externalMediaContentIdentifier@
externalMediaContentIdentifierSelector :: Selector '[] (Id NSString)
externalMediaContentIdentifierSelector = mkSelector "externalMediaContentIdentifier"

-- | @Selector@ for @setExternalMediaContentIdentifier:@
setExternalMediaContentIdentifierSelector :: Selector '[Id NSString] ()
setExternalMediaContentIdentifierSelector = mkSelector "setExternalMediaContentIdentifier:"

