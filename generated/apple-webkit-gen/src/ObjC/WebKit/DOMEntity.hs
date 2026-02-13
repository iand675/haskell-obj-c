{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMEntity@.
module ObjC.WebKit.DOMEntity
  ( DOMEntity
  , IsDOMEntity(..)
  , publicId
  , systemId
  , notationName
  , notationNameSelector
  , publicIdSelector
  , systemIdSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- publicId@
publicId :: IsDOMEntity domEntity => domEntity -> IO (Id NSString)
publicId domEntity =
  sendMessage domEntity publicIdSelector

-- | @- systemId@
systemId :: IsDOMEntity domEntity => domEntity -> IO (Id NSString)
systemId domEntity =
  sendMessage domEntity systemIdSelector

-- | @- notationName@
notationName :: IsDOMEntity domEntity => domEntity -> IO (Id NSString)
notationName domEntity =
  sendMessage domEntity notationNameSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @publicId@
publicIdSelector :: Selector '[] (Id NSString)
publicIdSelector = mkSelector "publicId"

-- | @Selector@ for @systemId@
systemIdSelector :: Selector '[] (Id NSString)
systemIdSelector = mkSelector "systemId"

-- | @Selector@ for @notationName@
notationNameSelector :: Selector '[] (Id NSString)
notationNameSelector = mkSelector "notationName"

