{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INFocusStatus@.
module ObjC.Intents.INFocusStatus
  ( INFocusStatus
  , IsINFocusStatus(..)
  , init_
  , initWithIsFocused
  , isFocused
  , initSelector
  , initWithIsFocusedSelector
  , isFocusedSelector


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

-- | @- init@
init_ :: IsINFocusStatus inFocusStatus => inFocusStatus -> IO (Id INFocusStatus)
init_ inFocusStatus  =
    sendMsg inFocusStatus (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithIsFocused:@
initWithIsFocused :: (IsINFocusStatus inFocusStatus, IsNSNumber isFocused) => inFocusStatus -> isFocused -> IO (Id INFocusStatus)
initWithIsFocused inFocusStatus  isFocused =
  withObjCPtr isFocused $ \raw_isFocused ->
      sendMsg inFocusStatus (mkSelector "initWithIsFocused:") (retPtr retVoid) [argPtr (castPtr raw_isFocused :: Ptr ())] >>= ownedObject . castPtr

-- | @- isFocused@
isFocused :: IsINFocusStatus inFocusStatus => inFocusStatus -> IO (Id NSNumber)
isFocused inFocusStatus  =
    sendMsg inFocusStatus (mkSelector "isFocused") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithIsFocused:@
initWithIsFocusedSelector :: Selector
initWithIsFocusedSelector = mkSelector "initWithIsFocused:"

-- | @Selector@ for @isFocused@
isFocusedSelector :: Selector
isFocusedSelector = mkSelector "isFocused"

