{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INShareFocusStatusIntent@.
module ObjC.Intents.INShareFocusStatusIntent
  ( INShareFocusStatusIntent
  , IsINShareFocusStatusIntent(..)
  , initWithFocusStatus
  , focusStatus
  , initWithFocusStatusSelector
  , focusStatusSelector


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

-- | @- initWithFocusStatus:@
initWithFocusStatus :: (IsINShareFocusStatusIntent inShareFocusStatusIntent, IsINFocusStatus focusStatus) => inShareFocusStatusIntent -> focusStatus -> IO (Id INShareFocusStatusIntent)
initWithFocusStatus inShareFocusStatusIntent  focusStatus =
withObjCPtr focusStatus $ \raw_focusStatus ->
    sendMsg inShareFocusStatusIntent (mkSelector "initWithFocusStatus:") (retPtr retVoid) [argPtr (castPtr raw_focusStatus :: Ptr ())] >>= ownedObject . castPtr

-- | @- focusStatus@
focusStatus :: IsINShareFocusStatusIntent inShareFocusStatusIntent => inShareFocusStatusIntent -> IO (Id INFocusStatus)
focusStatus inShareFocusStatusIntent  =
  sendMsg inShareFocusStatusIntent (mkSelector "focusStatus") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithFocusStatus:@
initWithFocusStatusSelector :: Selector
initWithFocusStatusSelector = mkSelector "initWithFocusStatus:"

-- | @Selector@ for @focusStatus@
focusStatusSelector :: Selector
focusStatusSelector = mkSelector "focusStatus"

