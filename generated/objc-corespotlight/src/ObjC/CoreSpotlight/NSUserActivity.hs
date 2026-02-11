{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSUserActivity@.
module ObjC.CoreSpotlight.NSUserActivity
  ( NSUserActivity
  , IsNSUserActivity(..)
  , contentAttributeSet
  , setContentAttributeSet
  , contentAttributeSetSelector
  , setContentAttributeSetSelector


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

-- | @- contentAttributeSet@
contentAttributeSet :: IsNSUserActivity nsUserActivity => nsUserActivity -> IO (Id CSSearchableItemAttributeSet)
contentAttributeSet nsUserActivity  =
  sendMsg nsUserActivity (mkSelector "contentAttributeSet") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setContentAttributeSet:@
setContentAttributeSet :: (IsNSUserActivity nsUserActivity, IsCSSearchableItemAttributeSet value) => nsUserActivity -> value -> IO ()
setContentAttributeSet nsUserActivity  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsUserActivity (mkSelector "setContentAttributeSet:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @contentAttributeSet@
contentAttributeSetSelector :: Selector
contentAttributeSetSelector = mkSelector "contentAttributeSet"

-- | @Selector@ for @setContentAttributeSet:@
setContentAttributeSetSelector :: Selector
setContentAttributeSetSelector = mkSelector "setContentAttributeSet:"

