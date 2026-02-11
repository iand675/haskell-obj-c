{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INIntentResponse@.
module ObjC.Intents.INIntentResponse
  ( INIntentResponse
  , IsINIntentResponse(..)
  , userActivity
  , setUserActivity
  , userActivitySelector
  , setUserActivitySelector


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

-- | @- userActivity@
userActivity :: IsINIntentResponse inIntentResponse => inIntentResponse -> IO (Id NSUserActivity)
userActivity inIntentResponse  =
  sendMsg inIntentResponse (mkSelector "userActivity") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setUserActivity:@
setUserActivity :: (IsINIntentResponse inIntentResponse, IsNSUserActivity value) => inIntentResponse -> value -> IO ()
setUserActivity inIntentResponse  value =
withObjCPtr value $ \raw_value ->
    sendMsg inIntentResponse (mkSelector "setUserActivity:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @userActivity@
userActivitySelector :: Selector
userActivitySelector = mkSelector "userActivity"

-- | @Selector@ for @setUserActivity:@
setUserActivitySelector :: Selector
setUserActivitySelector = mkSelector "setUserActivity:"

