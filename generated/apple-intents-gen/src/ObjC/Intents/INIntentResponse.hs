{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @INIntentResponse@.
module ObjC.Intents.INIntentResponse
  ( INIntentResponse
  , IsINIntentResponse(..)
  , userActivity
  , setUserActivity
  , setUserActivitySelector
  , userActivitySelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Intents.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- userActivity@
userActivity :: IsINIntentResponse inIntentResponse => inIntentResponse -> IO (Id NSUserActivity)
userActivity inIntentResponse =
  sendMessage inIntentResponse userActivitySelector

-- | @- setUserActivity:@
setUserActivity :: (IsINIntentResponse inIntentResponse, IsNSUserActivity value) => inIntentResponse -> value -> IO ()
setUserActivity inIntentResponse value =
  sendMessage inIntentResponse setUserActivitySelector (toNSUserActivity value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @userActivity@
userActivitySelector :: Selector '[] (Id NSUserActivity)
userActivitySelector = mkSelector "userActivity"

-- | @Selector@ for @setUserActivity:@
setUserActivitySelector :: Selector '[Id NSUserActivity] ()
setUserActivitySelector = mkSelector "setUserActivity:"

