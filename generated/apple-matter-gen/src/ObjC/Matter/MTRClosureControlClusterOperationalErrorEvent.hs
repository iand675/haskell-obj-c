{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRClosureControlClusterOperationalErrorEvent@.
module ObjC.Matter.MTRClosureControlClusterOperationalErrorEvent
  ( MTRClosureControlClusterOperationalErrorEvent
  , IsMTRClosureControlClusterOperationalErrorEvent(..)
  , errorState
  , setErrorState
  , errorStateSelector
  , setErrorStateSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- errorState@
errorState :: IsMTRClosureControlClusterOperationalErrorEvent mtrClosureControlClusterOperationalErrorEvent => mtrClosureControlClusterOperationalErrorEvent -> IO (Id NSArray)
errorState mtrClosureControlClusterOperationalErrorEvent =
  sendMessage mtrClosureControlClusterOperationalErrorEvent errorStateSelector

-- | @- setErrorState:@
setErrorState :: (IsMTRClosureControlClusterOperationalErrorEvent mtrClosureControlClusterOperationalErrorEvent, IsNSArray value) => mtrClosureControlClusterOperationalErrorEvent -> value -> IO ()
setErrorState mtrClosureControlClusterOperationalErrorEvent value =
  sendMessage mtrClosureControlClusterOperationalErrorEvent setErrorStateSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @errorState@
errorStateSelector :: Selector '[] (Id NSArray)
errorStateSelector = mkSelector "errorState"

-- | @Selector@ for @setErrorState:@
setErrorStateSelector :: Selector '[Id NSArray] ()
setErrorStateSelector = mkSelector "setErrorState:"

