{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTROperationalStateClusterOperationalErrorEvent@.
module ObjC.Matter.MTROperationalStateClusterOperationalErrorEvent
  ( MTROperationalStateClusterOperationalErrorEvent
  , IsMTROperationalStateClusterOperationalErrorEvent(..)
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
errorState :: IsMTROperationalStateClusterOperationalErrorEvent mtrOperationalStateClusterOperationalErrorEvent => mtrOperationalStateClusterOperationalErrorEvent -> IO (Id MTROperationalStateClusterErrorStateStruct)
errorState mtrOperationalStateClusterOperationalErrorEvent =
  sendMessage mtrOperationalStateClusterOperationalErrorEvent errorStateSelector

-- | @- setErrorState:@
setErrorState :: (IsMTROperationalStateClusterOperationalErrorEvent mtrOperationalStateClusterOperationalErrorEvent, IsMTROperationalStateClusterErrorStateStruct value) => mtrOperationalStateClusterOperationalErrorEvent -> value -> IO ()
setErrorState mtrOperationalStateClusterOperationalErrorEvent value =
  sendMessage mtrOperationalStateClusterOperationalErrorEvent setErrorStateSelector (toMTROperationalStateClusterErrorStateStruct value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @errorState@
errorStateSelector :: Selector '[] (Id MTROperationalStateClusterErrorStateStruct)
errorStateSelector = mkSelector "errorState"

-- | @Selector@ for @setErrorState:@
setErrorStateSelector :: Selector '[Id MTROperationalStateClusterErrorStateStruct] ()
setErrorStateSelector = mkSelector "setErrorState:"

