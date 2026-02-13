{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRRVCOperationalStateClusterOperationalErrorEvent@.
module ObjC.Matter.MTRRVCOperationalStateClusterOperationalErrorEvent
  ( MTRRVCOperationalStateClusterOperationalErrorEvent
  , IsMTRRVCOperationalStateClusterOperationalErrorEvent(..)
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
errorState :: IsMTRRVCOperationalStateClusterOperationalErrorEvent mtrrvcOperationalStateClusterOperationalErrorEvent => mtrrvcOperationalStateClusterOperationalErrorEvent -> IO (Id MTRRVCOperationalStateClusterErrorStateStruct)
errorState mtrrvcOperationalStateClusterOperationalErrorEvent =
  sendMessage mtrrvcOperationalStateClusterOperationalErrorEvent errorStateSelector

-- | @- setErrorState:@
setErrorState :: (IsMTRRVCOperationalStateClusterOperationalErrorEvent mtrrvcOperationalStateClusterOperationalErrorEvent, IsMTRRVCOperationalStateClusterErrorStateStruct value) => mtrrvcOperationalStateClusterOperationalErrorEvent -> value -> IO ()
setErrorState mtrrvcOperationalStateClusterOperationalErrorEvent value =
  sendMessage mtrrvcOperationalStateClusterOperationalErrorEvent setErrorStateSelector (toMTRRVCOperationalStateClusterErrorStateStruct value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @errorState@
errorStateSelector :: Selector '[] (Id MTRRVCOperationalStateClusterErrorStateStruct)
errorStateSelector = mkSelector "errorState"

-- | @Selector@ for @setErrorState:@
setErrorStateSelector :: Selector '[Id MTRRVCOperationalStateClusterErrorStateStruct] ()
setErrorStateSelector = mkSelector "setErrorState:"

