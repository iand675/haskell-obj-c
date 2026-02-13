{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CXTransaction@.
module ObjC.CallKit.CXTransaction
  ( CXTransaction
  , IsCXTransaction(..)
  , initWithActions
  , initWithAction
  , addAction
  , uuid
  , complete
  , actions
  , actionsSelector
  , addActionSelector
  , completeSelector
  , initWithActionSelector
  , initWithActionsSelector
  , uuidSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CallKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithActions:@
initWithActions :: (IsCXTransaction cxTransaction, IsNSArray actions) => cxTransaction -> actions -> IO (Id CXTransaction)
initWithActions cxTransaction actions =
  sendOwnedMessage cxTransaction initWithActionsSelector (toNSArray actions)

-- | @- initWithAction:@
initWithAction :: (IsCXTransaction cxTransaction, IsCXAction action) => cxTransaction -> action -> IO (Id CXTransaction)
initWithAction cxTransaction action =
  sendOwnedMessage cxTransaction initWithActionSelector (toCXAction action)

-- | Add the provided action to the receiver's list of actions
--
-- ObjC selector: @- addAction:@
addAction :: (IsCXTransaction cxTransaction, IsCXAction action) => cxTransaction -> action -> IO ()
addAction cxTransaction action =
  sendMessage cxTransaction addActionSelector (toCXAction action)

-- | Unique ID
--
-- ObjC selector: @- UUID@
uuid :: IsCXTransaction cxTransaction => cxTransaction -> IO (Id NSUUID)
uuid cxTransaction =
  sendMessage cxTransaction uuidSelector

-- | Whether all actions have been completed
--
-- ObjC selector: @- complete@
complete :: IsCXTransaction cxTransaction => cxTransaction -> IO Bool
complete cxTransaction =
  sendMessage cxTransaction completeSelector

-- | The list of actions contained by the receiver
--
-- ObjC selector: @- actions@
actions :: IsCXTransaction cxTransaction => cxTransaction -> IO (Id NSArray)
actions cxTransaction =
  sendMessage cxTransaction actionsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithActions:@
initWithActionsSelector :: Selector '[Id NSArray] (Id CXTransaction)
initWithActionsSelector = mkSelector "initWithActions:"

-- | @Selector@ for @initWithAction:@
initWithActionSelector :: Selector '[Id CXAction] (Id CXTransaction)
initWithActionSelector = mkSelector "initWithAction:"

-- | @Selector@ for @addAction:@
addActionSelector :: Selector '[Id CXAction] ()
addActionSelector = mkSelector "addAction:"

-- | @Selector@ for @UUID@
uuidSelector :: Selector '[] (Id NSUUID)
uuidSelector = mkSelector "UUID"

-- | @Selector@ for @complete@
completeSelector :: Selector '[] Bool
completeSelector = mkSelector "complete"

-- | @Selector@ for @actions@
actionsSelector :: Selector '[] (Id NSArray)
actionsSelector = mkSelector "actions"

