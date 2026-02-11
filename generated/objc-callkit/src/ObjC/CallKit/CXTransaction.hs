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
  , initWithActionsSelector
  , initWithActionSelector
  , addActionSelector
  , uuidSelector
  , completeSelector
  , actionsSelector


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

import ObjC.CallKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithActions:@
initWithActions :: (IsCXTransaction cxTransaction, IsNSArray actions) => cxTransaction -> actions -> IO (Id CXTransaction)
initWithActions cxTransaction  actions =
withObjCPtr actions $ \raw_actions ->
    sendMsg cxTransaction (mkSelector "initWithActions:") (retPtr retVoid) [argPtr (castPtr raw_actions :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithAction:@
initWithAction :: (IsCXTransaction cxTransaction, IsCXAction action) => cxTransaction -> action -> IO (Id CXTransaction)
initWithAction cxTransaction  action =
withObjCPtr action $ \raw_action ->
    sendMsg cxTransaction (mkSelector "initWithAction:") (retPtr retVoid) [argPtr (castPtr raw_action :: Ptr ())] >>= ownedObject . castPtr

-- | Add the provided action to the receiver's list of actions
--
-- ObjC selector: @- addAction:@
addAction :: (IsCXTransaction cxTransaction, IsCXAction action) => cxTransaction -> action -> IO ()
addAction cxTransaction  action =
withObjCPtr action $ \raw_action ->
    sendMsg cxTransaction (mkSelector "addAction:") retVoid [argPtr (castPtr raw_action :: Ptr ())]

-- | Unique ID
--
-- ObjC selector: @- UUID@
uuid :: IsCXTransaction cxTransaction => cxTransaction -> IO (Id NSUUID)
uuid cxTransaction  =
  sendMsg cxTransaction (mkSelector "UUID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Whether all actions have been completed
--
-- ObjC selector: @- complete@
complete :: IsCXTransaction cxTransaction => cxTransaction -> IO Bool
complete cxTransaction  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg cxTransaction (mkSelector "complete") retCULong []

-- | The list of actions contained by the receiver
--
-- ObjC selector: @- actions@
actions :: IsCXTransaction cxTransaction => cxTransaction -> IO (Id NSArray)
actions cxTransaction  =
  sendMsg cxTransaction (mkSelector "actions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithActions:@
initWithActionsSelector :: Selector
initWithActionsSelector = mkSelector "initWithActions:"

-- | @Selector@ for @initWithAction:@
initWithActionSelector :: Selector
initWithActionSelector = mkSelector "initWithAction:"

-- | @Selector@ for @addAction:@
addActionSelector :: Selector
addActionSelector = mkSelector "addAction:"

-- | @Selector@ for @UUID@
uuidSelector :: Selector
uuidSelector = mkSelector "UUID"

-- | @Selector@ for @complete@
completeSelector :: Selector
completeSelector = mkSelector "complete"

-- | @Selector@ for @actions@
actionsSelector :: Selector
actionsSelector = mkSelector "actions"

