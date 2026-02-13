{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Represents a decision to focus on a specific detectionID or detectionGroupID; optionally strong.
--
-- A strong decision keeps focus for as long as possible.
--
-- Generated bindings for @CNDecision@.
module ObjC.Cinematic.CNDecision
  ( CNDecision
  , IsCNDecision(..)
  , init_
  , new
  , detectionID
  , detectionGroupID
  , userDecision
  , groupDecision
  , strongDecision
  , detectionGroupIDSelector
  , detectionIDSelector
  , groupDecisionSelector
  , initSelector
  , newSelector
  , strongDecisionSelector
  , userDecisionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Cinematic.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCNDecision cnDecision => cnDecision -> IO (Id CNDecision)
init_ cnDecision =
  sendOwnedMessage cnDecision initSelector

-- | @+ new@
new :: IO (Id CNDecision)
new  =
  do
    cls' <- getRequiredClass "CNDecision"
    sendOwnedClassMessage cls' newSelector

-- | The detectionID of the detection to focus on if this is not a group decision.
--
-- ObjC selector: @- detectionID@
detectionID :: IsCNDecision cnDecision => cnDecision -> IO CLong
detectionID cnDecision =
  sendMessage cnDecision detectionIDSelector

-- | The detectionGroupID of the detection to focus on if this is a group decision.
--
-- ObjC selector: @- detectionGroupID@
detectionGroupID :: IsCNDecision cnDecision => cnDecision -> IO CLong
detectionGroupID cnDecision =
  sendMessage cnDecision detectionGroupIDSelector

-- | Whether this is a user-created decision, or a base decision.
--
-- ObjC selector: @- userDecision@
userDecision :: IsCNDecision cnDecision => cnDecision -> IO Bool
userDecision cnDecision =
  sendMessage cnDecision userDecisionSelector

-- | Whether this is a group decision or not.
--
-- ObjC selector: @- groupDecision@
groupDecision :: IsCNDecision cnDecision => cnDecision -> IO Bool
groupDecision cnDecision =
  sendMessage cnDecision groupDecisionSelector

-- | Whether this is a strong decision or not. A strong decision keeps focus for as long as possible.
--
-- ObjC selector: @- strongDecision@
strongDecision :: IsCNDecision cnDecision => cnDecision -> IO Bool
strongDecision cnDecision =
  sendMessage cnDecision strongDecisionSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CNDecision)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id CNDecision)
newSelector = mkSelector "new"

-- | @Selector@ for @detectionID@
detectionIDSelector :: Selector '[] CLong
detectionIDSelector = mkSelector "detectionID"

-- | @Selector@ for @detectionGroupID@
detectionGroupIDSelector :: Selector '[] CLong
detectionGroupIDSelector = mkSelector "detectionGroupID"

-- | @Selector@ for @userDecision@
userDecisionSelector :: Selector '[] Bool
userDecisionSelector = mkSelector "userDecision"

-- | @Selector@ for @groupDecision@
groupDecisionSelector :: Selector '[] Bool
groupDecisionSelector = mkSelector "groupDecision"

-- | @Selector@ for @strongDecision@
strongDecisionSelector :: Selector '[] Bool
strongDecisionSelector = mkSelector "strongDecision"

