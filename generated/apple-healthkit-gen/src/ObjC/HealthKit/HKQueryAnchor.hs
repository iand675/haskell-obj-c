{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | HKQueryAnchor
--
-- This object encapsulates the state of an HKAnchoredObjectQuery
--
-- Generated bindings for @HKQueryAnchor@.
module ObjC.HealthKit.HKQueryAnchor
  ( HKQueryAnchor
  , IsHKQueryAnchor(..)
  , anchorFromValue
  , init_
  , anchorFromValueSelector
  , initSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.HealthKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | anchorFromValue:
--
-- Creates an HKQueryAnchor with an integer anchor which was previously obtained from an                HKAnchoredObjectQuery prior to iOS 9.0.
--
-- ObjC selector: @+ anchorFromValue:@
anchorFromValue :: CULong -> IO (Id HKQueryAnchor)
anchorFromValue value =
  do
    cls' <- getRequiredClass "HKQueryAnchor"
    sendClassMessage cls' anchorFromValueSelector value

-- | @- init@
init_ :: IsHKQueryAnchor hkQueryAnchor => hkQueryAnchor -> IO (Id HKQueryAnchor)
init_ hkQueryAnchor =
  sendOwnedMessage hkQueryAnchor initSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @anchorFromValue:@
anchorFromValueSelector :: Selector '[CULong] (Id HKQueryAnchor)
anchorFromValueSelector = mkSelector "anchorFromValue:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id HKQueryAnchor)
initSelector = mkSelector "init"

