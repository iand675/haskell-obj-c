{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @HKSampleQuery@.
module ObjC.HealthKit.HKSampleQuery
  ( HKSampleQuery
  , IsHKSampleQuery(..)
  , limit
  , sortDescriptors
  , limitSelector
  , sortDescriptorsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.HealthKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | limit
--
-- The maximum number of results the receiver will return upon completion.
--
-- ObjC selector: @- limit@
limit :: IsHKSampleQuery hkSampleQuery => hkSampleQuery -> IO CULong
limit hkSampleQuery =
  sendMessage hkSampleQuery limitSelector

-- | sortDescriptors
--
-- An array of NSSortDescriptors.
--
-- ObjC selector: @- sortDescriptors@
sortDescriptors :: IsHKSampleQuery hkSampleQuery => hkSampleQuery -> IO (Id NSArray)
sortDescriptors hkSampleQuery =
  sendMessage hkSampleQuery sortDescriptorsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @limit@
limitSelector :: Selector '[] CULong
limitSelector = mkSelector "limit"

-- | @Selector@ for @sortDescriptors@
sortDescriptorsSelector :: Selector '[] (Id NSArray)
sortDescriptorsSelector = mkSelector "sortDescriptors"

