{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRModeSelectClusterSemanticTagStruct@.
module ObjC.Matter.MTRModeSelectClusterSemanticTagStruct
  ( MTRModeSelectClusterSemanticTagStruct
  , IsMTRModeSelectClusterSemanticTagStruct(..)
  , mfgCode
  , setMfgCode
  , value
  , setValue
  , mfgCodeSelector
  , setMfgCodeSelector
  , setValueSelector
  , valueSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- mfgCode@
mfgCode :: IsMTRModeSelectClusterSemanticTagStruct mtrModeSelectClusterSemanticTagStruct => mtrModeSelectClusterSemanticTagStruct -> IO (Id NSNumber)
mfgCode mtrModeSelectClusterSemanticTagStruct =
  sendMessage mtrModeSelectClusterSemanticTagStruct mfgCodeSelector

-- | @- setMfgCode:@
setMfgCode :: (IsMTRModeSelectClusterSemanticTagStruct mtrModeSelectClusterSemanticTagStruct, IsNSNumber value) => mtrModeSelectClusterSemanticTagStruct -> value -> IO ()
setMfgCode mtrModeSelectClusterSemanticTagStruct value =
  sendMessage mtrModeSelectClusterSemanticTagStruct setMfgCodeSelector (toNSNumber value)

-- | @- value@
value :: IsMTRModeSelectClusterSemanticTagStruct mtrModeSelectClusterSemanticTagStruct => mtrModeSelectClusterSemanticTagStruct -> IO (Id NSNumber)
value mtrModeSelectClusterSemanticTagStruct =
  sendMessage mtrModeSelectClusterSemanticTagStruct valueSelector

-- | @- setValue:@
setValue :: (IsMTRModeSelectClusterSemanticTagStruct mtrModeSelectClusterSemanticTagStruct, IsNSNumber value) => mtrModeSelectClusterSemanticTagStruct -> value -> IO ()
setValue mtrModeSelectClusterSemanticTagStruct value =
  sendMessage mtrModeSelectClusterSemanticTagStruct setValueSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @mfgCode@
mfgCodeSelector :: Selector '[] (Id NSNumber)
mfgCodeSelector = mkSelector "mfgCode"

-- | @Selector@ for @setMfgCode:@
setMfgCodeSelector :: Selector '[Id NSNumber] ()
setMfgCodeSelector = mkSelector "setMfgCode:"

-- | @Selector@ for @value@
valueSelector :: Selector '[] (Id NSNumber)
valueSelector = mkSelector "value"

-- | @Selector@ for @setValue:@
setValueSelector :: Selector '[Id NSNumber] ()
setValueSelector = mkSelector "setValue:"

