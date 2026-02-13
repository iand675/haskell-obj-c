{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRModeSelectClusterSemanticTag@.
module ObjC.Matter.MTRModeSelectClusterSemanticTag
  ( MTRModeSelectClusterSemanticTag
  , IsMTRModeSelectClusterSemanticTag(..)
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
mfgCode :: IsMTRModeSelectClusterSemanticTag mtrModeSelectClusterSemanticTag => mtrModeSelectClusterSemanticTag -> IO (Id NSNumber)
mfgCode mtrModeSelectClusterSemanticTag =
  sendMessage mtrModeSelectClusterSemanticTag mfgCodeSelector

-- | @- setMfgCode:@
setMfgCode :: (IsMTRModeSelectClusterSemanticTag mtrModeSelectClusterSemanticTag, IsNSNumber value) => mtrModeSelectClusterSemanticTag -> value -> IO ()
setMfgCode mtrModeSelectClusterSemanticTag value =
  sendMessage mtrModeSelectClusterSemanticTag setMfgCodeSelector (toNSNumber value)

-- | @- value@
value :: IsMTRModeSelectClusterSemanticTag mtrModeSelectClusterSemanticTag => mtrModeSelectClusterSemanticTag -> IO (Id NSNumber)
value mtrModeSelectClusterSemanticTag =
  sendMessage mtrModeSelectClusterSemanticTag valueSelector

-- | @- setValue:@
setValue :: (IsMTRModeSelectClusterSemanticTag mtrModeSelectClusterSemanticTag, IsNSNumber value) => mtrModeSelectClusterSemanticTag -> value -> IO ()
setValue mtrModeSelectClusterSemanticTag value =
  sendMessage mtrModeSelectClusterSemanticTag setValueSelector (toNSNumber value)

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

