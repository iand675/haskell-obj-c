{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSValue@.
module ObjC.SceneKit.NSValue
  ( NSValue
  , IsNSValue(..)
  , valueWithSCNVector3
  , valueWithSCNVector4
  , valueWithSCNMatrix4
  , scnVector3Value
  , scnVector4Value
  , scnMatrix4Value
  , scnMatrix4ValueSelector
  , scnVector3ValueSelector
  , scnVector4ValueSelector
  , valueWithSCNMatrix4Selector
  , valueWithSCNVector3Selector
  , valueWithSCNVector4Selector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SceneKit.Internal.Classes
import ObjC.SceneKit.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | @+ valueWithSCNVector3:@
valueWithSCNVector3 :: SCNVector3 -> IO (Id NSValue)
valueWithSCNVector3 v =
  do
    cls' <- getRequiredClass "NSValue"
    sendClassMessage cls' valueWithSCNVector3Selector v

-- | @+ valueWithSCNVector4:@
valueWithSCNVector4 :: SCNVector4 -> IO (Id NSValue)
valueWithSCNVector4 v =
  do
    cls' <- getRequiredClass "NSValue"
    sendClassMessage cls' valueWithSCNVector4Selector v

-- | @+ valueWithSCNMatrix4:@
valueWithSCNMatrix4 :: SCNMatrix4 -> IO (Id NSValue)
valueWithSCNMatrix4 v =
  do
    cls' <- getRequiredClass "NSValue"
    sendClassMessage cls' valueWithSCNMatrix4Selector v

-- | @- SCNVector3Value@
scnVector3Value :: IsNSValue nsValue => nsValue -> IO SCNVector3
scnVector3Value nsValue =
  sendMessage nsValue scnVector3ValueSelector

-- | @- SCNVector4Value@
scnVector4Value :: IsNSValue nsValue => nsValue -> IO SCNVector4
scnVector4Value nsValue =
  sendMessage nsValue scnVector4ValueSelector

-- | @- SCNMatrix4Value@
scnMatrix4Value :: IsNSValue nsValue => nsValue -> IO SCNMatrix4
scnMatrix4Value nsValue =
  sendMessage nsValue scnMatrix4ValueSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @valueWithSCNVector3:@
valueWithSCNVector3Selector :: Selector '[SCNVector3] (Id NSValue)
valueWithSCNVector3Selector = mkSelector "valueWithSCNVector3:"

-- | @Selector@ for @valueWithSCNVector4:@
valueWithSCNVector4Selector :: Selector '[SCNVector4] (Id NSValue)
valueWithSCNVector4Selector = mkSelector "valueWithSCNVector4:"

-- | @Selector@ for @valueWithSCNMatrix4:@
valueWithSCNMatrix4Selector :: Selector '[SCNMatrix4] (Id NSValue)
valueWithSCNMatrix4Selector = mkSelector "valueWithSCNMatrix4:"

-- | @Selector@ for @SCNVector3Value@
scnVector3ValueSelector :: Selector '[] SCNVector3
scnVector3ValueSelector = mkSelector "SCNVector3Value"

-- | @Selector@ for @SCNVector4Value@
scnVector4ValueSelector :: Selector '[] SCNVector4
scnVector4ValueSelector = mkSelector "SCNVector4Value"

-- | @Selector@ for @SCNMatrix4Value@
scnMatrix4ValueSelector :: Selector '[] SCNMatrix4
scnMatrix4ValueSelector = mkSelector "SCNMatrix4Value"

