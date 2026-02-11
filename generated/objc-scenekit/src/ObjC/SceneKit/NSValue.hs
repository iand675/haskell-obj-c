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
  , valueWithSCNVector3Selector
  , valueWithSCNVector4Selector
  , valueWithSCNMatrix4Selector
  , scnVector3ValueSelector
  , scnVector4ValueSelector
  , scnMatrix4ValueSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
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
    sendClassMsg cls' (mkSelector "valueWithSCNVector3:") (retPtr retVoid) [argSCNVector3 v] >>= retainedObject . castPtr

-- | @+ valueWithSCNVector4:@
valueWithSCNVector4 :: SCNVector4 -> IO (Id NSValue)
valueWithSCNVector4 v =
  do
    cls' <- getRequiredClass "NSValue"
    sendClassMsg cls' (mkSelector "valueWithSCNVector4:") (retPtr retVoid) [argSCNVector4 v] >>= retainedObject . castPtr

-- | @+ valueWithSCNMatrix4:@
valueWithSCNMatrix4 :: SCNMatrix4 -> IO (Id NSValue)
valueWithSCNMatrix4 v =
  do
    cls' <- getRequiredClass "NSValue"
    sendClassMsg cls' (mkSelector "valueWithSCNMatrix4:") (retPtr retVoid) [argSCNMatrix4 v] >>= retainedObject . castPtr

-- | @- SCNVector3Value@
scnVector3Value :: IsNSValue nsValue => nsValue -> IO SCNVector3
scnVector3Value nsValue  =
  sendMsgStret nsValue (mkSelector "SCNVector3Value") retSCNVector3 []

-- | @- SCNVector4Value@
scnVector4Value :: IsNSValue nsValue => nsValue -> IO SCNVector4
scnVector4Value nsValue  =
  sendMsgStret nsValue (mkSelector "SCNVector4Value") retSCNVector4 []

-- | @- SCNMatrix4Value@
scnMatrix4Value :: IsNSValue nsValue => nsValue -> IO SCNMatrix4
scnMatrix4Value nsValue  =
  sendMsgStret nsValue (mkSelector "SCNMatrix4Value") retSCNMatrix4 []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @valueWithSCNVector3:@
valueWithSCNVector3Selector :: Selector
valueWithSCNVector3Selector = mkSelector "valueWithSCNVector3:"

-- | @Selector@ for @valueWithSCNVector4:@
valueWithSCNVector4Selector :: Selector
valueWithSCNVector4Selector = mkSelector "valueWithSCNVector4:"

-- | @Selector@ for @valueWithSCNMatrix4:@
valueWithSCNMatrix4Selector :: Selector
valueWithSCNMatrix4Selector = mkSelector "valueWithSCNMatrix4:"

-- | @Selector@ for @SCNVector3Value@
scnVector3ValueSelector :: Selector
scnVector3ValueSelector = mkSelector "SCNVector3Value"

-- | @Selector@ for @SCNVector4Value@
scnVector4ValueSelector :: Selector
scnVector4ValueSelector = mkSelector "SCNVector4Value"

-- | @Selector@ for @SCNMatrix4Value@
scnMatrix4ValueSelector :: Selector
scnMatrix4ValueSelector = mkSelector "SCNMatrix4Value"

