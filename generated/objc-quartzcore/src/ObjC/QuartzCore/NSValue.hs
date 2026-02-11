{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSValue@.
module ObjC.QuartzCore.NSValue
  ( NSValue
  , IsNSValue(..)
  , valueWithCATransform3D
  , caTransform3DValue
  , valueWithCATransform3DSelector
  , caTransform3DValueSelector


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

import ObjC.QuartzCore.Internal.Classes
import ObjC.QuartzCore.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | @+ valueWithCATransform3D:@
valueWithCATransform3D :: CATransform3D -> IO (Id NSValue)
valueWithCATransform3D t =
  do
    cls' <- getRequiredClass "NSValue"
    sendClassMsg cls' (mkSelector "valueWithCATransform3D:") (retPtr retVoid) [argCATransform3D t] >>= retainedObject . castPtr

-- | @- CATransform3DValue@
caTransform3DValue :: IsNSValue nsValue => nsValue -> IO CATransform3D
caTransform3DValue nsValue  =
  sendMsgStret nsValue (mkSelector "CATransform3DValue") retCATransform3D []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @valueWithCATransform3D:@
valueWithCATransform3DSelector :: Selector
valueWithCATransform3DSelector = mkSelector "valueWithCATransform3D:"

-- | @Selector@ for @CATransform3DValue@
caTransform3DValueSelector :: Selector
caTransform3DValueSelector = mkSelector "CATransform3DValue"

