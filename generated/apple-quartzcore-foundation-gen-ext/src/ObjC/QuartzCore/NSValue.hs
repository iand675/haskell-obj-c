{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSValue@.
module ObjC.QuartzCore.NSValue
  ( NSValue
  , IsNSValue(..)
  , valueWithCATransform3D
  , caTransform3DValue
  , caTransform3DValueSelector
  , valueWithCATransform3DSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' valueWithCATransform3DSelector t

-- | @- CATransform3DValue@
caTransform3DValue :: IsNSValue nsValue => nsValue -> IO CATransform3D
caTransform3DValue nsValue =
  sendMessage nsValue caTransform3DValueSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @valueWithCATransform3D:@
valueWithCATransform3DSelector :: Selector '[CATransform3D] (Id NSValue)
valueWithCATransform3DSelector = mkSelector "valueWithCATransform3D:"

-- | @Selector@ for @CATransform3DValue@
caTransform3DValueSelector :: Selector '[] CATransform3D
caTransform3DValueSelector = mkSelector "CATransform3DValue"

