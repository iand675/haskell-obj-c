{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CIRenderTask@.
module ObjC.CoreImage.CIRenderTask
  ( CIRenderTask
  , IsCIRenderTask(..)
  , waitUntilCompletedAndReturnError
  , waitUntilCompletedAndReturnErrorSelector


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

import ObjC.CoreImage.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- waitUntilCompletedAndReturnError:@
waitUntilCompletedAndReturnError :: (IsCIRenderTask ciRenderTask, IsNSError error_) => ciRenderTask -> error_ -> IO (Id CIRenderInfo)
waitUntilCompletedAndReturnError ciRenderTask  error_ =
withObjCPtr error_ $ \raw_error_ ->
    sendMsg ciRenderTask (mkSelector "waitUntilCompletedAndReturnError:") (retPtr retVoid) [argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @waitUntilCompletedAndReturnError:@
waitUntilCompletedAndReturnErrorSelector :: Selector
waitUntilCompletedAndReturnErrorSelector = mkSelector "waitUntilCompletedAndReturnError:"

