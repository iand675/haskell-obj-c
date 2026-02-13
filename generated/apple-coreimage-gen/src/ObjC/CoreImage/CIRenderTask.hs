{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreImage.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- waitUntilCompletedAndReturnError:@
waitUntilCompletedAndReturnError :: (IsCIRenderTask ciRenderTask, IsNSError error_) => ciRenderTask -> error_ -> IO (Id CIRenderInfo)
waitUntilCompletedAndReturnError ciRenderTask error_ =
  sendMessage ciRenderTask waitUntilCompletedAndReturnErrorSelector (toNSError error_)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @waitUntilCompletedAndReturnError:@
waitUntilCompletedAndReturnErrorSelector :: Selector '[Id NSError] (Id CIRenderInfo)
waitUntilCompletedAndReturnErrorSelector = mkSelector "waitUntilCompletedAndReturnError:"

