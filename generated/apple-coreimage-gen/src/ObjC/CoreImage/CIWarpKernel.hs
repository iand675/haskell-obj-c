{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CIWarpKernel@.
module ObjC.CoreImage.CIWarpKernel
  ( CIWarpKernel
  , IsCIWarpKernel(..)
  , kernelWithString
  , kernelWithStringSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreImage.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ kernelWithString:@
kernelWithString :: IsNSString string => string -> IO (Id CIWarpKernel)
kernelWithString string =
  do
    cls' <- getRequiredClass "CIWarpKernel"
    sendClassMessage cls' kernelWithStringSelector (toNSString string)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @kernelWithString:@
kernelWithStringSelector :: Selector '[Id NSString] (Id CIWarpKernel)
kernelWithStringSelector = mkSelector "kernelWithString:"

