{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CIColorKernel@.
module ObjC.CoreImage.CIColorKernel
  ( CIColorKernel
  , IsCIColorKernel(..)
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
kernelWithString :: IsNSString string => string -> IO (Id CIColorKernel)
kernelWithString string =
  do
    cls' <- getRequiredClass "CIColorKernel"
    sendClassMessage cls' kernelWithStringSelector (toNSString string)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @kernelWithString:@
kernelWithStringSelector :: Selector '[Id NSString] (Id CIColorKernel)
kernelWithStringSelector = mkSelector "kernelWithString:"

