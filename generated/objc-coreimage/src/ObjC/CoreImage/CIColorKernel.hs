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

-- | @+ kernelWithString:@
kernelWithString :: IsNSString string => string -> IO (Id CIColorKernel)
kernelWithString string =
  do
    cls' <- getRequiredClass "CIColorKernel"
    withObjCPtr string $ \raw_string ->
      sendClassMsg cls' (mkSelector "kernelWithString:") (retPtr retVoid) [argPtr (castPtr raw_string :: Ptr ())] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @kernelWithString:@
kernelWithStringSelector :: Selector
kernelWithStringSelector = mkSelector "kernelWithString:"

