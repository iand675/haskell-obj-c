{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @AVCaptionGrouper@.
module ObjC.AVFoundation.AVCaptionGrouper
  ( AVCaptionGrouper
  , IsAVCaptionGrouper(..)
  , addCaption
  , addCaptionSelector


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

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- addCaption:@
addCaption :: (IsAVCaptionGrouper avCaptionGrouper, IsAVCaption input) => avCaptionGrouper -> input -> IO ()
addCaption avCaptionGrouper  input =
withObjCPtr input $ \raw_input ->
    sendMsg avCaptionGrouper (mkSelector "addCaption:") retVoid [argPtr (castPtr raw_input :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @addCaption:@
addCaptionSelector :: Selector
addCaptionSelector = mkSelector "addCaption:"

