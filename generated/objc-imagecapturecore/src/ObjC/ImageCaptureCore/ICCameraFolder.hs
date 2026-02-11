{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | ICCameraFolder
--
-- This class represents a folder on an ICCameraDevice object.
--
-- Generated bindings for @ICCameraFolder@.
module ObjC.ImageCaptureCore.ICCameraFolder
  ( ICCameraFolder
  , IsICCameraFolder(..)
  , contents
  , contentsSelector


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

import ObjC.ImageCaptureCore.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | contents
--
-- ￼A list of items contained by this folder.
--
-- ObjC selector: @- contents@
contents :: IsICCameraFolder icCameraFolder => icCameraFolder -> IO (Id NSArray)
contents icCameraFolder  =
  sendMsg icCameraFolder (mkSelector "contents") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @contents@
contentsSelector :: Selector
contentsSelector = mkSelector "contents"

