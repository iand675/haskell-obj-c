{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
contents icCameraFolder =
  sendMessage icCameraFolder contentsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @contents@
contentsSelector :: Selector '[] (Id NSArray)
contentsSelector = mkSelector "contents"

