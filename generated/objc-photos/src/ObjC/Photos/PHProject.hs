{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PHProject@.
module ObjC.Photos.PHProject
  ( PHProject
  , IsPHProject(..)
  , projectExtensionData
  , hasProjectPreview
  , projectExtensionDataSelector
  , hasProjectPreviewSelector


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

import ObjC.Photos.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- projectExtensionData@
projectExtensionData :: IsPHProject phProject => phProject -> IO (Id NSData)
projectExtensionData phProject  =
  sendMsg phProject (mkSelector "projectExtensionData") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Property to determine if a project preview was previously set. Use -[PHProjectChangeRequest setProjectPreviewImage:] to set a project preview.
--
-- ObjC selector: @- hasProjectPreview@
hasProjectPreview :: IsPHProject phProject => phProject -> IO Bool
hasProjectPreview phProject  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg phProject (mkSelector "hasProjectPreview") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @projectExtensionData@
projectExtensionDataSelector :: Selector
projectExtensionDataSelector = mkSelector "projectExtensionData"

-- | @Selector@ for @hasProjectPreview@
hasProjectPreviewSelector :: Selector
hasProjectPreviewSelector = mkSelector "hasProjectPreview"

