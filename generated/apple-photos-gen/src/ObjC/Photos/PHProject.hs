{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PHProject@.
module ObjC.Photos.PHProject
  ( PHProject
  , IsPHProject(..)
  , projectExtensionData
  , hasProjectPreview
  , hasProjectPreviewSelector
  , projectExtensionDataSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Photos.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- projectExtensionData@
projectExtensionData :: IsPHProject phProject => phProject -> IO (Id NSData)
projectExtensionData phProject =
  sendMessage phProject projectExtensionDataSelector

-- | Property to determine if a project preview was previously set. Use -[PHProjectChangeRequest setProjectPreviewImage:] to set a project preview.
--
-- ObjC selector: @- hasProjectPreview@
hasProjectPreview :: IsPHProject phProject => phProject -> IO Bool
hasProjectPreview phProject =
  sendMessage phProject hasProjectPreviewSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @projectExtensionData@
projectExtensionDataSelector :: Selector '[] (Id NSData)
projectExtensionDataSelector = mkSelector "projectExtensionData"

-- | @Selector@ for @hasProjectPreview@
hasProjectPreviewSelector :: Selector '[] Bool
hasProjectPreviewSelector = mkSelector "hasProjectPreview"

