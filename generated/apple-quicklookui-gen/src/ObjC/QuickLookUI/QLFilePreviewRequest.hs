{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | This class contains information about the preview that should be provided.
--
-- Generated bindings for @QLFilePreviewRequest@.
module ObjC.QuickLookUI.QLFilePreviewRequest
  ( QLFilePreviewRequest
  , IsQLFilePreviewRequest(..)
  , fileURL
  , fileURLSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.QuickLookUI.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The url of the file for which a preview is being requested.
--
-- ObjC selector: @- fileURL@
fileURL :: IsQLFilePreviewRequest qlFilePreviewRequest => qlFilePreviewRequest -> IO (Id NSURL)
fileURL qlFilePreviewRequest =
  sendMessage qlFilePreviewRequest fileURLSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @fileURL@
fileURLSelector :: Selector '[] (Id NSURL)
fileURLSelector = mkSelector "fileURL"

