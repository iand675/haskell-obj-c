{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- addCaption:@
addCaption :: (IsAVCaptionGrouper avCaptionGrouper, IsAVCaption input) => avCaptionGrouper -> input -> IO ()
addCaption avCaptionGrouper input =
  sendMessage avCaptionGrouper addCaptionSelector (toAVCaption input)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @addCaption:@
addCaptionSelector :: Selector '[Id AVCaption] ()
addCaptionSelector = mkSelector "addCaption:"

