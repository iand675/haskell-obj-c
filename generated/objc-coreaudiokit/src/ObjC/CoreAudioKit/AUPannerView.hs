{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AUPannerView
--
-- An AUPannerView object retrieves and instantiates a generic panner view for the given panner unit
--
-- Generated bindings for @AUPannerView@.
module ObjC.CoreAudioKit.AUPannerView
  ( AUPannerView
  , IsAUPannerView(..)
  , auPannerViewWithAudioUnit
  , audioUnit
  , auPannerViewWithAudioUnitSelector
  , audioUnitSelector


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

import ObjC.CoreAudioKit.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | +AUPannerViewWithAudioUnit:
--
-- Static constructor used to create the view
--
-- @au@ — The Panner Audio Unit associated with the view
--
-- Returns: Returns the newly created view object autoreleased or nil on error
--
-- ObjC selector: @+ AUPannerViewWithAudioUnit:@
auPannerViewWithAudioUnit :: Ptr () -> IO (Id AUPannerView)
auPannerViewWithAudioUnit au =
  do
    cls' <- getRequiredClass "AUPannerView"
    sendClassMsg cls' (mkSelector "AUPannerViewWithAudioUnit:") (retPtr retVoid) [argPtr au] >>= retainedObject . castPtr

-- | audioUnit
--
-- Read-only property for the audio unit associated with the view
--
-- Returns: The audio unit associated with the generic panner view
--
-- ObjC selector: @- audioUnit@
audioUnit :: IsAUPannerView auPannerView => auPannerView -> IO (Ptr ())
audioUnit auPannerView  =
  fmap castPtr $ sendMsg auPannerView (mkSelector "audioUnit") (retPtr retVoid) []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @AUPannerViewWithAudioUnit:@
auPannerViewWithAudioUnitSelector :: Selector
auPannerViewWithAudioUnitSelector = mkSelector "AUPannerViewWithAudioUnit:"

-- | @Selector@ for @audioUnit@
audioUnitSelector :: Selector
audioUnitSelector = mkSelector "audioUnit"

