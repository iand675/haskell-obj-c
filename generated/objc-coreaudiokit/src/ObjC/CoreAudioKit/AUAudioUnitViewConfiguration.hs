{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AUAudioUnitViewConfiguration
--
-- Properties of the configuration that a host uses to embed the view of an audio unit.
--
-- Hosts may support embedding the view of an audio unit in different configurations. These		configurations may vary in the size reserved for the audio unit's view and the additional 		control surfaces that are displayed along with it. The host can propose several view 		configurations and the audio unit should report the ones which it supports.
--
-- See the documentation for supportedViewConfigurations.
--
-- Generated bindings for @AUAudioUnitViewConfiguration@.
module ObjC.CoreAudioKit.AUAudioUnitViewConfiguration
  ( AUAudioUnitViewConfiguration
  , IsAUAudioUnitViewConfiguration(..)
  , initWithWidth_height_hostHasController
  , width
  , height
  , hostHasController
  , initWithWidth_height_hostHasControllerSelector
  , widthSelector
  , heightSelector
  , hostHasControllerSelector


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
import ObjC.Foundation.Internal.Classes

-- | initWithWidth
--
-- Designated initializer.
--
-- @width@ — The width associated with this view configuration.
--
-- @height@ — The height associated with this view configuration.
--
-- @hostHasController@ — This property controls whether the host shows its own control surface in this view 		configuration.
--
-- Returns: Returns the newly created view configuration object.
--
-- ObjC selector: @- initWithWidth:height:hostHasController:@
initWithWidth_height_hostHasController :: IsAUAudioUnitViewConfiguration auAudioUnitViewConfiguration => auAudioUnitViewConfiguration -> CDouble -> CDouble -> Bool -> IO (Id AUAudioUnitViewConfiguration)
initWithWidth_height_hostHasController auAudioUnitViewConfiguration  width height hostHasController =
  sendMsg auAudioUnitViewConfiguration (mkSelector "initWithWidth:height:hostHasController:") (retPtr retVoid) [argCDouble (fromIntegral width), argCDouble (fromIntegral height), argCULong (if hostHasController then 1 else 0)] >>= ownedObject . castPtr

-- | width
--
-- The width of the view, measured in points.
--
-- Setting the width to 0 will match any width.
--
-- ObjC selector: @- width@
width :: IsAUAudioUnitViewConfiguration auAudioUnitViewConfiguration => auAudioUnitViewConfiguration -> IO CDouble
width auAudioUnitViewConfiguration  =
  sendMsg auAudioUnitViewConfiguration (mkSelector "width") retCDouble []

-- | height
--
-- The height of the view, measured in points.
--
-- Setting the height to 0 will match any height.
--
-- ObjC selector: @- height@
height :: IsAUAudioUnitViewConfiguration auAudioUnitViewConfiguration => auAudioUnitViewConfiguration -> IO CDouble
height auAudioUnitViewConfiguration  =
  sendMsg auAudioUnitViewConfiguration (mkSelector "height") retCDouble []

-- | hostHasController
--
-- Boolean property specifying whether the host displays its own control surface				when showing the view of the audio unit.
--
-- ObjC selector: @- hostHasController@
hostHasController :: IsAUAudioUnitViewConfiguration auAudioUnitViewConfiguration => auAudioUnitViewConfiguration -> IO Bool
hostHasController auAudioUnitViewConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg auAudioUnitViewConfiguration (mkSelector "hostHasController") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithWidth:height:hostHasController:@
initWithWidth_height_hostHasControllerSelector :: Selector
initWithWidth_height_hostHasControllerSelector = mkSelector "initWithWidth:height:hostHasController:"

-- | @Selector@ for @width@
widthSelector :: Selector
widthSelector = mkSelector "width"

-- | @Selector@ for @height@
heightSelector :: Selector
heightSelector = mkSelector "height"

-- | @Selector@ for @hostHasController@
hostHasControllerSelector :: Selector
hostHasControllerSelector = mkSelector "hostHasController"

