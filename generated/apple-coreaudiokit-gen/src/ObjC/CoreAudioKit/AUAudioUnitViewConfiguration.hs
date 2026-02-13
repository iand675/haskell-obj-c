{-# LANGUAGE DataKinds #-}
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
  , heightSelector
  , hostHasControllerSelector
  , initWithWidth_height_hostHasControllerSelector
  , widthSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
initWithWidth_height_hostHasController auAudioUnitViewConfiguration width height hostHasController =
  sendOwnedMessage auAudioUnitViewConfiguration initWithWidth_height_hostHasControllerSelector width height hostHasController

-- | width
--
-- The width of the view, measured in points.
--
-- Setting the width to 0 will match any width.
--
-- ObjC selector: @- width@
width :: IsAUAudioUnitViewConfiguration auAudioUnitViewConfiguration => auAudioUnitViewConfiguration -> IO CDouble
width auAudioUnitViewConfiguration =
  sendMessage auAudioUnitViewConfiguration widthSelector

-- | height
--
-- The height of the view, measured in points.
--
-- Setting the height to 0 will match any height.
--
-- ObjC selector: @- height@
height :: IsAUAudioUnitViewConfiguration auAudioUnitViewConfiguration => auAudioUnitViewConfiguration -> IO CDouble
height auAudioUnitViewConfiguration =
  sendMessage auAudioUnitViewConfiguration heightSelector

-- | hostHasController
--
-- Boolean property specifying whether the host displays its own control surface				when showing the view of the audio unit.
--
-- ObjC selector: @- hostHasController@
hostHasController :: IsAUAudioUnitViewConfiguration auAudioUnitViewConfiguration => auAudioUnitViewConfiguration -> IO Bool
hostHasController auAudioUnitViewConfiguration =
  sendMessage auAudioUnitViewConfiguration hostHasControllerSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithWidth:height:hostHasController:@
initWithWidth_height_hostHasControllerSelector :: Selector '[CDouble, CDouble, Bool] (Id AUAudioUnitViewConfiguration)
initWithWidth_height_hostHasControllerSelector = mkSelector "initWithWidth:height:hostHasController:"

-- | @Selector@ for @width@
widthSelector :: Selector '[] CDouble
widthSelector = mkSelector "width"

-- | @Selector@ for @height@
heightSelector :: Selector '[] CDouble
heightSelector = mkSelector "height"

-- | @Selector@ for @hostHasController@
hostHasControllerSelector :: Selector '[] Bool
hostHasControllerSelector = mkSelector "hostHasController"

