{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A PHProjectMapElement object representing a map with annotations. In general, these will only be included for projects created from existing Apple Print Product projects.
--
-- Generated bindings for @PHProjectMapElement@.
module ObjC.PhotosUI.PHProjectMapElement
  ( PHProjectMapElement
  , IsPHProjectMapElement(..)
  , mapType
  , heading
  , pitch
  , altitude
  , annotations
  , altitudeSelector
  , annotationsSelector
  , headingSelector
  , mapTypeSelector
  , pitchSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PhotosUI.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The type of the map in the project.
--
-- ObjC selector: @- mapType@
mapType :: IsPHProjectMapElement phProjectMapElement => phProjectMapElement -> IO CInt
mapType phProjectMapElement =
  sendMessage phProjectMapElement mapTypeSelector

-- | @- heading@
heading :: IsPHProjectMapElement phProjectMapElement => phProjectMapElement -> IO CDouble
heading phProjectMapElement =
  sendMessage phProjectMapElement headingSelector

-- | @- pitch@
pitch :: IsPHProjectMapElement phProjectMapElement => phProjectMapElement -> IO CDouble
pitch phProjectMapElement =
  sendMessage phProjectMapElement pitchSelector

-- | @- altitude@
altitude :: IsPHProjectMapElement phProjectMapElement => phProjectMapElement -> IO CDouble
altitude phProjectMapElement =
  sendMessage phProjectMapElement altitudeSelector

-- | @- annotations@
annotations :: IsPHProjectMapElement phProjectMapElement => phProjectMapElement -> IO (Id NSArray)
annotations phProjectMapElement =
  sendMessage phProjectMapElement annotationsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @mapType@
mapTypeSelector :: Selector '[] CInt
mapTypeSelector = mkSelector "mapType"

-- | @Selector@ for @heading@
headingSelector :: Selector '[] CDouble
headingSelector = mkSelector "heading"

-- | @Selector@ for @pitch@
pitchSelector :: Selector '[] CDouble
pitchSelector = mkSelector "pitch"

-- | @Selector@ for @altitude@
altitudeSelector :: Selector '[] CDouble
altitudeSelector = mkSelector "altitude"

-- | @Selector@ for @annotations@
annotationsSelector :: Selector '[] (Id NSArray)
annotationsSelector = mkSelector "annotations"

