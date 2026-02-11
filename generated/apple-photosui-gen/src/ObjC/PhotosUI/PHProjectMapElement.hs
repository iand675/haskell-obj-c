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
  , mapTypeSelector
  , headingSelector
  , pitchSelector
  , altitudeSelector
  , annotationsSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PhotosUI.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The type of the map in the project.
--
-- ObjC selector: @- mapType@
mapType :: IsPHProjectMapElement phProjectMapElement => phProjectMapElement -> IO CInt
mapType phProjectMapElement  =
    sendMsg phProjectMapElement (mkSelector "mapType") retCInt []

-- | @- heading@
heading :: IsPHProjectMapElement phProjectMapElement => phProjectMapElement -> IO CDouble
heading phProjectMapElement  =
    sendMsg phProjectMapElement (mkSelector "heading") retCDouble []

-- | @- pitch@
pitch :: IsPHProjectMapElement phProjectMapElement => phProjectMapElement -> IO CDouble
pitch phProjectMapElement  =
    sendMsg phProjectMapElement (mkSelector "pitch") retCDouble []

-- | @- altitude@
altitude :: IsPHProjectMapElement phProjectMapElement => phProjectMapElement -> IO CDouble
altitude phProjectMapElement  =
    sendMsg phProjectMapElement (mkSelector "altitude") retCDouble []

-- | @- annotations@
annotations :: IsPHProjectMapElement phProjectMapElement => phProjectMapElement -> IO (Id NSArray)
annotations phProjectMapElement  =
    sendMsg phProjectMapElement (mkSelector "annotations") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @mapType@
mapTypeSelector :: Selector
mapTypeSelector = mkSelector "mapType"

-- | @Selector@ for @heading@
headingSelector :: Selector
headingSelector = mkSelector "heading"

-- | @Selector@ for @pitch@
pitchSelector :: Selector
pitchSelector = mkSelector "pitch"

-- | @Selector@ for @altitude@
altitudeSelector :: Selector
altitudeSelector = mkSelector "altitude"

-- | @Selector@ for @annotations@
annotationsSelector :: Selector
annotationsSelector = mkSelector "annotations"

