{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKTileOverlay@.
module ObjC.MapKit.MKTileOverlay
  ( MKTileOverlay
  , IsMKTileOverlay(..)
  , initWithURLTemplate
  , geometryFlipped
  , setGeometryFlipped
  , minimumZ
  , setMinimumZ
  , maximumZ
  , setMaximumZ
  , urlTemplate
  , canReplaceMapContent
  , setCanReplaceMapContent
  , canReplaceMapContentSelector
  , geometryFlippedSelector
  , initWithURLTemplateSelector
  , maximumZSelector
  , minimumZSelector
  , setCanReplaceMapContentSelector
  , setGeometryFlippedSelector
  , setMaximumZSelector
  , setMinimumZSelector
  , urlTemplateSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MapKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithURLTemplate:@
initWithURLTemplate :: (IsMKTileOverlay mkTileOverlay, IsNSString urlTemplate) => mkTileOverlay -> urlTemplate -> IO (Id MKTileOverlay)
initWithURLTemplate mkTileOverlay urlTemplate =
  sendOwnedMessage mkTileOverlay initWithURLTemplateSelector (toNSString urlTemplate)

-- | @- geometryFlipped@
geometryFlipped :: IsMKTileOverlay mkTileOverlay => mkTileOverlay -> IO Bool
geometryFlipped mkTileOverlay =
  sendMessage mkTileOverlay geometryFlippedSelector

-- | @- setGeometryFlipped:@
setGeometryFlipped :: IsMKTileOverlay mkTileOverlay => mkTileOverlay -> Bool -> IO ()
setGeometryFlipped mkTileOverlay value =
  sendMessage mkTileOverlay setGeometryFlippedSelector value

-- | @- minimumZ@
minimumZ :: IsMKTileOverlay mkTileOverlay => mkTileOverlay -> IO CLong
minimumZ mkTileOverlay =
  sendMessage mkTileOverlay minimumZSelector

-- | @- setMinimumZ:@
setMinimumZ :: IsMKTileOverlay mkTileOverlay => mkTileOverlay -> CLong -> IO ()
setMinimumZ mkTileOverlay value =
  sendMessage mkTileOverlay setMinimumZSelector value

-- | @- maximumZ@
maximumZ :: IsMKTileOverlay mkTileOverlay => mkTileOverlay -> IO CLong
maximumZ mkTileOverlay =
  sendMessage mkTileOverlay maximumZSelector

-- | @- setMaximumZ:@
setMaximumZ :: IsMKTileOverlay mkTileOverlay => mkTileOverlay -> CLong -> IO ()
setMaximumZ mkTileOverlay value =
  sendMessage mkTileOverlay setMaximumZSelector value

-- | @- URLTemplate@
urlTemplate :: IsMKTileOverlay mkTileOverlay => mkTileOverlay -> IO (Id NSString)
urlTemplate mkTileOverlay =
  sendMessage mkTileOverlay urlTemplateSelector

-- | @- canReplaceMapContent@
canReplaceMapContent :: IsMKTileOverlay mkTileOverlay => mkTileOverlay -> IO Bool
canReplaceMapContent mkTileOverlay =
  sendMessage mkTileOverlay canReplaceMapContentSelector

-- | @- setCanReplaceMapContent:@
setCanReplaceMapContent :: IsMKTileOverlay mkTileOverlay => mkTileOverlay -> Bool -> IO ()
setCanReplaceMapContent mkTileOverlay value =
  sendMessage mkTileOverlay setCanReplaceMapContentSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithURLTemplate:@
initWithURLTemplateSelector :: Selector '[Id NSString] (Id MKTileOverlay)
initWithURLTemplateSelector = mkSelector "initWithURLTemplate:"

-- | @Selector@ for @geometryFlipped@
geometryFlippedSelector :: Selector '[] Bool
geometryFlippedSelector = mkSelector "geometryFlipped"

-- | @Selector@ for @setGeometryFlipped:@
setGeometryFlippedSelector :: Selector '[Bool] ()
setGeometryFlippedSelector = mkSelector "setGeometryFlipped:"

-- | @Selector@ for @minimumZ@
minimumZSelector :: Selector '[] CLong
minimumZSelector = mkSelector "minimumZ"

-- | @Selector@ for @setMinimumZ:@
setMinimumZSelector :: Selector '[CLong] ()
setMinimumZSelector = mkSelector "setMinimumZ:"

-- | @Selector@ for @maximumZ@
maximumZSelector :: Selector '[] CLong
maximumZSelector = mkSelector "maximumZ"

-- | @Selector@ for @setMaximumZ:@
setMaximumZSelector :: Selector '[CLong] ()
setMaximumZSelector = mkSelector "setMaximumZ:"

-- | @Selector@ for @URLTemplate@
urlTemplateSelector :: Selector '[] (Id NSString)
urlTemplateSelector = mkSelector "URLTemplate"

-- | @Selector@ for @canReplaceMapContent@
canReplaceMapContentSelector :: Selector '[] Bool
canReplaceMapContentSelector = mkSelector "canReplaceMapContent"

-- | @Selector@ for @setCanReplaceMapContent:@
setCanReplaceMapContentSelector :: Selector '[Bool] ()
setCanReplaceMapContentSelector = mkSelector "setCanReplaceMapContent:"

