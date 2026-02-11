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
  , initWithURLTemplateSelector
  , geometryFlippedSelector
  , setGeometryFlippedSelector
  , minimumZSelector
  , setMinimumZSelector
  , maximumZSelector
  , setMaximumZSelector
  , urlTemplateSelector
  , canReplaceMapContentSelector
  , setCanReplaceMapContentSelector


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

import ObjC.MapKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithURLTemplate:@
initWithURLTemplate :: (IsMKTileOverlay mkTileOverlay, IsNSString urlTemplate) => mkTileOverlay -> urlTemplate -> IO (Id MKTileOverlay)
initWithURLTemplate mkTileOverlay  urlTemplate =
withObjCPtr urlTemplate $ \raw_urlTemplate ->
    sendMsg mkTileOverlay (mkSelector "initWithURLTemplate:") (retPtr retVoid) [argPtr (castPtr raw_urlTemplate :: Ptr ())] >>= ownedObject . castPtr

-- | @- geometryFlipped@
geometryFlipped :: IsMKTileOverlay mkTileOverlay => mkTileOverlay -> IO Bool
geometryFlipped mkTileOverlay  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mkTileOverlay (mkSelector "geometryFlipped") retCULong []

-- | @- setGeometryFlipped:@
setGeometryFlipped :: IsMKTileOverlay mkTileOverlay => mkTileOverlay -> Bool -> IO ()
setGeometryFlipped mkTileOverlay  value =
  sendMsg mkTileOverlay (mkSelector "setGeometryFlipped:") retVoid [argCULong (if value then 1 else 0)]

-- | @- minimumZ@
minimumZ :: IsMKTileOverlay mkTileOverlay => mkTileOverlay -> IO CLong
minimumZ mkTileOverlay  =
  sendMsg mkTileOverlay (mkSelector "minimumZ") retCLong []

-- | @- setMinimumZ:@
setMinimumZ :: IsMKTileOverlay mkTileOverlay => mkTileOverlay -> CLong -> IO ()
setMinimumZ mkTileOverlay  value =
  sendMsg mkTileOverlay (mkSelector "setMinimumZ:") retVoid [argCLong (fromIntegral value)]

-- | @- maximumZ@
maximumZ :: IsMKTileOverlay mkTileOverlay => mkTileOverlay -> IO CLong
maximumZ mkTileOverlay  =
  sendMsg mkTileOverlay (mkSelector "maximumZ") retCLong []

-- | @- setMaximumZ:@
setMaximumZ :: IsMKTileOverlay mkTileOverlay => mkTileOverlay -> CLong -> IO ()
setMaximumZ mkTileOverlay  value =
  sendMsg mkTileOverlay (mkSelector "setMaximumZ:") retVoid [argCLong (fromIntegral value)]

-- | @- URLTemplate@
urlTemplate :: IsMKTileOverlay mkTileOverlay => mkTileOverlay -> IO (Id NSString)
urlTemplate mkTileOverlay  =
  sendMsg mkTileOverlay (mkSelector "URLTemplate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- canReplaceMapContent@
canReplaceMapContent :: IsMKTileOverlay mkTileOverlay => mkTileOverlay -> IO Bool
canReplaceMapContent mkTileOverlay  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mkTileOverlay (mkSelector "canReplaceMapContent") retCULong []

-- | @- setCanReplaceMapContent:@
setCanReplaceMapContent :: IsMKTileOverlay mkTileOverlay => mkTileOverlay -> Bool -> IO ()
setCanReplaceMapContent mkTileOverlay  value =
  sendMsg mkTileOverlay (mkSelector "setCanReplaceMapContent:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithURLTemplate:@
initWithURLTemplateSelector :: Selector
initWithURLTemplateSelector = mkSelector "initWithURLTemplate:"

-- | @Selector@ for @geometryFlipped@
geometryFlippedSelector :: Selector
geometryFlippedSelector = mkSelector "geometryFlipped"

-- | @Selector@ for @setGeometryFlipped:@
setGeometryFlippedSelector :: Selector
setGeometryFlippedSelector = mkSelector "setGeometryFlipped:"

-- | @Selector@ for @minimumZ@
minimumZSelector :: Selector
minimumZSelector = mkSelector "minimumZ"

-- | @Selector@ for @setMinimumZ:@
setMinimumZSelector :: Selector
setMinimumZSelector = mkSelector "setMinimumZ:"

-- | @Selector@ for @maximumZ@
maximumZSelector :: Selector
maximumZSelector = mkSelector "maximumZ"

-- | @Selector@ for @setMaximumZ:@
setMaximumZSelector :: Selector
setMaximumZSelector = mkSelector "setMaximumZ:"

-- | @Selector@ for @URLTemplate@
urlTemplateSelector :: Selector
urlTemplateSelector = mkSelector "URLTemplate"

-- | @Selector@ for @canReplaceMapContent@
canReplaceMapContentSelector :: Selector
canReplaceMapContentSelector = mkSelector "canReplaceMapContent"

-- | @Selector@ for @setCanReplaceMapContent:@
setCanReplaceMapContentSelector :: Selector
setCanReplaceMapContentSelector = mkSelector "setCanReplaceMapContent:"

