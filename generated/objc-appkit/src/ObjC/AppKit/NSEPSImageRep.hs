{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object that can render an image from encapsulated PostScript (EPS) code.
--
-- Generated bindings for @NSEPSImageRep@.
module ObjC.AppKit.NSEPSImageRep
  ( NSEPSImageRep
  , IsNSEPSImageRep(..)
  , imageRepWithData
  , initWithData
  , prepareGState
  , boundingBox
  , epsRepresentation
  , imageRepWithDataSelector
  , initWithDataSelector
  , prepareGStateSelector
  , boundingBoxSelector
  , epsRepresentationSelector


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

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | Creates and returns a representation of an image initialized with the specified EPS data. Convenience of @-initWithData:@. - Note: This method always returns @nil@ on macOS 14.0 and later.
--
-- ObjC selector: @+ imageRepWithData:@
imageRepWithData :: IsNSData epsData => epsData -> IO (Id NSEPSImageRep)
imageRepWithData epsData =
  do
    cls' <- getRequiredClass "NSEPSImageRep"
    withObjCPtr epsData $ \raw_epsData ->
      sendClassMsg cls' (mkSelector "imageRepWithData:") (retPtr retVoid) [argPtr (castPtr raw_epsData :: Ptr ())] >>= retainedObject . castPtr

-- | Returns a representation of an image initialized with the specified EPS data. - Note: This method always returns @nil@ on macOS 14.0 and later.
--
-- ObjC selector: @- initWithData:@
initWithData :: (IsNSEPSImageRep nsepsImageRep, IsNSData epsData) => nsepsImageRep -> epsData -> IO (Id NSEPSImageRep)
initWithData nsepsImageRep  epsData =
withObjCPtr epsData $ \raw_epsData ->
    sendMsg nsepsImageRep (mkSelector "initWithData:") (retPtr retVoid) [argPtr (castPtr raw_epsData :: Ptr ())] >>= ownedObject . castPtr

-- | The @-[NSEPSImageRep draw]@ method sends this message to itself just before rendering the EPS code. The default implementation of this method does nothing. It can be overridden in a subclass to prepare the graphics state as needed.
--
-- ObjC selector: @- prepareGState@
prepareGState :: IsNSEPSImageRep nsepsImageRep => nsepsImageRep -> IO ()
prepareGState nsepsImageRep  =
  sendMsg nsepsImageRep (mkSelector "prepareGState") retVoid []

-- | The rectangle that bounds the image representation.
--
-- ObjC selector: @- boundingBox@
boundingBox :: IsNSEPSImageRep nsepsImageRep => nsepsImageRep -> IO NSRect
boundingBox nsepsImageRep  =
  sendMsgStret nsepsImageRep (mkSelector "boundingBox") retNSRect []

-- | The EPS representation of the image representation.
--
-- ObjC selector: @- EPSRepresentation@
epsRepresentation :: IsNSEPSImageRep nsepsImageRep => nsepsImageRep -> IO (Id NSData)
epsRepresentation nsepsImageRep  =
  sendMsg nsepsImageRep (mkSelector "EPSRepresentation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @imageRepWithData:@
imageRepWithDataSelector :: Selector
imageRepWithDataSelector = mkSelector "imageRepWithData:"

-- | @Selector@ for @initWithData:@
initWithDataSelector :: Selector
initWithDataSelector = mkSelector "initWithData:"

-- | @Selector@ for @prepareGState@
prepareGStateSelector :: Selector
prepareGStateSelector = mkSelector "prepareGState"

-- | @Selector@ for @boundingBox@
boundingBoxSelector :: Selector
boundingBoxSelector = mkSelector "boundingBox"

-- | @Selector@ for @EPSRepresentation@
epsRepresentationSelector :: Selector
epsRepresentationSelector = mkSelector "EPSRepresentation"

