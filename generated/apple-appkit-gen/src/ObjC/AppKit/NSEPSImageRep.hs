{-# LANGUAGE DataKinds #-}
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
  , boundingBoxSelector
  , epsRepresentationSelector
  , imageRepWithDataSelector
  , initWithDataSelector
  , prepareGStateSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' imageRepWithDataSelector (toNSData epsData)

-- | Returns a representation of an image initialized with the specified EPS data. - Note: This method always returns @nil@ on macOS 14.0 and later.
--
-- ObjC selector: @- initWithData:@
initWithData :: (IsNSEPSImageRep nsepsImageRep, IsNSData epsData) => nsepsImageRep -> epsData -> IO (Id NSEPSImageRep)
initWithData nsepsImageRep epsData =
  sendOwnedMessage nsepsImageRep initWithDataSelector (toNSData epsData)

-- | The @-[NSEPSImageRep draw]@ method sends this message to itself just before rendering the EPS code. The default implementation of this method does nothing. It can be overridden in a subclass to prepare the graphics state as needed.
--
-- ObjC selector: @- prepareGState@
prepareGState :: IsNSEPSImageRep nsepsImageRep => nsepsImageRep -> IO ()
prepareGState nsepsImageRep =
  sendMessage nsepsImageRep prepareGStateSelector

-- | The rectangle that bounds the image representation.
--
-- ObjC selector: @- boundingBox@
boundingBox :: IsNSEPSImageRep nsepsImageRep => nsepsImageRep -> IO NSRect
boundingBox nsepsImageRep =
  sendMessage nsepsImageRep boundingBoxSelector

-- | The EPS representation of the image representation.
--
-- ObjC selector: @- EPSRepresentation@
epsRepresentation :: IsNSEPSImageRep nsepsImageRep => nsepsImageRep -> IO (Id NSData)
epsRepresentation nsepsImageRep =
  sendMessage nsepsImageRep epsRepresentationSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @imageRepWithData:@
imageRepWithDataSelector :: Selector '[Id NSData] (Id NSEPSImageRep)
imageRepWithDataSelector = mkSelector "imageRepWithData:"

-- | @Selector@ for @initWithData:@
initWithDataSelector :: Selector '[Id NSData] (Id NSEPSImageRep)
initWithDataSelector = mkSelector "initWithData:"

-- | @Selector@ for @prepareGState@
prepareGStateSelector :: Selector '[] ()
prepareGStateSelector = mkSelector "prepareGState"

-- | @Selector@ for @boundingBox@
boundingBoxSelector :: Selector '[] NSRect
boundingBoxSelector = mkSelector "boundingBox"

-- | @Selector@ for @EPSRepresentation@
epsRepresentationSelector :: Selector '[] (Id NSData)
epsRepresentationSelector = mkSelector "EPSRepresentation"

