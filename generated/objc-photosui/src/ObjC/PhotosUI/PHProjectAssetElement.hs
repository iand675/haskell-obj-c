{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A PHProjectAssetElement object represents a media asset within a PHProjectSectionContent. The underlying PHAsset can be accessed by converting the provided cloudAssetIdentifier to a localIdentifier, then using the fetchAssetsWithLocalIdentifiers:options: class method defined in PHAsset.h.
--
-- Generated bindings for @PHProjectAssetElement@.
module ObjC.PhotosUI.PHProjectAssetElement
  ( PHProjectAssetElement
  , IsPHProjectAssetElement(..)
  , cloudAssetIdentifier
  , annotation
  , regionsOfInterest
  , horizontallyFlipped
  , verticallyFlipped
  , cloudAssetIdentifierSelector
  , annotationSelector
  , regionsOfInterestSelector
  , horizontallyFlippedSelector
  , verticallyFlippedSelector


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
import ObjC.Photos.Internal.Classes

-- | Cloud identifier for the underlying PHAsset. This identifier must be converted to a localIdentifier before fetching, but if archiving the identifier in project data the provided PHCloudIdentifier should always be used.
--
-- ObjC selector: @- cloudAssetIdentifier@
cloudAssetIdentifier :: IsPHProjectAssetElement phProjectAssetElement => phProjectAssetElement -> IO (Id PHCloudIdentifier)
cloudAssetIdentifier phProjectAssetElement  =
  sendMsg phProjectAssetElement (mkSelector "cloudAssetIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | If a user has explicitly annotated an asset (e.g., caption) that value will be provided in this property.
--
-- ObjC selector: @- annotation@
annotation :: IsPHProjectAssetElement phProjectAssetElement => phProjectAssetElement -> IO (Id NSString)
annotation phProjectAssetElement  =
  sendMsg phProjectAssetElement (mkSelector "annotation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Array of regions of interest (faces, objects, etc.) in the assets. Note: Photos will filter out features of an asset that it doesn't believe to be meaningful in the context of the user's full library. For example, random faces in a crowd.
--
-- ObjC selector: @- regionsOfInterest@
regionsOfInterest :: IsPHProjectAssetElement phProjectAssetElement => phProjectAssetElement -> IO (Id NSArray)
regionsOfInterest phProjectAssetElement  =
  sendMsg phProjectAssetElement (mkSelector "regionsOfInterest") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The following properties are only used when the user creates a new project from an existing Apple Print Product.
--
-- YES if the asset was presented horizontally flipped in the originating project.
--
-- ObjC selector: @- horizontallyFlipped@
horizontallyFlipped :: IsPHProjectAssetElement phProjectAssetElement => phProjectAssetElement -> IO Bool
horizontallyFlipped phProjectAssetElement  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg phProjectAssetElement (mkSelector "horizontallyFlipped") retCULong []

-- | YES if the asset was presented vertically flipped in the originating project.
--
-- ObjC selector: @- verticallyFlipped@
verticallyFlipped :: IsPHProjectAssetElement phProjectAssetElement => phProjectAssetElement -> IO Bool
verticallyFlipped phProjectAssetElement  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg phProjectAssetElement (mkSelector "verticallyFlipped") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @cloudAssetIdentifier@
cloudAssetIdentifierSelector :: Selector
cloudAssetIdentifierSelector = mkSelector "cloudAssetIdentifier"

-- | @Selector@ for @annotation@
annotationSelector :: Selector
annotationSelector = mkSelector "annotation"

-- | @Selector@ for @regionsOfInterest@
regionsOfInterestSelector :: Selector
regionsOfInterestSelector = mkSelector "regionsOfInterest"

-- | @Selector@ for @horizontallyFlipped@
horizontallyFlippedSelector :: Selector
horizontallyFlippedSelector = mkSelector "horizontallyFlipped"

-- | @Selector@ for @verticallyFlipped@
verticallyFlippedSelector :: Selector
verticallyFlippedSelector = mkSelector "verticallyFlipped"

