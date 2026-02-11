{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A snapshot of the text in your view, which the system uses to create user-visible effects.
--
-- An @NSTextPreview@ object provides a static image of your view’s text content that the system can use to create animations. You provide preview objects in response to system requests, such as ones from Writing Tools. In addition to creating an image of your view’s text, you also specify the location of that text in your view’s frame rectangle. When creating animations, the system places the image on top of your view’s content and animates changes to the image instead of to your view.
--
-- Create an @NSTextPreview@ object in response to specific system requests. Create an image with a transparent background and render your view’s text into the image using the current text attributes. Construct your @NSTextPreview@ object with both the image and the frame rectangle that represents the location of the rendered text in your view’s coordinate system. To highlight specific portions of text, instead of all the text in the image, provide a set of candidate rectangles with the locations of the text you want to highlight.
--
-- Generated bindings for @NSTextPreview@.
module ObjC.AppKit.NSTextPreview
  ( NSTextPreview
  , IsNSTextPreview(..)
  , initWithSnapshotImage_presentationFrame_candidateRects
  , initWithSnapshotImage_presentationFrame
  , init_
  , previewImage
  , presentationFrame
  , candidateRects
  , initWithSnapshotImage_presentationFrame_candidateRectsSelector
  , initWithSnapshotImage_presentationFrameSelector
  , initSelector
  , previewImageSelector
  , presentationFrameSelector
  , candidateRectsSelector


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

-- | Creates a text preview using the specified image and rectangles that indicate the portions of text to highlight.
--
-- - Parameters:     - snapshotImage: An image that contains the requested text from your view.     Create the image using a transparent background and the current rendering     attributes for your text.     - presentationFrame: A rectangle in the coordinate space of your text view.     The system uses this rectangle to place your image precisely over your     view’s actual text. Set its size to the size of your snapshot image, and     set its origin to the point that allows the system to place your image     directly over the text.     - candidateRects: An array of <doc://com.apple.documentation/documentation/foundation/nsvalue>     objects, each of which contains an <doc://com.apple.documentation/documentation/foundation/nsrect>     in the coordinate space of your text view. Each rectangle contains a     bounding rectangle for text that is part of the preview. When applying     visual effects, the system adds highlights only to the text in the specified rectangles.
--
-- ObjC selector: @- initWithSnapshotImage:presentationFrame:candidateRects:@
initWithSnapshotImage_presentationFrame_candidateRects :: (IsNSTextPreview nsTextPreview, IsNSArray candidateRects) => nsTextPreview -> Ptr () -> NSRect -> candidateRects -> IO (Id NSTextPreview)
initWithSnapshotImage_presentationFrame_candidateRects nsTextPreview  snapshotImage presentationFrame candidateRects =
withObjCPtr candidateRects $ \raw_candidateRects ->
    sendMsg nsTextPreview (mkSelector "initWithSnapshotImage:presentationFrame:candidateRects:") (retPtr retVoid) [argPtr snapshotImage, argNSRect presentationFrame, argPtr (castPtr raw_candidateRects :: Ptr ())] >>= ownedObject . castPtr

-- | Creates a text preview using the specified image.
--
-- - Parameters:     - snapshotImage: An image that contains the requested text from your view.     Create the image using a transparent background and the current rendering     attributes for your text.     - presentationFrame: A rectangle in your frame’s coordinate space. The     system uses this rectangle to place your image precisely over your view’s     actual text. Set its size to the size of your snapshot image, and set its     origin to the point that allows the system to place your image directly over the text.
--
-- ObjC selector: @- initWithSnapshotImage:presentationFrame:@
initWithSnapshotImage_presentationFrame :: IsNSTextPreview nsTextPreview => nsTextPreview -> Ptr () -> NSRect -> IO (Id NSTextPreview)
initWithSnapshotImage_presentationFrame nsTextPreview  snapshotImage presentationFrame =
  sendMsg nsTextPreview (mkSelector "initWithSnapshotImage:presentationFrame:") (retPtr retVoid) [argPtr snapshotImage, argNSRect presentationFrame] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsNSTextPreview nsTextPreview => nsTextPreview -> IO (Id NSTextPreview)
init_ nsTextPreview  =
  sendMsg nsTextPreview (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The image that contains the requested text from your view.
--
-- You specify this image at initialization time. The system uses it to implement any visual effects involving your view’s text. Create the image with your text on a transparent background.
--
-- ObjC selector: @- previewImage@
previewImage :: IsNSTextPreview nsTextPreview => nsTextPreview -> IO (Ptr ())
previewImage nsTextPreview  =
  fmap castPtr $ sendMsg nsTextPreview (mkSelector "previewImage") (retPtr retVoid) []

-- | The frame rectangle that places the preview image directly over the matching text.
--
-- You specify this value at initialization time. The system uses it to position the preview image over the text in your view. Make sure the frame rectangle is in your view's coordinate space.
--
-- ObjC selector: @- presentationFrame@
presentationFrame :: IsNSTextPreview nsTextPreview => nsTextPreview -> IO NSRect
presentationFrame nsTextPreview  =
  sendMsgStret nsTextPreview (mkSelector "presentationFrame") retNSRect []

-- | Rectangles that define the specific portions of text to highlight.
--
-- At initialization time, you set this property to an array of <doc://com.apple.documentation/documentation/foundation/nsvalue> objects, each of which contains an <doc://com.apple.documentation/documentation/foundation/nsrect> in the coordinate space of the target view. Each rectangle contains a bounding rectangle for text that is part of the preview. When applying visual effects, the system adds highlights only to the text in the specified rectangles.
--
-- ObjC selector: @- candidateRects@
candidateRects :: IsNSTextPreview nsTextPreview => nsTextPreview -> IO (Id NSArray)
candidateRects nsTextPreview  =
  sendMsg nsTextPreview (mkSelector "candidateRects") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithSnapshotImage:presentationFrame:candidateRects:@
initWithSnapshotImage_presentationFrame_candidateRectsSelector :: Selector
initWithSnapshotImage_presentationFrame_candidateRectsSelector = mkSelector "initWithSnapshotImage:presentationFrame:candidateRects:"

-- | @Selector@ for @initWithSnapshotImage:presentationFrame:@
initWithSnapshotImage_presentationFrameSelector :: Selector
initWithSnapshotImage_presentationFrameSelector = mkSelector "initWithSnapshotImage:presentationFrame:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @previewImage@
previewImageSelector :: Selector
previewImageSelector = mkSelector "previewImage"

-- | @Selector@ for @presentationFrame@
presentationFrameSelector :: Selector
presentationFrameSelector = mkSelector "presentationFrame"

-- | @Selector@ for @candidateRects@
candidateRectsSelector :: Selector
candidateRectsSelector = mkSelector "candidateRects"

