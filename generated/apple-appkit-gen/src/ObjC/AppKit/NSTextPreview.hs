{-# LANGUAGE DataKinds #-}
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
  , candidateRectsSelector
  , initSelector
  , initWithSnapshotImage_presentationFrameSelector
  , initWithSnapshotImage_presentationFrame_candidateRectsSelector
  , presentationFrameSelector
  , previewImageSelector


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

-- | Creates a text preview using the specified image and rectangles that indicate the portions of text to highlight.
--
-- - Parameters:     - snapshotImage: An image that contains the requested text from your view.     Create the image using a transparent background and the current rendering     attributes for your text.     - presentationFrame: A rectangle in the coordinate space of your text view.     The system uses this rectangle to place your image precisely over your     view’s actual text. Set its size to the size of your snapshot image, and     set its origin to the point that allows the system to place your image     directly over the text.     - candidateRects: An array of <doc://com.apple.documentation/documentation/foundation/nsvalue>     objects, each of which contains an <doc://com.apple.documentation/documentation/foundation/nsrect>     in the coordinate space of your text view. Each rectangle contains a     bounding rectangle for text that is part of the preview. When applying     visual effects, the system adds highlights only to the text in the specified rectangles.
--
-- ObjC selector: @- initWithSnapshotImage:presentationFrame:candidateRects:@
initWithSnapshotImage_presentationFrame_candidateRects :: (IsNSTextPreview nsTextPreview, IsNSArray candidateRects) => nsTextPreview -> Ptr () -> NSRect -> candidateRects -> IO (Id NSTextPreview)
initWithSnapshotImage_presentationFrame_candidateRects nsTextPreview snapshotImage presentationFrame candidateRects =
  sendOwnedMessage nsTextPreview initWithSnapshotImage_presentationFrame_candidateRectsSelector snapshotImage presentationFrame (toNSArray candidateRects)

-- | Creates a text preview using the specified image.
--
-- - Parameters:     - snapshotImage: An image that contains the requested text from your view.     Create the image using a transparent background and the current rendering     attributes for your text.     - presentationFrame: A rectangle in your frame’s coordinate space. The     system uses this rectangle to place your image precisely over your view’s     actual text. Set its size to the size of your snapshot image, and set its     origin to the point that allows the system to place your image directly over the text.
--
-- ObjC selector: @- initWithSnapshotImage:presentationFrame:@
initWithSnapshotImage_presentationFrame :: IsNSTextPreview nsTextPreview => nsTextPreview -> Ptr () -> NSRect -> IO (Id NSTextPreview)
initWithSnapshotImage_presentationFrame nsTextPreview snapshotImage presentationFrame =
  sendOwnedMessage nsTextPreview initWithSnapshotImage_presentationFrameSelector snapshotImage presentationFrame

-- | @- init@
init_ :: IsNSTextPreview nsTextPreview => nsTextPreview -> IO (Id NSTextPreview)
init_ nsTextPreview =
  sendOwnedMessage nsTextPreview initSelector

-- | The image that contains the requested text from your view.
--
-- You specify this image at initialization time. The system uses it to implement any visual effects involving your view’s text. Create the image with your text on a transparent background.
--
-- ObjC selector: @- previewImage@
previewImage :: IsNSTextPreview nsTextPreview => nsTextPreview -> IO (Ptr ())
previewImage nsTextPreview =
  sendMessage nsTextPreview previewImageSelector

-- | The frame rectangle that places the preview image directly over the matching text.
--
-- You specify this value at initialization time. The system uses it to position the preview image over the text in your view. Make sure the frame rectangle is in your view's coordinate space.
--
-- ObjC selector: @- presentationFrame@
presentationFrame :: IsNSTextPreview nsTextPreview => nsTextPreview -> IO NSRect
presentationFrame nsTextPreview =
  sendMessage nsTextPreview presentationFrameSelector

-- | Rectangles that define the specific portions of text to highlight.
--
-- At initialization time, you set this property to an array of <doc://com.apple.documentation/documentation/foundation/nsvalue> objects, each of which contains an <doc://com.apple.documentation/documentation/foundation/nsrect> in the coordinate space of the target view. Each rectangle contains a bounding rectangle for text that is part of the preview. When applying visual effects, the system adds highlights only to the text in the specified rectangles.
--
-- ObjC selector: @- candidateRects@
candidateRects :: IsNSTextPreview nsTextPreview => nsTextPreview -> IO (Id NSArray)
candidateRects nsTextPreview =
  sendMessage nsTextPreview candidateRectsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithSnapshotImage:presentationFrame:candidateRects:@
initWithSnapshotImage_presentationFrame_candidateRectsSelector :: Selector '[Ptr (), NSRect, Id NSArray] (Id NSTextPreview)
initWithSnapshotImage_presentationFrame_candidateRectsSelector = mkSelector "initWithSnapshotImage:presentationFrame:candidateRects:"

-- | @Selector@ for @initWithSnapshotImage:presentationFrame:@
initWithSnapshotImage_presentationFrameSelector :: Selector '[Ptr (), NSRect] (Id NSTextPreview)
initWithSnapshotImage_presentationFrameSelector = mkSelector "initWithSnapshotImage:presentationFrame:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSTextPreview)
initSelector = mkSelector "init"

-- | @Selector@ for @previewImage@
previewImageSelector :: Selector '[] (Ptr ())
previewImageSelector = mkSelector "previewImage"

-- | @Selector@ for @presentationFrame@
presentationFrameSelector :: Selector '[] NSRect
presentationFrameSelector = mkSelector "presentationFrame"

-- | @Selector@ for @candidateRects@
candidateRectsSelector :: Selector '[] (Id NSArray)
candidateRectsSelector = mkSelector "candidateRects"

