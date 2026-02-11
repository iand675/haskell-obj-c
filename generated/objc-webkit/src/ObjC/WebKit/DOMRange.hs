{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMRange@.
module ObjC.WebKit.DOMRange
  ( DOMRange
  , IsDOMRange(..)
  , setStart_offset
  , setEnd_offset
  , setStartBefore
  , setStartAfter
  , setEndBefore
  , setEndAfter
  , collapse
  , selectNode
  , selectNodeContents
  , compareBoundaryPoints_sourceRange
  , deleteContents
  , extractContents
  , cloneContents
  , insertNode
  , surroundContents
  , cloneRange
  , toString
  , detach
  , createContextualFragment
  , compareNode
  , intersectsNode
  , comparePoint_offset
  , isPointInRange_offset
  , setStart
  , setEnd
  , compareBoundaryPoints
  , startContainer
  , startOffset
  , endContainer
  , endOffset
  , collapsed
  , commonAncestorContainer
  , text
  , webArchive
  , markupString
  , setStart_offsetSelector
  , setEnd_offsetSelector
  , setStartBeforeSelector
  , setStartAfterSelector
  , setEndBeforeSelector
  , setEndAfterSelector
  , collapseSelector
  , selectNodeSelector
  , selectNodeContentsSelector
  , compareBoundaryPoints_sourceRangeSelector
  , deleteContentsSelector
  , extractContentsSelector
  , cloneContentsSelector
  , insertNodeSelector
  , surroundContentsSelector
  , cloneRangeSelector
  , toStringSelector
  , detachSelector
  , createContextualFragmentSelector
  , compareNodeSelector
  , intersectsNodeSelector
  , comparePoint_offsetSelector
  , isPointInRange_offsetSelector
  , setStartSelector
  , setEndSelector
  , compareBoundaryPointsSelector
  , startContainerSelector
  , startOffsetSelector
  , endContainerSelector
  , endOffsetSelector
  , collapsedSelector
  , commonAncestorContainerSelector
  , textSelector
  , webArchiveSelector
  , markupStringSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- setStart:offset:@
setStart_offset :: (IsDOMRange domRange, IsDOMNode refNode) => domRange -> refNode -> CInt -> IO ()
setStart_offset domRange  refNode offset =
withObjCPtr refNode $ \raw_refNode ->
    sendMsg domRange (mkSelector "setStart:offset:") retVoid [argPtr (castPtr raw_refNode :: Ptr ()), argCInt (fromIntegral offset)]

-- | @- setEnd:offset:@
setEnd_offset :: (IsDOMRange domRange, IsDOMNode refNode) => domRange -> refNode -> CInt -> IO ()
setEnd_offset domRange  refNode offset =
withObjCPtr refNode $ \raw_refNode ->
    sendMsg domRange (mkSelector "setEnd:offset:") retVoid [argPtr (castPtr raw_refNode :: Ptr ()), argCInt (fromIntegral offset)]

-- | @- setStartBefore:@
setStartBefore :: (IsDOMRange domRange, IsDOMNode refNode) => domRange -> refNode -> IO ()
setStartBefore domRange  refNode =
withObjCPtr refNode $ \raw_refNode ->
    sendMsg domRange (mkSelector "setStartBefore:") retVoid [argPtr (castPtr raw_refNode :: Ptr ())]

-- | @- setStartAfter:@
setStartAfter :: (IsDOMRange domRange, IsDOMNode refNode) => domRange -> refNode -> IO ()
setStartAfter domRange  refNode =
withObjCPtr refNode $ \raw_refNode ->
    sendMsg domRange (mkSelector "setStartAfter:") retVoid [argPtr (castPtr raw_refNode :: Ptr ())]

-- | @- setEndBefore:@
setEndBefore :: (IsDOMRange domRange, IsDOMNode refNode) => domRange -> refNode -> IO ()
setEndBefore domRange  refNode =
withObjCPtr refNode $ \raw_refNode ->
    sendMsg domRange (mkSelector "setEndBefore:") retVoid [argPtr (castPtr raw_refNode :: Ptr ())]

-- | @- setEndAfter:@
setEndAfter :: (IsDOMRange domRange, IsDOMNode refNode) => domRange -> refNode -> IO ()
setEndAfter domRange  refNode =
withObjCPtr refNode $ \raw_refNode ->
    sendMsg domRange (mkSelector "setEndAfter:") retVoid [argPtr (castPtr raw_refNode :: Ptr ())]

-- | @- collapse:@
collapse :: IsDOMRange domRange => domRange -> Bool -> IO ()
collapse domRange  toStart =
  sendMsg domRange (mkSelector "collapse:") retVoid [argCULong (if toStart then 1 else 0)]

-- | @- selectNode:@
selectNode :: (IsDOMRange domRange, IsDOMNode refNode) => domRange -> refNode -> IO ()
selectNode domRange  refNode =
withObjCPtr refNode $ \raw_refNode ->
    sendMsg domRange (mkSelector "selectNode:") retVoid [argPtr (castPtr raw_refNode :: Ptr ())]

-- | @- selectNodeContents:@
selectNodeContents :: (IsDOMRange domRange, IsDOMNode refNode) => domRange -> refNode -> IO ()
selectNodeContents domRange  refNode =
withObjCPtr refNode $ \raw_refNode ->
    sendMsg domRange (mkSelector "selectNodeContents:") retVoid [argPtr (castPtr raw_refNode :: Ptr ())]

-- | @- compareBoundaryPoints:sourceRange:@
compareBoundaryPoints_sourceRange :: (IsDOMRange domRange, IsDOMRange sourceRange) => domRange -> CUShort -> sourceRange -> IO CShort
compareBoundaryPoints_sourceRange domRange  how sourceRange =
withObjCPtr sourceRange $ \raw_sourceRange ->
    fmap fromIntegral $ sendMsg domRange (mkSelector "compareBoundaryPoints:sourceRange:") retCInt [argCUInt (fromIntegral how), argPtr (castPtr raw_sourceRange :: Ptr ())]

-- | @- deleteContents@
deleteContents :: IsDOMRange domRange => domRange -> IO ()
deleteContents domRange  =
  sendMsg domRange (mkSelector "deleteContents") retVoid []

-- | @- extractContents@
extractContents :: IsDOMRange domRange => domRange -> IO (Id DOMDocumentFragment)
extractContents domRange  =
  sendMsg domRange (mkSelector "extractContents") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- cloneContents@
cloneContents :: IsDOMRange domRange => domRange -> IO (Id DOMDocumentFragment)
cloneContents domRange  =
  sendMsg domRange (mkSelector "cloneContents") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- insertNode:@
insertNode :: (IsDOMRange domRange, IsDOMNode newNode) => domRange -> newNode -> IO ()
insertNode domRange  newNode =
withObjCPtr newNode $ \raw_newNode ->
    sendMsg domRange (mkSelector "insertNode:") retVoid [argPtr (castPtr raw_newNode :: Ptr ())]

-- | @- surroundContents:@
surroundContents :: (IsDOMRange domRange, IsDOMNode newParent) => domRange -> newParent -> IO ()
surroundContents domRange  newParent =
withObjCPtr newParent $ \raw_newParent ->
    sendMsg domRange (mkSelector "surroundContents:") retVoid [argPtr (castPtr raw_newParent :: Ptr ())]

-- | @- cloneRange@
cloneRange :: IsDOMRange domRange => domRange -> IO (Id DOMRange)
cloneRange domRange  =
  sendMsg domRange (mkSelector "cloneRange") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- toString@
toString :: IsDOMRange domRange => domRange -> IO (Id NSString)
toString domRange  =
  sendMsg domRange (mkSelector "toString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- detach@
detach :: IsDOMRange domRange => domRange -> IO ()
detach domRange  =
  sendMsg domRange (mkSelector "detach") retVoid []

-- | @- createContextualFragment:@
createContextualFragment :: (IsDOMRange domRange, IsNSString html) => domRange -> html -> IO (Id DOMDocumentFragment)
createContextualFragment domRange  html =
withObjCPtr html $ \raw_html ->
    sendMsg domRange (mkSelector "createContextualFragment:") (retPtr retVoid) [argPtr (castPtr raw_html :: Ptr ())] >>= retainedObject . castPtr

-- | @- compareNode:@
compareNode :: (IsDOMRange domRange, IsDOMNode refNode) => domRange -> refNode -> IO CShort
compareNode domRange  refNode =
withObjCPtr refNode $ \raw_refNode ->
    fmap fromIntegral $ sendMsg domRange (mkSelector "compareNode:") retCInt [argPtr (castPtr raw_refNode :: Ptr ())]

-- | @- intersectsNode:@
intersectsNode :: (IsDOMRange domRange, IsDOMNode refNode) => domRange -> refNode -> IO Bool
intersectsNode domRange  refNode =
withObjCPtr refNode $ \raw_refNode ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg domRange (mkSelector "intersectsNode:") retCULong [argPtr (castPtr raw_refNode :: Ptr ())]

-- | @- comparePoint:offset:@
comparePoint_offset :: (IsDOMRange domRange, IsDOMNode refNode) => domRange -> refNode -> CInt -> IO CShort
comparePoint_offset domRange  refNode offset =
withObjCPtr refNode $ \raw_refNode ->
    fmap fromIntegral $ sendMsg domRange (mkSelector "comparePoint:offset:") retCInt [argPtr (castPtr raw_refNode :: Ptr ()), argCInt (fromIntegral offset)]

-- | @- isPointInRange:offset:@
isPointInRange_offset :: (IsDOMRange domRange, IsDOMNode refNode) => domRange -> refNode -> CInt -> IO Bool
isPointInRange_offset domRange  refNode offset =
withObjCPtr refNode $ \raw_refNode ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg domRange (mkSelector "isPointInRange:offset:") retCULong [argPtr (castPtr raw_refNode :: Ptr ()), argCInt (fromIntegral offset)]

-- | @- setStart::@
setStart :: (IsDOMRange domRange, IsDOMNode refNode) => domRange -> refNode -> CInt -> IO ()
setStart domRange  refNode offset =
withObjCPtr refNode $ \raw_refNode ->
    sendMsg domRange (mkSelector "setStart::") retVoid [argPtr (castPtr raw_refNode :: Ptr ()), argCInt (fromIntegral offset)]

-- | @- setEnd::@
setEnd :: (IsDOMRange domRange, IsDOMNode refNode) => domRange -> refNode -> CInt -> IO ()
setEnd domRange  refNode offset =
withObjCPtr refNode $ \raw_refNode ->
    sendMsg domRange (mkSelector "setEnd::") retVoid [argPtr (castPtr raw_refNode :: Ptr ()), argCInt (fromIntegral offset)]

-- | @- compareBoundaryPoints::@
compareBoundaryPoints :: (IsDOMRange domRange, IsDOMRange sourceRange) => domRange -> CUShort -> sourceRange -> IO CShort
compareBoundaryPoints domRange  how sourceRange =
withObjCPtr sourceRange $ \raw_sourceRange ->
    fmap fromIntegral $ sendMsg domRange (mkSelector "compareBoundaryPoints::") retCInt [argCUInt (fromIntegral how), argPtr (castPtr raw_sourceRange :: Ptr ())]

-- | @- startContainer@
startContainer :: IsDOMRange domRange => domRange -> IO (Id DOMNode)
startContainer domRange  =
  sendMsg domRange (mkSelector "startContainer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- startOffset@
startOffset :: IsDOMRange domRange => domRange -> IO CInt
startOffset domRange  =
  sendMsg domRange (mkSelector "startOffset") retCInt []

-- | @- endContainer@
endContainer :: IsDOMRange domRange => domRange -> IO (Id DOMNode)
endContainer domRange  =
  sendMsg domRange (mkSelector "endContainer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- endOffset@
endOffset :: IsDOMRange domRange => domRange -> IO CInt
endOffset domRange  =
  sendMsg domRange (mkSelector "endOffset") retCInt []

-- | @- collapsed@
collapsed :: IsDOMRange domRange => domRange -> IO Bool
collapsed domRange  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg domRange (mkSelector "collapsed") retCULong []

-- | @- commonAncestorContainer@
commonAncestorContainer :: IsDOMRange domRange => domRange -> IO (Id DOMNode)
commonAncestorContainer domRange  =
  sendMsg domRange (mkSelector "commonAncestorContainer") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- text@
text :: IsDOMRange domRange => domRange -> IO (Id NSString)
text domRange  =
  sendMsg domRange (mkSelector "text") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | webArchive
--
-- A WebArchive representing the range.
--
-- ObjC selector: @- webArchive@
webArchive :: IsDOMRange domRange => domRange -> IO (Id WebArchive)
webArchive domRange  =
  sendMsg domRange (mkSelector "webArchive") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | markupString
--
-- A markup string representing the range.
--
-- ObjC selector: @- markupString@
markupString :: IsDOMRange domRange => domRange -> IO (Id NSString)
markupString domRange  =
  sendMsg domRange (mkSelector "markupString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setStart:offset:@
setStart_offsetSelector :: Selector
setStart_offsetSelector = mkSelector "setStart:offset:"

-- | @Selector@ for @setEnd:offset:@
setEnd_offsetSelector :: Selector
setEnd_offsetSelector = mkSelector "setEnd:offset:"

-- | @Selector@ for @setStartBefore:@
setStartBeforeSelector :: Selector
setStartBeforeSelector = mkSelector "setStartBefore:"

-- | @Selector@ for @setStartAfter:@
setStartAfterSelector :: Selector
setStartAfterSelector = mkSelector "setStartAfter:"

-- | @Selector@ for @setEndBefore:@
setEndBeforeSelector :: Selector
setEndBeforeSelector = mkSelector "setEndBefore:"

-- | @Selector@ for @setEndAfter:@
setEndAfterSelector :: Selector
setEndAfterSelector = mkSelector "setEndAfter:"

-- | @Selector@ for @collapse:@
collapseSelector :: Selector
collapseSelector = mkSelector "collapse:"

-- | @Selector@ for @selectNode:@
selectNodeSelector :: Selector
selectNodeSelector = mkSelector "selectNode:"

-- | @Selector@ for @selectNodeContents:@
selectNodeContentsSelector :: Selector
selectNodeContentsSelector = mkSelector "selectNodeContents:"

-- | @Selector@ for @compareBoundaryPoints:sourceRange:@
compareBoundaryPoints_sourceRangeSelector :: Selector
compareBoundaryPoints_sourceRangeSelector = mkSelector "compareBoundaryPoints:sourceRange:"

-- | @Selector@ for @deleteContents@
deleteContentsSelector :: Selector
deleteContentsSelector = mkSelector "deleteContents"

-- | @Selector@ for @extractContents@
extractContentsSelector :: Selector
extractContentsSelector = mkSelector "extractContents"

-- | @Selector@ for @cloneContents@
cloneContentsSelector :: Selector
cloneContentsSelector = mkSelector "cloneContents"

-- | @Selector@ for @insertNode:@
insertNodeSelector :: Selector
insertNodeSelector = mkSelector "insertNode:"

-- | @Selector@ for @surroundContents:@
surroundContentsSelector :: Selector
surroundContentsSelector = mkSelector "surroundContents:"

-- | @Selector@ for @cloneRange@
cloneRangeSelector :: Selector
cloneRangeSelector = mkSelector "cloneRange"

-- | @Selector@ for @toString@
toStringSelector :: Selector
toStringSelector = mkSelector "toString"

-- | @Selector@ for @detach@
detachSelector :: Selector
detachSelector = mkSelector "detach"

-- | @Selector@ for @createContextualFragment:@
createContextualFragmentSelector :: Selector
createContextualFragmentSelector = mkSelector "createContextualFragment:"

-- | @Selector@ for @compareNode:@
compareNodeSelector :: Selector
compareNodeSelector = mkSelector "compareNode:"

-- | @Selector@ for @intersectsNode:@
intersectsNodeSelector :: Selector
intersectsNodeSelector = mkSelector "intersectsNode:"

-- | @Selector@ for @comparePoint:offset:@
comparePoint_offsetSelector :: Selector
comparePoint_offsetSelector = mkSelector "comparePoint:offset:"

-- | @Selector@ for @isPointInRange:offset:@
isPointInRange_offsetSelector :: Selector
isPointInRange_offsetSelector = mkSelector "isPointInRange:offset:"

-- | @Selector@ for @setStart::@
setStartSelector :: Selector
setStartSelector = mkSelector "setStart::"

-- | @Selector@ for @setEnd::@
setEndSelector :: Selector
setEndSelector = mkSelector "setEnd::"

-- | @Selector@ for @compareBoundaryPoints::@
compareBoundaryPointsSelector :: Selector
compareBoundaryPointsSelector = mkSelector "compareBoundaryPoints::"

-- | @Selector@ for @startContainer@
startContainerSelector :: Selector
startContainerSelector = mkSelector "startContainer"

-- | @Selector@ for @startOffset@
startOffsetSelector :: Selector
startOffsetSelector = mkSelector "startOffset"

-- | @Selector@ for @endContainer@
endContainerSelector :: Selector
endContainerSelector = mkSelector "endContainer"

-- | @Selector@ for @endOffset@
endOffsetSelector :: Selector
endOffsetSelector = mkSelector "endOffset"

-- | @Selector@ for @collapsed@
collapsedSelector :: Selector
collapsedSelector = mkSelector "collapsed"

-- | @Selector@ for @commonAncestorContainer@
commonAncestorContainerSelector :: Selector
commonAncestorContainerSelector = mkSelector "commonAncestorContainer"

-- | @Selector@ for @text@
textSelector :: Selector
textSelector = mkSelector "text"

-- | @Selector@ for @webArchive@
webArchiveSelector :: Selector
webArchiveSelector = mkSelector "webArchive"

-- | @Selector@ for @markupString@
markupStringSelector :: Selector
markupStringSelector = mkSelector "markupString"

